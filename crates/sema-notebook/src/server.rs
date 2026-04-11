//! HTTP server for the notebook interface.
//!
//! This module handles HTTP routing and request/response serialization.
//! All interpreter interaction goes through [`bridge::EngineHandle`].

use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::Deserialize;

use crate::bridge::{BridgeError, EngineHandle};
use crate::format::{CellType, Notebook};
use crate::render;
use crate::ui;
use crate::vfs;

// ── Error mapping ───────────────────────────────────────────────

fn bridge_err(e: BridgeError, status: StatusCode) -> (StatusCode, String) {
    match e {
        BridgeError::EngineDown => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()),
        BridgeError::Request(_) => (status, e.to_string()),
    }
}

// ── Shared state ────────────────────────────────────────────────

struct AppState {
    engine: EngineHandle,
    vfs_root: PathBuf,
}

// ── Server startup ──────────────────────────────────────────────

/// Start the notebook HTTP server.
pub async fn serve(notebook_path: Option<PathBuf>, host: &str, port: u16) {
    let (notebook, nb_path) = match &notebook_path {
        Some(path) => {
            if path.exists() {
                match Notebook::load(path) {
                    Ok(nb) => (nb, Some(path.clone())),
                    Err(e) => {
                        eprintln!("Error loading notebook: {e}");
                        std::process::exit(1);
                    }
                }
            } else {
                let title = path
                    .file_stem()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_else(|| "Untitled".to_string());
                (Notebook::new(&title), Some(path.clone()))
            }
        }
        None => (Notebook::new("Untitled"), None),
    };

    let vfs_root = notebook_path
        .as_ref()
        .and_then(|p| p.parent().map(|d| d.to_path_buf()))
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

    let engine = EngineHandle::spawn(notebook, nb_path);
    let state = Arc::new(AppState { engine, vfs_root });

    let app = Router::new()
        // Browser UI
        .route("/", get(index_handler))
        .route("/ui/{*path}", get(ui_asset_handler))
        // Notebook API
        .route("/api/notebook", get(get_notebook))
        .route("/api/cells", post(create_cell))
        .route(
            "/api/cells/{id}",
            get(get_cell).post(update_cell).delete(delete_cell),
        )
        .route("/api/cells/{id}/eval", post(eval_cell))
        .route("/api/cells/reorder", post(reorder_cells))
        .route("/api/eval-all", post(eval_all))
        .route("/api/env", get(get_env))
        .route("/api/reset", post(reset))
        .route("/api/save", post(save_notebook))
        // VFS endpoints
        .route("/vfs/read", get(vfs_read_handler))
        .route("/vfs/write", post(vfs_write_handler))
        .route("/vfs/list", get(vfs_list_handler))
        .with_state(state);

    let addr = format!("{host}:{port}");
    eprintln!("Sema Notebook server listening on http://{host}:{port}");

    let listener = tokio::net::TcpListener::bind(&addr)
        .await
        .unwrap_or_else(|e| {
            eprintln!("Failed to bind to {addr}: {e}");
            std::process::exit(1);
        });

    axum::serve(listener, app)
        .await
        .unwrap_or_else(|e| eprintln!("Server error: {e}"));
}

// ── UI handlers ─────────────────────────────────────────────────

async fn index_handler() -> Html<String> {
    Html(ui::index_html())
}

async fn ui_asset_handler(Path(path): Path<String>) -> Response {
    match ui::asset(&path) {
        Some((content, mime)) => {
            let headers = [(axum::http::header::CONTENT_TYPE, mime)];
            (headers, content).into_response()
        }
        None => StatusCode::NOT_FOUND.into_response(),
    }
}

// ── Notebook API handlers ───────────────────────────────────────

async fn get_notebook(
    State(state): State<Arc<AppState>>,
) -> Result<Json<render::NotebookResponse>, (StatusCode, String)> {
    state
        .engine
        .get_notebook()
        .await
        .map(Json)
        .map_err(|e| bridge_err(e, StatusCode::INTERNAL_SERVER_ERROR))
}

#[derive(Deserialize)]
struct CreateCellRequest {
    #[serde(rename = "type", default = "default_code")]
    cell_type: String,
    #[serde(default)]
    source: String,
    #[serde(default)]
    after: Option<String>,
}

fn default_code() -> String {
    "code".to_string()
}

async fn create_cell(
    State(state): State<Arc<AppState>>,
    Json(req): Json<CreateCellRequest>,
) -> Result<Json<render::CreateCellData>, (StatusCode, String)> {
    let cell_type = match req.cell_type.as_str() {
        "code" => CellType::Code,
        "markdown" => CellType::Markdown,
        other => {
            return Err((
                StatusCode::BAD_REQUEST,
                format!("Unknown cell type: {other}"),
            ))
        }
    };
    state
        .engine
        .create_cell(cell_type, req.source, req.after)
        .await
        .map(Json)
        .map_err(|e| bridge_err(e, StatusCode::BAD_REQUEST))
}

async fn get_cell(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<render::RenderedCell>, (StatusCode, String)> {
    state
        .engine
        .get_cell(id)
        .await
        .map(Json)
        .map_err(|e| bridge_err(e, StatusCode::NOT_FOUND))
}

#[derive(Deserialize)]
struct UpdateCellRequest {
    #[serde(default)]
    source: Option<String>,
    #[serde(rename = "type")]
    cell_type: Option<String>,
}

async fn update_cell(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
    Json(req): Json<UpdateCellRequest>,
) -> Result<Json<render::RenderedCell>, (StatusCode, String)> {
    state
        .engine
        .update_cell(id, req.source, req.cell_type)
        .await
        .map(Json)
        .map_err(|e| bridge_err(e, StatusCode::NOT_FOUND))
}

async fn eval_cell(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<render::EvalResponse>, (StatusCode, String)> {
    state
        .engine
        .eval_cell(id)
        .await
        .map(Json)
        .map_err(|e| bridge_err(e, StatusCode::BAD_REQUEST))
}

async fn delete_cell(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    state
        .engine
        .delete_cell(id.clone())
        .await
        .map(|_| Json(serde_json::json!({"deleted": id})))
        .map_err(|e| bridge_err(e, StatusCode::NOT_FOUND))
}

#[derive(Deserialize)]
struct ReorderRequest {
    cell_ids: Vec<String>,
}

async fn reorder_cells(
    State(state): State<Arc<AppState>>,
    Json(req): Json<ReorderRequest>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    state
        .engine
        .reorder_cells(req.cell_ids)
        .await
        .map(|_| Json(serde_json::json!({"ok": true})))
        .map_err(|e| bridge_err(e, StatusCode::BAD_REQUEST))
}

#[derive(Deserialize, Default)]
struct EvalAllRequest {
    #[serde(default)]
    sources: Vec<(String, String)>,
}

async fn eval_all(
    State(state): State<Arc<AppState>>,
    body: Option<Json<EvalAllRequest>>,
) -> Result<Json<Vec<render::EvalResponse>>, (StatusCode, String)> {
    let sources = body.map(|b| b.0.sources).unwrap_or_default();
    state
        .engine
        .eval_all(sources)
        .await
        .map(Json)
        .map_err(|e| bridge_err(e, StatusCode::INTERNAL_SERVER_ERROR))
}

async fn get_env(
    State(state): State<Arc<AppState>>,
) -> Result<Json<render::EnvResponse>, (StatusCode, String)> {
    state
        .engine
        .get_env()
        .await
        .map(Json)
        .map_err(|e| bridge_err(e, StatusCode::INTERNAL_SERVER_ERROR))
}

async fn reset(
    State(state): State<Arc<AppState>>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    state
        .engine
        .reset()
        .await
        .map(|_| Json(serde_json::json!({"ok": true})))
        .map_err(|e| bridge_err(e, StatusCode::INTERNAL_SERVER_ERROR))
}

async fn save_notebook(
    State(state): State<Arc<AppState>>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    state
        .engine
        .save()
        .await
        .map(|path| Json(serde_json::json!({"saved": path})))
        .map_err(|e| bridge_err(e, StatusCode::BAD_REQUEST))
}

// ── VFS handlers ────────────────────────────────────────────────

#[derive(Deserialize)]
struct VfsPathQuery {
    path: String,
}

async fn vfs_read_handler(
    State(state): State<Arc<AppState>>,
    Query(q): Query<VfsPathQuery>,
) -> Result<String, (StatusCode, String)> {
    vfs::read_file(&state.vfs_root, &q.path).map_err(|e| (StatusCode::BAD_REQUEST, e))
}

#[derive(Deserialize)]
struct VfsWriteRequest {
    path: String,
    content: String,
}

async fn vfs_write_handler(
    State(state): State<Arc<AppState>>,
    Json(req): Json<VfsWriteRequest>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    vfs::write_file(&state.vfs_root, &req.path, &req.content)
        .map_err(|e| (StatusCode::BAD_REQUEST, e))?;
    Ok(Json(serde_json::json!({"ok": true})))
}

async fn vfs_list_handler(
    State(state): State<Arc<AppState>>,
    Query(q): Query<VfsPathQuery>,
) -> Result<Json<Vec<vfs::FileEntry>>, (StatusCode, String)> {
    vfs::list_dir(&state.vfs_root, &q.path)
        .map_err(|e| (StatusCode::BAD_REQUEST, e))
        .map(Json)
}
