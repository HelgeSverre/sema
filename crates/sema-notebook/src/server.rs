//! HTTP server for the notebook interface.
//!
//! The Sema interpreter uses `Rc` and is single-threaded. To bridge this
//! with axum's async runtime, all evaluation runs on a dedicated OS thread
//! via `tokio::task::spawn_blocking`. An `Arc<Mutex<>>` is NOT used
//! because `Interpreter` is `!Send`. Instead, we use a channel-based
//! approach: the engine lives on its own thread and processes requests.

use std::path::PathBuf;
use std::sync::Arc;

use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::response::{Html, IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, oneshot};

use crate::format::{CellType, Notebook};
use crate::render;
use crate::ui;
use crate::vfs;

// ── Engine thread communication ──────────────────────────────────

type EngineResult<T> = Result<T, String>;

/// A request sent to the engine thread.
enum EngineRequest {
    GetNotebook {
        reply: oneshot::Sender<render::NotebookResponse>,
    },
    CreateCell {
        cell_type: CellType,
        source: String,
        after: Option<String>,
        reply: oneshot::Sender<EngineResult<CreateCellData>>,
    },
    GetCell {
        id: String,
        reply: oneshot::Sender<EngineResult<render::RenderedCell>>,
    },
    UpdateCell {
        id: String,
        source: Option<String>,
        cell_type: Option<String>,
        reply: oneshot::Sender<EngineResult<render::RenderedCell>>,
    },
    EvalCell {
        id: String,
        reply: oneshot::Sender<EngineResult<render::EvalResponse>>,
    },
    DeleteCell {
        id: String,
        reply: oneshot::Sender<EngineResult<()>>,
    },
    ReorderCells {
        cell_ids: Vec<String>,
        reply: oneshot::Sender<EngineResult<()>>,
    },
    EvalAll {
        reply: oneshot::Sender<Vec<render::EvalResponse>>,
    },
    GetEnv {
        reply: oneshot::Sender<render::EnvResponse>,
    },
    Reset {
        reply: oneshot::Sender<()>,
    },
    Save {
        reply: oneshot::Sender<EngineResult<String>>,
    },
}

#[derive(Clone, Serialize)]
struct CreateCellData {
    id: String,
    #[serde(flatten)]
    cell: render::RenderedCell,
}

/// Shared server state — just a channel to the engine thread.
struct AppState {
    tx: mpsc::Sender<EngineRequest>,
    vfs_root: PathBuf,
}

/// Spawn the engine on a dedicated OS thread and return a channel sender.
fn spawn_engine_thread(
    notebook: Notebook,
    notebook_path: Option<PathBuf>,
) -> mpsc::Sender<EngineRequest> {
    let (tx, mut rx) = mpsc::channel::<EngineRequest>(64);

    std::thread::spawn(move || {
        use crate::engine::Engine;

        let mut engine = Engine::new(notebook);
        let nb_path = notebook_path;

        // Block on receiving requests
        while let Some(req) = {
            // Use a small tokio runtime just to receive from the channel
            let rt = tokio::runtime::Builder::new_current_thread()
                .build()
                .unwrap();
            rt.block_on(rx.recv())
        } {
            match req {
                EngineRequest::GetNotebook { reply } => {
                    let _ = reply.send(render::notebook_response(&engine.notebook));
                }
                EngineRequest::CreateCell {
                    cell_type,
                    source,
                    after,
                    reply,
                } => {
                    let id = match cell_type {
                        CellType::Code => engine.notebook.add_code_cell(&source),
                        CellType::Markdown => engine.notebook.add_markdown_cell(&source),
                    };
                    if let Some(after_id) = &after {
                        let new_idx = engine.notebook.cell_index(&id).unwrap();
                        if let Some(after_idx) = engine.notebook.cell_index(after_id) {
                            let cell = engine.notebook.cells.remove(new_idx);
                            let insert_at = if after_idx < new_idx {
                                after_idx + 1
                            } else {
                                after_idx
                            };
                            engine.notebook.cells.insert(insert_at, cell);
                        }
                    }
                    let cell = engine.notebook.cell(&id).unwrap();
                    let cell_number = engine
                        .notebook
                        .cells
                        .iter()
                        .filter(|c| c.cell_type == CellType::Code)
                        .position(|c| c.id == id)
                        .map(|i| i + 1);
                    let rendered = render::render_cell(cell, cell_number);
                    let _ = reply.send(Ok(CreateCellData { id, cell: rendered }));
                }
                EngineRequest::GetCell { id, reply } => {
                    let result = match engine.notebook.cell(&id) {
                        Some(cell) => {
                            let cell_number = engine
                                .notebook
                                .cells
                                .iter()
                                .filter(|c| c.cell_type == CellType::Code)
                                .position(|c| c.id == id)
                                .map(|i| i + 1);
                            Ok(render::render_cell(cell, cell_number))
                        }
                        None => Err(format!("Cell not found: {id}")),
                    };
                    let _ = reply.send(result);
                }
                EngineRequest::UpdateCell {
                    id,
                    source,
                    cell_type,
                    reply,
                } => {
                    let result = (|| {
                        let cell = engine
                            .notebook
                            .cell_mut(&id)
                            .ok_or_else(|| format!("Cell not found: {id}"))?;
                        if let Some(s) = source {
                            cell.source = s;
                            if !cell.outputs.is_empty() {
                                cell.stale = true;
                            }
                        }
                        if let Some(ct) = cell_type {
                            cell.cell_type = match ct.as_str() {
                                "code" => CellType::Code,
                                "markdown" => CellType::Markdown,
                                other => return Err(format!("Unknown type: {other}")),
                            };
                        }
                        let cell = engine.notebook.cell(&id).unwrap();
                        let cell_number = engine
                            .notebook
                            .cells
                            .iter()
                            .filter(|c| c.cell_type == CellType::Code)
                            .position(|c| c.id == id)
                            .map(|i| i + 1);
                        Ok(render::render_cell(cell, cell_number))
                    })();
                    let _ = reply.send(result);
                }
                EngineRequest::EvalCell { id, reply } => {
                    let result = engine.eval_cell(&id).map(|r| {
                        let output = render::render_output(&r.output);
                        render::EvalResponse {
                            id: id.clone(),
                            output,
                            stdout: r.stdout,
                        }
                    });
                    let _ = reply.send(result);
                }
                EngineRequest::DeleteCell { id, reply } => {
                    let result = match engine.notebook.cell_index(&id) {
                        Some(idx) => {
                            engine.notebook.cells.remove(idx);
                            Ok(())
                        }
                        None => Err(format!("Cell not found: {id}")),
                    };
                    let _ = reply.send(result);
                }
                EngineRequest::ReorderCells { cell_ids, reply } => {
                    let result = (|| {
                        let mut reordered = Vec::with_capacity(cell_ids.len());
                        for id in &cell_ids {
                            let idx = engine
                                .notebook
                                .cell_index(id)
                                .ok_or_else(|| format!("Cell not found: {id}"))?;
                            reordered.push(engine.notebook.cells[idx].clone());
                        }
                        for cell in &engine.notebook.cells {
                            if !cell_ids.contains(&cell.id) {
                                reordered.push(cell.clone());
                            }
                        }
                        engine.notebook.cells = reordered;
                        Ok(())
                    })();
                    let _ = reply.send(result);
                }
                EngineRequest::EvalAll { reply } => {
                    let results = engine.eval_all();
                    let responses = results
                        .into_iter()
                        .map(|(id, result)| {
                            let (output, stdout) = match result {
                                Ok(r) => (render::render_output(&r.output), r.stdout),
                                Err(e) => (
                                    render::RenderedOutput {
                                        output_type: crate::format::OutputType::Error,
                                        content: e,
                                        mime_type: "text/x-sema-error".to_string(),
                                        meta: render::OutputMeta::default(),
                                    },
                                    None,
                                ),
                            };
                            render::EvalResponse { id, output, stdout }
                        })
                        .collect();
                    let _ = reply.send(responses);
                }
                EngineRequest::GetEnv { reply } => {
                    let _ = reply.send(render::EnvResponse {
                        bindings: engine.env_bindings(),
                    });
                }
                EngineRequest::Reset { reply } => {
                    engine.reset();
                    let _ = reply.send(());
                }
                EngineRequest::Save { reply } => {
                    let result = match &nb_path {
                        Some(path) => engine
                            .notebook
                            .save(path)
                            .map(|_| path.display().to_string()),
                        None => Err("No file path".to_string()),
                    };
                    let _ = reply.send(result);
                }
            }
        }
    });

    tx
}

/// Start the notebook HTTP server.
pub async fn serve(notebook_path: Option<PathBuf>, port: u16) {
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

    let tx = spawn_engine_thread(notebook, nb_path);

    let state = Arc::new(AppState { tx, vfs_root });

    let app = Router::new()
        // Browser UI
        .route("/", get(index_handler))
        .route("/ui/{*path}", get(ui_asset_handler))
        // Notebook API
        .route("/api/notebook", get(get_notebook))
        .route("/api/cells", post(create_cell))
        .route("/api/cells/{id}", get(get_cell))
        .route("/api/cells/{id}", post(update_cell))
        .route("/api/cells/{id}/eval", post(eval_cell))
        .route("/api/cells/{id}/delete", post(delete_cell))
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

    let addr = format!("0.0.0.0:{port}");
    eprintln!("Sema Notebook server listening on http://localhost:{port}");

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

// ── Helper to send a request and await the reply ─────────────────

async fn send_request<T>(
    tx: &mpsc::Sender<EngineRequest>,
    make_req: impl FnOnce(oneshot::Sender<T>) -> EngineRequest,
) -> Result<T, (StatusCode, String)> {
    let (reply_tx, reply_rx) = oneshot::channel();
    tx.send(make_req(reply_tx)).await.map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            "Engine thread died".to_string(),
        )
    })?;
    reply_rx.await.map_err(|_| {
        (
            StatusCode::INTERNAL_SERVER_ERROR,
            "Engine dropped reply".to_string(),
        )
    })
}

// ── UI handlers ──────────────────────────────────────────────────

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

// ── Notebook API handlers ────────────────────────────────────────

async fn get_notebook(
    State(state): State<Arc<AppState>>,
) -> Result<Json<render::NotebookResponse>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::GetNotebook { reply }).await?;
    Ok(Json(resp))
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
) -> Result<Json<CreateCellData>, (StatusCode, String)> {
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
    let resp = send_request(&state.tx, |reply| EngineRequest::CreateCell {
        cell_type,
        source: req.source,
        after: req.after,
        reply,
    })
    .await?;
    resp.map(Json).map_err(|e| (StatusCode::BAD_REQUEST, e))
}

async fn get_cell(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<render::RenderedCell>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::GetCell { id, reply }).await?;
    resp.map(Json).map_err(|e| (StatusCode::NOT_FOUND, e))
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
    let resp = send_request(&state.tx, |reply| EngineRequest::UpdateCell {
        id,
        source: req.source,
        cell_type: req.cell_type,
        reply,
    })
    .await?;
    resp.map(Json).map_err(|e| (StatusCode::NOT_FOUND, e))
}

async fn eval_cell(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<render::EvalResponse>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::EvalCell { id, reply }).await?;
    resp.map(Json).map_err(|e| (StatusCode::BAD_REQUEST, e))
}

async fn delete_cell(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::DeleteCell {
        id: id.clone(),
        reply,
    })
    .await?;
    resp.map(|_| Json(serde_json::json!({"deleted": id})))
        .map_err(|e| (StatusCode::NOT_FOUND, e))
}

#[derive(Deserialize)]
struct ReorderRequest {
    cell_ids: Vec<String>,
}

async fn reorder_cells(
    State(state): State<Arc<AppState>>,
    Json(req): Json<ReorderRequest>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::ReorderCells {
        cell_ids: req.cell_ids,
        reply,
    })
    .await?;
    resp.map(|_| Json(serde_json::json!({"ok": true})))
        .map_err(|e| (StatusCode::BAD_REQUEST, e))
}

async fn eval_all(
    State(state): State<Arc<AppState>>,
) -> Result<Json<Vec<render::EvalResponse>>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::EvalAll { reply }).await?;
    Ok(Json(resp))
}

async fn get_env(
    State(state): State<Arc<AppState>>,
) -> Result<Json<render::EnvResponse>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::GetEnv { reply }).await?;
    Ok(Json(resp))
}

async fn reset(
    State(state): State<Arc<AppState>>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    send_request(&state.tx, |reply| EngineRequest::Reset { reply }).await?;
    Ok(Json(serde_json::json!({"ok": true})))
}

async fn save_notebook(
    State(state): State<Arc<AppState>>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    let resp = send_request(&state.tx, |reply| EngineRequest::Save { reply }).await?;
    resp.map(|path| Json(serde_json::json!({"saved": path})))
        .map_err(|e| (StatusCode::BAD_REQUEST, e))
}

// ── VFS handlers ─────────────────────────────────────────────────

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
