//! Engine bridge — async API for communicating with the `!Send` interpreter.
//!
//! The Sema interpreter uses `Rc` and is single-threaded. This module
//! bridges async callers (like the HTTP server) to the interpreter via
//! a dedicated OS thread and an mpsc channel. All evaluation happens on
//! the engine thread; callers get typed async methods that hide the
//! channel plumbing.

use std::fmt;
use std::path::PathBuf;

use tokio::sync::{mpsc, oneshot};

use crate::format::{CellType, Notebook};
use crate::render;

// ── Error type ──────────────────────────────────────────────────

/// Errors returned by [`EngineHandle`] methods.
#[derive(Debug)]
pub enum BridgeError {
    /// The engine thread has exited or the channel is closed.
    EngineDown,
    /// The engine processed the request but returned an error.
    Request(String),
}

impl fmt::Display for BridgeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BridgeError::EngineDown => write!(f, "Engine thread died"),
            BridgeError::Request(msg) => write!(f, "{msg}"),
        }
    }
}

// ── Internal request enum (private) ─────────────────────────────

type EngineResult<T> = Result<T, String>;

enum EngineRequest {
    GetNotebook {
        reply: oneshot::Sender<render::NotebookResponse>,
    },
    CreateCell {
        cell_type: CellType,
        source: String,
        after: Option<String>,
        reply: oneshot::Sender<EngineResult<render::CreateCellData>>,
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
        /// Optional map of cell_id -> source to sync before evaluating.
        sources: Vec<(String, String)>,
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

// ── Engine handle ───────────────────────────────────────────────

/// Async handle to the notebook engine running on a dedicated thread.
pub struct EngineHandle {
    tx: mpsc::Sender<EngineRequest>,
}

impl EngineHandle {
    /// Spawn the engine on a dedicated OS thread and return a handle.
    pub fn spawn(notebook: Notebook, notebook_path: Option<PathBuf>) -> Self {
        let (tx, mut rx) = mpsc::channel::<EngineRequest>(64);

        std::thread::spawn(move || {
            use crate::engine::Engine;

            let mut engine = Engine::new(notebook);
            let nb_path = notebook_path;

            let rt = tokio::runtime::Builder::new_current_thread()
                .build()
                .unwrap();

            while let Some(req) = rt.block_on(rx.recv()) {
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
                        let result = (|| {
                            let id = match cell_type {
                                CellType::Code => engine.notebook.add_code_cell(&source),
                                CellType::Markdown => engine.notebook.add_markdown_cell(&source),
                            };
                            if let Some(after_id) = &after {
                                let after_idx =
                                    engine.notebook.cell_index(after_id).ok_or_else(|| {
                                        format!("Cell not found: {after_id}")
                                    })?;
                                let new_idx = engine.notebook.cell_index(&id).unwrap();
                                let cell = engine.notebook.cells.remove(new_idx);
                                let insert_at = if after_idx < new_idx {
                                    after_idx + 1
                                } else {
                                    after_idx
                                };
                                engine.notebook.cells.insert(insert_at, cell);
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
                            Ok(render::CreateCellData { id, cell: rendered })
                        })();
                        let _ = reply.send(result);
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
                    EngineRequest::EvalAll { sources, reply } => {
                        for (id, source) in sources {
                            if let Some(cell) = engine.notebook.cell_mut(&id) {
                                cell.source = source;
                            }
                        }
                        let results = engine.eval_all();
                        let responses = results
                            .into_iter()
                            .map(|(id, result)| {
                                let output = match result {
                                    Ok(r) => render::render_output(&r.output),
                                    Err(e) => render::RenderedOutput {
                                        output_type: crate::format::OutputType::Error,
                                        content: e,
                                        mime_type: "text/x-sema-error".to_string(),
                                        meta: render::OutputMeta::default(),
                                    },
                                };
                                render::EvalResponse { id, output }
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

        Self { tx }
    }

    // ── Private send helper ─────────────────────────────────────

    async fn send<T>(
        &self,
        make_req: impl FnOnce(oneshot::Sender<T>) -> EngineRequest,
    ) -> Result<T, BridgeError> {
        let (reply_tx, reply_rx) = oneshot::channel();
        self.tx
            .send(make_req(reply_tx))
            .await
            .map_err(|_| BridgeError::EngineDown)?;
        reply_rx.await.map_err(|_| BridgeError::EngineDown)
    }

    // ── Public typed methods ────────────────────────────────────

    pub async fn get_notebook(&self) -> Result<render::NotebookResponse, BridgeError> {
        self.send(|reply| EngineRequest::GetNotebook { reply }).await
    }

    pub async fn create_cell(
        &self,
        cell_type: CellType,
        source: String,
        after: Option<String>,
    ) -> Result<render::CreateCellData, BridgeError> {
        self.send(|reply| EngineRequest::CreateCell {
            cell_type,
            source,
            after,
            reply,
        })
        .await?
        .map_err(BridgeError::Request)
    }

    pub async fn get_cell(&self, id: String) -> Result<render::RenderedCell, BridgeError> {
        self.send(|reply| EngineRequest::GetCell { id, reply })
            .await?
            .map_err(BridgeError::Request)
    }

    pub async fn update_cell(
        &self,
        id: String,
        source: Option<String>,
        cell_type: Option<String>,
    ) -> Result<render::RenderedCell, BridgeError> {
        self.send(|reply| EngineRequest::UpdateCell {
            id,
            source,
            cell_type,
            reply,
        })
        .await?
        .map_err(BridgeError::Request)
    }

    pub async fn eval_cell(&self, id: String) -> Result<render::EvalResponse, BridgeError> {
        self.send(|reply| EngineRequest::EvalCell { id, reply })
            .await?
            .map_err(BridgeError::Request)
    }

    pub async fn delete_cell(&self, id: String) -> Result<(), BridgeError> {
        self.send(|reply| EngineRequest::DeleteCell { id, reply })
            .await?
            .map_err(BridgeError::Request)
    }

    pub async fn reorder_cells(&self, cell_ids: Vec<String>) -> Result<(), BridgeError> {
        self.send(|reply| EngineRequest::ReorderCells { cell_ids, reply })
            .await?
            .map_err(BridgeError::Request)
    }

    pub async fn eval_all(
        &self,
        sources: Vec<(String, String)>,
    ) -> Result<Vec<render::EvalResponse>, BridgeError> {
        self.send(|reply| EngineRequest::EvalAll { sources, reply })
            .await
    }

    pub async fn get_env(&self) -> Result<render::EnvResponse, BridgeError> {
        self.send(|reply| EngineRequest::GetEnv { reply }).await
    }

    pub async fn reset(&self) -> Result<(), BridgeError> {
        self.send(|reply| EngineRequest::Reset { reply }).await
    }

    pub async fn save(&self) -> Result<String, BridgeError> {
        self.send(|reply| EngineRequest::Save { reply })
            .await?
            .map_err(BridgeError::Request)
    }
}
