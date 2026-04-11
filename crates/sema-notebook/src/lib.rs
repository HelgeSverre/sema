//! sema-notebook — Cell-based notebook interface for Sema.
//!
//! Provides a Jupyter-like notebook experience with:
//! - `.sema-nb` JSON file format for persistent notebooks
//! - Evaluation engine with shared environment across cells
//! - HTTP server with REST API for browser and agent interaction
//! - Embedded browser UI
//! - VFS shim for scoped filesystem access
//! - Markdown export
//!
//! ## Architecture
//!
//! The crate is structured for UI replaceability:
//! - `format` — file format types (pure data, no rendering)
//! - `engine` — evaluation logic (interpreter management)
//! - `render` — UI-agnostic rendering helpers (output formatting, API types)
//! - `server` — HTTP server (routes + handlers)
//! - `ui` — embedded browser UI (HTML/CSS/JS, replaceable)
//! - `vfs` — sandboxed filesystem access

pub mod bridge;
pub mod engine;
pub mod format;
pub mod render;
pub mod server;
pub mod ui;
pub mod vfs;

pub use engine::Engine;
pub use format::Notebook;
pub use server::serve;
