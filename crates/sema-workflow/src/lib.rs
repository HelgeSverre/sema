//! Sema dynamic-workflow runtime — Spike 1: a SEQUENTIAL, in-process runtime that
//! journals a frozen JSONL run-directory and returns a discriminated-union
//! `{:status …}` envelope.
//!
//! Scope (Spike 1): no parallel, no resume, no canonical hashing, no subprocess,
//! no daemon. The run directory is the stable public contract (treat like the
//! `.semac` bytecode format) — its layout and event vocabulary are FROZEN.
//!
//! This crate is a leaf: it depends only on `sema-core` + `sema-otel` + serde.
//! The builtins that invoke Sema thunks (`workflow/run`, `workflow/phase`,
//! `checkpoint`) live in `sema-stdlib`, which depends on this crate for the types.

pub mod context;
pub mod event;
mod journal;

pub use context::{current, resolve_run_id, set_workflow_scope, WorkflowCtx, WorkflowGuard};
pub use event::WorkflowEvent;
pub use journal::Journal;

/// Project-local run-directory root (cwd-relative, git-ignorable). NOT
/// `sema_home()` (`~/.sema`) — a run dir belongs to the project being worked on.
/// The CLI overrides the base via the `SEMA_WORKFLOW_RUN_DIR` seam (see
/// [`context::resolve_runs_root`]).
pub const RUNS_ROOT: &str = ".sema/runs";

/// Filename of the cross-run SQLite projection index, under the run-dir base
/// (`<run-dir>/index.db`). One DB indexes every run for the dashboard's cross-run views.
pub const INDEX_DB: &str = "index.db";
