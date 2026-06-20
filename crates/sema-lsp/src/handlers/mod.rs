//! LSP request handlers, grouped by endpoint family.
//!
//! Each submodule contributes one or more `impl BackendState` methods of the
//! form `handle_*`, called from the backend actor loop in [`crate::server`].
//! State lives in [`crate::state::BackendState`]; these modules only add
//! behavior. The split mirrors the LSP feature groups so each file stays
//! focused on a single concern.

pub(crate) mod command;
pub(crate) mod completion;
pub(crate) mod formatting;
pub(crate) mod hover;
pub(crate) mod navigation;
pub(crate) mod range_formatting;
pub(crate) mod semantic_tokens;
pub(crate) mod structure;
