#![allow(
    clippy::mutable_key_type,
    clippy::collapsible_match,
    clippy::too_many_arguments,
    clippy::cloned_ref_to_slice_refs
)]
pub mod anthropic;
pub mod builtins;
pub mod embeddings;
pub mod gemini;
pub mod ndjson;
pub mod ollama;
pub mod openai;
pub mod pricing;
pub mod provider;
pub mod sse;
pub mod types;
