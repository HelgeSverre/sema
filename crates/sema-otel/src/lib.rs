//! OpenTelemetry GenAI tracing facade for the Sema runtime.
//!
//! Sync, opt-in, **no-op when disabled**. The hot path only enqueues plain owned
//! data onto a background batch processor; the OTel layer never touches Sema
//! `Value`/`Env`/`Rc`. A down/slow collector can never block or panic the VM thread
//! (see the plan's §3.3 fail-safe contract).
//!
//! Design contracts (locked decisions):
//! - `init_from_env()` is the ONLY function that installs a global provider
//!   (Decision #14). Embedded hosts own their provider; the facade resolves its
//!   tracer lazily from `opentelemetry::global` (Decision #12), so it transparently
//!   emits against whatever a host installed and is a silent no-op otherwise.
//! - Root spans seed their parent from `Context::current()` (Decision #13) so an
//!   embedded Sema run nests under the host's active span.
//! - On `wasm32` the whole export machinery is compiled out; spans are pure no-ops
//!   (Decision #11).

mod provider_map;
pub use provider_map::gen_ai_provider_name;

/// Plain owned view of a tool advertised to the model (for the OpenInference
/// `llm.tools.*` / Traceloop `llm.request.functions.*` compat aliases). Keeps
/// `sema-otel` free of `sema-llm` types.
#[derive(Debug, Clone, Default)]
pub struct ToolView {
    pub name: String,
    pub description: String,
    /// The tool's parameter JSON schema, already serialized.
    pub json_schema: String,
}

/// Plain `Send` snapshot of an LLM response, so `sema-otel` need not depend on
/// `sema-llm` types. `sema-llm` maps `ChatResponse`/`Usage` into this.
#[derive(Debug, Clone, Default)]
pub struct ResponseFacts {
    pub input_tokens: u32,
    pub output_tokens: u32,
    pub cache_read_input_tokens: u32,
    pub cache_creation_input_tokens: u32,
    pub response_model: String,
    pub finish_reason: Option<String>,
    pub cost_usd: Option<f64>,
    /// Per-direction cost split (USD) for backends that show it separately
    /// (OpenInference `llm.cost.prompt` / `.completion`). `None` when pricing is unknown.
    pub cost_prompt_usd: Option<f64>,
    pub cost_completion_usd: Option<f64>,
    pub cache_hit: bool,
}

#[cfg(not(target_arch = "wasm32"))]
mod compat;
#[cfg(not(target_arch = "wasm32"))]
mod file_exporter;
#[cfg(not(target_arch = "wasm32"))]
mod imp;
#[cfg(not(target_arch = "wasm32"))]
pub use compat::compat_active;
#[cfg(not(target_arch = "wasm32"))]
pub use imp::*;

// Re-export so embedded hosts can name `TelemetryMode::OwnProvider(..)`'s payload
// without taking a direct `opentelemetry_sdk` dependency.
#[cfg(not(target_arch = "wasm32"))]
pub use opentelemetry_sdk::trace::SdkTracerProvider;

#[cfg(all(not(target_arch = "wasm32"), feature = "testing"))]
pub mod testing;

#[cfg(target_arch = "wasm32")]
mod noop;
#[cfg(target_arch = "wasm32")]
pub use noop::*;
