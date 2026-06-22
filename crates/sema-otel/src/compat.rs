//! Backend compatibility layer (`SEMA_OTEL_COMPAT`).
//!
//! Sema emits the canonical OTel GenAI semconv (`gen_ai.*`) by default and renders
//! first-class in any vanilla-OTel backend (Grafana/Tempo, SigNoz, Datadog, Honeycomb,
//! Elastic, New Relic, OpenLIT) and in Logfire/Braintrust — no config. This module adds
//! each non-conforming backend's NATIVE alias keys when `SEMA_OTEL_COMPAT` opts in, so
//! Sema also renders first-class in Phoenix/Arize (OpenInference), Traceloop
//! (OpenLLMetry), LangSmith, and fills Langfuse's native fields.
//!
//! Isolated and mapping-table-driven (like `provider_map.rs`): the emit functions
//! return `Vec<KeyValue>` (empty when the relevant backend isn't active), and `imp.rs`
//! applies them. Zero cost / zero alias attributes when `SEMA_OTEL_COMPAT` is unset.

use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::OnceLock;

use opentelemetry::{Array, KeyValue, StringValue, Value};

/// One bit per backend.
#[derive(Clone, Copy)]
pub(crate) struct CompatSet(u8);

impl CompatSet {
    pub(crate) const OPENINFERENCE: u8 = 1 << 0;
    pub(crate) const TRACELOOP: u8 = 1 << 1;
    pub(crate) const LANGSMITH: u8 = 1 << 2;
    pub(crate) const LANGFUSE: u8 = 1 << 3;
    pub(crate) const BRAINTRUST: u8 = 1 << 4;
    const ALL: u8 =
        Self::OPENINFERENCE | Self::TRACELOOP | Self::LANGSMITH | Self::LANGFUSE | Self::BRAINTRUST;

    pub(crate) fn is_empty(self) -> bool {
        self.0 == 0
    }
    fn has(self, bit: u8) -> bool {
        self.0 & bit != 0
    }
}

static ACTIVE: OnceLock<u8> = OnceLock::new();
/// Test override: `0xFF` is the "unset" sentinel (no real combo reaches it — `ALL` is
/// 31). When set, it bypasses the env-parsed `OnceLock`.
static TEST_OVERRIDE: AtomicU8 = AtomicU8::new(0xFF);

fn parse(tokens: &str) -> u8 {
    let mut bits = 0u8;
    for tok in tokens.split(',') {
        bits |= match tok.trim().to_ascii_lowercase().as_str() {
            "openinference" | "phoenix" | "arize" => CompatSet::OPENINFERENCE,
            "traceloop" | "openllmetry" => CompatSet::TRACELOOP,
            "langsmith" => CompatSet::LANGSMITH,
            "langfuse" => CompatSet::LANGFUSE,
            "braintrust" => CompatSet::BRAINTRUST,
            "all" => CompatSet::ALL,
            _ => 0, // unknown token ignored (never panic)
        };
    }
    bits
}

/// The active backend set. One `OnceLock` load on the fast path; reads a test override
/// first (a sentinel keeps production on the cached path).
pub(crate) fn active() -> CompatSet {
    let ov = TEST_OVERRIDE.load(Ordering::Relaxed);
    if ov != 0xFF {
        return CompatSet(ov);
    }
    CompatSet(*ACTIVE.get_or_init(|| {
        std::env::var("SEMA_OTEL_COMPAT")
            .ok()
            .map(|v| parse(&v))
            .unwrap_or(0)
    }))
}

/// Whether any backend compat is active (so callers can skip data prep cheaply).
pub fn compat_active() -> bool {
    !active().is_empty()
}

/// Test hook: force the active set from a token string (bypasses env).
#[cfg_attr(not(feature = "testing"), allow(dead_code))]
pub(crate) fn set_test_override(tokens: &str) {
    TEST_OVERRIDE.store(parse(tokens), Ordering::Relaxed);
}

// ---------------------------------------------------------------------------
// Provider back-translation (OpenInference uses its own enums, from the RAW name)
// ---------------------------------------------------------------------------

/// OpenInference `llm.provider` (the hosting provider). Back-translated from the RAW
/// Sema provider name (NOT the already-mapped `gen_ai.provider.name`).
fn openinference_provider(raw: &str) -> &str {
    match raw {
        "gemini" | "vertex" => "google",
        "mistral" => "mistralai",
        "x_ai" | "xai" => "xai",
        other => other, // openai/anthropic/groq/deepseek/perplexity/cohere/ollama pass through
    }
}

/// OpenInference `llm.system` (the AI product family). `None` when there's no enum value.
fn openinference_system(raw: &str) -> Option<&str> {
    match raw {
        "openai" => Some("openai"),
        "anthropic" => Some("anthropic"),
        "gemini" | "vertex" => Some("vertexai"),
        "mistral" => Some("mistralai"),
        "cohere" => Some("cohere"),
        "x_ai" | "xai" => Some("xai"),
        "deepseek" => Some("deepseek"),
        _ => None, // ollama / groq / perplexity: no enum value
    }
}

// ---------------------------------------------------------------------------
// Span-kind tagging (emitted at every span constructor)
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
pub(crate) enum Kind {
    Llm,
    Embedding,
    Tool,
    Agent,
    Chain,
    Retriever,
    Reranker,
}

/// Per-backend span-kind alias attributes for `kind`.
pub(crate) fn span_kind(kind: Kind) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::OPENINFERENCE) {
        let v = match kind {
            Kind::Llm => "LLM",
            Kind::Embedding => "EMBEDDING",
            Kind::Tool => "TOOL",
            Kind::Agent => "AGENT",
            Kind::Chain => "CHAIN",
            Kind::Retriever => "RETRIEVER",
            Kind::Reranker => "RERANKER",
        };
        kvs.push(KeyValue::new("openinference.span.kind", v));
    }
    if set.has(CompatSet::TRACELOOP) {
        let v = match kind {
            Kind::Llm | Kind::Embedding => "task",
            Kind::Tool => "tool",
            Kind::Agent => "agent",
            Kind::Chain | Kind::Retriever | Kind::Reranker => "workflow",
        };
        kvs.push(KeyValue::new("traceloop.span.kind", v));
    }
    if set.has(CompatSet::LANGSMITH) {
        let v = match kind {
            Kind::Llm => "llm",
            Kind::Embedding => "embedding",
            Kind::Tool => "tool",
            Kind::Agent | Kind::Chain => "chain",
            Kind::Retriever => "retriever",
            Kind::Reranker => "chain",
        };
        kvs.push(KeyValue::new("langsmith.span.kind", v));
    }
    if set.has(CompatSet::LANGFUSE) {
        let v = match kind {
            Kind::Llm | Kind::Embedding => "generation",
            _ => "span",
        };
        kvs.push(KeyValue::new("langfuse.observation.type", v));
    }
    kvs
}

// ---------------------------------------------------------------------------
// LLM dispatch: provider + model
// ---------------------------------------------------------------------------

/// `op` is the operation ("chat"/"embeddings"); `raw_provider` is the unmapped Sema
/// provider name; `request_model` the resolved request model.
pub(crate) fn llm_dispatch(op: &str, raw_provider: &str, request_model: &str) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || raw_provider.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::OPENINFERENCE) {
        if !request_model.is_empty() {
            kvs.push(KeyValue::new("llm.model_name", request_model.to_string()));
        }
        kvs.push(KeyValue::new(
            "llm.provider",
            openinference_provider(raw_provider).to_string(),
        ));
        if let Some(sys) = openinference_system(raw_provider) {
            kvs.push(KeyValue::new("llm.system", sys.to_string()));
        }
    }
    if set.has(CompatSet::TRACELOOP) {
        kvs.push(KeyValue::new(
            "llm.request.type",
            if op == "embeddings" {
                "embedding"
            } else {
                "chat"
            },
        ));
        if !request_model.is_empty() {
            kvs.push(KeyValue::new(
                "traceloop.entity.name",
                request_model.to_string(),
            ));
        }
    }
    if set.has(CompatSet::LANGSMITH) {
        // LangSmith classifies provider for cost via gen_ai.system (lowercase id).
        kvs.push(KeyValue::new("gen_ai.system", raw_provider.to_string()));
    }
    if set.has(CompatSet::LANGFUSE) && !request_model.is_empty() {
        kvs.push(KeyValue::new(
            "langfuse.observation.model.name",
            request_model.to_string(),
        ));
    }
    kvs
}

// ---------------------------------------------------------------------------
// LLM usage: tokens + cost
// ---------------------------------------------------------------------------

#[allow(clippy::too_many_arguments)]
pub(crate) fn llm_usage(
    input: u32,
    output: u32,
    total: u32,
    cache_read: u32,
    cache_creation: u32,
    cost: Option<f64>,
    cost_prompt: Option<f64>,
    cost_completion: Option<f64>,
) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::OPENINFERENCE) {
        kvs.push(KeyValue::new("llm.token_count.prompt", input as i64));
        kvs.push(KeyValue::new("llm.token_count.completion", output as i64));
        kvs.push(KeyValue::new("llm.token_count.total", total as i64));
        if cache_read > 0 {
            kvs.push(KeyValue::new(
                "llm.token_count.prompt_details.cache_read",
                cache_read as i64,
            ));
        }
        if cache_creation > 0 {
            kvs.push(KeyValue::new(
                "llm.token_count.prompt_details.cache_write",
                cache_creation as i64,
            ));
        }
        if let Some(c) = cost {
            kvs.push(KeyValue::new("llm.cost.total", c));
        }
        // Per-direction split — Phoenix shows prompt/completion cost separately.
        if let Some(c) = cost_prompt {
            kvs.push(KeyValue::new("llm.cost.prompt", c));
        }
        if let Some(c) = cost_completion {
            kvs.push(KeyValue::new("llm.cost.completion", c));
        }
    }
    if set.has(CompatSet::TRACELOOP) {
        kvs.push(KeyValue::new("llm.usage.total_tokens", total as i64));
        kvs.push(KeyValue::new("gen_ai.usage.prompt_tokens", input as i64));
        kvs.push(KeyValue::new(
            "gen_ai.usage.completion_tokens",
            output as i64,
        ));
        if cache_read > 0 {
            kvs.push(KeyValue::new(
                "gen_ai.usage.cache_read_input_tokens",
                cache_read as i64,
            ));
        }
        if cache_creation > 0 {
            kvs.push(KeyValue::new(
                "gen_ai.usage.cache_creation_input_tokens",
                cache_creation as i64,
            ));
        }
    }
    if set.has(CompatSet::LANGFUSE) {
        // Langfuse native usage_details + cost_details JSON objects.
        let usage = serde_json::json!({
            "input": input, "output": output, "total": total,
            "input_cached": cache_read,
        });
        kvs.push(KeyValue::new(
            "langfuse.observation.usage_details",
            usage.to_string(),
        ));
        if let Some(c) = cost {
            kvs.push(KeyValue::new(
                "langfuse.observation.cost_details",
                serde_json::json!({ "total": c }).to_string(),
            ));
        }
    }
    if set.has(CompatSet::BRAINTRUST) {
        if let Some(c) = cost {
            kvs.push(KeyValue::new(
                "braintrust.metrics",
                serde_json::json!({ "cost": c }).to_string(),
            ));
        }
    }
    kvs
}

// ---------------------------------------------------------------------------
// Request parameters (consolidated)
// ---------------------------------------------------------------------------

pub(crate) fn request_params(
    temperature: Option<f64>,
    max_tokens: Option<u32>,
    stop: &[String],
    reasoning_effort: Option<&str>,
) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || !(set.has(CompatSet::OPENINFERENCE) || set.has(CompatSet::LANGFUSE)) {
        return Vec::new();
    }
    let mut obj = serde_json::Map::new();
    if let Some(t) = temperature {
        obj.insert("temperature".into(), serde_json::json!(t));
    }
    if let Some(m) = max_tokens {
        obj.insert("max_tokens".into(), serde_json::json!(m));
    }
    if !stop.is_empty() {
        obj.insert("stop".into(), serde_json::json!(stop));
    }
    if let Some(r) = reasoning_effort {
        obj.insert("reasoning_effort".into(), serde_json::json!(r));
    }
    if obj.is_empty() {
        return Vec::new();
    }
    let json = serde_json::Value::Object(obj).to_string();
    let mut kvs = Vec::new();
    if set.has(CompatSet::OPENINFERENCE) {
        kvs.push(KeyValue::new("llm.invocation_parameters", json.clone()));
    }
    if set.has(CompatSet::LANGFUSE) {
        kvs.push(KeyValue::new("langfuse.observation.model.parameters", json));
    }
    kvs
}

// ---------------------------------------------------------------------------
// I/O content (already gated by capture_content() at the call site)
// ---------------------------------------------------------------------------

/// `input`/`output` are the structured-message JSON blobs Sema already builds.
pub(crate) fn io(input: &str, output: &str) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::OPENINFERENCE) {
        kvs.push(KeyValue::new("input.value", input.to_string()));
        kvs.push(KeyValue::new("output.value", output.to_string()));
        kvs.push(KeyValue::new("input.mime_type", "application/json"));
        kvs.push(KeyValue::new("output.mime_type", "application/json"));
    }
    if set.has(CompatSet::TRACELOOP) {
        kvs.push(KeyValue::new("traceloop.entity.input", input.to_string()));
        kvs.push(KeyValue::new("traceloop.entity.output", output.to_string()));
    }
    if set.has(CompatSet::BRAINTRUST) {
        kvs.push(KeyValue::new("braintrust.input_json", input.to_string()));
        kvs.push(KeyValue::new("braintrust.output_json", output.to_string()));
    }
    kvs
}

/// String-array attribute helper (tags).
fn string_array(key: &'static str, vals: &[String]) -> KeyValue {
    let arr: Vec<StringValue> = vals.iter().map(|s| s.clone().into()).collect();
    KeyValue::new(key, Value::Array(Array::String(arr)))
}

// ---------------------------------------------------------------------------
// Tool execution I/O (on the execute_tool span, content-gated by the caller)
// ---------------------------------------------------------------------------

pub(crate) fn tool_io(args_json: &str, result: &str) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::OPENINFERENCE) {
        // OpenInference has no tool-RESULT key — the result goes in output.value.
        kvs.push(KeyValue::new(
            "tool_call.function.arguments",
            args_json.to_string(),
        ));
        kvs.push(KeyValue::new("input.value", args_json.to_string()));
        kvs.push(KeyValue::new("input.mime_type", "application/json"));
        kvs.push(KeyValue::new("output.value", result.to_string()));
        kvs.push(KeyValue::new("output.mime_type", "text/plain"));
    }
    if set.has(CompatSet::TRACELOOP) {
        kvs.push(KeyValue::new(
            "traceloop.entity.input",
            args_json.to_string(),
        ));
        kvs.push(KeyValue::new("traceloop.entity.output", result.to_string()));
    }
    if set.has(CompatSet::LANGFUSE) {
        kvs.push(KeyValue::new(
            "langfuse.observation.input",
            args_json.to_string(),
        ));
        kvs.push(KeyValue::new(
            "langfuse.observation.output",
            result.to_string(),
        ));
    }
    if set.has(CompatSet::BRAINTRUST) {
        kvs.push(KeyValue::new(
            "braintrust.input_json",
            args_json.to_string(),
        ));
        kvs.push(KeyValue::new("braintrust.output_json", result.to_string()));
    }
    kvs
}

// ---------------------------------------------------------------------------
// Advertised tool schemas (on the chat/LLM span)
// ---------------------------------------------------------------------------

pub(crate) fn tools(views: &[crate::ToolView]) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || views.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    for (i, t) in views.iter().enumerate() {
        if set.has(CompatSet::OPENINFERENCE) {
            kvs.push(KeyValue::new(
                format!("llm.tools.{i}.tool.json_schema"),
                t.json_schema.clone(),
            ));
        }
        if set.has(CompatSet::TRACELOOP) {
            kvs.push(KeyValue::new(
                format!("llm.request.functions.{i}.name"),
                t.name.clone(),
            ));
            kvs.push(KeyValue::new(
                format!("llm.request.functions.{i}.description"),
                t.description.clone(),
            ));
            kvs.push(KeyValue::new(
                format!("llm.request.functions.{i}.parameters"),
                t.json_schema.clone(),
            ));
        }
    }
    kvs
}

// ---------------------------------------------------------------------------
// Trace-level I/O rollup (on the run's ROOT span — agent, or standalone chat)
// ---------------------------------------------------------------------------

pub(crate) fn trace_io(input: &str, output: &str) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::LANGFUSE) {
        kvs.push(KeyValue::new("langfuse.trace.input", input.to_string()));
        kvs.push(KeyValue::new("langfuse.trace.output", output.to_string()));
    }
    kvs
}

// ---------------------------------------------------------------------------
// Tags + metadata
// ---------------------------------------------------------------------------

pub(crate) fn tags(tags: &[String]) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || tags.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::LANGFUSE) {
        kvs.push(string_array("langfuse.trace.tags", tags));
    }
    if set.has(CompatSet::BRAINTRUST) {
        kvs.push(string_array("braintrust.tags", tags));
    }
    if set.has(CompatSet::LANGSMITH) {
        kvs.push(KeyValue::new("langsmith.span.tags", tags.join(",")));
    }
    kvs
}

pub(crate) fn metadata(meta: &[(String, String)]) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || meta.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    for (k, v) in meta {
        if set.has(CompatSet::LANGFUSE) {
            kvs.push(KeyValue::new(
                format!("langfuse.trace.metadata.{k}"),
                v.clone(),
            ));
        }
        if set.has(CompatSet::LANGSMITH) {
            kvs.push(KeyValue::new(format!("langsmith.metadata.{k}"), v.clone()));
        }
        if set.has(CompatSet::TRACELOOP) {
            kvs.push(KeyValue::new(
                format!("traceloop.association.properties.{k}"),
                v.clone(),
            ));
        }
    }
    if set.has(CompatSet::BRAINTRUST) {
        let obj: serde_json::Map<String, serde_json::Value> = meta
            .iter()
            .map(|(k, v)| (k.clone(), serde_json::Value::String(v.clone())))
            .collect();
        kvs.push(KeyValue::new(
            "braintrust.metadata",
            serde_json::Value::Object(obj).to_string(),
        ));
    }
    kvs
}

// ---------------------------------------------------------------------------
// Retriever + reranker spans (OpenInference RETRIEVER / RERANKER)
// ---------------------------------------------------------------------------

/// Cap on indexed document attributes per span, to bound a large candidate set.
const MAX_DOCS: usize = 64;

/// Reranker request metadata: `reranker.query` (content-gated), `reranker.model_name`,
/// `reranker.top_k`. OpenInference-only.
pub(crate) fn reranker_meta(
    query: &str,
    model: &str,
    top_k: Option<usize>,
    capture: bool,
) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || !set.has(CompatSet::OPENINFERENCE) {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if !model.is_empty() {
        kvs.push(KeyValue::new("reranker.model_name", model.to_string()));
    }
    if let Some(k) = top_k {
        kvs.push(KeyValue::new("reranker.top_k", k as i64));
    }
    if capture {
        kvs.push(KeyValue::new("reranker.query", query.to_string()));
    }
    kvs
}

/// Reranker input/output documents flattened under `field`
/// (`input_documents` / `output_documents`). Content is gated; scores always emit.
pub(crate) fn reranker_documents(
    field: &str,
    docs: &[(String, Option<f64>)],
    capture: bool,
) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || !set.has(CompatSet::OPENINFERENCE) {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    for (i, (content, score)) in docs.iter().take(MAX_DOCS).enumerate() {
        if capture {
            kvs.push(KeyValue::new(
                format!("reranker.{field}.{i}.document.content"),
                content.clone(),
            ));
        }
        if let Some(s) = score {
            kvs.push(KeyValue::new(
                format!("reranker.{field}.{i}.document.score"),
                *s,
            ));
        }
    }
    kvs
}

/// Retrieval result documents: `retrieval.documents.{i}.document.{id,score}` always,
/// `.content` gated. OpenInference-only.
pub(crate) fn retrieval_documents(docs: &[(String, String, f64)], capture: bool) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || !set.has(CompatSet::OPENINFERENCE) {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    for (i, (id, content, score)) in docs.iter().take(MAX_DOCS).enumerate() {
        kvs.push(KeyValue::new(
            format!("retrieval.documents.{i}.document.id"),
            id.clone(),
        ));
        kvs.push(KeyValue::new(
            format!("retrieval.documents.{i}.document.score"),
            *score,
        ));
        if capture {
            kvs.push(KeyValue::new(
                format!("retrieval.documents.{i}.document.content"),
                content.clone(),
            ));
        }
    }
    kvs
}

// ---------------------------------------------------------------------------
// Embedding detail (on the embeddings span)
// ---------------------------------------------------------------------------

/// Max number of indexed input-text attributes emitted for one embed call, to bound a
/// large batch's attribute count. Raw vectors are never emitted (both backends gate them).
const MAX_EMBED_TEXTS: usize = 128;

/// OpenInference `embedding.model_name`. No-op unless OpenInference is active.
pub(crate) fn embedding_model(model: &str) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || model.is_empty() || !set.has(CompatSet::OPENINFERENCE) {
        return Vec::new();
    }
    vec![KeyValue::new("embedding.model_name", model.to_string())]
}

/// OpenInference per-text `embedding.embeddings.{i}.embedding.text`. Content-gated by the
/// caller (`capture`); capped at `MAX_EMBED_TEXTS`.
pub(crate) fn embedding_texts(texts: &[String], capture: bool) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() || !capture || !set.has(CompatSet::OPENINFERENCE) {
        return Vec::new();
    }
    texts
        .iter()
        .take(MAX_EMBED_TEXTS)
        .enumerate()
        .map(|(i, t)| {
            KeyValue::new(
                format!("embedding.embeddings.{i}.embedding.text"),
                t.clone(),
            )
        })
        .collect()
}

// ---------------------------------------------------------------------------
// Streaming: time-to-first-token (on the LLM span)
// ---------------------------------------------------------------------------

/// `completion_start` is the absolute first-token timestamp (RFC3339). The TTFT *duration*
/// itself is always emitted vendor-neutrally as `sema.gen_ai.server.time_to_first_token`
/// by the caller; here we add only the backend-native keys.
pub(crate) fn streaming(completion_start: &str) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::LANGFUSE) {
        // Langfuse computes its TTFT column from this absolute first-token time.
        kvs.push(KeyValue::new(
            "langfuse.observation.completion_start_time",
            completion_start.to_string(),
        ));
    }
    if set.has(CompatSet::TRACELOOP) {
        // OpenLLMetry's real streaming span attribute is the boolean `gen_ai.is_streaming`
        // (its streaming *duration* lives in a histogram metric, not a span attribute, so
        // there is no per-span TTFT key to populate here).
        kvs.push(KeyValue::new("gen_ai.is_streaming", true));
    }
    kvs
}

// ---------------------------------------------------------------------------
// Trace identity: session + release (applied to every span in `start()`)
// ---------------------------------------------------------------------------

/// LangSmith reads its OWN session key (it ignores `session.id` /
/// `gen_ai.conversation.id`); Langfuse maps `langfuse.release`. Both no-op unless their
/// backend is active.
pub(crate) fn identity(session: Option<&str>, release: Option<&str>) -> Vec<KeyValue> {
    let set = active();
    if set.is_empty() {
        return Vec::new();
    }
    let mut kvs = Vec::new();
    if set.has(CompatSet::LANGSMITH) {
        if let Some(s) = session {
            kvs.push(KeyValue::new("langsmith.trace.session_id", s.to_string()));
        }
    }
    if set.has(CompatSet::LANGFUSE) {
        if let Some(r) = release {
            kvs.push(KeyValue::new("langfuse.release", r.to_string()));
        }
    }
    kvs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_tokens_case_and_aliases() {
        assert_eq!(parse(""), 0);
        assert_eq!(parse("openinference"), CompatSet::OPENINFERENCE);
        assert_eq!(parse("Phoenix"), CompatSet::OPENINFERENCE);
        assert_eq!(
            parse("arize, langsmith"),
            CompatSet::OPENINFERENCE | CompatSet::LANGSMITH
        );
        assert_eq!(parse("openllmetry"), CompatSet::TRACELOOP);
        assert_eq!(parse("all"), CompatSet::ALL);
        assert_eq!(parse("bogus"), 0); // unknown ignored
    }

    #[test]
    fn provider_back_translation() {
        assert_eq!(openinference_provider("gemini"), "google");
        assert_eq!(openinference_provider("mistral"), "mistralai");
        assert_eq!(openinference_provider("ollama"), "ollama");
        assert_eq!(openinference_system("gemini"), Some("vertexai"));
        assert_eq!(openinference_system("ollama"), None);
    }

    fn has(kvs: &[KeyValue], key: &str) -> bool {
        kvs.iter().any(|kv| kv.key.as_str() == key)
    }

    #[test]
    fn streaming_and_identity_gate_on_active_backend() {
        set_test_override("langfuse");
        let s = streaming("2026-06-22T00:00:00.000Z");
        assert!(has(&s, "langfuse.observation.completion_start_time"));
        assert!(!has(&s, "gen_ai.is_streaming"));
        let id = identity(Some("sess-1"), Some("v1.0"));
        assert!(has(&id, "langfuse.release"));
        assert!(!has(&id, "langsmith.trace.session_id"));

        set_test_override("traceloop,langsmith");
        // OpenLLMetry's real streaming span attribute is the bool gen_ai.is_streaming.
        assert!(has(&streaming("t"), "gen_ai.is_streaming"));
        let id = identity(Some("sess-1"), None);
        assert!(has(&id, "langsmith.trace.session_id"));
        assert!(!has(&id, "langfuse.release")); // release None -> not emitted

        // Nothing active -> empty.
        set_test_override("");
        assert!(streaming("t").is_empty());
        assert!(identity(Some("s"), Some("r")).is_empty());

        // Restore the "unset" sentinel so sibling tests read the env path.
        TEST_OVERRIDE.store(0xFF, Ordering::Relaxed);
    }
}
