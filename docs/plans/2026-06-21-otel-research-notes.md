# OTel Observability — Research Notes (Sourced Evidence)

Companion to `docs/plans/2026-06-21-otel-observability.md`. The plan stays lean and
prescriptive; this doc preserves the SOURCED evidence behind every load-bearing
decision so future implementers can re-verify rather than re-research.

> **Volatility warning.** Every `gen_ai.*` attribute and every `opentelemetry-*`
> crate API below is pre-1.0 / Development-stability and has already churned
> (e.g. `gen_ai.system` → `gen_ai.provider.name`, `prompt_tokens` →
> `input_tokens`, `export(&mut self)` → `export(&self)`). Pin versions, isolate
> the mapping in one module, and re-verify on every bump. Codebase `file:line`
> anchors below were verified against workspace **1.21.1**; `builtins.rs` is now
> 5244 lines — anchor on **function names**, not line numbers, when they drift.

---

## 1. Rust OTel crate landscape (June 2026)

### 1.1 Versions — the synchronized 0.32 train

All published May 2026 as one release train; **bump them together, never mix
major-minor across the family**.

| Crate | Version | Role |
|---|---|---|
| `opentelemetry` | 0.32.0 | API crate (MSRV 1.75.0) |
| `opentelemetry_sdk` | 0.32.1 | SDK — carries `BatchSpanProcessor` + `SpanExporter` trait (patch ahead of API) |
| `opentelemetry-otlp` | 0.32.0 | OTLP exporter |
| `opentelemetry-semantic-conventions` | 0.32.0 | `gen_ai.*` constants (released 2026-05-08) |
| `opentelemetry-stdout` | 0.32 train | Debug-only stdout exporter (format **intentionally unspecified**) |

Sources:
- <https://docs.rs/opentelemetry/latest/opentelemetry/>
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/>
- <https://docs.rs/opentelemetry-otlp/latest/opentelemetry_otlp/>
- <https://docs.rs/crate/opentelemetry-semantic-conventions/latest>

**API churn to know:** 0.28 was the big rewrite. Provider types renamed to
`SdkTracerProvider` / `SdkMeterProvider` (old `TracerProvider` global-init shapes
gone). The `SpanExporter` trait (0.32) dropped `async-trait`:

```rust
fn export(&self, batch: Vec<SpanData>) -> impl Future<Output = OTelSdkResult> + Send;
// + shutdown(&self), shutdown_with_timeout(&self, Duration),
//   force_flush(&self), set_resource(&mut self, &Resource)
```

`export` now takes `&self` (not `&mut`) with an owned `Vec<SpanData>`, returns
`OTelSdkResult` (`Result<(), OTelSdkError>` — variants `InternalFailure`,
`Timeout`, `AlreadyShutdown`), and the implementor must be `Send + Sync + Debug`.
**The `impl Future` return makes the trait NOT dyn-compatible** — you cannot
`Box<dyn SpanExporter>`; build the provider inside the concrete arm.
Pre-0.30 tutorials (e.g. the oneuptime article using
`export(&mut self) -> BoxFuture<ExportResult>`) **will not compile** against 0.32.

Sources:
- <https://github.com/open-telemetry/opentelemetry-rust/blob/main/docs/migration_0.28.md>
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/trace/trait.SpanExporter.html>
- <https://oneuptime.com/blog/post/2026-02-06-custom-opentelemetry-span-exporter-rust/view> (OBSOLETE API — counter-example)

### 1.2 Both-transport feature selection

`opentelemetry-otlp` **default features**: `http-proto`, `internal-logs`, `logs`,
`metrics`, `reqwest-blocking-client`, `trace`. Out of the box you get
HTTP/protobuf over blocking reqwest (no tonic).

| Transport | Feature | Port | Weight |
|---|---|---|---|
| HTTP/protobuf | `http-proto` (+ `reqwest-blocking-client`) | 4318 | light (reqwest + http + prost) |
| HTTP/JSON | `http-json` | 4318 | light |
| gRPC | `grpc-tonic` | 4317 | heavy (tonic + prost + tower + hyper + h2 + tokio) |

Removed transports: **surf** (HTTP) and **grpcio** are gone; tonic is the only
gRPC path, reqwest the only HTTP path.

**Env-var selection is a half-truth.** `OTEL_EXPORTER_OTLP_PROTOCOL` accepts
`grpc | http/protobuf | http/json` (signal-specific
`OTEL_EXPORTER_OTLP_TRACES_PROTOCOL` / `_METRICS_PROTOCOL` override), and the
exporter natively honors `OTEL_EXPORTER_OTLP_{ENDPOINT,HEADERS,TIMEOUT}` with
precedence `programmatic > signal-specific > generic > defaults`. **But the env
var only selects among transports compiled in.** Shipping only `http-proto` and
setting the var to `grpc` silently no-ops. To be truly runtime-selectable you
must compile **both** features AND branch in code:

```rust
// pseudo: read OTEL_EXPORTER_OTLP_PROTOCOL, dispatch builder arm
"grpc"            => SpanExporter::builder().with_tonic()...                 // built INSIDE a tokio context
"http/json"       => SpanExporter::builder().with_http().with_protocol(Protocol::HttpJson)...
_ /* default */   => SpanExporter::builder().with_http().with_protocol(Protocol::HttpBinary)...
```

Build the full `SdkTracerProvider` inside each arm (exporter types differ; trait
not dyn-safe). Leave endpoint/headers/timeout to the env vars unless Sema config
overrides. Recommend gating `grpc-tonic` behind a cargo feature so default builds
stay HTTP-light.

Sources:
- <https://lib.rs/crates/opentelemetry-otlp/features>
- <https://opentelemetry.io/docs/languages/sdk-configuration/otlp-exporter/>
- <https://opentelemetry.io/docs/specs/otel/protocol/exporter/>

### 1.3 Batch / flush model + non-fatal export

Since 0.28+ the default `BatchSpanProcessor` (and `PeriodicReader`) run on a
**dedicated background OS thread** and need **no async runtime** with the
`reqwest-blocking-client` HTTP transport. The runtime-coupled processor is now an
experimental opt-in (`experimental_trace_batch_span_processor_with_async_runtime`).
The hot path only does a non-blocking enqueue onto a bounded channel.

**Collector-down behavior (the fail-safe guarantee):** when the collector is
unreachable, exports fail on the background thread and are **not** propagated to
the caller; once `max_queue_size` is exceeded spans are **DROPPED, not blocked**,
with one throttled warning and a final dropped-count at shutdown. Per spec,
`shutdown`/`force_flush` are timeout-bounded and never block forever. This means
the steady state already satisfies "down collector never blocks the script."

Tunables (defaults / env): `max_queue_size` 2048 (`OTEL_BSP_MAX_QUEUE_SIZE`),
`max_export_batch_size` 512 (`OTEL_BSP_MAX_EXPORT_BATCH_SIZE`), `scheduled_delay`
5000ms (`OTEL_BSP_SCHEDULE_DELAY`), `max_concurrent_exports` 1, export timeout
10000ms (`OTEL_EXPORTER_OTLP_TIMEOUT` / `OTEL_BSP_EXPORT_TIMEOUT`). **Recommend a
short export timeout (~3s)** so a dead endpoint releases the slot quickly,
especially at shutdown.

**Sharp edges:**
- `global::set_error_handler` / `handle_error` were **REMOVED** in modern
  opentelemetry-rust (replaced by per-operation `OTelSdkError`). You cannot
  register a global swallow-callback. Achieve silent-by-default by **not
  subscribing the `opentelemetry` tracing targets** (`internal-logs` feature,
  `otel_error!`) in Sema's subscriber.
- `shutdown()` is blocking and must **NOT** be called from a current-thread tokio
  runtime's own thread (deadlock; historical force_flush deadlocks #1395/#364).
  Call it from a separate OS thread / `spawn_blocking`.
- Use `BatchSpanProcessor`, never `SimpleSpanProcessor`, for the OTLP path —
  Simple exports synchronously on span-end and would block on a dead collector.

Exit ordering: `let _ = provider.force_flush();` then
`let _ = provider.shutdown_with_timeout(Duration::from_secs(3));` — discard
Results; not from a current-thread runtime's own thread.

Sources:
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/trace/struct.BatchSpanProcessor.html>
- <https://github.com/open-telemetry/opentelemetry-rust/blob/main/opentelemetry-sdk/src/trace/span_processor.rs>
- <https://opentelemetry.io/docs/specs/otel/trace/sdk/>
- <https://opentelemetry.io/docs/specs/otel/error-handling/>
- <https://github.com/open-telemetry/opentelemetry-rust/issues/1395>, <.../issues/364>, <.../issues/2571>

### 1.4 File-exporter approach (`SEMA_OTEL_FILE` / JSONL)

Two routes. (a) Built-in `opentelemetry-stdout` via `.with_simple_exporter(...)`
— but its format is explicitly unspecified/debug-only, **not** a stable JSONL
contract. (b) **Implement the `SpanExporter` trait yourself** (recommended for a
stable opt-in fallback):

- Hold a `std::sync::Mutex<std::io::BufWriter<std::fs::File>>` opened with
  `OpenOptions::new().create(true).append(true)`.
- In `export(&self, batch)`, serialize each `SpanData`
  (`opentelemetry_sdk::trace::SpanData`) to a Sema-defined serde struct
  (name, trace_id, span_id, parent, start/end, attributes, status), write one
  JSON object per line, return `std::future::ready(Ok(()))` (file writes are
  sync — no real await needed).
- Register via `.with_simple_exporter()` for deterministic immediate capture, or
  `.with_batch_exporter()` to keep I/O off the hot path. Honor the contract:
  `export` is never called concurrently for the same instance; must not block
  indefinitely; after shutdown it must error. `force_flush`/`shutdown` flush the
  `BufWriter`.

This is the clean way to capture spans with **no running collector**. Make
`SEMA_OTEL_FILE` independent and individually opt-in from the OTLP path; when
neither is set, install **no** tracer provider at all (true zero-cost no-op —
the default `NoopTracerProvider` does nothing). The "send_to_logfire=False +
OTLP→otel-tui" pattern (Logfire) is the same idea.

Sources:
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/trace/trait.SpanExporter.html>
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/trace/struct.SdkTracerProvider.html>
- <https://opentelemetry.io/docs/languages/rust/exporters/>

---

## 2. GenAI semantic conventions (current state)

**Status & source-of-truth shift.** The GenAI semconv is still **Development**
(experimental); latest is **v1.41.x**, transition baseline **v1.36**. The
canonical docs **MOVED** to a dedicated repo,
[`open-telemetry/semantic-conventions-genai`](https://github.com/open-telemetry/semantic-conventions-genai).
The old `opentelemetry.io/docs/specs/semconv/gen-ai/*` pages are now relocation
**stubs** — their "Deprecated/moved" badges are about the **page relocation, not
the attributes**. Pin the dedicated repo (and the
`opentelemetry-semantic-conventions` crate version that tracks it, with the
`semconv_experimental` feature) as source of truth. Gate newer behavior with
`OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental` (default keeps
≤1.36.0 emission).

### 2.1 Span names + kinds

| Span | Kind | Name format |
|---|---|---|
| LLM call | CLIENT | `{gen_ai.operation.name} {gen_ai.request.model}` — e.g. `chat gpt-4o`, `chat claude-sonnet-4-6` |
| Tool exec | INTERNAL | `execute_tool {gen_ai.tool.name}` — v1.41 **requires** tool name in span name |
| Agent | INTERNAL (Sema) | `invoke_agent {gen_ai.agent.name}` (bare `invoke_agent` if name absent) |
| Workflow | INTERNAL | `invoke_workflow {id}` — **new in v1.41** |

`gen_ai.operation.name` well-known values: `chat`, `text_completion`,
`embeddings`, `generate_content`, `create_agent`, `invoke_agent`,
`invoke_workflow`, `execute_tool`, `retrieval`, `create_memory`, `search_memory`.

**Agent span-kind rationale:** spec says `invoke_agent` SHOULD be **CLIENT for a
remote agent service**, **INTERNAL for a local/in-process framework agent**. Sema
agents run in-process → **INTERNAL is correct** — cite the local/remote rule as
justification. Canonical agent trace tree (OTel blog):
`invoke_agent (INTERNAL) → [chat (CLIENT), execute_tool (INTERNAL), chat, execute_tool, chat]`.

### 2.2 Attribute strings

LLM span (all current/correct):
`gen_ai.operation.name`, `gen_ai.provider.name`, `gen_ai.request.model`,
`gen_ai.response.model`, `gen_ai.request.max_tokens` (int),
`gen_ai.request.temperature` (double), `gen_ai.request.top_p` (double),
`gen_ai.usage.input_tokens` (int), `gen_ai.usage.output_tokens` (int),
`gen_ai.response.finish_reasons` (**string[]** — wrap Sema's single
`stop_reason` in a 1-element array), `gen_ai.response.id`,
`gen_ai.conversation.id` (session correlation — recommend adding).

**Renames to heed:**
- `gen_ai.system` → **`gen_ai.provider.name`** (v1.37).
- `gen_ai.usage.prompt_tokens` / `completion_tokens` → **`input_tokens` /
  `output_tokens`** (pre-1.36 → 1.36+).

Provider well-known values: `openai`, `anthropic`, `aws.bedrock`,
`azure.ai.openai`, `gcp.vertex_ai`, `gcp.gemini`, `cohere`, `mistral_ai`, `groq`,
`deepseek`, `perplexity`, `x_ai`, `ibm.watsonx.ai`. **NOTE: `gemini` is NOT
valid → use `gcp.gemini`** (and `gcp.vertex_ai` for Vertex). Suggested
Sema-provider → `gen_ai.provider.name` mapping: `anthropic→anthropic`,
`openai→openai`, `gemini→gcp.gemini`, `vertex→gcp.vertex_ai`, `ollama→ollama`
(no standard value; keep raw as a known non-standard).

Anthropic prompt-caching (additive sub-attributes, **INCLUDED IN**
`gen_ai.usage.input_tokens`, not summed on top):
`gen_ai.usage.cache_creation.input_tokens`,
`gen_ai.usage.cache_read.input_tokens`. (See §4 — Sema's `Usage`/`AnthropicUsage`
currently DROP these; populating them needs a provider+type change.)

Tool span: `gen_ai.tool.name`, `gen_ai.tool.call.id`, `gen_ai.tool.type`
(values `function` | `extension` | `datastore` — Sema's client-executed tools =
**`function`**), `gen_ai.tool.description` (cheap to add), `error.type` on
failure.

### 2.3 Metrics (the two required client histograms)

| Metric | Type | Unit | Dimensions | Recommended buckets |
|---|---|---|---|---|
| `gen_ai.client.token.usage` | Histogram | `{token}` | `gen_ai.token.type` (`input`/`output`) + operation.name + provider.name + request.model + response.model | `[1,4,16,64,256,1024,4096,16384,65536,262144,1048576,4194304,16777216,67108864]` |
| `gen_ai.client.operation.duration` | Histogram | `s` (seconds) | operation.name + provider.name + request.model (+ error.type on failure) | `[0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,5.12,10.24,20.48,40.96,81.92]` |

Spec rule: when both used and **billable** tokens are available, report billable.
Sema measures `duration_ms` today (tool calls only — see §4) → divide by 1000 for
the seconds metric. Re-verify the metrics API shape against pinned 0.32 (metrics
API churns).

### 2.4 Content capture + privacy

Capture is **OFF by default** (correct). The **mechanism changed** in v1.37:
content is now captured via a structured **EVENT** named
`gen_ai.client.inference.operation.details` carrying `gen_ai.input.messages`,
`gen_ai.output.messages`, `gen_ai.system_instructions`. Putting those on the span
as serialized JSON is an **explicitly-allowed FALLBACK mode** "where backends
lack structured attribute support." Three modes: (1) not recorded [default],
(2) span attributes [size-limited, the MVP fallback], (3) external storage +
reference URL [recommended for prod]. The older `gen_ai.prompt` /
`gen_ai.completion` attributes are **deprecated**.

**Standard env flag:** `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true`
(honor this; a `SEMA_*` alias is optional). Truncate large content to bound span
size; messages follow a documented Input/Output messages JSON schema.

### 2.5 Cost-attribute situation

**There is NO standard cost attribute** in the OTel GenAI spec — it standardizes
tokens only and expects backends to derive dollars. De-facto community attribute
is **`gen_ai.usage.cost_usd`** (emitted as both a span attribute and a histogram),
computed at emit time from a self-owned pricing table. Peer backends differ:
- **Langfuse**: SDK-native `cost_details` structure (keys
  `input`/`output`/`cache_read_input_tokens`/`total`) or auto-infers from model.
- **Datadog**: `total_cost` (manual, stored as **nanodollars**) or auto-estimates
  from tokens.
- **Pydantic Logfire**: `operation.cost` (auto-rolls-up parent spans), computed
  from the open `genai-prices` dataset.

**Recommendation:** emit the bare `gen_ai.usage.cost_usd` (drop any `sema.`
prefix — `gen_ai.*`-aware backends are more likely to surface it; a prefixed
custom attr is invisible to token-only backends) AND always emit accurate
`gen_ai.usage.input_tokens`/`output_tokens` so any backend can cross-check or
recompute.

Sources (§2):
- <https://github.com/open-telemetry/semantic-conventions-genai> (canonical)
- <https://opentelemetry.io/docs/specs/semconv/registry/attributes/gen-ai/> (strings + enums)
- <https://opentelemetry.io/blog/2026/genai-observability/> (span tree, metrics, content)
- <https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/> (relocation stub)
- <https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-agent-spans/> (invoke_agent kind)
- <https://greptime.com/blogs/2026-05-09-opentelemetry-genai-semantic-conventions> (v1.37 event capture, system→provider.name, v1.41)
- <https://hexdocs.pm/opentelemetry_semantic_conventions/gen-ai-metrics.html> (units/buckets)
- <https://langfuse.com/docs/observability/features/token-and-cost-tracking>
- <https://docs.datadoghq.com/llm_observability/monitoring/cost/>
- <https://www.datadoghq.com/blog/llm-otel-semantic-convention/>

---

## 3. Peer-framework comparison

The ecosystem is converging on `gen_ai.*` as the **wire baseline**, with each
layer adding richer typing on top. Best fail-safe patterns to copy: (a) the
`BatchSpanProcessor` background-thread / bounded-queue / drop-on-full model, and
(b) **content capture as a separate explicit opt-in from tracing-enabled**,
default OFF.

| Framework | Span model | Graceful-degradation pattern |
|---|---|---|
| **OpenLLMetry / Traceloop** | OTel `gen_ai.*` (leads the WG); migrating `gen_ai.prompt/completion` → `input.messages/output.messages` | Content **ON by default** (do NOT copy); `TRACELOOP_TRACE_CONTENT=false` global + per-workflow/per-user override. Async `BatchSpanProcessor`; `disable_batch=True` = synchronous, **local-dev/debug only** |
| **Langfuse** | OTel backend; OTLP at `/api/public/otel`; span→observation, specialized "generation" observation with model/params/`usage_details`/`cost_details` | Hierarchy from OTel context propagation. **Trace-level attrs (user_id/session_id/metadata/tags) must be propagated to EVERY span** (`propagate_attributes()`) or filtering breaks. Default ingestion filters to LLM-relevant spans (drops the rest). Prototype-pollution guard: drops attr keys containing `__proto__`/`constructor`/`prototype` |
| **Mastra** | New "AI Tracing" follows OTel GenAI SemConv v1.38: `chat {model}`, `execute_tool {tool}`, `invoke_agent {id}`, `invoke_workflow {id}` | Transport selectable `http/json`/`http/protobuf`/`grpc`; `BatchSpanProcessor` (batchSize 100, timeout 30000ms). `enabled:false` or sampled-out → returned traceId is **undefined** (clean no-op). "OTEL Bridge" (join host OTel) vs "Exporter" (standalone) split |
| **Vercel AI SDK** | `ai.generateText`/`ai.streamText`/`ai.embed`/`ai.toolCall`; **dual-emits** native `ai.*` AND OTel `gen_ai.*` | **OFF by default** (`experimental_telemetry.isEnabled`); `recordInputs`/`recordOutputs` (both default true) gate content **independently** of enablement. **"Errors inside integrations are caught and do not break the generation flow"** |
| **OpenInference / Phoenix** | **Required `openinference.span.kind` enum**: LLM/EMBEDDING/CHAIN/RETRIEVER/RERANKER/TOOL/AGENT/GUARDRAIL/EVALUATOR/PROMPT; attrs in `llm.*`/`openinference.*` (e.g. `llm.model_name`, `llm.token_count.prompt`) — **diverges from `gen_ai.*`** | Privacy via `TraceConfig` + `OPENINFERENCE_HIDE_INPUTS`/`OPENINFERENCE_HIDE_OUTPUTS` (+ more `HIDE_*`). Transport-agnostic OTLP |
| **Pydantic Logfire** | Thin OTel wrapper; spans for agent run + each model request + each tool call | **Best no-op story**: if package not installed/configured "virtually no overhead and nothing is sent". `send_to_logfire=False` + `OTEL_EXPORTER_OTLP_ENDPOINT` → any collector (e.g. otel-tui). Scrubs sensitive data before export. Cost via `operation.cost` (auto-rolls-up parents) |

**Takeaways for Sema:** emit `gen_ai.*` as the native baseline (most portable,
zero custom mapping for Langfuse/Phoenix/Logfire/Jaeger/Grafana). Keep
"tracing enabled" and "content capture" as **two independent default-OFF
switches** (Vercel's `recordInputs/Outputs` + OpenInference `HIDE_*`; do NOT copy
Traceloop's content-on-by-default). Build hierarchy from a thread-local "current
span" stack (trivial in single-threaded Sema). Propagate
`gen_ai.conversation.id` to every child span (Langfuse requirement). Fail-closed
at init like Logfire. Two small guards worth stealing: drop attr keys containing
`__proto__`/`constructor`/`prototype`, and bound captured-content size.

Sources (§3):
- <https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/>
- <https://github.com/open-telemetry/semantic-conventions-genai/blob/main/docs/gen-ai/gen-ai-spans.md>
- <https://www.traceloop.com/docs/openllmetry/privacy/traces>, <https://github.com/traceloop/openllmetry/issues/3515>
- <https://langfuse.com/integrations/native/opentelemetry>, <https://langfuse.com/docs/observability/sdk/overview>
- <https://mastra.ai/docs/observability/tracing/exporters/otel>, <https://mastra.ai/reference/observability/otel-tracing/otel-config>
- <https://ai-sdk.dev/docs/ai-sdk-core/telemetry>
- <https://arize-ai.github.io/openinference/spec/semantic_conventions.html>, <https://github.com/Arize-ai/openinference>
- <https://logfire.pydantic.dev/docs/ai-observability/>, <https://logfire.pydantic.dev/docs/reference/advanced/metrics-in-spans/>

---

## 4. Data-availability map (Sema → GenAI attributes, with `file:line`)

All `file:line` verified against workspace **1.21.1** (`crates/sema-llm/src/builtins.rs`
unless noted). **Anchor on function names** — line numbers drift.

### 4.1 The LLM-call dispatch chain (where the LLM span lives)

There is **NOT one single function** — dispatch is a 5-function chain:

| Function | Line | Role |
|---|---|---|
| `do_complete` | builtins.rs:4137 | TOP wrapper; cache-hit early-return (zero usage, `stop_reason="cache_hit"`) at 4158–4179; resets serving provider at ~4140 |
| `do_complete_inner` | builtins.rs:4210 | fallback fan-out over `FALLBACK_CHAIN` |
| `complete_with_retry` | builtins.rs:4290 | THE retry loop (`loop { provider.complete(request.clone()) }` with backoff) |
| `do_complete_with_provider` | builtins.rs:4313 | per-fallback-entry; calls `set_serving_provider` at 4334 |
| `do_complete_uncached` | builtins.rs:4340 | no-chain path; calls `set_serving_provider` at 4349 |

**Right span site (skeptical conclusion):** capture provider name + `ChatResponse`
together **inside `do_complete_uncached` (4340) / `do_complete_with_provider`
(4313)** where both the resolved provider name and `resp` are in scope, **before
`track_usage` runs**. Do NOT read the serving provider at `do_complete` level — see
the consume race below.

### 4.2 Data in scope per attribute

| GenAI attribute | Source | `file:line` | Caveat |
|---|---|---|---|
| `gen_ai.provider.name` | `LAST_SERVING_PROVIDER` / `provider.name()` | `set_serving_provider` 92, `take` 96, set at 4334/4349 | **Consumed+cleared by `track_usage` (223) AND `take_serving_provider` (96) via `.take()`** — read inside the dispatch fn, not via peek at `do_complete` |
| `gen_ai.request.model` | `ChatRequest.model` | types.rs:147 (struct) | **EMPTY at `do_complete` entry** for unpinned models; default substituted at 4328/4344 — prefer `response.model` |
| `gen_ai.response.model` | `ChatResponse.model` | types.rs:181 (struct) | authoritative model id |
| `gen_ai.request.max_tokens` | `ChatRequest.max_tokens` (Option<u32>) | types.rs:147 | — |
| `gen_ai.request.temperature` | `ChatRequest.temperature` (Option<f64>) | types.rs:147 | often None |
| `gen_ai.request.stop_sequences` | `ChatRequest.stop_sequences` | types.rs:147 | — |
| `gen_ai.usage.input_tokens` | `Usage.prompt_tokens` | types.rs:191 | **0 on cache hit** (4172–4176, by design) |
| `gen_ai.usage.output_tokens` | `Usage.completion_tokens` | types.rs:191 | **0 on cache hit** |
| `gen_ai.response.finish_reasons` | `ChatResponse.stop_reason` | types.rs:181 | single string → wrap in 1-element array |
| `gen_ai.tool.name` / `.call.id` | `ToolCall.name` / `.id` | types.rs:140 | tool span |
| cache_hit (non-standard) | `stop_reason == "cache_hit"` | 4177; counters `CACHE_HITS`/`CACHE_MISSES` 78–79 | tag e.g. `gen_ai.cache.hit=true`, zero usage |
| `gen_ai.usage.cost_usd` (non-standard) | `pricing::calculate_cost_for(provider, usage)` | track_usage 220–224; pricing.rs:152–165 | Option (None if model not in snapshot); **never stored per-call** (only accumulated into SESSION_COST 255) — span must capture the return |
| reasoning_effort (non-standard) | `ChatRequest.reasoning_effort` | types.rs:147 | minimal/low/medium/high/none/xhigh |

### 4.3 CANNOT populate (aspirational — flag in plan)

| GenAI attribute | Why |
|---|---|
| `gen_ai.request.top_p` / `top_k` / `frequency_penalty` / `presence_penalty` / `seed` | **No such fields** on `ChatRequest` (types.rs:147) |
| `gen_ai.usage.cache_creation.input_tokens` / `cache_read.input_tokens` | **DROPPED**: `AnthropicUsage` (anthropic.rs:378–382) deserializes only `input_tokens`/`output_tokens`; `Usage` (types.rs:191) has no cache fields. Needs provider+type change |
| reasoning/thinking token counts | Not captured (`Usage` has only prompt/completion) |
| LLM-call latency / server duration | **No existing source** — only TOOL calls are `Instant`-timed (start 4580, `duration_ms` 4594). An LLM span must measure its own wall-clock |
| retry/attempt count | Measured in `complete_with_retry` (local `attempt`, 4290) but **DISCARDED** — not retrievable without a code change |
| `gen_ai.response.id` | `ChatResponse` has no id field (types.rs:181); anthropic response id not deserialized (anthropic.rs:353) |
| streaming usage/spans | `llm/stream` bypasses `do_complete` + `track_usage` entirely (calls `provider.stream_complete` directly at ~1565/1579) — a `do_complete` span misses streaming; streaming usage never tracked |
| per-call cost (stored) | Recomputed each time; only accumulated into SESSION_COST (255) |

### 4.4 Tool span — richest real data (`run_tool_loop` 4516, `execute_tool_call` 4640)

Per `ToolCall tc`: name (`tc.name`), args (`tc.arguments` → `json_to_value`),
call id (`tc.id`), result (String, full available; callback truncates to 200),
`is_error` (bool, 4584–4593), **`duration_ms` — REAL `Instant`-measured (4580/4594,
the one place latency exists)**, round index (`_round` 4537), `consecutive_errors`
(4535). All already surfaced to the `:on-tool-call` callback as a map (4571–4614)
with `{:event :tool :args :result :error :duration-ms}` — mirror for span attrs.

### 4.5 Agent span (`agent/run` → `run_tool_loop`)

`agent/run` calls `run_tool_loop` at 2168; `llm/complete`-with-tools at 1468.
Available: agent model/system/tool names/max_turns, input user message, prior
history, final result + final_messages. **No aggregate token/cost computed at
agent level** — only per-call `track_usage` accumulates into session globals; an
agent-span aggregate needs summing across the loop.

### 4.6 Sync/async bridge + crate-dependency fit

- `BlockingRuntime` (http.rs:17) wraps `Option<tokio::runtime::Runtime>`; its Drop
  (35–41) calls `shutdown_background()` — **never blocks on drop** (a plain
  Runtime panics when dropped inside an async context). `create_runtime`
  (http.rs:44) builds a multi-thread runtime. Each provider OWNS its runtime
  (anthropic.rs:10/15) and `complete` does `self.runtime.block_on(...)`
  (anthropic.rs:406).
- **OTel fit:** the VM is single-threaded sync (`Rc` everywhere). The default
  thread-based `BatchSpanProcessor` + HTTP transport needs no Sema runtime; span
  emission is non-blocking and safe from the VM thread; only **export** needs a
  runtime, and only **gRPC construction** must happen inside a tokio context
  (reuse `BlockingRuntime` for the build call ONLY). OTel types are `Send+Sync`
  and live off-thread — they never touch Sema's `Rc` Values. Mirror
  `BlockingRuntime`'s non-blocking-Drop discipline; flush from a lifecycle hook,
  not Drop.
- **Crate edges (verified, no cycles):** `sema-core` → nothing internal;
  `sema-stdlib` → `sema-core` only; `sema-llm` → `sema-core` only (Cargo.toml:11–20,
  **no sema-eval**); `sema-eval` → `sema-core` + `sema-llm` (eval/Cargo.toml:18).
  A new **`sema-otel` crate = `sema-core` + opentelemetry only**; then `sema-llm`
  (and `sema-stdlib`, `sema-eval`, binary) may depend on it without a cycle.
  **Do NOT add opentelemetry to `sema-core`** — it is shared with `sema-wasm` and
  must stay a tokio-free leaf.
- **Lifecycle:** `reset_runtime_state` (builtins.rs:115) resets ALL thread-local
  LLM state and runs on **every** Interpreter build — 3 init sites:
  `eval.rs:67` (Interpreter::new), `eval.rs:85` (new_with_sandbox), `lib.rs:85`
  (builder build()). OTel init must be **idempotent** (`std::sync::Once` /
  thread-local guard) or it leaks/duplicates tracer providers. There is no
  explicit Interpreter Drop tearing down LLM state today — add an explicit
  `force_flush` before process exit (binary main exit path) and optionally at the
  top of `reset_runtime_state` to flush the prior generation's spans.

Sources (§4): codebase `file:line` anchors above (all verified at 1.21.1).
