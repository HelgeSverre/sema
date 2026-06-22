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

---

## 5. Embedded / multi-runtime OTel

The standalone-CLI plan (`docs/plans/2026-06-21-otel-observability.md`) is correct
for **mode 1 — Sema owns the process**. But Sema also ships as (mode 2) an
**embedded Rust library** (`crates/sema/src/lib.rs` `InterpreterBuilder` /
`Interpreter`) and (mode 3) a **WASM module in the browser** (`crates/sema-wasm`,
sema.run). The init model that's right for the CLI (`init_from_env()` installs a
**global** `SdkTracerProvider`; an `OtelGuard` owns shutdown; a dedicated OS
export thread; a tokio `BlockingRuntime` for gRPC) is **embedding-hostile** and
**WASM-impossible**. This subsection captures the SOURCED research behind making
`sema-otel` a single sink-agnostic facade whose default is a true no-op
everywhere, with mode-specific *wiring* on top rather than mode-specific *logic*.

> The locked standalone decisions (silent no-op default, `SEMA_OTEL_FILE` JSONL
> fallback, both OTLP transports compiled, GenAI semconv, zero observe-LLM
> coupling) are **mode-agnostic and survive unchanged**. What changes per mode is
> *who calls init, who owns the provider, and who owns shutdown.*

### 5.1 The library rule: instrument, don't configure

The single principle every mature embeddable runtime converges on:
**libraries EMIT telemetry against a host-configured provider; they do NOT
install global state.** opentelemetry-rust says this verbatim:

> `set_tracer_provider`: "Sets the given TracerProvider instance as the current
> global provider. **Libraries should NOT call this function.** It is intended for
> applications/executables." (same warning on `set_meter_provider`).

The mechanism is a single process-global slot — last-writer-wins, ordering
dependent, **no error on conflict**. If `sema-otel` (as a library) calls
`set_tracer_provider`, it either (a) clobbers a host that already configured OTel
(host spans silently stop exporting) or (b) gets clobbered by a host that
configures later. The safe default exists: when no provider is set,
`opentelemetry::global` returns a `NoopTracerProvider` that discards everything,
so a library can call `global::tracer(...)` / `global::tracer_with_scope(scope)`
**unconditionally** and simply produce nothing when unconfigured.

Recommended library instrumentation pattern: build an `InstrumentationScope`
(name `"sema"`, version, schema) and call `global::tracer_with_scope(scope)` —
**never** capture/own a provider. This is exactly what the standalone plan's §3.4
facade already does for the global path; the embedding change is to **stop the
OTLP / `SEMA_OTEL_FILE` paths from setting the global in library contexts.**

opentelemetry-rust 0.28+ reinforces that ownership is an application concern:
`global::shutdown_tracer_provider()` and `global::set_error_handler` were
**removed**; apps retain an owned `SdkTracerProvider` and call `.shutdown()` /
`.force_flush()` themselves (it's a cloneable handle; shutdown auto-fires when the
last clone drops). So an embedder can — and must — own lifecycle entirely.

Sources:
- <https://docs.rs/opentelemetry/latest/opentelemetry/global/fn.set_tracer_provider.html>
- <https://docs.rs/opentelemetry/latest/opentelemetry/global/index.html>
- <https://docs.rs/opentelemetry/latest/opentelemetry/trace/index.html>
- <https://github.com/open-telemetry/opentelemetry-rust/blob/main/docs/migration_0.28.md>
- <https://github.com/open-telemetry/opentelemetry-rust/discussions/1651>
- <https://signoz.io/blog/opentelemetry-rust/>

### 5.2 `tracing` + `tracing-opentelemetry` vs direct `opentelemetry`

The ecosystem-idiomatic pattern for a Rust **library** is to instrument with the
`tracing` crate (`#[instrument]`, `info_span!`, `.instrument()`) and let the
**host** own the subscriber and the `tracing-opentelemetry` bridge layer that
converts tracing spans → OTel spans. This is how **wasmtime, sqlx, reqwest,
tonic, hyper** all expose telemetry: they emit `tracing` and hold **zero** global
OTel state; the embedding app decides whether to bridge to OTel at all. **None of
them call `global::set_tracer_provider`.**

`tracing-opentelemetry` passes semconv attributes straight through as span fields
(`trace_span!("request", "server.port" = 80, "url.full" = ...)`), supports
reserved `otel.name` / `otel.kind` (`"client"`/`"server"`) fields, and
`OpenTelemetrySpanExt::set_parent` / `Span::current().set_parent(cx)` for external
context — so `gen_ai.*` attributes work as tracing fields.

The standalone plan (§3.1) **deliberately chose raw `opentelemetry`** over the
tracing bridge, citing (1) exact control over GenAI CLIENT span kind + precise
attribute names + the two specific histograms, and (2) the VM is sync, so there's
no async span context to thread. **Both reasons are defensible for the CLI** — but
they are precisely what makes the design embedding-unfriendly: raw-`opentelemetry`
naturally reaches for the global provider and does **not** auto-nest under a
host's current `tracing` span, whereas a tracing-based design is parent-by-default
and owns no global state.

Tradeoffs for Sema specifically:
- **raw opentelemetry** = precise span-kind/name/attr/metric control, but *you*
  must solve global-provider ownership and you do **not** auto-nest under a host's
  `tracing` span.
- **tracing + bridge** = automatic nesting under the host's current `tracing`
  span and zero global state in the lib, but metrics are weaker through the bridge
  (logs aren't bridged at all without `opentelemetry-appender-tracing`) and the
  two GenAI histograms are more awkward to emit precisely.

**Bridge caveat for both:** `tracing-opentelemetry` materializes the OTel span
when the tracing span **CLOSES** (not at open) — do **not** read back the OTel
`SpanContext` mid-span (e.g. to inject `traceparent` into an outbound LLM HTTP
call); it won't be there yet.

**Recommendation:** keep raw-`opentelemetry` as the CLI default (precise GenAI
control), but **strongly consider a `tracing`-feature variant** of the same GenAI
spans (emit with `otel.kind` / `gen_ai.*` fields). Most Rust embedders already run
a `tracing` subscriber + `tracing-opentelemetry`; that variant gives them
auto-nesting and zero OTel-API coupling. It is the single most embedding-friendly
option and worth a feature flag even if raw-OTel stays default.

Sources:
- <https://docs.rs/tracing-opentelemetry>
- <https://docs.rs/tracing-opentelemetry/latest/tracing_opentelemetry/trait.OpenTelemetrySpanExt.html>
- <https://docs.wasmtime.dev/api/wasmtime/struct.Store.html>
- <https://docs.wasmtime.dev/contributing-architecture.html>
- <https://github.com/launchbadge/sqlx/discussions/2333>
- <https://crates.io/crates/tracing-opentelemetry-instrumentation-sdk>

### 5.3 Context propagation — nest Sema's spans under the host's trace

The high-value embedded behavior is trace **continuity**: a host's HTTP request
span should be the **parent** of Sema's agent/LLM run, so the host's distributed
trace contains Sema's `gen_ai.*` spans. Two mechanisms by host world:

1. **Host uses raw opentelemetry:** child spans attach to the thread's active
   `opentelemetry::Context`. Create via
   `tracer.start_with_context(name, &Context::current())`,
   `Context::current_with_span(span)`, `tracer.in_span(...)`, or
   `mark_span_as_active`. The propagation API "MUST NOT accept a Span/SpanContext
   as parent, only a full Context."
2. **Host uses tracing:** nesting is automatic via the tracing span tree; for an
   external/remote context call `Span::current().set_parent(parent_context)`.

**Async note (future-proofing):** if Sema ever gains async LLM calls you MUST use
`FutureExt::with_context(Context::current_with_span(span))` — `mark_span_as_active`
guards do **not** survive `.await`. (Today the VM is sync, so not yet relevant.)

**The gap in the current plan:** §3.5's thread-local `RefCell<Vec<Context>>` stack
reproduces parent/child nesting *within* Sema, but it never reads
`opentelemetry::Context::current()` at the top of a Sema run — so embedded Sema
would start a **new root trace** instead of nesting under the host's request span.
Fix: seed the bottom of the stack from `Context::current()` and create Sema's root
span via `tracer.start_with_context(name, &Context::current())`. Cross-host-guest
runtimes do exactly this — wasmtime/WASI-OTel/Runwasi/wasmCloud pass trace context
across the host↔guest boundary via **W3C Trace Context** (`traceparent`;
propagators via `OTEL_PROPAGATORS`) and have short-lived guests **buffer**
telemetry to the host.

Sources:
- <https://opentelemetry.io/docs/concepts/context-propagation/>
- <https://uptrace.dev/get/opentelemetry-rust/propagation>
- <https://docs.rs/opentelemetry/latest/opentelemetry/trace/trait.Span.html>
- <https://github.com/WebAssembly/wasi-otel>
- <https://runwasi.dev/opentelemetry.html>
- <https://wasmcloud.com/docs/v1/deployment/observability/observability-with-opentelemetry/>

### 5.4 What peer embeddable SDKs do (ideas to steal)

Every mature peer treats "embedded" as **read-from-host / bring-your-own /
no-op-where-unexportable**, never library-owns-global:

- **Vercel AI SDK** (Node + Edge + Browser): telemetry **OFF unless**
  `experimental_telemetry.isEnabled:true` **per call** — there is no global
  "init telemetry" the SDK owns. Accepts an optional `tracer` field to inject a
  non-singleton provider; otherwise **reads** the global `@opentelemetry/api`
  singleton but **never SETS it**. Same instrumentation API everywhere; capability
  degrades to no-op where no exporter runs (custom spans unsupported on Edge,
  browser export effectively unsupported). "Errors inside integrations are caught
  and do not break the generation flow."
- **OpenLLMetry / Traceloop:** `Traceloop.init(exporter=...)` takes an **explicit
  exporter** so it plugs INTO an existing pipeline; `disable_batch=True` swaps
  Batch→immediate for notebooks/REPLs; `instruments={}` / `block_instruments={}`
  pick active instrumentations. "SDK init options ALWAYS take precedence over env
  vars."
- **Mastra AI Tracing:** marks framework-internal ops as **INTERNAL** spans and by
  default does **NOT** export them ("OTel first, drowned in noise"). Injectable
  exporter objects, arrays for multi-destination, `configSelector(runtime ctx)`
  routing, dev=immediate / prod=batch. **Deprecated** its legacy global telemetry
  config in favor of the injectable-exporter model.
- **Deno built-in OTel** (`OTEL_DENO=true`): the **runtime** owns the pipeline and
  auto-registers providers with `@opentelemetry/api`; guest code just uses the API.
  `OTEL_EXPORTER_OTLP_PROTOCOL=console` = zero-collector local debug (analog to
  `SEMA_OTEL_FILE`).
- **wasmtime / Store-Engine:** all state on an explicit `Store`/`Engine`, **no
  global telemetry**; emits `tracing`; the embedder owns any subscriber.

**Steal:** (1) BYO-provider + read-global-never-set; (2) programmatic config that
takes precedence over env (an embedder can't always set process env); (3)
internal-span filtering (default `vm_span`/load/import/macro-expand spans OFF or
INTERNAL+unexported so an LLM trace isn't drowned in VM plumbing); (4)
flush-mode-by-environment (notebook/REPL → immediate/simple; CLI/server → batch);
(5) per-call/per-scope content toggles as a secondary surface.

Sources:
- <https://ai-sdk.dev/docs/ai-sdk-core/telemetry>
- <https://vercel.com/docs/tracing/instrumentation>
- <https://www.traceloop.com/docs/openllmetry/configuration>
- <https://github.com/traceloop/openllmetry/issues/3478>
- <https://mastra.ai/blog/aitracing>
- <https://mastra.ai/docs/observability/tracing/exporters/otel>
- <https://docs.deno.com/runtime/fundamentals/open_telemetry/>

### 5.5 Embedding-mode constraints from the Sema codebase

The public embedding surface (`crates/sema/src/lib.rs`) is small and fully
synchronous: `InterpreterBuilder` (new/`with_stdlib`/`with_llm`/`with_sandbox`/
`with_allowed_paths`/`build`) and `Interpreter` (new/builder/`eval`/`eval_str`/
`register_fn`/`load_file`/`preload_module`/`global_env`). **There are NO
observability hooks today** — no logger, no tracer, no step-limit (the step-limit
gap is documented but unimplemented in
`docs/plans/2026-03-11-embedding-api-improvements.md`). An embedded OTel design
must add its own opt-in entry point; nothing in the current builder lets a host
inject a tracer/exporter or scope telemetry to one interpreter.

Hard constraints (verified):
- **No global tracing/log/OTel state exists today.** An exhaustive grep across all
  crates for `tracing::` / `tracing_subscriber` / `log::` / `env_logger` /
  `set_logger` / `opentelemetry` / `set_global_default` returned **zero**
  non-test matches. Sema is currently silent, so there's no conflict today — the
  risk is entirely about what `sema-otel` WOULD install.
- **`reset_runtime_state` (`builtins.rs`) runs on EVERY `build()`** — 3 init sites
  (`eval.rs:67` `Interpreter::new`, `eval.rs:85` `new_with_sandbox`, `lib.rs:85`
  builder `build()`). It wipes all thread-local LLM provider/usage/budget state.
  Any global OTel init must be **idempotent** (`std::sync::Once`) AND **scoped to
  application mode** — naive re-init across these 3 sites duplicates/leaks
  providers or clobbers a host's.
- **No process-wide tokio runtime.** Each LLM provider owns its own
  `BlockingRuntime` (`http.rs:8-48`) whose Drop does `shutdown_background()` to
  avoid the "drop runtime in async context" panic. In embedding, the host may
  already run inside a tokio runtime (the MCP-server drop-panic comment at
  `http.rs:11-16` shows this is a real, hit case). So: do **not** spawn Sema's own
  export runtime/threads by default in embedding; make the span processor
  pluggable (Batch for CLI; Simple/file for embed); shutdown/Drop must be
  panic-safe inside a host async context (mirror `BlockingRuntime::shutdown_background`,
  `http.rs:35-41`).
- **Per-interpreter vs per-thread scoping:** per-instance (isolated, in
  `EvalContext`): module cache, current_file stack, eval/call callbacks, span
  table, depth/step counters. **Per-thread (shared across ALL interpreters on the
  thread):** LLM provider registry / usage / budgets / `LAST_SERVING_PROVIDER`
  (`builtins.rs`), the string interner, output hooks, async signal callbacks,
  `CURRENT_VM`. **Implication:** GenAI/LLM telemetry that reads provider state is
  inherently **per-thread**, while span/timing data from `EvalContext` is
  per-interpreter. Store the OTel scope **per-`EvalContext`** (like callbacks), not
  as a thread-local/global, to honor the documented isolation contract — and do
  not let it be silently reset by the `build()`-time `reset_runtime_state` pattern.
- **Keep the eval path `Rc`-based and `!Send`.** `Value`/`Env`/`EvalContext` are
  `Rc`-based and `!Send`; OTel plumbing threaded through eval must not require
  `Send`/`Sync` on the eval path (OTel types are `Send+Sync` and live off-thread,
  so they never touch Sema's `Rc` Values).

**Recommended embedding API shape (additive, doesn't change CLI defaults):**
- `sema_otel::install_from_env() -> Option<OtelGuard>` — APPLICATION-ONLY (CLI /
  REPL / notebook-server `main`). The **only** function allowed to call
  `global::set_tracer_provider`. `Once`-guarded; hands the `OtelGuard` back to the
  caller to own shutdown.
- `sema_otel::set_provider(provider)` / `use_global()` — embedder hands Sema a
  provider it built, OR tells Sema to use whatever `global::tracer_provider()` the
  host already installed (the default). In both cases Sema **never** calls
  `set_tracer_provider` and **never** installs a shutdown-owning guard.
- Facade `*_span(...)` constructors resolve the tracer lazily via
  `global::tracer_with_scope(InstrumentationScope "sema" + version)` — degrades to
  the global no-op when nothing is set.
- Root span seeds parent from `Context::current()` (§5.3).
- `InterpreterBuilder::with_telemetry(Off | UseHostGlobal | OwnProvider(...))`,
  defaulting `Off`, matching the existing `.with_sandbox`/`.with_llm` ergonomics;
  `Interpreter::new()`/`build()` stay guaranteed no-ops that never touch global OTel.

Sources (§5.5): codebase `file:line` anchors above; `website/docs/embedding.md`
(per-interpreter isolation `:124`, `:267`; per-thread LLM state `:270`; interner
`:265`,`:269`; `!Send` `:265-268`); `docs/plans/2026-03-11-embedding-api-improvements.md`.

### 5.6 WASM / browser reality — the Rust 0.32 stack can't export from wasm32

**The data model compiles on wasm; the export plumbing does not.** Creating a
`SdkTracerProvider`, tracers, spans, attributes, and GenAI semconv all compile and
run on `wasm32` (just data structures). What breaks is exclusively the
processor + exporter + runtime layers:

- **No OS threads → `BatchSpanProcessor` can't exist.** Since 0.28 the default
  `BatchSpanProcessor`/`BatchLogProcessor`/`PeriodicReader` spawn a **dedicated OS
  background thread** with blocking calls (no async runtime needed). Browser
  `wasm32-unknown-unknown` has no threads and no blocking — structurally
  incompatible. The standalone plan's "dedicated background OS thread" exporter
  (§3.3) **cannot run.**
- **`opentelemetry-otlp` does NOT compile on wasm32.** Default transports are
  http/protobuf via blocking reqwest and gRPC via tonic. gRPC/tonic is a
  non-starter (no sockets/h2); the reqwest path **fails to compile** because wasm
  `reqwest::Response` and `wasm_bindgen_futures` internals (`Rc<RefCell<...>>`) are
  **not `Send`**, violating the exporter's `Send + Sync` bounds. Tracked in the
  still-OPEN upstream issue **open-telemetry/opentelemetry-rust#3155** (Sept 2025);
  maintainers have **not** committed to supporting it. So the locked "always
  compile both OTLP transports" decision **cannot hold on wasm** — it won't link.
  The plan already half-acknowledges this (`otel` must stay `#[cfg(not(wasm32))]`,
  mirroring how `sema-llm` is excluded).
- **No filesystem → `SEMA_OTEL_FILE` JSONL fallback has no target.** `std::fs`
  doesn't exist on wasm32; `sema-core` already uses a thread-local VFS
  (`BTreeMap`) for exactly this reason — it is in-memory, not a real file.
- **`std::time::Instant::now()` is unimplemented on wasm32-unknown-unknown**
  ("time not implemented on this platform") and **panics** inside span-close timing
  — any duration math must route through `web-time` / `performance.now()`. This is
  a **runtime** panic, so it can slip past a quick compile smoke test.

**There is nothing to observe in the browser anyway.** `sema-wasm`
(`Cargo.toml`) depends on `sema-core`/`reader`/`eval`/`fmt`/`vm` but **NOT**
`sema-llm` — and there is no `tokio`/`reqwest` anywhere in the wasm dependency
closure (`sema-stdlib` puts them under `[target.'cfg(not(wasm32))'.dependencies]`;
`sema-llm` depends on tokio+reqwest **unconditionally**, so it can't build on
wasm). `register_llm_builtins` is gated out at `eval.rs:65/83`
(`#[cfg(not(target_arch="wasm32"))]`), so the `llm/*` / GenAI builtins **don't
exist** as bound functions in the WASM interpreter. **The entire `gen_ai.*` span
surface the plan instruments lives in a crate that is absent from the wasm
build.** `sema-stdlib`'s `http`/`io`/`kv`/`server`/etc. modules are likewise
`#[cfg(not(wasm32))]`-gated; wasm uses its own bespoke async `http/*` via a
marker/replay-with-cache bridge (`lib.rs:188,220,1844-1928`).

**The idiomatic wasm pattern: bridge span data out to JS, don't export from
wasm.** The mature, batteries-included browser path lives in **JavaScript**:
`@opentelemetry/sdk-trace-web` + `@opentelemetry/exporter-trace-otlp-http` (OTLP
over `fetch`), with a browser-safe JS `BatchSpanProcessor` (no OS threads),
`ZoneContextManager`, and W3C propagators — it handles OTLP encoding, batching
tuned for page-unload, retry, CORS, and CSP that the Rust path makes you
reinvent. Existing Rust-wasm tracing crates (`tracing-wasm`, `wasm-tracing`,
`tracing-web`) are all `tracing_subscriber::Layer`s that **forward** span/event
data to console/Performance, not exporters. The generalization — and the only
realistic browser export path for Sema — is a **JS-callback sink**: the wasm
module exposes a `setSpanSink(fn)` analog (mirroring the existing
`setOutputSink(fn)` at `lib.rs:2803`, `registerFn(name, fn)` at `lib.rs:2437`,
`installAtomicsSleep` at `lib.rs:2791` — all install a `js_sys::Function` into a
thread-local), emits each finished span as a plain JS object (name, IDs,
`performance.now()`-based timing, attributes), and lets the host page forward to
its JS OTel SDK / collector. `sema-wasm/Cargo.toml` already enables the web-sys
features a fetch exporter or bridge would need (`Window`, `Request`,
`RequestInit`, `Headers`, `Response`, `AbortController`) + `wasm-bindgen-futures`
+ `js-sys` — **no new heavy deps required.**

A pure-Rust in-wasm exporter is the **last resort**: `SimpleSpanProcessor` (no
background thread; exports synchronously on span-end) + a hand-written
`SpanExporter` POSTing OTLP/HTTP-JSON via `web_sys::fetch` + `JsFuture`, using
`web-time` for timing and `?Send`/`spawn_local` to dodge the `Send` bounds. Viable
but high-maintenance (re-implements OTLP JSON, CORS/CSP) relative to the JS
bridge.

**Bottom line for scoping:** WASM emits **NOTHING** under the current plan, and
that is the **correct MVP default** — the GenAI spans live in `sema-llm` (absent
from wasm), and the export machinery (threads/tokio/fs) is unavailable. The plan
should state plainly that `sema-otel` compiles to a **pure no-op** under
`cfg(target_arch = "wasm32")` (depend only on the API crate's `NoopTracerProvider`
default; never link `opentelemetry-otlp`/tokio; cfg-wall the Batch processor and
gRPC `BlockingRuntime`; keep the RAII facade types compiling and returning
`None`-holding spans), mirroring the existing `sema-llm` exclusion at `eval.rs:65`.
Add a build/regression test that asserts `sema-wasm` builds with telemetry code
present and produces a no-op (no panic, no tokio link) on wasm32 — **never** rely
on runtime detection. Treat browser telemetry as out-of-scope for the MVP, served
later by a JS-callback bridge if ever needed, not the OTLP SDK.

Sources (§5.6):
- <https://github.com/open-telemetry/opentelemetry-rust/issues/3155>
- <https://github.com/open-telemetry/opentelemetry-rust/blob/main/docs/migration_0.28.md>
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/trace/struct.BatchSpanProcessor.html>
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/trace/struct.SimpleSpanProcessor.html>
- <https://docs.rs/opentelemetry_sdk/latest/opentelemetry_sdk/trace/span_processor_with_async_runtime/struct.BatchSpanProcessor.html>
- <https://opentelemetry.io/docs/languages/js/getting-started/browser/>
- <https://www.npmjs.com/package/@opentelemetry/exporter-trace-otlp-http>
- <https://crates.io/crates/wasm-tracing>, <https://lib.rs/crates/tracing-web>
- <https://github.com/tokio-rs/tracing/issues/2720>
- <https://opentelemetry.io/docs/languages/rust/exporters/>
- <https://docs.rs/web-sys/latest/web_sys/struct.Window.html>
- codebase: `crates/sema-wasm/Cargo.toml`, `crates/sema-wasm/src/lib.rs`
  (`:188`,`:220`,`:366-389`,`:421-562`,`:1844-1928`,`:2437`,`:2791`,`:2803`),
  `crates/sema-eval/src/eval.rs:65,83`, `crates/sema-stdlib/src/lib.rs:11-38`,
  `crates/sema-stdlib/Cargo.toml:30-32`, `crates/sema-llm/Cargo.toml`,
  `crates/sema-llm/src/http.rs:8-48`, `playground/src/sema-worker.js:13-44`.

### 5.7 Mode summary + decisions needed

| Mode | Who installs the provider | Who owns shutdown | Default behavior |
|---|---|---|---|
| 1 — standalone CLI/REPL/notebook | Sema (`install_from_env()` in `main`) | Sema (`OtelGuard`, flush-on-exit) | install global iff endpoint/`SEMA_OTEL_FILE` set; else no-op |
| 2 — embedded Rust library | **Host** (BYO provider) or host global | **Host** | read host global; silent no-op if host installed nothing; explicit opt-in to self-install |
| 3 — WASM/browser | nobody (MVP) | n/a | **pure no-op, compiled out**; future JS-callback bridge |

**Open decisions for the plan to settle:**
1. **MVP scope:** confirm WASM = pure no-op for v1 (recommended), deferring the
   JS-bridge exporter until in-browser LLM/agent runs exist.
2. **Embedded default:** does the embedded `Interpreter` attach to the host's
   **ambient/global** provider by default (recommended — silent if none), or stay
   fully inert unless the host calls a `.with_otel_*()` builder method?
3. **Trace continuity:** commit to seeding the facade parent from
   `opentelemetry::Context::current()` (§5.3)? Highest-value embedded behavior;
   couples `sema-otel` to the opentelemetry `Context` API.
4. **Ownership contract:** confirm `Interpreter::build()` NEVER installs/shuts down
   a global provider, and that self-install hands the `OtelGuard` back to the host.
5. **tracing-bridge variant:** raw-OTel only, or also offer a `tracing`-feature
   variant so Sema spans show up natively in a host's `tracing` subscriber (§5.2)?
6. **Precedence when host already has a global provider AND Sema is env-configured:**
   detect-and-defer to host, warn, or proceed? Define the rule.
