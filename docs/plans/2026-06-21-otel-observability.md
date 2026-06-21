# OpenTelemetry Observability for LLM/Agent Runs (and the VM)

**Status:** Plan — validated, implementation-ready. **NOT started.**
**Date:** 2026-06-21 (rewritten with validated findings + resolved decisions)
**Owner:** unassigned
**Scope:** Standards-compliant, opt-in OpenTelemetry tracing + metrics for Sema's
LLM and agent runs, with a span model usable for the VM generally. No proprietary
SDK lock-in; export to any OTLP backend (Jaeger / Langfuse / Datadog / Honeycomb /
Grafana / Phoenix).

> Line numbers below are anchored to `sema-llm/src/builtins.rs` at workspace
> version **1.21.1** (5244 lines). They drift — **anchor on function names**, the
> line numbers are a convenience.

---

## 0. Resolved decisions (locked)

These eight questions are answered; do not re-litigate during implementation.

1. **Crate version pin (was Open Q#1).** Pin the synchronized **0.32 train**, bumped
   together: `opentelemetry` 0.32.0, `opentelemetry_sdk` 0.32.1 (patch ahead of the
   API), `opentelemetry-otlp` 0.32.0, `opentelemetry-semantic-conventions` 0.32.0.
   MSRV 1.75. Enable the semconv crate's `semconv_experimental` feature for the
   `gen_ai.*` constants. Source of truth for attribute strings is
   `github.com/open-telemetry/semantic-conventions-genai` (the old
   `opentelemetry.io/docs/specs/semconv/gen-ai/*` pages are now relocation stubs —
   their "deprecated" badges are about the page move, not the attributes). Treat all
   0.x as break-on-minor; re-verify builder/trait shapes on every bump.

2. **Cost attribute namespace (was Open Q#2).** Emit the de-facto community attribute
   **`gen_ai.usage.cost_usd`** (NOT `sema.`-prefixed) as a span attribute (and
   optionally a histogram), computed via `pricing::calculate_cost_for`. ALWAYS also
   emit accurate `gen_ai.usage.input_tokens` / `output_tokens` so any backend can
   recompute. A `sema.`-prefixed attribute is invisible to `gen_ai.*`-aware backends
   (Langfuse/Datadog) and is the weaker choice.

3. **Embeddings token split (was Open Q#3).** Embeddings emit
   `gen_ai.usage.input_tokens` only (no output); `gen_ai.operation.name="embeddings"`.
   In the token-usage histogram record only the `input` bucket. Deferred to M4
   (embeddings bypass `do_complete` like streaming).

4. **`(otel/span …)` is a builtin, not a special form (was Open Q#4).**
   `(otel/span name f)` runs thunk `f` inside a named INTERNAL span, returns `f`'s
   value. `(otel/event name attrs-map)` adds an event to the current span. Both are
   builtins registered in `sema-stdlib` behind the enabled flag (no-op when off).
   `sema-stdlib` must NOT depend on `sema-eval`/`sema-llm` — it CAN depend on
   `sema-otel` (which depends only on `sema-core` + opentelemetry), preserving the
   CLAUDE.md invariant.

5. **Notebook trace identity (was Open Q#5).** One trace per "Run All" with a child
   `vm_span` per executed cell; LLM/tool spans nest beneath the issuing cell.
   Propagate `gen_ai.conversation.id` (or a Sema session id) to every child span so
   backends (Langfuse requires per-span trace-level attrs) aggregate correctly.
   Single-cell incremental eval emits a standalone one-cell trace. This is M4, not MVP.

6. **Silent no-op default + opt-in file fallback (Locked Decision #1).** With no OTLP
   endpoint and no `SEMA_OTEL_FILE`, install **no tracer provider at all** (true
   zero-cost no-op). A down/unreachable collector can NEVER block or add latency to
   the VM thread (see §3.3). `SEMA_OTEL_FILE=path` installs a custom JSONL exporter
   independent of any collector. Full fail-safe contract in §3.3.

7. **No `observe-llm` coupling (Locked Decision #2).** This plan and all Sema
   code/docs contain zero references to `observe-llm`; it was used only as generic
   OTel/GenAI background. Keep it that way.

8. **Both OTLP transports, env-selected (Locked Decision #3).** Compile **both**
   `http-proto` (default, via `reqwest-blocking-client`) and `grpc-tonic`. Read
   `OTEL_EXPORTER_OTLP_PROTOCOL` at init and dispatch in code — the env var does NOT
   auto-flip linked transports. Default `http/protobuf` (lighter, no runtime
   coupling). Details in §3.1 / §3.2.

---

## 1. Motivation — why this is table-stakes

Sema markets itself with "stop rewriting the agent loop — the scaffolding *is* the
runtime". Retries, caching, cost caps, rate limits, and tool dispatch already live in
`sema-llm`. The one piece of agent scaffolding that is *not* in the runtime is
**observability**, the piece that decides whether an agent framework is trustworthy
in production:

- **Cost/latency are invisible per-step.** Today the only signals are the per-call
  `on-tool-call` callback (a 200-char truncated preview) and aggregate
  `llm/last-usage` / `llm/session-usage`. There is no way to answer "which LLM call
  in this agent run was slow?", "how many tokens did this notebook cost?", or "which
  tool errored mid-loop?" without `eprintln` archaeology.
- **Tool `duration_ms` is already measured and thrown away.** In `run_tool_loop`
  (`builtins.rs:4516`) each tool call is `Instant`-timed (`start_time` at `:4580`,
  `duration_ms` at `:4594`) and handed to the `on-tool-call` callback at `:4612` —
  then discarded. NOTE: this is the ONLY wall-clock measurement in the LLM path;
  there is **no** latency measured around the LLM call itself, so an LLM-call span
  must `Instant`-measure its own duration.
- **The whole industry converged on OTel GenAI semantic conventions.** The CNCF
  GenAI semconv (Development/experimental, latest ~v1.41 as of mid-2026) is natively
  consumed by Datadog, Langfuse, MLflow, Phoenix, and emitted by Claude Code, Codex,
  Copilot. Adopting it gives Sema standards-compliant token/cost/latency tracing
  across every vendor for free.
- **Differentiator for the notebook.** A notebook run becomes one inspectable trace
  (cell spans → LLM spans → tool spans) — a flagship demo for sema.run.

This plan delivers observability at the **non-streaming completion chokepoint** plus
the tool/agent dispatch sites, so every Sema LLM program gets traced with **zero
per-call user code** and **zero overhead when disabled**.

> **Coverage caveat (corrects the old "single chokepoint" claim).** `do_complete`
> covers **non-streaming completions only**. Uncovered paths and where each is closed:
> - **Cache hits** short-circuit before provider dispatch with ZERO usage
>   (`do_complete` early-return at `:4158-4179`, `stop_reason="cache_hit"`) — emit a
>   span tagged `gen_ai.cache.hit=true` with zero usage in **M1**.
> - **Streaming** (`llm/stream`) bypasses `do_complete` AND `track_usage` entirely
>   (`builtins.rs:1565/1579`) — add a streaming span in **M4**.
> - **Embeddings** bypass `do_complete` — add `embeddings` spans in **M4**.

---

## 2. Standard reference — OTel GenAI semantic conventions

Verbatim attribute names from the GenAI semconv (v1.37+ baseline; `gen_ai.system`
→ `gen_ai.provider.name`, `usage.prompt_tokens/completion_tokens` →
`usage.input_tokens/output_tokens` are already applied below).

### Spans

| Span | kind | name | when |
|------|------|------|------|
| LLM call | `CLIENT` | `{gen_ai.operation.name} {gen_ai.request.model}` e.g. `chat claude-sonnet-4-6` | per non-streaming completion |
| Tool call | `INTERNAL` | `execute_tool {gen_ai.tool.name}` (v1.41 REQUIRES the tool name in the span name) | per tool dispatch in `run_tool_loop` |
| Agent run | `INTERNAL` | `invoke_agent {gen_ai.agent.name}` (or bare `invoke_agent`) | per `agent/run` / `llm/complete` with tools |

**Agent span kind rationale:** the spec says `invoke_agent` SHOULD be `CLIENT` for a
remote agent service, `INTERNAL` for an in-process framework agent. Sema agents run
in-process → `INTERNAL`.

`gen_ai.operation.name` well-known values: `chat`, `text_completion`, `embeddings`,
`generate_content`, `create_agent`, `invoke_agent`, `invoke_workflow`,
`execute_tool`, `retrieval`. (`invoke_workflow` is v1.41-new; not needed for MVP.)

### Provider mapping (Sema → `gen_ai.provider.name`)

Isolate this in one module — `gen_ai.*` is experimental and churns. The bare
`gemini` value used in the old plan is **non-conformant** (mis-buckets Gemini).

| Sema provider | `gen_ai.provider.name` |
|---------------|------------------------|
| `anthropic` | `anthropic` |
| `openai` | `openai` |
| `gemini` | `gcp.gemini` |
| `vertex` | `gcp.vertex_ai` |
| `ollama` | `ollama` *(no standard value; keep raw)* |

Full spec valid set: `openai`, `anthropic`, `aws.bedrock`, `azure.ai.openai`,
`gcp.vertex_ai`, `gcp.gemini`, `cohere`, `mistral_ai`, `groq`, `deepseek`,
`perplexity`, `x_ai`, `ibm.watsonx.ai`.

### LLM-span attributes

- `gen_ai.operation.name` — `chat` \| `text_completion` \| `embeddings`
- `gen_ai.provider.name` — via the mapping above (read AFTER dispatch; see §3.4)
- `gen_ai.request.model` (resolved request model) / `gen_ai.response.model`
  (authoritative — use `response.model`; `request.model` is EMPTY at entry for
  unpinned models, substituted downstream at `:4328/:4344`)
- `gen_ai.request.temperature` (double), `gen_ai.request.max_tokens` (int),
  `gen_ai.request.stop_sequences` (string[])
- `gen_ai.usage.input_tokens` / `gen_ai.usage.output_tokens` (int; **0 on cache hit**)
- `gen_ai.response.finish_reasons` (string[] — wrap the single
  `ChatResponse.stop_reason` in a 1-element array)
- `gen_ai.conversation.id` (cheap, standard; for agent/notebook session correlation)
- `gen_ai.response.id` — **NOT available** (`ChatResponse` has no id; Anthropic
  response id is not deserialized). Aspirational; omit until the type carries it.
- **cost:** `gen_ai.usage.cost_usd` (see Resolved Decision #2)
- **non-standard but available:** `gen_ai.cache.hit` (bool, from
  `stop_reason=="cache_hit"`), `sema.gen_ai.request.reasoning_effort`
  (`ChatRequest.reasoning_effort`)

**Anthropic prompt-cache tokens — aspirational, needs a code change.** The spec
defines `gen_ai.usage.cache_creation.input_tokens` and
`gen_ai.usage.cache_read.input_tokens` (both INCLUDED in `input_tokens`, not
additive). The current `AnthropicUsage` (`anthropic.rs:378`) and the shared `Usage`
struct DROP these fields, so they cannot be populated without threading new fields
through both. Park as a follow-up (see Open items).

**Cannot populate (flag as aspirational, do not invent):** `top_p` / `top_k` /
`frequency_penalty` / `presence_penalty` / `seed` (no such fields on `ChatRequest`);
reasoning/thinking token counts; `gen_ai.response.id`.

### Tool-span attributes

- `gen_ai.tool.name` (= `tc.name`), `gen_ai.tool.call.id` (= `tc.id`),
  `gen_ai.tool.type` = `function` (Sema tools are client-executed),
  `gen_ai.tool.description` (cheap; from the tool definition)
- `error.type` on failure (`is_error` is computed at `:4584-4593`)

### Metrics (two histograms, per the spec)

- `gen_ai.client.token.usage` — Histogram, unit `{token}`, dims
  `gen_ai.token.type` (`input`/`output`) + `gen_ai.operation.name` +
  `gen_ai.provider.name` + `gen_ai.request.model` + `gen_ai.response.model`.
  Recommended `ExplicitBucketBoundaries`:
  `[1,4,16,64,256,1024,4096,16384,65536,262144,1048576,4194304,16777216,67108864]`.
- `gen_ai.client.operation.duration` — Histogram, unit **`s` (seconds)**, dims
  `gen_ai.operation.name` + `gen_ai.provider.name` + `gen_ai.request.model`
  (+ `error.type` on failure). Recommended boundaries:
  `[0.01,0.02,0.04,0.08,0.16,0.32,0.64,1.28,2.56,5.12,10.24,20.48,40.96,81.92]`.
  Sema measures ms today → divide by 1000. **Cache-hit calls report zero tokens by
  design** — document that token metrics undercount real spend on cache hits.

Re-verify the metrics API shape against the pinned 0.32 crate (metrics API churns).

### Privacy / content capture

Content (prompts, tool args, completions) is **opt-in, OFF by default**. The
**standard** flag is `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true`
(accept `SEMA_OTEL_CAPTURE_CONTENT` as an alias). When enabled, attach
`gen_ai.input.messages` / `gen_ai.output.messages` / `gen_ai.system_instructions` as
**span-attribute FALLBACK mode** — note the spec-preferred form (v1.37+) is a
structured event `gen_ai.client.inference.operation.details`; MVP ships the
fallback, with a note to add the event later. Truncate large message content to
bound span size; drop attribute keys containing `__proto__`/`constructor`/`prototype`
(prototype-pollution guard); keep any scrubber cheap and panic-proof (it runs on the
calling thread).

---

## 3. Proposed architecture for a Rust sync VM

### 3.1 Crate choice + Cargo features

New crate `crates/sema-otel`, depending only on `sema-core` + the otel crates:

```toml
# crates/sema-otel/Cargo.toml
[dependencies]
opentelemetry        = "0.32"
opentelemetry_sdk    = "0.32"          # resolves 0.32.1; default thread-based processor — NO rt-tokio
opentelemetry-otlp   = { version = "0.32", default-features = false, features = [
    "trace", "metrics", "http-proto", "reqwest-blocking-client", "grpc-tonic",
] }
opentelemetry-semantic-conventions = { version = "0.32", features = ["semconv_experimental"] }
serde_json = { workspace = true }       # for the JSONL file exporter
```

> Build note: this API tree (`export(&self) -> impl Future`, `OTelSdkError`, NO
> `global::set_error_handler`) is **0.30+**. Pre-0.30 tutorials (`export(&mut self)
> -> BoxFuture`, `runtime::Tokio` in the batch builder) will NOT compile against
> 0.32 — do not copy them.

> Optional: gate `grpc-tonic` behind a cargo feature so default builds stay
> HTTP-light. `grpc-tonic` pulls the full tonic/hyper/h2/tower/tokio stack; the
> HTTP path needs none of it. (Human call — see Open items. Functionality is
> identical either way, only build cost differs.)

**Why a new `sema-otel` crate, not deps on `sema-llm`:**
- Confirmed dependency edges: `sema-core` depends on nothing internal;
  `sema-stdlib` and `sema-llm` depend on `sema-core` ONLY (no `sema-eval`);
  `sema-eval` depends on `sema-core` + `sema-llm`. `sema-otel` sits below
  `sema-llm`/`sema-stdlib` (depends only on `sema-core` + otel), so `sema-llm`,
  `sema-eval`, `sema-stdlib`, and the binary can all depend on it with **no cycle**
  and the CLAUDE.md invariant intact.
- Do NOT add opentelemetry to `sema-core`: it is shared with `sema-wasm` and must
  stay a tokio-free leaf. `opentelemetry-otlp` is `#[cfg(not(target_arch =
  "wasm32"))]`-gated, mirroring how `sema-llm` is excluded from wasm.

**Why not `tracing` + `tracing-opentelemetry`:** the GenAI semconv wants explicit
CLIENT spans with precise attribute names and two specific metrics; a thin manual
span API over `opentelemetry` is clearer than fighting `tracing` macro coercion, and
the VM is sync (no async span context to thread).

### 3.2 Transport selection (both, env-dispatched) — Locked Decision #3

The OTLP exporter already reads `OTEL_EXPORTER_OTLP_{PROTOCOL,ENDPOINT,HEADERS,
TIMEOUT}` with spec precedence (programmatic > signal-specific `_TRACES_*` > generic
`OTEL_EXPORTER_OTLP_*` > defaults), BUT `OTEL_EXPORTER_OTLP_PROTOCOL` does NOT flip
between linked transports automatically. At init, read it and dispatch:

```text
match OTEL_EXPORTER_OTLP_PROTOCOL (default "http/protobuf"):
  "http/protobuf" -> SpanExporter::builder().with_http().with_protocol(Protocol::HttpBinary).build()
  "http/json"     -> SpanExporter::builder().with_http().with_protocol(Protocol::HttpJson).build()
  "grpc"          -> SpanExporter::builder().with_tonic().build()   // MUST be built inside a tokio context
```

- Build the **full `SdkTracerProvider` inside each arm** — the exporter builder
  types differ and the `SpanExporter` trait is NOT dyn-compatible (its `export`
  returns `impl Future`, so `Box<dyn SpanExporter>` is impossible).
- Leave endpoint/headers/timeout to the env vars unless Sema-specific config is
  set. Default ports (4318 HTTP, 4317 gRPC) are handled by the exporter.
- **gRPC construction needs a live tokio reactor.** Only for the `grpc` arm, reuse
  Sema's existing `BlockingRuntime` semantics (`http::create_runtime`,
  non-blocking `Drop`) to host **construction only** (`runtime.enter()` /
  `block_on` around the `.build()` call). Do NOT drive per-span export through it.
- The HTTP path (default) needs no tokio runtime at all (see §3.3).

### 3.3 Fail-safe architecture (concrete) — Locked Decision #1

This is the core guarantee: **a down/slow/unreachable collector never blocks, never
adds latency, never panics, and never surfaces an error to the running Sema script.**

**(a) Default thread-based BatchSpanProcessor — NOT rt-tokio.** Since opentelemetry
0.28+, the default `BatchSpanProcessor` runs export on its **own dedicated
background OS thread** and needs no async runtime when paired with
`reqwest-blocking-client`. This keeps OTel fully decoupled from Sema's
single-threaded `Rc` world. (The old plan's `rt-tokio` dedicated-runtime model is
**obsolete** — the async-runtime processor is now an experimental opt-in we do NOT
use.) The hot path only does a **non-blocking enqueue** onto a bounded channel —
safe to call from the VM thread. Use `BatchSpanProcessor`, **never**
`SimpleSpanProcessor`, for the OTLP path (Simple exports synchronously on span-end
and would block on a dead collector).

**(b) Bounded queue, drop-on-full.** When the collector is down, exports stall on the
background thread, the bounded queue fills, and **new spans are dropped** (not
blocked, not backpressured). The SDK logs one warning and a final dropped-count at
shutdown. Tunables (env + `BatchConfig`): `OTEL_BSP_MAX_QUEUE_SIZE` (default 2048),
`OTEL_BSP_MAX_EXPORT_BATCH_SIZE` (512), `OTEL_BSP_SCHEDULE_DELAY` (5000ms),
`max_concurrent_exports` (1). Set a **short exporter timeout (~3s)** via
`OTEL_EXPORTER_OTLP_TIMEOUT` so a dead endpoint releases the export slot quickly.

**(c) Silent-by-default error surfacing.** `global::set_error_handler` /
`handle_error` were **REMOVED** in modern opentelemetry-rust (replaced by
per-operation `OTelSdkError`). Export errors are swallowed internally (spec mandates
exporters MUST NOT propagate for unreachable endpoints) and logged through the
`tracing` crate under the `internal-logs` feature. Achieve silent-by-default by
**NOT subscribing the `opentelemetry` tracing targets** in Sema's subscriber (or
filtering them OFF). Optionally a debug flag flips them to WARN for operators.

**(d) Total-failure-safe init.** A single `init_from_env() -> Option<OtelGuard>`
wraps the entire builder in `Result`; on ANY error (endpoint parse, exporter build,
thread spawn) log once at debug and return `None`, so every call site degrades to a
no-op. **Never** `unwrap`/`expect` on the OTel path.

**(e) Flush/shutdown with bounded timeout.** Keep an owned `SdkTracerProvider`
handle. At process exit, in order:
```rust
let _ = provider.force_flush();
let _ = provider.shutdown_with_timeout(Duration::from_secs(3));
```
Discard both `Result`s. Call from a thread that is **NOT** a current-thread tokio
runtime's own thread (the `shutdown()` deadlock footgun). This guarantees exit
cannot hang on a dead collector and a failed flush never reaches the script. Mirror
`BlockingRuntime`'s non-blocking-`Drop` discipline so teardown never deadlocks the
single-threaded VM. Critical for short-lived `sema run file.sema` runs that would
otherwise lose the last spans.

**(f) File-fallback exporter — `SEMA_OTEL_FILE`.** A custom `SpanExporter`
implementing the 0.32 trait:
```rust
// export is NOT async fn; trait is NOT dyn-compatible (impl Future return).
fn export(&self, batch: Vec<SpanData>) -> impl Future<Output = OTelSdkResult> + Send {
    // serialize each SpanData (opentelemetry_sdk::trace::SpanData) to one JSON line
    // (Sema-defined stable schema: name, trace_id, span_id, parent, start/end,
    //  attributes, status), write '\n', flush.
    std::future::ready(write_result)   // pure sync work, no real await
}
```
Backed by `Mutex<BufWriter<File>>` opened with `OpenOptions::append(true).create(true)`.
Register via `.with_simple_exporter()` for deterministic immediate JSONL capture
(or `.with_batch_exporter()` to keep I/O off the hot path). `force_flush`/`shutdown`
flush the `BufWriter`. Independent of any collector — the offline capture path.
Prefer this over `opentelemetry-stdout` (whose format is explicitly unspecified).

**(g) Install-nothing when fully off.** `SEMA_OTEL_FILE` present → add file
processor; OTLP endpoint present → add OTLP processor; **neither** → install NO
tracer provider at all (the default `NoopTracerProvider` does nothing; zero hot-path
cost, no `global::set_tracer_provider` call).

**(h) Regression guard.** Integration test: point the OTLP exporter at a closed port
(`127.0.0.1:1`) and assert a span-emitting Sema script still completes within normal
time bounds and exits cleanly — proves the down-collector-never-blocks invariant.

### 3.4 Single span site + a thin facade

`sema-otel` exposes a small, sync, **no-op-when-disabled** facade:

```rust
// sema-otel public surface (sketch)
pub fn init_from_env() -> Option<OtelGuard>;   // None unless a sink is configured
pub struct OtelGuard;                           // force_flush + shutdown_with_timeout on Drop

// RAII spans — Drop ends the span and records duration (the span Instant-times itself)
pub struct LlmSpan { /* holds a BoxedSpan or None */ }
pub fn llm_span(op: &str) -> LlmSpan;           // started before dispatch; provider/model set later
impl LlmSpan {
    pub fn set_request(&self, temp: Option<f64>, max_tokens: Option<u32>);
    pub fn set_dispatch(&self, provider: &str, req_model: &str); // called AFTER provider resolved
    pub fn set_response(&self, resp: &ResponseFacts);            // tokens, finish, cost, cache_hit
    pub fn record_error(&self, kind: &str, msg: &str);
    pub fn set_messages(&self, input: &str, output: &str, system: Option<&str>); // gated internally
}
pub fn tool_span(name: &str, call_id: &str) -> ToolSpan;
pub fn agent_span(name: &str) -> AgentSpan;
pub fn vm_span(name: &str) -> VmSpan;
```

`ResponseFacts` is a plain `Send` struct (`input_tokens`, `output_tokens`,
`response_model`, `finish_reason`, `cost_usd`, `cache_hit`) so `sema-otel` need not
depend on `sema-llm` types — `sema-llm` maps `ChatResponse`/`Usage` into it. When no
sink is configured, every constructor returns a span holding `None`; all methods
early-return (branch-predictable, near-zero cost). **The OTel layer never touches
Sema `Value`/`Env`/`Rc`** — the call site serializes plain owned data and hands it
across; the background export thread never sees an `Rc`.

**Where the LLM span lives (corrects the old `do_complete` claim).** Provider and
model are only known AFTER dispatch (`set_serving_provider` at `:4334` /`:4349`), and
both `take_serving_provider` (`:96`) and `track_usage` (`:223`) **CONSUME** the
serving-provider stamp via `.take()`. So:

- **Start** the `llm_span` in `do_complete` (`:4137`) so cache-hit calls
  (early-return at `:4158-4179`) still get a span tagged `gen_ai.cache.hit=true`
  with zero usage.
- **Capture provider + model + response together INSIDE
  `do_complete_uncached` (`:4340`) and `do_complete_with_provider` (`:4313`)**,
  where the resolved provider name and `resp` are both in scope, BEFORE
  `track_usage` runs — do NOT rely on a non-consuming peek at `do_complete` level
  (it would steal the stamp from `track_usage` or get `None`).
- Use `response.model` for `gen_ai.response.model` (authoritative) and the resolved
  `request.model` for `gen_ai.request.model`.
- The retry loop is `complete_with_retry` (`:4290`); its `attempt` count is
  **discarded**. Per-HTTP-attempt sub-spans (retries visible) require instrumenting
  inside `complete_with_retry` — deferred (see Open items).

### 3.5 Context propagation (parent/child nesting)

The VM is single-threaded; keep the active span in a **thread-local stack** inside
`sema-otel` (one `RefCell<Vec<Context>>`). `agent_span`/`vm_span` push; child
`llm_span`/`tool_span` attach to the top as parent; `Drop` pops. This reproduces
`invoke_agent` → `chat`/`execute_tool` → `chat` nesting without async machinery.
Propagate trace-level attrs (`gen_ai.conversation.id`, optional Sema session id) to
every child span — Langfuse silently breaks filtering/aggregation otherwise. (If
async LLM calls ever land, switch to `opentelemetry::Context` attach/detach guards;
the facade signatures do not change.)

### 3.6 VM-general spans (beyond LLM)

`vm_span(name)` (INTERNAL) wraps notebook-cell evaluation, `load`/`import`, or a
`sema run <file>` top-level — the "usable for the VM generally" requirement.

---

## 4. Exposing it to Sema users

### 4.1 Off by default, env-driven

| Env var | effect |
|---------|--------|
| `OTEL_EXPORTER_OTLP_ENDPOINT` | standard OTLP endpoint — **presence enables OTLP export** (e.g. `http://localhost:4318`) |
| `OTEL_EXPORTER_OTLP_PROTOCOL` | `http/protobuf` (default) \| `http/json` \| `grpc` — dispatched in code (§3.2) |
| `OTEL_EXPORTER_OTLP_HEADERS` / `_TIMEOUT` | standard OTLP knobs (Langfuse auth, short timeout) |
| `SEMA_OTEL_FILE=path` | opt-in JSONL file capture, independent of any collector (§3.3f) |
| `OTEL_SERVICE_NAME` | resource service name (default `sema`) |
| `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true` | **standard** content-capture flag (alias: `SEMA_OTEL_CAPTURE_CONTENT`); default OFF |
| `OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental` | opt into latest experimental GenAI semconv |
| `OTEL_BSP_MAX_QUEUE_SIZE` / `_MAX_EXPORT_BATCH_SIZE` / `_SCHEDULE_DELAY` | batch processor bounds (§3.3b) |

If **neither** an OTLP endpoint nor `SEMA_OTEL_FILE` is set, no provider is installed
(zero-cost no-op). Honoring the standard `OTEL_*` vars means users point Sema at
local Jaeger (one `docker run`), hosted Langfuse, or Datadog with no code change.

### 4.2 Initialization & shutdown

- **CLI / REPL / notebook server:** call `sema_otel::init_from_env()` once near the
  top of `main()` in `crates/sema/src/main.rs`, holding the returned `OtelGuard` for
  the process lifetime; its `Drop` does the bounded flush+shutdown (§3.3e).
- **Embedders** (`crates/sema/src/lib.rs` `Interpreter`): document that hosts call
  `init_from_env()` themselves; do NOT init inside `Interpreter::new()`. Note:
  `reset_runtime_state` (`builtins.rs:115`) runs on EVERY interpreter build (3 sites:
  `eval.rs:67`, `eval.rs:85`, `lib.rs:85`), so OTel init MUST be idempotent (a
  `std::sync::Once` for the global provider, or a thread-local guard) — naive
  re-init would leak/duplicate tracer providers. Spans are no-ops if no provider was
  installed, so this is safe.

### 4.3 Optional Sema-level surface (Milestone 3, not MVP)

- `(otel/span name f)` — run thunk `f` inside a named INTERNAL span; returns `f`'s
  value (Resolved Decision #4).
- `(otel/event name attrs-map)` — add a span event to the current span.

Thin wrappers over the `sema-otel` facade, registered in `sema-stdlib` behind the
enabled flag (no-op when off). `sema-stdlib` depends on `sema-otel`, never on
`sema-eval`/`sema-llm`.

---

## 5. Phased task list

### Milestone 0 — `sema-otel` crate skeleton (½–1 day)
- **Files:** new `crates/sema-otel/{Cargo.toml,src/lib.rs}`; workspace `Cargo.toml`
  add `sema-otel = { version = "=1.21.1", path = "crates/sema-otel" }` to
  `[workspace.dependencies]` (mirror the existing `=1.21.1` pins at lines 29–40).
- **Content:** `init_from_env()` reading the §4.1 table; the §3.2 transport
  dispatch; default thread-based `BatchSpanProcessor`; the §3.3f JSONL file
  exporter; install-nothing-when-off (§3.3g); `OtelGuard` doing bounded
  flush+shutdown on `Drop` (§3.3e). The RAII facade
  (`LlmSpan`/`ToolSpan`/`AgentSpan`/`VmSpan`) with the thread-local parent stack and
  the Sema→`gen_ai.provider.name` mapping module. All constructors return no-op
  spans when no sink is configured.
- **Acceptance:** `SEMA_OTEL_FILE=/tmp/t.jsonl` + a unit test that opens and drops
  an `LlmSpan` writes one JSON line; with no sink set, the same path installs no
  provider and a microbench shows span create/drop is a predictable no-op branch
  (no exporter, no alloc). The §3.3h closed-port test (`127.0.0.1:1`) passes.
  `make lint` clean.

### Milestone 1 (MVP) — LLM-call spans (1 day)
- **Files:** `crates/sema-llm/Cargo.toml` add `sema-otel` (wasm-excluded like the
  existing tokio/reqwest usage); `crates/sema-llm/src/builtins.rs`:
  - **Start** `llm_span("chat")` in `do_complete` (`:4137`) so cache hits get a
    span. On the cache-hit early-return (`:4158-4179`), tag `gen_ai.cache.hit=true`,
    zero usage, finish, and return.
  - **Capture** provider/model/response INSIDE `do_complete_uncached` (`:4340`) and
    `do_complete_with_provider` (`:4313`) — `set_dispatch(provider, req_model)` then
    `set_response(ResponseFacts)` (incl. `cost_usd` via
    `pricing::calculate_cost_for`, `response.model`, `finish_reasons` as a
    1-element array) BEFORE `track_usage` (`:220`) consumes the serving-provider
    stamp.
- **Operation name:** `chat` for completions; `embeddings` deferred to M4.
- **Acceptance:** with local Jaeger (`docker run jaegertracing/all-in-one`),
  `OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 sema -e '(llm/auto-configure)(llm/complete "ping" {:max-tokens 10})'`
  produces one CLIENT span `chat <model>` carrying `gen_ai.provider.name`,
  request/response model, input/output tokens, finish reason, and
  `gen_ai.usage.cost_usd` matching `llm/last-usage`. A cache-hit repeat produces a
  span with `gen_ai.cache.hit=true` and zero usage. **Deterministic CI test:** a
  test-only `FakeProvider` (impl `LlmProvider`; cross-ref
  `2026-06-21-llm-cassettes.md`) + `opentelemetry_sdk` `InMemorySpanExporter`,
  asserting span attributes — no network/keys.

### Milestone 2 — tool + agent spans (1 day)
- **Files:** `builtins.rs`:
  - `run_tool_loop` (`:4516`): open an `agent_span` (INTERNAL) for the whole loop
    (named from the agent, or `invoke_agent` bare). Agent entry points:
    `agent/run` → `run_tool_loop` at `:2168`; `llm/complete`-with-tools →
    `run_tool_loop` at `:1468`.
  - Wrap each `execute_tool_call` (`:4640`) in a `tool_span` carrying
    `gen_ai.tool.name` (`tc.name`), `gen_ai.tool.call.id` (`tc.id`),
    `gen_ai.tool.type="function"`, `gen_ai.tool.description`. Record `error.type`
    when `is_error` (`:4584-4593`). Re-use the already-measured `duration_ms`
    (`:4594`) for the tool span (this is the one real latency source).
  - Stamp `gen_ai.conversation.id` on every child span.
- **Acceptance:** an `agent/run` with one tool (FakeProvider scripted: tool_call →
  final answer) produces the trace tree `invoke_agent` →
  (`chat`, `execute_tool <name>`, `chat`) with correct parent/child nesting and the
  tool's `call.id`. Verified via the in-memory exporter in CI.

### Milestone 3 — metrics + content capture + Sema surface (1 day)
- **Files:** `sema-otel/src/lib.rs` (`SdkMeterProvider` + the two histograms with
  the §2 units and explicit bucket boundaries, recorded from
  `LlmSpan::set_response`; duration in seconds = `duration_ms/1000`); content
  capture gated by `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT` in
  `set_messages` (called from the LLM/agent sites), span-attribute fallback mode,
  truncated + scrubbed; `crates/sema-stdlib/src/*.rs` registering `(otel/span …)`
  and `(otel/event …)`.
- **Acceptance:** `gen_ai.client.token.usage` and `…operation.duration` appear in
  the metrics pipeline (file/console or OTLP); with the content flag set the span
  carries `gen_ai.input.messages`/`output.messages` and **without it those are
  absent** (privacy test). `(otel/span "x" (fn () 42))` returns `42` and emits one
  INTERNAL span. Eval test pinning the flag-off no-op returning `42`.

### Milestone 4 (vision) — notebook trace + streaming + embeddings (1–2 days)
- **Files:** `crates/sema-notebook/src/*` — one root trace per "Run All" with a
  child `vm_span` per cell (shared cell env makes this natural), LLM/tool spans
  nested beneath the issuing cell, `gen_ai.conversation.id` propagated; single-cell
  eval = standalone one-cell trace. `builtins.rs` `llm/stream` (`:1565/1579`) —
  bypasses `do_complete`, add a span there. `embed` paths — `embeddings` operation
  spans with `input_tokens` only.
- **Acceptance:** the demo notebook with OTel enabled yields one trace whose root
  has one child per executed cell, LLM/tool spans nested beneath; a streamed
  completion produces a span; `llm/embed` produces an `embeddings` span with input
  token usage.

### Milestone 5 — docs (½ day)
- **Files:** new `website/docs/llm/observability.md` (` ```sema ` blocks); link from
  `website/docs/llm/index.md`; a short `README.md` section; document the env vars, a
  `docker run jaegertracing/all-in-one` walkthrough, a Langfuse OTLP quickstart, and
  the `SEMA_OTEL_FILE` offline-capture mode.
- **Acceptance:** docs build; the documented Jaeger walkthrough reproduces a trace
  end-to-end on a clean machine.

---

## 6. Minimal first milestone vs full vision

**MVP (Milestones 0–1):** an opt-in OTel path (OTLP endpoint or `SEMA_OTEL_FILE`)
that emits one spec-compliant `chat {model}` CLIENT span per non-streaming LLM call
(incl. cache-hit spans) with tokens, finish reason, cost, and self-measured latency,
exportable to any OTLP backend, with a deterministic in-memory CI test (no keys) and
the closed-port fail-safe test. The single highest-leverage observability win.

**Full vision (Milestones 2–4):** the complete `invoke_agent` → `execute_tool` →
`chat` trace tree with the two GenAI metrics, privacy-gated content capture, a
Sema-level `(otel/span …)` surface, streaming + embeddings coverage, and the notebook
"one trace per run" experience.

---

## 7. Codebase-fit notes (idiomatic to this repo)

- **Span site, not "one chokepoint".** `do_complete` (`:4137`) centralizes cache →
  fallback → retry, but provider/model are only known after dispatch and both
  `take_serving_provider` (`:96`) and `track_usage` (`:223`) consume the
  serving-provider stamp — so start the span at `do_complete` (covers cache hits)
  and capture provider/model/response inside `do_complete_uncached` (`:4340`) /
  `do_complete_with_provider` (`:4313`) (§3.4). Streaming/embeddings are NOT covered
  by `do_complete` (M4).
- **Measure LLM latency fresh; reuse tool latency.** There is no existing wall-clock
  around the LLM call — the `llm_span` `Instant`-times itself. The tool span reuses
  the already-measured `duration_ms` (`:4594`).
- **Dependency direction preserved.** `sema-otel` (sema-core + otel only) sits below
  `sema-llm`/`sema-stdlib`; the CLAUDE.md "stdlib/llm must not depend on each other
  or on eval" rule holds.
- **wasm stays clean.** `opentelemetry-otlp` is `cfg(not(wasm32))`-gated like
  `sema-llm`; the playground build is unaffected; `sema-core` gains no otel dep.
- **Drop-safety.** The default thread-based batch processor needs no tokio runtime
  on the HTTP path; only the gRPC arm reuses `BlockingRuntime` for construction.
  `OtelGuard::drop` does the bounded flush+shutdown, never from a current-thread
  runtime's own thread, mirroring `http.rs` non-blocking-`Drop` discipline.
- **Idempotent init.** `reset_runtime_state` (`:115`) runs on every interpreter
  build (3 sites); OTel init is guarded by `Once`/thread-local so providers are not
  duplicated.
- **`Rc`/single-thread reality.** Parent/child nesting uses a thread-local span
  stack; the OTel layer only ever receives plain `Send` owned data, never an `Rc`.
- **Mock-first testing.** Deterministic CI rides `FakeProvider` (cross-ref
  `docs/plans/2026-06-21-llm-cassettes.md`) + `InMemorySpanExporter`, plus the
  closed-port fail-safe test — the whole telemetry path verified with no network and
  no API keys.

---

## 8. Open items (human calls / parked)

1. **Anthropic prompt-cache token attributes** (`gen_ai.usage.cache_creation.input_tokens`
   / `cache_read.input_tokens`) need a real code change: `AnthropicUsage`
   (`anthropic.rs:378`) and the shared `Usage` struct currently DROP cache fields.
   Include this small provider/type change in the OTel work, or ship MVP without
   cache-token attributes and do it separately?
2. **gRPC as a cargo feature vs always-compiled.** `grpc-tonic` pulls the full
   tonic/hyper/h2/tower/tokio stack into an otherwise HTTP-light path. Compile gRPC
   by default, or gate it behind a feature (functionality identical, only build cost
   differs)?
3. **Retry sub-spans + streaming span scope.** Per-HTTP-attempt sub-spans inside
   `complete_with_retry` (`:4290`, attempt count currently discarded) and the
   streaming span (`llm/stream` bypasses `do_complete`) — fold into M4 vision scope
   or park as follow-ups? Both need small code changes and are not required for MVP.
