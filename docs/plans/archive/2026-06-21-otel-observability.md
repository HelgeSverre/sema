# OpenTelemetry Observability for LLM/Agent Runs (and the VM)

**Status:** IMPLEMENTED (M0–M5), 2026-06-22. All milestones shipped + verified
(deterministic in-memory-exporter tests + live end-to-end through the CLI file
exporter). See the `sema-otel` crate and the `otel_*` integration tests.
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

These sixteen questions are answered; do not re-litigate during implementation.
(Decisions #11–#16 are the embedded-OTel / wasm calls resolved 2026-06-21.)

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
   coupling). **gRPC is ALWAYS compiled (no cargo feature gate)** (owner decision,
   2026-06-21): the heavy transitive stack (`hyper`/`h2`/`tower`/`hyper-util`/
   `tokio-stream`) is already in the workspace via `reqwest`+`axum`, so gRPC's
   net-new cost is only ~5-8 crates (`tonic`/`prost`) + ~0.5-1 MB binary; the
   zero-config "`OTEL_EXPORTER_OTLP_PROTOCOL=grpc` just works" UX wins. Details in
   §3.1 / §3.2.

9. **Cache-token attributes + Sema `Usage` expansion (was Open item #1; owner
   decision: include + surface in Sema).** Add `cache_creation_input_tokens` and
   `cache_read_input_tokens` to the shared `Usage` struct (`types.rs`), populate
   them from providers that report cache usage (Anthropic `AnthropicUsage`,
   `anthropic.rs`; others as available), and surface them **both** in the
   Sema-facing usage maps (`llm/session-usage`, `llm/last-usage` — useful for cost
   analysis independent of OTel) **and** as span attributes
   `gen_ai.usage.cache_creation.input_tokens` / `gen_ai.usage.cache_read.input_tokens`.
   This is a small provider/type change done as part of the OTel work.
   **STATUS — Sema-side SHIPPED (2026-06-21, pre-OTel):** the `Usage` fields,
   provider population (OpenAI `prompt_tokens_details.cached_tokens`; Gemini
   `cachedContentTokenCount`; Anthropic distinct cache fields — non-streaming AND
   streaming), session accumulation, and `:cache-read-tokens` / `:cache-creation-tokens`
   in both usage maps are done, tested (FakeProvider regression), and live-verified
   (OpenAI + Gemini implicit reads). Remaining for OTel: emit the two
   `gen_ai.usage.cache_*` span attributes from these already-populated fields.
   Note: Anthropic caching is opt-in via request-side `cache_control` markers,
   which are NOT yet wired — the parser is ready but cache counts stay 0 until a
   future `:cache`-enable feature lands (parked, separate from OTel).

10. **Retry sub-spans + streaming span are IN v1 (was Open item #3; owner decision:
    include both).** Per-HTTP-attempt child spans inside `complete_with_retry`
    (`builtins.rs:4290` — surfaces 429/5xx retries + backoff; the attempt count is
    currently discarded, so thread span context through) AND a streaming-call span
    for `llm/stream` (which bypasses `do_complete`) are part of the initial
    implementation, not parked. Folded into the milestones below.

11. **WASM/JS = pure compile-out no-op for v1 (was §9.3 Q#1; owner decision,
    2026-06-21).** The browser build has no span *source* — every LLM/GenAI builtin is
    already `cfg(not(target_arch="wasm32"))`-gated, like `sema-llm`. So on wasm the
    `sema-otel` facade compiles to no-op spans and links no `opentelemetry-otlp` /
    `BatchSpanProcessor` / `std::fs`. The JS-bridge exporter (`setSpanSink(fn)` +
    `JsBridgeExporter`) is **deferred** until in-browser LLM/agent runs exist. The
    "always compile both OTLP transports" (#8) and `SEMA_OTEL_FILE` JSONL fallback
    (#6) are scoped to **native non-wasm** targets. Add a build-regression test that
    `sema-wasm` still builds with the otel facade present.

12. **Embedded default = attach to the host's ambient provider (was §9.3 Q#2; owner
    decision, 2026-06-21).** An embedded `Interpreter` emits to whatever **global** OTel
    provider the host already installed, and is a silent no-op if the host installed
    nothing. Sema spans "just appear" in a host that already runs OTel, with zero host
    wiring. Sema NEVER installs/hijacks a provider in this mode (see #14).

13. **Commit to trace-context continuity via `Context::current()` (was §9.3 Q#3; owner
    decision, 2026-06-21).** Sema's root span is started with
    `tracer.start_with_context(name, &opentelemetry::Context::current())` so Sema's
    LLM/tool spans auto-nest under the host's current span (e.g. an HTTP request span).
    This is the single highest-value embedded behavior and is worth coupling
    `sema-otel` to the `opentelemetry` `Context` API (a deliberate exception to §3.4's
    otherwise-minimal facade). On wasm this is a no-op (#11).

14. **`build()` never owns the provider (was §9.3 Q#4; owner decision confirmed,
    2026-06-21).** `InterpreterBuilder::build()` MUST NOT install or shut down a tracer
    provider. In the `FromEnv` self-install path, the `OtelGuard` (which owns
    flush/shutdown) is **handed back to the host to own**, so `reset_runtime_state` on
    every build can never leak or duplicate providers. Host owns lifecycle; Sema only
    emits.

15. **Direct-OTel only — no `tracing`-bridge variant for v1 (was §9.3 Q#5; owner
    decision, 2026-06-21).** `sema-otel` emits raw `opentelemetry` spans, parented via
    `Context` (#13). We do NOT ship a `tracing`/`tracing-opentelemetry` re-emit variant
    in v1 (one code path; revisit if `tracing`-based hosts need auto-nesting in their
    own subscriber).

16. **Env config defers to a host-installed provider (was §9.3 Q#6; owner decision,
    2026-06-21).** If a host has already set a global OTel provider AND Sema is
    env-configured (`SEMA_OTEL_*` present) in `FromEnv` mode, Sema **detects the
    existing global provider and declines to overwrite it** — it emits into the host's
    pipeline rather than clobbering host telemetry. (No warning required; defer
    silently. A diagnostic breadcrumb is optional.)

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
>   (`builtins.rs:1565/1579`) — add a streaming span in **M2** (in v1 scope per
>   Decision #10).
> - **Retries** (`complete_with_retry`, `:4290`) — emit a child span per HTTP
>   attempt (429/5xx + backoff) under the LLM span; in **M1** scope per Decision #10.
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

> Transport build (resolved, Decision #8): **compile both, no cargo feature gate.**
> `grpc-tonic`'s heavy transitive stack (`hyper`/`h2`/`tower`/`hyper-util`/
> `tokio-stream`) is already in the workspace via `reqwest`+`axum`, so the net-new
> cost is only ~5-8 crates (`tonic`/`prost`) + ~0.5-1 MB binary. `grpc` works with
> zero config out of the box.

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
  - **Retry sub-spans (Decision #10):** in `complete_with_retry` (`:4290`), open a
    short child span per HTTP attempt tagged with the attempt number and outcome
    (429/5xx/network + the backoff applied), nested under the LLM span.
- **Cache-token `Usage` expansion (Decision #9):** add `cache_creation_input_tokens`
  / `cache_read_input_tokens` to `Usage` (`types.rs`), populate from `AnthropicUsage`
  (`anthropic.rs`), surface in `llm/session-usage` + `llm/last-usage` maps, and emit
  `gen_ai.usage.cache_creation.input_tokens` / `gen_ai.usage.cache_read.input_tokens`
  on the span when present. (Small provider/type change; useful beyond OTel.)
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
  - **Streaming span (Decision #10):** `llm/stream` (`:1565/1579`) bypasses
    `do_complete`/`track_usage`, so wrap it directly in a `chat` span that captures
    model + whatever usage the stream surfaces at completion.
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

### Milestone 4 (vision) — notebook trace + embeddings (1–2 days)
- **Files:** `crates/sema-notebook/src/*` — one root trace per "Run All" with a
  child `vm_span` per cell (shared cell env makes this natural), LLM/tool spans
  nested beneath the issuing cell, `gen_ai.conversation.id` propagated; single-cell
  eval = standalone one-cell trace. `embed` paths — `embeddings` operation spans
  with `input_tokens` only.
  (Streaming spans moved to M2 and retry sub-spans to M1 per Decision #10.)
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

All previously-open items are now resolved (owner decisions, 2026-06-21) and
captured in §0:
- Anthropic cache-token attributes → **include + surface in Sema `Usage`** (Decision #9).
- gRPC transport → **always compiled, no feature gate** (Decision #8).
- Retry sub-spans + streaming span → **in v1 scope** (Decision #10).

No open questions remain. Ready to implement.

---

## 9. Embedded OTel (host Rust app + WASM/JS)

§§0–8 are scoped to the **standalone CLI/REPL/notebook-server** shape: Sema owns the
process, so it legitimately owns the global tracer provider and the shutdown
sequence. That model is correct for that mode and **nothing in §§0–8 changes**. But
Sema also ships as (2) an embeddable Rust library (`crates/sema/src/lib.rs`
`Interpreter`/`InterpreterBuilder`) and (3) a browser/WASM runtime (`sema-wasm`,
sema.run). In both, the standalone init model is wrong-to-harmful. This section
extends — never overrides — the locked decisions for those two modes.

The unifying principle (matches Decision #6's "silent no-op default" and the
opentelemetry-rust library contract): **libraries instrument, applications
configure.** `sema-otel` should *emit* GenAI-conventioned spans against whatever
provider exists and never *install* one except in standalone mode. opentelemetry's
own docs are explicit: `global::set_tracer_provider` "Libraries should NOT call this
function. It is intended for applications/executables." When no provider is set,
`global::tracer(...)` returns a `NoopTracerProvider` that silently discards — so the
facade can call it unconditionally and pay nothing when unconfigured.

### 9.1 The three deployment modes at a glance

| Mode | Who calls init | Who owns the global provider | Who owns shutdown | Export backend |
|------|----------------|------------------------------|-------------------|----------------|
| **1. Standalone** (CLI/REPL/notebook server) | `sema_otel::init_from_env()` in `main()` (§4.2) | **Sema** (installs it) | **Sema** (`OtelGuard::drop`, §3.3e) | thread-based `BatchSpanProcessor` → OTLP / `SEMA_OTEL_FILE` JSONL |
| **2. Embedded-in-Rust** (`Interpreter`) | the **host app** (or Sema self-installs via explicit opt-in) | the **host** (Sema attaches, does NOT hijack) | the **host** | host's own processor/exporter; or host hands Sema a provider |
| **3. WASM/JS** (`sema-wasm`) | nobody (no provider) | n/a | n/a | **none** (compile-out no-op); future JS-callback bridge → host JS OTel SDK |

The crate stays one sink-agnostic facade. What differs per mode is **wiring**, not
**logic**: who calls init, who owns the provider, who owns shutdown. The mode-agnostic
locked decisions (semconv, cost attributes, both OTLP transports, JSONL fallback,
zero `observe-llm` coupling) survive unchanged — but several are inherently
standalone-only and must be re-scoped where the runtime can't honor them (noted
inline below).

### 9.2 Mode 2 — embedded in a host Rust app

A host Rust app almost always already runs OpenTelemetry (or `tracing` +
`tracing-opentelemetry`) with its own global provider, batch processor, and shutdown.
The embedding contract therefore inverts the standalone one:

**(a) Never hijack the global from a library default.** `Interpreter::new()` /
`InterpreterBuilder::build()` MUST NOT call `global::set_tracer_provider` (nor
`set_meter_provider` / `set_text_map_propagator`). Doing so either clobbers a host
that already configured OTel (host spans silently stop exporting) or gets clobbered
when the host configures later — last-writer-wins, no error. This is the central
embedding bug to avoid, and it is the natural failure mode of the raw-`opentelemetry`
design chosen in §3.1 (raw OTel reaches for the global; a `tracing`-based lib owns no
global). This also already aligns with the *existing* codebase reality: an exhaustive
grep found **zero** `set_global_default` / `set_tracer_provider` / `tracing_subscriber`
calls anywhere in non-test code — Sema installs no global telemetry today, so there is
no conflict yet. The risk is entirely about what `sema-otel` *would* install.

**(b) Bring-your-own provider + read-the-global default.** The default embedded
behavior is to emit spans against whatever the host already installed via
`opentelemetry::global` (silent no-op if the host installed nothing — same contract
as standalone-off). Make the facade resolve its tracer **lazily** via
`global::tracer_with_scope(InstrumentationScope::builder("sema").with_version(…).build())`
rather than a captured/owned provider, so it transparently honors the host. Offer two
explicit opt-ins for hosts that want more control (mirroring the
`.with_sandbox`/`.with_llm` builder ergonomics):
   - `UseHostGlobal` (default) — read `global`, install nothing.
   - `OwnProvider(SdkTracerProvider)` — host hands Sema a provider it built; Sema
     uses it but still does NOT call `set_tracer_provider`, and does NOT install a
     shutdown-owning `OtelGuard` (the host owns lifecycle).
   - `FromEnv` — Sema self-installs the standalone path and hands the resulting
     `OtelGuard` **back to the host** to own (for hosts that genuinely want Sema to
     own OTel).

**(c) Parent-context adoption (the highest-value embedded behavior).** §3.5's
thread-local `RefCell<Vec<Context>>` reproduces nesting *within* Sema but currently
seeds from empty, so embedded Sema starts a **new root trace** instead of nesting
under the host's request span. Fix: seed the bottom of the stack from
`opentelemetry::Context::current()` and start Sema's root span via
`tracer.start_with_context(name, &Context::current())`. Then a host HTTP request span
becomes the parent of Sema's `invoke_agent` → `chat`/`execute_tool` tree — the whole
point of embedding observability. (W3C `traceparent` adoption falls out for free if
the host installed a propagator.)

**(d) Idempotent, never auto-init at build time.** `reset_runtime_state`
(`builtins.rs:115`) runs on **every** interpreter build (3 sites: `eval.rs:67`,
`eval.rs:85`, `lib.rs:85`), and it wipes per-thread LLM/provider state. So (i) any
self-install must be `std::sync::Once`-guarded *and* application-scoped (already noted
in §4.2 — extended here: the guard is necessary but insufficient; the global *write*
must also be restricted to the `FromEnv`/standalone path), and (ii) do NOT park OTel
config where `reset_runtime_state` destroys it. **Scope the OTel handle per
`EvalContext`** (like the eval/call callbacks already are), NOT in a thread-local or
global — that matches the documented per-interpreter isolation contract
(`embedding.md`) and survives `build()`.

**(e) Drop-safety inside a host async context.** A host may call Sema from inside its
own tokio runtime (the MCP-server drop-panic case in `http.rs:11-16` is a real, hit
scenario). If Sema self-installs (`FromEnv`), its `OtelGuard::drop`
flush+shutdown must mirror `BlockingRuntime`'s non-blocking-`Drop` /
`shutdown_background` discipline (`http.rs:35-41`) and never block on the host's
runtime thread. In `UseHostGlobal`/`OwnProvider` modes Sema installs no guard at all,
so there is nothing to shut down — the host owns it.

**(f) Send/!Send + LLM-state caveats.** The eval path is `Rc`-based and `!Send`
(`Value`/`Env`/`EvalContext`); OTel plumbing must not require `Send`/`Sync` on the
eval path — the facade already hands only plain `Send` `ResponseFacts` to the export
thread (§3.4), keep it that way. Note also that GenAI/LLM telemetry granularity is
inherently **per-thread**, not per-interpreter: the provider registry / usage /
budgets it draws from are thread-local (`builtins.rs:29-97`) and shared across all
interpreters on a thread. Decision #7's zero-`observe-llm` coupling already keeps the
span layer thin here; just be aware the *data source* is per-thread.

**Mode-2 API/config surface (additive to §4):**
- `sema-otel` facade gains a non-installing init variant:
  `sema_otel::use_host_global()` (read `global`, no install) and
  `sema_otel::use_provider(SdkTracerProvider)` (BYO, no install). `init_from_env()`
  remains the **only** function that may call `set_tracer_provider`, documented
  application-only.
- Facade tracer resolution switches to lazy `global::tracer_with_scope("sema")`.
- Root-span constructors (`vm_span`/`agent_span`, and `llm_span` when no Sema parent
  exists) seed parent from `Context::current()`.
- `InterpreterBuilder::with_telemetry(TelemetryMode)` where
  `TelemetryMode ∈ { Off (default), UseHostGlobal, OwnProvider(SdkTracerProvider),
  FromEnv }`; `Interpreter::new()`/`build()` stay a guaranteed no-op that never touches
  global OTel state. Store the chosen scope per `EvalContext`.
- `website/docs/embedding.md` documents: hosts own init + shutdown; Sema never
  installs a global provider on its own; the four `TelemetryMode`s and the
  parent-nesting behavior.

### 9.3 Mode 3 — WASM / browser (sema.run)

The standalone export machinery is **structurally impossible** on
`wasm32-unknown-unknown`, and three locked decisions cannot hold there as written:

- **No OS threads** → the thread-based `BatchSpanProcessor` (Decision #6 / §3.3a, the
  "dedicated background OS thread") cannot exist. WASM is single-threaded `Rc`; the
  only concurrency is the JS event loop + an optional Web Worker.
- **No `tokio`, no `reqwest`** in the wasm dependency closure → neither OTLP transport
  from Decision #8 (`http-proto` via reqwest-blocking, `grpc-tonic`) can build. The
  reqwest path additionally fails to *compile* (wasm `reqwest::Response` /
  `JsFuture` internals are `!Send`, violating the exporter's `Send + Sync` bounds —
  upstream `open-telemetry/opentelemetry-rust#3155`, still open, no committed fix).
- **No filesystem** → `SEMA_OTEL_FILE` (Decision #6 / §3.3f) has no target; the wasm
  VFS is in-memory `BTreeMap` only.
- **`std::time::Instant` panics** on wasm32 ("time not implemented") — any duration
  math would have to route through `js_sys::Date` / `performance.now()`.

Crucially, **there is nothing to observe in the browser anyway**: `sema-llm` is fully
compiled out of wasm (`register_llm_builtins` is `#[cfg(not(target_arch="wasm32"))]`
at `eval.rs:65`; `sema-llm` depends unconditionally on tokio+reqwest), so the entire
GenAI span surface this plan instruments has **no source** in the wasm build. The
`http`/`io`/`server`/etc. stdlib modules are likewise `cfg(not(wasm32))`-gated
(`sema-stdlib/src/lib.rs:11-38`).

**Recommended minimal viable behavior (MVP): pure compile-out no-op.** Under
`cfg(target_arch = "wasm32")` the `sema-otel` facade compiles to no-op spans
(constructors return `None`-holding spans; `otel/span` / `otel/event` remain callable
and inert in the playground), and **none** of `opentelemetry-otlp` / `tokio` /
`std::fs` / `BatchSpanProcessor` is linked. This mirrors the existing `sema-llm` wasm
exclusion, keeps the sema.run bundle small, dodges #3155 / the `Instant` panic / the
missing-fs problem, and is the correct default per Decision #6. Re-scope the
standalone decisions for wasm: "always compile both OTLP transports" becomes "on
**native non-wasm** targets"; the `SEMA_OTEL_FILE` JSONL fallback is a no-op on wasm.
`sema-core` must stay a tokio-free leaf (§3.1 already states this) — do not let any
otel dep reach it.

**Nicer option (post-MVP, only if browser tracing is ever wanted): a JS-callback
bridge, not in-wasm export.** Expose a wasm-only sink, e.g. `setSpanSink(fn)` /
`setOtelSink(fn)`, mirroring the existing `setOutputSink(fn)` (`lib.rs:2803`) and
`registerFn(name, fn)` (`lib.rs:2437`) callback pattern (store a `js_sys::Function`
in a thread-local). Sema emits each finished span as a plain JS object (name,
trace/span/parent ids, `performance.now()`-based timing, attributes incl. `gen_ai.*`)
to that callback; the host page forwards them to its own
`@opentelemetry/sdk-trace-web` + `exporter-trace-otlp-http` SDK, which already solves
OTLP-JSON encoding, batching tuned for page-unload, retry, CORS, CSP, and W3C
context. This is the idiomatic Rust-wasm pattern (`tracing-wasm`/`tracing-web` are all
JS-bridge layers, none export OTel from inside wasm) and needs no new heavy deps —
`sema-wasm`'s `Cargo.toml` already enables the `web-sys` fetch features. Reuse the
SpanData→stable-JSON serializer written for the `SEMA_OTEL_FILE` exporter (one
schema, two sinks). A Rust-side in-wasm `fetch` exporter (`SimpleSpanProcessor` +
hand-written `web_sys::fetch` exporter + `web-time` + `spawn_local` to dodge `!Send`)
is technically possible but is the **last-resort, high-maintenance** path — prefer the
JS bridge.

**Mode-3 API/config surface:**
- MVP: **zero new surface.** Rely on `cfg(target_arch = "wasm32")` gating; add a
  build-regression test asserting `sema-wasm` builds with the otel facade present,
  produces a no-op, and links no tokio/otlp (the Vercel-Edge bundler pitfall — never
  rely on runtime detection).
- Post-MVP bridge: a wasm-only `setSpanSink(js_sys::Function)` on the `SemaInterpreter`
  wasm interface + a `JsBridgeExporter` behind `cfg(target_arch = "wasm32")`.

### 9.4 Open decisions for the owner — RESOLVED 2026-06-21

All six were answered by the owner and promoted to locked decisions in §0:
Q#1 → **#11** (wasm = compile-out no-op, JS bridge deferred);
Q#2 → **#12** (embedded default = attach to host's ambient provider);
Q#3 → **#13** (commit to `Context::current()` trace continuity);
Q#4 → **#14** (`build()` never owns the provider);
Q#5 → **#15** (direct-OTel only, no `tracing`-bridge variant in v1);
Q#6 → **#16** (env config defers to a host-installed global provider).

The original framing is preserved below for context.

1. **WASM MVP scope.** Confirm wasm/JS = **pure compile-out no-op** for v1, deferring
   the JS-bridge exporter until in-browser LLM/agent runs exist (recommended). I.e.
   we are NOT building the JS bridge now.
2. **Embedded default policy.** Should an embedded `Interpreter` **by default** attach
   to the host's ambient/global provider (recommended — Sema spans "just appear" in a
   host that already runs OTel; silent if the host installed nothing), or stay fully
   inert unless the host explicitly calls `.with_telemetry(…)`?
3. **Trace-context continuity commitment.** Do we commit to seeding the facade's
   parent from `opentelemetry::Context::current()` (§9.2c)? It is the single
   highest-value embedded behavior but couples `sema-otel` directly to the
   `opentelemetry` `Context` API (vs. §3.4's deliberately minimal facade).
4. **Provider/shutdown ownership contract.** Confirm `build()` NEVER installs or shuts
   down a provider, and that in the `FromEnv` self-install path the `OtelGuard` is
   handed back to the host to own (so `reset_runtime_state` on every build cannot
   leak/duplicate providers).
5. **`tracing`-bridge variant (embedded ergonomics).** §3.1 picked raw
   `opentelemetry`. Most Rust hosts standardize on the `tracing` crate +
   `tracing-opentelemetry` (wasmtime/sqlx/reqwest/tonic all emit `tracing` only). Do
   we stay direct-OTel-only (host sees raw OTel spans, parented via `Context`), or also
   ship an optional `tracing`-feature variant that re-emits the same `gen_ai.*` spans
   as `tracing` spans (with `otel.kind`/`gen_ai.*` fields) for auto-nesting in a host's
   existing subscriber? (Caveat: `tracing-opentelemetry` materializes the OTel span at
   tracing-span *close*, so SpanContext can't be read back mid-span.) Affects embedded
   ergonomics only.
6. **Host-provider-already-installed precedence.** If a host has set a global provider
   AND Sema is also configured via env (`SEMA_OTEL_*` present) in `FromEnv` mode,
   should Sema detect the existing provider and **decline to overwrite** (defer to
   host), warn, or proceed? Need the precedence rule between host-installed provider
   and Sema env config.
