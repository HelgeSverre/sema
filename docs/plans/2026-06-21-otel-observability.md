# OpenTelemetry Observability for LLM/Agent Runs (and the VM)

**Status:** Spike / plan (not started)
**Date:** 2026-06-21
**Owner:** unassigned
**Scope:** Standards-compliant, opt-in OpenTelemetry tracing + metrics for Sema's
LLM and agent runs, with a span model usable for the VM generally. No proprietary
SDK lock-in; export to any OTLP backend (Jaeger / Langfuse / Datadog / Honeycomb).

---

## 1. Motivation — why this is table-stakes

Sema markets itself with "stop rewriting the agent loop — the scaffolding *is* the
runtime" (homepage, README). Retries, caching, cost caps, rate limits, and tool
dispatch already live in `sema-llm`. The one piece of agent scaffolding that is
*not* in the runtime is **observability**, and it is the piece that decides whether
an agent framework is trustworthy in production:

- **Cost/latency are invisible per-step.** Today the only signals are the per-call
  `on-tool-call` callback (a 200-char truncated preview) and aggregate
  `llm/last-usage` / `llm/session-usage`. There is no way to answer "which LLM
  call in this agent run was slow?", "how many tokens did this notebook cost?",
  or "which tool errored mid-loop?" without `eprintln` archaeology.
- **`duration_ms` is already measured and thrown away** (`run_tool_loop`,
  `builtins.rs:4480`). The data exists; nothing exports it.
- **The whole industry converged on OTel GenAI semantic conventions.** The CNCF
  GenAI semconv (Development/experimental, riding semconv ~v1.41 as of mid-2026)
  is natively consumed by Datadog, Langfuse, MLflow, and emitted by Claude Code,
  Codex, and Copilot. Adopting it gives Sema standards-compliant token/cost/latency
  tracing across every vendor *for free*, with no bespoke format to maintain.
- **Differentiator for the notebook.** A notebook run can become one inspectable
  trace (cell spans → LLM spans → tool spans), turning "why was this slow / what
  did it cost" into a one-click trace view — a flagship demo for sema.run.

This plan delivers observability at the **single chokepoint** where all provider
calls already funnel (`do_complete` / provider `complete` in `sema-llm`) plus the
tool-dispatch site, so every Sema LLM program gets traced with **zero per-call user
code** and **zero overhead when the flag is off**.

---

## 2. Standard reference — OTel GenAI semantic conventions

The span/metric model we emit (verbatim attribute names from the GenAI semconv):

### Spans

| Span | kind | name | when |
|------|------|------|------|
| LLM call | `CLIENT` | `{gen_ai.operation.name} {gen_ai.request.model}` e.g. `chat claude-sonnet-4-6` | per `complete`/`stream_complete` |
| Tool call | `INTERNAL` | `execute_tool {gen_ai.tool.name}` | per tool dispatch in `run_tool_loop` |
| Agent run | `INTERNAL` | `invoke_agent {agent.name}` | per `agent/run` / `llm/chat` with tools |

### LLM-span attributes

- `gen_ai.operation.name` — `chat` \| `text_completion` \| `embeddings`
- `gen_ai.provider.name` — `anthropic` \| `openai` \| `gemini` \| `ollama` \| ...
- `gen_ai.request.model` / `gen_ai.response.model`
- `gen_ai.request.temperature`, `gen_ai.request.max_tokens`
- `gen_ai.usage.input_tokens`, `gen_ai.usage.output_tokens`
- `gen_ai.response.finish_reasons` (from `ChatResponse.stop_reason`)
- `gen_ai.response.id` (when the provider returns one)
- **Sema extension** (namespaced to avoid colliding with the spec):
  `sema.gen_ai.usage.cost_usd` — our computed cost (the spec has no cost attribute)

### Tool-span attributes

- `gen_ai.tool.name`, `gen_ai.tool.call.id` (already parsed into `ToolCall.id`),
  `gen_ai.tool.type` = `function`
- `error.type` on failure

### Metrics (two histograms, per the spec)

- `gen_ai.client.token.usage` — bucketed by `gen_ai.token.type` (`input`/`output`)
- `gen_ai.client.operation.duration` (seconds)
- both tagged with `gen_ai.provider.name`, `gen_ai.request.model`,
  `gen_ai.operation.name`

### Privacy

Content (prompts, tool args, completions) is **opt-in** per the spec. Default =
metadata only (model, tokens, duration, finish reason). When explicitly enabled we
attach `gen_ai.input.messages`, `gen_ai.output.messages`, `gen_ai.system_instructions`.

---

## 3. Proposed architecture for a Rust sync VM

### 3.1 Crate choice

Use the standard `opentelemetry-rust` stack — it is the only path that yields
spec-compliant OTLP consumable by Jaeger/Langfuse/Datadog unmodified:

```toml
# new crate: crates/sema-otel/Cargo.toml
opentelemetry        = "0.31"
opentelemetry_sdk    = { version = "0.31", features = ["rt-tokio"] }
opentelemetry-otlp   = { version = "0.31", features = ["grpc-tonic", "trace", "metrics"] }
opentelemetry-semantic-conventions = "0.31"   # for stable attr-name constants
```

**Why a new `sema-otel` crate, not adding deps to `sema-llm`:**
- `sema-llm` and `sema-stdlib` deliberately depend on `sema-core` but *not* on each
  other or on `sema-eval` (CLAUDE.md "Critical"). A telemetry crate that both
  `sema-llm` (LLM/tool spans) and the binary (VM spans, init/shutdown) use must sit
  *below* them. `sema-otel` depends only on `sema-core` + the otel crates.
- Keeps the otel dependency tree (tonic, prost, hyper) out of `sema-wasm`
  (`opentelemetry-otlp` is gated `#[cfg(not(target_arch = "wasm32"))]`, mirroring how
  `sema-llm` is already excluded from wasm in `eval.rs:65`).

**Why not just `tracing` + `tracing-opentelemetry`:** the GenAI semconv wants
explicit CLIENT spans with precise attribute names and two specific metrics; a thin
manual span API over `opentelemetry` is clearer than fighting `tracing` macro
attribute coercion, and the VM is sync (no async span context to thread). We can
add a `tracing` bridge later if desired.

### 3.2 The tokio-runtime fit

`sema-llm` already owns per-provider `BlockingRuntime`s (`http.rs:17`) and calls
`runtime.block_on(...)`. The OTLP batch exporter wants a tokio runtime. We
initialize one dedicated tokio runtime for the OTLP exporter inside `sema-otel` at
init time (reusing the `BlockingRuntime` drop-safe pattern from `http.rs` so a
short-lived interpreter dropped inside the MCP stdio loop never panics). Spans are
created/ended synchronously on the calling (VM) thread; the batch span processor
hands completed spans to the exporter runtime in the background.

### 3.3 Single chokepoint + a thin facade

`sema-otel` exposes a small, sync, **no-op-when-disabled** facade so call sites stay
clean and pay nothing when telemetry is off:

```rust
// sema-otel public surface (sketch)
pub fn init_from_env() -> Option<OtelGuard>;   // returns None if SEMA_OTEL unset
pub struct OtelGuard;                            // flush + shutdown on Drop

// RAII spans — Drop ends the span and records duration
pub struct LlmSpan { /* holds an opentelemetry BoxedSpan or None */ }
pub fn llm_span(op: &str, model: &str, provider: &str) -> LlmSpan;
impl LlmSpan {
    pub fn set_request(&self, temp: Option<f64>, max_tokens: Option<u32>);
    pub fn set_response(&self, resp: &ResponseFacts);  // tokens, finish, id, cost
    pub fn record_error(&self, kind: &str, msg: &str);
    // content capture, gated internally by SEMA_OTEL_CAPTURE_CONTENT
    pub fn set_messages(&self, input: &str, output: &str, system: Option<&str>);
}
pub fn tool_span(name: &str, call_id: &str) -> ToolSpan;
pub fn agent_span(name: &str) -> AgentSpan;
```

`ResponseFacts` is a plain struct (`input_tokens`, `output_tokens`, `model`,
`finish_reason`, `response_id`, `cost_usd`) so `sema-otel` need not depend on
`sema-llm` types — `sema-llm` maps its `ChatResponse`/`Usage` into it. When
`SEMA_OTEL` is unset, every constructor returns a span holding `None` and all
methods are early-return — branch-predictable, near-zero cost.

### 3.4 Context propagation (parent/child nesting)

Sema's VM is single-threaded; we keep the active span in a **thread-local stack**
inside `sema-otel` (one `RefCell<Vec<Context>>`). `agent_span` pushes; its child
`llm_span`/`tool_span` attach to the top of the stack as parent; Drop pops. This
reproduces LLM-call → tool-call → LLM-call nesting without any async context
machinery. (When/if async LLM calls land, switch to `opentelemetry::Context`
attach/detach guards — the facade signature does not change.)

### 3.5 VM-general spans (beyond LLM)

The same facade gives the VM cheap structured tracing without the GenAI vocabulary:
a generic `vm_span(name)` (INTERNAL kind) usable to wrap notebook-cell evaluation,
`load`/`import` of a module, or a `sema run <file>` top-level. The notebook threads
one root span per run and a child per cell (see §6). This is the "usable for the VM
generally" requirement — LLM is the headline consumer, not the only one.

---

## 4. Exposing it to Sema users

### 4.1 Off by default, env-driven (matches OTel conventions)

| Env var | effect |
|---------|--------|
| `SEMA_OTEL=1` | turn telemetry on (else everything is no-op) |
| `SEMA_OTEL_EXPORTER=otlp` \| `console` \| `none` | exporter choice (default `otlp`) |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | standard OTLP endpoint (e.g. `http://localhost:4317`) |
| `OTEL_EXPORTER_OTLP_HEADERS` / `_TIMEOUT` | standard OTLP knobs (Langfuse auth, etc.) |
| `OTEL_SERVICE_NAME` | resource service name (default `sema`) |
| `SEMA_OTEL_CAPTURE_CONTENT=1` | opt-in prompt/completion/tool-arg capture (privacy default = off) |
| `OTEL_SEMCONV_STABILITY_OPT_IN=gen_ai_latest_experimental` | acknowledge experimental semconv churn |

Honoring the standard `OTEL_*` vars means users point Sema at local Jaeger (one
`docker run`), hosted Langfuse, or Datadog with **no code change**. The `console`
exporter writes spans to stderr for zero-setup local debugging.

### 4.2 Initialization & shutdown

- **CLI / REPL / notebook server**: call `sema_otel::init_from_env()` once at the
  top of `main()` (`crates/sema/src/main.rs:461`), holding the returned
  `OtelGuard` for the process lifetime. `Drop` flushes the batch processor —
  essential for short-lived `sema run file.sema` invocations that would otherwise
  lose the last spans.
- **Embedders** (`crates/sema/src/lib.rs` `Interpreter`): document that hosts call
  `init_from_env()` themselves; we do *not* init inside `Interpreter::new()` (an
  embedder may run many interpreters and own its own tracer provider). Spans are
  no-ops if no provider was installed, so this is safe.

### 4.3 Optional Sema-level surface (Milestone 3, not MVP)

Two small builtins for users who want manual control inside scripts:

- `(otel/span name f)` — run thunk `f` inside a named INTERNAL span; returns `f`'s
  value. Useful to group a multi-step workflow.
- `(otel/event name attrs-map)` — add a span event/annotation to the current span.

These are thin wrappers over the `sema-otel` facade, registered in
`sema-stdlib` behind the same enabled-flag (no-op when off).

---

## 5. Phased task list

### Milestone 0 — `sema-otel` crate skeleton (½ day)
- **Files:** new `crates/sema-otel/{Cargo.toml,src/lib.rs}`; workspace
  `Cargo.toml` (add `sema-otel` to `[workspace.dependencies]` at the `=1.20.4`
  pin, mirroring lines 29–40).
- **Content:** `init_from_env()` reading the env table in §4.1; OTLP tracer +
  meter provider behind a drop-safe tokio runtime; `console` and `none` exporters;
  `OtelGuard` flushing on Drop. The RAII span facade (`LlmSpan`/`ToolSpan`/
  `AgentSpan`/`vm_span`) with the thread-local parent stack. All constructors
  return no-op spans when `SEMA_OTEL` is unset.
- **Acceptance:** `SEMA_OTEL=1 SEMA_OTEL_EXPORTER=console` + a unit test that opens
  and drops an `LlmSpan` emits one span to stderr; with the flag unset, the same
  code path allocates no exporter and a microbench shows span create/drop is a
  predictable branch (no exporter, no alloc). `make lint` clean.

### Milestone 1 (MVP) — LLM-call spans at the chokepoint (1 day)
- **Files:** `crates/sema-llm/Cargo.toml` (add `sema-otel`, wasm-excluded like the
  existing tokio/reqwest usage); `crates/sema-llm/src/builtins.rs` —
  `do_complete_uncached` (`:4235`) and `do_complete_with_provider` (`:4192`):
  wrap the `provider.complete(...)` call in an `llm_span`. Map `ChatResponse`/
  `Usage`/`stop_reason` into `ResponseFacts` (incl. `cost_usd` from
  `pricing::calculate_cost_for`, computed in `track_usage` today —
  thread the cost into the span or recompute). Set `gen_ai.provider.name` from the
  serving-provider stamp (`set_serving_provider`, `:4214/4246`).
- **Operation name:** `chat` for `complete`; later `embeddings` for `embed`.
- **Acceptance:** with a local Jaeger (`docker run jaegertracing/all-in-one`),
  `SEMA_OTEL=1 sema -e '(llm/auto-configure)(llm/complete "ping" {:max-tokens 10})'`
  produces one CLIENT span named `chat <model>` carrying provider, request/response
  model, input/output tokens, finish reason, and `sema.gen_ai.usage.cost_usd`
  matching `llm/last-usage`. **Deterministic CI test:** add a `FakeProvider`
  (test-only, implements `LlmProvider`; see also the cassettes plan
  `2026-06-21-llm-cassettes.md`) + an in-memory span exporter
  (`opentelemetry_sdk` `InMemorySpanExporter`) and assert the emitted span's
  attributes — this is the regression guard and needs no network/keys.

### Milestone 2 — tool + agent spans (1 day)
- **Files:** `builtins.rs` `run_tool_loop` (`:4425`): open an `agent_span` for the
  whole loop (named from the agent, or `chat` when called via `llm/chat`); wrap
  each `execute_tool_call` (`:4479`) in a `tool_span` carrying `gen_ai.tool.name`
  and `gen_ai.tool.call.id` (= `tc.id`). Record `error.type` when a tool errors
  (pairs naturally with the deferred "recoverable tool errors" work — once a tool
  error is caught instead of `?`-propagated, the span records it). Re-use the
  already-measured `duration_ms`.
- **Acceptance:** an `agent/run` with one tool (FakeProvider scripted to emit a
  tool_call then a final answer) produces a trace tree: `invoke_agent` →
  (`chat`, `execute_tool <name>`, `chat`) with correct parent/child nesting and
  the tool's `call.id`. Verified via the in-memory exporter in CI.

### Milestone 3 — metrics + content capture + Sema surface (1 day)
- **Files:** `sema-otel/src/lib.rs` (meter provider + the two histograms, recorded
  from `LlmSpan::set_response`); content capture gated by
  `SEMA_OTEL_CAPTURE_CONTENT` in `LlmSpan::set_messages`, called from `do_complete*`
  / `run_tool_loop`; `crates/sema-stdlib/src/*.rs` (new module or fold into an
  existing one) registering `(otel/span ...)` and `(otel/event ...)`.
- **Acceptance:** `gen_ai.client.token.usage` and `…operation.duration` show up in
  the metrics pipeline (console exporter or Prometheus scrape via the OTLP
  collector); with `SEMA_OTEL_CAPTURE_CONTENT=1` the span carries
  `gen_ai.input.messages`/`output.messages` and **without it those attributes are
  absent** (privacy test). `(otel/span "x" (fn () 42))` returns `42` and emits one
  INTERNAL span. Dual-eval test for the no-op (flag-off) behavior returning `42`.

### Milestone 4 (vision) — notebook per-run trace + streaming + embeddings (1–2 days)
- **Files:** `crates/sema-notebook/src/*` — one root `vm_span` per notebook
  evaluation, one child per cell (the notebook has a shared cell env, so this is a
  natural fit); `builtins.rs` `llm/stream` path (`~:1521`) — currently bypasses
  `do_complete`, so add a span there too so streamed calls are not invisible;
  `embed` paths — `embeddings` operation spans.
- **Acceptance:** running the demo notebook with `SEMA_OTEL=1` yields a single
  trace whose root has one child span per executed cell, with LLM/tool spans
  nested beneath the cell that issued them; a streamed completion produces a span;
  an `llm/embed` produces an `embeddings` span with token usage.

### Milestone 5 — docs (½ day)
- **Files:** new `website/docs/llm/observability.md` (`` ```sema `` blocks);
  link from `website/docs/llm/index.md`; a short section in `README.md`; note the
  env vars and a `docker run jaegertracing/all-in-one` + Langfuse OTLP quickstart.
- **Acceptance:** docs build; the documented `SEMA_OTEL=1` + Jaeger walkthrough
  reproduces a trace end-to-end on a clean machine.

---

## 6. Minimal first milestone vs full vision

**MVP (Milestones 0–1):** an opt-in `SEMA_OTEL=1` flag that emits one
spec-compliant `chat {model}` CLIENT span per LLM call with tokens, finish reason,
cost, and latency, exportable to any OTLP backend, with a deterministic in-memory
CI test (no keys). This alone makes every Sema LLM program observable in Jaeger/
Langfuse/Datadog and is the single highest-leverage observability win.

**Full vision (Milestones 2–4):** the complete LLM-call → tool-call → agent-loop
trace tree with the two GenAI metrics, privacy-gated content capture, a Sema-level
`(otel/span ...)` surface, streaming + embeddings coverage, and the notebook
"one trace per run" experience — observable-by-default agents as a differentiator.

---

## 7. Codebase-fit notes (idiomatic to this repo)

- **One chokepoint, no scatter.** `do_complete` already centralizes cache →
  fallback → rate-limit → retry. Spans go around the actual `provider.complete`
  call inside `do_complete_uncached` / `do_complete_with_provider`, so a cache hit
  (which serves no provider) correctly emits no LLM span — consistent with the
  existing `LAST_SERVING_PROVIDER` reset at `:4105`.
- **Reuse, don't recompute.** `duration_ms` (`:4480`) and cost
  (`pricing::calculate_cost_for` in `track_usage`, `:209`) already exist; the span
  consumes them rather than adding new measurement.
- **Dependency direction preserved.** `sema-otel` sits below `sema-llm`/`sema-stdlib`
  (depends only on `sema-core` + otel), so the "stdlib/llm must not depend on each
  other or on eval" rule (CLAUDE.md) holds.
- **wasm stays clean.** `opentelemetry-otlp` is `#[cfg(not(target_arch =
  "wasm32"))]`-gated exactly like `sema-llm` is excluded in `eval.rs:65`; the
  playground build is unaffected.
- **Drop-safety.** The exporter's tokio runtime uses the same `BlockingRuntime`
  background-shutdown pattern as `http.rs` so dropping an interpreter (e.g. in the
  MCP stdio loop) never panics, and `OtelGuard::drop` flushes for short CLI runs.
- **`Rc`/single-thread reality.** Parent/child nesting uses a thread-local span
  stack, not async `Context` propagation — matches the sync VM. Facade signatures
  are stable if async LLM calls arrive later.
- **Mock-first testing.** The deterministic CI tests ride the `FakeProvider` /
  cassette infrastructure (cross-ref `docs/plans/2026-06-21-llm-cassettes.md`) plus
  `opentelemetry_sdk::InMemorySpanExporter`, so the whole telemetry path is
  verified with no network and no API keys — the gap that has historically let
  LLM-path regressions ship.

## 8. Open questions

1. **`opentelemetry` version pin** — confirm latest stable (0.31 assumed) at
   implementation time; the crate churns and metrics API has moved between releases.
2. **Cost attribute namespace** — `sema.gen_ai.usage.cost_usd` (chosen) vs piggy-
   backing a future spec attribute if one lands.
3. **Embeddings token split** — `gen_ai.usage.input_tokens` only (no output); confirm
   the metric `gen_ai.token.type` bucketing for embeddings.
4. **Should `(otel/span ...)` be a special form or a builtin?** A builtin taking a
   thunk (`fn`) is simpler and sufficient; revisit only if we want lexical-block
   sugar.
5. **Notebook trace identity** — one trace per "Run All" vs one per cell-eval; §6
   assumes per-run with cell children, but per-cell traces may suit incremental
   editing better.
