# Bulletproofing Sema's LLM / Agentic Features

**Date:** 2026-06-21
**Status:** Not started. Execution-ready, task-by-task.
**Companions:**
- `docs/plans/2026-06-21-llm-cassettes.md` — the record/replay tape primitive. Phase 1 of THIS plan uses a simpler in-process `FakeProvider` for unit tests; cassettes are the broader CI-corpus layer. They are complementary: `FakeProvider` for hand-scripted scenarios, cassettes for recorded real-provider corpora. Phase 1 lands the fake provider; Phase 6 wires cassettes into CI.
- `docs/plans/2026-06-21-mcp-client-spike.md` — MCP tool calls share the same seam; observability spans (Phase 5) should cover them.

## Context (from the audit)

A full audit (live-verified against OpenAI, Anthropic, Gemini) found the LLM/agentic
stack is **real and substantially works** — completion, chat, streaming, structured
extraction with validate+reask, embeddings, vector search, concurrent batch/pmap,
exact cost tracking, budget enforcement, and a genuine multi-round agent tool loop
that re-enters Sema and executes Sema-defined tools. The headline thesis ("the
scaffolding is the runtime") substantially holds.

But there is **one critical correctness bug** that undercuts the headline agent
promise, **zero deterministic CI coverage** of any real LLM/agent path, **thin
resilience**, and **two factual doc errors**. This plan fixes those, in priority order.

### The single chokepoint

Every LLM call funnels through `with_provider` / `&dyn LlmProvider` in
`crates/sema-llm/src/builtins.rs`. The `LlmProvider` trait
(`crates/sema-llm/src/provider.rs`) is sync. That seam is what makes a fake provider,
retry, and tracing all localizable.

### Key verified locations

- `run_tool_loop` — `crates/sema-llm/src/builtins.rs:4425`
- Tool result text-stuffing — `builtins.rs:4501-4504` (`"[Tool result for {}]: {}"`)
- `execute_tool_call` — `builtins.rs:4516` (propagates errors via `?` → aborts whole loop)
- `ChatMessage { role, content }` — `crates/sema-llm/src/types.rs:69-73` (no `tool_call_id`, no assistant `tool_calls`)
- `ToolCall { id, name, arguments }` — `types.rs:91-96` (`id` is parsed but discarded on the return path)
- Retry loop (429-only) — `builtins.rs:4241-4259`
- `do_complete` dispatch (cache→fallback→rate-limit) — `builtins.rs:4101-4261`
- `llm/stream` bypassing `do_complete` — `builtins.rs:1521-1560`
- Per-provider request serializers — `anthropic.rs`, `openai.rs`, `gemini.rs`, `ollama.rs`
- Homepage factual error — `website/sema-homepage-v2.html:545` ("a bytecode VM and a tree-walker")
- "dynamic pricing from llm-prices.com" overstatement — `README.md:340`
- `llm/classify` signature `(categories text opts)` — `builtins.rs:1757-1763`
- Gemini default thinking model + empty-output footgun — `gemini.rs:34`

---

## Phase 0 — Accuracy fixes (do first; trivial, credibility-damaging)

**Goal:** Remove outright false public claims. No code-behavior change. Each is a
few minutes and unblocks honest marketing while the rest of the plan lands.

### Task 0.1 — Fix homepage "tree-walker" factual error
- **Files:** `website/sema-homepage-v2.html` (line ~545); also grep `website/public/v1.html`, `website/public/v2.html`, `website/sema-homepage-v1.html` for the same string.
- **Context:** The tree-walker was retired; the VM is the sole evaluator (CLAUDE.md/MEMORY). The error sits in the section framed as "honest", which makes it especially damaging. `README.md:352` is already correct.
- **Steps:**
  1. `rg -n "tree-walker" website/` to find every occurrence in served HTML.
  2. Change "A bytecode VM and a tree-walker." → "A bytecode compiler and a stack-based VM."
  3. Leave `README.md:352` as-is (already correct).
- **Acceptance:** `rg "tree-walker" website/` returns no served-page hits claiming a tree-walker exists at runtime.

### Task 0.2 — Fix "dynamic pricing from llm-prices.com" overstatement
- **Files:** `README.md` (line ~340).
- **Context:** Pricing is an embedded models.dev snapshot via `include_str!(pricing-data.json)` (`pricing.rs:23-185`), deliberately not fetched at runtime. The word "dynamic" only appears in README, not `docs/llm/cost.md`. Cost math itself is exact.
- **Steps:**
  1. Reword to: "built-in budget tracking with a bundled pricing snapshot (models.dev), updated per release".
  2. `rg -n "llm-prices|dynamic pricing" README.md docs/ website/` to catch any sibling copies.
- **Acceptance:** No public copy claims live/dynamic price fetching.

### Task 0.3 — Fix `llm/classify` documented signature
- **Files:** `website/sema-homepage-v2.html` (~line 430), `website/docs/llm/*.md`, any README example.
- **Context:** Real signature is `(llm/classify categories text opts)` (`builtins.rs:1757-1763`). The homepage shows the non-existent `{:labels [...]}` map form. Pick one: fix the docs OR add the overload (Task 4.x optional). For Phase 0, fix the docs.
- **Steps:**
  1. `rg -n "llm/classify" website/ README.md` and correct each example to the real arg order.
- **Acceptance:** Every `llm/classify` example matches `(llm/classify categories text [opts])`.

---

## Phase 1 — Deterministic test harness (FakeProvider) + first CI tests

**Goal:** A test-only `FakeProvider` implementing `LlmProvider` that returns scripted
responses (including `tool_calls`), registered via `ProviderRegistry` in tests. This
is what lets every subsequent fix be locked in with a deterministic, key-free CI test.
**This is the prerequisite for Phases 2–4 — land it first.**

> Relationship to cassettes: `FakeProvider` is hand-scripted (assert exact loop
> behavior, error injection, ordering). Cassettes (Phase 6) record real provider
> output for example/corpus coverage. Build the fake first; it's smaller and gives
> precise control over tool-call shapes and error paths.

### Task 1.1 — Implement `FakeProvider`
- **Files:** new `crates/sema-llm/src/fake.rs` (gated `#[cfg(any(test, feature = "test-provider"))]` or a small always-compiled module behind `pub(crate)` for use from `crates/sema/tests`).
- **Context:** Trait at `provider.rs:4-32`. The fake must cover `complete`, `stream_complete`, `batch_complete`, `embed`. It needs to script multi-round tool loops: response 1 emits `tool_calls`, response 2 (after seeing tool results) emits final text.
- **Steps:**
  1. Define `FakeProvider { name, default_model, script: RefCell<VecDeque<FakeResponse>>, recorded_requests: RefCell<Vec<ChatRequest>> }`.
  2. `FakeResponse` is either a canned `ChatResponse` (with optional `tool_calls`, `usage`, `stop_reason`) or an error (`LlmError::RateLimited`, `LlmError::Api(5xx)`, network) for resilience tests.
  3. `complete` pops the next scripted response, records the request (for assertions on the messages the loop built), returns it.
  4. `stream_complete` emits a recorded `Vec<String>` of chunks to `on_chunk`, then returns the final `ChatResponse`.
  5. `batch_complete` pops one scripted response per request, preserving order.
  6. `embed` returns scripted vectors.
  7. Provide a builder: `FakeProvider::builder().reply("hi").tool_call("get_weather", json!({"city":"Oslo"})).reply("It is sunny").build()`.
- **Acceptance:** Unit test in `fake.rs`: a scripted two-step (tool_call then final) `complete` sequence returns the expected responses in order and records both requests.

### Task 1.2 — Test plumbing to register `FakeProvider`
- **Files:** new `crates/sema/tests/llm_fake_test.rs`; helper that builds an `Interpreter`, registers the fake as default provider, and runs Sema source.
- **Context:** `reset_runtime_state` (`builtins.rs:115`) clears thread-local LLM state per interpreter — use a fresh interpreter per test to avoid leakage. `ProviderRegistry` (`provider.rs:35-91`).
- **Steps:**
  1. Add a test-only entry point (e.g. `sema_llm::register_test_provider(name, Box<dyn LlmProvider>)`) that inserts into `PROVIDER_REGISTRY` and sets it default. Keep it `pub` but doc-hidden / behind `cfg(test)` exposure as appropriate.
  2. Helper `eval_with_fake(src, fake) -> Result<Value, SemaError>`.
- **Acceptance:** `(llm/complete "say hi")` against a fake returns the scripted string, no network, in `make test`.

### Task 1.3 — First deterministic agent-loop test (oracle for Phase 2)
- **Files:** `crates/sema/tests/llm_fake_test.rs`.
- **Context:** This test is the regression oracle for the Phase 2 tool-result fix. It must assert on the *messages the loop sends back to the provider on round 2* (via `FakeProvider.recorded_requests`), not just the final string — that's how we catch the OpenAI-shaped breakage.
- **Steps:**
  1. Script: round 1 → `tool_call` for a Sema `deftool`; round 2 → final answer.
  2. Run a `defagent` + `agent/run`. Assert final answer == scripted.
  3. Assert the round-2 request contains a correlated tool-result message (this assertion will FAIL until Phase 2 lands — mark `#[ignore]` with a `// unignore after Phase 2` note, or write it green against current text-stuffing and tighten it in Phase 2).
- **Acceptance:** Agent loop runs deterministically offline; the message-shape assertion is in place to drive Phase 2.

---

## Phase 2 — Fix the critical tool-result protocol bug (CRITICAL)

**Goal:** Echo the assistant's `tool_calls` back into history and return tool results
as provider-native correlated messages (Anthropic `tool_use`/`tool_result` blocks;
OpenAI `role:"tool"` with `tool_call_id`). This fixes the agent loop on OpenAI-family
providers, where it currently re-calls the tool until max-turns and returns empty.

### Task 2.1 — Extend the message model with tool correlation
- **Files:** `crates/sema-llm/src/types.rs` (`ChatMessage` ~69-73, `ToolCall` ~91-96).
- **Context:** `ChatMessage` has only `role`+`content`. `ToolCall.id` is parsed from responses but never threaded back into a request. Need: (a) an assistant message can carry `tool_calls`; (b) a tool-result message carries `tool_call_id` + `name`.
- **Steps:**
  1. Add `tool_calls: Vec<ToolCall>` to `ChatMessage` (default empty) for the assistant echo.
  2. Add a tool-result representation: either a new `role: "tool"` with `tool_call_id: Option<String>` field, or a `MessageContent::ToolResult { tool_call_id, name, content }` variant. Prefer an explicit `tool_call_id: Option<String>` field on `ChatMessage` for minimal churn.
  3. Update `ChatMessage::new` callers / add `ChatMessage::tool_result(id, name, content)` and `ChatMessage::assistant_with_tool_calls(content, calls)` constructors.
- **Acceptance:** Crate compiles; existing serializer call sites updated; no behavior change yet.

### Task 2.2 — Rewrite `run_tool_loop` to use correlated messages
- **Files:** `crates/sema-llm/src/builtins.rs:4425` (`run_tool_loop`), `builtins.rs:4501-4504`.
- **Context:** Currently pushes `ChatMessage::new("user", "[Tool result for X]: ...")` and never echoes the assistant turn.
- **Steps:**
  1. After a response with `tool_calls`, push the assistant message carrying those `tool_calls` (and any text) into history.
  2. For each tool call, push a `tool_result` message with the `tool_call_id` (from `tc.id`), tool name, and the stringified result — instead of the `[Tool result for X]:` user text.
  3. Keep the `on_tool_call` start/end events unchanged.
- **Acceptance:** The Phase 1.3 round-2 message-shape assertion passes; final answer correct against the fake.

### Task 2.3 — Thread correlation through each provider's request serializer
- **Files:** `crates/sema-llm/src/anthropic.rs`, `openai.rs`, `gemini.rs`, `ollama.rs`.
- **Context:** `openai.rs` hardcodes `tool_calls: None` on every history message; Anthropic needs `tool_use`/`tool_result` content blocks; Gemini needs `functionCall`/`functionResponse`; Ollama is OpenAI-compatible.
- **Steps:**
  1. **OpenAI** (`openai.rs` build_request_body): serialize assistant messages with their `tool_calls`; serialize tool-result messages as `{role:"tool", tool_call_id, content}`.
  2. **Anthropic** (`anthropic.rs`): assistant turn → `tool_use` blocks; tool result → a user message with `tool_result` blocks keyed by `tool_use_id`.
  3. **Gemini** (`gemini.rs`): assistant `functionCall` parts; tool result → `functionResponse` parts.
  4. **Ollama** (`ollama.rs`): mirror OpenAI shape.
- **Acceptance:**
  - Deterministic: a `FakeProvider` configured to *validate* it received correlated tool-result messages passes for the OpenAI shape (assert via `recorded_requests`).
  - Live (manual, key-gated, `#[ignore]`): the same agent that previously looped 5× on `gpt-4o-mini` now completes in 2 rounds with a non-empty answer. Document the manual verification step.

---

## Phase 3 — Recoverable tool errors + per-tool input validation

**Goal:** A single failing or hallucinated-argument tool call must NOT abort the whole
`agent/run`. Feed the error back into the loop so the model self-corrects (bounded).
Validate model-supplied args against the tool's parameter schema before invoking.

### Task 3.1 — Catch tool errors and feed them back
- **Files:** `crates/sema-llm/src/builtins.rs:4479` (the `execute_tool_call(...)?` call), `run_tool_loop`.
- **Context:** `execute_tool_call` propagates via `?`, killing the run. Mirror agent-browser's "covered by X" self-correction.
- **Steps:**
  1. Wrap `execute_tool_call` in a match. On `Err(e)`, push a tool-result message whose content is a structured error string (tool name + error + hint), instead of returning.
  2. Add an optional bound (e.g. an internal cap on consecutive tool errors per round / per loop) to avoid infinite error loops; still respect `max-tool-rounds`.
  3. Fire the `on_tool_call` end event with the error as the result.
- **Acceptance:** Deterministic test: a `deftool` whose handler raises on round 1; the fake then returns a corrected call on round 2; `agent/run` completes successfully (loop did not abort).

### Task 3.2 — Validate tool args against the parameter schema before invoke
- **Files:** `builtins.rs` `execute_tool_call` (~4516) and the existing extraction validator (`validate_extraction`, `format_reask_prompt` — already in `builtins.rs` for `llm/extract`).
- **Context:** `deftool` PARAMETERS already drives JSON-arg ordering; reuse `validate_extraction` machinery at the tool boundary. eve's `ToolInputValidationError`/`meta.issues` pattern.
- **Steps:**
  1. Before invoking the handler, validate `tc.arguments` against the tool's parameter schema (types, required-unless-`:optional`).
  2. On mismatch, return a structured validation-issues message back into the loop (Task 3.1 path), so the model retries with corrected args.
- **Acceptance:** Deterministic test: fake emits a tool call with a wrong-typed arg → loop feeds back the validation issue → fake emits corrected args → success.

---

## Phase 4 — Resilience: retry/backoff, timeouts, streaming parity

**Goal:** Make resilience match the docs ("retries with backoff", "exponential
backoff"). Add same-provider retry on transient 5xx/network/timeout with exponential
backoff + jitter; add `:max-retries` and a real `:timeout`; route streaming through
the budget/cache/fallback/rate-limit layer (or document the bypass).

### Task 4.1 — Exponential backoff + jitter, broaden retry triggers
- **Files:** `crates/sema-llm/src/builtins.rs:4241-4259` (retry loop), `crates/sema-llm/src/http.rs`.
- **Context:** Today retry fires ONLY on `LlmError::RateLimited` (429), max 3, fixed wait; 5xx/network/timeout return immediately. No backoff/jitter. Anthropic streaming hardcodes `retry_after=5000ms` ignoring the header.
- **Steps:**
  1. Extend the retry match to cover transient `LlmError::Api(5xx)`, network, and timeout errors (keep 4xx-non-429 non-retryable).
  2. Implement capped exponential backoff with jitter (e.g. base 500ms, factor 2, cap 30s, full jitter). For 429, honor server `retry_after` when present, else backoff.
  3. Read Anthropic's actual `retry-after` header instead of the hardcoded 5000ms (`anthropic.rs:89-93,168-172`).
- **Acceptance:** Deterministic test: `FakeProvider` returns a 5xx then a success → `complete` succeeds after one retry. A 429-then-success test asserts backoff path is taken. No test relies on wall-clock sleeps longer than a few ms (inject a no-op/shrinkable sleep or assert on attempt count).

### Task 4.2 — `:max-retries` and real `:timeout` options
- **Files:** `builtins.rs` (option parsing for `llm/complete`/`llm/chat`/`agent/run`), `http.rs:6,52-60` (`create_client`).
- **Context:** `create_client(None)` is always called → the per-call `:timeout` argument is dead and 120s is hardcoded.
- **Steps:**
  1. Add `:max-retries` (default 2) threaded into the retry loop.
  2. Thread a `:timeout` (ms) into `create_client` per call (or cache clients by timeout) so the argument is actually honored.
- **Acceptance:** A `:max-retries 0` test makes a transient error surface immediately (no retry). A `:timeout` test confirms the value reaches `create_client` (unit-test the plumbing; live timeout test `#[ignore]`).

### Task 4.3 — Streaming routes through the dispatch layer
- **Files:** `builtins.rs:1521-1560` (`llm/stream`), `do_complete` (~4101-4261).
- **Context:** `llm/stream` calls `stream_complete` directly, bypassing cache/budget/fallback/rate-limit. Usage IS tracked post-stream.
- **Steps:**
  1. Before opening the stream: enforce budget pre-check, enforce rate-limit, and check cache (a cache hit can short-circuit before any network).
  2. Optionally route through a streaming-aware `do_complete` wrapper that applies fallback on stream-open error.
  3. If full parity is deferred, document the remaining bypass explicitly in `docs/llm/`.
- **Acceptance:** Deterministic test: `llm/stream` inside `llm/with-budget` with a tiny cap aborts before/at the over-budget call; a cached stream call short-circuits without invoking the fake's stream path.

### Task 4.4 — Accumulate streamed tool-call deltas (Anthropic/OpenAI)
- **Files:** `anthropic.rs:147-241`, `openai.rs:193-284`.
- **Context:** Both discard `tool_use`/`tool_call` deltas during streaming (return empty `tool_calls`); only Gemini/Ollama accumulate. Streaming agents are broken on Anthropic/OpenAI.
- **Steps:**
  1. Anthropic: collect `content_block_start`/`content_block_delta` for `tool_use` blocks; assemble `tool_calls` in the final response.
  2. OpenAI: accumulate `tool_call` deltas (index-keyed `function.name`/`arguments` fragments).
- **Acceptance:** Deterministic test (fake stream emitting tool-call chunks) yields a non-empty `tool_calls` on the returned `ChatResponse`.

---

## Phase 5 — Observability (opt-in OpenTelemetry GenAI tracing)

**Goal:** A CLIENT span per LLM call and an `execute_tool` span per tool dispatch,
emitting GenAI-semconv attributes (provider, model, token usage, finish reason,
latency) + the two GenAI metrics. Gated by an env flag; content capture opt-in.
Highest-leverage observability win; standards-compliant; differentiator for notebooks.

### Task 5.1 — Wire OTel SDK at startup behind `SEMA_OTEL=1`
- **Files:** new `crates/sema-llm/src/otel.rs` (or a small module in `sema-eval` startup); `Cargo.toml` deps (`opentelemetry`, `opentelemetry_sdk`, `opentelemetry-otlp`).
- **Context:** Single-threaded runtime but providers already `block_on` tokio. Respect `OTEL_EXPORTER_OTLP_*` and `OTEL_SEMCONV_STABILITY_OPT_IN`. Flush on shutdown for short CLI runs.
- **Steps:**
  1. Initialize tracer + meter providers once, only when `SEMA_OTEL=1`. No-op when unset (zero overhead).
  2. Register a shutdown/flush hook so short-lived runs don't drop spans.
- **Acceptance:** With `SEMA_OTEL=1` and a local OTLP collector (manual), an `llm/complete` call produces one CLIENT span; with the flag unset, no spans and no measurable overhead (assert the init path is skipped).

### Task 5.2 — Emit LLM-call and tool-call spans at the chokepoint
- **Files:** `builtins.rs` (`with_provider` / `do_complete`, `execute_tool_call`/native-fn dispatch).
- **Context:** `duration_ms` already measured in the loop (`builtins.rs:4480`) but not exported. `track_usage` centralizes usage.
- **Steps:**
  1. CLIENT span `"{operation} {model}"` with `gen_ai.provider.name`, `gen_ai.request.model`, `gen_ai.response.model`, `gen_ai.usage.input_tokens`/`output_tokens`, `gen_ai.response.finish_reasons`, `gen_ai.request.temperature`/`max_tokens`.
  2. `execute_tool` span with `gen_ai.tool.name`, `gen_ai.tool.call.id`, nested under the agent/LLM span via context propagation.
  3. Emit `gen_ai.client.token.usage` and `gen_ai.client.operation.duration` histograms.
  4. Gate prompt/tool-arg content behind `SEMA_OTEL_CAPTURE_CONTENT=1` (default metadata-only).
- **Acceptance:** Manual collector shows nested LLM→tool spans with token/latency attributes; content absent unless the capture flag is set.

---

## Phase 6 — Close remaining feature gaps

**Goal:** Structured-output primitive, batch budget pre-flight, Gemini empty-output
footgun, and CI corpus via cassettes. These are the highest-value remaining gaps.

### Task 6.1 — Structured-output primitive with schema + repair loop
- **Files:** `builtins.rs` (new `llm/generate-object` or a `:result`/`:schema` option on `prompt`/`agent/run`), reuse `json_to_value` + `validate_extraction` + `format_reask_prompt`.
- **Context:** `ChatRequest.json_mode` is a bare bool (`types.rs:108`); schema validation + reask exists only inside `llm/extract`. `agent/run` and tool handlers return stringified results, so workflows can't branch on typed results. (eve `generateObject`; Mastra schema-at-every-boundary.)
- **Steps:**
  1. Add `llm/generate-object` taking a Sema schema → JSON Schema; set provider json mode; validate; on failure feed the concrete validation error back for a bounded repair retry; raise a distinct error after exhaustion.
  2. Support `:output :object | :array | :enum | :no-schema`.
  3. (Optional) accept the schema on `prompt`/`defagent` returns.
- **Acceptance:** Deterministic test: fake returns malformed JSON on attempt 1, valid on attempt 2 → `llm/generate-object` returns the typed map; an always-invalid fake raises the distinct error after the bound.

### Task 6.2 — Batch budget pre-flight + honest budget docs
- **Files:** `builtins.rs` (`llm/batch`/`llm/pmap` ~2164-2238, `track_usage` ~205-260), `docs/llm/cost.md`.
- **Context:** Budgets are post-call hard caps on continuation; a single concurrent batch can overshoot before the cap fires.
- **Steps:**
  1. Add a pre-call token estimate gate before dispatching a batch fan-out, so concurrent calls respect the cap.
  2. Document the post-call semantics honestly (the homepage "hard spend cap, scoped to a block" is true for continuation, not pre-emptive).
- **Acceptance:** Deterministic test: a batch that would exceed the budget is gated before all requests fire (assert fewer-than-N fake calls); doc note added.

### Task 6.3 — Gemini empty-output footgun hint
- **Files:** `gemini.rs:34` (default model), response handling.
- **Context:** Default Gemini is a thinking model; small `:max-tokens` → empty string, exit 0, no warning.
- **Steps:**
  1. Detect empty content with non-zero thinking/usage and attach `.with_hint("empty response — increase :max-tokens for thinking models")`, or surface a soft warning.
- **Acceptance:** A response with empty content + reasoning tokens surfaces the hint rather than a silent empty string.

### Task 6.4 — Wire cassettes into CI (corpus coverage)
- **Files:** per `docs/plans/2026-06-21-llm-cassettes.md` (M3/M4); `crates/sema/tests/`, playground/notebook `llm-tools` examples, `Makefile`.
- **Context:** `FakeProvider` (Phase 1) covers hand-scripted unit scenarios; cassettes cover recorded real-provider corpora for examples and the agentic suite.
- **Steps:**
  1. Execute the cassette plan's M1–M3.
  2. Record tapes for the `llm-tools` playground examples and the agentic suite; commit them.
  3. Wire `SEMA_LLM_CASSETTE_MODE=replay` into `make test` so the LLM/agentic suite runs green with no keys.
- **Acceptance:** `make test` runs the LLM/agentic suite green in CI with zero credentials; at least one `.sema` example with a committed tape replays offline.

### Task 6.5 — Agent eval harness (DEFERRED — future item)

- **Status:** Deferred by owner (2026-06-21) — capture the intent, do not build yet.
  Sequenced after the testing foundation (Phases 1, 6.4) since evals reuse the
  same `FakeProvider`/cassette machinery as their deterministic substrate.
- **Why:** Agent frameworks are judged on whether the agent *accomplishes the
  task*, not whether the HTTP call succeeds. A repeatable eval harness (Mastra
  evals; DSPy/Instructor-style scoring) is both a robustness tool and a genuine
  differentiator, and it is the natural home for regression-testing the agent
  loop against real tasks once Phase 2 lands. Tracked in `docs/IDEAS.md` adjacent
  to `defschema`/`defpipe`.
- **Rough shape (to scope later):** a `deftest`/`eval`-style surface (cf. the
  `sema test` idea in `docs/IDEAS.md`) that runs an agent against a fixture task +
  cassette and scores the outcome (assertion, LLM-as-judge, or structured-output
  match), with pass/fail in CI. Reuses `FakeProvider`/cassettes for determinism.
- **Acceptance (when undeferred):** at least one agentic task scored
  deterministically in CI via a committed cassette, gating regressions in the
  tool loop.

---

## Execution order & gating

1. **Phase 0** — trivial accuracy fixes (parallelizable, do immediately).
2. **Phase 1** — `FakeProvider` + first tests. **Blocks Phases 2–4** (they need deterministic oracles).
3. **Phase 2** — critical tool-result bug. **Highest correctness priority.**
4. **Phase 3** — recoverable tool errors + input validation.
5. **Phase 4** — resilience (retry/backoff/timeout) + streaming parity.
6. **Phase 5** — observability (independent; can interleave after Phase 1).
7. **Phase 6** — feature gaps + cassette CI corpus.

## Overall "Done When"

- The same agent loop completes (non-empty, ≤2 rounds for a single-tool task) on
  both an OpenAI-shaped and an Anthropic-shaped provider — verified deterministically
  via `FakeProvider` and manually via live keys.
- The LLM/agentic test suite runs green in CI with **no API keys** (`FakeProvider`
  for unit scenarios, cassettes for corpus).
- A failing tool call no longer aborts `agent/run`; the model self-corrects.
- Retry covers transient 5xx/network/timeout with exponential backoff + jitter;
  `:max-retries` and `:timeout` are honored.
- Streaming respects budget/cache/rate-limit (or the bypass is documented), and
  surfaces tool calls on Anthropic/OpenAI.
- No public copy contains the tree-walker error or the "dynamic pricing" overstatement;
  `llm/classify` examples match the real signature.
- (Stretch) `SEMA_OTEL=1` emits standards-compliant LLM + tool spans; `llm/generate-object`
  returns schema-validated typed data with a bounded repair loop.
