# Concurrent `llm/complete` · `classify` · `extract` + True Cancellation

**Status:** Planning → implementing (2026-06-23). Follow-on from the shipped
concurrent-`llm/embed` slice (`docs/plans/2026-06-23-async-agent-parallelization.md` §8.5).
**Branch:** `feat/async-awaitio`.

> **Prereq landed:** the adversarial-#7 reap was narrowed to TERMINAL-only +
> `async/timeout` now cancels its target (`cancel_promise_task`), fixing the
> `async-pipeline`/`async-stress` regression (`0b08e0f`). The "abandoned task's
> detached span finalizes while TLS is alive" guarantee that this slice depends on
> is now provided by cancel-on-timeout, not by a blanket clear.

---

## Slice A — concurrent single-shot completions

### What & why
`(async/pool-map llm/complete prompts N)`, parallel `llm/classify` over a batch, and
batch `llm/extract` should overlap their network round-trips instead of serializing on
the one VM thread — the same win `llm/embed` already has. This is the headline batch-LLM
scenario (parallel summarize/classify/extract over a corpus).

### The mechanism (mirror `llm/embed`, scaled to `do_complete`)
`do_complete` is a **single** completion (internal retry + fallback + cache + cassette);
the natives (`llm/complete`/`classify`/`extract`) call `track_usage` and shape the final
`Value` *after* it. A native that yields is **not re-invoked on resume** (the scheduler
resumes the bytecode after the CALL via `replace_stack_top`), so — exactly as for `embed`
— **all** post-call work (span finalize, cache store, cassette record, `track_usage`,
content→Value) must run **in the poller**, on the VM thread, when the future lands.

**Offload the whole retry+fallback unit via `spawn_blocking`** (the embed precedent).
This *reuses the sync `provider.complete()` path* — so the OpenAI `DROP_TEMPERATURE`
self-heal, network retry, and `set_serving_provider` are all preserved with **zero
drift** (dissolves blockers B1 + the retry-drift / DROP_TEMPERATURE majors from the parent
plan — they only existed for the abandoned "spawn `complete_async`" approach).

Per-completion flow when `in_async_context()` (native targets only):
1. **VM thread (native), pre-yield:** open `conversation` scope if none; `llm_span_detached("chat")`; `set_request`/`set_output_type`/`set_tools`/`apply_call_telemetry`; resolve cache key + lookup (**hit → finalize span inline, `track_usage(0)`, return value, NO yield**); cassette decision (**replay → finalize inline, no yield; miss → Err; record/none → proceed**); resolve the fallback chain (or default provider) into `Arc<dyn LlmProvider>` clones + per-entry model + `max_retries`.
2. **Offload:** `shared_rt().spawn_blocking(move || run_fallback_retry(chain_arcs, request, max_retries))` → `Result<CompleteOutcome, String>` where `CompleteOutcome { resp, serving_provider, retry_events: Vec<RetryEvent> }`. Backoff `std::thread::sleep` runs on the **worker**, never the VM thread. Retry attempts are collected as **data** (`RetryEvent { attempt, kind, msg, wait_ms }`), NOT emitted as spans on the worker (no otel TLS there).
3. **Yield** `AwaitIo`; `notify_io_complete()` in the worker on send.
4. **Poller (VM thread, on Ready):** emit `retry_span` children from `retry_events`; `set_dispatch`/`set_response`/`set_messages` on the detached chat span; `set_serving_provider`; `store_cached`; cassette `record_entry`; `track_usage(&resp.usage)` (budget-Err → `IoPoll::Ready(Err)`); then the per-native **`finalize(resp) -> Result<Value, SemaError>`** closure → `IoPoll::Ready(value)`. On Err: `span.record_error` + `Ready(Err)`.

`finalize` per native:
- `llm/complete` → `Value::string(&resp.content)` (prompt-string path; `complete_with_prompt`/`llm/chat` keep sync for now).
- `llm/classify` → category keyword-or-string (existing parse).
- `llm/extract` → parse+validate. **Attempt-0 only is offloaded.** On validation failure with retries remaining, the re-ask attempts run on the **sync** `do_complete` path inside the finalize (VM thread; documented: a re-asking extract briefly blocks siblings — the common single-attempt extract is fully concurrent). This avoids re-yield-from-poller; revisit if real workloads re-ask often.

### Refactor needed (B5)
Extract from `do_complete` a **synchronous on-VM-thread stage** (conv scope, span open,
request-attr set, cache key+lookup, cassette decision, provider/chain resolution) and an
**offloadable wire stage** (`run_fallback_retry`). Introduce `complete_with_retry_collecting`
returning `(ChatResponse, Vec<RetryEvent>)` so **both** the sync path (emits retry spans
inline) and the async path (emits in poller) share one retry loop — no behavioral drift.
The existing sync `do_complete` keeps its current behavior (re-expressed in terms of the
shared stages); FakeProvider sync tests must stay byte-identical.

### FakeProvider
Add a per-reply `delay_ms` (chat) honored by the offloaded path so the "in-flight ≥ 2"
deterministic gate is provable without keys (mirrors the embed `embed_delay`).

### Acceptance gate (Slice A)
- **Deterministic (FakeProvider, CI):** `(async/pool-map llm/complete prompts 3)` over a delayed fake → wall ≈ max-not-sum; peak in-flight ≥ 2; results correct & input-order-preserved; `llm/classify` batch overlaps; sync path byte-identical (existing FakeProvider tests untouched); per-task OTel spans isolated (N distinct chat spans, no cross-contamination); cache-hit & cassette-replay still no-yield + zero-usage.
- **Accounting:** `track_usage` runs exactly once per completion (no double-charge; the native no longer accounts); budget overrun fails the task.
- **Live (keys in env, cheap models):** 4× concurrent `llm/complete` overlap vs serial; one real `llm/classify` pool.
- **No regressions:** `cargo test --workspace`, `make examples`, `make lint` all green.

---

## Slice B — true cancellation (AbortHandle)

### Current state (best-effort)
On `async/cancel` / `async/timeout` expiry the task is marked terminal and its
`oneshot::Receiver` drops, but the in-flight future runs to completion (HTTP request
finishes, subprocess keeps running, tokens still spent). `cancel_promise_task` (just
added) drops the `IoHandle` but does not abort the underlying work.

### The mechanism
Add an **abort seam** to `IoHandle`: an optional `on_abort: Option<Box<dyn FnOnce()>>`
set at construction, run by a new `IoHandle::abort(&self)`. The scheduler invokes it
whenever a task holding `Blocked(AwaitIo)` transitions to cancelled/failed —
i.e. inside `cancel_task`, `cancel_promise_task` (timeout), and the interrupt-clear path.

Wire real abort where the runtime supports it (two honest tiers):
- **http/shell (`spawn`-based, async):** capture the tokio `JoinHandle::abort_handle()`; the abort hook calls `.abort()` → drops the in-flight reqwest future (connection torn down) / the subprocess future. Add `kill_on_drop(true)` to the `tokio::process::Command` so an aborted subprocess is actually killed.
- **LLM (`spawn_blocking`, blocking reqwest):** `spawn_blocking` work **cannot** be interrupted mid-call. The abort hook drops the receiver (result discarded). **Honest limitation, documented.** The *round-level* win is real: with Slice A's poller state machine, a cancelled multi-round `complete`/re-asking `extract` issues **no further rounds**.

### Acceptance gate (Slice B)
- **http/shell abort observed:** an `async/timeout` over a slow local HTTP endpoint / a `sleep 10` subprocess → the connection is dropped / the child process is **killed** (assert via a side-effect file the subprocess would write only if it ran to completion, or process-table check) within ~timeout, not after the full duration.
- **Siblings freed immediately** on cancel (already true; re-assert).
- **No teardown abort** (the #7 gate stays green), no double-free, abort hook runs at most once.
- **LLM tier documented** as best-effort-with-round-cutoff; a FakeProvider test asserts a cancelled 3-round loop stops issuing rounds.
- **No regressions:** workspace + examples + lint green.

---

## Execution
TDD, commit per step, on `feat/async-awaitio`; fast-forward to `main` only after the
full gate (workspace + `make examples` + `make lint`) is green AND an independent
adversarial pass clears it (the discipline that caught the macro-vs-native, the
`IO_INFLIGHT` underflow, and this very reap regression). Bench: `async/pool-map
llm/complete` speedup + cancel latency. Then docs inventory + dynamic-workflows plan review.
