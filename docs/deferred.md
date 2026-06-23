# Deferred items

Things that came out of the May 2026 quality sweep (Wave 6 audit) but were intentionally not fixed because they're too risky, too design-dependent, or have a cheap workaround. Each entry says *why* it's deferred so a future pass can decide whether to revisit.

## ASYNC-2 — Breakpoints/stepping don't fire inside async task code

**Found 2026-06-23.** A breakpoint set on a line that runs only inside an `async`/`async/spawn` task (or via `async/map`/`pool-map`/channels) is silently ignored, in both the WASM playground and the native DAP. Root cause: breakpoint/step checking lives in `VM::run_inner(ctx, Some(debug))` (`vm.rs:875`), but the scheduler steps every task via `run_async`/`execute_async` → `run_inner(ctx, None)` (`scheduler.rs:894`) — non-debug mode, no `DebugState` plumbing. Synchronous-code debugging is unaffected. **Deferred because** the fix is a debugger×scheduler integration with a stop/resume design decision (a breakpoint hit mid-task must pause the scheduler and round-trip a `DebugCommand` through the playground's step-driven JS bridge), not a localized patch. Full write-up: `docs/bugs/async-breakpoints.md`; scoped fix: `docs/plans/2026-06-23-async-debugger.md`.

## ASYNC-1 — Dynamic-scope flags vs deferred async tasks (cache/budget visibility)

**Found 2026-06-23, during the concurrent-`llm/*` work.** `llm/with-cache` (and similarly `llm/with-budget`, per-call `:tags`/`:metadata`) sets a **dynamically-scoped thread-local** (`CACHE_ENABLED`, `BUDGET_*`, `CALL_TAGS`…) for the duration of its thunk, then resets it. An async task spawned inside that thunk reads the flag **when it actually executes** — and the scheduler can defer that execution past the point where the thunk returned and the flag was reset. Symptom: a single `(llm/with-cache … (fn () (async/all (list (async/spawn (fn () (llm/complete …)))))))` often reports `:misses 0` in `(llm/cache-stats)` (the task ran with `CACHE_ENABLED` already reset), and the `async_cache_miss_is_counted` test was removed as flaky for this reason. **Caching itself still works** for async completions awaited in-extent (a same-prompt repeat is served as a hit), so this is primarily an *accounting/visibility* nuance — but the same mechanism could mean `llm/with-budget` does **not** reliably gate concurrent completions, which would be a real correctness gap. **Deferred because** the fix is a design decision (snapshot the dynamic scope onto each task at `async/spawn` time and reinstall it when the task runs — a per-task dynamic-environment capture, akin to the per-task OTel context swap already shipped), not a one-liner, and it's orthogonal to the concurrency/cancellation slices it surfaced under. Revisit when wiring budgets to concurrent agent fan-out.

---

Verified 2026-06-09: U6 ("did you mean" hints — shipped via `suggest_similar` in sema-core, attached in both backends) and U9 (REPL completeness check — replaced by the lexer-based `SemaValidator` in `crates/sema/src/repl/validator.rs`) were removed because they have since been fixed. Remaining entries re-verified as still open.

---

## VM-1 — Stack traces on runtime errors (error UX)

**Today:** the VM reports runtime errors with a message + actionable hints, but does **not** yet build a stack trace (`at +` / `at foo`, source spans) or attach the `:stack-trace` field to caught errors.

**Why hard:** arithmetic compiles to *intrinsic opcodes* (e.g. `ADD`) and many calls are `CALL_NATIVE`/intrinsics that don't carry a function identity, so a faithful trace needs either a lazy walk of `self.frames` (gives user-function frames, not the innermost intrinsic name like `+`) or a cheap per-call frame record.

**Acceptance suite:** 8 `#[ignore]`d tests in `crates/sema/tests/integration_test.rs` (`test_stack_trace_*`, `test_arity_error_shows_call_form`) encode the desired behavior — un-ignore them when implementing.

---

## D5 — Typed `try`/`catch` form

**Today:** `(try expr (catch e ...))` catches *every* error type, including `:unbound`, `:arity`, `:type-error` — the kind of errors that usually mean a typo. The docs (`website/docs/language/special-forms.md` near "Re-throw errors you don't intend to handle") explicitly warn about this.

**The bug shape:** silent bug-masking. A typo inside `try` is swallowed and the catch block runs as if the operation failed for "real" reasons.

**Proposed fix (not done):** add `(catch [:user :type-error] e ...)` syntax that filters by the `:type` field, mirroring Clojure's `catch ExceptionType` or Common Lisp's `handler-case`. Optionally lint-warn on the un-filtered form.

**Why deferred:** non-trivial language design. Affects reader (new pattern in catch clause), special-form lowering in both backends, and prelude macros that use `try`. Needs an ADR before code.

**Workaround today:** users can do `(try ... (catch e (if (= (:type e) :user) (handle e) (throw e))))` to re-raise unexpected errors. That's a documented pattern in special-forms.md.

---

## N5 — `server.rs` response-helper `.unwrap()`s

**Today:** `crates/sema-stdlib/src/server.rs` lines ~1028-1099 (as of 2026-06-09) unwrap on `as_map_rc()` / `__stream_handler` / `__ws_handler` after a single-marker `is_*_response` check. A user who constructs a partially-formed response map (sets `__file_path` flag but forgets `__stream_handler`) panics the HTTP server thread.

**Proposed fix:** convert each unwrap to `.ok_or_else(|| SemaError::eval("..."))?` and propagate via `Result<ServerResponse, SemaError>` — sending a `ServerResponse::Error` over the oneshot instead of panicking.

**Why deferred:** the helper functions return `()` today; restructuring to propagate errors via the existing `oneshot::Sender<ServerResponse>` requires a new `ServerResponse::Error` variant and changes to the axum-side handler. Medium-effort refactor with non-trivial blast radius.

**Workaround today:** users normally build response maps with `http/ok`, `http/file`, etc. — those constructors always produce well-formed maps. The bug only triggers if a user builds a map by hand with the wrong `__*` markers. Low-likelihood in practice.

---

## N7 — `sort` accepts heterogeneous types silently

**Today:** `(sort (list 1 "a" {:k 1}))` returns an order based on `Value`'s `Ord` impl (which depends on Spur indices and tag order). Reproducible within a process but not portable, and it's never what the user wanted.

**Proposed fix:** either (a) raise a type error when the input is heterogeneous, or (b) define a stable cross-type total order and document it.

**Why deferred:** design call. Strictness is the safer choice for users but breaks anyone relying on the current behavior; defining a stable order is a long-term spec commitment. Wants an ADR.

**Workaround today:** `(sort-by ...)` with an explicit comparator — works correctly across types because the user provides the comparator.

---

## L2 — Code-lens execution + `sema/evalResult` notification untested e2e

**Today:** `crates/sema-lsp/tests/e2e/test_code_lens.py` only verifies the lens command name; it never calls `workspace/executeCommand` with `sema.runTopLevel` and never listens for the `sema/evalResult` custom notification described in `website/docs/lsp.md:138-150`. A regression in the eval subprocess path or the notification payload would slip through.

**Proposed fix:** add a python e2e test that:
1. Sends `workspace/executeCommand { command: "sema.runTopLevel", arguments: [{uri, formIndex: 0}] }`
2. Waits on the client's incoming-notification queue for `method == "sema/evalResult"` with a small timeout
3. Asserts payload includes `ok`, `value`, `elapsedMs` fields

Pattern can mirror the diagnostic-waiting in `test_diagnostics.py`.

**Why deferred:** medium effort — the python test harness needs to handle async notifications cleanly, and the test depends on a subprocess `sema eval` running and returning. Not flaky-prone, just a lift to write right.

**Workaround today:** the unit-level path (the lens command itself) is tested. Integration regressions would surface during manual testing of the editor extension.

---

## VFS — clones on every read

**Today (updated 2026-06-09):** `vfs_read` returns `Option<Vec<u8>>`, cloning file contents on each call — the function now lives in `crates/sema-core/src/vfs.rs:15` (the embedded-binary VFS). The originally-cited `crates/sema-notebook/src/vfs.rs` has since become a different thing (disk-backed path-sandboxed shim) and is no longer relevant to this entry.

**Proposed fix:** return `Cow<'_, [u8]>` so cached reads can be borrowed, or back the VFS with `Arc<HashMap>` so the file table can hand out cheap reference-counted handles.

**Why deferred:** identified in PR #14 review (severity: medium). VFS read isn't a current hotspot — the notebook is interactive, not a high-throughput file server. Revisit if the notebook starts serving real bundles.

---

## CORE-2 — recursive-closure Rc cycle (memory leak), both backends

**Today:** a self-referential closure forms an `Rc` cycle that reference counting can't
reclaim. On the **tree-walker** it's the whole-`Env` capture (`Lambda { env: Env }` +
the env binding the name → the lambda). On the **VM** it's narrower but real: a
local/returned recursive closure captures its own name as an `UpvalueCell` whose
`Closed(Value)` holds the closure (`crates/sema-vm/src/resolve.rs:280-297`;
`docs/plans/2026-02-16-compilation-strategy-investigation.md:1014-1016` calls it "the
MOST common source of long-lived reference chains"). Top-level defines (globals) avoid it.

**Correction (2026-06-18):** an earlier note claimed retiring the tree-walker closes
CORE-2 because "the VM is cycle-free." That is **wrong** — the VM has its own cycle (above).
Retiring the TW removes only the whole-`Env` variant.

**Real fix:** cycle collection / a tracing GC over the `Rc<Value>`/`Env`/`UpvalueCell`
graph (every production Scheme ships one for exactly this reason). The attempted `Weak`
captured-env fix was dropped — it broke the common "module exports a fn calling a private
helper" pattern (`vm_module_test`).

**Why deferred:** only bites very long-lived sessions (REPL/notebook/server) with repeated
recursive local defines; CLI/script runs are unaffected. A GC is a large investment; revisit
when a real long-running workload shows growth (a `Rc::strong_count` leak test would size it).

---

## WASM-4 — `register_wasm_io` is a single ~1093-line function

**Today:** `crates/sema-wasm/src/lib.rs` registers all WASM I/O builtins in one ~1093-line function. Large WASM functions carry a known V8 Turboshaft miscompilation/crash risk on ARM64 (see the chromium-wasm-crash note in MEMORY).

**Proposed fix:** split into smaller per-area registration functions (pure refactor, no behavior change).

**Why deferred (decided 2026-06-18):** latent risk only; the crash has not been observed since. Revisit if it recurs in the playground. Large diff on a hot path, not worth the churn now.

---

## C1 follow-up — caught-HOF-callback errors lack a stack trace

**Today:** after the C1 fix (HOF callbacks routed into the running VM), one residual symptom of wrapping a VM closure as a `NativeFn` remains: a VM error caught from inside a HOF callback lacks a `:stack-trace`. (The sibling `(type (fn …))` → `:native-fn` artifact was fixed 2026-06-19 via the `NativeFn::is_closure` marker — see VM-2 above, now resolved.)

**Why deferred (decided 2026-06-18):** cosmetic / low-impact; it stems from the closure-as-NativeFn boundary, not from upvalue timing (which C1 fixed). Tied to VM-1 (stack traces). Revisit if it bites real usage.

---

## LC — Living Code LLM layers (`ask` / `heal!` / `evolve` / `observe!` / `become!`) — killed for good

**What it was:** layers 3–6 of the Living Code design (`docs/design/living-code.md`) — LLM-driven introspection (`ask`, `ask/code`, `ask/patch!`), auto-repair (`heal!`), genetic programming (`evolve`), and runtime self-modification (`observe!`, `become!`, `history`, `rollback!`, `freeze!`). Shipped on the tree-walker (PR #30, commits `248ebd8`/`fb0d7e6`/`69f1514`), then silently dropped when the tree-walker was retired in 1.18.0 — never ported to the VM, unbound at runtime, undiscovered for two releases.

**Why killed (not deferred):** (1) non-deterministic by construction — `evolve`/`heal!` emit a fresh LLM sample each run, so there is no regression test you can write, which is *exactly* how it rotted unnoticed; (2) `become!` (LLM rewrites a running function in place) carries a safety surface — doctest gates, sandboxes, rate limits, audit logs, freeze switches, rollback history — larger than the feature itself, a permanent tax on every VM/env change; (3) zero demand — no issue, no playground example, no website doc referenced it, and nobody noticed its disappearance.

**Salvage — also parked:** the whole feature is parked, nothing implemented. Only layer 0 (runtime docstrings `doc`/`meta`) was seriously considered, and a feasibility pass confirmed it's *clean* to build — the `Function` struct already carries serialized compile-time metadata (`source_file`, `local_scopes`), so a `doc` field rides the same path and the `.semac` string table (no source-text drag, binary path inherits it free). But with doctests + the LLM layers gone, `doc`/`meta` alone wasn't worth the standing maintenance (a `.semac` format-version bump + ~10 `Function` construction sites to carry forever), so it was **cut for maintainability** (2026-06-20) and parked as a clean plan to revisit later: `docs/plans/archive/2026-06-20-docstrings-and-introspection.md`. **Doctests (layer 1)** were dropped earlier as YAGNI. **Layer 2** (`read-source`/`source-of`/`;;@directives`) was scaffolding for the dead LLM layers — not salvaged either.

**Artifacts retired 2026-06-20:** PR #30 closed; `docs/plans/2026-02-24-living-code-phase4.md` archived; `docs/design/living-code.md` banner-marked RETIRED.

---

## P6 — `partition` / `frequencies` / `list/group-by` double-clone (perf, won't-do)

`crates/sema-stdlib/src/list.rs`. These clone each element twice (once for the callback args, once when pushing into the output bucket). Could be cut to one clone by consuming `items.iter().cloned()`.

**Why won't-do:** moved here from `docs/wip.md` on 2026-06-20. The earlier P1 work established that `Rc::clone` is too cheap to measure on these HOF-dispatch-bound paths; the same applies here. Revisit only if a profile actually fingers `partition`/`group-by` as a hotspot.

## P7 — `CALL_NATIVE` clones `Rc<NativeFn>` per call (perf, spiked → discarded)

`crates/sema-vm/src/vm.rs`, CALL_NATIVE handler: `let native = self.native_fns[native_id].clone();` — one `Rc` bump per native call, purely to release the borrow on `self.native_fns` so `self.stack` can also be borrowed.

**Spiked and discarded 2026-06-20.** Implemented the raw-pointer alternative (`Rc::as_ptr` + a minimal `unsafe` deref; the safety invariant holds — `native_fns` is built once at VM construction and never mutated during dispatch). It compiled, passed all tests + clippy, and was correct. But benchmarking before/after on `higher-order-fold`, `hashmap-bench`, and `string-pipeline` showed the delta entirely within noise (means < 1σ apart; the "winner" sign even flipped across workloads). A single non-atomic `Rc` bump on a single-threaded VM is free in practice. Adding `unsafe` to the hottest dispatch path — plus the standing burden of re-auditing the "never mutate `native_fns`" invariant on every future edit — for zero measured gain makes the codebase strictly worse. Not doing it. The only lever here is the `unsafe` one (a safe borrow-restructure is blocked by the re-entrant-HOF `&mut self` path), so this stays closed unless the call shape changes materially.

---

## TOOL-1 — Migrate the Makefile to a justfile

**Deferred (revisit later) — 2026-06-22.** The build/dev/deploy automation lives in a
single growing `Makefile`. The intent is to move it to a [`justfile`](https://github.com/casey/just),
whose tooling is better suited to a task runner (real argument passing, no `.PHONY`
bookkeeping, no tab/space footguns, per-recipe shebangs, `just --list` discovery, dotenv
support). Not urgent — the Makefile works — so parked until there's appetite for the
one-time port plus updating CI and the docs that reference `make` targets. When it
happens, mirror the current targets (`build`/`release`/`test`/`lint`/`examples`/
`smoke-bytecode`/`deploy`/`deploy-all`/…) one-for-one first, then improve.

## TOOL-2 — Speed up CI drastically (it's painful)

**Deferred (revisit later) — 2026-06-22.** A release cycle takes painfully long: the
`verify` gate (full `cargo test --workspace` + examples + smoke-bytecode + lint +
docs-check) runs ~12–15 min on a **cold** cache, and it runs **per workflow** (CI on the
branch push, `publish.yml` verify on the tag, `publish-npm.yml` verify on the tag) — so a
release re-builds the world several times. Observed leads for a future push:

- **Caching is the big lever.** `Swatinem/rust-cache` keys per *job*, so each workflow's
  verify job has its own (often cold) cache; warm it / share it, or move to `sccache`
  with a shared backend. Cold-cache full builds are the dominant cost.
- **Split the gate for fast-fail.** Run `fmt` + `clippy` + `docs-check` as a quick job
  that fails in ~1 min; run the heavy `cargo test`/examples/smoke separately and in
  parallel (test sharding via `cargo-nextest --partition`).
- **Don't re-verify per registry.** crates.io and npm publishes each gate on `verify`
  today (kept separate because npm's OIDC whitelists the workflow *filename* — see
  `publish-npm.yml`). Find a way to share one verify result across both without breaking
  the OIDC filename match (e.g. a reusable verify that both `needs:`, gated so it runs
  once per SHA).
- **Faster runners.** GitHub's free runners are 2 vCPU. Managed drop-ins that work on a
  *personal* account (not just orgs): **Namespace** and **Ubicloud** (Blacksmith is
  org-only). ~2–3× wall-clock on a compile-heavy Rust suite.
- **cargo-dist Windows flakiness** (separate but related): the Windows build intermittently
  fails fetching from crates.io; mitigated by `.cargo/config.toml` (`[http] multiplexing
  = false`, `[net] retry = 10`) — keep an eye on whether that's enough.

---

## CASS-1 — Cassette tape corpus + replay-in-CI (cassettes M4)

**Deferred (revisit later) — 2026-06-22.** Cassette M1–M3 shipped in 1.23.0 (record/replay
for `complete`/`chat`/`extract`/agents/streaming/embeddings; `with-cassette` + `llm/cassette-*`
+ env vars). M4 — making the LLM/agentic suite run keyless in CI off committed tapes — is
unstarted. The implementation plan was archived to `docs/plans/archive/2026-06-21-llm-cassettes.md`.
Remaining work:

- **Record a tape corpus** for the playground `llm-tools` examples and the agentic test
  suite; wire `SEMA_LLM_CASSETTE_MODE=replay` into `make test` so the suite runs green with
  no API keys. (The keyless oracle today is the scripted `FakeProvider`; cassettes would add
  real-response replay on top.)
- **Open questions** carried from the plan: a `NullProvider` inner so pure-replay needs zero
  credentials; tape versioning/migration when `ChatRequest`/`ChatResponse` shapes change (the
  `"v":1` field is the hook); tapes beside tests (`tests/tapes/`) vs. a top-level `cassettes/`
  (leaning beside-tests); one-tape-per-test vs. shared (leaning per-test).

---

## LLM-1 — LLM bulletproofing remnants (from the archived plan)

**Deferred (revisit later) — 2026-06-22.** The bulletproofing plan
(`docs/plans/archive/2026-06-21-llm-bulletproofing.md`) shipped Phases 0–3, 4.1, 4.2, 4.4,
5, and 6.3. What's left:

- ~~**4.3 — streaming through the dispatch layer**~~ ✅ **DONE 2026-06-23.** `llm/stream`
  now applies rate-limit + fallback at stream-open and an opt-in budget pre-gate
  (`:on-stream :pre-gate`); mid-stream failure surfaces + keeps the partial (no failover —
  the spike proved a retry would duplicate). Cache stays off for streams (cassettes cover
  deterministic replay). Verified live.
- **6.1 — `llm/generate-object`**: schema-validated structured output with a bounded repair
  loop (today only `llm/extract` does schema+reask). Reuse `validate_extraction` +
  `format_reask_prompt`.
- **6.2 — batch budget pre-flight**: budgets are post-call caps, so a concurrent
  `llm/batch`/`llm/pmap` fan-out can overshoot before the cap fires. Add a pre-dispatch
  token-estimate gate.
- **6.5 — agent eval harness**: a `deftest`/`eval` surface that scores an agent against a
  fixture task + cassette in CI. Explicitly deferred by owner; reuses FakeProvider/cassettes.

(Cassette CI corpus — plan's 6.4 — is tracked separately as CASS-1.)

---

## PG-1 — Playground → downloadable native binary

**Deferred (revisit later) — 2026-06-23.** Captured 2026-06-19 as a curiosity and
archived to `docs/plans/archive/2026-06-19-playground-binary-export.md`. The
playground runs the WASM build, but `sema build` isn't compilation — it's
concatenation (`[stock runtime] + [VFS archive] + [trailer]`), so the browser
could produce a byte-identical runnable native binary with **no compiler**: pick a
target, fetch the stock runtime (ideally mirrored same-origin on sema.run), append
the archive built from the editor contents, write the `SEMAEXEC` trailer, download.

**Feasibility high, effort low (~half a day)** — mostly UI + hosting the runtime
mirror. Preferred first step: factor archive-writing into a lib and expose a
`sema-wasm` binding returning `Uint8Array` (avoids format drift vs. reimplementing
the format in JS). Pointers: `crates/sema/src/archive.rs` (format),
`crates/sema/src/cross_compile.rs` (`SUPPORTED_TARGETS`, runtime download/cache),
`crates/sema/src/main.rs` `Commands::Build` + `pkg.rs`.

**Why deferred:** not scheduled — no demand pull, just an attractive proof-of-concept.
Resume from the plan's "Smallest proof-of-concept" section.

---

## A note on the truly long-term language design items

These are not deferred — they're design questions that need a deliberate decision before any code lands. They're tracked in `docs/wip.md` (the "Wave 6c" cluster), not here.
