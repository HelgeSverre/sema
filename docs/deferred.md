# Deferred items

Things that came out of the May 2026 quality sweep (Wave 6 audit) but were intentionally not fixed because they're too risky, too design-dependent, or have a cheap workaround. Each entry says *why* it's deferred so a future pass can decide whether to revisit.

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

**Salvage:** only layer 0 — runtime docstrings (`doc`/`meta`) — is being revived, tracked in `docs/plans/2026-06-20-docstrings-and-introspection.md` (not under the "living code" brand). The `Function` struct already carries serialized compile-time metadata (`source_file`, `local_scopes`), so a `doc` field fits the existing pattern. **Doctests (layer 1) were considered and dropped as YAGNI** (2026-06-20): a net-new `sema test` subcommand + runner only earns its keep if doctests actually get written. Layer 2 (`read-source`/`source-of`/`;;@directives`) was mostly scaffolding for the dead LLM layers and is *not* salvaged either — it needs reader surgery and source-text storage the VM doesn't have, for speculative demand.

**Artifacts retired 2026-06-20:** PR #30 closed; `docs/plans/2026-02-24-living-code-phase4.md` archived; `docs/design/living-code.md` banner-marked RETIRED.

---

## A note on the truly long-term language design items

These are not deferred — they're design questions that need a deliberate decision before any code lands. They're tracked in `docs/wip.md` (the "Wave 6c" cluster), not here.
