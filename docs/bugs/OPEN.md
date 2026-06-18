# Open bug items

The single list of genuinely-open findings as of 2026-06-18, after the triage
(`2026-06-18-triage.md`) and the two fix passes that closed 22 findings. Every
other entry from the 2026-05-29 audit and the old standalone write-ups is
**fixed** — see git history + the triage record. Items below are split into
"needs a decision" (we want your call before touching them) and "planned, just
needs doing".

---

## Needs a decision

### C1 — `set!` through stdlib HOF callbacks silently lost on the VM  ·  HIGH
`crates/sema-vm/src/vm.rs`. `(let ((c 0)) (map (fn (x) (set! c (+ c x))) '(1 2 3)) c)` returns `6` on the tree-walker but `0` on the VM: the VM closes open upvalues before every non-VM call, so mutations made inside stdlib HOF callbacks don't flow back. Silent wrong results + backend divergence.
- **Option 1** — stop closing upvalues before non-VM calls; rely on the open-cell check on every upvalue access. Smaller change, reintroduces a per-access branch (needs perf measurement).
- **Option 2** — route HOF callbacks in-VM (extend `call_callback` to carry VM context), eliminating the fresh-VM fallback. Architecturally cleaner, larger.

### CORE-2 — every named `define` creates an Rc cycle (env → lambda → env)  ·  memory leak
`crates/sema-eval/src/special_forms.rs` (eval_define) + `crates/sema-core/src/value.rs` (Env). Self-referential closures never drop → unbounded growth in long REPL/notebook/server sessions.
- **(A)** `Weak` ref for the lambda's captured env (preserves self-recursion + clean semantics; most complex).
- **(B)** post-define patch removing the lambda from its own captured env (loses self-recursion).
- **(C)** accept the cycle as intended GC-free design, document it, add a REPL memory-bound test.

### time/parse treats all naive datetimes as UTC  ·  correctness
`crates/sema-stdlib/src/datetime.rs:36-42`. `(time/parse "2024-01-15 12:30:00")` returns the same timestamp regardless of `$TZ`. The old `test_time_parse` was widened to *mask* this rather than fix it.
- **(A)** document that naive strings are interpreted as UTC (no code change; predictable, simplest).
- **(B)** add an optional timezone argument, defaulting to UTC.
- **(C)** interpret naive strings in the machine's local timezone (matches some user intuition; non-deterministic across machines/CI).

### NB-2 — notebook server has no auth on mutating endpoints  ·  security
`crates/sema-notebook/src/server.rs`. `/vfs/write` and other mutating routes are unauthenticated. Fine for trusted localhost, dangerous if bound to a non-loopback interface.
- **(A)** document as localhost-only + bind to 127.0.0.1 by default (no auth; minimal).
- **(B)** add a token/secret gate on mutating endpoints.

### WASM-4 — `register_wasm_io` is a single ~1093-line function  ·  reliability
`crates/sema-wasm/src/lib.rs`. Large single function carries a known V8 Turboshaft large-function miscompilation/crash risk on ARM64 (see the chromium-wasm-crash note). Pure refactor (split into smaller fns), no behavior change, but large and touches the hot WASM registration path.
- **(A)** refactor now (mitigates the crash risk; large diff, careful review).
- **(B)** defer until/unless the crash recurs.

### STD-10 — `db/exec-batch` is an SQL-injection surface  ·  security (design)
`crates/sema-stdlib/src/*` (sqlite). Batch exec takes raw SQL with no parameterization.
- **(A)** document it's only for static SQL + steer users to `db/exec` with params (no code change).
- **(B)** add guardrails / reject interpolated input (harder; may break legitimate uses).

### eval-tw oracle circularity in dual-eval tests  ·  test integrity
`crates/sema/tests/dual_eval_*.rs`. Many tests use the tree-walker to build the *expected* value, so a shared reader/fundamental-op bug would be invisible. Partial fix exists.
- **(A)** complete the conversion of foundational ops to hand-constructed literal expected values (~23 ops; medium).
- **(B)** accept + document as a known limitation (the VM variant still catches divergence on higher-level ops).

### platform-specific-windows — tests assume Unix  ·  portability
`crates/sema/tests/integration_test.rs` (path_join, `/tmp` hardcodes). No Windows CI today.
- **(A)** invest now (cfg-gate or normalize separators + temp_dir).
- **(B)** defer until Windows support is on the roadmap.

### fragile-error-message-matching — permission errors still string-matched  ·  test quality (minor)
~26 `.contains("Permission denied")` / `.contains("outside allowed directories")` checks remain; could match `SemaError::PermissionDenied`/`PathDenied` structurally.
- **(A)** extend structured matching (small, mechanical).
- **(B)** accept current state (Eval catch-all matching is already documented as acceptable).

---

## Planned — just needs doing (no decision required)

### VM-1 — `.semac` bytecode verifier has no `CallNative` arm  ·  P0
`crates/sema-vm/src/serialize.rs` `validate_chunk_bytecode`. A crafted `.semac` with an out-of-range `native_id` passes validation (only a `debug_assert!` guards it at runtime → release panic / OOB). Fix: thread the native-table length into the verifier and add a `CallNative` arm validating `native_id < n_natives`. Small, high-value — slated for the next fix pass.
