# Plan: Retire the tree-walker (VM becomes the only evaluator)

**Status:** scoping / not started. **Size:** large, multi-phase (not a single PR).
**Goal:** the bytecode VM is the sole evaluator. Remove the tree-walking interpreter,
the `--tw` flag, the dual-backend test harness, and all "two backends" framing in
the docs.

## The reality check (why this is bigger than "delete sema-eval")

The VM is **not** independent of the tree-walker today. It borrows it for four
subsystems — all of which must get VM-native (or standalone) replacements *before*
the tree-walker can be deleted:

1. **Macro expansion** — `Interpreter::expand_for_vm` (`crates/sema-eval/src/eval.rs:196`)
   evaluates `defmacro` bodies **via the tree-walker** (`eval_value`) to register macros,
   then expands macro calls, before handing forms to `sema_vm::compile_program`. The VM
   has no macro evaluator of its own. **This is the critical-path blocker.**
2. **Module `load` / `import`** — `__vm-load` / `__vm-import` delegate back to the
   tree-walker's `eval_load` / `eval_import` through the eval callback
   (`eval.rs:899-935`).
3. **The stdlib eval/call bridge** — `set_eval_callback(eval_value)` /
   `set_call_callback(call_value)` (`eval.rs:82-83`) point at tree-walker functions.
   Stdlib HOFs, the `eval`/`apply` runtime path, and async callbacks call through these.
   (C1 routes HOF callbacks into a running VM when present, but the registered targets
   and the no-VM fallback are still the tree-walker.)
4. **Prelude loading** — `load_prelude` (`eval.rs:890`) evaluates the Sema prelude at
   startup via the tree-walker.

So the work is: **make the VM self-sufficient in (1)–(4), migrate every consumer and
test off the tree-walker, then delete it.**

## Full coupling inventory (grounded in the tree at this commit)

### Code
- `crates/sema-eval` — hosts the tree-walker (`eval.rs` `eval_value`/trampoline,
  `special_forms.rs`) **and** shared machinery we must keep or re-home: the
  `Interpreter` struct, the module system (`EvalContext` cache, `eval_load`/`eval_import`),
  prelude loading, macro registration, and the eval/call callback wiring.
- CLI: `--tw` flag (`crates/sema/src/main.rs:155,330`) + `eval_with_mode*` dispatch;
  REPL `use_vm` mode (`crates/sema/src/repl/commands.rs`).
- Public/embedding API: `Interpreter::eval`, `eval_str`, `eval_string`, `eval_in_global`
  are tree-walker entry points (`eval.rs:117-137`). Used by embedders (see
  `website/docs/embedding.md`, `embedding-js.md`).
- Consumers still on the tree-walker:
  - **sema-wasm** — the playground "Tree" engine toggle calls `eval_string` (TW)
    (`lib.rs:1590,1640,1743`); other paths use `eval_str_compiled` (VM). Removing TW
    removes the playground's Tree/VM switch.
  - **sema-dap** — `Interpreter::new()` then debug executes on the VM; verify no TW eval.
  - Already VM-only: **sema-notebook** (`eval_str_compiled`), **sema-mcp**
    (`eval_str_compiled` / bytecode).

### Tests (the big migration)
- `crates/sema/tests/common/mod.rs`: `eval_tw` (= `eval_str`, TW) and `eval_vm`
  (= `eval_str_compiled`, VM); `dual_eval_tests!` / `dual_eval_error_tests!` generate a
  `_tw` and a `_vm` case each.
- Dual-eval suites (~1,487 cases across 9 files): collections 264, core 180, data 67,
  ergonomic 25, io 86, map 147, stdlib 196, `dual_eval_test` 455, types 67.
- **`integration_test.rs` (~1,025), `http_test.rs`, `server_test.rs`** — their local
  `eval()` helper uses `eval_str` → **tree-walker**. These all run on the TW today.

### Docs
- 22 `--tw` mentions across `cli.md`, `dap.md`, `embedding-js.md`,
  `language/macros-modules.md`, `language/special-forms.md`, and several `internals/*`.
- Tree-walker is described as a backend (and `internals/evaluator.md` is *about* it) in
  `internals/architecture.md`, `evaluator.md`, `bytecode-vm.md`, `performance.md`,
  `lisp-comparison.md`. CLAUDE.md also documents the dual-eval testing model.

## Phased plan (dependency-ordered)

**Phase 0 — Architecture decision.** Where do macro expansion, module load/import,
prelude loading, and the `Interpreter` API live once the TW is gone? Likely: a slimmed
`sema-eval` that owns macro expansion + module system + `Interpreter`, implemented on the
VM; or fold these into `sema-vm`. Decide crate boundaries first.

**Phase 1 — Make the VM self-sufficient (the hard engineering).**
- 1a. **Macro expansion without the TW.** Either compile+run `defmacro` bodies on the VM
  to produce the expander, or build a small dedicated macro evaluator. Must preserve
  quasiquote/unquote/splicing, `macroexpand`, auto-gensym, and prelude macros.
- 1b. **`load`/`import` on the VM** — replace the `__vm-load`/`__vm-import`→TW delegation
  with VM-native module evaluation (the load-on-VM groundwork exists; finish import).
- 1c. **eval/call bridge → VM** — repoint `set_eval_callback`/`set_call_callback` (and the
  `eval`/`apply` runtime) at VM-based implementations; keep C1's in-VM HOF routing.
- 1d. **Prelude on the VM** — load the prelude via the VM at startup.

**Phase 2 — Switch consumers to the VM.** Embedding API (`Interpreter::eval*` → VM),
sema-wasm (drop the Tree toggle; all eval → VM), confirm dap/lsp.

**Phase 3 — Migrate tests.** Collapse `dual_eval_*` to single-backend VM tests (rework the
macros to emit one VM case, keep the literal expected values from the eval-tw work as the
oracle — we lose the TW/VM differential, so literal expectations become the correctness
anchor). Switch `integration_test`/`http`/`server` `eval()` to the VM; fix fallout
(genuinely TW-only behaviors, if any, surface here).

**Phase 4 — Delete the tree-walker.** Remove `eval_value`/trampoline + TW `special_forms`
paths, `--tw`, REPL `use_vm`, `eval_tw`. **This closes CORE-2** — the Rc env↔closure leak
is a tree-walker whole-`Env`-capture artifact and dies with it (the VM captures
per-variable upvalue cells, so the cycle can't form). No GC needed.

**Phase 5 — Docs.** Remove all `--tw`; rewrite/retire `internals/evaluator.md`; reframe
architecture/performance/lisp-comparison/bytecode-vm and CLAUDE.md to a single-evaluator
story.

## Risks / open questions
- **Macro expansion is the make-or-break.** If running `defmacro` bodies on the VM is
  awkward, a minimal standalone macro interpreter may be needed — which is "a small
  tree-walker" by another name. Validate this approach in a Phase-1 spike before committing.
- **Loss of differential testing.** Dual-eval's value was catching TW↔VM divergence.
  Going VM-only, literal expected values (already started for foundational ops) and
  fuzzing become the correctness strategy.
- **Embedding-API break.** `Interpreter::eval` semantics change backend; document as a
  breaking change for embedders; bump major-ish.
- **Behaviors that only ever worked on the TW** (e.g. the CORE-2 returned-mutual-recursion
  corner) will be gone — acceptable, but inventory during Phase 3.

## Recommendation
Do **Phase 1a (macro-expansion spike)** first as a feasibility gate — everything else is
mechanical once the VM can expand macros standalone. Treat Phases 1–5 as separate PRs.
