# Architecture Decisions and Future Plans

This document records key architectural decisions in Sema and planned future work.

## Naming Conventions

The stdlib uses four naming conventions, reflecting Sema's evolution from Scheme roots toward its own identity:

### `module/function` (slash-namespaced) — preferred

The modern, preferred style for new functions. Groups functions by module for discoverability.

```scheme
(string/trim "  hello  ")
(list/map inc '(1 2 3))
(map/keys my-map)
(file/read "data.txt")
```

All new stdlib additions should use this convention.

### `function-name` (dash-separated) — legacy Scheme/R7RS compatibility

Kept as aliases for compatibility with Scheme code and familiarity for users coming from R7RS.

```scheme
(string-append "a" "b")
(string-length "hello")
(string-ref "hello" 0)
```

These are not deprecated but the slash-namespaced variants should be preferred in new code and documentation.

### `type->type` (arrow) — type conversions

Standard Lisp/Scheme convention for conversion functions.

```scheme
(string->number "42")
(char->integer #\A)
(number->string 3.14)
```

### `predicate?` (question mark) — predicates

Standard Scheme convention for boolean-returning functions.

```scheme
(string? x)
(null? lst)
(even? 4)
(string/contains? "hello" "ell")
```

### No-namespace builtins

A few ubiquitous primitives are kept without a namespace prefix for Scheme familiarity and brevity.

```scheme
(substring "hello" 1 3)
(format "~a is ~a" "Sema" "great")
(str 42)
```

## Rc Cycles and Memory Management

- Sema uses `Rc` (not `Arc`) throughout, since the interpreter is single-threaded. This avoids atomic reference counting overhead.
- Recursive `define` creates Rc cycles by design: a lambda captures its environment, which contains a binding to the lambda itself. This is necessary for recursive closures to work.
- No cycle collector exists. Recursive closures leak memory — the Rc cycle is never broken.
- This is acceptable for the current use case: scripting and short-lived interpreter sessions where the OS reclaims memory on exit.
- **Future work:**
  - Investigate weak self-references for named lambdas (the lambda's own binding in its captured env could be a `Weak<RefCell<...>>`)
  - Alternatively, a simple mark-and-sweep pass over the global environment on interpreter drop could break known cycles

## Sandbox / Permission System

- The `--sandbox` CLI flag restricts dangerous native functions at runtime via a capability bitset (`Caps` type in `sema-core`).
- Eight capability groups: `fs-read`, `fs-write`, `shell`, `network`, `env-read`, `env-write`, `process`, `llm`.
- Sandboxed functions remain registered (discoverable, tab-completable) but return a `PermissionDenied` error when invoked.
- Implementation: `register_fn_gated()` wraps closures with a `Sandbox::check()` guard at registration time. When the sandbox is unrestricted (default), zero overhead — functions are registered directly.
- The WASM playground (`sema.run`) uses compile-time feature flags (`#[cfg(not(target_arch = "wasm32"))]`) to shim out dangerous APIs entirely — this is complementary to the runtime sandbox.
- Embedders can use `InterpreterBuilder::with_sandbox(Sandbox::deny(...))` for fine-grained control.
- Presets: `--sandbox=strict` (deny shell, fs-write, network, env-write, process, llm) and `--sandbox=all` (deny everything).
- **Not a process sandbox** — this is an in-language permission check. It prevents stdlib natives from doing I/O but does not provide OS-level isolation.

## Evaluator Callback Architecture (Mini-Eval Removal)

- The 620-line mini-evaluator (`sema_eval_value` + `call_function`) that previously lived in `sema-stdlib/src/list.rs` has been **deleted**.
- It existed because `sema-stdlib` cannot depend on `sema-eval` (circular dependency). It was a hand-optimized fast-path evaluator that provided ~4× speedup on hot loops by skipping the trampoline, call stack, and span tracking.
- It was replaced with a **callback architecture**: `sema-core` provides thread-local `eval_callback` and `call_callback` functions, registered by `sema-eval` during interpreter initialization. All stdlib functions now call through the real evaluator.
- **Trade-off:** 1BRC benchmark regressed from ~960ms to ~3050ms (3.2×) on 1M rows. This is acceptable for correctness — the mini-eval diverged from the real evaluator (no `try/catch`, `do`, macros, modules) and was a maintenance blocker for the bytecode VM transition.
- **Fast-path optimizations** recovered ~14% of the regression (3050ms → ~2630ms on 1M rows):
  1. Thread-local shared `EvalContext` (`with_stdlib_ctx`) eliminates per-call allocation of 6 RefCells in `call_function` and IO streaming functions.
  2. Inline `NativeFn` dispatch in `call_function` skips the `call_callback` thread-local indirection for native function calls.
  3. Self-evaluating fast path in `eval_value` short-circuits Int, Float, String, Bool, Nil, Symbol, Keyword, and other self-evaluating forms before depth/step tracking.
  4. Deferred cloning in `eval_value_inner` avoids `Value::clone()` and `Env::clone()` on the first trampoline iteration (the common non-TCO case).
- **Remaining gap** (~2630ms vs ~960ms original mini-eval) is dominated by the tree-walker's fundamental per-expression overhead: Env chain lookups, Rc refcounting, call-stack management, and trampoline dispatch. This cannot be closed without a bytecode VM.

## Package System

- Currently no package manager. `import` resolves local files only (relative to the importing file).
- No central registry is planned — the ecosystem is not large enough to justify the infrastructure.
- **Future: GitHub-based imports**
  - `github:username/repo` style imports that auto-download to `~/.sema/packages/`
  - Could also support URL imports: `(import "https://raw.githubusercontent.com/...")`  for single-file dependencies
  - Lock file (`sema.lock`) for reproducible builds, recording exact commit SHAs

## LSP Server

- Editor support currently consists of syntax highlighting only, with grammars for VS Code, Vim, Emacs, and Helix (see `editors/` directory).
- **Future: `sema-lsp` crate** using the `tower-lsp` crate
- Features to implement, in priority order:
  1. **Diagnostics** — surface parse errors from `sema-reader` in real-time
  2. **Go-to-definition** — resolve symbols through the module system
  3. **Completion** — stdlib function names, imported symbols, local bindings
  4. **Hover docs** — show function signatures and docstrings
- Can reuse `sema-reader` for parsing and `sema-eval` for limited type inference.
- The existing editor highlighting grammars in `editors/` provide a foundation to build on.
