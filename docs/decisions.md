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

## Mini-Evaluator Synchronization

- `sema_eval_value` in `sema-stdlib/src/list.rs` (~620 lines) is a fast-path evaluator used by `file/fold-lines` and higher-order functions.
- It duplicates a subset of the main evaluator in `sema-eval/src/eval.rs` to avoid the overhead of the full trampoline-based evaluation loop.
- This provides ~4x speedup on hot paths but must be manually kept in sync when new special forms or evaluation semantics are added.
- This is a known maintenance concern — divergence between the two evaluators can cause subtle bugs.
- **Future options:**
  - Code generation or a shared trait to reduce drift risk
  - A bytecode VM would eliminate the need for two evaluators entirely, since the hot loop would be bytecode dispatch rather than AST walking

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
