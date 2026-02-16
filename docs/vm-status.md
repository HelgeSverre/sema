# Bytecode VM Status

> Last updated: 2026-02-16

## Current State

The bytecode VM (`sema-vm` crate) is opt-in via `--vm` CLI flag. All 4 original root-cause bugs are **fixed**. The VM passes **173 unit tests** and **122 integration tests** (3 ignored). **39/44 examples** pass through the `--vm` path; 5 fail due to 3 remaining issues.

## Architecture

```
Source → Reader → Macro Expand → Lower (CoreExpr) → Resolve (slots) → Compile (bytecode) → VM Execute
                  ↑ tree-walker                                                              ↑
                  └── defmacro evaluated here                                                |
                      before compilation                                        VM closures: same-VM CallFrame push
                                                                                NativeFn fallback for stdlib HOF interop
```

**Same-VM execution:** VM closures carry an opaque `payload: Option<Rc<dyn Any>>` on `NativeFn`. The payload holds a `VmClosurePayload` (closure + function table). When `call_value` encounters a payload, it downcasts and pushes a `CallFrame` on the **same VM** — no fresh `VM::new()`. This eliminates native stack growth. True TCO is implemented via `tail_call_vm_closure`, which reuses the current frame, enabling 100K+ depth tail recursion.

**NativeFn fallback:** Closures passed to stdlib higher-order functions (map, filter, etc.) still go through the NativeFn wrapper interface, which creates a short-lived VM. This ensures interop with `sema-stdlib` which depends on `sema-core`, not `sema-vm`.

**Dependency flow:** `sema-core ← sema-reader ← sema-vm ← sema-eval`. The VM crate cannot depend on sema-eval.

## Resolved Bugs

All 4 original bugs are fixed:

| Bug | Problem | Fix |
|-----|---------|-----|
| **1. Self-ref injection corrupting locals** | `make_closure` wrote a NativeFn self-reference into a local slot for all named functions, not just named-let loops | Desugar named-let to `letrec` + `lambda` in `lower.rs`, eliminating self-ref injection entirely |
| **2. Missing arity checking** | NativeFn wrapper silently filled missing args with Nil and ignored extras | Added strict arity validation in both the NativeFn fallback wrapper and `call_vm_closure` |
| **3. compile_named_let missing func_id patch/upvalues** | Named-let didn't patch child func_ids or support upvalues | Same named-let desugaring as Bug 1 — `compile_named_let` is no longer needed |
| **4. Fresh VM per closure → stack overflow** | Each closure call created `VM::new()` + `vm.run()`, exhausting the Rust stack after ~200-500 calls | Same-VM execution via opaque payload in NativeFn (see Architecture above) |

## Remaining Issues

Three root causes account for the 5 failing examples and 3 ignored tests:

### 1. Recursive inner define (2 examples: emoji.sema, lazy.sema)

```scheme
(define (outer x)
  (define (inner y) ..recursive call to inner..)
  (inner x))
```

Internal `define` of a lambda that recurses on itself doesn't work. The resolver resolves the lambda body before defining the local slot for the function name, so the recursive reference is unresolved. The tree-walker handles this via self-reference injection at apply time.

**Fix:** Desugar internal `define` of lambdas to `letrec` in the lowering pass.

### 2. delay/force not capturing lexical variables (1 example: scheme-basics.sema; 1 ignored test: `test_complex_lazy_stream_take`)

```scheme
(define (f a b) (delay (+ a b)))
```

The `delay` lowering passes the body as `CoreExpr::Const(body_val)` (raw unevaluated AST), which goes through tree-walker eval on `force`. The tree-walker doesn't have access to VM locals.

**Fix:** Lower `delay` to a zero-arg `Lambda` (thunk) instead of passing raw AST. `force` then calls the thunk.

### 3. Module import (1 example: modules-demo.sema; 2 ignored tests: `test_example_pattern_modules_import`, `test_example_pattern_modules_selective_import`)

Selective import with `:only` syntax fails with `import: selective names must be symbols`. Module import has filesystem/interop limitations in the VM path.

### 4. Timeout (1 example: meta-eval-stress.sema)

Meta-evaluation stress test runs the interpreted meta-evaluator, which is inherently slow. Not a VM bug.

## Fix Priority

| Priority | Fix | Effort | Impact |
|----------|-----|--------|--------|
| 1 | Desugar recursive inner define to letrec | S (1-2h) | Fixes 2 failing examples |
| 2 | Lower delay to thunk lambda | S (1-2h) | Fixes 1 example + 1 ignored test |
| 3 | Module import in VM path | M (half day) | Fixes 1 example + 2 ignored tests |

## Test Coverage

- **sema-vm unit tests:** 173 passing, 0 failing
- **VM integration tests:** 122 passing, 0 failing, 3 ignored
- **Examples:** 39/44 passing via `--vm`
- **CI:** GitHub Actions with `cargo-llvm-cov` → Codecov
- **Local:** `make coverage` for lcov, `make coverage-html` for HTML report

## Deferred Work

- **NaN boxing / tagged values:** Performance optimization for value representation
- **Inline caching:** Optimization for repeated global lookups
- **True GC:** Replace Rc-based reference counting with a tracing garbage collector
- **Macro expansion caching:** Cache expanded macros to avoid redundant tree-walker evaluation
