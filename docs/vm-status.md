# Bytecode VM Status

> Last updated: 2026-02-16

## Current State

The bytecode VM (`sema-vm` crate) is opt-in via `--vm` CLI flag. All known bugs are **fixed**. The VM passes **173 unit tests** and **130 integration tests**. **44/44 examples** pass through the `--vm` path.

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

| Bug                                                     | Problem                                                                                                            | Fix                                                                                             |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------- |
| **1. Self-ref injection corrupting locals**             | `make_closure` wrote a NativeFn self-reference into a local slot for all named functions, not just named-let loops | Desugar named-let to `letrec` + `lambda` in `lower.rs`, eliminating self-ref injection entirely |
| **2. Missing arity checking**                           | NativeFn wrapper silently filled missing args with Nil and ignored extras                                          | Added strict arity validation in both the NativeFn fallback wrapper and `call_vm_closure`       |
| **3. compile_named_let missing func_id patch/upvalues** | Named-let didn't patch child func_ids or support upvalues                                                          | Same named-let desugaring as Bug 1 — `compile_named_let` is no longer needed                    |
| **4. Fresh VM per closure → stack overflow**            | Each closure call created `VM::new()` + `vm.run()`, exhausting the Rust stack after ~200-500 calls                 | Same-VM execution via opaque payload in NativeFn (see Architecture above)                       |
| **5. Recursive inner define**                           | `(define (f) (define (g) (g)) (g))` failed — resolver resolved lambda body before defining local slot              | Fixed in resolver: allocate local slot before resolving RHS                                     |
| **6. delay/force not capturing lexical vars**           | `(define (f a) (delay a))` failed — delay passed raw AST, tree-walker couldn't see VM locals                       | Fixed: delay now lowers to zero-arg lambda thunk that captures lexical environment              |
| **7. `__vm-import` selective import**                   | Selective names list pushed as single element instead of spreading individual symbols                              | Fixed: spread symbols individually in the reconstructed import form                             |

## Test Coverage

- **sema-vm unit tests:** 173 passing, 0 failing
- **VM integration tests:** 130 passing, 0 failing, 0 ignored
- **Examples:** 44/44 passing via `--vm`
- **CI:** GitHub Actions with `cargo-llvm-cov` → Codecov
- **Local:** `make coverage` for lcov, `make coverage-html` for HTML report

## Deferred Work

- **NaN boxing / tagged values:** Performance optimization for value representation
- **Inline caching:** Optimization for repeated global lookups
- **True GC:** Replace Rc-based reference counting with a tracing garbage collector
- **Macro expansion caching:** Cache expanded macros to avoid redundant tree-walker evaluation
