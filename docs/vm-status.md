# Bytecode VM Status

> Last updated: 2026-02-24 (v1.11.0)

## Current State

The bytecode VM (`sema-vm` crate) is opt-in via `--vm` CLI flag. All known bugs are **fixed**. The VM is mature and passes all dual-eval tests — both backends produce identical results for all pure-computation features.

- **sema-vm unit tests:** 261 passing
- **VM integration tests:** 142 passing
- **Dual-eval tests:** 1,057 test cases × 2 backends = 2,114 individual tests
- **Examples:** 54/54 passing via `--vm`
- **Total project tests:** 3,700+ passing, 0 failures

## Architecture

```
Source → Reader → Macro Expand → Lower (CoreExpr) → Optimize → Resolve (slots) → Compile (bytecode) → VM Execute
                  ↑ tree-walker                                                                         ↑
                  └── defmacro evaluated here                                                           |
                      before compilation                                           VM closures: same-VM CallFrame push
                                                                                   NativeFn fallback for stdlib HOF interop
```

**Same-VM execution:** VM closures carry an opaque `payload: Option<Rc<dyn Any>>` on `NativeFn`. The payload holds a `VmClosurePayload` (closure + function table). When `call_value` encounters a payload, it downcasts and pushes a `CallFrame` on the **same VM** — no fresh `VM::new()`. This eliminates native stack growth. True TCO is implemented via `tail_call_vm_closure`, which reuses the current frame, enabling 100K+ depth tail recursion.

**NativeFn fallback:** Closures passed to stdlib higher-order functions (map, filter, etc.) still go through the NativeFn wrapper interface, which creates a short-lived VM. This ensures interop with `sema-stdlib` which depends on `sema-core`, not `sema-vm`.

**Dependency flow:** `sema-core ← sema-reader ← sema-vm ← sema-eval`. The VM crate cannot depend on sema-eval.

**NaN-boxed values:** All values are 8-byte NaN-boxed `u64`. Small ints (±17.5 trillion), symbols, keywords, chars, bools, and nil are unboxed immediates — no heap allocation. The VM benefits from smaller stack slots and better cache locality (8–12% speedup over the pre-NaN-boxing VM).

**Optimizer:** Constant folding pass runs between lowering and resolution — folds arithmetic, comparisons, boolean simplification, if/and/or with constant operands, and dead constant elimination.

## Opcodes

46 opcodes across 7 categories:

- **Stack/constants:** Const, Nil, True, False, Pop, Dup
- **Variables:** LoadLocal(0-3), StoreLocal(0-3), LoadUpvalue, StoreUpvalue, LoadGlobal, StoreGlobal, DefineGlobal
- **Control flow:** Jump, JumpIfFalse, JumpIfTrue, Call, TailCall, Return, Throw
- **Functions:** MakeClosure, CallNative, CallGlobal
- **Arithmetic (generic):** Add, Sub, Mul, Div, Negate, Not, Eq, Lt, Gt, Le, Ge
- **Arithmetic (int fast-path):** AddInt, SubInt, MulInt, LtInt, EqInt — operate directly on NaN-boxed bits, no Clone/Drop
- **Data constructors:** MakeList, MakeVector, MakeMap, MakeHashMap
- **Intrinsic stdlib ops:** Car, Cdr, Cons, IsNull, IsPair, IsList, IsNumber, IsString, IsSymbol, Length, Append, Get, ContainsQ

## Resolved Bugs

All 7 known bugs are fixed:

| Bug                                                     | Problem                                                                                                            | Fix                                                                                             |
| ------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------- |
| **1. Self-ref injection corrupting locals**             | `make_closure` wrote a NativeFn self-reference into a local slot for all named functions, not just named-let loops | Desugar named-let to `letrec` + `lambda` in `lower.rs`, eliminating self-ref injection entirely |
| **2. Missing arity checking**                           | NativeFn wrapper silently filled missing args with Nil and ignored extras                                          | Added strict arity validation in both the NativeFn fallback wrapper and `call_vm_closure`       |
| **3. compile_named_let missing func_id patch/upvalues** | Named-let didn't patch child func_ids or support upvalues                                                          | Same named-let desugaring as Bug 1 — `compile_named_let` is no longer needed                    |
| **4. Fresh VM per closure → stack overflow**            | Each closure call created `VM::new()` + `vm.run()`, exhausting the Rust stack after ~200-500 calls                 | Same-VM execution via opaque payload in NativeFn (see Architecture above)                       |
| **5. Recursive inner define**                           | `(define (f) (define (g) (g)) (g))` failed — resolver resolved lambda body before defining local slot              | Fixed in resolver: allocate local slot before resolving RHS                                     |
| **6. delay/force not capturing lexical vars**           | `(define (f a) (delay a))` failed — delay passed raw AST, tree-walker couldn't see VM locals                       | Fixed: delay now lowers to zero-arg lambda thunk that captures lexical environment              |
| **7. `__vm-import` selective import**                   | Selective names list pushed as single element instead of spreading individual symbols                              | Fixed: spread symbols individually in the reconstructed import form                             |
| **8. `and` optimizer returning `#f` for falsy values**  | `fold_and` in optimizer replaced constant falsy values (nil) with `#f` instead of preserving the original value    | Fixed: return the actual falsy constant, matching tree-walker semantics                         |

## Performance

- **1BRC (10M rows, VM):** ~15.9s — dominated by Rc/drop (~35%), VM dispatch (16.5%), HashMap::clone (5.8%)
- **Compute benchmarks (VM):** TAK 8.04s, deriv 1.84s (post-NaN-boxing)
- **VM vs tree-walker:** ~1.7–2× faster on compute-heavy workloads
- **Janet comparison:** ~1.7× behind Janet on 1BRC (both are embeddable bytecode VMs, no JIT)
- See [Performance Roadmap](performance-roadmap.md) for detailed analysis and optimization plan

## Deferred Work

- **Tracing GC:** Replace Rc-based reference counting — estimated ~1.3× speedup
- **Direct threading:** Computed goto dispatch — estimated 15–30% on tight loops
- **Macro expansion caching:** Cache expanded macros to avoid redundant tree-walker evaluation
- **Register-based VM:** Would reduce push/pop traffic but requires full rewrite
