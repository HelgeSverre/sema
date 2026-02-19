# VM Bug: Exception handling across call frames

## Status: FIXED

## Summary

`try/catch` in the VM failed to correctly catch exceptions thrown from VM closure calls, especially when the `try` expression was nested inside another expression (e.g., as an argument to a function call).

## Reproduction

```scheme
;; Works (inline throw):
(try (throw "boom") (catch e "caught"))  ;; → "caught" ✓

;; FAILED (throw from called closure):
(define (thrower) (throw "boom"))
(try (thrower) (catch e "caught"))  ;; → uncaught exception

;; FAILED (try as argument — stack corruption):
(println (try (thrower) (catch e "caught")))  ;; → "not callable" error
```

## Root Cause (two bugs)

### Bug 1: Off-by-one in parent frame PC lookup

When a throw propagates from a callee frame to a parent frame, `handle_exception` uses `parent.pc` to search the exception table. But `parent.pc` is the **resume PC** (byte after the CALL instruction), not the CALL instruction itself. Exception table intervals are half-open `[try_start, try_end)`, so when the CALL is the last instruction in the try body, `parent.pc == try_end` and the `< try_end` check fails.

**Fix:** `pc_for_lookup = parent.pc.saturating_sub(1) as u32` — moves lookup PC back inside the CALL instruction.

### Bug 2: Stack depth in exception entries ignores operand stack

The compiler set `ExceptionEntry.stack_depth` to `self.n_locals` — the number of local variable slots. But this didn't account for **operand stack temporaries** pushed before the try body (e.g., a function value pushed for a surrounding call).

**Fix:** Added `stack_height: u16` tracking to `Compiler`. Incremented in `compile_call` when pushing function/argument values, decremented after CALL. `compile_try` now sets `stack_depth: self.n_locals + self.stack_height`.

## Files changed

- `crates/sema-vm/src/vm.rs` — `handle_exception`: `saturating_sub(1)` fix + `#[cold]` annotation + 6 regression tests
- `crates/sema-vm/src/compiler.rs` — Added `stack_height` tracking, updated `compile_call` and `compile_try`

## Regression tests

```scheme
(try (thrower) (catch e "caught"))                    ;; throw from closure
(try ((fn () (throw 42))) (catch e (:value e)))       ;; throw from lambda
(try (outer) (catch e "caught"))                      ;; throw 2 calls deep
(try (begin (thrower) 123) (catch e "caught"))        ;; call not last in try
(+ 1 (try (thrower) (catch e 2)))                     ;; try as arithmetic arg
(list 1 2 (try (thrower) (catch e 3)) 4)              ;; try in list constructor
```
