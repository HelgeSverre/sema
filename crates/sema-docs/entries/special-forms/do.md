---
name: "do"
module: "special-forms"
syntax: "(do ((var init [step]) ...) (test [result ...]) body ...)"
---

Scheme-style iterative loop with explicit variable bindings, step expressions, and a termination test. Each binding specifies a variable name, an initial value, and an optional step expression. On each iteration, all variables are updated in parallel using their step expressions (or retain their current value if no step is given). The loop terminates when the test expression evaluates to truthy, at which point the result expressions are evaluated and the last result is returned. If no result expressions are provided, the loop returns `nil`.

The body expressions, if any, are evaluated on every iteration before the step update and are typically used for side effects. `do` is useful for numeric iteration, accumulation, and any loop that requires parallel variable updates. For sequential binding semantics, use `let*` inside the loop body instead.

```sema
(do ((i 0 (+ i 1))
     (sum 0 (+ sum i)))
    ((= i 10) sum))
;; => 45
```

```sema
(do ((i 0 (+ i 1)))
    ((= i 5))
  (println i))
;; prints 0 through 4, returns nil
```

```sema
(do ((n 5 (- n 1)))
    ((= n 0) "liftoff")
  (println n))
;; prints 5, 4, 3, 2, 1 — then returns "liftoff"
```

**Gotcha:** the step expression decides termination, so make sure the test will actually become true. `(/ n 2)` produces floats (`2.5`, `1.25`, …) that never equal `0`, so `((= n 0) ...)` would loop forever — decrement with `(- n 1)` or test with `(< n 1)`.

**Note:** The VM compiles `do` to a dedicated `DoLoop` IR node with parallel step assignment.