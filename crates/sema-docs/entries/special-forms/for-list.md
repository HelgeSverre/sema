---
name: "for/list"
module: "special-forms"
syntax: "(for/list ((var expr) ...) body)"
---

List comprehension: evaluate the body for each combination of elements across the given sequences and collect the results into a list. Each binding pairs a variable with a sequence expression. Multiple bindings produce nested iteration (cartesian product), with the rightmost binding changing fastest. The body should produce a single value for each iteration.

**Not built in.** `for/list` is *not* a core special form or prelude macro — it must be defined or imported before use (see the Note below). For a single sequence, the builtin `map` is equivalent: `(map (fn (x) (* x x)) (range 5))`. The examples below assume a `for/list` macro is in scope.

`for/list` supports `:when` filter clauses interleaved with bindings. A `:when` clause evaluates a predicate expression; if it is falsy, that iteration is skipped and no value is collected. This allows concise filtered comprehensions without separate `filter` calls.

```sema
(for/list ((x (range 5)))
  (* x x))
;; => (0 1 4 9 16)
```

```sema
(for/list ((x (range 1 21))
           (:when (even? x)))
  (* x x))
;; => (4 16 36 64 100 144 196 256 324 400)
```

```sema
(for/list ((x (list 1 2 3))
           (y (list 10 20)))
  (+ x y))
;; => (11 21 12 22 13 23)   ; y (rightmost) varies fastest
```

**Note:** `for/list` is typically implemented as a macro that expands to nested `map` and `append` calls. It is not a core special form and may need to be defined or imported from a comprehension library.