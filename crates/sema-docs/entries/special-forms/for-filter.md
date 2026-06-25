---
name: "for/filter"
module: "special-forms"
syntax: "(for/filter ((var sequence) ...) body)"
---

`for/filter` is a list-comprehension form that iterates over a sequence, binds each element to `var`, evaluates `body` as a predicate, and returns a list of the elements for which the predicate is truthy, preserving order.

**Not built in.** `for/filter` is *not* a core special form or prelude macro — it must be defined or imported from a comprehension library before use. For everyday filtering, reach for the builtin `filter`, which is always available:

```sema
(filter even? (range 10))
;; => (0 2 4 6 8)
```

```sema
(define (positive? n) (> n 0))
(filter positive? '(-3 -1 0 2 4))
;; => (2 4)
```

When you do supply a `for/filter` macro, the comprehension reads as:

```sema
(for/filter ((x (range 10)))
  (even? x))
;; => (0 2 4 6 8)   ; once for/filter is defined
```
