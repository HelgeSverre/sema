---
name: "for/fold"
module: "special-forms"
syntax: "(for/fold ((acc init) ...) ((var sequence) ...) body)"
---

`for/fold` threads an accumulator through a sequence. On the first iteration each `acc` is bound to its `init` value; on each subsequent iteration `acc` is updated to the result of `body`. When the sequence is exhausted, the final accumulator value (or values, when multiple accumulators are provided) is returned.

**Not built in.** `for/fold` is *not* a core special form or prelude macro — it must be defined or imported before use. For the common single-accumulator case, the builtin `foldl` does the same job and is always available (its callback takes `(acc elem)`):

```sema
(foldl + 0 (range 5))
;; => 10
```

```sema
(foldl (fn (acc x) (cons x acc)) '() '(1 2 3 4))
;; => (4 3 2 1)
```

A `for/fold` macro adds two conveniences over `foldl`: a named accumulator binding, and multiple accumulators threaded in one pass (each step returns `(values ...)`):

```sema
(for/fold ((total 0) (count 0))
  ((x '(1 2 3 4 5)))
  (values (+ total x) (+ count 1)))
;; total => 15, count => 5   ; once for/fold is defined
```
