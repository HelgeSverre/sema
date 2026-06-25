---
name: "for"
module: "special-forms"
syntax: "(for-each fn seq) | (for-range (var start end [step]) body ...)"
---

Sema has **no `for` special form**. To iterate for side effects, use `for-each` (apply a function to each element of a sequence) or `for-range` (counted integer loop). Both return `nil`; neither leaks its loop variable into the enclosing scope. Writing a bare `(for ...)` raises `Unbound variable: for` — it is not a builtin.

Use `for-each` to walk a collection, and `for-range` when you need a numeric counter. To accumulate a result functionally instead of mutating, reach for `map`, `foldl`, or `filter` rather than a loop.

```sema
(for-each println (list 1 2 3))
;; prints 1, 2, 3 on separate lines, returns nil
```

```sema
(for-range (i 0 5)
  (println i))
;; prints 0 1 2 3 4
```

Accumulate into a mutable cell with an explicit `set!`:

```sema
(let ((total 0))
  (for-each (lambda (n) (set! total (+ total n)))
            (list 10 20 30))
  total)
;; => 60
```

Nested iteration (cartesian product) is just nested `for-each`:

```sema
(for-each
  (lambda (x)
    (for-each (lambda (y) (println (list x y)))
              (list 10 20)))
  (list 1 2))
;; prints (1 10) (1 20) (2 10) (2 20)
```

**See also:** `for-range` (counted loop), `for-each` (sequence iteration), `map`/`foldl` (build a value instead of looping for effect), `while` (condition-driven loop), `doseq` does not exist either.
