---
name: "every"
module: "lists"
section: "Searching"
---

Return `#t` if `pred` is truthy for every element (and `#t` for the empty list, vacuously); `#f` otherwise. Stops at the first failure. Alias: `every?`.

```sema
(every even? '(2 4 6))     ; => #t
(every even? '(2 3 6))     ; => #f
(every even? '())          ; => #t
```

The mirror of `any` (at least one element must pass). Watch the empty-list base cases: `every` is `#t` (nothing to violate), `any` is `#f`.

