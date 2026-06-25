---
name: "even?"
module: "math"
section: "Numeric Predicates"
params: [{ name: n, type: int }]
returns: "bool"
---

Test whether an integer is even. Requires an integer — passing a float errors rather than coercing. See `odd?` for the complement.

```sema
(even? 4)      ; => #t
(even? 0)      ; => #t
(even? -2)     ; => #t
(even? 3)      ; => #f
```
