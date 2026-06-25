---
name: "even?"
module: "predicates"
section: "Numeric Predicates"
---

Test if an integer is even. Requires an integer — passing a float raises a type error.

```sema
(even? 4)    ; => #t
(even? 3)    ; => #f
(even? -2)   ; => #t   (sign doesn't matter)
(even? 0)    ; => #t
```
