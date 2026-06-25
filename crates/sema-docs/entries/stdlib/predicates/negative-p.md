---
name: "negative?"
module: "predicates"
section: "Numeric Predicates"
---

Test if a number is strictly less than zero. Zero is neither negative nor positive.

```sema
(negative? -1)    ; => #t
(negative? 0)     ; => #f
(negative? 1)     ; => #f
(negative? -0.5)  ; => #t
```
