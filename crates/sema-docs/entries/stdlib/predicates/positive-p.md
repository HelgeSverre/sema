---
name: "positive?"
module: "predicates"
section: "Numeric Predicates"
---

Test if a number is strictly greater than zero. Zero is neither positive nor negative.

```sema
(positive? 1)     ; => #t
(positive? 0)     ; => #f
(positive? -1)    ; => #f
(positive? 0.5)   ; => #t
```
