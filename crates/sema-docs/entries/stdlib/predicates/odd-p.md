---
name: "odd?"
module: "predicates"
section: "Numeric Predicates"
---

Test if an integer is odd. Requires an integer — passing a float raises a type error.

```sema
(odd? 3)    ; => #t
(odd? 4)    ; => #f
(odd? -3)   ; => #t   (sign doesn't matter)
(odd? 0)    ; => #f
```
