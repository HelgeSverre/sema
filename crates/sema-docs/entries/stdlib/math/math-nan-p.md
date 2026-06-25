---
name: "math/nan?"
module: "math"
section: "Numeric Predicates"
---

Test whether a value is NaN ("not a number"). This is the *only* reliable NaN check: NaN is never equal to anything, including itself, so `(= x math/nan)` is always `#f`. NaN arises from undefined operations like `(/ 0.0 0.0)` or `(sqrt -1)`.

```sema
(math/nan? math/nan)        ; => #t
(math/nan? 42)              ; => #f
(math/nan? (sqrt -1))       ; => #t
(= math/nan math/nan)       ; => #f   ; why you need this predicate
```
