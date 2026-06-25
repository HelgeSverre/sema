---
name: "math/quotient"
module: "math"
section: "Integer Math"
---

Integer quotient: divide and truncate toward zero. Unlike [`/`](#op-2), it never yields a fraction. Pairs with [`math/remainder`](#math-remainder) so that `(+ (* (math/quotient a b) b) (math/remainder a b))` reconstructs `a`.

```sema
(math/quotient 10 3)  ; => 3
(math/quotient 7 2)   ; => 3
(math/quotient -7 2)  ; => -3   ; truncates toward zero (not floored to -4)
```
