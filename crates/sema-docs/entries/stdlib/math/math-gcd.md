---
name: "math/gcd"
module: "math"
section: "Integer Math"
---

Greatest common divisor of two integers — the largest number that divides both. `(math/gcd n 0)` is `n`. Pairs with `math/lcm` (least common multiple).

```sema
(math/gcd 12 8)    ; => 4
(math/gcd 15 10)   ; => 5
(math/gcd 7 13)    ; => 1   (coprime)
```
