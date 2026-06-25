---
name: "math/lcm"
module: "math"
section: "Integer Math"
---

Least common multiple of two integers — the smallest number both divide evenly. For coprime inputs it is just their product. Pairs with `math/gcd`.

```sema
(math/lcm 4 6)     ; => 12
(math/lcm 3 5)     ; => 15   (coprime: 3 * 5)
```
