---
name: "mod"
module: "math"
section: "Basic Arithmetic"
params: [{ name: a, type: number }, { name: b, type: number }]
returns: "number"
---

Remainder after truncated division. The result takes the sign of the *dividend* (the left operand), so `mod` behaves like C-style `%`, not a true mathematical floor-modulo.

```sema
(mod 10 3)    ; => 1
(mod 7 2)     ; => 1
(mod -7 2)    ; => -1   ; sign follows the dividend, not the divisor
```

`mod`, [`modulo`](#modulo), and [`math/remainder`](#math-remainder) are the same truncated remainder — there is no separate floor-mod builtin. To force a non-negative result for a positive divisor `n`, normalize with `(mod (+ (mod x n) n) n)`.
