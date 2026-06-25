---
name: "pow"
module: "math"
section: "Numeric Utilities"
---

Raise a number to a power. Two non-negative integers give an integer result; a fractional or negative exponent (or float operands) yields a float. Same as [`math/pow`](#math-pow); `expt` is a Scheme-style alias.

```sema
(pow 2 10)    ; => 1024
(pow 3 3)     ; => 27
(pow 2 -1)    ; => 0.5   ; negative exponent -> float
(pow 9 0.5)   ; => 3.0   ; fractional exponent -> square root
```
