---
name: "expt"
module: "math"
section: "Scheme Aliases"
params: [{ name: base, type: number }, { name: exp, type: number }]
returns: "number"
---

Raise a base to a power, `(expt base exponent)`. The Scheme name for `pow`. Integer base and exponent give an integer result; a fractional exponent gives a float (so `(expt x 0.5)` is a square root) and a negative exponent gives the reciprocal.

```sema
(expt 2 10)    ; => 1024
(expt 9 0.5)   ; => 3.0     (square root)
(expt 2 -1)    ; => 0.5     (reciprocal)
```
