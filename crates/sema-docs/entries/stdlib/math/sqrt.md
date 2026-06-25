---
name: "sqrt"
module: "math"
section: "Numeric Utilities"
---

Square root. Always returns a float (even for perfect squares). A negative input yields `NaN` rather than raising.

```sema
(sqrt 16)     ; => 4.0
(sqrt 2)      ; => 1.4142135623730951
(sqrt -1)     ; => NaN   ; no complex numbers; guard with (math/nan? ...)
```
