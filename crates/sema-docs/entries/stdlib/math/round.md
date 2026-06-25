---
name: "round"
module: "math"
section: "Numeric Utilities"
---

Round to the nearest integer, with ties (`.5`) rounded *away from zero*. For decimal-place rounding use [`math/round-to`](#math-round-to); to drop the fraction toward zero use [`truncate`](#truncate).

```sema
(round 3.5)   ; => 4
(round 3.4)   ; => 3
(round 2.5)   ; => 3    ; tie rounds away from zero (not to even)
(round -2.5)  ; => -3
```
