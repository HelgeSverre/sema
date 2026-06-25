---
name: "floor"
module: "math"
section: "Numeric Utilities"
params: [{ name: n, type: number }]
returns: "int"
---

Round down toward negative infinity. For negatives this rounds *away* from zero (`-2.3` → `-3`), unlike `int`, which truncates toward zero (`-2.3` → `-2`).

```sema
(floor 3.7)   ; => 3
(floor -2.3)  ; => -3   (down, not -2)
(int -2.3)    ; => -2   (contrast: toward zero)
```
