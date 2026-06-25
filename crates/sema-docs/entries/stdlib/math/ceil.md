---
name: "ceil"
module: "math"
section: "Numeric Utilities"
---

Round up toward positive infinity. For negatives this rounds *toward* zero (`-2.7` → `-2`). The mirror of `floor`; alias `ceiling`.

```sema
(ceil 3.2)    ; => 4
(ceil -2.7)   ; => -2
(ceil 5)      ; => 5    (already an integer, unchanged)
```
