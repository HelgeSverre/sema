---
name: "bit/shift-left"
module: "math"
section: "Bitwise Operations"
---

Left bit shift: `(bit/shift-left n k)` shifts `n`'s bits left by `k`, which is the same as multiplying by `2^k`. Handy for building bit flags or fast power-of-two scaling.

```sema
(bit/shift-left 1 4)   ; => 16   (1 * 2^4)
(bit/shift-left 3 2)   ; => 12   (3 * 2^2)
```
