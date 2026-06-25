---
name: "int"
module: "math"
params: [{ name: x, type: "number | string" }]
returns: "int"
---

Convert a number or numeric string to an integer. Floats are truncated **toward zero** (the fractional part is dropped, not rounded). Signals an error if a string cannot be parsed as an integer.

Truncation differs from `floor` on negatives: `int` chops toward zero while `floor` always rounds down. `(int -3.9)` is `-3`, but `(floor -3.9)` is `-4`. Use `int` for "drop the decimals", `floor`/`ceil` when you need a specific rounding direction.

```sema
(int 3.9)     ; => 3
(int -3.9)    ; => -3   (toward zero, not -4)
(int "42")    ; => 42
(floor -3.9)  ; => -4   (contrast: rounds down)
```
