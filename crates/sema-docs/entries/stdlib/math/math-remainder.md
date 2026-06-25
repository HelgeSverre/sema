---
name: "math/remainder"
module: "math"
section: "Integer Math"
params: [{ name: a, type: int }, { name: b, type: int }]
returns: "int"
---

Remainder after truncated division: `(- a (* (math/quotient a b) b))`. The sign follows the dividend `a`. Identical to [`mod`](#mod); pairs with [`math/quotient`](#math-quotient).

```sema
(math/remainder 10 3) ; => 1
(math/remainder 7 2)  ; => 1
(math/remainder -7 3) ; => -1   ; sign follows the dividend
```
