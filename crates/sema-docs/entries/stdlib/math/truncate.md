---
name: "truncate"
module: "math"
section: "Scheme Aliases"
---

Drop the fractional part, rounding toward zero (so negatives round *up*). Unlike [`round`](#round), it never inspects the fraction — `3.9` truncates to `3`.

```sema
(truncate 3.7)  ; => 3
(truncate -3.7) ; => -3   ; toward zero, not floor (-4)
(truncate 3.9)  ; => 3
```
