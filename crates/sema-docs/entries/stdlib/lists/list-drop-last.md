---
name: "list/drop-last"
module: "lists"
section: "Slicing"
---

Return all but the last `n` elements (drops from the tail; the counterpart to `drop`). Clamps to empty.

```sema
(list/drop-last 2 (list 1 2 3 4))   ; => (1 2)
(list/drop-last 9 (list 1 2))       ; => ()
```
