---
name: "list/nth-or"
module: "lists"
section: "Access & Search"
---

Return the element at `index`, or `default` when the index is out of bounds — the safe counterpart to `nth` (which errors).

```sema
(list/nth-or (list 10 20 30) 1 :none)   ; => 20
(list/nth-or (list 10 20 30) 9 :none)   ; => :none
```
