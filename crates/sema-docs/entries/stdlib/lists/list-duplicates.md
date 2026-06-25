---
name: "list/duplicates"
module: "lists"
section: "Set Operations"
---

Return the distinct values that appear more than once in a list. Each repeated value is listed only once, no matter how many times it occurs.

The inverse view is `list/unique`, which drops the repeats and keeps one of each value.

```sema
(list/duplicates '(1 2 2 3 3 3 4))   ; => (2 3)
(list/duplicates '(1 2 3))           ; => ()
```
