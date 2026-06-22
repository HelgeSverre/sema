---
name: "list/contains?"
module: "lists"
section: "Access & Search"
---

Return `#t` if the sequence contains `elem` (structural equality), else `#f`. Unlike `member` (which returns the Scheme-style tail or `#f`), this reads as a predicate and allocates nothing.

```sema
(list/contains? (list 1 2 3) 2)   ; => #t
(list/contains? (list 1 2 3) 9)   ; => #f
```
