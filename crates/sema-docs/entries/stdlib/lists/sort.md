---
name: "sort"
module: "lists"
section: "Higher-Order Functions"
params: [{ name: lst, type: list }, { name: cmp, type: function, doc: "optional comparator" }]
returns: "list"
---

Return a new list sorted in ascending order. The input is left unchanged. Pass an optional two-argument comparator to control the order (e.g. `>` for descending).

```sema
(sort '(3 1 4 1 5))                ; => (1 1 3 4 5)
(sort '(3 1 4 1 5) >)              ; => (5 4 3 1 1)   ; custom comparator
(sort '("banana" "apple" "cherry")) ; => ("apple" "banana" "cherry")
```

See also: `sort-by` (sort by a derived key rather than a comparator).
