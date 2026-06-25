---
name: "assoc"
module: "lists"
section: "Association Lists"
params: [{ name: key, type: any }, { name: alist, type: list }]
returns: "any"
---

Look up a key in an association list (a list of `(key value)` pairs), using `equal?` (structural) comparison. Returns the **whole matching pair**, not just the value — so reach for the value with `cadr`. Returns `#f` when no key matches.

```sema
(define alist '(("a" 1) ("b" 2) ("c" 3)))
(assoc "b" alist)         ; => ("b" 2)
(assoc "z" alist)         ; => #f

;; Get the value, not the pair:
(cadr (assoc "b" alist))  ; => 2
```

Because `#f` doubles as "not found", check membership before destructuring an unknown key. For larger or mutable lookups, a hash map (`{...}` / `get`) is faster than scanning an alist. See also `assq`/`assv` (other comparison flavors — in Sema all three compare by value).

