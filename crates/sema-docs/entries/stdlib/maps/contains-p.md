---
name: "contains?"
module: "maps"
section: "Maps"
---

Test whether a map contains a key. Use this rather than `(get m k)` when a stored value might itself be `nil` — `get` returning `nil` can't distinguish "key absent" from "key present with value nil", but `contains?` can. Works on both maps and hashmaps.

```sema
(contains? {:a 1} :a)              ; => #t
(contains? {:a 1} :b)              ; => #f
(contains? {:a nil} :a)            ; => #t   (key present even though value is nil)
(contains? (hashmap/new :a 1) :a)  ; => #t
```
