---
name: "get"
module: "maps"
section: "Maps"
---

Look up a value by key. Works on both maps and hashmaps. Returns `nil` for a missing key, or a third *default* argument if you supply one. For nested lookups use `get-in`.

A keyword in head position is shorthand for `get`: `(:a m)` is the same as `(get m :a)`.

```sema
(get {:a 1 :b 2} :a)          ; => 1
(get {:a 1 :b 2} :z)          ; => nil
(get {:a 1} :z "missing")     ; => "missing"  (default for absent key)
(:a {:a 1 :b 2})              ; => 1           (keyword-as-function shorthand)
```
