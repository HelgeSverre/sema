---
name: "hashmap/keys"
module: "maps"
section: "HashMaps"
---

Return the keys of a hashmap as a list. Hashmaps are unordered, so the order is unspecified — sort it yourself, or convert with `hashmap/to-map` / `map/sort-keys` if you need a stable order.

```sema
(hashmap/keys (hashmap/new :a 1 :b 2))   ; => (:a :b)
```
