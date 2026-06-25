---
name: "assoc"
module: "maps"
section: "Maps"
syntax: "(assoc map key val ...)"
returns: "map"
---

Add or update key-value pairs, returning a **new** map; the original is never mutated.

Accepts any number of trailing key/value pairs, applied left to right. `assoc` only touches the top level — to reach into a nested map use `assoc-in`/`map/assoc-in`. To remove a key instead, use `dissoc`.

```sema
(assoc {:a 1} :b 2)          ; => {:a 1 :b 2}
(assoc {:a 1} :a 99)         ; => {:a 99}  (existing key replaced)
(assoc {} :a 1 :b 2 :c 3)    ; => {:a 1 :b 2 :c 3}  (multiple pairs)

;; The input is unchanged — assoc returns a fresh map.
(let ((m {:a 1}))
  (assoc m :b 2)
  m)                         ; => {:a 1}
```
