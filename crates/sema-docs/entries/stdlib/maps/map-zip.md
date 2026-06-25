---
name: "map/zip"
module: "maps"
section: "HashMaps"
---

Build a map by pairing a list of keys with a list of values positionally (the first key with the first value, and so on). If the lists differ in length, zipping stops at the shorter one.

```sema
(map/zip '(:a :b :c) '(1 2 3))   ; => {:a 1 :b 2 :c 3}
(map/zip '(:a :b :c) '(1 2))     ; => {:a 1 :b 2}  (extra key dropped)
```
