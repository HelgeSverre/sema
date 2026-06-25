---
name: "string/after-last"
module: "strings"
section: "Slicing & Extraction"
---

Everything after the *last* occurrence of a needle (handy for file extensions). Returns the original string if the needle isn't found. Use `string/after` to split on the first occurrence instead.

```sema
(string/after-last "a.b.c" ".")        ; => "c"
(string/after-last "report.tar.gz" ".") ; => "gz"
(string/after-last "no-dot" ".")       ; => "no-dot"   (needle not found)
```
