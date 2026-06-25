---
name: "string/chop-end"
module: "strings"
section: "Prefix & Suffix"
---

Remove a suffix if present, otherwise return the string unchanged. The inverse of `string/ensure-end`.

```sema
(string/chop-end "file.txt" ".txt")  ; => "file"
(string/chop-end "file.txt" ".md")   ; => "file.txt"   (suffix absent: unchanged)
```
