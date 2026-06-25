---
name: "string/ensure-start"
module: "strings"
section: "Prefix & Suffix"
---

Ensure a string starts with a prefix (adds it only if missing; idempotent). The inverse of `string/chop-start`.

```sema
(string/ensure-start "/path" "/")   ; => "/path"
(string/ensure-start "path" "/")    ; => "/path"
```
