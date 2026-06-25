---
name: "string/replace-last"
module: "strings"
section: "Replacement"
---

Replace only the last occurrence of a substring (a literal match, not a regex). The mirror of `string/replace-first`; use `string/replace` to replace all.

```sema
(string/replace-last "aaa" "a" "b")     ; => "aab"
(string/replace-last "a.b.c" "." "-")   ; => "a.b-c"
```
