---
name: "string/replace-first"
module: "strings"
section: "Replacement"
---

Replace only the first occurrence of a substring (a literal match, not a regex). Use `string/replace` to replace all occurrences, or `string/replace-last` for the trailing one.

```sema
(string/replace-first "aaa" "a" "b")     ; => "baa"
(string/replace-first "a.b.c" "." "-")   ; => "a-b.c"
```
