---
name: "string/remove"
module: "strings"
section: "Replacement"
---

Remove all occurrences of a literal substring. Equivalent to `(string/replace s sub "")`.

```sema
(string/remove "hello world" "o")    ; => "hell wrld"
(string/remove "a-b-c" "-")          ; => "abc"
```
