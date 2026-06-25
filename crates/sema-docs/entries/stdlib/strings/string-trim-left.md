---
name: "string/trim-left"
module: "strings"
section: "Core String Operations"
---

Remove leading whitespace only. See `string/trim-right` for the trailing side and `string/trim` for both.

```sema
(string/trim-left "  hi")     ; => "hi"
(string/trim-left "  hi  ")   ; => "hi  "   ; trailing space kept
```
