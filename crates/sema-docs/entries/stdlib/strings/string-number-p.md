---
name: "string/number?"
module: "strings"
section: "Core String Operations"
---

Test if a string represents a valid number. Useful as a non-raising guard before `string/to-number` (which errors on bad input). Surrounding whitespace counts as invalid.

```sema
(string/number? "42")      ; => #t
(string/number? "3.14")    ; => #t
(string/number? "1e3")     ; => #t
(string/number? "-3.5")    ; => #t
(string/number? "hello")   ; => #f
(string/number? "  42 ")   ; => #f  ; whitespace not tolerated
(string/number? "")        ; => #f
```
