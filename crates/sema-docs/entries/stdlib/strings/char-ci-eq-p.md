---
name: "char-ci=?"
module: "strings"
section: "Character Comparison (R7RS)"
---

Case-insensitive character equality — like `char=?` but folds case first. Use this for case-insensitive matching; use `char=?` for an exact code-point comparison.

```sema
(char-ci=? #\A #\a)   ; => #t
(char-ci=? #\A #\B)   ; => #f
(char=? #\A #\a)      ; => #f   (case-sensitive sibling)
```
