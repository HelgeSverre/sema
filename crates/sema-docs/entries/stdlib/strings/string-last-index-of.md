---
name: "string/last-index-of"
module: "strings"
section: "Core String Operations"
---

Find the last occurrence of a substring. Returns the character index (of the start of the match) or `nil` if not found. The mirror of `string/index-of`, which searches from the front.

```sema
(string/last-index-of "abcabc" "abc")   ; => 3
(string/last-index-of "a.b.c" ".")      ; => 3   ; rightmost dot
(string/last-index-of "hello" "xyz")    ; => nil
```
