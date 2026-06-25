---
name: "string/repeat"
module: "strings"
section: "Core String Operations"
---

Repeat a string N times, concatenating the copies. N of 0 yields the empty string. Handy for drawing rules and indentation.

```sema
(string/repeat "ab" 3)   ; => "ababab"
(string/repeat "-" 5)    ; => "-----"
(string/repeat "x" 0)    ; => ""
```
