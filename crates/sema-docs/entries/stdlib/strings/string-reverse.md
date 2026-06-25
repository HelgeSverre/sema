---
name: "string/reverse"
module: "strings"
section: "Core String Operations"
---

Reverse a string. Reverses by Unicode scalar character, so accented letters and emoji stay intact (it does not split a multi-byte character).

```sema
(string/reverse "hello")   ; => "olleh"
(string/reverse "héllo")   ; => "olléh"
```
