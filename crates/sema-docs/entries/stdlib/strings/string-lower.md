---
name: "string/lower"
module: "strings"
section: "Core String Operations"
aliases: ["string-downcase"]
---

Convert a string to lowercase (Unicode-aware). For case-insensitive *comparison*, prefer `string/foldcase` — lowercasing doesn't canonicalize every caseless equivalence (e.g. `ß`).

```sema
(string/lower "HELLO")   ; => "hello"
(string/lower "HÉLLO")   ; => "héllo"
```
