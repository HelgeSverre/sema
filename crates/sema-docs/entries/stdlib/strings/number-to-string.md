---
name: "number/to-string"
module: "strings"
section: "Type Conversions"
aliases: ["number->string"]
---

Convert a number to a string. The inverse is `string->number`.

```sema
(number/to-string 42)      ; => "42"
(number/to-string 3.14)    ; => "3.14"
(number/to-string -7)      ; => "-7"
```
