---
name: "integer/to-char"
module: "strings"
section: "Characters"
aliases: ["integer->char"]
---

Convert a Unicode code point to a character. The inverse of `char/to-integer`. Errors on a code point that isn't a valid scalar value (negative, surrogate range `D800`–`DFFF`, or above `10FFFF`).

```sema
(integer/to-char 65)    ; => #\A
(integer/to-char 955)   ; => #\λ
```
