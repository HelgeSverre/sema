---
name: "string/starts-with?"
module: "strings"
section: "Core String Operations"
---

Test whether a string begins with a given prefix. See `string/ends-with?` for the suffix check, and `string/index-of` to find a substring anywhere.

```sema
(string/starts-with? "hello" "he")   ; => #t
(string/starts-with? "hello" "lo")   ; => #f
(string/starts-with? "hello" "")     ; => #t  ; empty prefix always matches
```
