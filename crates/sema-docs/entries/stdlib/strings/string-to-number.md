---
name: "string/to-number"
module: "strings"
section: "Type Conversions"
aliases: ["string->number"]
---

Parse a string as a number, returning an integer for whole values and a float otherwise. Raises an error if the text isn't a clean number — there is no whitespace tolerance, so trim first.

Use `string/number?` to test parseability without raising, and `string->float` when you always want a float (even for integer text).

```sema
(string/to-number "42")     ; => 42
(string/to-number "3.14")   ; => 3.14
(string/to-number "1e3")    ; => 1000.0
(string/to-number (string/trim "  42  "))  ; => 42
```
