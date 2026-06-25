---
name: "string/to-keyword"
module: "strings"
section: "Type Conversions"
aliases: ["string->keyword"]
---

Convert a string to a keyword. The inverse is `keyword->string`. The leading `:` is not part of the contents — don't include it in the input.

```sema
(string/to-keyword "name")                  ; => :name
(keyword->string (string/to-keyword "name")) ; => "name"
```
