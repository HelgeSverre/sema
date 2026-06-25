---
name: "string/snake-case"
module: "strings"
section: "Case Conversion"
---

Convert any identifier to snake_case (lowercase words joined by underscores). Shares the `string/words` tokenizer with `string/kebab-case` / `string/camel-case` / `string/pascal-case`, so whitespace, underscores, hyphens, and camelCase humps are all boundaries.

```sema
(string/snake-case "helloWorld")    ; => "hello_world"
(string/snake-case "Hello World")   ; => "hello_world"
(string/snake-case "getUserID")     ; => "get_user_id"
```
