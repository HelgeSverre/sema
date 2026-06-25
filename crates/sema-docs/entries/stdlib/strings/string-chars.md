---
name: "string/chars"
module: "strings"
section: "Core String Operations"
---

Convert a string to a list of characters. Splits on Unicode *characters* (not bytes), so multi-byte glyphs stay whole. The inverse is `list->string`; for raw code points use `string/codepoints`.

```sema
(string/chars "abc")   ; => (#\a #\b #\c)
(string/chars "héy")   ; => (#\h #\é #\y)
```
