---
name: "modulo"
module: "math"
section: "Scheme Aliases"
params: [{ name: a, type: number }, { name: b, type: number }]
returns: "number"
---

Alias for [`mod`](#mod): truncated remainder whose sign follows the dividend.

```sema
(modulo 10 3)  ; => 1
(modulo -7 2)  ; => -1
```
