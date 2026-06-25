---
name: "file/list"
module: "file-io"
section: "Directory Operations"
---

List the entries (files and subdirectories) directly inside a directory, as bare names in no particular order.

Returns just the entry names, not full paths, and does not descend into subdirectories. Use `file/glob` with `**` when you need to walk a tree or filter by pattern; sort the result yourself if you need a stable order.

```sema
(file/list "src/")   ; => ("main.rs" "vm" "lib.rs" ...)
(sort (file/list "src/"))   ; alphabetical
```
