---
name: "file/copy"
module: "file-io"
section: "File Operations"
---

Copy a file from a source path to a destination path, overwriting the destination if it exists. The original is left in place (use `file/rename` to move instead).

```sema
(file/copy "src.txt" "dst.txt")        ; dst.txt now mirrors src.txt
(file/copy "config.toml" "config.bak") ; quick backup before editing
```
