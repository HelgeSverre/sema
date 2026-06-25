---
name: "file/delete"
module: "file-io"
section: "File Operations"
---

Delete a file. Errors if the path does not exist, so guard with `file/exists?` when the file may be absent.

```sema
(when (file/exists? "tmp.txt")
  (file/delete "tmp.txt"))
```
