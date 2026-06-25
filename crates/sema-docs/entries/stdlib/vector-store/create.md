---
name: "vector-store/create"
module: "vector-store"
params: [{ name: name, type: string }]
returns: "string"
---

Create a new, empty in-memory vector store registered under the given name, returning the name. The name is a process-local handle the other `vector-store/*` functions look up — not a file.

The store lives only in memory and vanishes when the process exits unless you `vector-store/save` it. Calling `create` again with the same name replaces the existing store with an empty one, discarding its documents. To load from (and later persist to) disk, use `vector-store/open` instead.

```sema
(vector-store/create "docs")   ; => "docs"
(vector-store/count "docs")    ; => 0
```
