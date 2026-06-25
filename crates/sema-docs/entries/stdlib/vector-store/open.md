---
name: "vector-store/open"
module: "vector-store"
params: [{ name: name, type: string }, { name: path, type: string }]
returns: "string"
---

Open a vector store under the given name, loading it from the JSON file at `path` if it exists or creating an empty one otherwise, and associate the path so later `vector-store/save` calls can omit it. Returns the name.

This is the disk-backed counterpart to `vector-store/create`: missing file means you start empty, an existing file restores every document. Because the path is remembered, the usual lifecycle is `open` once at startup, `add`/`search` during the run, then a bare `(vector-store/save name)` to flush back to the same file.

```sema
(vector-store/open "docs" "docs.json")    ; loads if present, else empty
;; ... add / search ...
(vector-store/save "docs")                 ; writes back to docs.json (path remembered)
```
