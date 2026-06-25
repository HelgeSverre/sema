---
name: "vector-store/add"
module: "vector-store"
params: [{ name: name, type: string }, { name: id, type: string }, { name: embedding, type: bytevector }, { name: metadata }]
returns: "string"
---

Add (or replace, by `id`) a document in the named vector store with its embedding bytevector and an arbitrary metadata value. Returns the document id.

Re-adding the same `id` overwrites the previous entry, so this doubles as an update. The `metadata` can be anything — typically a map carrying the original text plus fields you'll want back from `vector-store/search` (which echoes `:metadata` verbatim). Store enough to reconstruct the document; the embedding itself is opaque numbers. The embedding's byte length must be a multiple of 8 (it is a packed array of f64), which `llm/embed` and `embedding/list->embedding` both guarantee.

```sema
;; Index a chunk of text alongside its embedding and the source text itself.
(vector-store/add "docs" "doc-1"
                  (llm/embed "Sema is a homoiconic Lisp.")
                  {:title "Intro" :text "Sema is a homoiconic Lisp."})

;; Same id again → replaces the earlier document.
(vector-store/add "docs" "doc-1" (llm/embed "Updated text") {:title "Intro v2"})
```
