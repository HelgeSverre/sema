---
outline: [2, 3]
---

# Vector Store & Math

## In-Memory Vector Store

Sema includes an in-memory vector store for semantic search over embeddings. Create named stores, add documents with embeddings and metadata, and search by cosine similarity. Stores can optionally be persisted to disk as JSON.

### `vector-store/create`

Create a named in-memory vector store. Returns the store name.

```sema
(vector-store/create "my-store")
```

### `vector-store/open`

Open a named store backed by a file. If the file exists, its contents are loaded; otherwise an empty store is created. The path is remembered for subsequent `vector-store/save` calls.

```sema
(vector-store/open "my-store" "embeddings.json")
```

### `vector-store/add`

Add a document with an ID, embedding (bytevector), and metadata map.

```sema
(vector-store/add "my-store" "doc-1"
  (llm/embed "Hello world")
  {:source "greeting.txt" :page 1})
```

If a document with the same ID exists, it is replaced.

### `vector-store/search`

Search by cosine similarity. Takes store name, query embedding, and k (number of results). Returns a list of maps with `:id`, `:score`, and `:metadata`.

```sema
(vector-store/search "my-store" (llm/embed "Hi there") 5)
;; => ({:id "doc-1" :score 0.92 :metadata {:source "greeting.txt" :page 1}} ...)
```

### `vector-store/delete`

Delete a document by ID. Returns `#t` if found, `#f` otherwise.

```sema
(vector-store/delete "my-store" "doc-1")  ; => #t
```

### `vector-store/count`

Return the number of documents in a store.

```sema
(vector-store/count "my-store")  ; => 42
```

### `vector-store/save`

Save a store to disk as JSON. If the store was opened with `vector-store/open`, the path is used automatically. Otherwise, pass a path explicitly.

```sema
;; Explicit path
(vector-store/save "my-store" "embeddings.json")

;; Implicit path (if opened with vector-store/open)
(vector-store/save "my-store")
```

The file format is a JSON document with base64-encoded embeddings and full metadata, portable across platforms.

## Vector Math

These functions operate on embedding bytevectors (packed f64 arrays in little-endian format, as returned by `llm/embed` or `embedding/list->embedding`).

### `vector/cosine-similarity`

Cosine similarity between two embedding vectors. Returns a float between -1.0 and 1.0.

```sema
(vector/cosine-similarity
  (embedding/list->embedding '(1.0 0.0))
  (embedding/list->embedding '(0.0 1.0)))
; => 0.0
```

### `vector/dot-product`

Dot product of two embedding vectors.

```sema
(vector/dot-product
  (embedding/list->embedding '(1.0 2.0 3.0))
  (embedding/list->embedding '(4.0 5.0 6.0)))
; => 32.0
```

### `vector/normalize`

Return a unit-length copy of the vector.

```sema
(vector/normalize (embedding/list->embedding '(3.0 4.0)))
;; => embedding with values (0.6 0.8)
```

### `vector/distance`

Euclidean distance between two embedding vectors.

```sema
(vector/distance
  (embedding/list->embedding '(0.0 0.0))
  (embedding/list->embedding '(3.0 4.0)))
; => 5.0
```

## Full Example

A RAG-style workflow: embed documents, store them, search semantically, and persist to disk.

```sema
;; Open a persistent store (creates file if it doesn't exist)
(vector-store/open "docs" "my-docs.json")

(define texts '("Rust is a systems language"
                "Python is great for ML"
                "Lisp is homoiconic"))

(for-each
  (lambda (text)
    (vector-store/add "docs" text (llm/embed text) {:text text}))
  texts)

;; Save to disk
(vector-store/save "docs")

;; Retrieve the most relevant chunks for a question...
(define question "Which language is homoiconic?")
(define hits (vector-store/search "docs" (llm/embed question) 2))

;; ...then generate an answer grounded in only that context (the "G" in RAG)
(define context (string/join (map (lambda (h) (:text (:metadata h))) hits) "\n"))
(llm/complete
  (prompt (system "Answer using only the provided context. Be concise.")
          (user (format "Context:\n~a\n\nQuestion: ~a" context question)))
  {:max-tokens 120})
;; => "Lisp — it is homoiconic."
```

Next time you run, `(vector-store/open "docs" "my-docs.json")` will load the saved embeddings instantly — no re-embedding needed.

::: warning Use one embedding model per store
Every document and the query must share the same embedding dimensions. Mixing embedding
models (or providers) in one store raises a *dimension-mismatch* error at search time — so
pick one embedding model per store.
:::
