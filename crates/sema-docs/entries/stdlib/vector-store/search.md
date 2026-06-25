---
name: "vector-store/search"
module: "vector-store"
params: [{ name: name, type: string }, { name: query, type: bytevector }, { name: k, type: int }]
returns: "list"
---

Return the top `k` documents in the named store ranked by cosine similarity to the query embedding. Each result is a map with `:id`, `:score` (1.0 = identical direction, 0.0 = orthogonal), and `:metadata` — the exact value you passed to `vector-store/add`. Results come back highest-score first.

This is the retrieval half of a RAG pipeline: embed the user's question with the **same model** you used to index the documents, search, then feed the matched documents' text into a prompt. The query embedding's dimensionality must match the stored vectors, since cosine similarity is element-wise.

```sema
;; Typical use — embed the query with llm/embed, then search.
(vector-store/search "docs" (llm/embed "how do I log in?") 5)
;; => ({:id "faq-7" :score 0.83 :metadata {:title "Signing in"}} ...)
```

The query is a plain embedding bytevector, so you can search with hand-built vectors too (handy for tests without a network round-trip):

```sema
(vector-store/create "demo")
(vector-store/add "demo" "x" (embedding/list->embedding (list 1.0 0.0)) {:t "x-axis"})
(vector-store/add "demo" "y" (embedding/list->embedding (list 0.0 1.0)) {:t "y-axis"})
(vector-store/search "demo" (embedding/list->embedding (list 0.9 0.1)) 1)
;; => ({:id "x" :score 0.993... :metadata {:t "x-axis"}})
```
