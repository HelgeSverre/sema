---
outline: [2, 3]
---

# Embeddings & Similarity

Generate vector embeddings from text and compute similarity between them. On startup
`(llm/auto-configure)` picks an embedding provider by **precedence** — `JINA_API_KEY`,
then `VOYAGE_API_KEY`, then `COHERE_API_KEY`; if none is set it falls back to
`OPENAI_API_KEY` (`text-embedding-3-small`). The first key present wins.

## Configuration

### `llm/configure-embeddings`

Configure a dedicated embedding provider separately from the chat provider — so you can use
one provider for chat and another for embeddings. Pass `:default-model` to pick the model
(otherwise each provider uses its default: `jina-embeddings-v3`, `voyage-3`, or
`text-embedding-3-small`):

```sema
(llm/configure-embeddings :voyage
  {:api-key (env "VOYAGE_API_KEY") :default-model "voyage-3-large"})

;; OpenAI-compatible embedding provider, with a model and optional base URL
(llm/configure-embeddings :openai
  {:api-key (env "OPENAI_API_KEY") :default-model "text-embedding-3-large"})
```

## Generating Embeddings

### `llm/embed`

Generate an embedding for a string or a list of strings. Returns a **bytevector** containing densely-packed f64 values in little-endian format. This representation is 2× more memory efficient and 4× faster for similarity computations compared to a list of floats.

```sema
;; Single embedding (returns a bytevector)
(define v1 (llm/embed "hello world"))

;; Pick the model per call with an options map
(llm/embed "hello world" {:model "text-embedding-3-small"})

;; Batch embeddings
(llm/embed ["cat" "dog" "fish"])       ; => list of bytevectors
```

## Embedding Accessors

### `embedding/length`

Returns the number of dimensions (f64 elements) in an embedding bytevector.

```sema
(define v (llm/embed "hello"))
(embedding/length v)                   ; => 1024 (depends on provider)
```

### `embedding/ref`

Access a specific dimension by index.

```sema
(define v (llm/embed "hello"))
(embedding/ref v 0)                    ; => 0.0123 (first dimension)
```

### `embedding/->list`

Convert an embedding bytevector to a list of floats (useful for interop).

```sema
(define v (llm/embed "hello"))
(embedding/->list v)                   ; => (0.0123 -0.0456 ...)
```

### `embedding/list->embedding`

Convert a list of numbers to an embedding bytevector.

```sema
(define v (embedding/list->embedding '(0.1 0.2 0.3)))
(embedding/length v)                   ; => 3
```

## Computing Similarity

### `llm/similarity`

Compute cosine similarity between two embedding vectors. Returns a value between -1.0 and 1.0. Accepts both bytevectors (fast path) and lists of floats (backward compatible).

```sema
(define v1 (llm/embed "hello world"))
(define v2 (llm/embed "hi there"))
(llm/similarity v1 v2)                 ; => 0.87 (cosine similarity)

;; Also works with plain lists
(llm/similarity '(0.1 0.2 0.3) '(0.4 0.5 0.6))
```

## Reranking

### `llm/rerank`

Reorder a list of candidate documents by their relevance to a query using a hosted **cross-encoder** reranker (Cohere, Jina, or Voyage — the same **API key** you already use for embeddings, e.g. `COHERE_API_KEY` / `JINA_API_KEY` / `VOYAGE_API_KEY`; see [Supported Embedding Providers](#supported-embedding-providers) below for setup). Where `llm/similarity` / `vector-store/search` embed the query and documents *independently* (a bi-encoder), a reranker reads the query and each document *together*, so it's far more precise. The standard pattern is to retrieve a generous shortlist by vector search, then rerank it to the best few.

```sema
(llm/rerank "how do I read a file?"
            (list "vectors are cool" "use file/read to read a file" "unrelated trivia")
            {:top-k 2})
;; => ({:index 1 :score 0.91 :document "use file/read to read a file"} ...)
```

Returns `{:index :score :document}` maps, highest relevance first; `:index` points back into the input list. Options: `:top-k`, `:model`, and `:provider` (`:cohere` / `:jina` / `:voyage`). See the **[RAG guide](/docs/llm/rag)** for the full retrieve → rerank → answer pipeline.

## Token Counting

### `llm/token-count`

Estimate the number of tokens in a string or list of strings. Uses a heuristic (chars/4) — no tokenizer dependency required.

```sema
(llm/token-count "hello world")           ; => 3
(llm/token-count '("hello" "world"))      ; => sum of individual counts
```

### `llm/token-estimate`

Returns a detailed estimate map with the token count and the estimation method used.

```sema
(llm/token-estimate "hello world")
; => {:method "chars/4" :tokens 3}
```

## Supported Embedding Providers

| Provider | Env Variable     |
| -------- | ---------------- |
| Jina     | `JINA_API_KEY`   |
| Voyage   | `VOYAGE_API_KEY` |
| Cohere   | `COHERE_API_KEY` |
| OpenAI   | `OPENAI_API_KEY` |

See [Provider Management](./providers.md) for the full provider capability table.
