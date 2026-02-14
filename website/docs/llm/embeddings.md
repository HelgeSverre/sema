---
outline: [2, 3]
---

# Embeddings & Similarity

Generate vector embeddings from text and compute similarity between them. Embeddings are auto-configured from environment variables (`JINA_API_KEY`, `VOYAGE_API_KEY`, or `COHERE_API_KEY`).

## Generating Embeddings

### `llm/embed`

Generate an embedding vector for a string or a list of strings.

```scheme
;; Single embedding
(define v1 (llm/embed "hello world"))

;; Batch embeddings
(llm/embed ["cat" "dog" "fish"])       ; => list of vectors
```

## Computing Similarity

### `llm/similarity`

Compute cosine similarity between two embedding vectors. Returns a value between -1.0 and 1.0.

```scheme
(define v1 (llm/embed "hello world"))
(define v2 (llm/embed "hi there"))
(llm/similarity v1 v2)                 ; => 0.87 (cosine similarity)
```

## Supported Embedding Providers

| Provider  | Env Variable     |
| --------- | ---------------- |
| Jina      | `JINA_API_KEY`   |
| Voyage    | `VOYAGE_API_KEY` |
| Cohere    | `COHERE_API_KEY` |
| OpenAI    | `OPENAI_API_KEY` |

See [Provider Management](./providers.md) for the full provider capability table.
