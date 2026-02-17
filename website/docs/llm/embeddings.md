---
outline: [2, 3]
---

# Embeddings & Similarity

Generate vector embeddings from text and compute similarity between them. Embeddings are auto-configured from environment variables (`JINA_API_KEY`, `VOYAGE_API_KEY`, or `COHERE_API_KEY`).

## Configuration

### `llm/configure-embeddings`

Configure a dedicated embedding provider separately from the chat provider.

```scheme
(llm/configure-embeddings :jina {:api-key (env "JINA_API_KEY")})
(llm/configure-embeddings :voyage {:api-key (env "VOYAGE_API_KEY")})
(llm/configure-embeddings :cohere {:api-key (env "COHERE_API_KEY")})

;; OpenAI-compatible embedding provider
(llm/configure-embeddings :openai {:api-key (env "OPENAI_API_KEY")})
```

This allows you to use one provider for chat (e.g., Anthropic) and a different one for embeddings.

## Generating Embeddings

### `llm/embed`

Generate an embedding for a string or a list of strings. Returns a **bytevector** containing densely-packed f64 values in little-endian format. This representation is 2× more memory efficient and 4× faster for similarity computations compared to a list of floats.

```scheme
;; Single embedding (returns a bytevector)
(define v1 (llm/embed "hello world"))

;; Batch embeddings
(llm/embed ["cat" "dog" "fish"])       ; => list of bytevectors
```

## Embedding Accessors

### `embedding/length`

Returns the number of dimensions (f64 elements) in an embedding bytevector.

```scheme
(define v (llm/embed "hello"))
(embedding/length v)                   ; => 1024 (depends on provider)
```

### `embedding/ref`

Access a specific dimension by index.

```scheme
(define v (llm/embed "hello"))
(embedding/ref v 0)                    ; => 0.0123 (first dimension)
```

### `embedding/->list`

Convert an embedding bytevector to a list of floats (useful for interop).

```scheme
(define v (llm/embed "hello"))
(embedding/->list v)                   ; => (0.0123 -0.0456 ...)
```

### `embedding/list->embedding`

Convert a list of numbers to an embedding bytevector.

```scheme
(define v (embedding/list->embedding '(0.1 0.2 0.3)))
(embedding/length v)                   ; => 3
```

## Computing Similarity

### `llm/similarity`

Compute cosine similarity between two embedding vectors. Returns a value between -1.0 and 1.0. Accepts both bytevectors (fast path) and lists of floats (backward compatible).

```scheme
(define v1 (llm/embed "hello world"))
(define v2 (llm/embed "hi there"))
(llm/similarity v1 v2)                 ; => 0.87 (cosine similarity)

;; Also works with plain lists
(llm/similarity '(0.1 0.2 0.3) '(0.4 0.5 0.6))
```

## Token Counting

### `llm/token-count`

Estimate the number of tokens in a string or list of strings. Uses a heuristic (chars/4) — no tokenizer dependency required.

```scheme
(llm/token-count "hello world")           ; => 3
(llm/token-count '("hello" "world"))      ; => sum of individual counts
```

### `llm/token-estimate`

Returns a detailed estimate map with the token count and the estimation method used.

```scheme
(llm/token-estimate "hello world")
; => {:method "chars/4" :tokens 3}
```

## Supported Embedding Providers

| Provider  | Env Variable     |
| --------- | ---------------- |
| Jina      | `JINA_API_KEY`   |
| Voyage    | `VOYAGE_API_KEY` |
| Cohere    | `COHERE_API_KEY` |
| OpenAI    | `OPENAI_API_KEY` |

See [Provider Management](./providers.md) for the full provider capability table.
