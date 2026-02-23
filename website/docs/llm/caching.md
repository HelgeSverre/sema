---
outline: [2, 3]
---

# Response Caching

Sema provides an in-memory response cache for LLM calls, keyed on prompt + model + temperature. Caching is per-session and useful for iterative development, avoiding duplicate API calls when re-running scripts with the same prompts.

## Cache Scope

### `llm/with-cache`

Wraps a thunk (lambda), enabling the response cache for all LLM calls within it. An optional second argument is an options map with `:ttl` (time-to-live in seconds, default 3600). Returns the thunk's result.

```sema
(llm/with-cache (lambda () (llm/complete "hello")))

(llm/with-cache (lambda () (llm/complete "hello")) {:ttl 7200})
```

## Inspection & Debugging

### `llm/cache-key`

Generate the SHA-256 cache key for a given prompt and options. Useful for debugging cache behavior. Takes 1–2 args: a prompt string and an optional options map.

```sema
(llm/cache-key "hello" {:model "gpt-4" :temperature 0.5})
```

### `llm/cache-stats`

Returns a map with `:hits`, `:misses`, and `:size` (number of cached entries).

```sema
(llm/cache-stats)  ; => {:hits 0 :misses 0 :size 0}
```

## Cache Management

### `llm/cache-clear`

Clear all cached responses. Returns the number of entries cleared.

```sema
(llm/cache-clear)  ; => 0
```
