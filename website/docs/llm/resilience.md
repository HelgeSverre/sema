---
outline: [2, 3]
---

# Resilience & Retry

## Fallback Provider Chains

### `llm/with-fallback`

Wraps a thunk with a fallback chain of providers. If the LLM call fails with one provider, automatically tries the next provider in the list.

```scheme
(llm/with-fallback [:anthropic :openai :groq]
  (lambda () (llm/complete "Hello")))
```

## Rate Limiting

### `llm/with-rate-limit`

Wraps a thunk with token-bucket rate limiting. Takes a rate (requests per second) and a thunk. Useful to avoid hitting API rate limits.

```scheme
(llm/with-rate-limit 5 (lambda () (llm/complete "Hello")))
```

## Generic Retry

### `retry`

Retries a thunk on failure with exponential backoff. Takes a thunk and an optional options map.

```scheme
;; Default: 3 attempts, 100ms base delay, 2.0 backoff
(retry (lambda () (http/get "https://example.com")))

;; Custom options
(retry (lambda () (http/get "https://example.com"))
  {:max-attempts 5 :base-delay-ms 200 :backoff 1.5})
```

Options:

| Key              | Type    | Default | Description                        |
| ---------------- | ------- | ------- | ---------------------------------- |
| `:max-attempts`  | integer | 3       | Maximum number of attempts         |
| `:base-delay-ms` | integer | 100     | Initial delay between retries (ms) |
| `:backoff`       | float   | 2.0     | Backoff multiplier                 |

> **Note:** `retry` is in the stdlib (not LLM-specific) — it works with any function.

## LLM Convenience Functions

### `llm/summarize`

Summarize text using an LLM. Takes text and an optional options map.

```scheme
(llm/summarize "Long article text here...")
(llm/summarize "Long text" {:model "claude-haiku-4-5-20251001" :max-tokens 200})
```

### `llm/compare`

Compare two texts using an LLM. Takes two strings and an optional options map.

```scheme
(llm/compare "Text A" "Text B")
(llm/compare "Text A" "Text B" {:model "claude-haiku-4-5-20251001"})
```
