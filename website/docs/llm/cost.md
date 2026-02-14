---
outline: [2, 3]
---

# Cost Tracking & Budgets

## Usage Tracking

### `llm/last-usage`

Get token usage from the most recent LLM call.

```scheme
(llm/last-usage)   ; => {:prompt-tokens 42 :completion-tokens 15 ...}
```

### `llm/session-usage`

Get cumulative usage across all LLM calls in the current session.

```scheme
(llm/session-usage)
```

### `llm/reset-usage`

Reset session usage counters.

```scheme
(llm/reset-usage)
```

## Budget Enforcement

### `llm/set-budget`

Set a spending limit (in dollars) for the session. LLM calls that would exceed the budget will fail.

```scheme
(llm/set-budget 1.00)   ; set $1.00 spending limit
```

### `llm/budget-remaining`

Check current budget status.

```scheme
(llm/budget-remaining)   ; => {:limit 1.0 :spent 0.05 :remaining 0.95}
```

### `llm/clear-budget`

Remove the spending limit.

```scheme
(llm/clear-budget)
```

### `llm/set-pricing`

Set custom pricing for unlisted models (input/output cost per million tokens).

```scheme
(llm/set-pricing "my-model" 1.0 3.0)
```

## Batch & Parallel

### `llm/batch`

Send multiple prompts concurrently and collect all results.

```scheme
(llm/batch ["Translate 'hello' to French"
            "Translate 'hello' to Spanish"
            "Translate 'hello' to German"])
```

### `llm/pmap`

Map a function over items, sending all resulting prompts in parallel.

```scheme
(llm/pmap
  (fn (word) (format "Define: ~a" word))
  '("serendipity" "ephemeral" "ubiquitous")
  {:max-tokens 50})
```
