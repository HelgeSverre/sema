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

## Pricing Sources

Sema tracks LLM costs using pricing data from multiple sources, checked in this order:

1. **Custom pricing** — set via `(llm/set-pricing "model" input output)`, always wins
2. **Dynamic pricing** — fetched from [llm-prices.com](https://www.llm-prices.com) during `(llm/auto-configure)`, cached locally at `~/.sema/pricing-cache.json`
3. **Built-in estimates** — hardcoded fallback table (may be outdated)
4. **Unknown** — if no source matches, cost tracking returns `nil` and budget enforcement is best-effort

Dynamic pricing is fetched with a short timeout (2s) and failures are silently ignored. The language works fully offline — the cache persists between sessions.

### `llm/pricing-status`

Check which pricing source is active and when it was last updated.

```scheme
(llm/pricing-status)
; => {:source fetched :updated-at "2025-10-10"}
; or {:source hardcoded} if no dynamic pricing is available
```

## Budget Enforcement

> **Note:** If pricing is unknown for a model (not in any source), budget enforcement operates in best-effort mode — the call proceeds with a one-time warning. Use `(llm/set-pricing)` to set pricing for unlisted models.

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

### `llm/with-budget`

Scoped budget — sets spending limits for the duration of a thunk, then restores the previous budget when done. At least one of `:max-cost-usd` or `:max-tokens` is required. When both are provided, **whichever limit is hit first** triggers the error.

```scheme
;; Cost-based budget
(llm/with-budget {:max-cost-usd 0.50} (lambda ()
  (llm/complete "Expensive operation")))

;; Token-based budget (useful when pricing is unknown or stale)
(llm/with-budget {:max-tokens 10000} (lambda ()
  (llm/complete "Limited tokens")))

;; Both limits — whichever is reached first stops execution
(llm/with-budget {:max-cost-usd 1.00 :max-tokens 50000} (lambda ()
  (llm/complete "Double-capped")
  (println (format "Budget: ~a" (llm/budget-remaining)))))
```

When a token budget is active, `llm/budget-remaining` includes `:token-limit`, `:tokens-spent`, and `:tokens-remaining` in addition to the cost fields.

### `llm/clear-budget`

Remove the spending limit.

```scheme
(llm/clear-budget)
```

### `llm/set-pricing`

Set custom pricing for a model (overrides both dynamic and built-in pricing). Costs are per million tokens.

```scheme
(llm/set-pricing "my-model" 1.0 3.0)   ; $1.00/M input, $3.00/M output
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
