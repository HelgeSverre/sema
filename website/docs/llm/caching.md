---
outline: [2, 3]
---

# Response Caching

Sema caches LLM responses so identical calls don't hit the API twice. The cache is
**persistent**: responses are written to `~/.sema/cache/llm/` (one JSON file per entry,
named by a SHA-256 key), so a re-run of a script — even in a new process — serves the
answer recorded by an earlier run. An in-memory layer sits on top for the current session.

A call is a cache hit when its **model, temperature, system prompt, and full message
list** all match a stored entry. `:max-tokens` and `:tools` are *not* part of the key.

Caching is **off by default** — turn it on for a block with `llm/with-cache`.

> For replay that you **commit and share** (deterministic tests, offline demos), see
> [Cassettes](./cassettes) instead. They're a different tool: a cassette stores a tape
> next to your code rather than in your personal cache dir, and the response cache is
> turned off inside `llm/with-cassette`.

## Cache scope

### `llm/with-cache`

Run a thunk with caching enabled for every LLM call inside it. The **options map comes
first** when you pass one; with a single argument it's just the thunk. `:ttl` sets the
time-to-live in seconds (default 3600). Previous cache settings are restored on exit.

```sema
;; thunk only
(llm/with-cache (lambda () (llm/complete "hello")))

;; with options — opts FIRST, then the thunk
(llm/with-cache {:ttl 7200} (lambda () (llm/complete "hello")))
```

A cache hit costs nothing: it makes no provider call, so it reports **zero** token usage
and spends nothing against a [budget](./cost). The two calls below show a miss then a hit:

```sema
(llm/with-cache (lambda ()
  (llm/complete "what is 2+2?")   ; miss — calls the model, stores the answer
  (llm/complete "what is 2+2?")   ; hit  — served from the cache, no API call
  (llm/cache-stats)))             ; => {:hits 1 :misses 1 :size 1}
```

## Inspection & debugging

### `llm/cache-key`

Generate the SHA-256 cache key for a prompt and options — handy for debugging why two
calls do or don't share a cache entry. Takes a prompt string and an optional options map.

```sema
(llm/cache-key "hello" {:model "gpt-4" :temperature 0.5})
```

### `llm/cache-stats`

Returns `{:hits :misses :size}`. Note that `:size` counts only the entries loaded into
memory **this session** — a cold start can serve hits from disk before `:size` reflects
them.

```sema
(llm/cache-stats)  ; => {:hits 0 :misses 0 :size 0}
```

## Cache management

### `llm/cache-clear`

Clear cached responses — both the in-memory entries and the files in `~/.sema/cache/llm/`.
Returns the number of entries cleared.

```sema
(llm/cache-clear)  ; => 0
```
