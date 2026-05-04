---
outline: [2, 3]
---

# Living Code

Living Code extends Sema's [metaprogramming primitives](../language/metaprogramming.md) with LLM intelligence -- programs that answer questions about themselves, repair their own bugs, and evolve toward better solutions.

These features require an LLM provider to be configured. See [Completion & Chat](./completion.md) for setup.

::: tip Prerequisites
Living Code builds on docstrings, doctests, `source-of`, `become!`, and `history`. If you're not familiar with those, read [Metaprogramming](../language/metaprogramming.md) first.
:::

### Quick Taste

```sema
(defn factorial "Compute n!.
    >>> (factorial 0)
    1
    >>> (factorial 5)
    120"
  (n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

;; Ask questions about any value
(ask factorial "Is this tail-recursive?")

;; Get code suggestions
(ask/code factorial "Make this tail-recursive")

;; Auto-repair: LLM rewrites until all doctests pass
(heal! parse-date)

;; Evolve: breed candidates via LLM, scored by fitness
(evolve :name "fast-sort" :spec '((>>> (fast-sort '(3 1 2)) (1 2 3)))
        :fitness (fn (f r) (get r :passed)) :seed-prompt "Write a sort function.")
```

## Conversational Introspection

### `(ask target question)`

Ask a natural-language question about any value. The LLM receives the value's source, metadata, and docstring as context.

```sema
(ask factorial "Will this blow the stack for large inputs?")
; => "Yes, this implementation is not tail-recursive. For n > ~10000,
;     you'll hit a stack overflow. Consider using an accumulator pattern..."

(ask {:host "db.prod" :password "hunter2"} "Any security concerns?")
; => "The password is hardcoded in plain text..."
```

`ask` works on any value -- functions, maps, lists, strings. For functions, it automatically includes source code, parameters, and docstring in the LLM context.

### `(ask/code target instruction)`

Like `ask`, but the LLM returns a Sema code expression that can be evaluated or applied with [`become!`](../language/metaprogramming.md#become-sym-new-definition).

```sema
(ask/code factorial "Make this tail-recursive")
; => "(fn (n) (let loop ((i n) (acc 1)) (if (<= i 1) acc (loop (- i 1) (* acc i)))))"
```

::: warning
`ask/code` returns a string containing code, not an evaluated result. Inspect it before applying with `become!`.
:::

### `(heal! sym)`

Auto-repair a function by running its doctests. If any fail, the LLM is asked to rewrite the function to pass them. The process repeats (up to 5 attempts) until all doctests pass or attempts are exhausted.

```sema
;; A parser that only handles one format
(defn parse-date "Parse various date formats.
    >>> (parse-date \"2024-01-15\")
    \"2024-01-15\"
    >>> (parse-date \"Jan 15, 2024\")
    \"2024-01-15\"
    >>> (parse-date \"15/01/2024\")
    \"2024-01-15\""
  (s) s)  ; naive: just returns the string

(heal! parse-date)
;; The LLM iteratively rewrites parse-date until all 3 doctests pass.
;; On success, the new implementation replaces the old one.
```

`heal!` uses the doctest gate from `become!` -- each candidate must pass all doctests. The old version is saved to [`history`](../language/metaprogramming.md#history-sym).

You can also heal from the CLI:

```bash
sema test --heal src/lib.sema   # run doctests, auto-heal failures
```

::: tip
`heal!` works best with small, well-specified functions. Write tight doctests that cover edge cases -- the LLM uses them as both the specification and the acceptance criteria.
:::

## Genetic Programming

### `(evolve :name ... :spec ... :fitness fn :seed-prompt ...)`

Generate a population of candidate function implementations via LLM, test them against a specification, and iteratively breed and mutate the best ones to produce an optimized solution.

**Required parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `:name` | string | Name for the evolved function |
| `:spec` | list or function | Test specification (see below) |
| `:fitness` | function | `(fn (candidate results) score)` -- higher is better |
| `:seed-prompt` | string | Description of what the function should do |

**Optional parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `:population` | integer | 10 | Number of candidates per generation |
| `:generations` | integer | 5 | Number of evolution rounds |
| `:max-attempts` | integer | 3 | LLM retries per candidate |
| `:verbose` | boolean | `#f` | Print progress to stderr |

**Spec modes:**

Inline spec -- a quoted list of `(>>> expr expected)` forms:

```sema
(evolve
  :name "fast-square"
  :spec '((>>> (fast-square 0) 0)
          (>>> (fast-square 5) 25)
          (>>> (fast-square -3) 9))
  :fitness (fn (f results)
    (if (= (get results :passed) (get results :total))
        (/ 1000.0 (max 0.01 (get results :time-ms)))
        0))
  :seed-prompt "Write a function that squares a number."
  :population 5
  :generations 3
  :verbose #t)
```

Docstring spec -- pass a function whose docstring contains doctests:

```sema
(defn my-sort "Sort a list.
    >>> (my-sort '(3 1 2))
    (1 2 3)
    >>> (my-sort '())
    ()
    >>> (my-sort '(1))
    (1)"
  (lst) lst)  ; placeholder

(evolve
  :name "my-sort"
  :spec my-sort
  :fitness (fn (f results)
    (* (get results :passed)
       (/ 100.0 (max 0.1 (get results :time-ms)))))
  :seed-prompt "Write an efficient sorting function in Sema.")
```

**The evolution loop:**

1. **Seed** -- LLM generates `:population` candidates from the seed prompt
2. **Evaluate** -- each candidate is parsed, evaluated in a sandbox, tested against the spec, and scored by the fitness function
3. **Select** -- top 30% survive as elites
4. **Breed** -- remaining slots filled by crossover (combine two parents) or mutation (improve one parent) via LLM
5. **Repeat** for `:generations` rounds
6. **Return** the best candidate as a function value

**Fitness function:**

The fitness function receives two arguments: the candidate function value and a results map `{:passed n :total n :time-ms f}`. Return a number -- higher is better.

```sema
;; Correctness only
(fn (f results) (get results :passed))

;; Correctness + speed
(fn (f results)
  (if (= (get results :passed) (get results :total))
      (/ 1000.0 (max 0.01 (get results :time-ms)))
      0))

;; Multi-objective: 70% correctness, 30% speed
(fn (f results)
  (+ (* 0.7 (/ (get results :passed) (max 1 (get results :total))))
     (* 0.3 (/ 10.0 (max 0.01 (get results :time-ms))))))
```

::: tip
Start with correctness-only fitness. Once `evolve` reliably produces correct candidates, add performance objectives.
:::

## Scenarios

### Self-Healing Parser

Define the full specification as doctests, then let the system heal itself:

```sema
(defn parse-date "Parse date strings into ISO format.
    >>> (parse-date \"2024-01-15\")
    \"2024-01-15\"
    >>> (parse-date \"Jan 15, 2024\")
    \"2024-01-15\"
    >>> (parse-date \"15/01/2024\")
    \"2024-01-15\""
  (s) s)

(doctest parse-date)     ; => {:passed 1 :total 3}
(heal! parse-date)       ; LLM fixes it
(doctest parse-date)     ; => {:passed 3 :total 3}
(source-of parse-date)   ; inspect the new implementation
(freeze! parse-date)     ; lock it down
```

### Observe, Ask, Upgrade

Use [`observe!`](../language/metaprogramming.md#observe-sym-sample-size-callback) to collect usage data, then let the LLM decide how to optimize:

```sema
(defn compute "Expensive computation.
    >>> (compute 1)
    42
    >>> (compute 2)
    84"
  (x) (begin (for-each (fn (_) nil) (range 10000)) (* x 42)))

;; Observe usage patterns
(observe! compute 50
  (fn (log)
    (let ((unique (length (dedup (map #(get % :args) log))))
          (total  (length log)))
      (when (< unique (/ total 2))
        ;; Many repeated calls -- ask for memoization
        (let ((suggestion (ask/code compute "Add memoization")))
          (println f"Suggested:\n${suggestion}"))))))
```

### Evolving for Performance

Use `evolve` to find the fastest implementation, then benchmark and freeze:

```sema
(define best-fib
  (evolve
    :name "fib"
    :spec '((>>> (fib 0) 0)
            (>>> (fib 1) 1)
            (>>> (fib 10) 55)
            (>>> (fib 20) 6765))
    :fitness (fn (f results)
      (if (= (get results :passed) (get results :total))
          (/ 1000.0 (max 0.01 (get results :time-ms)))
          0))
    :seed-prompt "Write a fast Fibonacci function. Consider memoization or iteration."
    :population 8
    :generations 3
    :verbose #t))

;; Benchmark the winner
(bench (fn () (best-fib 30)) 1000)
```

## Best Practices

1. **Doctests are contracts.** Every function that participates in `heal!` or `evolve` needs doctests. They are what the LLM tries to satisfy.

2. **Keep targets small.** Don't `ask` or `heal!` an entire module. Target individual functions with clear specifications.

3. **Inspect before applying.** Use `ask` to understand, `ask/code` to generate, review the output, then `become!` to apply.

4. **Use `observe!` before optimizing.** Measure before you modify. Let the data tell you what to optimize.

5. **Freeze after healing.** Once `heal!` or `evolve` produces a good result, `(freeze! sym)` prevents accidental modification.

## Related

- [Metaprogramming](../language/metaprogramming.md) -- docstrings, doctests, introspection, `become!`/`rollback!`/`freeze!` (no LLM needed)
- [Completion & Chat](./completion.md) -- LLM provider setup
- [Tools & Agents](./tools-agents.md) -- structured LLM interactions
