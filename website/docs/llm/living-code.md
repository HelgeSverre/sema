---
outline: [2, 3]
---

# Living Code

Sema programs can inspect, test, and rewrite themselves at runtime. Living Code is a set of special forms and stdlib functions that turn code into a conversational, self-aware medium -- programs that document themselves, answer questions about their own behavior, heal their own bugs, and evolve toward better solutions.

::: warning
Living Code features can change program behavior at runtime. Use doctests as contracts, `history`/`rollback!` for reversibility, and `freeze!` to stabilize proven code.
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

;; Inspect
(doc factorial)                ; pretty-print docs + source
(source-of factorial)          ; => "(fn (n) (if (<= n 1) 1 ...))"
(doctest factorial)            ; => {:passed 2 :total 2 :name "factorial"}

;; Converse
(ask factorial "Is this tail-recursive?")

;; Upgrade
(become! factorial
  (fn (n) (let loop ((i n) (acc 1))
    (if (<= i 1) acc (loop (- i 1) (* acc i))))))

;; Review
(history factorial)            ; => ({:version 1 :value <lambda> :source "..."})
```

## Docstrings & Doctests

### `defn` with docstrings

`defn` accepts an optional docstring as its second argument (before params). The docstring is stored as metadata and used by `doc`, `doctest`, `ask`, and `heal!`. The Scheme-style `define` also supports docstrings: `(define (f x) "docstring" body...)`.

```sema
(defn factorial "Compute n!.
    >>> (factorial 0)
    1
    >>> (factorial 5)
    120"
  (n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))
```

Doctest syntax:

- `>>> expr` followed by expected result on the next line
- `!! substring` expects an error containing substring
- `>>>!` evaluates but doesn't check (setup)

### `(doc sym)`

Pretty-print documentation for a symbol. Shows name, parameters, docstring, and source.

```sema
(doc factorial)
```

### `(doc/search query)`

Search all defined symbols by name or docstring content. Returns a list of `{:name ... :doc ...}` maps.

```sema
(doc/search "sort")    ; find all functions mentioning "sort"
```

### `(doctest sym)`

Run all doctests from a symbol's docstring. Returns `{:passed n :total n :name "..."}`.

```sema
(doctest factorial)    ; => {:name "factorial" :passed 2 :total 2}
```

### `(meta sym)`

Return all metadata for a symbol as a map. Includes `:name`, `:type`, `:params`, `:arity`, `:doc`, `:source`, `:variadic?`, and any custom metadata.

```sema
(meta factorial)
; => {:arity 1 :doc "Compute n!. ..." :name "factorial" :params (n) :type :lambda}
```

### Testing from the CLI

```bash
sema test src/**/*.sema       # run all doctests in matching files
sema test --heal src/lib.sema # run doctests, auto-heal failures with LLM
```

::: tip
Treat docstrings as specifications: concise purpose + concrete examples. The LLM reads them during `ask`, `heal!`, and `evolve`.
:::

## Source Introspection

### `(source-of sym)`

Return the source text of a definition as a string.

```sema
(source-of factorial)  ; => "(fn (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"
```

### Reader directives: `;;@key value`

Attach structured metadata to definitions using directive comments. Place them on the line before a `defn`, `define`, or `defmacro`.

```sema
;;@since 1.5
;;@stability stable
;;@author helge
(defn parse-date "Parse a date string." (s) ...)
```

Directives are captured by the reader and available through `read-source`.

### `(read-source path)`

Read and parse source files, returning structured data for each top-level definition. Supports glob patterns.

```sema
(read-source "src/**/*.sema")
; => list of {:type :defn :name "..." :doc "..." :params (...) :body (...) :directives {...} :source "..."}
```

::: tip
Combine `read-source` with higher-order functions to build custom tooling:
```sema
;; Find all deprecated functions
(->> (read-source "src/**/*.sema")
     (filter #(get-in % [:directives :deprecated]))
     (map :name))
```
:::

## Conversational Introspection

These forms require an LLM provider to be configured.

### `(ask target question)`

Ask a natural-language question about any value. The LLM receives the value's source, metadata, and docstring as context.

```sema
(ask factorial "Will this blow the stack for large inputs?")
; => "Yes, this implementation is not tail-recursive. For n > ~10000,
;     you'll hit a stack overflow. Consider using an accumulator pattern..."

(ask {:host "db.prod" :password "hunter2"} "Any security concerns?")
; => "The password is hardcoded in plain text..."
```

### `(ask/code target instruction)`

Like `ask`, but the LLM returns a Sema code expression that can be evaluated.

```sema
(ask/code factorial "Make this tail-recursive")
; => "(fn (n) (let loop ((i n) (acc 1)) (if (<= i 1) acc (loop (- i 1) (* acc i)))))"
```

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

::: warning
`heal!` modifies the function in place. Use `(source-of sym)` after healing to inspect the new implementation, and `(history sym)` to see previous versions.
:::

## Runtime Self-Modification

### `(observe! sym sample-size callback)`

Wrap a function with an invisible logger. After `sample-size` calls, the original function is restored and `callback` is invoked with the call log (a list of `{:args ... :result ... :time-ms ...}` maps).

```sema
(defn fib "Fibonacci.
    >>> (fib 0)
    0
    >>> (fib 10)
    55"
  (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))

(observe! fib 100
  (fn (log)
    (let ((total-ms (foldl + 0 (map #(get % :time-ms) log)))
          (unique-args (length (dedup (map #(get % :args) log)))))
      (println f"100 calls: ${total-ms}ms total, ${unique-args} unique inputs"))))

;; Now use fib normally — after 100 calls, the callback fires and fib is restored.
```

### `(become! sym new-definition)`

Replace a function's definition at runtime. If the function has doctests, the candidate must pass all of them or the replacement is rejected. Saves the old version to history.

```sema
;; Upgrade fib to a memoized version
(become! fib
  (let ((cache {}))
    (fn (n)
      (or (get cache n)
          (let ((v (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))
            (set! cache (assoc cache n v))
            v)))))
;; Doctests run automatically — if they fail, fib is unchanged.
```

::: danger
`become!` is powerful but dangerous. Always ensure functions have doctests before using it in production. The doctest gate is your safety net.
:::

### `(history sym)`

Return the version history of a binding as a list of `{:version n :value <old-fn> :source "..."}` maps. Each `become!` and `rollback!` creates a history entry.

```sema
(history fib)
; => ({:version 1 :value <lambda> :source "(fn (n) ...)"})
```

### `(rollback! sym version)`

Restore a previous version from history. Creates a new history entry for the current version before rolling back.

```sema
(rollback! fib 1)   ; restore the original implementation
(fib 10)            ; => 55 (works, but slow again)
```

### `(freeze! sym)`

Permanently prevent further `become!` or `rollback!` on a function. Use this to stabilize proven implementations.

```sema
(freeze! fib)
(become! fib (fn (n) 0))  ; => Error: 'fib' is frozen and cannot be modified
```

::: tip
The safety workflow: `observe!` to understand behavior, `become!` to upgrade (gated by doctests), `history` to inspect changes, `rollback!` if something goes wrong, `freeze!` once stable.
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

## Putting It Together

### Scenario 1: Self-Healing Parser

A date parser that handles ISO format but breaks on other inputs. Define the full specification as doctests, then let the system heal itself:

```sema
(defn parse-date "Parse date strings into ISO format.
    >>> (parse-date \"2024-01-15\")
    \"2024-01-15\"
    >>> (parse-date \"Jan 15, 2024\")
    \"2024-01-15\"
    >>> (parse-date \"15/01/2024\")
    \"2024-01-15\""
  (s) s)

;; Check what fails
(doctest parse-date)     ; => {:passed 1 :total 3 :name "parse-date"}

;; Auto-repair
(heal! parse-date)

;; Verify
(doctest parse-date)     ; => {:passed 3 :total 3 :name "parse-date"}

;; Inspect what changed
(source-of parse-date)

;; Lock it down
(freeze! parse-date)
```

### Scenario 2: Adaptive Memoization

A function that's expensive for repeated calls. Use `observe!` to collect usage data, then let the LLM decide how to optimize:

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
        ;; Many repeated calls — ask for memoization
        (let ((suggestion (ask/code compute "Add memoization")))
          (println f"Suggested:\n${suggestion}"))))))

;; ... use compute normally ...
;; After 50 calls, the callback fires with the analysis
```

### Scenario 3: Evolving for Performance

Use `evolve` to find the fastest implementation of a function, then benchmark and freeze the winner:

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

1. **Doctests are contracts.** Every function that participates in `become!`, `heal!`, or `evolve` should have doctests. They are the safety net that prevents regressions.

2. **Keep targets small.** Don't `ask` or `heal!` an entire module. Target individual functions with clear specifications.

3. **Use history and rollback.** Before `become!`, know that you can always `(rollback! sym 1)` to get back. Check `(history sym)` to see what changed.

4. **Freeze stable code.** Once a function is battle-tested, `(freeze! sym)` prevents accidental modification.

5. **Start with `ask` before `ask/code`.** Use `ask` to understand the problem, `ask/code` to generate a solution, and `become!` to apply it -- not all at once.

6. **Use `observe!` before optimizing.** Measure before you modify. Let the data tell you what to optimize.

## Related

- [Completion & Chat](./completion.md) -- LLM provider configuration needed for `ask`, `heal!`, and `evolve`
- [Tools & Agents](./tools-agents.md) -- structured LLM interactions
- [Special Forms](../language/special-forms.md) -- full language reference
