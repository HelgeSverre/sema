---
outline: [2, 3]
---

# Metaprogramming

Sema treats code as data. Every function carries metadata -- its name, parameters, docstring, source text, and version history. These introspection and runtime modification primitives let you build documentation engines, test runners, profilers, and hot-swapping systems entirely within the language.

None of these features require an LLM. For LLM-powered code intelligence (`ask`, `heal!`, `evolve`), see [Living Code](../llm/living-code.md).

## Docstrings & Doctests

### `defn` with docstrings

`defn` accepts an optional docstring as its second argument (before params). The docstring is stored as metadata and accessible via `doc`, `meta`, and `doctest`.

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
- `!! substring` expects an error containing that substring
- `>>>!` evaluates but doesn't check the result (setup step)

### `(doc sym)`

Pretty-print documentation for a symbol. Shows name, parameters, docstring, and source.

```sema
(doc factorial)
;; factorial (n)
;;   Compute n!.
;;   >>> (factorial 0)
;;   1
;;   ...
```

### `(doc/search query)`

Search all defined symbols by name or docstring content. Returns a list of `{:name ... :doc ...}` maps.

```sema
(doc/search "sort")       ; find all functions mentioning "sort"
(doc/search "deprecated") ; find deprecated functions
```

### `(doctest sym)`

Run all doctests from a symbol's docstring. Returns `{:passed n :total n :name "..."}`.

```sema
(doctest factorial)    ; => {:name "factorial" :passed 2 :total 2}
```

Failed tests print details to stderr:

```
  ✓ (factorial 0) => 1
  ✗ (factorial 5) => expected 120, got 0
```

### Testing from the CLI

```bash
sema test src/**/*.sema       # run all doctests in matching files
```

::: tip
Treat docstrings as executable specifications. They serve triple duty: documentation for humans, test cases for `doctest`, and context for LLM features like `ask` and `heal!`.
:::

## Metadata & Introspection

### `(meta sym)`

Return all metadata for a symbol as a map. Includes `:name`, `:type`, `:params`, `:arity`, `:doc`, `:source`, `:variadic?`, and any custom metadata set by directives or `become!`.

```sema
(meta factorial)
; => {:arity 1 :doc "Compute n!. ..." :name "factorial" :params (n) :type :lambda}

(meta map)
; => {:name "map" :type :native-fn}
```

### `(source-of sym)`

Return the source text of a definition as a string. Returns `nil` for native functions.

```sema
(source-of factorial)
; => "(defn factorial \"Compute n!. ...\" (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"
```

### Reader directives: `;;@key value`

Attach structured metadata to definitions using directive comments. Place them on the line before a `defn`, `define`, or `defmacro`.

```sema
;;@since 1.5
;;@stability stable
;;@deprecated "Use parse-datetime instead"
(defn parse-date "Parse a date string." (s) ...)
```

Directives are captured by the reader and available through `read-source`.

### `(read-source path)`

Read and parse source files, returning structured data for each top-level definition. Supports glob patterns.

```sema
(read-source "src/utils.sema")
; => list of {:type :defn :name "..." :doc "..." :params (...) :body (...) :source "..."}

(read-source "src/**/*.sema")  ; glob across directories
```

::: tip Codebase tooling
Combine `read-source` with higher-order functions to build custom tooling without leaving Sema:

```sema
;; Find all deprecated functions
(->> (read-source "src/**/*.sema")
     (filter #(get-in % [:directives :deprecated]))
     (map #(println f"deprecated: ${(:name %)} -- ${(get-in % [:directives :deprecated])}")))

;; Generate a function index
(->> (read-source "src/**/*.sema")
     (filter #(= (:type %) :defn))
     (map #(println f"  ${(:name %)} -- ${(or (:doc %) \"(no doc)\")}")))
```
:::

## Runtime Self-Modification

These forms let programs change their own behavior at runtime, with built-in safety through doctests, version history, and freezing.

::: warning
Runtime self-modification is powerful but requires discipline. Always use doctests as contracts, keep history for rollback, and freeze stable code.
:::

### `(observe! sym sample-size callback)`

Wrap a function with an invisible logger. After `sample-size` calls, the original function is restored and `callback` is invoked with the call log -- a list of `{:args ... :result ... :time-ms ...}` maps.

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

;; Use fib normally -- after 100 calls, the callback fires and fib is restored.
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
;; Doctests run automatically -- if they fail, fib is unchanged.
```

::: danger
`become!` modifies live program state. Always ensure functions have doctests before using it. The doctest gate is your safety net -- without it, any replacement is accepted unconditionally.
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

::: tip The safety lifecycle
`observe!` to understand behavior, `become!` to upgrade (gated by doctests), `history` to inspect changes, `rollback!` if something goes wrong, `freeze!` once stable.
:::

## Benchmarking

### `(bench thunk iterations)`

Run a zero-argument function `iterations` times and return timing statistics.

```sema
(bench (fn () (fib 20)) 1000)
; => {:iterations 1000 :total-ms 342.5 :mean-ms 0.3425}
```

## Scenarios

### Hot-Swapping in a Running Server

Replace a handler's implementation without restarting the server:

```sema
(defn handle-request "Handle API requests.
    >>> (handle-request {:path \"/health\"})
    {:status 200 :body \"ok\"}"
  (req)
  (cond
    ((= (:path req) "/health") {:status 200 :body "ok"})
    (else {:status 404 :body "not found"})))

;; Later, upgrade the handler with new routes
(become! handle-request
  (fn (req)
    (cond
      ((= (:path req) "/health") {:status 200 :body "ok"})
      ((= (:path req) "/version") {:status 200 :body "1.1"})
      (else {:status 404 :body "not found"}))))
;; Doctests verify /health still works before the swap takes effect.
```

### Profiling with observe!

Find which inputs are most common and how long they take:

```sema
(observe! my-parser 500
  (fn (log)
    (let ((by-input (group-by #(get % :args) log)))
      (->> by-input
           (map (fn ([args calls])
             {:input args
              :count (length calls)
              :avg-ms (/ (foldl + 0 (map #(get % :time-ms) calls))
                         (length calls))}))
           (sort-by :count >)
           (take 10)
           (for-each println)))))
```

### Building a Documentation Generator

Generate a stdlib reference from source files:

```sema
(defn generate-docs (glob-pattern)
  (let ((defs (read-source glob-pattern)))
    (for-each
      (fn (d)
        (when (= (:type d) :defn)
          (println f"### `${(:name d)}`\n")
          (when (:doc d)
            (println (:doc d))
            (println))))
      defs)))

(generate-docs "src/stdlib/**/*.sema")
```

## Related

- [Special Forms](./special-forms.md) -- full language reference for `defn`, `define`, etc.
- [Macros & Modules](./macros-modules.md) -- compile-time metaprogramming with `defmacro`
- [Living Code](../llm/living-code.md) -- LLM-powered features that build on these primitives (`ask`, `heal!`, `evolve`)
