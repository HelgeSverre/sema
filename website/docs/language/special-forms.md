---
outline: [2, 3]
---

# Special Forms

Special forms are built into the evaluator — they control evaluation order and cannot be redefined.

## Definitions & Assignment

### `define`

Bind a value or define a function.

```sema
(define x 42)                          ; bind a value
(define (square x) (* x x))           ; define a function (shorthand)
```

### `set!`

Mutate an existing binding.

```sema
(set! x 99)
```

## Functions

### `lambda`

Create an anonymous function.

```sema
(lambda (x y) (+ x y))
```

### `fn`

Alias for `lambda`.

```sema
(fn (x) (* x x))
(fn (x . rest) rest)                   ; rest parameters with dot notation
```

### `defun`

Define a named function (equivalent to `(define (name params...) body...)`).

```sema
(defun square (x) (* x x))
(defun greet (name) f"Hello, ${name}!")
```

::: tip Clojure alias
`defn` is accepted as an alias for `defun`.
:::

## Conditionals

### `if`

Two-branch conditional.

```sema
(if (> x 0) "positive" "non-positive")
```

### `cond`

Multi-branch conditional with `else` fallback.

```sema
(cond
  ((< x 0) "negative")
  ((= x 0) "zero")
  (else "positive"))
```

### `case`

Match a value against literal alternatives.

```sema
(case (:status response)
  ((:ok) "success")
  ((:error :timeout) "failure")
  (else "unknown"))
```

### `when`

Execute body only if condition is true. Returns `nil` otherwise.

```sema
(when (> x 0) (println "positive"))
```

### `unless`

Execute body only if condition is false.

```sema
(unless (> x 0) (println "not positive"))
```

## Threading Macros

Built-in macros for pipeline-style code. Available automatically — no import needed.

### `->`

Thread-first: inserts the value as the first argument of each form.

```sema
(-> 5 (+ 3) (* 2))                    ; => 16
(-> response :body json/decode :data)  ; nested access
```

### `->>`

Thread-last: inserts the value as the last argument of each form.

```sema
(->> (range 1 100)
     (filter even?)
     (map #(* % %))
     (take 5))                         ; => (4 16 36 64 100)
```

### `as->`

Thread-as: bind the threaded value to a name for arbitrary placement.

```sema
(as-> 5 x (+ x 3) (* x x) (- x 1))   ; => 63
```

### `some->`

Nil-safe thread-first: stops and returns `nil` if any step produces `nil`.

```sema
(some-> config :database :connection-string db/connect)
;; returns nil if any step is nil, instead of crashing
```

## Conditional Binding

### `when-let`

Bind a value and execute body only if non-nil.

```sema
(when-let (user (db/find-user id))
  (send-email user "Welcome back"))
```

### `if-let`

Bind a value and branch on nil/non-nil.

```sema
(if-let (cached (cache/get key))
  cached
  (compute-fresh-value))
```

## Short Lambda

### `#(...)`

Concise anonymous functions. `%` (or `%1`) is the first argument, `%2` the second, etc.

```sema
(map #(+ % 1) '(1 2 3))               ; => (2 3 4)
(map #(* % %) '(1 2 3 4))             ; => (1 4 9 16)
(filter #(> % 3) '(1 2 3 4 5))        ; => (4 5)
(#(+ %1 %2) 3 4)                      ; => 7
```

## Bindings

### `let`

Parallel bindings — all init expressions are evaluated before any binding is created.

```sema
(let ((x 10) (y 20))
  (+ x y))
```

### `let*`

Sequential bindings — each binding is visible to subsequent ones.

```sema
(let* ((x 10) (y (* x 2)))
  (+ x y))
```

### `letrec`

Recursive bindings — all bindings are visible to all init expressions. Useful for mutually recursive functions.

```sema
(letrec ((even? (fn (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd?  (fn (n) (if (= n 0) #f (even? (- n 1))))))
  (even? 10))
```

### Named `let`

Loop construct with tail-call optimization.

```sema
(let loop ((i 0) (sum 0))
  (if (= i 100)
    sum
    (loop (+ i 1) (+ sum i))))
```

## Destructuring

`let`, `let*`, `define`, and `lambda` all support destructuring patterns in binding positions.

### Vector Destructuring

Extract elements from lists and vectors by position.

```sema
(let (([a b c] '(1 2 3)))
  (+ a b c))                           ; => 6

(let (([first & rest] '(1 2 3 4)))
  rest)                                 ; => (2 3 4)

(let (([_ second] '(1 2)))
  second)                               ; => 2
```

### Map Destructuring

Extract values from maps using `{:keys [...]}`.

```sema
(let (({:keys [name age]} {:name "Alice" :age 30}))
  (println name))                       ; prints "Alice"
```

Explicit key-pattern pairs:

```sema
(let (({:x val} {:x 42}))
  val)                                  ; => 42
```

### Destructuring in `define`

```sema
(define [a b c] '(1 2 3))              ; binds a=1, b=2, c=3
(define {:keys [host port]} config)     ; binds host, port from map
```

### Destructuring in Function Parameters

```sema
(define (sum-pair [a b])
  (+ a b))
(sum-pair '(3 4))                       ; => 7

(define (greet {:keys [name title]})
  (format "Hello ~a ~a" title name))
(greet {:name "Smith" :title "Dr."})    ; => "Hello Dr. Smith"
```

Nested patterns are supported:

```sema
(let (([[a b] c] '((1 2) 3)))
  (+ a b c))                           ; => 6
```

## Pattern Matching

### `match`

Match a value against patterns with optional guards. Returns `nil` if no clause matches.

```sema
(match value
  (pattern body ...)
  (pattern when guard body ...)
  ...)
```

#### Literal Matching

```sema
(match status
  (:ok "success")
  (:error "failure")
  (_ "unknown"))
```

#### Binding Patterns

Symbols bind the matched value. `_` is a wildcard.

```sema
(match (+ 1 2)
  (x (format "got ~a" x)))             ; => "got 3"
```

#### Vector Patterns

```sema
(match '(1 2 3)
  ([a b c] (+ a b c)))                 ; => 6

(match args
  ([] (print-help))
  ([cmd & rest] (dispatch cmd rest)))
```

#### Map Patterns

Structural matching — keys must exist in the value:

```sema
(match response
  ({:type :ok :data d}   (process d))
  ({:type :error :msg m} (log-error m))
  (_                     (println "unknown")))
```

With `{:keys [...]}` shorthand:

```sema
(match config
  ({:keys [host port]} (connect host port)))
```

#### Guards

Add `when` after a pattern for conditional matching:

```sema
(match n
  (x when (> x 100) "big")
  (x when (> x 0)   "small")
  (_                 "non-positive"))
```

#### Nested Patterns

```sema
(match '(1 (2 3))
  ([a [b c]] (+ a b c)))               ; => 6
```

## Sequencing & Logic

### `begin`

Evaluate expressions in order, return the last result.

```sema
(begin expr1 expr2 ... exprN)
```

::: tip Common Lisp alias
`progn` is accepted as an alias for `begin`.
:::

### `and`

Short-circuit logical AND. Returns the last truthy value or `#f`.

```sema
(and a b c)
```

### `or`

Short-circuit logical OR. Returns the first truthy value or `#f`.

```sema
(or a b c)
```

## Iteration

### `do`

Scheme `do` loop with variable bindings, step expressions, and a termination test.

```sema
;; (do ((var init step) ...) (test result ...) body ...)
(do ((i 0 (+ i 1))
     (sum 0 (+ sum i)))
    ((= i 10) sum))                    ; => 45
```

With a body for side effects:

```sema
(do ((i 0 (+ i 1)))
    ((= i 5))
  (println i))                         ; prints 0..4
```

## Lazy Evaluation

### `delay`

Create a promise — the expression is not evaluated until forced.

```sema
(define p (delay (+ 1 2)))
```

### `force`

Evaluate a promise and memoize the result. Non-promise values pass through.

```sema
(force p)                              ; => 3 (evaluate and memoize)
(force p)                              ; => 3 (returns cached value)
(force 42)                             ; => 42 (non-promise passes through)
```

### `promise?`

Check if a value is a promise.

```sema
(promise? p)                           ; => #t
```

### `promise-forced?`

Check if a promise has already been forced.

```sema
(promise-forced? p)                    ; => #t (after forcing)
```

## Record Types

### `define-record-type`

Define a record type with constructor, predicate, and field accessors.

```sema
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 3 4))
(point? p)                             ; => #t
(point-x p)                           ; => 3
(point-y p)                           ; => 4
(record? p)                           ; => #t
(type p)                              ; => :point
(equal? (make-point 1 2) (make-point 1 2))  ; => #t
```

## Error Handling

### `try` / `catch`

Catch errors with structured error maps.

```sema
(try
  (/ 1 0)
  (catch e
    (println (format "Error: ~a" (:message e)))
    (:type e)))        ; => :eval
```

::: warning
`try`/`catch` catches **all** error types — not just user exceptions thrown with `throw`. This includes internal errors like `:unbound` (typos in variable names), `:permission-denied`, and `:arity` (wrong number of arguments). Catching everything can silently mask bugs. **Re-throw errors you don't intend to handle.**
:::

#### Error map fields

Every caught error is a map with at least `:type`, `:message`, and `:stack-trace`. Some error types include additional fields:

| `:type` | Description | Extra fields |
|---|---|---|
| `:reader` | Syntax / parse error | — |
| `:eval` | General evaluation error | — |
| `:type-error` | Wrong argument type | `:expected`, `:got` |
| `:arity` | Wrong number of arguments | — |
| `:unbound` | Undefined variable | `:name` |
| `:llm` | LLM provider error | — |
| `:io` | File / network I/O error | — |
| `:permission-denied` | Sandboxed capability denied | `:function`, `:capability` |
| `:user` | Thrown with `throw` | `:value` (the original thrown value) |

#### Discriminating error types

Use the `:type` field to handle specific errors and re-throw the rest:

```sema
(try
  (some-operation)
  (catch e
    (cond
      ((= (get e :type) :permission-denied)
       (println "Access denied!"))
      ((= (get e :type) :user)
       (println (format "User error: ~a" (get e :message))))
      (else
       (throw e)))))  ;; re-throw unexpected errors
```

### `throw`

Throw any value as an error.

```sema
(throw "something went wrong")
(throw {:code 404 :reason "not found"})
```
