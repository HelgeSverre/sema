---
outline: [2, 3]
---

# Special Forms

Special forms are built into the evaluator — they control evaluation order and cannot be redefined.

## Definitions & Assignment

### `define`

Bind a value or define a function.

```scheme
(define x 42)                          ; bind a value
(define (square x) (* x x))           ; define a function (shorthand)
```

### `set!`

Mutate an existing binding.

```scheme
(set! x 99)
```

## Functions

### `lambda`

Create an anonymous function.

```scheme
(lambda (x y) (+ x y))
```

### `fn`

Alias for `lambda`.

```scheme
(fn (x) (* x x))
(fn (x . rest) rest)                   ; rest parameters with dot notation
```

## Conditionals

### `if`

Two-branch conditional.

```scheme
(if (> x 0) "positive" "non-positive")
```

### `cond`

Multi-branch conditional with `else` fallback.

```scheme
(cond
  ((< x 0) "negative")
  ((= x 0) "zero")
  (else "positive"))
```

### `case`

Match a value against literal alternatives.

```scheme
(case (:status response)
  ((:ok) "success")
  ((:error :timeout) "failure")
  (else "unknown"))
```

### `when`

Execute body only if condition is true. Returns `nil` otherwise.

```scheme
(when (> x 0) (println "positive"))
```

### `unless`

Execute body only if condition is false.

```scheme
(unless (> x 0) (println "not positive"))
```

## Bindings

### `let`

Parallel bindings — all init expressions are evaluated before any binding is created.

```scheme
(let ((x 10) (y 20))
  (+ x y))
```

### `let*`

Sequential bindings — each binding is visible to subsequent ones.

```scheme
(let* ((x 10) (y (* x 2)))
  (+ x y))
```

### `letrec`

Recursive bindings — all bindings are visible to all init expressions. Useful for mutually recursive functions.

```scheme
(letrec ((even? (fn (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd?  (fn (n) (if (= n 0) #f (even? (- n 1))))))
  (even? 10))
```

### Named `let`

Loop construct with tail-call optimization.

```scheme
(let loop ((i 0) (sum 0))
  (if (= i 100)
    sum
    (loop (+ i 1) (+ sum i))))
```

## Sequencing & Logic

### `begin`

Evaluate expressions in order, return the last result.

```scheme
(begin expr1 expr2 ... exprN)
```

### `and`

Short-circuit logical AND. Returns the last truthy value or `#f`.

```scheme
(and a b c)
```

### `or`

Short-circuit logical OR. Returns the first truthy value or `#f`.

```scheme
(or a b c)
```

## Iteration

### `do`

Scheme `do` loop with variable bindings, step expressions, and a termination test.

```scheme
;; (do ((var init step) ...) (test result ...) body ...)
(do ((i 0 (+ i 1))
     (sum 0 (+ sum i)))
    ((= i 10) sum))                    ; => 45
```

With a body for side effects:

```scheme
(do ((i 0 (+ i 1)))
    ((= i 5))
  (println i))                         ; prints 0..4
```

## Lazy Evaluation

### `delay`

Create a promise — the expression is not evaluated until forced.

```scheme
(define p (delay (+ 1 2)))
```

### `force`

Evaluate a promise and memoize the result. Non-promise values pass through.

```scheme
(force p)                              ; => 3 (evaluate and memoize)
(force p)                              ; => 3 (returns cached value)
(force 42)                             ; => 42 (non-promise passes through)
```

### `promise?`

Check if a value is a promise.

```scheme
(promise? p)                           ; => #t
```

### `promise-forced?`

Check if a promise has already been forced.

```scheme
(promise-forced? p)                    ; => #t (after forcing)
```

## Record Types

### `define-record-type`

Define a record type with constructor, predicate, and field accessors.

```scheme
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

```scheme
(try
  (/ 1 0)
  (catch e
    (println (format "Error: ~a" (:message e)))
    (:type e)))        ; => :eval
```

Error maps in `catch` have keys: `:type`, `:message`, `:stack-trace`, and variant-specific keys (`:value`, `:expected`/`:got`, `:name`).

### `throw`

Throw any value as an error.

```scheme
(throw "something went wrong")
(throw {:code 404 :reason "not found"})
```
