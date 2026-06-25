---
name: "defn"
module: "special-forms"
syntax: "(defn name (params ...) body ...)"
---

Define a named function. `defn` is an alias for `defun` and behaves identically. It expands into `(define (name params ...) body ...)`, taking a name symbol, a parameter list, and one or more body expressions.

Parameter lists are plain symbols, with optional rest arguments via dot notation `(x . rest)`. The last body expression is evaluated in tail position. `defn` always returns `nil`.

Use whichever spelling matches your preference or codebase conventions. The evaluator recognizes both `defun` and `defn` as the same special form.

```sema
(defn add (a b)
  (+ a b))
(add 3 4)  ; => 7
```

Variadic functions collect trailing arguments into a rest list:

```sema
(defn greet-many (greeting . names)
  (map (fn (n) (string-append greeting " " n)) names))
(greet-many "Hi" "Ada" "Bob")  ; => ("Hi Ada" "Hi Bob")
```

Parameters cannot themselves be destructuring patterns (unlike `let` or `fn`'s in-list patterns) — destructure in the body instead:

```sema
(defn sum-pair (pair)
  (let (([a b] pair))
    (+ a b)))
(sum-pair '(3 4))  ; => 7
```
