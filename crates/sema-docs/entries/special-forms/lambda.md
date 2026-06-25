---
name: "lambda"
module: "special-forms"
syntax: "(lambda (params ...) body ...)"
---

Create an anonymous function. `lambda` takes a parameter list and one or more body expressions, returning a callable function value that closes over the environment where it was defined.

The parameter list may be written as a list `(x y)` or a vector `[x y]` of symbols — both bind positional arguments. Rest arguments use dot notation: `(lambda (x . rest) rest)` binds `rest` to a list of the remaining arguments. A destructuring pattern in a parameter position (e.g. `([a b])` or `({:keys [k]})`) is desugared into a `let*` inside the body, so it pulls apart the single argument passed in that slot. A bare symbol in place of the whole parameter list (`(lambda args ...)`) is **not** supported and raises a type error — use `(lambda (. args) ...)`-style rest binding via `(x . rest)` instead.

The alias `fn` is accepted as an alternative spelling (Clojure-style). Both are handled identically by the evaluator.

```sema
((lambda (x y) (+ x y)) 3 4)  ; => 7
```

```sema
(define square (lambda (x) (* x x)))
(square 5)  ; => 25
```

```sema
((lambda (x . rest) rest) 1 2 3)  ; => (2 3)
```

```sema
((lambda [a b] (+ a b)) 1 2)     ; => 3  ; [a b] is a 2-param list, not destructuring
```

```sema
((lambda ([a b]) (+ a b)) '(1 2)) ; => 3  ; destructure ONE list argument
```
