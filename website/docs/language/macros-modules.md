---
outline: [2, 3]
---

# Macros & Modules

## Macros

Sema supports `defmacro`-style macros with quasiquoting, unquoting, and splicing.

### `defmacro`

Define a macro that transforms code at expansion time.

```sema
(defmacro unless2 (test . body)
  `(if ,test nil (begin ,@body)))

(unless2 #f (println "runs!"))
```

### `macroexpand`

Inspect the expansion of a macro call without evaluating it.

```sema
(macroexpand '(unless2 #f (println "x")))
```

### `gensym`

Generate a unique symbol for hygienic-ish macro writing.

```sema
(gensym "tmp")   ; => tmp_42 (unique each call)
```

### Built-in Macros

Sema includes several macros that are auto-loaded at startup. These don't need to be defined or imported:

- `->`, `->>`, `as->`, `some->` — [Threading macros](./special-forms.html#thread-first)
- `when-let`, `if-let` — [Conditional binding](./special-forms.html#when-let)

See [Special Forms](./special-forms.html) for full documentation.

## Metaprogramming

### `eval`

Evaluate data as code.

```sema
(eval '(+ 1 2))   ; => 3
```

### `read`

Parse a string into a Sema value.

```sema
(read "(+ 1 2)")   ; => (+ 1 2) as a list value
```

### `read-many`

Parse a string containing multiple forms.

```sema
(read-many "(+ 1 2) (* 3 4)")   ; => ((+ 1 2) (* 3 4))
```

### `type`

Return the type of a value as a string.

```sema
(type 42)              ; => "integer"
(type "hi")            ; => "string"
(type :foo)            ; => "keyword"
```

### Type Conversion Functions

```sema
(string->symbol "foo")       ; => foo
(keyword->string :bar)       ; => "bar"
(string->keyword "name")     ; => :name
(symbol->string 'foo)        ; => "foo"
```

## Modules

### `module`

Define a module with explicit exports.

```sema
;; math-utils.sema
(module math-utils
  (export square cube)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (define (internal-helper x) x))      ; not exported
```

### `import`

Import a module from a file. Only exported bindings become available.

```sema
;; main.sema
(import "math-utils.sema")
(square 5)   ; => 25
(cube 3)     ; => 27
```
