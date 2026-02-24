---
outline: [2, 3]
---

# Vectors

Vectors are **indexed, immutable** collections written with square-bracket syntax. They're ideal when you want **O(1) indexed access** and a compact literal form.

Most list functions also accept vectors, but some return lists even when passed a vector — see [Vectors vs Lists](#vectors-vs-lists) for details.

## Literal Syntax

```sema
[1 2 3]              ; vector of integers
["a" "b" "c"]        ; vector of strings
[]                   ; empty vector
[1 [2 3] 4]          ; nested vectors
```

## Construction

### `vector`

Create a vector from its arguments.

```sema
(vector 1 2 3)       ; => [1 2 3]
(vector)             ; => []
(vector "a" "b")     ; => ["a" "b"]
```

## Predicates & Introspection

### `vector?`

Test whether a value is a vector.

```sema
(vector? [1 2 3])          ; => #t
(vector? '(1 2 3))         ; => #f
(vector? 42)               ; => #f
```

### `length` / `count` / `empty?`

Vectors participate in Sema's generic collection functions:

```sema
(length [10 20 30])   ; => 3
(count [10 20 30])    ; => 3
(empty? [])           ; => #t
(empty? [1])          ; => #f
```

## Indexed Access

### `nth`

Return the element at index `n` (zero-based). Works on both lists and vectors.

```sema
(nth [10 20 30] 0)    ; => 10
(nth [10 20 30] 2)    ; => 30
```

Out of bounds is an error:

```sema
(nth [10 20 30] 3)
; => error: index 3 out of bounds (length 3)
```

::: tip
Use `first` for safe "index 0" access — it returns `nil` on empty sequences.
:::

### `first`

Return the first element of a vector (or list). Returns `nil` for empty vectors.

```sema
(first [1 2 3])   ; => 1
(first [])        ; => nil
```

### `rest`

Return everything after the first element. **Preserves type** — vector in, vector out.

```sema
(rest [1 2 3])    ; => [2 3]
(rest [])         ; => []
(rest [1])        ; => []
```

## Conversion

### `vector->list`

Convert a vector to a list.

```sema
(vector->list [1 2 3])   ; => (1 2 3)
(vector->list [])         ; => ()
```

### `list->vector`

Convert a list to a vector.

```sema
(list->vector '(1 2 3))   ; => [1 2 3]
(list->vector '())         ; => []
```

## Vectors vs Lists

Both lists and vectors work as "sequences", but they're optimized for different things.

### When to use a vector

- You need **fast indexed access** (`nth` is O(1) on vectors)
- You have a **fixed-size** structure (e.g. `[x y]`, `[start end]`, `[status body]`)
- You want compact literals for configuration-style data

### When to use a list

- You're building data incrementally with `cons`, `append`, or recursion
- You expect to process data head+tail style
- You want idiomatic Lisp code-as-data

### Return type behavior

Many sequence functions accept vectors but return lists:

| Function | Vector in → | Type preserved? |
|----------|-------------|-----------------|
| `rest` | vector out | ✅ Yes |
| `reverse` | vector out | ✅ Yes |
| `map` | list out | ❌ No |
| `filter` | list out | ❌ No |
| `append` | list out | ❌ No |

```sema
(map #(* % 2) [1 2 3])     ; => (2 4 6)    — list!
(reverse [1 2 3])           ; => [3 2 1]    — vector
(append [1 2] [3 4])        ; => (1 2 3 4)  — list!
```

If you need a vector result after a transformation, convert at the end:

```sema
(->> [1 2 3]
     (map #(* % 2))
     (list->vector))
; => [2 4 6]
```

## Destructuring

Sema supports **sequential destructuring** with a vector pattern in `let`, `define`, and function parameters. This works on both list and vector values.

### Exact destructuring

```sema
(let (([x y] [10 20]))
  (+ x y))
; => 30

(let (([x y] '(10 20)))
  (+ x y))
; => 30
```

### Rest destructuring with `&`

```sema
(let (([head second & tail] [1 2 3 4 5]))
  [head second tail])
; => [1 2 (3 4 5)]
```

Note: `tail` is a **list**, not a vector.

### Nested destructuring

```sema
(let (([a [b c] d] [1 [2 3] 4]))
  (+ a b c d))
; => 10
```

## Pattern Matching

`match` supports vector patterns:

```sema
(define (describe-point p)
  (match p
    ([0 0] "origin")
    ([x 0] (string/append "on x-axis at " (number/to-string x)))
    ([0 y] (string/append "on y-axis at " (number/to-string y)))
    ([x y] (string/append "point " (number/to-string x) ", " (number/to-string y)))))

(describe-point [0 0])     ; => "origin"
(describe-point '(5 0))    ; => "on x-axis at 5"
```

## Practical Examples

### Tuple-style returns

Vectors are great for fixed-arity return values:

```sema
(define (min-max xs)
  [(list/min xs) (list/max xs)])

(min-max '(3 1 4 1 5))
; => [1 5]
```

### Chunking and re-vectorizing

```sema
(->> (range 10)
     (list/chunk 3)
     (map list->vector))
; => ([0 1 2] [3 4 5] [6 7 8] [9])
```

## Performance

| Operation | Complexity |
|-----------|-----------|
| `nth` | O(1) |
| `first` | O(1) |
| `rest` | O(n) — creates a new vector |
| `length` | O(1) |

Vectors are **immutable** — there is no `vector-set!`. To "update" a vector, construct a new one.
