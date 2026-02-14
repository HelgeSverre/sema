---
outline: [2, 3]
---

# Vectors

Vectors are fixed-size, indexed collections written with bracket syntax. Most list functions also work on vectors.

## Literal Syntax

Vectors use square brackets:

```scheme
[1 2 3]              ; a vector of three integers
["a" "b" "c"]        ; a vector of strings
[]                   ; empty vector
[1 [2 3] 4]          ; nested vectors
```

## Construction

### `vector`

Create a vector from arguments.

```scheme
(vector 1 2 3)       ; => [1 2 3]
(vector)             ; => []
(vector "a" "b")     ; => ["a" "b"]
```

## Conversion

### `vector->list`

Convert a vector to a list.

```scheme
(vector->list [1 2 3])   ; => (1 2 3)
(vector->list [])         ; => ()
```

### `list->vector`

Convert a list to a vector.

```scheme
(list->vector '(1 2 3))   ; => [1 2 3]
(list->vector '())         ; => []
```

## Using List Functions with Vectors

Most list functions work seamlessly with vectors:

```scheme
(map (fn (x) (* x x)) [1 2 3])     ; => (1 4 9)
(filter even? [1 2 3 4 5])          ; => (2 4)
(length [1 2 3])                    ; => 3
(nth [10 20 30] 1)                  ; => 20
(first [1 2 3])                     ; => 1
(rest [1 2 3])                      ; => (2 3)
(reverse [1 2 3])                   ; => (3 2 1)
```
