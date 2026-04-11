---
outline: [2, 3]
---

# Typed Arrays

Typed arrays provide contiguous, unboxed numeric storage for performance-critical workloads. Unlike regular lists (which NaN-box every element), typed arrays store raw `f64` or `i64` values in a flat `Vec`, giving better cache locality and avoiding per-element boxing overhead.

Two types are available:
- **`f64-array`** — 64-bit floating-point arrays
- **`i64-array`** — 64-bit signed integer arrays

Both support copy-on-write mutation via `Rc::make_mut`.

## Construction

### `f64-array`

Create an f64 array from values.

```sema
(f64-array 1.0 2.5 3.7)  ; => #f64(1.0 2.5 3.7)
(f64-array)               ; => #f64()
```

### `i64-array`

Create an i64 array from values.

```sema
(i64-array 1 2 3)  ; => #i64(1 2 3)
(i64-array)        ; => #i64()
```

### `f64-array/make`

Create an f64 array of a given length, optionally filled with a value (default `0.0`).

```sema
(f64-array/make 5)       ; => #f64(0.0 0.0 0.0 0.0 0.0)
(f64-array/make 3 1.5)   ; => #f64(1.5 1.5 1.5)
```

### `i64-array/make`

Create an i64 array of a given length, optionally filled with a value (default `0`).

```sema
(i64-array/make 5)     ; => #i64(0 0 0 0 0)
(i64-array/make 3 42)  ; => #i64(42 42 42)
```

### `f64-array/range`

Create an f64 array from a numeric range. `(f64-array/range start end)` or `(f64-array/range start end step)`.

```sema
(f64-array/range 0 5)        ; => #f64(0.0 1.0 2.0 3.0 4.0)
(f64-array/range 0 1 0.25)   ; => #f64(0.0 0.25 0.5 0.75)
```

### `i64-array/range`

Create an i64 array from an integer range.

```sema
(i64-array/range 0 5)      ; => #i64(0 1 2 3 4)
(i64-array/range 0 10 2)   ; => #i64(0 2 4 6 8)
```

### `f64-array/from-list`

Convert a list of numbers to an f64 array.

```sema
(f64-array/from-list '(1 2 3))  ; => #f64(1.0 2.0 3.0)
```

### `i64-array/from-list`

Convert a list of integers to an i64 array.

```sema
(i64-array/from-list '(10 20 30))  ; => #i64(10 20 30)
```

## Access & Mutation

### `f64-array/ref` / `i64-array/ref`

Get the element at a given index.

```sema
(f64-array/ref (f64-array 1.0 2.0 3.0) 1)  ; => 2.0
(i64-array/ref (i64-array 10 20 30) 0)      ; => 10
```

### `f64-array/set!` / `i64-array/set!`

Set the element at a given index. Uses copy-on-write -- the original array is unchanged unless it has a single reference.

```sema
(f64-array/set! (f64-array 1.0 2.0 3.0) 1 9.9)  ; => #f64(1.0 9.9 3.0)
(i64-array/set! (i64-array 10 20 30) 2 99)       ; => #i64(10 20 99)
```

### `f64-array/length` / `i64-array/length`

Return the number of elements.

```sema
(f64-array/length (f64-array 1.0 2.0 3.0))  ; => 3
(i64-array/length (i64-array/make 10))       ; => 10
```

## Aggregation

### `f64-array/sum` / `i64-array/sum`

Sum all elements. Runs in a tight Rust loop with no boxing overhead.

```sema
(f64-array/sum (f64-array 1.0 2.0 3.0))  ; => 6.0
(i64-array/sum (i64-array 1 2 3 4 5))    ; => 15
```

### `f64-array/dot`

Compute the dot product of two f64 arrays (must be the same length).

```sema
(f64-array/dot (f64-array 1.0 2.0 3.0) (f64-array 4.0 5.0 6.0))
; => 32.0  (1*4 + 2*5 + 3*6)
```

## Higher-Order Functions

### `f64-array/map` / `i64-array/map`

Apply a function to each element, returning a new typed array. The callback must return the matching numeric type.

```sema
(f64-array/map (lambda (x) (* x 2.0)) (f64-array 1.0 2.0 3.0))
; => #f64(2.0 4.0 6.0)

(i64-array/map (lambda (x) (* x x)) (i64-array 1 2 3 4))
; => #i64(1 4 9 16)
```

### `f64-array/fold` / `i64-array/fold`

Fold over a typed array with an accumulator.

```sema
(f64-array/fold (lambda (acc x) (+ acc x)) 0.0 (f64-array 1.0 2.0 3.0))
; => 6.0

(i64-array/fold (lambda (acc x) (max acc x)) 0 (i64-array 3 1 4 1 5))
; => 5
```

## Type Predicates

### `f64-array?` / `i64-array?`

Test whether a value is a typed array.

```sema
(f64-array? (f64-array 1.0 2.0))  ; => #t
(f64-array? '(1.0 2.0))           ; => #f
(i64-array? (i64-array 1 2))      ; => #t
```

## Examples

### Embedding similarity with dot product

```sema
;; Compute cosine similarity between two embedding vectors
(define (cosine-similarity a b)
  (let ((dot (f64-array/dot a b))
        (mag-a (sqrt (f64-array/dot a a)))
        (mag-b (sqrt (f64-array/dot b b))))
    (/ dot (* mag-a mag-b))))

(define v1 (f64-array 1.0 0.0 0.0))
(define v2 (f64-array 0.707 0.707 0.0))
(cosine-similarity v1 v2)
; => ~0.707
```

### Numeric computation

```sema
;; Sum of squares of even numbers from 0 to 99
(define nums (i64-array/range 0 100))
(define evens (i64-array/map (lambda (x) (if (even? x) (* x x) 0)) nums))
(i64-array/sum evens)
; => 161700
```
