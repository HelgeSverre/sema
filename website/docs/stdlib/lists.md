---
outline: [2, 3]
---

# Lists

Lists are the fundamental data structure in Sema. They are built from cons pairs and support a rich set of operations.

## Construction & Access

### `list`

Create a new list.

```scheme
(list 1 2 3)       ; => (1 2 3)
(list)             ; => ()
(list "a" "b")     ; => ("a" "b")
```

### `cons`

Prepend an element to a list.

```scheme
(cons 0 '(1 2 3))  ; => (0 1 2 3)
(cons 1 '())       ; => (1)
```

### `car`

Return the first element of a list.

```scheme
(car '(1 2 3))     ; => 1
```

### `cdr`

Return the rest of a list (everything after the first element).

```scheme
(cdr '(1 2 3))     ; => (2 3)
(cdr '(1))         ; => ()
```

### `first`

Alias for `car`. Return the first element.

```scheme
(first '(1 2 3))   ; => 1
```

### `rest`

Alias for `cdr`. Return the rest of the list.

```scheme
(rest '(1 2 3))    ; => (2 3)
```

### `cadr`, `caddr`, ...

Compositions of `car` and `cdr`. Available: `caar`, `cadr`, `cdar`, `cddr`, `caaar`, `caadr`, `cadar`, `caddr`, `cdaar`, `cdadr`, `cddar`, `cdddr`.

```scheme
(cadr '(1 2 3))    ; => 2
(caddr '(1 2 3))   ; => 3
```

### `last`

Return the last element of a list.

```scheme
(last '(1 2 3))    ; => 3
```

### `nth`

Return the element at index N (zero-based).

```scheme
(nth '(10 20 30) 1)   ; => 20
(nth '(10 20 30) 0)   ; => 10
```

## Association Lists

### `assoc`

Look up a key in an association list (list of pairs). Uses `equal?` comparison.

```scheme
(define alist '(("a" 1) ("b" 2) ("c" 3)))
(assoc "b" alist)   ; => ("b" 2)
(assoc "z" alist)   ; => #f
```

### `assq`

Like `assoc` but uses `eq?` comparison (pointer/symbol equality).

```scheme
(assq 'b '((a 1) (b 2)))   ; => (b 2)
```

### `assv`

Like `assoc` but uses `eqv?` comparison (value equality for numbers).

```scheme
(assv 2 '((1 "one") (2 "two")))   ; => (2 "two")
```

## Basic Operations

### `length`

Return the number of elements in a list.

```scheme
(length '(1 2 3))  ; => 3
(length '())       ; => 0
```

### `append`

Concatenate lists.

```scheme
(append '(1 2) '(3 4))     ; => (1 2 3 4)
(append '(1) '(2) '(3))    ; => (1 2 3)
```

### `reverse`

Reverse a list.

```scheme
(reverse '(1 2 3))   ; => (3 2 1)
```

### `range`

Generate a list of integers. With one argument, generates 0 to N-1. With two, generates from start to end-1.

```scheme
(range 5)       ; => (0 1 2 3 4)
(range 1 5)     ; => (1 2 3 4)
```

## Higher-Order Functions

### `map`

Apply a function to each element of one or more lists.

```scheme
(map (fn (x) (* x x)) '(1 2 3))      ; => (1 4 9)
(map + '(1 2 3) '(10 20 30))          ; => (11 22 33)
```

### `filter`

Return elements that satisfy a predicate.

```scheme
(filter even? '(1 2 3 4 5))   ; => (2 4)
(filter string? '(1 "a" 2))   ; => ("a")
```

### `foldl`

Left fold. `(foldl f init list)` — accumulates from left to right.

```scheme
(foldl + 0 '(1 2 3 4 5))   ; => 15
(foldl cons '() '(1 2 3))  ; => (3 2 1)
```

### `foldr`

Right fold. `(foldr f init list)` — accumulates from right to left.

```scheme
(foldr cons '() '(1 2 3))   ; => (1 2 3)
```

### `reduce`

Like `foldl` but uses the first element as the initial value.

```scheme
(reduce + '(1 2 3 4 5))   ; => 15
```

### `for-each`

Apply a function to each element for side effects.

```scheme
(for-each println '("a" "b" "c"))
;; prints: a, b, c (each on a new line)
```

### `sort`

Sort a list in ascending order.

```scheme
(sort '(3 1 4 1 5))   ; => (1 1 3 4 5)
```

### `sort-by`

Sort a list by a key function.

```scheme
(sort-by length '("bb" "a" "ccc"))   ; => ("a" "bb" "ccc")
(sort-by abs '(-3 1 -2))             ; => (1 -2 -3)
```

### `flat-map`

Map a function over a list and flatten the results by one level.

```scheme
(flat-map (fn (x) (list x (* x 10))) '(1 2 3))
; => (1 10 2 20 3 30)
```

### `apply`

Apply a function to a list of arguments.

```scheme
(apply + '(1 2 3))   ; => 6
(apply max '(3 1 4)) ; => 4
```

## Sublists

### `take`

Take the first N elements.

```scheme
(take 3 '(1 2 3 4 5))   ; => (1 2 3)
(take 10 '(1 2))         ; => (1 2)
```

### `drop`

Drop the first N elements.

```scheme
(drop 2 '(1 2 3 4 5))   ; => (3 4 5)
```

### `flatten`

Flatten nested lists into a single list.

```scheme
(flatten '(1 (2 (3)) 4))   ; => (1 2 3 4)
```

### `flatten-deep`

Recursively flatten all nested lists.

```scheme
(flatten-deep '(1 (2 (3 (4)))))   ; => (1 2 3 4)
```

### `zip`

Combine corresponding elements from two lists into pairs.

```scheme
(zip '(1 2 3) '("a" "b" "c"))   ; => ((1 "a") (2 "b") (3 "c"))
```

### `partition`

Split a list into two lists based on a predicate. Returns a list of two lists: elements that satisfy the predicate and those that don't.

```scheme
(partition even? '(1 2 3 4 5))   ; => ((2 4) (1 3 5))
```

## Searching

### `member`

Return the tail of the list starting from the first matching element.

```scheme
(member 3 '(1 2 3 4))   ; => (3 4)
(member 9 '(1 2 3))     ; => #f
```

### `any`

Test if any element satisfies a predicate.

```scheme
(any even? '(1 3 5 6))   ; => #t
(any even? '(1 3 5))     ; => #f
```

### `every`

Test if all elements satisfy a predicate.

```scheme
(every even? '(2 4 6))     ; => #t
(every even? '(2 3 6))     ; => #f
```

### `list/index-of`

Return the index of the first occurrence of a value, or -1 if not found.

```scheme
(list/index-of '(10 20 30) 20)   ; => 1
(list/index-of '(10 20 30) 99)   ; => -1
```

### `list/unique`

Remove duplicate elements, preserving order.

```scheme
(list/unique '(1 2 2 3 3 3))   ; => (1 2 3)
```

### `list/dedupe`

Remove consecutive duplicates from a list.

```scheme
(list/dedupe '(1 1 2 2 3 3 2))   ; => (1 2 3 2)
```

## Grouping

### `list/group-by`

Group elements by a function, returning a map.

```scheme
(list/group-by even? '(1 2 3 4 5))   ; => {#f (1 3 5) #t (2 4)}
```

### `list/interleave`

Interleave elements from two lists.

```scheme
(list/interleave '(1 2 3) '(a b c))   ; => (1 a 2 b 3 c)
```

### `list/chunk`

Split a list into chunks of a given size.

```scheme
(list/chunk 2 '(1 2 3 4 5))   ; => ((1 2) (3 4) (5))
(list/chunk 3 '(1 2 3 4 5 6)) ; => ((1 2 3) (4 5 6))
```

### `frequencies`

Count occurrences of each element, returning a map.

```scheme
(frequencies '(a b a c b a))   ; => {a 3 b 2 c 1}
```

### `interpose`

Insert a separator between elements.

```scheme
(interpose ", " '("a" "b" "c"))   ; => ("a" ", " "b" ", " "c")
```

## Aggregation

### `list/sum`

Sum all numbers in a list.

```scheme
(list/sum '(1 2 3 4 5))   ; => 15
```

### `list/min`

Return the minimum value in a list.

```scheme
(list/min '(3 1 4 1 5))   ; => 1
```

### `list/max`

Return the maximum value in a list.

```scheme
(list/max '(3 1 4 1 5))   ; => 5
```

## Random

### `list/shuffle`

Return a randomly shuffled copy of a list.

```scheme
(list/shuffle '(1 2 3 4 5))   ; => (3 1 5 2 4) (varies)
```

### `list/pick`

Pick a random element from a list.

```scheme
(list/pick '(1 2 3 4 5))   ; => 3 (varies)
```

## Construction

### `list/repeat`

Create a list by repeating a value N times.

```scheme
(list/repeat 3 0)   ; => (0 0 0)
(list/repeat 4 "x") ; => ("x" "x" "x" "x")
```

### `make-list`

Alias for `list/repeat`.

```scheme
(make-list 3 0)   ; => (0 0 0)
```

### `iota`

Generate a list of numbers. `(iota count)`, `(iota count start)`, or `(iota count start step)`.

```scheme
(iota 5)         ; => (0 1 2 3 4)
(iota 3 10)      ; => (10 11 12)
(iota 4 0 2)     ; => (0 2 4 6)
```

## Splitting

### `list/split-at`

Split a list at a given index, returning two lists.

```scheme
(list/split-at '(1 2 3 4 5) 3)   ; => ((1 2 3) (4 5))
```

### `list/take-while`

Take elements from the front while a predicate holds.

```scheme
(list/take-while (fn (x) (< x 4)) '(1 2 3 4 5))   ; => (1 2 3)
```

### `list/drop-while`

Drop elements from the front while a predicate holds.

```scheme
(list/drop-while (fn (x) (< x 4)) '(1 2 3 4 5))   ; => (4 5)
```
