---
outline: [2, 3]
---

# Math & Arithmetic

## Basic Arithmetic

### `+`

Add numbers together. Accepts any number of arguments.

```sema
(+ 1 2 3)     ; => 6
(+ 10)        ; => 10
(+)           ; => 0
```

### `-`

Subtract numbers. With one argument, negates. With multiple, subtracts left to right.

```sema
(- 10 3)      ; => 7
(- 10 3 2)    ; => 5
(- 5)         ; => -5
```

### `*`

Multiply numbers together.

```sema
(* 4 5)       ; => 20
(* 2 3 4)     ; => 24
(*)           ; => 1
```

### `/`

Divide numbers. Integer division when both operands are integers.

```sema
(/ 10 2)      ; => 5
(/ 10 3)      ; => 3
(/ 10.0 3)    ; => 3.333...
```

### `mod`

Modulo (remainder after division).

```sema
(mod 10 3)    ; => 1
(mod 7 2)     ; => 1
```

## Comparison

### `<`

Less than. Supports chaining.

```sema
(< 1 2)       ; => #t
(< 1 2 3)     ; => #t
(< 3 2)       ; => #f
```

### `>`

Greater than.

```sema
(> 3 2)       ; => #t
(> 1 2)       ; => #f
```

### `<=`

Less than or equal.

```sema
(<= 1 2)      ; => #t
(<= 2 2)      ; => #t
```

### `>=`

Greater than or equal.

```sema
(>= 3 2)      ; => #t
(>= 2 2)      ; => #t
```

### `=`

Numeric equality.

```sema
(= 1 1)       ; => #t
(= 1 2)       ; => #f
```

## Numeric Utilities

### `abs`

Absolute value.

```sema
(abs -5)      ; => 5
(abs 3)       ; => 3
(abs -3.14)   ; => 3.14
```

### `min`

Return the smallest of one or more numbers.

```sema
(min 1 2 3)   ; => 1
(min 5)       ; => 5
```

### `max`

Return the largest of one or more numbers.

```sema
(max 1 2 3)   ; => 3
(max 5)       ; => 5
```

### `pow`

Raise a number to a power.

```sema
(pow 2 10)    ; => 1024
(pow 3 3)     ; => 27
```

### `sqrt`

Square root.

```sema
(sqrt 16)     ; => 4.0
(sqrt 2)      ; => 1.4142...
```

### `log`

Natural logarithm.

```sema
(log 1)       ; => 0.0
(log 100)     ; => 4.605...
```

### `floor`

Round down to nearest integer.

```sema
(floor 3.7)   ; => 3
(floor -2.3)  ; => -3
```

### `ceil`

Round up to nearest integer.

```sema
(ceil 3.2)    ; => 4
(ceil -2.7)   ; => -2
```

### `round`

Round to nearest integer.

```sema
(round 3.5)   ; => 4
(round 3.4)   ; => 3
```

## Trigonometry

### `sin`

Sine (argument in radians).

```sema
(sin 0)       ; => 0.0
(sin pi)      ; => ~0.0
```

### `cos`

Cosine (argument in radians).

```sema
(cos 0)       ; => 1.0
(cos pi)      ; => -1.0
```

### `math/tan`

Tangent (argument in radians).

```sema
(math/tan 0)       ; => 0.0
(math/tan (/ pi 4)); => ~1.0
```

### `math/asin`

Inverse sine. Returns radians.

```sema
(math/asin 1)      ; => ~1.5707 (π/2)
(math/asin 0)      ; => 0.0
```

### `math/acos`

Inverse cosine. Returns radians.

```sema
(math/acos 0)      ; => ~1.5707 (π/2)
(math/acos 1)      ; => 0.0
```

### `math/atan`

Inverse tangent. Returns radians.

```sema
(math/atan 1)      ; => ~0.7854 (π/4)
(math/atan 0)      ; => 0.0
```

### `math/atan2`

Two-argument inverse tangent. Returns the angle in radians between the positive x-axis and the point (x, y).

```sema
(math/atan2 1 1)   ; => ~0.7854 (π/4)
(math/atan2 0 -1)  ; => ~3.1416 (π)
```

## Hyperbolic Functions

### `math/sinh`

Hyperbolic sine.

```sema
(math/sinh 0)      ; => 0.0
(math/sinh 1)      ; => 1.1752...
```

### `math/cosh`

Hyperbolic cosine.

```sema
(math/cosh 0)      ; => 1.0
(math/cosh 1)      ; => 1.5430...
```

### `math/tanh`

Hyperbolic tangent.

```sema
(math/tanh 0)      ; => 0.0
(math/tanh 1)      ; => 0.7615...
```

## Exponential & Logarithmic

### `math/exp`

Euler's number raised to a power (e^x).

```sema
(math/exp 1)       ; => 2.71828...
(math/exp 0)       ; => 1.0
```

### `math/log10`

Base-10 logarithm.

```sema
(math/log10 100)   ; => 2.0
(math/log10 1000)  ; => 3.0
```

### `math/log2`

Base-2 logarithm.

```sema
(math/log2 8)      ; => 3.0
(math/log2 1024)   ; => 10.0
```

## Integer Math

### `math/gcd`

Greatest common divisor.

```sema
(math/gcd 12 8)    ; => 4
(math/gcd 15 10)   ; => 5
```

### `math/lcm`

Least common multiple.

```sema
(math/lcm 4 6)     ; => 12
(math/lcm 3 5)     ; => 15
```

### `math/quotient`

Integer quotient (truncated division).

```sema
(math/quotient 10 3)  ; => 3
(math/quotient 7 2)   ; => 3
```

### `math/remainder`

Remainder after truncated division.

```sema
(math/remainder 10 3) ; => 1
(math/remainder 7 2)  ; => 1
```

## Random Numbers

### `math/random`

Return a random float between 0.0 (inclusive) and 1.0 (exclusive).

```sema
(math/random)      ; => 0.7291... (varies)
```

### `math/random-int`

Return a random integer in a range (inclusive on both ends).

```sema
(math/random-int 1 100)  ; => 42 (varies)
(math/random-int 0 9)    ; => 7 (varies)
```

## Interpolation & Clamping

### `math/clamp`

Clamp a value to a range.

```sema
(math/clamp 15 0 10)   ; => 10
(math/clamp -5 0 10)   ; => 0
(math/clamp 5 0 10)    ; => 5
```

### `math/sign`

Return the sign of a number: -1, 0, or 1.

```sema
(math/sign -5)     ; => -1
(math/sign 0)      ; => 0
(math/sign 42)     ; => 1
```

### `math/lerp`

Linear interpolation between two values. `(math/lerp a b t)` returns `a + (b - a) * t`.

```sema
(math/lerp 0 100 0.5)   ; => 50.0
(math/lerp 0 100 0.25)  ; => 25.0
(math/lerp 10 20 0.0)   ; => 10.0
```

### `math/map-range`

Map a value from one range to another. `(math/map-range value in-min in-max out-min out-max)`.

```sema
(math/map-range 5 0 10 0 100)    ; => 50.0
(math/map-range 0.5 0 1 0 255)   ; => 127.5
```

## Angle Conversion

### `math/degrees->radians`

Convert degrees to radians.

```sema
(math/degrees->radians 180)   ; => 3.14159...
(math/degrees->radians 90)    ; => 1.5707...
```

### `math/radians->degrees`

Convert radians to degrees.

```sema
(math/radians->degrees pi)    ; => 180.0
(math/radians->degrees 1)     ; => 57.295...
```

## Numeric Predicates

### `even?`

Test if an integer is even.

```sema
(even? 4)      ; => #t
(even? 3)      ; => #f
```

### `odd?`

Test if an integer is odd.

```sema
(odd? 3)       ; => #t
(odd? 4)       ; => #f
```

### `positive?`

Test if a number is positive.

```sema
(positive? 1)  ; => #t
(positive? -1) ; => #f
(positive? 0)  ; => #f
```

### `negative?`

Test if a number is negative.

```sema
(negative? -1) ; => #t
(negative? 1)  ; => #f
```

### `zero?`

Test if a number is zero.

```sema
(zero? 0)      ; => #t
(zero? 1)      ; => #f
```

### `math/nan?`

Test if a value is NaN (not a number).

```sema
(math/nan? math/nan)       ; => #t
(math/nan? 42)             ; => #f
```

### `math/infinite?`

Test if a value is infinite.

```sema
(math/infinite? math/infinity)  ; => #t
(math/infinite? 42)             ; => #f
```

## Constants

### `pi`

The mathematical constant π (3.14159...).

```sema
pi             ; => 3.141592653589793
```

### `e`

Euler's number (2.71828...).

```sema
e              ; => 2.718281828459045
```

### `math/infinity`

Positive infinity.

```sema
math/infinity  ; => Inf
```

### `math/nan`

Not a number.

```sema
math/nan       ; => NaN
```

## Scheme Aliases

### `modulo`

Alias for `mod`.

```sema
(modulo 10 3)  ; => 1
```

### `expt`

Alias for `pow` (Scheme name for exponentiation).

```sema
(expt 2 10)   ; => 1024
```

### `ceiling`

Alias for `ceil`.

```sema
(ceiling 3.2)  ; => 4
```

### `truncate`

Truncate toward zero.

```sema
(truncate 3.7)  ; => 3
(truncate -3.7) ; => -3
```

## Bitwise Operations

### `bit/and`

Bitwise AND.

```sema
(bit/and 5 3)      ; => 1
(bit/and 15 9)     ; => 9
```

### `bit/or`

Bitwise OR.

```sema
(bit/or 5 3)       ; => 7
(bit/or 8 4)       ; => 12
```

### `bit/xor`

Bitwise XOR.

```sema
(bit/xor 5 3)      ; => 6
```

### `bit/not`

Bitwise NOT (complement).

```sema
(bit/not 5)        ; => -6
```

### `bit/shift-left`

Left bit shift.

```sema
(bit/shift-left 1 4)   ; => 16
(bit/shift-left 3 2)   ; => 12
```

### `bit/shift-right`

Right bit shift.

```sema
(bit/shift-right 16 2) ; => 4
(bit/shift-right 8 1)  ; => 4
```
