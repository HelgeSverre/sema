---
outline: [2, 3]
---

# Regex

Regular expression functions for pattern matching, searching, and replacement. Patterns use Rust regex syntax.

## Matching

### `regex/match?`

Test if a pattern matches anywhere in a string. Returns `#t` or `#f`.

```scheme
(regex/match? "\\d+" "abc123")     ; => #t
(regex/match? "\\d+" "no digits")  ; => #f
```

### `regex/match`

Match a pattern and return the full match plus any capture groups as a list. Returns `#f` if no match.

```scheme
(regex/match "^(\\w+)@(\\w+)" "user@host")
; => ("user@host" "user" "host")

(regex/match "(\\d+)-(\\d+)" "2025-01")
; => ("2025-01" "2025" "01")

(regex/match "xyz" "abc")
; => #f
```

### `regex/find-all`

Find all non-overlapping matches of a pattern.

```scheme
(regex/find-all "\\d+" "a1b2c3")    ; => ("1" "2" "3")
(regex/find-all "[A-Z]" "Hello World")  ; => ("H" "W")
```

## Replacement

### `regex/replace`

Replace the first match of a pattern.

```scheme
(regex/replace "\\d" "a1b2" "X")   ; => "aXb2"
```

### `regex/replace-all`

Replace all matches of a pattern.

```scheme
(regex/replace-all "\\d" "a1b2" "X")   ; => "aXbX"
(regex/replace-all "\\s+" "a  b  c" " ")  ; => "a b c"
```

## Splitting

### `regex/split`

Split a string by a regex pattern.

```scheme
(regex/split "," "a,b,c")          ; => ("a" "b" "c")
(regex/split "\\s+" "hello  world")  ; => ("hello" "world")
```
