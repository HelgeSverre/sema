---
outline: [2, 3]
---

# Strings & Characters

## Core String Operations

### `string-append`

Concatenate strings together.

```scheme
(string-append "hello" " " "world")   ; => "hello world"
(string-append "a" "b" "c")           ; => "abc"
```

### `string-length`

Return the length of a string.

```scheme
(string-length "hello")   ; => 5
(string-length "")        ; => 0
```

### `string-ref`

Return the character at a given index.

```scheme
(string-ref "hello" 0)    ; => #\h
(string-ref "hello" 4)    ; => #\o
```

### `substring`

Extract a substring by start and end index.

```scheme
(substring "hello" 1 3)   ; => "el"
(substring "hello" 0 5)   ; => "hello"
```

### `str`

Convert any value to its string representation.

```scheme
(str 42)           ; => "42"
(str #t)           ; => "#t"
(str '(1 2 3))    ; => "(1 2 3)"
```

### `format`

Format a string with `~a` placeholders.

```scheme
(format "~a is ~a" "Sema" "great")   ; => "Sema is great"
(format "~a + ~a = ~a" 1 2 3)        ; => "1 + 2 = 3"
```

## Slash-Namespaced String Operations

### `string/split`

Split a string by a delimiter.

```scheme
(string/split "a,b,c" ",")        ; => ("a" "b" "c")
(string/split "hello world" " ")  ; => ("hello" "world")
```

### `string/join`

Join a list of strings with a separator.

```scheme
(string/join '("a" "b" "c") ", ")  ; => "a, b, c"
(string/join '("x" "y") "-")      ; => "x-y"
```

### `string/trim`

Remove whitespace from both ends.

```scheme
(string/trim "  hello  ")   ; => "hello"
(string/trim "\thello\n")   ; => "hello"
```

### `string/trim-left`

Remove whitespace from the left.

```scheme
(string/trim-left "  hi")   ; => "hi"
```

### `string/trim-right`

Remove whitespace from the right.

```scheme
(string/trim-right "hi  ")  ; => "hi"
```

### `string/upper`

Convert string to uppercase.

```scheme
(string/upper "hello")   ; => "HELLO"
```

### `string/lower`

Convert string to lowercase.

```scheme
(string/lower "HELLO")   ; => "hello"
```

### `string/capitalize`

Capitalize the first character.

```scheme
(string/capitalize "hello")   ; => "Hello"
```

### `string/title-case`

Capitalize the first character of each word.

```scheme
(string/title-case "hello world")   ; => "Hello World"
```

### `string/contains?`

Test if a string contains a substring.

```scheme
(string/contains? "hello" "ell")   ; => #t
(string/contains? "hello" "xyz")   ; => #f
```

### `string/starts-with?`

Test if a string starts with a prefix.

```scheme
(string/starts-with? "hello" "he")   ; => #t
(string/starts-with? "hello" "lo")   ; => #f
```

### `string/ends-with?`

Test if a string ends with a suffix.

```scheme
(string/ends-with? "hello" "lo")   ; => #t
(string/ends-with? "hello" "he")   ; => #f
```

### `string/replace`

Replace all occurrences of a substring.

```scheme
(string/replace "hello" "l" "r")   ; => "herro"
(string/replace "aaa" "a" "b")    ; => "bbb"
```

### `string/index-of`

Return the index of the first occurrence of a substring, or -1 if not found.

```scheme
(string/index-of "hello" "ll")   ; => 2
(string/index-of "hello" "xyz")  ; => -1
```

### `string/chars`

Convert a string to a list of characters.

```scheme
(string/chars "abc")   ; => (#\a #\b #\c)
```

### `string/repeat`

Repeat a string N times.

```scheme
(string/repeat "ab" 3)   ; => "ababab"
(string/repeat "-" 5)    ; => "-----"
```

### `string/pad-left`

Pad a string on the left to a given width.

```scheme
(string/pad-left "42" 5 "0")   ; => "00042"
(string/pad-left "hi" 5)       ; => "   hi"
```

### `string/pad-right`

Pad a string on the right to a given width.

```scheme
(string/pad-right "hi" 5)       ; => "hi   "
(string/pad-right "42" 5 "0")   ; => "42000"
```

### `string/number?`

Test if a string represents a valid number.

```scheme
(string/number? "42")      ; => #t
(string/number? "3.14")   ; => #t
(string/number? "hello")  ; => #f
```

### `string/map`

Apply a character function to each character in a string, returning a new string.

```scheme
(string/map char-upcase "hello")   ; => "HELLO"
```

### `string/reverse`

Reverse a string.

```scheme
(string/reverse "hello")   ; => "olleh"
```

## Characters

Character literals are written with the `#\` prefix.

```scheme
#\a                ; character literal
#\space            ; named character: space
#\newline          ; named character: newline
#\tab              ; named character: tab
```

### `char->integer`

Convert a character to its Unicode code point.

```scheme
(char->integer #\A)   ; => 65
(char->integer #\a)   ; => 97
```

### `integer->char`

Convert a Unicode code point to a character.

```scheme
(integer->char 65)    ; => #\A
(integer->char 955)   ; => #\λ
```

### `char-alphabetic?`

Test if a character is alphabetic.

```scheme
(char-alphabetic? #\a)   ; => #t
(char-alphabetic? #\5)   ; => #f
```

### `char-numeric?`

Test if a character is numeric.

```scheme
(char-numeric? #\5)      ; => #t
(char-numeric? #\a)      ; => #f
```

### `char-whitespace?`

Test if a character is whitespace.

```scheme
(char-whitespace? #\space)   ; => #t
(char-whitespace? #\a)       ; => #f
```

### `char-upper-case?`

Test if a character is uppercase.

```scheme
(char-upper-case? #\A)   ; => #t
(char-upper-case? #\a)   ; => #f
```

### `char-upcase`

Convert a character to uppercase.

```scheme
(char-upcase #\a)   ; => #\A
```

### `char-downcase`

Convert a character to lowercase.

```scheme
(char-downcase #\Z)   ; => #\z
```

### `char->string`

Convert a character to a single-character string.

```scheme
(char->string #\a)   ; => "a"
```

### `string->char`

Convert a single-character string to a character.

```scheme
(string->char "a")   ; => #\a
```

## Character Comparison (R7RS)

### `char=?`

Character equality.

```scheme
(char=? #\a #\a)   ; => #t
(char=? #\a #\b)   ; => #f
```

### `char<?`

Character less-than (by code point).

```scheme
(char<? #\a #\b)   ; => #t
```

### `char>?`

Character greater-than.

```scheme
(char>? #\b #\a)   ; => #t
```

### `char<=?`

Character less-than-or-equal.

```scheme
(char<=? #\a #\b)   ; => #t
(char<=? #\a #\a)   ; => #t
```

### `char>=?`

Character greater-than-or-equal.

```scheme
(char>=? #\b #\a)   ; => #t
```

### `char-ci=?`

Case-insensitive character equality.

```scheme
(char-ci=? #\A #\a)   ; => #t
```

## Type Conversions

### `string->number`

Parse a string as a number.

```scheme
(string->number "42")     ; => 42
(string->number "3.14")  ; => 3.14
```

### `number->string`

Convert a number to a string.

```scheme
(number->string 42)      ; => "42"
(number->string 3.14)   ; => "3.14"
```

### `string->symbol`

Convert a string to a symbol.

```scheme
(string->symbol "foo")   ; => foo
```

### `symbol->string`

Convert a symbol to a string.

```scheme
(symbol->string 'foo)   ; => "foo"
```

### `string->keyword`

Convert a string to a keyword.

```scheme
(string->keyword "name")   ; => :name
```

### `keyword->string`

Convert a keyword to a string.

```scheme
(keyword->string :name)   ; => "name"
```

### `string->list`

Convert a string to a list of characters.

```scheme
(string->list "abc")   ; => (#\a #\b #\c)
```

### `list->string`

Convert a list of characters to a string.

```scheme
(list->string '(#\h #\i))   ; => "hi"
```
