---
outline: [2, 3]
---

# Strings & Characters

## Core String Operations

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

Return the byte index of the first occurrence of a substring, or `nil` if not found.

```scheme
(string/index-of "hello" "ll")   ; => 2
(string/index-of "hello" "xyz")  ; => nil
```

### `string/last-index-of`

Find the last occurrence of a substring. Returns the index or -1 if not found.

```scheme
(string/last-index-of "abcabc" "abc")   ; => 3
(string/last-index-of "hello" "xyz")    ; => -1
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

### `string/empty?`

Test if a string is empty.

```scheme
(string/empty? "")      ; => #t
(string/empty? "hello") ; => #f
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

## Unicode & Encoding

### `string/byte-length`

Return the UTF-8 byte length of a string (as opposed to character count from `string-length`). Useful for understanding the actual memory footprint — emoji and CJK characters use more bytes than ASCII.

```scheme
(string/byte-length "hello")   ; => 5   (ASCII: 1 byte each)
(string/byte-length "héllo")   ; => 6   (é is 2 bytes in UTF-8)
(string/byte-length "日本語")   ; => 9   (CJK: 3 bytes each)
(string/byte-length "😀")      ; => 4   (emoji: 4 bytes)
```

Compare with `string-length` which counts characters:

```scheme
(string-length "😀")           ; => 1   (one character)
(string/byte-length "😀")      ; => 4   (four bytes)
```

### `string/codepoints`

Return a list of Unicode codepoint integers for each character in a string. This reveals the internal structure of composed characters and emoji sequences.

```scheme
(string/codepoints "ABC")      ; => (65 66 67)
(string/codepoints "é")        ; => (233)
(string/codepoints "😀")       ; => (128512)
```

Emoji that appear as a single glyph are often multiple codepoints joined by Zero Width Joiner (U+200D = 8205):

```scheme
;; 👨‍👩‍👦 is actually 👨 + ZWJ + 👩 + ZWJ + 👦
(string/codepoints "👨‍👩‍👦")   ; => (128104 8205 128105 8205 128102)

;; 👋🏽 is 👋 + skin tone modifier
(string/codepoints "👋🏽")      ; => (128075 127997)
```

### `string/from-codepoints`

Construct a string from a list of Unicode codepoint integers. This is the inverse of `string/codepoints` and enables building emoji programmatically by combining codepoints.

```scheme
(string/from-codepoints (list 65 66 67))   ; => "ABC"
(string/from-codepoints (list 233))        ; => "é"
```

Build emoji by combining people with ZWJ (8205):

```scheme
;; Build a family: 👨 + ZWJ + 👩 + ZWJ + 👧
(string/from-codepoints (list 128104 8205 128105 8205 128103))
;; => 👨‍👩‍👧

;; Build a profession: 👩 + ZWJ + 💻
(string/from-codepoints (list 128105 8205 128187))
;; => 👩‍💻

;; Add skin tone: 👋 + modifier
(string/from-codepoints (list 128075 127997))
;; => 👋🏽

;; Build flags from Regional Indicators (A=127462):
(string/from-codepoints (list 127475 127476))
;; => 🇳🇴 (NO = Norway)
```

Roundtrip any string through codepoints:

```scheme
(string/from-codepoints (string/codepoints "Hello 世界"))
;; => "Hello 世界"
```

### `string/normalize`

Normalize a string to a Unicode normalization form. Supported forms: `:nfc`, `:nfd`, `:nfkc`, `:nfkd` (as keywords or strings).

- **NFC** — Canonical Decomposition, followed by Canonical Composition (most common)
- **NFD** — Canonical Decomposition
- **NFKC** — Compatibility Decomposition, followed by Canonical Composition
- **NFKD** — Compatibility Decomposition

```scheme
;; NFC: combine decomposed characters
;; e + combining acute accent → é
(string/normalize "e\u0301" :nfc)    ; => "é"

;; NFD: decompose composed characters
(string-length (string/normalize "é" :nfd))  ; => 2 (e + combining accent)

;; NFKC/NFKD: compatibility decomposition (ligatures, etc.)
(string/normalize "\uFB01" :nfkc)    ; => "fi" (ﬁ ligature → two letters)

;; String form names also work
(string/normalize "e\u0301" "NFC")   ; => "é"
```

### `string/foldcase`

Apply Unicode case folding to a string. Useful for case-insensitive comparisons and normalization. Uses full Unicode-aware lowercasing.

```scheme
(string/foldcase "HELLO")        ; => "hello"
(string/foldcase "Hello World")  ; => "hello world"
(string/foldcase "Straße")       ; => "straße"
(string/foldcase "ΩΜΕΓΑ")        ; => "ωμεγα"
```

### `string-ci=?`

Case-insensitive string equality comparison. Compares two strings after applying case folding to both.

```scheme
(string-ci=? "Hello" "hello")   ; => #t
(string-ci=? "ABC" "abc")       ; => #t
(string-ci=? "CAFÉ" "café")     ; => #t
(string-ci=? "hello" "world")   ; => #f
```

## Scheme Compatibility Aliases

These functions use legacy Scheme/R7RS naming conventions. They work identically to their modern equivalents and are kept for compatibility. Prefer the `string/` namespaced variants in new code.

### `string-append`

Concatenate strings together.

```scheme
(string-append "hello" " " "world")   ; => "hello world"
(string-append "a" "b" "c")           ; => "abc"
```

### `string-length`

Return the number of characters in a string.

```scheme
(string-length "hello")   ; => 5
(string-length "")        ; => 0
(string-length "héllo")   ; => 5
(string-length "日本語")   ; => 3
```

### `string-ref`

Return the character at a given index.

```scheme
(string-ref "hello" 0)    ; => #\h
(string-ref "hello" 4)    ; => #\o
```

### `substring`

Extract a substring by start and end character index.

```scheme
(substring "hello" 1 3)   ; => "el"
(substring "hello" 0 5)   ; => "hello"
(substring "héllo" 1 2)   ; => "é"
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

## Slicing & Extraction

### `string/after`

Everything after the first occurrence of a needle. Returns the original string if needle not found.

```scheme
(string/after "hello@world.com" "@")  ; => "world.com"
(string/after "no-match" "@")         ; => "no-match"
```

### `string/after-last`

Everything after the last occurrence of a needle.

```scheme
(string/after-last "a.b.c" ".")  ; => "c"
```

### `string/before`

Everything before the first occurrence of a needle.

```scheme
(string/before "hello@world.com" "@")  ; => "hello"
(string/before "no-match" "@")         ; => "no-match"
```

### `string/before-last`

Everything before the last occurrence of a needle.

```scheme
(string/before-last "a.b.c" ".")  ; => "a.b"
```

### `string/between`

Extract the portion between two delimiters.

```scheme
(string/between "[hello]" "[" "]")  ; => "hello"
(string/between "start:middle:end" "start:" ":end")  ; => "middle"
```

### `string/take`

Take the first N characters (positive) or last N characters (negative).

```scheme
(string/take "hello" 3)   ; => "hel"
(string/take "hello" -2)  ; => "lo"
```

## Prefix & Suffix

### `string/chop-start`

Remove a prefix if present, otherwise return unchanged.

```scheme
(string/chop-start "Hello World" "Hello ")  ; => "World"
(string/chop-start "Hello" "Bye")           ; => "Hello"
```

### `string/chop-end`

Remove a suffix if present.

```scheme
(string/chop-end "file.txt" ".txt")  ; => "file"
(string/chop-end "file.txt" ".md")   ; => "file.txt"
```

### `string/ensure-start`

Ensure a string starts with a prefix (adds it if missing).

```scheme
(string/ensure-start "/path" "/")   ; => "/path"
(string/ensure-start "path" "/")    ; => "/path"
```

### `string/ensure-end`

Ensure a string ends with a suffix.

```scheme
(string/ensure-end "path" "/")   ; => "path/"
(string/ensure-end "path/" "/")  ; => "path/"
```

### `string/wrap`

Wrap a string with left and right delimiters.

```scheme
(string/wrap "hello" "(" ")")   ; => "(hello)"
(string/wrap "hello" "**")      ; => "**hello**"
```

### `string/unwrap`

Remove surrounding delimiters if both present.

```scheme
(string/unwrap "(hello)" "(" ")")  ; => "hello"
(string/unwrap "hello" "(" ")")    ; => "hello"
```

## Replacement

### `string/replace-first`

Replace only the first occurrence of a substring.

```scheme
(string/replace-first "aaa" "a" "b")  ; => "baa"
```

### `string/replace-last`

Replace only the last occurrence.

```scheme
(string/replace-last "aaa" "a" "b")  ; => "aab"
```

### `string/remove`

Remove all occurrences of a substring.

```scheme
(string/remove "hello world" "o")  ; => "hell wrld"
```

## Case Conversion

### `string/snake-case`

Convert to snake_case.

```scheme
(string/snake-case "helloWorld")     ; => "hello_world"
(string/snake-case "Hello World")   ; => "hello_world"
```

### `string/kebab-case`

Convert to kebab-case.

```scheme
(string/kebab-case "helloWorld")     ; => "hello-world"
(string/kebab-case "Hello World")   ; => "hello-world"
```

### `string/camel-case`

Convert to camelCase.

```scheme
(string/camel-case "hello_world")    ; => "helloWorld"
(string/camel-case "Hello World")    ; => "helloWorld"
```

### `string/pascal-case`

Convert to PascalCase.

```scheme
(string/pascal-case "hello_world")   ; => "HelloWorld"
(string/pascal-case "hello world")   ; => "HelloWorld"
```

### `string/headline`

Convert to Title Case headline.

```scheme
(string/headline "hello_world")   ; => "Hello World"
(string/headline "helloWorld")    ; => "Hello World"
```

### `string/words`

Split a string into words (splits on non-alphanumeric boundaries).

```scheme
(string/words "hello_world")     ; => ("hello" "world")
(string/words "helloWorld")      ; => ("hello" "World")
(string/words "Hello World!")    ; => ("Hello" "World")
```
