---
outline: [2, 3]
---

# Data Types

Sema has 20 built-in data types covering numbers, text, collections, and LLM primitives.

## Type Table

| Type         | Syntax               | Examples                                                           |
| ------------ | -------------------- | ------------------------------------------------------------------ |
| Integer      | digits               | `42`, `-7`, `0`                                                    |
| Float        | digits with `.`      | `3.14`, `-0.5`, `1e10`                                             |
| String       | double-quoted        | `"hello"`, `"line\nbreak"`, `"\x1B;"`                              |
| F-String     | `f"...${expr}..."` | `f"Hello ${name}"`, `f"${(+ 1 2)}"`                               |
| Boolean      | `#t` / `#f`          | `#t`, `#f`                                                         |
| Nil          | `nil`                | `nil`                                                              |
| Symbol       | bare identifier      | `foo`, `my-var`, `+`                                               |
| Keyword      | colon-prefixed       | `:name`, `:type`, `:ok`                                            |
| Character    | `#\` prefix          | `#\a`, `#\space`, `#\newline`                                      |
| List         | parenthesized        | `(1 2 3)`, `(+ a b)`                                               |
| Vector       | bracketed            | `[1 2 3]`, `["a" "b"]`                                             |
| Map          | curly-braced         | `{:name "Ada" :age 36}`                                            |
| HashMap      | `(hashmap/new ...)`  | `(hashmap/new :a 1 :b 2)`                                          |
| Prompt       | `(prompt ...)`       | LLM prompt (see [Prompts](../llm/prompts.md))                      |
| Message      | `(message ...)`      | LLM message (see [Prompts](../llm/prompts.md))                     |
| Conversation | `(conversation/new)` | LLM conversation (see [Conversations](../llm/conversations.md))    |
| Tool         | `(deftool ...)`      | LLM tool definition (see [Tools & Agents](../llm/tools-agents.md)) |
| Agent        | `(defagent ...)`     | LLM agent (see [Tools & Agents](../llm/tools-agents.md))           |
| Promise      | `(delay expr)`       | Lazy evaluation                                                    |
| Record       | `define-record-type` | `(define-record-type point ...)`                                   |
| Bytevector   | `#u8(...)` literal   | `#u8(1 2 3)`, `#u8()`                                              |

## Scalars

### Integer

Whole numbers. Standard arithmetic applies.

```scheme
42
-7
0
```

### Float

Floating-point numbers with a decimal point.

```scheme
3.14
-0.5
0.001
```

### String

Double-quoted text with escape sequences.

```scheme
"hello"
"line\nbreak"
"\x1B;"
```

### F-String (Interpolated String)

String interpolation with embedded expressions. `f"..."` expands to a `(str ...)` call at read time.

```scheme
f"Hello ${name}"              ; => (str "Hello " name)
f"2 + 2 = ${(+ 2 2)}"        ; => "2 + 2 = 4"
f"${(:name user)} is ${(:age user)} years old"
```

Use `\$` to include a literal dollar sign: `f"costs \$5"`.

### Boolean

`#t` for true, `#f` for false.

```scheme
#t
#f
```

### Nil

The empty/null value.

```scheme
nil
```

### Symbol

Bare identifiers used as variable names and in quoted data.

```scheme
foo
my-var
+
```

### Keyword

Colon-prefixed identifiers. Keywords are self-evaluating and can be used as accessor functions on maps.

```scheme
:name
:type
:ok

;; Keywords as functions
(:name {:name "Ada" :age 36})  ; => "Ada"
```

### Character

Character literals with `#\` prefix. Named characters are supported.

```scheme
#\a
#\space
#\newline
#\tab
```

## Collections

### List

Parenthesized sequences. Lists are the fundamental data structure in Sema.

```scheme
(1 2 3)
(+ a b)
'(hello world)
```

### Vector

Bracketed sequences with O(1) indexed access.

```scheme
[1 2 3]
["a" "b"]
```

### Map

Curly-braced key-value pairs with deterministic (sorted) ordering. Maps support [destructuring](./special-forms.md#map-destructuring) in `let`, `define`, `lambda`, and [`match`](./special-forms.md#match) patterns.

```scheme
{:name "Ada" :age 36}
{:a 1 :b 2 :c 3}
```

### HashMap

Hash-based maps for O(1) lookup performance with many keys.

```scheme
(hashmap/new :a 1 :b 2 :c 3)
```

### Bytevector

Byte arrays with `#u8(...)` literal syntax.

```scheme
#u8(1 2 3)
#u8()
(bytevector 1 2 3)
(make-bytevector 4)
```

## Special Types

### Promise

Lazy evaluation via `delay`/`force`. The expression is not evaluated until forced, and the result is memoized.

```scheme
(define p (delay (+ 1 2)))
(force p)       ; => 3
(promise? p)    ; => #t
```

### Record

User-defined record types with constructors, predicates, and field accessors.

```scheme
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 3 4))
(point-x p)    ; => 3
```

## String Escape Sequences

| Escape       | Description                          | Example               |
| ------------ | ------------------------------------ | --------------------- |
| `\n`         | Newline                              | `"line\nbreak"`       |
| `\t`         | Tab                                  | `"col1\tcol2"`        |
| `\r`         | Carriage return                      | `"text\r"`            |
| `\\`         | Backslash                            | `"path\\file"`        |
| `\"`         | Double quote                         | `"say \"hi\""`        |
| `\0`         | Null character                       | `"\0"`                |
| `\x<hex>;`   | Unicode scalar (R7RS, 1+ hex digits) | `"\x1B;"`, `"\x3BB;"` |
| `\uNNNN`     | Unicode code point (4 hex digits)    | `"\u03BB"` (λ)        |
| `\UNNNNNNNN` | Unicode code point (8 hex digits)    | `"\U0001F600"` (😀)   |
| `\$`         | Literal dollar sign (in f-strings)   | `f"costs \$5"`        |

## Type Predicates

```scheme
(null? '())        (nil? nil)         (empty? "")
(list? '(1))       (vector? [1])      (map? {:a 1})
(pair? '(1 2))     ; #t (non-empty list, Scheme compat)
(number? 42)       (integer? 42)      (float? 3.14)
(string? "hi")     (symbol? 'x)       (keyword? :k)
(char? #\a)        (record? r)        (bytevector? #u8())
(promise? (delay 1))  (promise-forced? p)
(bool? #t)         (fn? car)
(zero? 0)          (even? 4)          (odd? 3)
(positive? 1)      (negative? -1)
(eq? 'a 'a)        (= 1 1)

;; Scheme aliases: boolean? = bool?, procedure? = fn?, equal? = eq?

;; LLM type predicates
(prompt? p)        (message? m)       (conversation? c)
(tool? t)          (agent? a)
```

## Type Conversions

```scheme
(str 42)                    ; => "42" (any value to string)
(string->number "42")       ; => 42
(number->string 42)         ; => "42"
(string->symbol "foo")      ; => foo
(symbol->string 'foo)       ; => "foo"
(string->keyword "name")    ; => :name
(keyword->string :name)     ; => "name"
(char->integer #\A)         ; => 65
(integer->char 65)          ; => #\A
(char->string #\a)          ; => "a"
(string->char "a")          ; => #\a
(string->list "abc")        ; => (#\a #\b #\c)
(list->string '(#\h #\i))   ; => "hi"
(vector->list [1 2 3])      ; => (1 2 3)
(list->vector '(1 2 3))     ; => [1 2 3]
(bytevector->list #u8(65))   ; => (65)
(list->bytevector '(1 2 3))  ; => #u8(1 2 3)
(utf8->string #u8(104 105))  ; => "hi"
(string->utf8 "hi")          ; => #u8(104 105)
(type 42)                    ; => "integer"
```
