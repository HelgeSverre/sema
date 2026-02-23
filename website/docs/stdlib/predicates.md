---
outline: [2, 3]
---

# Predicates & Type Checking

Predicates return `#t` or `#f` and conventionally end with `?`.

## Emptiness Predicates

### `null?`

Test if a value is the empty list.

```sema
(null? '())    ; => #t
(null? '(1))   ; => #f
```

### `nil?`

Test if a value is `nil`.

```sema
(nil? nil)     ; => #t
(nil? 0)       ; => #f
```

### `empty?`

Test if a collection or string is empty.

```sema
(empty? "")        ; => #t
(empty? '())       ; => #t
(empty? "hello")   ; => #f
```

## Collection Predicates

### `list?`

Test if a value is a list.

```sema
(list? '(1))    ; => #t
(list? 42)      ; => #f
```

### `pair?`

Test if a value is a non-empty list (Scheme compatibility).

```sema
(pair? '(1 2))   ; => #t
(pair? '())      ; => #f
```

### `vector?`

Test if a value is a vector.

```sema
(vector? [1])   ; => #t
(vector? '(1))  ; => #f
```

### `map?`

Test if a value is a map.

```sema
(map? {:a 1})   ; => #t
(map? '())      ; => #f
```

## Numeric Predicates

### `number?`

Test if a value is a number (integer or float).

```sema
(number? 42)     ; => #t
(number? 3.14)   ; => #t
(number? "42")   ; => #f
```

### `integer?`

Test if a value is an integer.

```sema
(integer? 42)     ; => #t
(integer? 3.14)   ; => #f
```

### `float?`

Test if a value is a floating-point number.

```sema
(float? 3.14)   ; => #t
(float? 42)     ; => #f
```

### `zero?`

Test if a number is zero.

```sema
(zero? 0)   ; => #t
(zero? 1)   ; => #f
```

### `even?`

Test if an integer is even.

```sema
(even? 4)   ; => #t
(even? 3)   ; => #f
```

### `odd?`

Test if an integer is odd.

```sema
(odd? 3)   ; => #t
(odd? 4)   ; => #f
```

### `positive?`

Test if a number is positive.

```sema
(positive? 1)    ; => #t
(positive? -1)   ; => #f
```

### `negative?`

Test if a number is negative.

```sema
(negative? -1)   ; => #t
(negative? 1)    ; => #f
```

## Type Predicates

### `string?`

Test if a value is a string.

```sema
(string? "hi")   ; => #t
(string? 42)     ; => #f
```

### `symbol?`

Test if a value is a symbol.

```sema
(symbol? 'x)     ; => #t
(symbol? "x")    ; => #f
```

### `keyword?`

Test if a value is a keyword.

```sema
(keyword? :k)    ; => #t
(keyword? "k")   ; => #f
```

### `char?`

Test if a value is a character.

```sema
(char? #\a)      ; => #t
(char? "a")      ; => #f
```

### `bool?`

Test if a value is a boolean. `boolean?` is an alias.

```sema
(bool? #t)   ; => #t
(bool? 0)    ; => #f
```

### `fn?`

Test if a value is a function. `procedure?` is an alias.

```sema
(fn? car)        ; => #t
(fn? 42)         ; => #f
```

### `record?`

Test if a value is a record instance.

```sema
(record? my-record)   ; => #t
(record? 42)          ; => #f
```

### `bytevector?`

Test if a value is a bytevector.

```sema
(bytevector? #u8())   ; => #t
(bytevector? '())     ; => #f
```

## Promise Predicates

### `promise?`

Test if a value is a promise (created with `delay`).

```sema
(promise? (delay 1))   ; => #t
(promise? 42)          ; => #f
```

### `promise-forced?`

Test if a promise has been forced (evaluated).

```sema
(define p (delay (+ 1 2)))
(promise-forced? p)   ; => #f
(force p)
(promise-forced? p)   ; => #t
```

## Equality

### `eq?`

Test structural equality. `equal?` is an alias.

```sema
(eq? 'a 'a)           ; => #t
(eq? '(1 2) '(1 2))   ; => #t
(eq? 1 2)             ; => #f
```

### `=`

Numeric equality.

```sema
(= 1 1)       ; => #t
(= 1 1.0)     ; => #t
(= 1 2)       ; => #f
```

## LLM Type Predicates

### `prompt?`

Test if a value is an LLM prompt.

```sema
(prompt? (prompt (user "hi")))   ; => #t
```

### `message?`

Test if a value is an LLM message.

```sema
(message? (message :user "hi"))   ; => #t
```

### `conversation?`

Test if a value is a conversation.

```sema
(conversation? (conversation/new {}))   ; => #t
```

### `tool?`

Test if a value is a tool definition.

```sema
(deftool my-tool "A test tool" {:x {:type :string}} (lambda (x) x))
(tool? my-tool)   ; => #t
(tool? 42)        ; => #f
```

### `agent?`

Test if a value is an agent.

```sema
(defagent my-agent {:system "test"})
(agent? my-agent)   ; => #t
(agent? 42)         ; => #f
```
