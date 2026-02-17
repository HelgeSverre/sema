---
outline: [2, 3]
---

# Context

Sema provides an ambient context system — a key-value store that flows through your entire execution without explicit parameter passing. Inspired by [Laravel's Context](https://laravel.com/docs/12.x/context), it's designed for tracing, metadata propagation, and sharing configuration across deeply nested calls.

Context data is automatically appended as metadata to log output (`log/info`, `log/warn`, `log/error`, `log/debug`).

## Core Functions

### `context/set`

Set a key-value pair in the current context frame.

```scheme
(context/set :trace-id "abc-123")
(context/set :user-id 42)
```

### `context/get`

Retrieve a value by key. Returns `nil` if the key doesn't exist.

```scheme
(context/get :trace-id)   ; => "abc-123"
(context/get :missing)    ; => nil
```

### `context/has?`

Check if a key exists in the context.

```scheme
(context/has? :trace-id)  ; => #t
(context/has? :missing)   ; => #f
```

### `context/remove`

Remove a key from all context frames. Returns the removed value, or `nil`.

```scheme
(context/set :temp "data")
(context/remove :temp)    ; => "data"
(context/remove :temp)    ; => nil (already gone)
```

### `context/pull`

Get a value and remove it in one step (identical to `context/remove`).

```scheme
(context/set :token "abc")
(context/pull :token)     ; => "abc"
(context/has? :token)     ; => #f
```

### `context/all`

Get all context as a merged map.

```scheme
(context/set :a 1)
(context/set :b 2)
(context/all)  ; => {:a 1 :b 2}
```

### `context/merge`

Merge a map of key-value pairs into the current context.

```scheme
(context/merge {:trace-id "abc" :env "production" :version "1.0"})
(context/get :env)  ; => "production"
```

### `context/clear`

Clear all context, resetting to an empty state.

```scheme
(context/clear)
(context/all)  ; => {}
```

## Scoped Overrides

### `context/with`

Push a temporary context frame for the duration of a thunk. The frame is automatically popped when the thunk completes — even if it raises an error.

```scheme
(context/set :env "production")

(context/with {:env "staging" :debug #t}
  (lambda ()
    (context/get :env)      ; => "staging"
    (context/get :debug)))  ; => #t

(context/get :env)    ; => "production" (restored)
(context/get :debug)  ; => nil (gone)
```

Scopes nest naturally — inner values shadow outer ones:

```scheme
(context/set :a 1)
(context/with {:b 2}
  (lambda ()
    (context/with {:c 3}
      (lambda ()
        (list (context/get :a) (context/get :b) (context/get :c))))))
; => (1 2 3)
```

::: warning
Values set with `context/set` inside a `context/with` block are written to the inner frame and discarded when the scope exits. If you need a value to persist, set it before entering `context/with`.
:::

## Stacks

Context stacks are ordered lists of values that you can push to and pop from. Unlike key-value context, stacks are **not scoped** by `context/with` — pushes persist across scope boundaries.

### `context/push`

Append a value to a named stack.

```scheme
(context/push :breadcrumbs "login")
(context/push :breadcrumbs "dashboard")
(context/push :breadcrumbs "settings")
```

### `context/stack`

Get all values in a named stack as a list.

```scheme
(context/stack :breadcrumbs)
; => ("login" "dashboard" "settings")
```

### `context/pop`

Remove and return the last value from a stack. Returns `nil` if the stack is empty.

```scheme
(context/pop :breadcrumbs)  ; => "settings"
(context/stack :breadcrumbs)
; => ("login" "dashboard")
```

## Hidden Context

Hidden context stores values that are **not visible** via `context/get`, `context/all`, or log metadata. Use it for sensitive data like API keys or internal state.

### `context/set-hidden`

```scheme
(context/set-hidden :api-key "sk-secret-123")
```

### `context/get-hidden`

```scheme
(context/get-hidden :api-key)  ; => "sk-secret-123"
(context/get :api-key)         ; => nil (not visible in regular context)
```

### `context/has-hidden?`

```scheme
(context/has-hidden? :api-key)  ; => #t
```

## Log Integration

When context is non-empty, `log/info`, `log/warn`, `log/error`, and `log/debug` automatically append the context map as metadata:

```scheme
(context/set :trace-id "abc-123")
(context/set :user-id 42)
(log/info "Request processed")
```

Output:
```
[INFO] Request processed {:trace-id "abc-123" :user-id 42}
```

Hidden context is **not** included in log output.

## Examples

### Request tracing

```scheme
(context/set :request-id (uuid/v4))
(context/set :method "GET")
(context/set :path "/api/users")

(log/info "Request started")
; [INFO] Request started {:method "GET" :path "/api/users" :request-id "a1b2c3..."}

;; All downstream functions automatically include this context in their logs
(process-request)
```

### Pipeline breadcrumbs

```scheme
(define (process-document doc)
  (context/push :steps "parse")
  (let ((parsed (parse doc)))
    (context/push :steps "validate")
    (let ((valid (validate parsed)))
      (context/push :steps "transform")
      (transform valid))))

(process-document input)
(context/stack :steps)
; => ("parse" "validate" "transform")
```

### Scoped configuration

```scheme
;; Set default model
(context/set :model "claude-sonnet")

;; Override for a specific block
(context/with {:model "gpt-4o" :temperature 0.9}
  (lambda ()
    ;; Code here sees the overridden values
    (context/get :model)))       ; => "gpt-4o"

(context/get :model)             ; => "claude-sonnet"
```

## Function Reference

| Function | Args | Description |
|---|---|---|
| `context/set` | `key value` | Set a context value |
| `context/get` | `key` | Get a value (or `nil`) |
| `context/has?` | `key` | Check if key exists |
| `context/remove` | `key` | Remove and return value |
| `context/pull` | `key` | Get and remove (alias for remove) |
| `context/all` | | Get all context as a map |
| `context/merge` | `map` | Merge map into context |
| `context/clear` | | Clear all context |
| `context/with` | `map thunk` | Scoped override |
| `context/push` | `key value` | Push to named stack |
| `context/stack` | `key` | Get stack as list |
| `context/pop` | `key` | Pop from named stack |
| `context/set-hidden` | `key value` | Set hidden value |
| `context/get-hidden` | `key` | Get hidden value |
| `context/has-hidden?` | `key` | Check hidden key exists |
