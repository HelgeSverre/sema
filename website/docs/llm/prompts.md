---
outline: [2, 3]
---

# Prompts & Messages

Prompts in Sema are composable data structures — not string templates. They are built from message expressions and can be inspected, transformed, and composed before being sent to an LLM.

## Messages

### `message`

Create a message with a role and content.

```scheme
(message :system "You are a helpful assistant.")
(message :user "What is Lisp?")
(message :assistant "Lisp is a family of programming languages.")
```

### `message/role`

Get the role of a message.

```scheme
(message/role (message :user "hi"))     ; => :user
```

### `message/content`

Get the content of a message.

```scheme
(message/content (message :user "hi"))  ; => "hi"
```

## Prompts

### `prompt`

Build a prompt from message expressions. This creates a composable data structure.

```scheme
(define review-prompt
  (prompt
    (system "You are a code reviewer. Be concise.")
    (user "Review this function.")))
```

### `prompt/append`

Compose prompts by appending one to another.

```scheme
(define base (prompt (system "You are helpful.")))
(define question (prompt (user "What is 2+2?")))
(define full (prompt/append base question))

(llm/send full)
```

### `prompt/messages`

Get the list of messages from a prompt.

```scheme
(prompt/messages my-prompt)   ; => list of message values
```

### `prompt/set-system`

Create a new prompt with the system message replaced.

```scheme
(prompt/set-system my-prompt "new system instructions")
```

## Type Predicates

### `prompt?`

Check if a value is a prompt.

```scheme
(prompt? review-prompt)   ; => #t
```

### `message?`

Check if a value is a message.

```scheme
(message? (message :user "hi"))   ; => #t
```
