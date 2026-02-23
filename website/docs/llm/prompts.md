---
outline: [2, 3]
---

# Prompts & Messages

Prompts in Sema are composable data structures — not string templates. They are built from message expressions, and can be inspected, transformed, and composed before being sent to an LLM.

The core idea: build small prompt pieces, compose them together, fill in template slots, and send the result. Everything is a value you can pass around, store, and introspect.

```sema
;; Build reusable prompt pieces
(define safety
  (prompt (system "Follow policy. Refuse unsafe requests.")))

(define domain
  (prompt (system "You are a senior Lisp developer.")))

(define task
  (prompt (user "Review this function:\n\n{{code}}")))

;; Compose, fill, and send
(define p (prompt/concat safety domain task))
(define ready (prompt/fill p {:code "(define (f x) (+ x 1))"}))
(llm/send ready {:max-tokens 300})
```

## Messages

A message is a role–content pair. The role is a keyword: `:system`, `:user`, or `:assistant`.

### `message`

Create a message with a role and content.

```sema
(message :system "You are a helpful assistant.")
(message :user "What is Lisp?")
(message :assistant "Lisp is a family of programming languages.")
```

### `message/role`

Get the role of a message as a keyword.

```sema
(message/role (message :user "hi"))     ; => :user
```

### `message/content`

Get the text content of a message.

```sema
(message/content (message :user "hi"))  ; => "hi"
```

## Building Prompts

### `prompt`

Build a prompt from message expressions. Inside `prompt`, use the shorthand constructors `(system ...)`, `(user ...)`, and `(assistant ...)` — these are equivalent to `(message :system ...)`, etc.

```sema
(define review-prompt
  (prompt
    (system "You are a code reviewer. Be concise.")
    (user "Review this function.")))
```

### `prompt/messages`

Get the list of messages from a prompt.

```sema
(prompt/messages my-prompt)   ; => list of message values
(length (prompt/messages my-prompt))  ; => 2
```

## Composing Prompts

### `prompt/append`

Compose prompts by appending their messages together. Variadic — accepts 2 or more prompts.

```sema
(define base (prompt (system "You are helpful.")))
(define question (prompt (user "What is 2+2?")))
(define full (prompt/append base question))

;; Three or more prompts
(define safety (prompt (system "Be safe.")))
(define full (prompt/append base safety question))

(llm/send full)
```

### `prompt/concat`

Alias for `prompt/append`. Use whichever name reads better in context.

```sema
(define full (prompt/concat base-prompt safety-prompt domain-prompt))
```

## Templating

### `prompt/fill`

Substitute `{{key}}` placeholders in all message contents using a map. Unfilled slots are left as-is, so you can partially fill a template and fill the rest later.

```sema
(define template
  (prompt
    (system "You are a {{role}} reviewing {{language}} code.")
    (user "{{query}}")))

;; Full fill
(define filled (prompt/fill template {:role "expert" :language "Rust" :query "Explain this."}))

;; Partial fill — unfilled slots remain as {{...}}
(define partial (prompt/fill template {:role "code reviewer"}))
;; partial still has {{language}} and {{query}} unfilled
```

### `prompt/slots`

Return a list of unfilled `{{slot}}` names as keywords. Duplicates are removed.

```sema
(prompt/slots template)   ; => (:role :language :query)

;; After partial fill, only unfilled slots remain
(prompt/slots (prompt/fill template {:role "expert"}))
;; => (:language :query)

;; After full fill, no slots remain
(prompt/slots filled)     ; => ()
```

Use `prompt/slots` to validate that all required slots are filled before sending:

```sema
(when (not (null? (prompt/slots my-prompt)))
  (error "unfilled slots remain"))
```

## Modifying Prompts

### `prompt/set-system`

Replace all system messages with a single new one. Non-system messages are preserved.

```sema
(define p (prompt (system "old system") (user "hello")))
(define p2 (prompt/set-system p "new system instructions"))
;; p2 has: [(system "new system instructions"), (user "hello")]
```

## Type Predicates

### `prompt?`

Check if a value is a prompt.

```sema
(prompt? review-prompt)   ; => #t
(prompt? 42)              ; => #f
```

### `message?`

Check if a value is a message.

```sema
(message? (message :user "hi"))   ; => #t
(message? "not a message")       ; => #f
```
