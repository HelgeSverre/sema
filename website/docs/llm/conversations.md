---
outline: [2, 3]
---

# Conversations

Conversations are immutable data structures that maintain chat history. Each operation returns a new conversation value — the original is never modified. This means you always re-bind the result:

```sema
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/set-system conv "You are a concise tutor."))
(define conv (conversation/say conv "Explain closures in 2 bullets."))
(println (conversation/last-reply conv))

;; Branch to explore a different direction
(define alt (conversation/fork conv))
(define alt (conversation/say alt "Now explain with JavaScript examples."))
;; conv is unchanged — alt is an independent conversation
```

## Creating Conversations

### `conversation/new`

Create a new conversation, optionally with a model. `(conversation/new)` is equivalent to `(conversation/new {})`.

```sema
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/new))
```

## Interacting

### `conversation/say`

Send a user message to the LLM and get a response. Returns a new conversation with both the user message and the assistant's reply appended.

```sema
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/say conv "Remember: the secret number is 7"))
(define conv (conversation/say conv "What is the secret number?"))
(conversation/last-reply conv)          ; => "The secret number is 7."
```

With options:

```sema
(define conv (conversation/say conv "Explain more"
  {:temperature 0.5 :max-tokens 500}))
```

### `conversation/add-message`

Manually add a message without making an LLM call. Useful for constructing conversation history programmatically.

```sema
(define c (conversation/new))
(define c (conversation/add-message c :system "You are helpful."))
(define c (conversation/add-message c :user "hello"))
(define c (conversation/add-message c :assistant "hi there"))
```

### `conversation/say-as`

Send a message with a different system prompt for one turn only. The override applies to the API call but doesn't change the conversation's stored system message. Accepts a system string or a prompt value.

```sema
;; With a prompt value — uses its system message for this turn
(define argue-for (prompt (system "You argue IN FAVOR of Lisp.")))
(define conv (conversation/new {:model "claude-sonnet-4-20250514"}))
(define conv (conversation/say-as conv argue-for "Make your case."))

;; With a plain string — treated as system content
(define conv (conversation/say-as conv "You argue AGAINST Lisp." "Rebut the argument."))
```

## Inspecting

### `conversation/last-reply`

Get the content of the last assistant message.

```sema
(conversation/last-reply conv)   ; => "The secret number is 7."
```

### `conversation/messages`

Get the full list of messages as message values.

```sema
(conversation/messages conv)   ; => list of message values
(length (conversation/messages conv))  ; => 5
```

### `conversation/model`

Get the model associated with the conversation.

```sema
(conversation/model conv)   ; => "claude-haiku-4-5-20251001"
```

## System Message

### `conversation/system`

Get the system message content, or `nil` if none is set.

```sema
(define c (conversation/add-message (conversation/new) :system "Be helpful."))
(conversation/system c)                 ; => "Be helpful."
(conversation/system (conversation/new))  ; => nil
```

### `conversation/set-system`

Set or replace the system message. All existing system messages are replaced with one new one; other messages are preserved.

```sema
(define c (conversation/set-system (conversation/new) "You are a code reviewer."))
(conversation/system c)   ; => "You are a code reviewer."
```

## Filtering & Transforming

### `conversation/filter`

Keep only messages matching a predicate. Returns a new conversation.

```sema
;; Keep only user messages
(define user-only
  (conversation/filter conv (fn (m) (= (message/role m) :user))))

;; Remove system messages
(define no-system
  (conversation/filter conv (fn (m) (not (= (message/role m) :system)))))
```

### `conversation/map`

Apply a function to each message, returning a list of results (not a conversation).

```sema
;; Extract all message contents
(conversation/map conv message/content)

;; Build a summary with role prefixes
(conversation/map conv
  (fn (m) (string-append "[" (keyword->string (message/role m)) "] "
    (message/content m))))
```

## Usage & Cost

### `conversation/token-count`

Estimated token count for the conversation (heuristic: ~4 characters per token).

```sema
(conversation/token-count conv)   ; => 342
```

### `conversation/cost`

Estimated input cost in dollars based on the conversation's model pricing. Returns `nil` if pricing is unavailable for the model.

```sema
(conversation/cost conv)   ; => 0.00034 (or nil)
```

## Branching

### `conversation/fork`

Create an independent copy of a conversation. Since conversations are immutable, forking lets you explore different directions from the same point.

```sema
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/say conv "Remember the number 7"))

;; Fork and take two different paths
(define branch-a (conversation/say (conversation/fork conv) "What about Python?"))
(define branch-b (conversation/say (conversation/fork conv) "What about Rust?"))
;; conv, branch-a, branch-b are all independent
```

## Type Predicate

### `conversation?`

Check if a value is a conversation.

```sema
(conversation? conv)   ; => #t
(conversation? 42)     ; => #f
```
