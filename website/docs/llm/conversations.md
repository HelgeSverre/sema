---
outline: [2, 3]
---

# Conversations

Conversations are persistent, immutable data structures that maintain chat history with automatic LLM round-trips. Each operation returns a new conversation value — the original is never modified.

## Creating Conversations

### `conversation/new`

Create a new conversation, optionally with a default model.

```scheme
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/new {}))
```

## Interacting

### `conversation/say`

Send a message in the conversation. This sends the full history to the LLM and returns a new conversation with both the user message and the assistant's reply appended.

```scheme
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/say conv "Remember: the secret number is 7"))
(define conv (conversation/say conv "What is the secret number?"))
(conversation/last-reply conv)          ; => "The secret number is 7."
```

With options:

```scheme
(define conv (conversation/say conv "Explain more"
  {:temperature 0.5 :max-tokens 500}))
```

### `conversation/add-message`

Manually add a message to the conversation without making an LLM call.

```scheme
(define c (conversation/new {}))
(define c (conversation/add-message c :user "hello"))
(define c (conversation/add-message c :assistant "hi there"))
```

## Inspecting

### `conversation/last-reply`

Get the content of the last assistant message.

```scheme
(conversation/last-reply conv)
```

### `conversation/messages`

Get the full list of messages in the conversation.

```scheme
(conversation/messages conv)   ; => list of message values
```

### `conversation/model`

Get the model associated with the conversation.

```scheme
(conversation/model conv)   ; => "claude-haiku-4-5-20251001"
```

## Type Predicate

### `conversation?`

Check if a value is a conversation.

```scheme
(conversation? conv)   ; => #t
```
