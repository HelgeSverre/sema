---
outline: [2, 3]
---

# Completion & Chat

## Completion

### `llm/complete`

Send a single prompt string and get a completion back.

```scheme
;; Simple completion
(llm/complete "Say hello in 5 words" {:max-tokens 50})
```

With options:

```scheme
(llm/complete "Explain monads"
  {:model "claude-haiku-4-5-20251001"
   :max-tokens 200
   :temperature 0.3
   :system "You are a Haskell expert."})
```

### `llm/stream`

Stream a completion, printing chunks as they arrive.

```scheme
(llm/stream "Tell me a story" {:max-tokens 200})
```

With a callback function:

```scheme
(llm/stream "Tell me a story"
  (fn (chunk) (display chunk))
  {:max-tokens 200})
```

## Chat

### `llm/chat`

Send a list of messages and get a response. Supports system, user, and assistant messages.

```scheme
(llm/chat
  [(message :system "You are a helpful assistant.")
   (message :user "What is Lisp? One sentence.")]
  {:max-tokens 100})
```

### Multi-Modal Chat

Send messages that include images alongside text using `message/with-image`.

```scheme
;; Load an image and ask the LLM about it
(define img (file/read-bytes "photo.jpg"))
(define msg (message/with-image :user "Describe this image." img))
(llm/chat (list msg))
```

Combine with regular messages:

```scheme
(llm/chat
  [(message :system "You are an image analyst.")
   (message/with-image :user "What text is in this image?" (file/read-bytes "doc.png"))])
```

The image must be a bytevector. Media type (PNG, JPEG, GIF, WebP, PDF) is detected automatically from magic bytes. See [Vision Extraction](./extraction.md#vision-extraction) for structured data extraction from images.

### `llm/send`

Send a prompt value (composed from `prompt` expressions) to the LLM.

```scheme
(define review-prompt
  (prompt
    (system "You are a code reviewer. Be concise.")
    (user "Review this function.")))

(llm/send review-prompt {:max-tokens 200})
```

## Options

All completion and chat functions accept an options map with these keys:

| Key            | Description                                                   |
| -------------- | ------------------------------------------------------------- |
| `:model`       | Model name (e.g. `"claude-haiku-4-5-20251001"`)               |
| `:max-tokens`  | Maximum tokens in response                                    |
| `:temperature` | Sampling temperature (0.0–1.0)                                |
| `:system`      | System prompt (for `llm/complete`)                            |
| `:tools`       | List of tool values (see [Tools & Agents](./tools-agents.md)) |
