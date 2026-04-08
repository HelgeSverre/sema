# LLM Integration

Sema Web brings the full `llm/*` namespace to the browser. Since API keys cannot be safely stored in client-side code, all LLM calls are routed through a lightweight proxy server that holds the keys and forwards requests to the actual provider.

## Architecture

```
Browser (Sema code)              Your server
┌──────────────────────┐         ┌──────────────────────┐
│ (llm/chat ...)       │──POST──>│ /api/llm/chat        │──> OpenAI / Anthropic / ...
│ (llm/complete ...)   │──POST──>│ /api/llm/complete    │──> ...
│ (llm/embed ...)      │──POST──>│ /api/llm/embed       │──> ...
│ (llm/chat-stream ..) │──POST──>│ /api/llm/stream      │──> SSE tokens ──> signal
└──────────────────────┘         └──────────────────────┘
```

The proxy is a tiny server-side function you deploy alongside your app. See the [LLM Proxy](./llm-proxy) page for setup instructions.

## Configuration

Pass the `llmProxy` option when creating a Sema Web instance:

```js
const web = await SemaWeb.create({
  autoLoad: false,
  llmProxy: "https://your-backend.com/api/llm",
});
```

If your proxy requires authentication (recommended for production), also pass a token:

```js
const web = await SemaWeb.create({
  autoLoad: false,
  llmProxy: {
    url: "https://your-backend.com/api/llm",
    token: "user-session-token",
  },
});
```

The token is sent as `Authorization: Bearer {token}` on every request. This authenticates the **browser client to your proxy** -- it is not the LLM API key.

## API Reference

All LLM functions are asynchronous (they make HTTP requests). Use `evalAsync()` from JavaScript, or call them from within Sema components where async is handled automatically.

### `(llm/complete prompt opts?)`

Send a simple text prompt and get a completion string back.

```scheme
(llm/complete "Explain monads in one sentence")
;; => "A monad is a design pattern that..."

;; With options:
(llm/complete "Translate to French: hello"
  {:model "gpt-4o" :temperature 0.3})
```

**Parameters:**
- `prompt` -- string, the text prompt
- `opts` -- optional map with `:model`, `:temperature`, `:max-tokens`, `:system`

**Returns:** string (the completion text)

### `(llm/chat messages opts?)`

Chat completion with a list of messages. This is the most commonly used function.

```scheme
(llm/chat
  (list (message :system "You are a helpful assistant.")
        (message :user "What is the capital of France?"))
  {:model "claude-sonnet-4-20250514"})
;; => "The capital of France is Paris."
```

**Parameters:**
- `messages` -- list of message maps (use the `message` helper)
- `opts` -- optional map with `:model`, `:temperature`, `:max-tokens`

**Returns:** string (the assistant's response text)

### `(llm/chat-stream messages opts?)`

Streaming chat completion. Returns a **reactive signal** that updates progressively as tokens arrive from the server. Components that `deref` the signal automatically re-render.

```scheme
(def stream (llm/chat-stream
  (list (message :user "Write a poem about Lisp"))
  {:model "gpt-4o"}))

;; The signal value is a map:
(deref stream)
;; => {:text "In paren..." :done false :error nil}

;; When complete:
(deref stream)
;; => {:text "In parentheses we trust..." :done true :error nil}
```

**Parameters:**
- `messages` -- list of message maps
- `opts` -- optional map with `:model`, `:temperature`, `:max-tokens`

**Returns:** signal ID. Deref to get `{:text string :done boolean :error string-or-nil}`.

Use `(llm/close-stream signal-id)` to abort an in-flight stream explicitly when needed.

This is the only LLM function that works synchronously (it returns immediately with a signal). All other `llm/*` functions require async evaluation.

### `(llm/extract schema text opts?)`

Extract structured data from text using a JSON schema.

```scheme
(llm/extract
  {:name "string" :age "number" :hobbies ["string"]}
  "My name is Alice, I'm 30, and I like hiking and painting.")
;; => {:name "Alice" :age 30 :hobbies ["hiking" "painting"]}
```

**Parameters:**
- `schema` -- map describing the expected shape (JSON Schema-like)
- `text` -- the text to extract from
- `opts` -- optional map with `:model`, `:max-tokens`

**Returns:** map matching the schema

### `(llm/classify categories text opts?)`

Classify text into one of the given categories.

```scheme
(llm/classify
  ["positive" "negative" "neutral"]
  "I absolutely love this product!")
;; => {:category "positive"}
```

**Parameters:**
- `categories` -- list of category strings
- `text` -- the text to classify
- `opts` -- optional map with `:model`

**Returns:** map with `:category` key

### `(llm/embed text opts?)`

Get a vector embedding for text.

```scheme
(llm/embed "The quick brown fox")
;; => {:embedding [0.0123 -0.0456 ...] :model "text-embedding-3-small"}
```

**Parameters:**
- `text` -- the text to embed
- `opts` -- optional map with `:model`

**Returns:** map with `:embedding` (list of numbers) and `:model`

::: warning
Anthropic does not support embeddings. Use OpenAI, Gemini, Mistral, or Ollama for embedding operations.
:::

### `(llm/list-models)`

List available models from the configured provider.

```scheme
(llm/list-models)
;; => {:models ["gpt-4o" "gpt-4o-mini" "gpt-3.5-turbo" ...]}
```

**Returns:** map with `:models` (list of model name strings)

### `(message role content)`

Helper function for building chat message maps.

```scheme
(message :user "Hello!")
;; => {:role "user" :content "Hello!"}

(message :system "You are a pirate.")
;; => {:role "system" :content "You are a pirate."}

(message :assistant "Ahoy!")
;; => {:role "assistant" :content "Ahoy!"}
```

**Parameters:**
- `role` -- keyword or string: `:user`, `:assistant`, `:system`
- `content` -- string, the message content

## Streaming Chat UI Example

Here is a complete example that builds a chat interface with progressive token display using `llm/chat-stream` and the reactive system:

```scheme
;; ai-chat.sema — Streaming chat with progressive rendering

(define messages (atom '()))
(define input-text (atom ""))
(define current-stream (atom nil))

(define (send-message)
  (let ((text (deref input-text)))
    (when (not (string=? text ""))
      ;; Add user message to history
      (swap! messages #(append % (list {:role "user" :content text})))
      (put! input-text "")

      ;; Start streaming response
      (let ((stream (llm/chat-stream
                      (map #(message (string->keyword (:role %)) (:content %))
                           (deref messages))
                      {:model "gpt-4o"})))
        (put! current-stream stream)))))

(define (chat-messages)
  (sip (:div.messages
    (for-each #(sip
      (:div {:class (string-append "msg " (:role %))}
        (:p (:content %))))
      (deref messages))
    ;; Show streaming response
    (when (deref current-stream)
      (let ((s (deref (deref current-stream))))
        (when (:text s)
          (sip (:div.msg.assistant
            (:p (:text s))
            (unless (:done s)
              (sip (:span.typing "...")))))))))))

(define (chat-input)
  (sip (:div.input-row
    (:input {:value (deref input-text)
             :on-input #(put! input-text (.-value (.-target %)))
             :on-keydown #(when (string=? (.-key %) "Enter") (send-message))
             :placeholder "Type a message..."})
    (:button {:on-click send-message} "Send"))))

(define (app)
  (sip (:div.chat-app
    (chat-messages)
    (chat-input))))

(mount! (app) "#app")
```

The key insight is that `llm/chat-stream` returns a signal. When you `deref` it inside a component, that component automatically re-renders as new tokens arrive. No manual polling or callback wiring is needed.

## Options Reference

All `llm/*` functions accept an optional options map as the last argument:

| Key | Type | Description |
|-----|------|-------------|
| `:model` | string | Model identifier (e.g. `"gpt-4o"`, `"claude-sonnet-4-20250514"`) |
| `:temperature` | number | Sampling temperature (0.0 to 2.0) |
| `:max-tokens` | number | Maximum tokens in the response |
| `:system` | string | System prompt (for `llm/complete` and `llm/chat`) |

If no model is specified, the proxy's configured default model is used.
