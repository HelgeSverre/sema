---
outline: [2, 3]
---

# Tools & Agents

## Tools

Tools let you define functions that the LLM can invoke during a conversation. The LLM sees the tool's name, description, and parameter schema, and can call it when appropriate.

### `deftool`

Define a tool with a name, description, parameter schema, and handler function.

```sema
(deftool lookup-capital
  "Look up the capital of a country"
  {:country {:type :string :description "Country name"}}
  (lambda (country)
    (cond
      ((= country "Norway") "Oslo")
      ((= country "France") "Paris")
      (else "Unknown"))))
```

### Using Tools with Chat

Pass tools to `llm/chat` — the LLM will call them automatically when needed.

```sema
(llm/chat
  (list (message :user "What is the capital of Norway?"))
  {:tools (list lookup-capital) :max-tokens 100})
```

### Inspecting Tools

### `tool/name`

```sema
(tool/name lookup-capital)              ; => "lookup-capital"
```

### `tool/description`

```sema
(tool/description lookup-capital)       ; => "Look up the capital..."
```

### `tool/parameters`

```sema
(tool/parameters lookup-capital)        ; => {:country {:type :string ...}}
```

### `tool?`

```sema
(tool? lookup-capital)                  ; => #t
```

## Agents

Agents combine a system prompt, tools, and a multi-turn loop. They handle the back-and-forth of tool calls automatically.

### `defagent`

Define an agent with a system prompt, tools, model, and turn limit.

```sema
(deftool get-weather
  "Get weather for a city"
  {:city {:type :string}}
  (lambda (city)
    (format "~a: 22°C, sunny" city)))

(defagent weather-bot
  {:system "You are a weather assistant. Use the get-weather tool."
   :tools [get-weather]
   :model "claude-haiku-4-5-20251001"
   :max-turns 3})
```

### `agent/run`

Run an agent with a user message. The agent loops, calling tools as needed, until it has a final answer or hits the turn limit. The two-argument form returns the final answer as a **string**:

```sema
(agent/run weather-bot "What's the weather in Tokyo?")  ; => "It's sunny, 22°C."
```

An optional third argument takes per-run options. **Passing an options map changes the return value** to a map with the final reply *and* the full message history:

```sema
(define result
  (agent/run weather-bot "What's the weather in Tokyo?"
    {:reasoning-effort :high       ; reasoning effort for this run (see Completion)
     :messages prior-history       ; seed the loop with prior conversation
     :on-tool-call observe-tool})) ; observe each tool call — see below

(:response result)   ; => the final answer string
(:messages result)   ; => the full conversation (to continue or inspect)
```

**Observing tool calls.** `:on-tool-call` fires once when each tool starts and once when it ends. The event is a map — branch on `(:event e)`, the string `"start"` or `"end"`:

```sema
(define (observe-tool e)
  (when (= (:event e) "end")
    (println (:tool e) "→" (:result e) (format "(~ams)" (:duration-ms e)))))
```

The event map carries `:event` (`"start"` / `"end"`), `:tool` (the tool name), and `:args`; on `"end"` it adds `:result` (a preview of the return value), `:error` (a boolean), and `:duration-ms`.

**Error recovery.** A tool that throws, isn't found, or is called with arguments
that don't match its declared schema does **not** abort the run — the error is
fed back to the model as the tool result so it can correct itself and continue.
The loop is bounded by `:max-turns` and aborts after 5 consecutive tool errors.

### Inspecting Agents

### `agent/name`

```sema
(agent/name weather-bot)                ; => "weather-bot"
```

### `agent/system`

```sema
(agent/system weather-bot)              ; => "You are a weather assistant..."
```

### `agent/tools`

```sema
(agent/tools weather-bot)               ; => list of tool values
```

### `agent/model`

```sema
(agent/model weather-bot)               ; => "claude-haiku-4-5-20251001"
```

### `agent/max-turns`

```sema
(agent/max-turns weather-bot)           ; => 3
```

### `agent?`

```sema
(agent? weather-bot)                    ; => #t
```
