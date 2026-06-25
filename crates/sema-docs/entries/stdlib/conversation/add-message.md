---
name: "conversation/add-message"
module: "conversation"
params: [{ name: conv, type: conversation }, { name: role, type: keyword }, { name: content, type: string }]
returns: "conversation"
---

Append a message with the given role to the conversation **without calling the model**, returning a new conversation. Role must be `:system`, `:user`, `:assistant`, or `:tool`. Use it to seed a conversation with prior turns, inject a synthetic assistant message, or replay a saved transcript — anywhere you want to grow the history but not pay for an LLM call. Contrast with `conversation/say`, which appends a user message *and* the model's generated reply.

```sema
(-> (conversation/new {:model "claude-sonnet-4"})
    (conversation/add-message :user "2+2?")
    (conversation/add-message :assistant "4"))
```
