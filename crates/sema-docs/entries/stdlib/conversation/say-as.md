---
name: "conversation/say-as"
module: "conversation"
params: [{ name: conv, type: conversation }, { name: system }, { name: message, type: string }, { name: opts, type: map }]
returns: "conversation"
---

Like `conversation/say` but overrides the system prompt (a string or prompt value) for **this turn only** — the conversation's own system message is left unchanged in the returned conversation, so the next plain `conversation/say` reverts to it. The optional opts map accepts `:temperature` and `:max-tokens`.

Contrast with `conversation/set-system`, which *persistently* replaces the system message for all future turns. Reach for `say-as` to adopt a persona or instruction for a single exchange.

```sema
(conversation/say-as conv "You are a terse pirate." "Describe the weather.")
```
