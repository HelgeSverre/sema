---
name: "conversation/set-system"
module: "conversation"
params: [{ name: conv, type: conversation }, { name: system, type: string }]
returns: "conversation"
---

Return a new conversation with its system message set to (or replaced by) the given string, placed first among the messages. This **persists** for every future turn — contrast with `conversation/say-as`, which overrides the system prompt for one turn only. Read it back with `conversation/system`.

```sema
(conversation/system
  (conversation/set-system conv "You are a concise expert."))  ; => "You are a concise expert."
```
