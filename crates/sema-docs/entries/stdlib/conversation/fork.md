---
name: "conversation/fork"
module: "conversation"
params: [{ name: conv, type: conversation }]
returns: "conversation"
---

Return an independent copy of the conversation, sharing its history up to this point but diverging from here on. Use it to explore alternative continuations — ask the same context two different follow-ups, A/B different system prompts, or speculatively branch — without disturbing the original thread.

Because conversations are already immutable, `conversation/say` on the original would *also* leave it untouched; `fork` mainly signals intent and gives each branch its own name to thread forward. Each branch is a separate value you continue independently.

```sema
;; Explore two follow-ups from the same shared context:
(let* ((a (conversation/say conv "Summarize it formally."))
       (b (conversation/say (conversation/fork conv) "Summarize it casually.")))
  (list (conversation/last-reply a) (conversation/last-reply b)))
```
