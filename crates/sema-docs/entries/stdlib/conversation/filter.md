---
name: "conversation/filter"
module: "conversation"
params: [{ name: conv, type: conversation }, { name: pred }]
returns: "conversation"
---

Return a new conversation keeping only messages for which `(pred msg)` is truthy. The predicate receives each message value. The result is still a conversation (so you can keep talking to it) — unlike `conversation/map`, which returns a plain list of per-message results. Combine with `message/role` to drop or isolate roles, e.g. strip tool chatter before re-sending.

```sema
;; Keep only the user turns.
(conversation/filter conv (fn [m] (= (message/role m) :user)))
```
