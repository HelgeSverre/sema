---
name: "conversation/new"
module: "conversation"
params: [{ name: opts, type: map }]
returns: "conversation"
---

Create a new, empty conversation — a value that bundles a model, an optional system prompt, and an ordered list of messages. The optional map sets the `:model`; any other keys are stored as string metadata.

Conversations are **immutable**. Every operation (`conversation/say`, `conversation/add-message`, `conversation/set-system`, …) returns a *new* conversation rather than mutating in place, so you must thread the returned value forward — `let*` or a pipeline is the idiomatic way to chain turns. This immutability is what makes `conversation/fork` cheap and safe for branching.

```sema
(conversation/new {:model "claude-sonnet-4" :user "alice"})

;; Build up a conversation by threading the returned value (no LLM call here):
(let* ((c  (conversation/new {:model "claude-sonnet-4"}))
       (c1 (conversation/set-system c "You are concise."))
       (c2 (conversation/add-message c1 :user "Hello"))
       (c3 (conversation/add-message c2 :assistant "Hi.")))
  (conversation/map c3 message/role))  ; => (:system :user :assistant)
```
