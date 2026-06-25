---
name: "prompt/append"
module: "prompt"
params: [{ name: prompts, type: prompt }]
returns: "prompt"
---

Concatenate two or more prompts into a single prompt, preserving message order. Variadic; takes one or more prompt values.

Use this to build a conversation from reusable pieces — a shared system prompt, a few-shot example block, and the live user turn — without mutating any of them. The result is a fresh prompt; the inputs are untouched.

```sema
(define sys (prompt (system "You are a careful code reviewer.")))
(define ask (prompt (user "Review this diff.")))
(prompt/messages (prompt/append sys ask))
; => (<message system "You are a careful code reviewer."> <message user "Review this diff.">)
```

`prompt/concat` is an exact alias. Note that `append` does *not* merge or dedupe system messages — if two inputs each carry one you get two system messages in the result; use `prompt/set-system` to force a single leading system message.
