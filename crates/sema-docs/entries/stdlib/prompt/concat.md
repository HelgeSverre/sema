---
name: "prompt/concat"
module: "prompt"
params: [{ name: prompts, type: prompt }]
returns: "prompt"
---

Alias for `prompt/append`: concatenate one or more prompts into a single prompt, preserving message order. See `prompt/append` for the full behavior (system messages are *not* merged).

```sema
(prompt/messages
  (prompt/concat (prompt (user "a")) (prompt (user "b"))))
; => (<message user "a"> <message user "b">)
```
