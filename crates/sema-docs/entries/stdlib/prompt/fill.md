---
name: "prompt/fill"
module: "prompt"
params: [{ name: prompt, type: prompt }, { name: vars, type: map }]
returns: "prompt"
---

Substitute `{{key}}` placeholders in every message of the prompt using the vars map (keys looked up as keywords). Slots with no matching key are left unchanged, so a partial fill is safe and re-fillable.

The template stays reusable: `fill` returns a new prompt and never mutates the original, so one template can be filled many times with different vars.

```sema
(define template (prompt (user "Hi {{name}}, let's do {{topic}}")))
(prompt/messages (prompt/fill template {:name "Ada" :topic "loops"}))
; => (<message user "Hi Ada, let's do loops">)
```

A slot with no matching key is left verbatim — handy for filling in stages:

```sema
(prompt/messages (prompt/fill template {:name "Ada"}))
; => (<message user "Hi Ada, let's do {{topic}}">)
```

Use `prompt/slots` to discover which placeholder keys a template expects before filling.
