---
name: "conversation/say"
module: "conversation"
params: [{ name: conv, type: conversation }, { name: message, type: string }, { name: opts, type: map }]
returns: "conversation"
---

Send a user message to the conversation's model and return a **new** conversation with both the user message and the assistant's reply appended. This is the one conversation op that calls the LLM (and so costs tokens). The optional opts map accepts `:temperature`, `:max-tokens`, and `:system`.

Because it returns a new value (the original is untouched), you must capture the result to keep the thread going — chain multi-turn dialogues with `let*` or a `->`/pipeline, then read the answer with `conversation/last-reply`.

```sema
;; Single turn:
(conversation/say conv "What's the capital of France?" {:temperature 0.2})

;; Multi-turn — thread the returned conversation through each call:
(let* ((c1 (conversation/say conv "Name a primary color."))
       (c2 (conversation/say c1 "Now name another.")))
  (conversation/last-reply c2))
```

See also: `conversation/say-as` for a one-off system prompt, `conversation/add-message` to append a turn **without** calling the model.
