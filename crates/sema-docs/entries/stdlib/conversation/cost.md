---
name: "conversation/cost"
module: "conversation"
params: [{ name: conv, type: conversation }]
returns: "float"
---

Estimate the cost in USD of the conversation, treating its estimated tokens (~4 chars per token, the same heuristic as `conversation/token-count`) as input tokens for the conversation's model. This is a rough planning aid, not a billing figure: it ignores output-token pricing and the provider's real tokenizer. Returns `nil` if pricing for the model is unknown.

```sema
(conversation/cost conv)   ; => 0.0014
```
