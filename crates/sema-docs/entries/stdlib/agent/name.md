---
name: "agent/name"
module: "agent"
params: [{ name: agent, type: agent }]
returns: "string"
---

Return the name of an agent (the symbol it was bound to by [`defagent`](/docs/special-forms/defagent)).

```sema
(defagent weather-bot {:system "Report the weather." :model "claude-haiku-4-5-20251001"})
(agent/name weather-bot)   ; => "weather-bot"
```

See also: `agent/model`, `agent/system`, `agent/tools`, `agent/max-turns`, `agent/run`.
