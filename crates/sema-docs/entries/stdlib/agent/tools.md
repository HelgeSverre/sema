---
name: "agent/tools"
module: "agent"
params: [{ name: agent, type: agent }]
returns: "list"
---

Return the agent's tools as a list of tool values (the [`deftool`](/docs/special-forms/deftool) values passed in `:tools`). An agent with no tools returns `()`.

```sema
(deftool get-weather "Get weather for a city"
  {:city :string}
  (fn (args) (str "sunny in " (:city args))))

(defagent weather-bot
  {:system "Report the weather."
   :model "claude-haiku-4-5-20251001"
   :tools [get-weather]})

(count (agent/tools weather-bot))   ; => 1
(agent/tools weather-bot)           ; => (<tool get-weather>)
```

See also: `defagent`, `deftool`, `agent/run`.
