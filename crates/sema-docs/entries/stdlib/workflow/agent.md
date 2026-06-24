---
name: "agent"
module: "workflow"
section: "Dynamic Workflows"
syntax: "(agent prompt [opts])"
---

Macro: a journaled LLM **agent** leaf (Claude Code `workflow.js` `agent(prompt, {schema})`
semantics). Runs `prompt` through the configured provider and returns **typed data** when
`opts` carries a `:schema` (validated via [`llm/extract`](/docs/stdlib/llm/extract)), or
the completion text otherwise — so the next stage of a [`pipeline`](/docs/stdlib/concurrency/pipeline)
can consume the result directly without re-parsing. `opts` also carries `:name`, the agent
role shown in the dashboard (default `"agent"`).

The call is wrapped by [`workflow/agent`](/docs/stdlib/workflow/workflow-agent), which
emits `agent.started`/`agent.result` plus a per-agent `budget` event, so each invocation
becomes a correlated agent row under the current phase. Outside a `workflow/run` the
journaling is transparent — the LLM call still runs.

With `:tools [...]` (a list of [`deftool`](/docs/special-forms/deftool) values) the leaf
runs the real multi-round tool loop and journals **each genuine tool call** as an
`agent.tool_call` event (a tool twig in the drill-in). `:tools` returns the loop's final
text; it does not compose with `:schema` yet. Per-agent budget for a multi-round tool
agent is best-effort (the Budget event reflects the final round's usage).

```sema
;; typed: returns the parsed list of strings
(agent "List the auth-relevant source files under src/."
       {:name "scout" :schema [:list :string]})

;; untyped: returns completion text
(agent "Summarize the changelog in one line.")

;; fanned out — one agent row per item, two stages overlapping across items
(pipeline files
  (fn (f) (agent (str "Audit " f) {:name "auditor" :schema finding}))
  (fn (x) (agent (str "Verify " (:claim x)) {:name "verifier" :schema verdict})))
```

See also: `pipeline`, `parallel`, `defworkflow`, `checkpoint`.
