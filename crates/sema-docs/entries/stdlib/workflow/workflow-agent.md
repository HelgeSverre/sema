---
name: "workflow/agent"
module: "workflow"
section: "Dynamic Workflows"
---

Run a leaf step as a journaled **agent**: `(workflow/agent role thunk)` emits an
`agent.started` event before the thunk, an `agent.result` after (output + duration), and a
per-agent `budget` event, so the dashboard renders it as a correlated agent row under the
current phase. `role` is an opts map (`{:name "scout" …}`) or a bare label; the default
role is `"agent"`. Returns the thunk's value, or propagates its error after journaling the
result. Outside a `workflow/run` it is transparent — it just calls the thunk.

This is the journaling wrapper; the `agent` macro is the
ergonomic surface (it supplies the LLM-call thunk and handles `:schema`). Compose it
inside `pipeline` or
`parallel` to make a fanned-out set of leaves show up
as sibling agent rows.

```sema
;; usually written as the `agent` macro:
(agent (str "Explain " topic) {:name "writer" :schema article})

;; the macro expands to the wrapper around an LLM-call thunk:
(workflow/agent {:name "writer"}
  (fn () (llm/extract article (str "Explain " topic))))
```

See also: `agent`, `pipeline`, `workflow/run`, `checkpoint`.
