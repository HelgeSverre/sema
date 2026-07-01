# Sema Language Friction Log (micro/confidence)

While dogfooding the Sema language to implement the `micro/confidence` package (#52), I encountered a few areas where the primitives felt awkward or missing:

1. **`step` drops `:model` overrides when validating schemas:**
   The `step` macro defined in `prelude.rs` passes `st-schema#` and `st-prompt#` to `llm/extract`, but it drops the `st-opts#` map. This means that if we specify `(step prompt {:model "foo" :schema bar})`, the `llm/extract` call uses the globally configured default model instead of "foo". This breaks the core feature of `micro/confidence` which aims to escalate across different models! To fix this natively, `step` would need to pass its `opts` map down to `llm/extract` and `llm/complete`.

2. **No ergonomic way to emit typed journal events:**
   There is no generic `(workflow/journal event-name payload)` API available in Sema source. To emit the `micro.recipe.started` and `micro.candidate.completed` events so they land in the JSONL journal as requested, I had to abuse `(workflow/tool-call tool-name payload)`. This effectively forces the event `type` to be `agent.tool_call` with a stringy `tool_name` payload.

3. **Schema-invalid error handling is untyped:**
   When a step's schema validation fails (or runs out of re-asks), `llm/extract` throws a raw exception. Because we don't have typed errors (e.g. `{:error-type :schema-invalid}`), `try/catch` catches *any* error (including network failures or authentication errors) and we are forced to treat it generically as `schema_invalid` to trigger escalation.

4. **Budget exhaustion surfaces as `nil`:**
   When a workflow budget is exhausted, `step` silently short-circuits and returns `nil`. Detecting budget limits requires an explicit `(nil? candidate#)` check. If a step's schema *could* validly be `nil`, we'd have no reliable way to distinguish "budget cap reached" from "valid nil output".
