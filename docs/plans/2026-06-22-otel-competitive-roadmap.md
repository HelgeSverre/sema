# OTel GenAI: Competitive Gap Analysis & Best-in-Class Roadmap

**Status:** Research synthesis + roadmap. Tier-1 items already shipped (2026-06-22);
the rest is a prioritized backlog, not yet implemented.
**Date:** 2026-06-22
**Sources:** two parallel research passes — (A) OpenInference (Arize/Phoenix) +
OpenLLMetry (Traceloop); (B) Langfuse, LangSmith, Braintrust, Pydantic Logfire — each
grounded against the live platform docs and Sema's emitter (`crates/sema-otel/src/imp.rs`).

---

## 0. Strategic position

Sema is the **only** of these emitters already on the *current* OTel GenAI semconv
(`gen_ai.*`, schema 1.37, structured-JSON messages, **in-SDK cost computation**, a real
`sema.gen_ai.cache.hit` boolean, deterministic `gen_ai.conversation.id`, VM `vm_span`
cells, retry sub-spans). That makes it **already best-in-class in any vanilla-OTel
backend and in Logfire/Braintrust today**.

The gap is **platform-native namespace keys**: Phoenix/Arize read `openinference.*`,
Traceloop reads `traceloop.*` + indexed `gen_ai.prompt.{i}.*`, Langfuse/LangSmith read
their own `langfuse.*`/`langsmith.*` keys. They largely **ignore** `gen_ai.operation.name`
and the structured `.messages` blobs, so a Sema trace currently renders as "unknown
span / blank I/O" in those UIs.

**The "exceed them" move:** keep `gen_ai.*` as the source of truth and add a thin
**compat layer** (behind `SEMA_OTEL_COMPAT=openinference,traceloop,langsmith`) that
also writes the alias keys — isolated like `provider_map.rs`. Then Sema is the only
emitter that renders natively in **vanilla OTel + Phoenix + Traceloop + Langfuse +
LangSmith + Braintrust + Logfire**, with zero manual instrumentation. All of this is
auto-derivable from data already in scope at the call sites.

---

## 1. Shipped 2026-06-22 (Tier-1, highest leverage)

- `gen_ai.usage.cost` (the key **Langfuse** maps — fixes the observed cost=0) +
  `gen_ai.usage.total_tokens`, alongside the existing `gen_ai.usage.cost_usd`.
- `langfuse.observation.input` / `output` (content-gated) so captured content renders
  on the Langfuse generation.
- `deployment.environment.name` from `SEMA_OTEL_ENVIRONMENT` / `DEPLOYMENT_ENVIRONMENT`
  (Langfuse + Logfire filter on it).
- Scope `schema_url` + descriptive attributes; Resource `service.version` + runtime.

Live-verified against self-hosted Langfuse: cost, total tokens, and input/output now
populate; sessions/users group; gRPC + HTTP both deliver.

---

## 2. Roadmap (prioritized, all auto-emit / no manual instrumentation)

### P0 — Cheap, high-signal, "push one more KeyValue from data in scope"

| Item | Keys | Where | Notes |
|---|---|---|---|
| **Compat span-kind tagging** | `openinference.span.kind` (`LLM`/`TOOL`/`AGENT`/`EMBEDDING`), `traceloop.span.kind` (`task`/`tool`/`agent`), `langsmith.span.kind=llm` | every span constructor | Single biggest interop win — makes Phoenix/Traceloop/LangSmith classify + cost Sema spans. Gate behind `SEMA_OTEL_COMPAT`. |
| **Generic span I/O** | `input.value`/`output.value` (+ `*.mime_type`), `traceloop.entity.input/output` | `set_messages` | Phoenix/Traceloop key their I/O panes off these; pure relabel of existing JSON, content-gated. |
| **Tool args + result** | `gen_ai.tool.call.arguments`, `gen_ai.tool.call.result` (+ OpenInference `tool_call.function.arguments`) | `run_tool_loop` tool span | Already truncated for the callback; the single most useful agent-debugging datum. Content-gated. |
| **Advertised tool schemas** | `llm.tools.{i}.tool.json_schema` (OpenInference) / `llm.request.functions.{i}.*` | `chat` span in `do_complete` | `tool_schemas` already serialized for the wire request. "Which tools were available this turn." |
| **Trace-level I/O rollup** | `langfuse.trace.input` / `langfuse.trace.output` on the run's root span | agent span / standalone chat | Fills the Langfuse **trace** panel (distinct from observation I/O). Needs first-input/final-output on the root. |

### P1 — Genuine differentiators, modest effort

| Item | Keys | Notes |
|---|---|---|
| **Time-to-first-token (streaming)** | `langfuse.observation.completion_start_time` (ISO8601), `gen_ai.is_streaming`, OpenLLMetry `llm.chat_completions.streaming_time_to_first_token` | `llm/stream` knows first-chunk wall time. **Almost no emitter does this** — clear best-in-class signal. |
| **Auto-tags** | `langfuse.trace.tags`, `langsmith.span.tags` (CSV), `braintrust.tags` | Auto-tag with provider + model + operation + cache-hit; pass through a user `:tags`. |
| **Metadata passthrough** | `langfuse.trace.metadata.*`, `langsmith.metadata.*`, `braintrust.metadata` | Add a `:metadata` map option on `agent/run`/`llm/chat` → fan out per platform prefix. |
| **`llm.invocation_parameters`** | OpenInference consolidated JSON blob | Phoenix reads the blob; keep discrete `gen_ai.request.*` as source of truth. |
| **Cost split / aliases** | `llm.cost.total`/`prompt`/`completion` (OpenInference), cache-token aliases (`llm.token_count.prompt_details.cache_read`) | Sema self-computes cost — an advantage over OpenLLMetry (server-side) — surface it in their namespace too. |
| **LangSmith session/version** | `langsmith.trace.session_id`, `langfuse.release`/`langfuse.version` | LangSmith ignores `session.id`/`gen_ai.conversation.id`; needs its own key. |
| **Embedding detail** | `embedding.model_name`, texts (gated), `openinference.span.kind=EMBEDDING` | Skip raw vectors by default (both competitors gate them). |

### P2 — Needs a new Sema concept; roadmap notes only

- **Prompt templates / registry** (`llm.prompt_template.*`, `traceloop.prompt.*`): only
  feasible if Sema adds first-class prompt templating. Sema's f-strings (`f"...${x}..."`)
  are *exactly* a template + bound vars — a natural future fit. Track with the
  `defprompt`/structured-output idea in IDEAS.md.
- **Retriever / reranker / vector-DB spans** (`retrieval.documents.*`): no Sema RAG
  primitive yet. Revisit if vector-store builtins land.
- **Scores / evaluations** (`braintrust.scores`, Langfuse/LangSmith score APIs): not
  auto-derivable; a manual `(otel/score ...)` surface — see the Sema-native tracing
  scoping doc + the deferred evals item.
- **Content-capture granularity**: OpenInference has ~14 `HIDE_*` flags; Sema's single
  toggle matches OpenLLMetry and is fine. Add one embedding-vector suppression flag only
  if embedding-vector capture lands.

---

## 3. Trace structure: flat siblings is CORRECT

Earlier concern (tool spans flat under `invoke_agent`, siblings of `chat`, vs nested
under the requesting `chat`): the research confirms **both OpenInference and OpenLLMetry
also make the tool a child of the agent span, sibling to the LLM span** — the model's
tool-call *request* is not its own span. So Sema's flat structure **matches the
convention**; no change needed. (Langfuse rendered our long multi-turn run correctly as
AGENT → GENERATION×N + TOOL×M in temporal order.)

---

## 4. Recommended next slice

If we pursue this: ship the **P0 compat layer** behind `SEMA_OTEL_COMPAT` (span-kind
tags + `input/output.value` + tool args/result + advertised tool schemas) as one
focused milestone — it's all relabeling/forwarding of in-scope data and turns
currently-blank Sema traces into fully-populated ones across Phoenix, Traceloop, and
LangSmith simultaneously. Then P1 TTFT + auto-tags as the differentiators. Decide
whether the compat layer is opt-in (flag) or always-on (attribute bloat vs zero-config).
