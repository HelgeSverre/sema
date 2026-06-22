---
outline: [2, 3]
---

# Backend Compatibility

By default Sema labels its telemetry with the
[OpenTelemetry GenAI semantic conventions](https://github.com/open-telemetry/semantic-conventions/tree/main/docs/gen-ai)
— the standard `gen_ai.*` attribute names. Tools that follow that standard understand
Sema's traces with no extra configuration.

A handful of popular LLM-observability tools don't read `gen_ai.*` — they look for their
own attribute names instead, so a Sema span can show up in them as "unknown" or with
blank fields. For those tools, set the `SEMA_OTEL_COMPAT` environment variable and Sema
*also* writes their attribute names (in addition to the standard ones). Nothing about your
program changes — it's still the same automatic tracing, just labelled so more tools can
read it.

This is purely additive: the standard `gen_ai.*` attributes are always present;
`SEMA_OTEL_COMPAT` only adds extra copies under other names. Read the
[Tracing & Metrics](./observability) page first for how tracing works and how to point
Sema at a backend — this page only covers the per-tool labelling.

## Which tools need it

| Tool | Reads the standard `gen_ai.*`? | `SEMA_OTEL_COMPAT` value | What turning it on adds |
| --- | --- | --- | --- |
| Grafana / Tempo, Jaeger (plain OTel) | yes | — | nothing needed |
| [Logfire](https://pydantic.dev/logfire) | yes | — | nothing needed |
| Datadog LLM Observability | yes | — | nothing needed |
| Honeycomb | yes | — | nothing needed |
| [SigNoz](https://signoz.io/) | yes | — | nothing needed |
| Elastic / New Relic | yes | — | nothing needed |
| OpenLIT | yes | — | nothing needed |
| [Braintrust](https://www.braintrust.dev/) | yes | `braintrust` *(optional)* | native tags + metadata + cost metric |
| [Langfuse](https://langfuse.com/) | partly | `langfuse` | observation type/model, usage + cost detail, trace-level input/output, tags/metadata |
| [Arize Phoenix](https://phoenix.arize.com/) (OpenInference) | no | `openinference` | span types, model/provider, token counts, cost, message I/O, tool args + schemas |
| [Traceloop](https://www.traceloop.com/) (OpenLLMetry) | partly | `traceloop` | span types, entity I/O, indexed token keys, tool functions |
| [LangSmith](https://www.langchain.com/langsmith) | partly | `langsmith` | run types, session threading, tags/metadata |
| [Helicone](https://www.helicone.ai/) | n/a | — | not reachable via OTLP — see [Limitations](#limitations) |

Most tools work without any value — they already read the standard attributes. You only
need `SEMA_OTEL_COMPAT` for the four at the bottom of the table.

## Setting `SEMA_OTEL_COMPAT`

It's an environment variable like the others (see
[How to turn it on](./observability#how-to-turn-it-on)). Its value is a comma-separated
list of the tool names from the table above, lower-case:

```bash
# Just Phoenix:
SEMA_OTEL_COMPAT=openinference sema myagent.sema

# Phoenix and Langfuse at once:
SEMA_OTEL_COMPAT=openinference,langfuse sema myagent.sema

# Every supported tool's names — useful if you're not sure which backend you'll use:
SEMA_OTEL_COMPAT=all sema myagent.sema
```

Accepted values: `openinference` (also `phoenix`, `arize`), `traceloop` (also
`openllmetry`), `langsmith`, `langfuse`, `braintrust`, and `all`. Names you don't
recognise are ignored, so a typo won't break anything.

Some of the added detail — message text, tool arguments and results, and the trace-level
input/output summary — is **content**, so it only appears when you also turn on content
capture with `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true` (see
[Privacy](./observability#privacy)). Token counts, models, cost, and span types are
always added.

When `SEMA_OTEL_COMPAT` is unset, no extra attributes are written — the spans are exactly
what you get on the [Tracing & Metrics](./observability) page, at no added cost.

## Per-tool setup

### Arize Phoenix (OpenInference)

Phoenix is an open-source LLM trace viewer that runs in one container:

```bash
# Start Phoenix. UI on 6006; it accepts traces on 6006 (HTTP) and 4317 (gRPC).
docker run -d --name phoenix -p 6006:6006 -p 4317:4317 arizephoenix/phoenix:latest

SEMA_OTEL_COMPAT=openinference \
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:6006 \
OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true \
  sema -e '(llm/complete "say hi" {:max-tokens 16})'
```

Open `http://localhost:6006`. Each Sema span is typed (`LLM` / `TOOL` / `AGENT` /
`EMBEDDING`) and shows the model, provider, token counts, cost, the message I/O, and —
for agent runs — tool arguments, results, and the tool schemas offered to the model.

### Langfuse

Langfuse already reads several of Sema's standard attributes (cost and message I/O). The
`langfuse` value fills in the rest — the observation type and model, the usage/cost detail
objects, and the trace-level input/output summary:

```bash
SEMA_OTEL_COMPAT=langfuse \
OTEL_EXPORTER_OTLP_ENDPOINT="http://localhost:3000/api/public/otel" \
OTEL_EXPORTER_OTLP_HEADERS="Authorization=Basic <base64 of publickey:secretkey>" \
OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true \
  sema myagent.sema
```

(See the [Langfuse example](./observability#sending-to-hosted-langfuse) for how to build
the auth header.) Multi-turn runs group into
[Sessions](./observability#sessions-and-users-grouping-multi-turn-runs) via the
`:session-id` and `:user-id` options.

### Traceloop (OpenLLMetry)

Traceloop is mainly a hosted product, but it reads plain OTLP, so you can also view the
output in any OTLP backend (such as SigNoz). `SEMA_OTEL_COMPAT=traceloop` adds the
`traceloop.span.kind` and `traceloop.entity.*` attributes, the indexed token keys, and the
advertised tool functions.

### LangSmith

LangSmith ingests over OTLP but has no local/self-hosted option, so point Sema at its
hosted endpoint with your API key and `SEMA_OTEL_COMPAT=langsmith`. This adds LangSmith's
run types, session threading, and tags/metadata.

### Braintrust

Braintrust reads the standard attributes, so it works with no value set. Add `braintrust`
only if you want its native `braintrust.tags` and `braintrust.metadata` fields.

## Span-type mapping

How each Sema span is labelled for each tool when its compat value is on:

| Sema span | OpenInference | Traceloop | LangSmith | Langfuse |
| --- | --- | --- | --- | --- |
| `chat` | `LLM` | `task` | `llm` | `generation` |
| `embeddings` | `EMBEDDING` | `task` | `embedding` | `generation` |
| `execute_tool` | `TOOL` | `tool` | `tool` | `span` |
| `invoke_agent` | `AGENT` | `agent` | `chain` | `span` |
| notebook cell / retry | `CHAIN` | `workflow` | `chain` | `span` |

## Limitations

- **Message content requires the opt-in flag.** The message I/O, tool arguments and
  results, and the trace-level input/output only appear when
  `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true`. Token counts, models, cost,
  and span types are always added.
- **OpenInference has no separate tool-result field** — the result appears in the tool
  span's `output.value` rather than a dedicated attribute.
- **LangSmith recomputes cost** on its side from the token counts, so Sema's exact
  per-call cost (including cache pricing) may differ from the number LangSmith shows.
- **Helicone** is a proxy/gateway, not an OTLP receiver, so this setting can't send traces
  to it. Use Helicone's own gateway integration instead.
- **Not yet implemented:** streaming time-to-first-token, and the per-message *indexed*
  attribute form some older Traceloop/LangSmith parsers expect (Sema emits the structured
  and entity forms today). An auto-tagging option is also planned.
- **More attributes per span.** Compat adds extra copies of each value. If you only use a
  plain OTel backend, leave `SEMA_OTEL_COMPAT` unset to keep spans lean.
