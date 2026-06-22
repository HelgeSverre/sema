---
outline: [2, 3]
---

# Tracing & Metrics

Sema can record what happens inside every LLM and agent run — each model call, tool
execution, retry, and notebook cell — as [OpenTelemetry](https://opentelemetry.io/)
traces and metrics, and send them to a tool where you can browse them. You don't write
any instrumentation: switch it on with one environment variable and `llm/complete`,
`agent/run`, `llm/embed`, and the rest are recorded automatically.

If OpenTelemetry is new to you, the terms used below:

- **OpenTelemetry (OTel)** is an open, vendor-neutral standard for traces and metrics.
- A **trace** is one run. It is made of **spans** — individual timed operations such as a
  single LLM call or a tool execution. Spans nest, so an agent run appears as a tree.
- **OTLP** is the network protocol OTel uses. Sema speaks OTLP, so it works with any tool
  that accepts it — a free local viewer like [Jaeger](https://www.jaegertracing.io/), or
  a hosted service like [Langfuse](https://langfuse.com/), Grafana, or Datadog.
- Sema follows the OTel
  [GenAI semantic conventions](https://github.com/open-telemetry/semantic-conventions/tree/main/docs/gen-ai)
  — the agreed attribute names for LLM telemetry (token counts, model, cost, …) — so
  these tools understand the data with no per-tool glue.

Tracing is **off by default**. If you set no endpoint and no file, Sema installs nothing
and the tracing code does nothing. A slow or unreachable backend can never block, delay,
or crash your script — exports happen on a background thread with a bounded queue.

## How to turn it on

Sema reads its tracing settings from **environment variables** — values you set in your
shell. You can set them inline for a single command, or `export` them for the whole
session:

```bash
# Inline — applies to this one run only:
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 sema myscript.sema

# Or exported — applies to every command in this shell session:
export OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
sema myscript.sema
```

Two variables decide *where* the data goes, and **setting either one turns tracing on**:

- `OTEL_EXPORTER_OTLP_ENDPOINT` — send to a backend over the network (Jaeger, Langfuse, …).
- `SEMA_OTEL_FILE` — write to a local file instead (handy with no backend at all).

Set neither and tracing stays off. The full list of variables is in
[Configuration reference](#configuration-reference) below.

## Quick start: see a trace in one minute

[Jaeger](https://www.jaegertracing.io/) is a free trace viewer that runs in a single
container — a good way to see your first trace.

```bash
# 1. Start Jaeger. The UI is on port 16686; it accepts traces on 4318.
docker run --rm -d --name jaeger -p 4318:4318 -p 16686:16686 \
  -e COLLECTOR_OTLP_ENABLED=true jaegertracing/all-in-one

# 2. Point Sema at it and run something. No model is pinned here, so this uses
#    your default provider and its default model — just make sure an API key is
#    set (ANTHROPIC_API_KEY / OPENAI_API_KEY / GEMINI_API_KEY / …).
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
  sema -e '(llm/complete "say hi" {:max-tokens 16})'
```

Open `http://localhost:16686` in your browser, pick the **sema** service, and you'll see
one trace whose `chat` span carries the provider, model, input/output token counts,
cost, and finish reason.

> **Choosing a specific model.** The example uses whichever provider is active. To pick
> one, select it first: `(llm/set-default :openai)` then `{:model "gpt-5-mini"}`, or
> `(llm/set-default :anthropic)` then `{:model "claude-haiku-4-5-20251001"}`. A model id
> only works with the provider that offers it — sending an OpenAI model id to Anthropic
> returns a 404.

## Configuration reference

Every setting is an environment variable (see [How to turn it on](#how-to-turn-it-on)
for how to set them). The `OTEL_*` names come from OpenTelemetry itself; the
`SEMA_OTEL_*` names are Sema conveniences.

| Variable | What it does |
| --- | --- |
| `OTEL_EXPORTER_OTLP_ENDPOINT` | The address of your tracing backend, e.g. `http://localhost:4318`. **Setting this turns tracing on.** |
| `OTEL_EXPORTER_OTLP_PROTOCOL` | How to talk to it: `http/protobuf` (default) · `http/json` · `grpc`. Keep the default unless your backend only accepts gRPC. |
| `OTEL_EXPORTER_OTLP_HEADERS` | Extra HTTP headers, usually authentication — e.g. `Authorization=Bearer <token>`. Comma-separated `key=value` pairs. |
| `OTEL_EXPORTER_OTLP_TIMEOUT` | Per-export timeout in milliseconds. Keep it short (e.g. `3000`) so a dead backend never holds things up. |
| `OTEL_SERVICE_NAME` | The name your runs appear under in the backend (default `sema`). |
| `SEMA_OTEL_FILE` | Write traces to this file path, one JSON object per line, instead of sending them over the network. Also turns tracing on. |
| `SEMA_OTEL_ENVIRONMENT` | A label such as `prod` or `staging` for filtering (recorded as `deployment.environment.name`). |
| `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT` | Set to `true` to also record the **prompt and response text** (off by default — see [Privacy](#privacy)). Sema also accepts the shorter alias `SEMA_OTEL_CAPTURE_CONTENT`. |
| `OTEL_BSP_MAX_QUEUE_SIZE`, `OTEL_BSP_MAX_EXPORT_BATCH_SIZE`, `OTEL_BSP_SCHEDULE_DELAY` | Advanced: tune the background export batching. The defaults are fine for most uses. |

Both network transports (HTTP and gRPC) are built in; switch with
`OTEL_EXPORTER_OTLP_PROTOCOL` — no rebuild needed.

### Writing to a file instead of a backend

No backend running? Set `SEMA_OTEL_FILE` and Sema writes each finished span to a file as
one JSON object per line:

```bash
SEMA_OTEL_FILE=/tmp/sema-trace.jsonl \
  sema -e '(llm/complete "ping" {:max-tokens 16})'

cat /tmp/sema-trace.jsonl | jq .
```

The file is written synchronously, so even a one-line script captures its spans.

### Sending to hosted Langfuse

A hosted backend usually needs an endpoint plus an auth header. For
[Langfuse](https://langfuse.com/), the header is HTTP Basic auth built from your project
keys:

```bash
export OTEL_EXPORTER_OTLP_ENDPOINT="https://cloud.langfuse.com/api/public/otel"
export OTEL_EXPORTER_OTLP_HEADERS="Authorization=Basic $(echo -n "$LANGFUSE_PUBLIC_KEY:$LANGFUSE_SECRET_KEY" | base64)"
sema myagent.sema
```

Langfuse (and several other LLM-focused tools) can show even richer detail with one more
setting — see [Backend Compatibility](./otel-compat).

## What gets traced

| Span | Kind | Name | When |
| --- | --- | --- | --- |
| LLM call | `CLIENT` | `chat {model}` | every non-streaming completion (including cache hits) |
| Embeddings | `CLIENT` | `embeddings {model}` | every `llm/embed` |
| Tool call | `INTERNAL` | `execute_tool {name}` | every tool dispatch in an agent loop |
| Agent run | `INTERNAL` | `invoke_agent {name}` | every `agent/run` / tools-enabled completion |
| Notebook run | `INTERNAL` | `notebook.run_all` → `notebook.cell {id}` | a notebook "Run All" (one trace, one child span per cell) |
| Retry | `INTERNAL` | `llm.retry_attempt` | each HTTP retry (429 / 5xx / network), nested under the LLM span |

Each LLM span carries the standard GenAI attributes: `gen_ai.operation.name`,
`gen_ai.provider.name`, `gen_ai.request.model` / `gen_ai.response.model`,
`gen_ai.usage.input_tokens` / `output_tokens`, prompt-cache token counts,
`gen_ai.response.finish_reasons`, and the computed cost (`gen_ai.usage.cost`, plus
`gen_ai.usage.cost_usd`). Cache hits are flagged with `sema.gen_ai.cache.hit`. Tool spans
carry `gen_ai.tool.name` / `gen_ai.tool.call.id` / `gen_ai.tool.type`.

### Sessions and users (grouping multi-turn runs)

Every span carries a `gen_ai.conversation.id`, generated per run or supplied by you. For
tools that group by session (such as Langfuse), Sema also emits `session.id` and
`user.id`, so the turns of one conversation appear together:

```sema
(agent/run bot "what is 2 + 3?"  {:session-id "chat-42" :user-id "alice"})
(agent/run bot "now add 10"      {:session-id "chat-42" :user-id "alice"})
;; both runs appear under one session "chat-42", attributed to alice
```

`agent/run`, `llm/chat`, and `llm/complete` accept `:conversation-id`, `:session-id`, and
`:user-id`. If you omit `:session-id` it defaults to the conversation id; a standalone
completion gets a fresh conversation id automatically.

### Metrics

When you export over a network endpoint, Sema also records two standard GenAI metric
histograms:

- `gen_ai.client.token.usage` — token counts (dimension `gen_ai.token.type` = `input` or
  `output`).
- `gen_ai.client.operation.duration` — call latency in seconds.

> Cache hits report zero usage by design (no provider call was made), so token metrics
> undercount real spend when caching is in play.

## Adding your own spans

Two builtins let your Sema code record its own spans and events. Both do nothing when
tracing is off, so they are safe to leave in.

```sema
;; Wrap any work in a named span; returns the thunk's value.
(otel/span "ingest-batch"
  (fn ()
    (otel/event "started" {:batch-size 100})
    (process-batch)))
```

- `(otel/span name thunk)` — runs `thunk` inside a span called `name` and returns its
  value; the span records how long the thunk took. Any LLM or tool spans created inside
  nest beneath it.
- `(otel/event name attrs-map)` — attaches a point-in-time event to the current span.

## Privacy

Prompt and response **text** is never recorded unless you explicitly set
`OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true`. Token counts, model names,
cost, and timing carry no message text and are always exported. When content capture is
on, long messages are truncated to keep span sizes reasonable.

## Embedding Sema in a Rust application

When Sema runs as a library inside your own program, it **never installs a global tracer
provider on its own** — that is the host application's job. You choose how it connects to
telemetry with `InterpreterBuilder::with_telemetry(mode)`:

```rust
use sema::InterpreterBuilder;
use sema_otel::TelemetryMode;

// Emit against the provider your application already installed in `opentelemetry::global`.
let interp = InterpreterBuilder::new()
    .with_telemetry(TelemetryMode::UseHostGlobal)
    .build();
```

| `TelemetryMode` | Behavior |
| --- | --- |
| `Off` (default) | No telemetry; never touches any global state. |
| `UseHostGlobal` | Emit against the global provider your app already installed (silent no-op if there is none). |
| `OwnProvider(p)` | Emit against a provider you hand to Sema; installs **no** global provider. |
| `FromEnv` | Self-install from the `OTEL_*` / `SEMA_OTEL_FILE` variables. The provider is owned by the built `Interpreter` and flushes when it is dropped. If your app already runs OpenTelemetry, prefer `UseHostGlobal` or `OwnProvider`. |

Sema's spans automatically nest under whatever span is current
(`opentelemetry::Context::current()`), so a host request span becomes the parent of
Sema's `invoke_agent → chat / execute_tool` tree. `Interpreter::new()` and `build()` with
the default `Off` never touch global OpenTelemetry state.
