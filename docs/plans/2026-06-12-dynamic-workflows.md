# Dynamic Workflows in Sema — Implementation Plan

**Status:** Planned
**Date:** 2026-06-12
**Priority:** P1
**Goal:** Add a deterministic workflow runtime for Sema where `workflow.sema` orchestrates runs, `agent.sema` files act as callable workers, and a manager process owns journaling, scheduling, budgets, and supervision.

---

## Summary

The core design constraint is: **the workflow manager must not be an LLM chat loop**.

Instead:

- `workflow.sema` is the orchestration program
- `agent.sema` files are callable workers / subagents
- `sema-workflowd` is the deterministic manager / supervisor
- the dashboard is a projection of the run journal

This keeps planning in code, intermediate values in normal Sema variables, and side effects inside bounded worker agents rather than in the orchestrator itself.

---

## Product Shape

The feature is four layers:

1. **Dashboard / TUI / Web UI**
   - shows runs, phases, agent activity, logs, prompts, artifacts, costs
   - reads from the event journal / manager API only
2. **`sema-workflowd` manager**
   - owns runs, scheduling, budgets, concurrency, retries, cancellation, journaling
   - intentionally contains no LLM planning logic
3. **`workflow.sema` runtime**
   - evaluates orchestration forms such as `phase`, `agent/run`, `parallel`, `checkpoint`
4. **Subagents**
   - Sema agent values, Sema subprocesses, or compiled Sema executables

---

## Design Principles

1. **Workflow logic lives in Sema code.**
   - No external JSON DSL.
   - Workflows should be inspectable, testable, and eventually evolvable.
2. **Manager is deterministic and dumb.**
   - It supervises execution; it does not invent plans.
3. **JSONL first, SQLite second.**
   - Append-only event logs are the source of truth.
   - SQLite is a projection for querying and dashboards.
4. **Workers do effects.**
   - Filesystem, shell, browser, and LLM/tool loops belong in subagents.
5. **Remote execution should build on existing `sema build`.**
   - Workflow runners and agents should be deployable as standalone binaries.

---

## Existing Foundation in the Repo

This design fits the current codebase well:

- `sema eval --json` already provides machine-oriented structured success/error envelopes.
- sandbox flags already exist (`strict`, `no-shell`, `no-network`, `no-fs-write`, `--allowed-paths`).
- `sema build` already packages bytecode, imports, and included assets for deployment.
- `defagent` and `agent/run` already provide native LLM-backed workers.
- Living Code work (`become!`, `rollback!`, doctests, introspection, evolve) is a natural later extension for generated workflows.

---

## User-Facing Shape

### Workflow source

Workflows should be normal Sema code with a top-level `defworkflow`:

```scheme
(import workflow)

(defworkflow audit-auth
  "Audit a codebase for missing authorization checks."
  {:args {:paths [:list :string]
          :framework :string}
   :budget {:max-agents 64
            :max-concurrent 8
            :max-tokens 250000}
   :permissions [:read-repo :run-tests]
   :phases ["Inventory" "Audit" "Verify" "Report"]}

  (phase "Inventory")
  ...)
```

### Core workflow forms

MVP forms:

- `(defworkflow name doc? meta body...)`
- `(phase name)`
- `(agent/run worker input)`
- `(parallel opts? thunks)`
- `(log event)`
- `(emit event)`
- `(checkpoint label value)`

### CLI shape

Primary command:

```bash
sema workflow run workflows/audit-auth.sema \
  --args '{"paths":["app/Http","routes"],"framework":"Laravel"}' \
  --dashboard
```

Machine mode:

```bash
sema workflow run workflows/audit-auth.sema --args args.json --json
```

Expected result envelope:

```json
{
  "ok": true,
  "run_id": "run_20260612_abc123",
  "result_path": ".sema/runs/run_20260612_abc123/result.json",
  "dashboard_url": "http://127.0.0.1:7357/runs/run_20260612_abc123"
}
```

---

## Subagent Model

Support three worker shapes:

1. **In-process agent value**
   - `defagent` + `agent/run`
   - lowest overhead
2. **Subprocess Sema script**
   - `defsubagent` with `["sema" "agents/auth-audit.sema"]`
   - strongest isolation for local development
3. **Compiled executable**
   - `defsubagent` with `["./dist/auth-audit-agent"]`
   - best for CI, remote hosts, and reproducible deployment

All three should converge on the same logical interface from workflow code.

---

## Event Journal

Start with **JSONL as the system of record**:

```text
.sema/runs/<run-id>/
  events.jsonl
  result.json
  args.json
  metadata.json
  artifacts/
```

Representative child events:

```json
{"type":"agent.started","agent_id":"agent_42","ts":"2026-06-12T12:00:00Z"}
{"type":"agent.log","agent_id":"agent_42","message":"Reading controller"}
{"type":"agent.tool_call","agent_id":"agent_42","tool":"read-file","input":{"path":"app/Http/Controllers/UserController.php"}}
{"type":"agent.result","agent_id":"agent_42","ok":true,"value":{"findings":[]}}
```

SQLite should be a later projection over the same stream:

- `workflow_runs`
- `workflow_phases`
- `agent_runs`
- `events`
- `artifacts`
- `costs`
- `checkpoints`

---

## Manager Responsibilities

`sema-workflowd` owns:

- run lifecycle
- phase lifecycle
- agent scheduling
- concurrency limits
- budgets and token accounting
- retries and timeouts
- cancellation / pause / resume
- subprocess supervision
- event persistence
- dashboard serving
- remote worker routing later

It does **not** decide what the workflow means.

---

## Recommended MVP Scope

### Phase 1 — Event journal and run directory

Ship a minimal:

```bash
sema workflow run <workflow.sema> --args <json-or-file>
```

with:

- run directory creation under `.sema/runs/<run-id>/`
- append-only `events.jsonl`
- final `result.json`
- structured JSON output from the CLI

No dashboard required yet.

### Phase 2 — Deterministic workflow runtime

Add:

- `defworkflow`
- `phase`
- `log` / `emit` / `checkpoint`
- `run-workflow`

The workflow body executes in a workflow-aware evaluation context but remains normal Sema code.

### Phase 3 — Local subagent execution

Support:

- in-process `defagent` workers
- subprocess workers over stdin/stdout JSONL
- concurrency caps and per-agent timeouts

### Phase 4 — Dashboard / SQLite projection

Add:

- local dashboard server backed by the journal
- SQLite projection for querying runs and agent activity
- status pages for runs, phases, logs, prompts, results, and costs

### Phase 5 — Remote workers

Start with SSH-oriented deployment:

- upload workflow bundle / executable
- start remotely
- stream JSONL events back over stdout

HTTP worker daemons can come later.

### Phase 6 — Living Code integration

Once the workflow runtime is stable, integrate:

- `become!`
- `rollback!`
- doctests for workflows
- future `evolve-workflow`

This should be explicitly deferred until the runtime and journal format settle.

---

## Proposed Repository Layout

Exact filenames can change, but the implementation should stay roughly split this way:

```text
crates/sema/src/
  workflow.rs              # CLI entrypoints: run/list/status/build
  workflow_run.rs          # run setup and output envelopes

crates/sema-eval/src/
  workflow.rs              # workflow-aware runtime hooks / forms

crates/sema-stdlib/src/
  workflow.rs              # workflow helpers exposed to Sema
  workflow_agent.rs        # subprocess protocol helpers if needed

docs/
  plans/
  internals/               # later runtime / journal docs
```

If the manager grows large, split it into a dedicated crate rather than bloating the CLI crate.

---

## Security and Sandboxing

This feature increases execution power, so MVP must preserve current safety boundaries:

- workflow orchestration code should not gain unrestricted shell / filesystem powers by default
- worker permissions should map onto the existing sandbox flags and path restrictions
- budgets must include concurrency caps, total-agent caps, and token caps
- the journal must record enough metadata to audit who did what

The initial implementation should prefer explicit permission declarations in workflow metadata over implicit ambient capability.

---

## Testing Plan

Add tests in layers:

1. **Unit tests**
   - run-id generation
   - event serialization
   - JSONL protocol decoding
   - budget / concurrency accounting
2. **Integration tests**
   - `sema workflow run` creates the expected run directory
   - workflow phase transitions are journaled correctly
   - subprocess agent success / timeout / failure cases
3. **Golden tests**
   - stable `events.jsonl` snapshots for small fixture workflows
4. **Later end-to-end tests**
   - dashboard reads the same run journal without mutating it

---

## Non-Goals for MVP

Do **not** attempt all of this in the first implementation:

- autonomous LLM planning inside the manager
- sockets as the primary worker protocol
- remote HTTP worker daemons
- workflow evolution / self-modification
- a full web product before the journal is stable

---

## Open Questions

1. Should `defworkflow` live in the evaluator as a special form, or as a macro over an ordinary value shape plus `run-workflow`?
2. Should subprocess workers use a dedicated `defsubagent`, or should worker metadata be ordinary maps passed to `agent/run`?
3. Where should workflow journal schemas live so they can be reused by CLI, dashboard, and future remote workers?
4. Is the manager best shipped first inside the `sema` binary, or as a separate `sema-workflowd` binary from day one?
5. What is the smallest stable event vocabulary that still supports replay, dashboards, and future SQLite projection?

---

## Recommended First Milestone

The first milestone should be intentionally narrow:

> **Run a local `workflow.sema` file, produce a run directory with `events.jsonl` and `result.json`, and allow that workflow to call bounded local subagents.**

That milestone validates the architecture without prematurely committing to dashboard, remote execution, or Living Code evolution semantics.
