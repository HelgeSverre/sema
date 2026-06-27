# Deferred Workflow Work

This file tracks larger dynamic-workflow ideas that should not be folded into the current quick-fix pass. The source discussion is the GitHub issue comment on dynamic workflows: https://github.com/HelgeSverre/sema/issues/41#issuecomment-4815472955.

## Manager And Subprocess Agents

- Add a `sema-workflowd`-style manager that owns run lifecycle, scheduling, budgets, retries, cancellation, subprocess supervision, and dashboard serving.
- Keep the manager deterministic. It should supervise and journal work, not act as an LLM planning loop.
- Add subprocess agents with a JSONL protocol before sockets. JSONL is inspectable, replayable, and matches the journal-first model.
- Define `defsubagent` or equivalent metadata for command, protocol, timeout, sandbox, and compiled executable agents.

## Run Directory Format

- Snapshot the executed `workflow.sema` and `args.json` into each run directory.
- Add per-agent folders with `input.json`, `prompt.md`, `events.jsonl`, `stdout.log`, `stderr.log`, and `result.json`.
- Add first-class `artifacts/` output paths for reports, patches, and generated files.
- Treat the run directory as a stable public format that can be copied to another machine and replayed or inspected later.

## Resume And Cache Keys

- Extend agent cache keys beyond the current workflow source/version, args fingerprint, phase, name, prompt, and schema representation.
- Include model, system prompt, tool set/version, agent source, and relevant child sandbox in agent cache identity.
- Decide whether checkpoint keys should include an explicit caller-provided input hash for cases where a checkpoint value depends on external state.
- Preserve backward-compatible behavior or provide migration notes when content-key fields change.

## Permissions

- Keep `:permissions` as the canonical workflow metadata key; `:perms` is only a legacy alias.
- Move beyond CLI sandbox strings toward a structured permission schema, for example read-only, test-agent, patch-agent, and research-agent profiles.
- Map workflow/agent permissions to child process sandbox flags and `--allowed-paths`.
- Consider runtime-level enforcement for in-process workflow calls, not only CLI pre-run interpreter construction.

## Scheduler Semantics

- Make `parallel` a scheduler primitive with ordered results, independent completion order, bounded concurrency, and configurable fail-fast behavior.
- Add task or agent handles with `await`, `await-all`, `cancel`, and `status`.
- Make cancellation propagate downward to running child agents.
- Add `pipeline` as a streaming DAG/barrier-avoidance primitive once `parallel` semantics are settled.

## Dashboard Operations

- Project `events.jsonl` into the dashboard first; SQLite remains a secondary index.
- Add operator controls: pause run, resume run, cancel run, cancel agent, restart agent, inspect prompt, inspect result, inspect tool transcript, and export report.
- Prefer SSE over WebSockets for the first live local dashboard stream.
