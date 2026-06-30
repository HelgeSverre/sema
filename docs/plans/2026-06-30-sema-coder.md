# Sema Coder — triage of issue #53 (host/app primitives)

Issue #53 brainstorms the primitives that would make a "coding agent written
almost entirely in Sema" feasible. It is a long wishlist (13 categories). This
note records the triage and what shipped on
`claude/sema-coder-agent-primitives-6gxxo4`.

## Decision

Scope to **the TUI + app shell**, per the issue's own "uncomfortable truth":
the stdlib is already strong for scripting; what's missing is the handful of
*host* primitives that make Sema feel like an application runtime — and a
canonical app ("Sema Coder") that proves the loop end-to-end and is easy to
extend with new slash commands.

We deliberately did **not** clone every wishlist item. MCP-related work waits on
a separate branch (per the issue and the owner's direction).

## Shipped — Rust primitives

Small, safe, high-leverage. All on stable APIs, no new dependencies.

**Terminal screen control** (`sema-stdlib/src/terminal.rs`) — emit ANSI/VT
sequences so TUIs stop hand-writing escape codes:

```
term/enter-alt-screen  term/leave-alt-screen
term/clear  term/clear-line  term/clear-below
term/move-to  term/write-at  term/cursor-home
term/hide-cursor  term/show-cursor  term/save-cursor  term/restore-cursor
term/enable-mouse  term/disable-mouse
term/set-title  term/bell  term/flush
```

**Path safety** (`sema-stdlib/src/io.rs`):

```
path/canonicalize   ;; resolve symlinks + `.`/`..` (errors if missing)
path/relative-to    ;; pure path math: express PATH relative to BASE
path/within?        ;; containment check — the cornerstone of agent sandboxing
```

**Config location** (`sema-stdlib/src/system.rs`):

```
sys/config-dir      ;; platform config base (XDG / Application Support / APPDATA)
```

Tests: `crates/sema/tests/eval_test.rs` (path math, containment, config-dir,
terminal no-ops).

## Shipped — the app

`examples/sema-coder/` — a coding agent in Sema, reusing `defagent`/`deftool`/
`agent/run`, `file/*`, `shell`, `json/*`, and the new `term/*` + `path/within?`.

- Single JSON config at `<config-dir>/sema/sema-code/config.json`.
- **Extensible slash commands** two ways: a Sema registry (`register-command!`,
  one call) and declarative config entries (`commands` map → shell templates with
  `$ARGS`).
- On-brand styling (sema gold `#c8a855`) and a compact wordmark banner.

## Deferred (tracked, not built)

These remain valuable but are out of scope for the shell, and several can be
written in Sema on top of `shell` today:

- **Streaming process / PTY** (`proc/spawn`, `proc/read-stdout`, `pty/*`) — the
  biggest remaining unlock; needed to stream test output live. Today the `bash`
  tool returns output on completion.
- **Event model** (`event/select`, `time/tick`) — pairs with streaming procs.
- **Diff/patch** (`diff/unified`, `diff/apply`, …) — `edit-file` reports a plain
  replacement; structured diffs are a follow-up (can start in Sema).
- **Git primitives** (`git/status`, `git/diff`, …) — agent shells out for now.
- **fs watching**, **buffer/editor layer**, **AST/`read-string` reflection**,
  **structured diagnostics** (`sema/check-file`), **secret/PII redaction**,
  **archive/markdown helpers**, **test-harness DSL**.

Top three to do next, per the issue: structured process/event system, then diff/
patch + git, then Sema AST + structured diagnostics.
