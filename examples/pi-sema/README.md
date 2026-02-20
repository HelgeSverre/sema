# pi-sema

A terminal coding agent written in [Sema Lisp](https://sema-lang.com), inspired by [pi.dev](https://pi.dev). Demonstrates Sema's LLM primitives (`deftool`, `defagent`, `agent/run`) as a real-world agentic application.

## Requirements

- The `sema` binary
- An API key: set `ANTHROPIC_API_KEY` or `OPENAI_API_KEY`

## Usage

```bash
# Interactive REPL
sema examples/pi-sema/main.sema

# One-shot mode
sema examples/pi-sema/main.sema -- -p "explain this codebase"

# Override model
sema examples/pi-sema/main.sema -- -m claude-sonnet-4-20250514
```

## Architecture

```
examples/pi-sema/
├── main.sema       Entry point — CLI parsing, REPL loop
├── agent.sema      System prompt builder, defagent definition
├── tools.sema      7 LLM-callable tools
├── commands.sema   Slash command dispatch
├── session.sema    NDJSON session persistence
├── compact.sema    Conversation compaction
├── context.sema    AGENTS.md and skill discovery
├── display.sema    Terminal UX (colors, spinners)
└── util.sema       Path resolution, safety, string helpers
```

### Module dependency flow

```
util ← display ← tools
                ← context ← agent ← main
                ← session ← compact ← commands ← main
```

## Tools

The agent has 7 tools available for the LLM to call:

| Tool | Description |
|------|-------------|
| `read-file` | Read file contents with line numbers (truncates at 2000 lines) |
| `write-file` | Create or overwrite a file, auto-creating parent directories |
| `edit-file` | Surgical find-and-replace with diff preview (must read first) |
| `bash` | Run shell commands (with safety denylist) |
| `grep` | Search files by pattern via ripgrep/grep |
| `find-files` | Find files by glob pattern |
| `list-dir` | List directory contents |

## Slash Commands

| Command | Description |
|---------|-------------|
| `/help` | Show available commands |
| `/clear` | Clear conversation history and start a new session |
| `/model [name]` | Show or switch the current model |
| `/compact` | Force conversation compaction |
| `/plan <goal>` | Investigate codebase and generate a plan |
| `/plan show` | Display the current plan |
| `/plan clear` | Discard plan, return to normal mode |
| `/approve` | Approve plan and start execution |
| `/next` | Execute next batch of plan steps |
| `/session list` | List saved sessions |
| `/session new` | Start a new session |
| `/session resume <id>` | Resume a previous session |
| `/usage` | Show token usage stats |
| `/status` | Show current model, session, message count |
| `/quit` / `/exit` | Exit pi-sema |

## Plan Mode

pi-sema supports a plan-before-execute workflow for complex tasks:

1. **Create a plan:** `/plan <goal>` — The agent investigates the codebase with read-only tools (no writes allowed) and generates a concrete implementation plan.
2. **Review the plan:** The plan is displayed immediately. Use `/plan show` to re-read it.
3. **Approve and execute:** `/approve` starts execution. The agent executes 2-3 plan steps, then pauses for your review.
4. **Continue:** `/next` executes the next batch of steps. Repeat until done.
5. **Abort:** `/plan clear` discards the plan and returns to normal mode.

### Why plan mode?

- **Alignment** — You see exactly what the agent intends before any files change.
- **Safety** — Planning uses a separate read-only agent that physically cannot write files or run commands.
- **Control** — Execution pauses every 2-3 steps so you can course-correct.

Plans are persisted to the session file, so they survive `/session resume`.

## AGENTS.md Support

pi-sema automatically discovers and loads context files by walking up the directory tree from the working directory:

- `AGENTS.md`
- `.pi-sema/agents.md`
- `CLAUDE.md`

These are injected into the system prompt as project context, giving the agent awareness of project conventions, build commands, and architecture.

## Skill Discovery

Place `.md` files in `.pi-sema/skills/` in your project directory. Each file becomes an available skill listed in the system prompt. The first non-heading line is used as the skill description.

## Session Management

Conversations are persisted as append-only NDJSON files in `~/.pi-sema/sessions/`. Each session file contains:

- A session header (ID, model, working directory, timestamp)
- Message entries (role + content)
- Compaction entries (summaries of older messages)
- Model change entries

Use `/session list` to browse past sessions and `/session resume <id>` to pick up where you left off.

## Conversation Compaction

When the estimated token count exceeds 80,000 tokens, pi-sema automatically summarizes older messages using the LLM, keeping the 10 most recent messages intact. The summary focuses on files touched, commands run, key decisions, and current task state. You can also trigger this manually with `/compact`.

## Differences from pi.dev

**Included:** File read/write/edit, shell execution, grep/find, session persistence, context loading, conversation compaction, plan mode with tool-gated execution, slash commands, tool call rendering with spinners.

**Not included:** Multi-file diffs, permission system, web search, image handling, MCP server integration, cost tracking per session.
