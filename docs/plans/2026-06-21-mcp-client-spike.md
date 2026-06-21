# MCP Client — spike & scoping (Sema as an MCP *client*)

**Status:** Scoping / design sketch (2026-06-21). Not started. Answers the two
questions raised: **how** would it work and **where** does it live (agent-only vs
whole-language)?

## Context: today Sema is an MCP *server* only

`crates/sema-mcp/` exposes Sema **to** external agents (protocol.rs, server.rs,
tools.rs). It already contains the Sema-params ↔ JSON-Schema conversion
(`tools.rs`) that an MCP *client* needs in reverse. There is **no MCP client** —
Sema cannot currently consume external MCP servers' tools.

## Why a client is worth it

MCP is becoming the universal tool protocol. An MCP *client* lets a Sema agent
use the entire external MCP ecosystem (filesystem, GitHub, Slack, browsers,
databases, …) without Sema writing a `deftool` for each. That is a large
strategic multiplier for the agentic story — and it composes with the existing
`deftool`/`defagent`/agent-loop machinery instead of replacing it.

## The "where" decision — layered, with a clear boundary

The instinct to ask "is this just for `defagent` or for the whole language?" is
the right one. Recommendation: **build it as two layers with a hard boundary.**

```
┌─────────────────────────────────────────────────────────────┐
│ Layer 2 — Agent adapter (thin)                               │
│   mcp/tools->sema : turn MCP tool descriptors into the SAME  │
│   value shape `deftool` produces, so `defagent` consumes them│
│   exactly like local tools. NO new agent concepts.           │
├─────────────────────────────────────────────────────────────┤
│ Layer 1 — General MCP client primitive (stdlib namespace)    │
│   mcp/connect, mcp/tools, mcp/call, mcp/close — usable by    │
│   ANY Sema code, agent or not. This is a transport + RPC     │
│   client, nothing agent-specific.                            │
└─────────────────────────────────────────────────────────────┘
```

**Boundary rule:** Layer 1 knows nothing about agents — it's a protocol client,
like `http/*`. Layer 2 is a ~20-line adapter that maps an MCP tool's
`{name, description, inputSchema}` onto a Sema tool value (the same triple
`deftool` builds: name, description, params-map, handler) where the handler is a
closure that calls `mcp/call`. This keeps the agent layer thin and means
non-agent scripts get MCP for free.

This mirrors how the codebase already separates the general `http/*` builtins from
higher-level `llm/*` features that use them.

## Layer 1 — surface sketch

```sema
;; Connect to an MCP server. Transport inferred from the spec.
(define fs (mcp/connect {:command "npx" :args ["-y" "@modelcontextprotocol/server-filesystem" "/tmp"]}))
;; or HTTP/SSE:
(define gh (mcp/connect {:url "https://mcp.example.com/sse"}))

(mcp/tools fs)
; => [{:name "read_file" :description "…" :parameters {…json-schema…}} …]

(mcp/call fs "read_file" {:path "/tmp/notes.txt"})
; => {:content "…"}   (MCP result, normalized to a Sema value)

(mcp/close fs)
```

- `mcp/connect` returns an opaque **handle** (an Rc-backed resource value, like a
  file/stream handle). Connection + `initialize` handshake happen eagerly.
- `mcp/tools` performs `tools/list`; `mcp/call` performs `tools/call`.
- Errors surface as `SemaError` (reuse the JSON-RPC error mapping).

## Layer 2 — agent integration sketch

```sema
(define fs (mcp/connect {:command "npx" :args ["-y" "@modelcontextprotocol/server-filesystem" "/tmp"]}))

(defagent librarian
  {:model "claude-…"
   :system "You manage files."
   ;; MCP tools become first-class agent tools, indistinguishable from deftool ones:
   :tools (mcp/tools->sema fs)})         ; <- the entire adapter surface
```

`mcp/tools->sema` produces values structurally identical to what `deftool` yields
(name, description, JSON-schema params, handler), so the existing agent loop's
tool-dispatch path needs **zero changes**. Optionally support a convenience
`:mcp-servers [fs gh]` key on `defagent` that calls `mcp/tools->sema` internally.

## Implementation placement & dependency graph

- Put the client in `crates/sema-mcp/` as a new `client/` module (transport +
  JSON-RPC + tool conversion), reusing `protocol.rs` types and inverting
  `tools.rs`'s schema conversion (JSON-Schema → Sema params map).
- Register the `mcp/*` builtins where the binary wires stdlib (the binary already
  composes `sema-stdlib` + `sema-llm`; add `sema-mcp` client builtins alongside).
  This keeps `sema-stdlib` free of an MCP dependency (respects the existing
  "stdlib has no heavy deps" rule).
- Layer 2 (`mcp/tools->sema`) lives wherever the agent/tool value shape is
  defined so it can produce the identical structure.

## Transport

- **Phase 1: stdio** (spawn a subprocess, JSON-RPC over stdin/stdout). This is by
  far the most common MCP server deployment and the simplest to test.
- **Phase 2: HTTP + SSE** (remote servers). Reuse `sema-llm`'s `http.rs`/`sse.rs`
  patterns and the sync-over-async (`block_on`) approach.

## Security & sandbox boundary (non-negotiable)

Connecting to an MCP server = **spawning a process** and/or **network I/O**, and
the called tools can do anything the server allows. This MUST be gated by the
existing capability bitset (ADR #62):

- `mcp/connect` via stdio requires the process-spawn capability; via URL requires
  the network capability.
- A sandboxed Sema program with neither capability cannot open MCP connections —
  same model as `shell`/`http`.
- Document clearly that MCP tools run with the *server's* authority, not Sema's
  sandbox — connecting to an untrusted MCP server is equivalent to running
  untrusted code. This is a docs + capability-gating concern, called out here so
  it isn't an afterthought.

## Determinism / testing tie-in

MCP `tools/call` is I/O, so it has the same nondeterminism problem as LLM calls.
Design the **cassette** tape format (see `2026-06-21-llm-cassettes.md`) with an
open `kind` field so `"mcp-call"` interactions can be recorded/replayed too — then
agent tests that use MCP tools stay deterministic and offline in CI.

## Milestones

- **M1 — stdio client primitive:** `mcp/connect` (stdio) + `initialize` +
  `mcp/tools` + `mcp/call` + `mcp/close`; capability-gated; a test against the
  reference filesystem MCP server. *Acceptance:* list + call a real MCP tool from
  Sema.
- **M2 — agent adapter:** `mcp/tools->sema`; a `defagent` that uses an MCP tool
  end-to-end (replayed via cassette in CI). *Acceptance:* an agent completes a
  task using an external MCP tool, deterministically in CI.
- **M3 — HTTP/SSE transport + `:mcp-servers` sugar on `defagent`.**
- **M4 — MCP-call cassette recording** (shared format with LLM cassettes).

## Open questions

- Handle lifecycle: explicit `mcp/close` vs. drop-based cleanup vs. both. Leaning
  both (drop closes; explicit for determinism).
- Reconnect/retry policy for long-lived agent sessions.
- Should `defagent` own connection lifetime, or should the user pass a live
  handle (current sketch)? Passing a handle is more composable; revisit after M2.
- Surface for MCP **resources/prompts** (MCP has more than tools) — defer until
  tools land; tools are 90% of the value.
