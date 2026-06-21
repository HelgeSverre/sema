# Dynamic Workflows in Sema — Scoping & Research

**Status:** Scoping / research (2026-06-21). Not started. This is the *thinking* doc
that sits in front of the implementation plan; it reconciles GH issue #41 +
draft PR #42 with patterns stolen from Mastra, LangGraph, and coding-agent
harnesses, and proposes a smaller, better-grounded path to a first useful spike.

**Companions:**
- Issue #41 (design brain-dump, open) + draft PR #42 (`docs/plans/2026-06-12-dynamic-workflows.md`).
- `docs/plans/2026-06-21-llm-cassettes.md` — record/replay; the determinism/replay
  substrate this doc leans on.
- `docs/IDEAS.md` — `defpipe` ("typed LLM pipelines"), `sema test` (LLM mocking).

> **TL;DR.** The central thesis of #41/#42 is correct and worth keeping: a
> *deterministic Sema orchestrator* that spawns *LLM-backed leaf workers*. But the
> draft over-commits to infrastructure (a `sema-workflowd` daemon, SSH remote
> workers, a dashboard, Living Code evolution) before the two genuinely hard,
> genuinely novel pieces have been de-risked: **(a) a parallel scheduling
> primitive on a single-threaded `Rc` runtime, and (b) durable, replayable
> runs.** This doc recommends shrinking the first deliverable to a *library-level*
> workflow combinator set + a frozen JSONL run-journal, validated against one
> real demo, with no new daemon and no new value-type until the scheduler spike
> proves out.

---

## 1. The problem, and who actually wants it

### What dynamic workflows solve for Sema

Sema already ships the *leaf* of agentic work: `defagent`, `agent/run`, `deftool`,
the tool loop, budgets, cache, fallback, embeddings, vector store. What it lacks
is everything *above a single agent call*: a way to compose several agent/tool/
LLM steps into a run that is **reproducible, resumable, inspectable, budgeted,
and cheap to debug**.

Today, if you want "inventory N files → audit each → verify findings → write a
report", you hand-roll it in a `.sema` script with `map`/`let`/`for`, and you get
none of: a run identity, a per-step event log, mid-run resume after a crash,
bounded parallel fan-out, a structured failure model, or per-step cost
attribution. Every nontrivial agentic script re-grows the same orchestration
scaffolding — which is exactly the "stop rewriting the agent loop" pitch, one
level up. **Dynamic workflows make the *orchestration* scaffolding part of the
runtime, the same way LLM calls already are.**

The defining design constraint (from #41) is the whole value and must be
preserved verbatim:

> **The orchestrator is deterministic Sema code; only the leaf workers touch the
> LLM/filesystem/shell/network.**

This is the lesson every serious harness converged on (LangGraph, Mastra,
Inngest/Temporal-style durable execution, Claude Code's own workflow model): keep
the *plan* deterministic and replayable, let the *workers* be probabilistic.
Sema is unusually well-positioned to sell this because it *already markets a
deterministic cooperative scheduler* — "deterministic, replayable agent runs" is
that same story extended to I/O, not a me-too feature.

### Who wants it

- **The owner / flagship demos.** `examples/pi-sema` (the coding agent) and the
  `ai-tools` CLIs are exactly the shape that wants orchestration above a single
  agent. A "audit this repo for missing-auth, reproduce N known findings" demo is
  a concrete, sellable artifact.
- **Playground / notebook users** who want a multi-step LLM pipeline they can
  *watch* execute and replay offline.
- **People shipping a small agentic automation to a box** — the "compile a
  workflow + its agents into one binary via `sema build` and scp it to a VPS"
  story is differentiated and real, *but it is an endgame, not an MVP.*

Honest scoping note: this is a "make the great demo greater" feature, not a
correctness fix. The LLM audit (separate findings) flagged real bugs —
OpenAI-family tool-loop breakage, no CI mock — that are higher priority than any
of this. **Dynamic workflows should not jump that queue.** The cassette/mock work
(`2026-06-21-llm-cassettes.md`) is a *prerequisite* here anyway: without
deterministic replay there is no way to test a workflow runtime in CI.

---

## 2. Survey of borrowed models, and what fits Sema

Three families were studied. For each: the model, then the verdict against Sema's
strengths (homoiconic Lisp, bytecode VM, single-threaded `Rc`, deterministic
cooperative scheduler, existing `defagent`/sandbox/`sema build`).

### 2.1 Mastra — workflow engine: steps + combinators + suspend/resume

**Model.** `createStep()` = typed unit (input/output/state schema + execute);
`createWorkflow().then(...).commit()` composes them. Control flow is a small
combinator vocabulary: `.then` (sequential), `.parallel([…])` (fan-out, results
keyed by step id), `.branch([[cond,step],…])` (first-true), `.dountil`/`.dowhile`
(loops with `iterationCount`), `.foreach(step,{concurrency:N})` (bounded map),
`.map()` (reshape via `getStepResult`/`getInitData`). A `stateSchema` +
`state/setState` shares values *without* threading them through every step's I/O.
Runs return a **discriminated union**: `{status: success|failed|suspended|
waiting|tripwire, input, steps, state}`. `suspend(payload)` snapshots the run to
storage; `resume({step, resumeData})` continues after a restart. Studio UI
visualizes the graph + live step status + traces.

**Fit with Sema — strong, the best single source.**
- The combinator set maps *more naturally* onto Lisp than onto TS method
  chaining: `.then`/`.branch`/`.map`/`.parallel` are just function composition +
  a few macros over an introspectable value. **Steal the vocabulary nearly
  verbatim.**
- Discriminated-union `{:status …}` results are idiomatic Sema (maps + `match`)
  and almost free. **Steal — it's the contract that makes everything else
  composable.**
- A run-scoped shared-state bag is trivial in Sema (`Rc<RefCell<Map>>`) and a big
  ergonomics win. **Steal.**
- `stateSchema`/typed-step-I/O depends on a schema validator Sema only half-has
  (the `llm/extract` validator). **Adopt the *shape*, reuse the extract
  validator, don't build a type system.**
- Studio is a notebook/`sema-notebook` extension — **defer; nice endgame.**

### 2.2 LangGraph — state machine + checkpointing + deterministic replay

**Model.** A graph of nodes over a shared, reducer-merged state object, keyed by a
`thread_id`. After each step the *serialized state* is checkpointed to a store.
That buys three things: **resume-after-crash, time-travel debugging, and forked
branches** — the LLM stays probabilistic while the *workflow* becomes
deterministic and replayable.

**Fit with Sema — the conceptual core, but adopt the *cheap* version.**
- "Determinism lives in the workflow, not the model" is precisely #41's thesis.
- Full per-step VM-state serialization (LangGraph's checkpointer) is **expensive
  and a poor fit** for Sema's `Rc`-graph VM; #41 already and correctly rejects
  arbitrary continuation persistence.
- The achievable version is **content-keyed step memoization**: cache a completed
  step's result under a hash of `(step-id, agent, model, system, normalized
  input, code-version)`; "resume" = re-run the deterministic workflow from the top
  and *short-circuit* any step whose key is already in the journal. This is
  exactly how `llm/with-cache` already works, generalized to steps. Honest
  caveat: this only works if step inputs serialize **canonically and stably** —
  canonical Sema-value serialization is itself an open item (see §4).
- **Steal:** checkpoint-as-memoization, `thread_id`-style run identity, replay =
  re-evaluation. **Don't steal:** a bespoke state-graph executor or continuation
  snapshots.

### 2.3 Coding-agent harnesses (Claude Code / OpenAI Agents SDK / eve / Flue / agent-browser) + OTel

**Model (converged kernel).** LLM-in-a-loop over tools with: an explicit
max-iteration cap (never `while true`); ground-truth feedback each step;
schema-validated structured output with a **bounded repair-retry** that feeds the
validation error back to the model (Instructor/DSPy); recoverable tool errors fed
*back into the loop* rather than aborting (agent-browser's "covered by X");
durable/replayable state (Flue's append-only event log; eve's checkpointed
steps); human-in-the-loop approval gates that pause without consuming compute; and
OpenTelemetry GenAI spans as the standard observability substrate (one CLIENT
span per LLM call, an `execute_tool` span per tool, nested under an
`invoke_agent` span).

**Fit with Sema.**
- **Flue's append-only event log as source of truth** is uncannily natural for a
  homoiconic Lisp: the journal *is* Sema data; append is trivial; "replay" is
  reading forms back. This validates #41's JSONL-first choice — but note Flue
  writes S-expressions/structured events, and the conservative-resume contract
  ("a tool call with no recorded result is *interrupted/unknown*, never assumed
  done") is a pattern to copy verbatim. **Steal both.**
- **Recoverable tool/step errors + bounded repair-retry** are the highest-leverage
  *reliability* patterns and apply to workflow steps, not just the inner agent
  loop. **Steal into the step executor.**
- **OTel GenAI spans** are the right observability model, but they belong to the
  *LLM-bulletproofing* track at the `sema-llm` chokepoint, not invented inside the
  workflow runtime. The workflow runtime should **emit the same events to its
  JSONL journal AND (when enabled) to OTel** — observability-as-byproduct
  (Mastra's insight). **Steal the event vocabulary; share the sink.**
- **HITL approval gates** map onto Sema's existing async yield-signal mechanism
  (`async_ops.rs` already has `set_yield_signal`/`take_resume_value`). **Steal —
  but as a later milestone.**

### 2.4 What does *not* fit (or is premature)

- Runtime-agnostic deploy targets / Cloudflare Durable Objects / swappable
  sandboxes (Flue/eve): irrelevant to a single-binary Lisp.
- A separate long-lived **daemon** (`sema-workflowd`) before there's a runtime to
  supervise: this is #42's biggest over-commit (see §6).
- SSH remote workers, HTTP worker daemons: endgame, not MVP.
- Genetic `evolve-workflow` / Living Code: the dependency (Living Code, PR #30)
  is **closed, not merged** — there is nothing to build on. Hard-defer.

---

## 3. Proposed design sketch

The design is **library-first**: workflows are ordinary Sema values produced by
macros, executed by a runtime that lives *in-process* (no daemon), journaling to a
**frozen** JSONL run-directory. Everything else (daemon, dashboard, remote,
evolution) is a later projection over that stable core.

### 3.1 Surface syntax

```sema
(import workflow)

(defworkflow audit-auth
  "Audit a codebase for missing authorization checks."
  {:args   {:paths [:list :string] :framework :string}
   :budget {:max-agents 64 :max-concurrent 8 :max-tokens 250000 :on-exceed :hard}
   :perms  [:read-repo :run-tests]}

  ;; phases are just labeled, journaled scopes — not control flow
  (phase "Inventory"
    (let [files (inventory (:paths args))]
      (checkpoint :files files)))

  (phase "Audit"
    ;; bounded fan-out: N items, :max-concurrent at a time, results in input order
    (let [findings (workflow/foreach auditor (checkpoint :files)
                                     {:concurrency (:max-concurrent (budget))})]
      (checkpoint :findings findings)))

  (phase "Verify"
    (let [verified (workflow/filter (fn [f] (:confirmed (agent/run verifier f)))
                                    (checkpoint :findings))]
      (checkpoint :verified verified)))

  (phase "Report"
    {:status :success :report (agent/run reporter (checkpoint :verified))}))
```

Design decisions baked into the surface:

- **`defworkflow` is a macro over a plain value + `run-workflow`**, not a special
  form (resolves PR #42 Open Q1). Keeping it a macro keeps the VM untouched,
  keeps workflows homoiconic/inspectable, and matches the prelude pattern
  (`->`, `when-let`, `dotimes` are all macros). The body is normal Sema code in a
  workflow-aware dynamic context.
- **`phase` is a journaled labeled scope**, not a phase-transition state machine.
  It opens/closes `phase.started`/`phase.ended` events and is otherwise
  transparent. (The `:phases [...]` list in #41's metadata is dropped — phases are
  discovered from the code, not pre-declared.)
- **Combinators** (steal from Mastra, name slash-namespaced per Decision #24):
  `workflow/foreach`, `workflow/parallel`, `workflow/branch`, `workflow/pipeline`
  (`.then` chain), `workflow/map`. `agent/run` already exists and is the leaf.
- **`checkpoint`** is dual-purpose: `(checkpoint :k v)` records *and* returns `v`;
  `(checkpoint :k)` reads the recorded value (this is the memoization hook for
  resume — see §3.3). It doubles as the run-scoped state bag (steal Mastra
  `state`).
- **`budget`** reads the active budget map; the macro wires `:budget` into the
  existing `llm/with-budget` mechanism so token/cost caps already work.
- **Workers**: the three shapes from #41 (in-process `defagent`, subprocess
  `.sema` over stdin/stdout JSONL, compiled exe) all present the same logical
  interface to `agent/run`. **MVP ships in-process only**; subprocess/compiled are
  Phase 3+ (resolves PR #42 Open Q2 — keep workers as `defagent`/value, add
  `defsubagent` only when subprocess lands).

### 3.2 Execution model

- A workflow run is a normal Sema evaluation in a **workflow-aware dynamic
  context** (thread-local, mirrors how the LLM budget/cache scopes already work).
  No new evaluator, no new VM opcodes.
- The context carries: `run-id`, the open journal sink, the checkpoint/state map,
  the active budget, and a **scheduler handle** for bounded parallelism.
- **Parallelism is the one genuinely hard piece** and is deliberately *not*
  in-VM threads (Sema is single-threaded `Rc`). Two candidate substrates, to be
  decided by the §5 spike:
  1. **Subprocess fan-out** (preferred for isolation): `workflow/foreach`/
     `parallel` over *subprocess* leaf agents, supervised by a bounded pool;
     results collected in input order; cancellation = kill children. This is the
     only model that gives true parallel LLM I/O without touching the VM, and it
     aligns with the "leaf workers do effects" constraint.
  2. **Reuse the existing async/channel scheduler** (`async_ops.rs`:
     `async/spawn`, channels, yield signals) for *in-process* concurrent LLM
     calls. `llm/pmap`/`llm/batch` already do concurrent provider calls via
     `join_all` on one tokio runtime — generalizing that to arbitrary steps is
     mostly wiring, but it's I/O-concurrency, not CPU-parallelism, and it shares
     one VM.
  Recommendation: spike (1) first because it doubles as the isolation/`sema build`
  story; fall back to (2) for the in-process MVP if (1) is too big.

### 3.3 Determinism & replay

- **Orchestrator is deterministic; workers are not.** Replay determinism is
  achieved by **content-keyed step memoization** (LangGraph-lite), not state
  snapshots:
  - Each leaf call (`agent/run`, `workflow/foreach` element, etc.) and each
    `checkpoint` is keyed by a stable hash of its inputs + code version and its
    result is written to the journal.
  - `(workflow/resume <run-id>)` re-evaluates the *same deterministic workflow
    code* from the top; any step whose key is already journaled returns the
    recorded result instead of re-executing. The deterministic skeleton + cached
    leaves = a faithful resume without persisting VM state.
- **The conservative-resume contract (steal from Flue):** a leaf call that has a
  recorded *start* but no recorded *result* is `:interrupted` / unknown on resume,
  **never assumed complete**. External side effects (file writes, HTTP) require
  app-level idempotency — documented honestly, not promised away.
- **Two prerequisites, both real open items:**
  1. **Canonical Sema-value serialization** (stable key ordering, no float/NaN
     ambiguity) so input hashes are stable across runs. This is the long pole and
     must be designed before resume can be trusted.
  2. **Deterministic LLM replay** = the cassette layer
     (`2026-06-21-llm-cassettes.md`). A workflow replayed with a recorded cassette
     is byte-deterministic end-to-end; without one, only the *orchestration* is
     deterministic and leaf outputs vary. These two docs should ship in lockstep.

### 3.4 Error handling

- **Discriminated-union run result** (steal Mastra): every workflow and step
  returns/normalizes to `{:status :success|:failed|:suspended|:waiting … }`.
  Callers `match` on `:status`. No bare throws across step boundaries.
- **Recoverable step errors (steal from harnesses):** a failing `agent/run` /
  tool call inside a step is captured and, by policy, fed back as a structured
  error result so the workflow can branch on it — *not* propagated to abort the
  whole run by default. (This mirrors the recommended fix to the inner tool loop;
  the workflow layer must not regress it.)
- **Bounded repair-retry for structured step outputs:** when a step declares an
  output schema, validate and re-prompt with the concrete validation error, capped
  at N attempts, then surface `{:status :failed …}` (reuse the `llm/extract`
  validator + reask machinery).
- **Budgets are hard caps on continuation** (matches today's `llm/with-budget`
  semantics): the tripping call completes, then the run aborts with
  `{:status :failed :reason :budget-exceeded}`. Document the post-call /
  batch-overshoot nuance honestly.

### 3.5 Observability tie-in

- **Single event vocabulary, multiple sinks.** Steps emit a small, *frozen* set of
  events (`run.started`, `phase.started/ended`, `agent.started/result`,
  `agent.tool_call`, `checkpoint`, `budget`, `run.ended`). The default sink is the
  JSONL journal; when `SEMA_OTEL=1` the *same* events also become OTel GenAI spans
  (shared with the `sema-llm` chokepoint work). Metrics (tokens, cost, duration)
  derive from the events — observability-as-byproduct, not a parallel system.
- The **run directory is the stable public contract** (treat like the `.semac`
  bytecode format: documented as single source of truth):
  ```
  .sema/runs/<run-id>/
    events.jsonl     # append-only, the system of record
    args.json
    metadata.json    # workflow name, code version, budget, perms
    result.json      # the final {:status …} envelope
    checkpoints/      # content-keyed step results (resume memo)
    artifacts/
  ```
- Dashboard / SQLite / `sema-notebook` graph rendering are **read-only
  projections** over `events.jsonl` and ship later. `tail -f | jq` works on day
  one.

---

## 4. Open questions & risks

**Open questions (incl. PR #42's five, with proposed answers):**

1. *`defworkflow` special form vs macro?* → **Macro** over a value + `run-workflow`
   (§3.1). Keeps the VM untouched and workflows homoiconic.
2. *`defsubagent` vs plain maps for workers?* → **Plain `defagent`/values for MVP**;
   introduce `defsubagent` only when subprocess workers land (Phase 3).
3. *Where do journal schemas live?* → A **versioned spec doc**
   (`docs/internals/workflow-journal.md`) as single source of truth, mirroring the
   bytecode-format treatment; CLI/dashboard/SQLite/remote all read it.
4. *One binary vs `sema-workflowd` day-one?* → **One binary, no daemon for MVP.**
   The runtime is in-process; a daemon is only justified once long-running /
   remote / dashboard-serving exist.
5. *Minimal stable event vocabulary?* → The ~8 events in §3.5; freeze them before
   anything reads them.
6. *(new)* Canonical Sema-value serialization for step keys — design first?
7. *(new)* How do `:budget`/`:perms` compose with nested workflows and subprocess
   workers (inheritance vs re-declaration)?
8. *(new)* What's the acceptance demo, exactly? (No demo = "useful" is undefined.)

**Risks (highest first):**

- **R1 — Parallel scheduling on a single-threaded `Rc` VM.** No `Task`/`Future`/
  `AgentHandle` value type exists; the only concurrency is async/yield channels.
  `parallel`/`foreach` is the make-or-break unknown and #41/#42 hand-wave it. **If
  this can't be built cleanly, the whole feature degrades to sequential — still
  useful, but the pitch shrinks.** Gate everything on the §5 spike.
- **R2 — Resume correctness depends on canonical serialization** that doesn't exist
  yet. A wrong/unstable hash silently re-runs (cost) or silently skips (wrong
  result) steps. Treat as a correctness feature with its own tests.
- **R3 — Determinism is softer than the pitch.** Orchestrator is deterministic;
  leaf outputs are not without cassettes. Be precise in docs to avoid the same
  "oversold claim" problem the LLM audit found.
- **R4 — Structured agent output is assumed but absent.** `agent/run` returns a
  string; the whole "filter verified findings onward" model needs typed results.
  This is a dependency, not a sub-task.
- **R5 — Scope creep / over-infrastructure.** PR #42 already lists daemon +
  dashboard + SQLite + SSH + Living Code. Building any of those before the core
  is validated burns the budget on the wrong thing.
- **R6 — Living Code dependency is dead** (PR #30 closed). Phase 6 has no
  foundation. Hard-defer.
- **R7 — Priority.** Real LLM-loop bugs and the missing CI mock outrank this.
  Dynamic workflows should land *after* (and on top of) the cassette/mock work.

---

## 5. Staged experimentation plan (smallest useful spike first)

Each stage has an explicit acceptance oracle. Do not start a stage before the
previous one's gate passes.

### Spike 0 — Scheduler de-risk (GATE for the whole effort)
Prove the parallel primitive can exist. Build a throwaway `workflow/foreach` over
**subprocess** leaf agents (or over `async/spawn` channels) with bounded
concurrency, input-order-preserving results, and cancellation. Decide and
document whether a `Task`/`AgentHandle` value type lands or whether the existing
async machinery suffices.
**Acceptance:** run 8 trivial subprocess "agents" 3-at-a-time, get results in
input order, cancel mid-run cleanly. **If this is infeasible in a few days, scope
the feature to *sequential-only* and re-plan.**

### Spike 1 — Sequential runtime + frozen journal (smallest useful thing)
`(defworkflow …)` macro + `phase` + `checkpoint` + `log`, executed in-process,
**sequential only**, writing the run directory (§3.5) with the frozen ~8-event
vocabulary, returning a `{:status …}` envelope. `sema workflow run <file>
--args <json>` CLI; no dashboard, no parallel, no daemon, **local in-process
`defagent` workers only**.
**Acceptance:** a fixture workflow produces a **golden `events.jsonl` snapshot**
and a `result.json`; the run dir is `jq`-inspectable.

### Spike 2 — One real demo as the acceptance oracle
Pick a concrete fixture: a small repo (e.g. a Laravel sample) with **N known
missing-auth findings**. Write `audit-auth.sema` (inventory → audit → verify →
report) using Spike 1 + the cassette layer for deterministic leaf calls.
**Acceptance:** the workflow reproduces the N expected findings, recorded once as
a cassette, and runs **green in CI offline** as a golden integration test. *This
test is the definition of "useful."*

### Spike 3 — Bounded parallel + structured step output
Land `workflow/foreach`/`parallel` (from Spike 0) and `:schema` validation +
repair-retry on step/agent results so `verify` can branch on typed findings.
**Acceptance:** the demo's audit phase fans out N-at-a-time with deterministic
replay; a malformed agent result triggers one repair-retry then a clean
`:failed`.

### Spike 4 — Resume via content-keyed memoization
`(workflow/resume <run-id>)` re-runs the skeleton and short-circuits journaled
steps; interrupted-without-result steps are re-run; requires canonical value
serialization (design + test it here).
**Acceptance:** kill the demo mid-`Audit`, resume, and complete without re-calling
any already-journaled agent (verified via the cassette/usage counters).

### Later (each its own plan, explicitly deferred)
- Subprocess + compiled (`sema build`) workers; `defsubagent`.
- Dashboard / SQLite projection over the journal; `sema-notebook` graph view.
- OTel span emission (shared with `sema-llm` chokepoint).
- HITL approval gates (over the async yield mechanism).
- Remote SSH workers.
- Living Code / `evolve-workflow` — **only after PR #30 (Living Code) actually
  merges.**

---

## 6. Reconciliation with PR #42 — keep / change / discard

**Keep (the spine — #41/#42 got this right):**
- The central constraint: deterministic Sema orchestrator + LLM-leaf workers.
- Workflow logic in normal Sema code, no external JSON DSL.
- JSONL-first run journal as system of record; SQLite/dashboard as later
  projections; the `.sema/runs/<run-id>/` directory shape.
- The three convergent worker shapes (in-process / subprocess / compiled) as the
  *long-term* target, converging on one logical interface.
- The phased non-goals list (no autonomous manager planning, no sockets-first, no
  remote HTTP daemons in MVP, no self-modification in MVP).
- Mapping worker permissions onto existing sandbox flags.

**Change:**
- **`defworkflow` → macro, not special form** (PR #42 Open Q1). VM stays untouched.
- **No `sema-workflowd` daemon for MVP** (PR #42 Open Q4). Runtime is in-process;
  introduce a daemon only when long-running/remote/dashboard demand it. PR #42's
  "manager owns scheduling + budgets + retries + remote routing + dashboard" is
  too much surface for "dumb" and is premature.
- **Reorder the phases around the scheduler spike.** PR #42 puts the journal first
  and treats `parallel` as a Phase-3 checkbox. Make **scheduler de-risk Spike 0
  the gate**, because it's the actual unknown.
- **Add an explicit acceptance demo** (Spike 2). PR #42 has a testing plan but no
  end-to-end "useful" oracle — so "useful" is undefined.
- **Make the cassette layer + canonical serialization explicit dependencies**, not
  implicit. Resume and CI testability are impossible without them.
- **Add the discriminated-union result, run-scoped state bag, recoverable step
  errors, and bounded repair-retry** (stolen from Mastra/harnesses) to the core —
  PR #42 under-specifies the step result/error model.
- **Drop pre-declared `:phases [...]`** from metadata; discover phases from code.

**Discard (or hard-defer):**
- **Living Code / `evolve-workflow` (PR #42 Phase 6).** Dependency (PR #30) is
  closed/unmerged — nothing to build on. Remove from the plan until it merges.
- **SSH/remote workers and HTTP daemons as anything but a far-future stage.**
- **Dashboard/SQLite as early milestones** — they are read-only projections and
  must follow a *frozen* journal, not precede it.
- **Sockets-first worker protocol** (already a #42 non-goal; keep it discarded).

---

## 7. Bottom line

The idea is genuinely interesting and the thesis is right. The path to "useful"
is *narrower* than #41/#42 imply: **(1)** prove the parallel scheduler can exist
at all; **(2)** ship a sequential, in-process runtime with a frozen JSONL journal
and a discriminated-union result; **(3)** pin one real demo as the offline,
cassette-backed acceptance test; *then* layer parallel, structured output,
resume, and — much later — daemon/dashboard/remote/evolution. Steal Mastra's
combinator vocabulary and result model, LangGraph's
checkpoint-as-memoization-not-snapshot insight, and the harnesses'
append-log-as-truth + recoverable-error + conservative-resume contracts. Defer
everything that needs a daemon or the (dead) Living Code work.
