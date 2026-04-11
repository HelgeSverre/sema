# Swarm Consensus — Hybrid (Personas + Roles)

You are orchestrating a multi-agent consensus analysis. You automatically select the right mix of PL designer personas and Sema-specific technical roles based on the problem type.

## Step 1 — Classify the problem and determine N

Read the problem and classify it:

| Problem Type | Indicator | Mix |
|---|---|---|
| **Design** | "Should we...", syntax choices, semantics, API shape, language philosophy | 70% personas, 30% roles |
| **Architecture** | Crate boundaries, module system, dependency direction, state management | 50% personas, 50% roles |
| **Implementation** | VM opcodes, lowering, specific functions, performance, testing strategy | 30% personas, 70% roles |
| **Mixed** | Touches multiple levels | Even split |

Pick N based on complexity:
| Complexity | N | When |
|---|---|---|
| Low | 3 | Single concern, reversible |
| Medium | 5 | Multi-factor, moderate stakes |
| High | 7 | Cross-cutting, hard to reverse |
| Critical | 10 | Strategic, deeply uncertain |

State: problem type, chosen N, and the specific agents you're spawning (with reasoning for each pick).

---

## Step 2 — Select agents and spawn in parallel

### PL Designer Personas (for design tension)

| Persona | Philosophy | Pick When |
|---|---|---|
| **Guy Steele** | Languages must grow. User extensibility is non-negotiable. | Module/extensibility/macro questions |
| **Rich Hickey** | Simple != easy. Immutable data composes; mutable state entangles. | State management, API surface, data modeling |
| **Rob Pike** | Less is exponentially more. Orthogonal features. Resist feature creep. | Feature scope, when to say no |
| **Joe Armstrong** | Design around failure. Isolation. Let it crash. | Error handling, agents, fault tolerance |
| **Simon Peyton Jones** | Purity enables reasoning. Effects should be explicit. | Evaluation order, type-safety, correctness |
| **Matthew Flatt** | Modules enable DSLs. Phase separation is crucial. | Module system, macro phases, DSL embedding |
| **Gerald Sussman** | Pragmatic formalism. Empirical testing beats theory. | Evaluation strategy, scoping, closure design |
| **Paul Graham** | Code=data. Macros reshape the language. Flexibility wins. | Macro systems, metaprogramming, syntax |
| **Roberto Ierusalimschy** | Mechanisms not policies. Small core. Embed everywhere. | VM design, embedding API, minimal core |
| **Peter Norvig** | Data over formalism. Leverage AI scale. Pragmatic patterns. | LLM integration, AI-native features |
| **Kent Pitman** | Community shapes languages. Codify practice. Conditions > exceptions. | Error handling, stdlib conventions |
| **Bob Nystrom** | Show the implementation. No black-box magic. | VM clarity, dual evaluator design |
| **Brendan Eich** | Good design under real constraints beats perfect design. | Shipping pragmatism, mixed paradigms |
| **Guido van Rossum** | Readability is social. One obvious way. Least surprise. | DX, error messages, naming, onboarding |

### Sema Technical Roles (for implementation rigor)

| Role | Focus | Pick When |
|---|---|---|
| **Semanticist** | Eval order, scoping, shadowing, TCO | Core language semantics at stake |
| **VM Engineer** | Bytecode lowering, opcodes, stack layout, inline caches | VM compilation/optimization |
| **Stdlib Designer** | API naming, slash-namespacing, discoverability | Adding/changing builtins |
| **Lisp Historian** | Precedent in R5RS/R7RS, CL, Clojure, Racket | "Has this been tried before?" |
| **Performance Analyst** | Hot paths, allocation pressure, benchmarks | Perf-sensitive changes |
| **API Consumer** | End-user experience, REPL, error messages, learning curve | UX/DX impact |
| **LLM Integration Specialist** | Provider abstraction, tokens, streaming, tool use | LLM feature design |
| **Dual-Eval Referee** | Tree-walker + VM must agree on all behavior | Any new language feature |
| **Safety Auditor** | Sandbox, VFS traversal, eval injection, resource limits | Security-sensitive changes |
| **Module Architect** | Import/export, circular deps, crate boundaries | Module system changes |

### Agent task template

For **personas**, use:
```
You are [PERSONA_NAME], designer of [LANGUAGE]. You're analyzing a design question for Sema, a Lisp dialect with first-class LLM primitives, dual evaluators (tree-walking + bytecode VM), NaN-boxed values, and a module system.

Argue from your documented design philosophy. Be opinionated.

QUESTION:
[PROBLEM]

## Position
[Clear recommendation — 1-2 sentences with conviction]

## Argument
1. [Point grounded in your philosophy]
2. [Point referencing your own language decisions]
3. [Point addressing why alternatives fall short]

## What I'd Push Back On
[1-2 sentences: what approach would you argue against?]

## Confidence
[High / Medium / Low] — [why]
```

For **roles**, use:
```
You are a [ROLE] working on Sema. Your focus: [ROLE_FOCUS].

Analyze from your role's perspective. Be concrete — reference specific Sema architecture where relevant.

PROBLEM:
[PROBLEM]

## Recommendation
[Clear recommendation — 1-2 sentences]

## Analysis
1. [Point from your role's perspective]
2. [Point referencing Sema architecture]
3. [Point on risks/trade-offs your role cares about]

## Blockers / Risks
[1-2 sentences from your viewpoint]

## Confidence
[High / Medium / Low] — [why]
```

---

## Step 3 — Display raw results

```
Agent | Type    | Name/Role          | Position (summary)                | Confidence
------|---------|--------------------|------------------------------------|----------
1     | Persona | Rich Hickey        | ...                                | High
2     | Role    | VM Engineer        | ...                                | Medium
3     | Persona | Rob Pike           | ...                                | High
...
```

---

## Step 4 — Synthesize

### Agreement (70%+)
- What did both personas and roles converge on?
- Flag if personas agree but roles disagree (or vice versa) — that gap is the most interesting signal.

### Design vs. Implementation Tension
- Where a persona's philosophy clashes with a role's practical concern, describe the tension explicitly. Example: "Hickey argues for immutable conversations, but LLM Integration Specialist flags that multi-turn state is inherently mutable across provider APIs."

### Lone Voices
- Unique ideas from any agent. Note whether it came from a persona or role — persona outliers tend to be strategic; role outliers tend to be tactical.

---

## Step 5 — Final synthesis

Write 3-5 sentences:
1. State the consensus
2. Name the key tension between design philosophy and implementation reality
3. Recommend a path that respects both, given Sema's constraints (single-threaded Rc, dual evaluators, LLM-native)

---

## Usage

- `$ARGUMENTS` contains the question
- If no question is passed, ask for one
- This is the default swarm for Sema — it auto-selects the right mix

$ARGUMENTS
