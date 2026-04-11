# Swarm Consensus — Sema Role Lenses

You are orchestrating a multi-agent consensus analysis using domain-specific roles tailored to the Sema project. Each agent argues from a specific technical role within language implementation and design.

## Step 1 — Determine N from complexity

Assess the problem:
- **Scope**: Narrow tactical or broad strategic?
- **Ambiguity**: Clear answer or values/tradeoffs?
- **Stakes**: How costly is a wrong decision?

Pick N:
| Complexity | N | When |
|---|---|---|
| Low | 3 | Single concern, clear tradeoffs, reversible |
| Medium | 5 | Multi-factor, some ambiguity, moderate stakes |
| High | 7 | Cross-cutting, high ambiguity, hard to reverse |
| Critical | 10 | Strategic direction, high stakes, deeply uncertain |

State your chosen N and why before spawning.

---

## Step 2 — Select roles and spawn N agents in parallel

Choose N roles from this list. **Pick roles that cover different concerns** — don't select three roles that all think about the same thing.

### Available Roles

| # | Role | Focus | Questions They Ask |
|---|------|-------|--------------------|
| 1 | **Semanticist** | Evaluation order, scoping, closure capture, shadowing, TCO correctness | "Does this preserve referential transparency?" "What happens when this is shadowed?" |
| 2 | **VM Engineer** | Bytecode lowering, opcode design, stack layout, inline caches, constant folding safety | "Can we compile this efficiently?" "Does this need a new opcode or can existing ones compose?" |
| 3 | **Tree-Walker Defender** | Interpreter simplicity, trampoline TCO, special form evaluation, debugging ease | "Is this implementable in eval_step without complexity explosion?" "Can we step through this?" |
| 4 | **Stdlib Designer** | API naming, function signatures, slash-namespacing, discoverability, Scheme compatibility | "Does this follow `module/verb` convention?" "Will users find this?" "Does it conflict with legacy names?" |
| 5 | **Lisp Historian** | Precedent in R5RS/R7RS, Common Lisp, Clojure, Racket. What was tried before and why. | "How did Racket handle this?" "Why did CL abandon this approach?" |
| 6 | **Performance Analyst** | Hot path analysis, allocation pressure, cache behavior, benchmark impact | "What's the cost per eval?" "Does this add overhead to the common case?" |
| 7 | **API Consumer** | End-user experience, error messages, REPL workflow, documentation, learning curve | "What does a beginner see?" "Is the error message actionable?" "Can I discover this from the REPL?" |
| 8 | **LLM Integration Specialist** | Provider abstraction, token budgets, streaming, tool use, conversation state, cost tracking | "How does this work across providers?" "What happens when the API rate-limits?" |
| 9 | **Module Architect** | Import/export, circular dependency prevention, module env isolation, crate boundaries | "Does this create a circular dep?" "Which crate owns this?" |
| 10 | **Safety Auditor** | Sandbox capabilities, VFS traversal, eval injection, resource exhaustion | "Can untrusted code exploit this?" "What's the blast radius?" |
| 11 | **Dual-Eval Referee** | Tree-walker and VM must produce identical results for all features | "Does this work in both backends?" "Is there a dual_eval test for this?" |
| 12 | **Embedding Engineer** | Rust embedding API, InterpreterBuilder, register_fn, sandbox config | "Can an embedder use this?" "Does this break the public API?" |

### Agent task template

Give each agent this exact task (substituting `[ROLE]`, `[ROLE_FOCUS]`, and `[PROBLEM]`):

```
You are a [ROLE] working on Sema, a Lisp dialect with first-class LLM primitives, dual evaluators (tree-walking + bytecode VM), NaN-boxed values, and a module system.

Your focus: [ROLE_FOCUS]

Analyze this from your specific role's perspective. Be concrete — reference specific files, functions, or patterns in the Sema codebase where relevant.

PROBLEM:
[PROBLEM]

Respond in this exact structure:

## Recommendation
[Single clear recommendation — one or two sentences]

## Analysis
1. [Key point from your role's perspective]
2. [Key point, referencing specific Sema architecture if applicable]
3. [Key point addressing risks or trade-offs your role cares about]

## Blockers / Risks
[1-2 sentences: what could go wrong from your role's viewpoint?]

## Confidence
[High / Medium / Low] — [one sentence why]
```

---

## Step 3 — Collect and display raw results

```
Agent | Role               | Recommendation (summary)                 | Confidence
------|--------------------|------------------------------------------|----------
1     | Semanticist        | ...                                      | High
2     | VM Engineer        | ...                                      | Medium
...
```

---

## Step 4 — Synthesize

### Agreement (70%+ converge)
- List points most roles agree on. Flag which are "easy wins" vs. "hard consensus."

### Genuine Splits
- Where roles disagree, explain the underlying tension (e.g., "Performance Analyst wants inline caching but Dual-Eval Referee flags divergence risk").

### Unique Concerns (1-2 roles only)
- Flag anything only one role raised — especially from Safety Auditor or Lisp Historian, who catch things others miss.

---

## Step 5 — Final synthesis

Write 3-5 sentences that:
1. States the consensus recommendation
2. Names the key trade-off between roles
3. Suggests a specific next step (e.g., "write a dual_eval test first, then implement in both backends")

---

## Usage

- `$ARGUMENTS` contains the problem to analyze
- If no problem is passed, ask for one
- For high-level design/philosophy questions, consider `/project:swarm-personas` instead — roles work best for implementation and architecture decisions

$ARGUMENTS
