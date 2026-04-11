# Swarm Consensus

You are orchestrating a multi-agent consensus analysis. Your job is to spawn N independent agents, collect their responses, and synthesize consensus/divergence/outliers.

## Step 1 — Determine N from complexity

Assess the problem on these axes:
- **Scope**: Is this a narrow tactical question or a broad strategic one?
- **Ambiguity**: Is there a clearly correct answer, or does it depend on values/tradeoffs?
- **Stakes**: How costly is a wrong decision?
- **Domain breadth**: Does it touch multiple disciplines?

Pick N:
| Complexity | N | When |
|---|---|---|
| Low | 3 | Single-domain, clear tradeoffs, reversible |
| Medium | 5 | Multi-factor, some ambiguity, moderate stakes |
| High | 7 | Cross-domain, high ambiguity, hard to reverse |
| Critical | 10 | Strategic, high stakes, deeply uncertain |

State your chosen N and why before spawning.

---

## Step 2 — Spawn N agents in parallel

Use the Task tool to launch all N agents **simultaneously** (not sequentially).

Each agent gets the same core problem but a distinct **analytical lens** framing. Assign lenses from this list (rotate through them for larger N):

1. **First-principles** — Strip away assumptions. What is fundamentally true here?
2. **Skeptic** — What is most likely to go wrong? What are the weakest assumptions?
3. **Optimist** — What is the best realistic outcome? What would have to be true for this to work brilliantly?
4. **Risk analyst** — What are the tail risks? What failure modes are non-obvious?
5. **Contrarian** — What does conventional wisdom get wrong here? What is the counterintuitive answer?
6. **Pragmatist** — Ignoring what's ideal, what is actually executable given real constraints?
7. **Systems thinker** — What are the second and third-order effects? What feedback loops exist?
8. **Devil's advocate** — Steelman the opposite position as strongly as possible.
9. **Historian** — What analogous situations exist? What did people learn the hard way?
10. **Minimalist** — What is the simplest possible solution that could work?
11. **Pre-mortem** — Assume it already failed. Work backwards: what specifically killed it?
12. **Economist** — What are the real incentives at play? Who wins, who loses, and will behavior match intent?
13. **User proxy** — Forget the builders. What does the person actually affected experience end-to-end?
14. **Futurist** — In 3–5 years, how does this look? What trends does it ride or fight against?
15. **Bottleneck finder** — What is the single constraint that limits everything else? Where is the true chokepoint?
16. **Ethicist** — Who bears the costs of this decision? What obligations are being honored or ignored?
17. **Maximalist** — What does the boldest, most ambitious version of this look like? What would going all-in unlock?
18. **Regulator** — What rules, power structures, or institutional inertia does this touch? What requires permission?
19. **Archaeologist** — Strip away all the new framing. What old problem is this actually? Has it been solved before?
20. **Synthesizer** — Where do the opposing views secretly agree at a deeper level? What framing resolves the apparent conflict?

### Agent task template

Give each agent this exact task (substituting `[LENS]`, `[LENS_INSTRUCTION]`, and `[PROBLEM]`):

```
You are analyzing a problem through a specific lens. Do not hedge or equivocate — commit to a position.

LENS: [LENS]
INSTRUCTION: [LENS_INSTRUCTION]

PROBLEM:
[PROBLEM]

Respond in this exact structure:

## Primary Recommendation
[Single clear recommendation or answer — one sentence]

## Top 3 Supporting Points
1. [Point]
2. [Point]
3. [Point]

## Key Risks / Caveats
[1–2 sentences max]

## Confidence
[High / Medium / Low] — [one sentence why]
```

---

## Step 3 — Collect and display raw results

After all agents complete, display a compact table:

```
Agent | Lens            | Primary Recommendation (summary)        | Confidence
------|-----------------|----------------------------------------|----------
1     | First-principles| ...                                    | High
2     | Skeptic         | ...                                    | Medium
...
```

---

## Step 4 — Synthesize: Consensus / Divergence / Outliers

Analyze the N responses and produce a structured synthesis:

### 🟢 CONSENSUS (agreement among 70%+ of agents)
- List the points/recommendations that 70% or more agents converged on
- These are your **safe bets** — high signal, low variance

### 🟡 DIVERGENCE (genuine 50/50 splits)
- List points where agents split roughly evenly
- Label *why* they split: values-based? data interpretation? timeframe differences?
- These are **real judgment calls** — no objectively correct answer, context determines choice

### 🔴 OUTLIERS (proposed by only 1–2 agents)
- List unique ideas that appeared in only 1–2 responses
- Mark each: ⚡ *High-variance / potentially high-value* — worth stress-testing separately
- Do NOT dismiss these. Outliers from well-reasoned lenses often surface creative solutions the consensus misses.

---

## Step 5 — Final synthesis paragraph

Write 3–5 sentences that:
1. States the consensus answer plainly
2. Names the most important unresolved judgment call
3. Flags the single most interesting outlier idea and why it deserves consideration

---

## Usage notes

- The `$ARGUMENTS` variable contains the problem to analyze
- If no problem is passed, ask the user for it before proceeding
- If the problem is multi-part, run a separate swarm per sub-question
- For follow-up analysis on a specific outlier, you can re-run with: `/project:swarm Stress-test this specific idea: [outlier]`

---

$ARGUMENTS
