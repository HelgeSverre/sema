# docs_search accuracy tuning — domain-specialized retrieval

**Date:** 2026-06-25
**Status:** DEFERRED (planning only; not started). See `docs/deferred.md` → DOCS-SEARCH-1.
**Builds on:** `docs/plans/2026-06-25-mcp-docs-search.md` (the shipped BM25 `docs_search` tool).

## Why this plan exists

The shipped `docs_search` is a generic-ish lexical BM25 ranker (recall@5 ≈ 0.93 on a
keyword-ish oracle). Measured weakness: **vague, intent-only queries** where the user's
words don't overlap the docs' words — ~6/18 such queries missed (save→`file/write`,
"each item"→`map`, scramble→`hash/sha256`, "object to text"→`json/encode`, "same
time"→`async/all`, "N times"→`dotimes`). Pure lexical retrieval cannot bridge a
vocabulary gap.

## Strategic insight (the unlock)

This engine is **not generic** and never has to be. It is one frozen index over one fixed
corpus (819 structured entries) answering one question: *"which Sema symbol does this
coding intent map to?"* Crucially, **"no LLM at query time" says nothing about build
time.** The corpus is known at compile time, so we can spend arbitrarily expensive offline
compute — including a frontier LLM — to pre-chew the corpus and **bake** the results
(a regenerated, committed artifact like `builtin_docs.generated.json`; the binary build
itself stays offline/deterministic). The query path stays a fast, offline, deterministic
ranker that still satisfies the `FROM scratch` / no-network gate.

At 819 docs, **latency is a non-issue** (sub-ms; brute force beats any ANN). "Performance"
here means *build-once, zero first-query cost*. The real prize is **conceptual-query
accuracy**.

## Levers, ranked by impact

### Tier 1 — big accuracy unlocks
1. **Build-time document expansion (doc2query), baked.** An offline LLM generates a
   high-weight `search_aliases` field per entry: 5–15 paraphrased intent phrases, concept
   synonyms (save↔write↔persist, serialize↔encode↔marshal), and a "use this when…" line.
   Turns the vocabulary-gap problem into a build-time data problem. *Highest leverage.*
2. **Popularity prior (data already exists).** Bake the per-symbol call-frequency we
   computed across all `.sema` code; use it as a mild prior + tiebreaker so common
   functions win ambiguous queries (`map` over `llm/pmap`, `filter` over `regex/match`).
   A generic engine has no such signal.
3. **Hybrid lexical + offline semantic.** Precompute doc embeddings at build time, bake the
   vectors (≈0.3–1.2 MB). Embed the *query* at runtime with a **pure-Rust static embedding**
   (model2vec-style baked token→vector table + mean-pool) — sidesteps the ONNX/musl/scratch
   blocker. Fuse BM25 + cosine via reciprocal-rank fusion. Robust to keyword OR concept.

### Tier 2 — squeeze and harden
4. **Data-tuned ranking.** Generate a large gold set offline (LLM query→expected-symbol
   pairs, ~1–2k, held-out split). Tune k1/b, field weights, boosts, fusion weights by
   coordinate ascent on recall@5/MRR. Optionally a tiny **linear learning-to-rank** model
   over precomputable features (BM25, name-match, module-match, cosine, popularity,
   alias-hit), baked as constants — a dot-product at query time.
5. **Exploit corpus structure (domain signals).** Index `aliases` heavily
   (`car`↔`first`, `string-append`↔`string/append`); use the `see_also` graph for
   discovery; index `examples` code identifiers; Sema-aware tokenizer (operator words
   plus→`+`/star→`*`/equals→`=`/arrow→`->`, `?`/`!` predicates, `/`-namespace splitting);
   typo tolerance via edit-distance to the **fixed** corpus term lexicon (accurate precisely
   because the vocabulary is closed).

### Tier 3 — optional / later
6. **Two-stage rerank:** retrieve top-20 (hybrid), rerank to top-5 with the learned model
   (free at this scale).
7. **Real-query refinement:** if MCP usage telemetry becomes acceptable, mine actual agent
   queries to refine expansions.

## The permanent enabler: an eval harness
Bake the gold query set (~1–2k pairs, LLM-generated + hand-curated, held-out) as a committed
fixture, with a `cargo test`/bench reporting **recall@1 / recall@5 / MRR** on every change.
This makes every lever a measured A/B and catches regressions (cf. the earlier aggressive
name-boost that hurt recall@1).

## Recommended rollout (each step measured against the harness)
- **Phase 0** — gold eval set + harness. *Can't improve what we can't measure.*
- **Phase 1** — doc expansion (lever 1) + popularity prior (lever 2). Biggest gain, pure
  data + lexical, zero new query-time deps, gate-safe.
- **Phase 2** — tune ranking (lever 4) + structure signals (lever 5). Cheap squeeze.
- **Phase 3** — static-embedding hybrid (lever 3). Most code/binary cost; only if Phases
  1–2 leave conceptual gaps.

Phases 1–2 likely close most conceptual misses with **no new query-time dependencies** and
full scratch-gate compliance; Phase 3 is the reach goal.

## Risks / tradeoffs
- **Build-time LLM dependency:** introduce it like the pricing snapshot — a regeneration
  script producing a *committed, reviewable* artifact; binary build stays offline.
- **Expansion can hurt precision** (false friends) — harness + conservative fusion guard it.
- **Overfitting the synthetic oracle** — held-out split, diverse generation, some
  hand-authored queries.
- **Static embeddings add binary size** (few MB) — measure; the one lever with real cost,
  hence last.

## Bottom line
The win is **doing the hard, LLM-powered thinking once at build time and freezing it**, plus
exploiting signals (popularity, structure, closed vocabulary) a generic engine can't assume.
Deferred until conceptual-query quality is worth the investment.
