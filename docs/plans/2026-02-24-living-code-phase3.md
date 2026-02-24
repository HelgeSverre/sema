# Living Code Phase 3: LLM-Powered Introspection — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable Sema programs to ask questions about their own code and self-repair failing doctests using LLM integration.

**Architecture:** `ask`, `ask/code`, and `heal!` are special forms in sema-eval. They gather context from env metadata (Phase 1+2), build prompts, and call `llm/complete` by looking it up in the env and invoking via `sema_core::call_callback`. No sema-llm dependency needed — the LLM call goes through the env binding.

**Tech Stack:** Existing sema-eval special forms, sema-core callback system, sema-reader for parsing LLM responses.

---

### Task 1: `(ask target question)` — conversational introspection

Special form that takes a symbol (or string path, or value) and a question string. Gathers context, calls LLM, returns the answer as a string.

### Task 2: `(ask/code target instruction)` — code generation

Like `ask` but system prompt requests ONLY valid Sema code. Parses the response with `sema_reader::read` and returns a Value (executable code).

### Task 3: `(heal! fn)` — auto-repair via doctests

Special form that runs doctests on a function, and if any fail, enters a healing loop: builds a prompt with source + failure info, calls LLM for a fix, parses and evals the candidate in a sandbox, runs doctests against it, and if all pass, replaces the definition.

### Task 4: `sema test --heal` CLI flag

Extends the existing `sema test` command to attempt healing on functions with failing doctests.

---

## Dependency Graph

```
Task 1 (ask) ──→ Task 2 (ask/code) ──→ Task 3 (heal!) ──→ Task 4 (CLI --heal)
```

All sequential — each builds on the previous.
