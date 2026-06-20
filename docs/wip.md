# Work in progress — open threads from the May 2026 quality sweep

Status board for follow-up work that has been planned, partially explored, or that needs a design call before code can land. Each entry has enough context that you (or a future you / agent) can pick it up months from now without re-reading the audit.

For items that have been *parked* with a clear "we're not doing this", see `docs/deferred.md`. For items that are deemed shipped, see commits `09f6c21`, `01f250c`, `a5b37df`, `c2b551b`, `6c4086f`, `83cbd58`.

---

## Wave 6c — Design-then-code (need an ADR before fixing)

### D3 — `(match …)` should error on no-match

`crates/sema-eval/src/special_forms.rs:1813-1814` falls through with `Trampoline::Value(Value::nil())` when no clause matches. This is a Sussman-class lurking-bug — a non-exhaustive `match` is almost always a bug, and silently returning nil masks it.

**Proposed fix:** make the canonical `match` raise `:match-failed`. Introduce `match*` (or `match-or-nil`) for the lenient form. Update both backends + dual-eval tests.

**Why this needs a design call:** breaking-ish behavior change. Anyone whose code currently relies on the nil fall-through (probably some) would now error. We need to decide:
- Strict default + permissive variant (breaks existing code, safer long-term)
- Permissive default + strict variant `match!` (no break, but the safer behavior is opt-in)
- Compiler-warning on non-exhaustive `match` patterns we can statically detect (no runtime change, alerts users)

The first option is the right long-term call but needs a deprecation period and a changelog entry.

### N9 — Numeric domain policy

Inconsistent today (verified):
- `(/ 1 0)` → error "division by zero"
- `(sqrt -1)` → returns `NaN`
- `(pow 2 -1)` → silently float-promotes to `0.5`
- `(pow 0 0)` → returns `1`

Three different error policies (raise / NaN sentinel / silent promotion) for similar domain failures.

**Proposed fix:** pick a uniform policy and document it. Two reasonable choices:
- "Raise on all undefined domains" (Scheme-ish, predictable, breaks anyone relying on NaN)
- "Always return IEEE specials; raise only on integer-divide-by-zero" (Python-ish, less predictable, doesn't break)

**Why this needs a design call:** language-level decision. Document in `docs/adr.md` and `website/docs/stdlib/math.md`. Implementation is small once decided.

---

## Notes on what's NOT here

- **Items that shipped:** see commits `09f6c21` (Wave 1) through `83cbd58` (Wave 6a) — git log + the per-wave commit messages have the full list.
- **Items that are formally parked:** see `docs/deferred.md`. Each entry there explains *why* we won't fix and what the workaround is.
- **C1 (VM upvalue model)** and **C11 (.semac stack-balance verifier):** see `docs/limitations.md` items #31/#32 and `docs/adr.md` ADRs #55/#56. Multi-week design work; tracked there, not here.
- **#57 (source spans through runtime errors)** and **#58 (thread-local writer hook):** also in `docs/adr.md`. Pulled out of Wave 2 as too big for that pass.

---

## Recently shipped — removed from WIP

- **P2 — special-form Spur cache in the lowerer** — shipped commit `421ad2b`. A per-thread `HashMap<Spur, SpecialForm>` replaces the ~40-name interner-borrow chain in `lower_list`; ~1.20x faster lower-time on a 10k-form file (88.6ms → 73.6ms), emitted bytecode byte-identical. Thread-local (not a global `OnceLock`) because the interner is `thread_local!`.
- **L2 — LSP range formatting** — shipped commit `0556d5b`. `textDocument/rangeFormatting`: expands the selection to the smallest set of whole top-level forms and formats those via `sema-fmt` (Lisp-safe — never formats partial sub-expressions). 6 new tests.
- **L3 — DAP conditional + uncaught-exception breakpoints** — shipped commit `4d4759c`. `condition` on `SourceBreakpoint` gates pure breakpoint stops; an `uncaught` exception filter + `exceptionInfo` request. 2 new tests.
- **P6, P7 — moved to `docs/deferred.md`.** P6 (partition/group-by double-clone) was always "won't move the needle." P7 (`CALL_NATIVE` Rc-clone) was spiked in a worktree and measured: no improvement beyond noise on any HOF-heavy workload, so the `unsafe` was not worth it — discarded.
- **P1 — `get_sequence` borrows instead of clones** — shipped in commit `49c4c5c` ("perf(stdlib): borrow input slice in HOF helper (Wave 6b/P1)"). `get_sequence` now returns `&[Value]`; verified in `crates/sema-stdlib/src/list.rs` (call sites iterate by reference, `sort` etc. use `.to_vec()`).
- **A1 + A4 + D2 — Async semantics pass** — shipped in commit `72c8308` ("feat(async): A1+A4+D2 — FIFO scheduler, async/cancel returns bool, Cancelled state variant"). See `CHANGELOG.md` 1.15.0 under "Changed (async semantics pass — A1 + A4 + D2)".
