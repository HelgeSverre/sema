# Ergonomic Improvements — Tracking

**Issue:** https://github.com/HelgeSverre/sema/issues/6
**Date:** 2026-02-22

---

## Priority Order (effort vs gain)

### Phase 1 — Quick Wins

- [x] **1. String interpolation `f"..."`** ✅
  - **Effort:** Low | **Gain:** Very High
  - Reader macro: `f"Hello ${name}"` → `(str "Hello " name)`
  - Lexer tokenizes `f"..."` into `FString` token with `Literal`/`Expr` parts
  - Reader expands `FString` to `(str ...)` call via recursive `read()`
  - Supports: nested expressions `${(+ 1 2)}`, escape `\$`, `$` without `{` is literal
  - Files changed: `crates/sema-reader/src/lexer.rs`, `crates/sema-reader/src/reader.rs`
  - Also refactored escape handling into shared `read_string_escape()` function
  - 16 reader unit tests + 8 integration tests added

- [x] **2. Auto-load threading macros** ✅
  - **Effort:** Very Low | **Gain:** High
  - Created `crates/sema-eval/src/prelude.rs` with `->`, `->>`, `as->`, `some->`
  - Auto-loaded at interpreter startup via `load_prelude()` in both `new()` and `new_with_sandbox()`
  - 7 integration tests added

- [x] **3. Promote `when-let` / `if-let` to builtins** ✅
  - **Effort:** Very Low | **Gain:** Medium-High
  - Added `when-let` and `if-let` macros to the prelude (auto-loaded)
  - 4 integration tests added

### Phase 2 — Low Effort, High Value

- [x] **4. Add `get-in` / `update-in` / `assoc-in` / `deep-merge`** ✅
  - **Effort:** Low | **Gain:** High
  - `get-in` — nested access with optional default, nil-safe
  - `assoc-in` — nested set, creates intermediate maps if missing
  - `update-in` — nested update with function
  - `deep-merge` — recursive merge of nested maps (variadic)
  - Files changed: `crates/sema-stdlib/src/map.rs`
  - 12 integration tests added

- [x] **5. Short lambda `#(* % %)`** ✅
  - **Effort:** Low-Medium | **Gain:** High
  - Lexer emits `ShortLambdaStart` token on `#(`
  - Reader parses body, scans for `%`/`%1`/`%2`, rewrites `%` → `%1`
  - Expands to `(lambda (%1 %2 ...) body)` at parse time
  - Skips recursion into nested `(lambda ...)` / `(fn ...)` forms
  - Files changed: `crates/sema-reader/src/lexer.rs`, `crates/sema-reader/src/reader.rs`
  - 7 reader unit tests + 6 integration tests added

### Phase 3 — Medium Effort, Very High Value

- [ ] **6. Destructuring in `let` / `define` / `lambda`**
  - **Effort:** Medium-High | **Gain:** Very High
  - Start with map destructuring `{:keys [a b]}` in `let`
  - Then list destructuring `[first second & rest]`
  - Then function parameter destructuring
  - Files: `crates/sema-eval/src/special_forms.rs`, `crates/sema-eval/src/eval.rs`
  - Foundation for pattern matching (item 7)

- [ ] **7. Pattern matching `match`**
  - **Effort:** High | **Gain:** Very High
  - Implement after destructuring (shares infrastructure)
  - Special form: `(match expr [pattern body] ...)`
  - Support: literals, wildcards `_`, map patterns, list patterns, guards `when`
  - Files: `crates/sema-eval/src/special_forms.rs`, new `match.rs` module

### Backlog — Lower Priority

- [ ] **8. Regex literals `#"..."`**
  - Effort: Low | Gain: Low-Medium
  - Regex functions already exist, just need reader syntax

- [ ] **9. REPL `*1` / `*2` / `*e` history**
  - Effort: Very Low | Gain: Low
  - Store last results in REPL loop variables

- [ ] **10. `spy` / `time` / `assert` debug helpers**
  - Effort: Very Low | Gain: Low-Medium
  - `tap` already exists; add `spy` (labeled tap) and ensure `time`/`assert` exist

- [ ] **11. Multimethods `defmulti` / `defmethod`**
  - Effort: Medium | Gain: Medium
  - User-space pattern exists in `examples/multimethods.sema`
  - Promote to builtins if demand warrants

- [ ] **12. Keyword arguments `&keys`**
  - Effort: Medium | Gain: Low
  - Options-map pattern is idiomatic enough

---

## Completed

_(Items move here as they're implemented)_

---

## Notes

- All Phase 1 items can be done without touching the evaluator core
- Phase 2 items need minor evaluator/reader changes
- Phase 3 items are the biggest effort but highest long-term payoff
- Destructuring (item 6) is prerequisite for pattern matching (item 7)
- Items 8-12 are nice-to-have and can be done opportunistically
