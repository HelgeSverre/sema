# sema-reader — Audit Findings (2026-05-29)

**Status:** Open (unfixed) · **Scope:** `crates/sema-reader/src` · Part of the
[2026-05-29 codebase audit](./2026-05-29-codebase-audit-index.md).
Line numbers as of commit `f609807` — re-confirm before fixing.

## P1 — High

### [READ-1] Unbounded parser recursion → stack overflow on deeply nested input
- **Location:** `crates/sema-reader/src/reader.rs` — `parse_expr`/`parse_list`/`parse_vector`/
  `parse_map`/`parse_short_lambda` (`:84,167,210,246,323`); compounded by
  `rewrite_percent_args` (`:538`) and the recursive `read` for f-string interpolation (`:464`)
- **Confidence:** 95%
- **Why:** Recursion depth equals input nesting depth with no limit. ~10k-50k levels of nesting
  (e.g. many open parens, or nested `#( )`) overflows the thread stack → SIGSEGV. The input is
  untrusted and every public entry (`read`, `read_many`, `read_many_with_spans*`) is affected,
  including the WASM playground (browser-exploitable). `rewrite_percent_args` adds a second
  full-depth pass over the short-lambda AST, and f-string `${...}` triggers a nested `read`,
  both compounding the effective depth.
- **Fix:** Track a `depth` counter in `Parser`; return a `SemaError::Reader` past a limit
  (e.g. 1000). Ensure the depth budget also covers `rewrite_percent_args` and nested f-string
  `read` calls (or make `rewrite_percent_args` iterative).

## P3 — Low

### [READ-2] f-string `${x y}` silently drops extra forms
- **Location:** `crates/sema-reader/src/reader.rs:464`
- **Confidence:** 82%
- **Why:** `FStringPart::Expr(src)` calls `read(src)` which reads exactly one form. `f"${x y}"`
  parses to just `x`, silently discarding `y` (the empty-interpolation guard doesn't catch it).
- **Fix:** Use `read_many` and error on >1 form, or wrap multiple forms in an implicit `do`.

## Notes (not filed as findings)
- No `unwrap()` reachable from untrusted input was found outside `#[test]`; the `:298`
  `chars().next().unwrap()` is guarded by a `count() == 1` check.
- `byte_offsets` indexing is sound throughout (sentinel entry at `chars.len()` covers
  post-last-char access; in-loop accesses guarded by `i < chars.len()`).
- `\u`/`\U`/`\x` escape column tracking verified correct.
