# sema-notebook — Audit Findings (2026-05-29)

**Status:** Open (unfixed) · **Scope:** `crates/sema-notebook/src` · Part of the
[2026-05-29 codebase audit](./2026-05-29-codebase-audit-index.md).
Line numbers as of commit `f609807` — re-confirm before fixing.

## P2 — Moderate

### [NB-1] `reorder_cells` with duplicate IDs permanently corrupts the notebook
- **Location:** `crates/sema-notebook/src/bridge.rs:234-251`
- **Confidence:** 92%
- **Why:** The `ReorderCells` handler clones each matched cell into `reordered` with no duplicate
  check. A client sending `{"cell_ids": ["c123", "c123"]}` produces a duplicated cell; after
  `engine.notebook.cells = reordered`, `eval_all` evaluates it twice (double output) and
  `cell_index` returns the first occurrence — desync that persists across saves.
- **Fix:** Reject `cell_ids` containing duplicates (e.g. compare against a `HashSet`) before
  rebuilding.

### [NB-2] No authentication on mutating endpoints (incl. `/vfs/write`)
- **Location:** `crates/sema-notebook/src/server.rs:91-99` + `vfs.rs`
- **Confidence:** 85%
- **Why:** `POST /vfs/write` (and `/api/cells`, `/api/eval-all`, `/api/reset`) accept requests
  with no token/CSRF protection. Bound to `127.0.0.1` by default, but reachable from a browser the
  user visits via DNS-rebinding; combined with `vfs_root` defaulting to `current_dir()`, this
  allows writing/overwriting files in the working directory.
- **Fix:** Generate a startup secret token (printed to stderr) and require it in an
  `X-Sema-Token` header on all mutating endpoints.

## P3 — Low

### [NB-3] Engine thread panics on tokio-runtime build failure → silent dead server
- **Location:** `crates/sema-notebook/src/bridge.rs:110-112`
- **Confidence:** 80%
- **Why:** `Builder::new_current_thread().build().unwrap()` panics if the runtime can't be built
  (e.g. fd exhaustion). The engine thread dies, the mpsc channel closes, every endpoint returns
  500 with no log — the HTTP server appears alive but is non-functional.
- **Fix:** `.expect(...)`/propagate via the reply channel and log explicitly; consider restart.

## Notes (not filed as findings)
- Bind address defaults to `127.0.0.1` (main.rs:419); RCE-via-eval requires explicit
  `--host 0.0.0.0`. No hidden network exposure.
- `vfs.rs` `resolve_path` traversal guard (canonicalize parent + `starts_with`) is sound for both
  existing and non-existing paths.
- `gag::BufferRedirect` process-wide stdout capture (engine.rs:187) is safe here because all eval
  requests serialize through the single engine thread.
