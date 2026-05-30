# sema-stdlib — Audit Findings (2026-05-29)

**Status:** Open (unfixed) · **Scope:** `crates/sema-stdlib/src` · Part of the
[2026-05-29 codebase audit](./2026-05-29-codebase-audit-index.md).
Line numbers as of commit `f609807` — re-confirm before fixing.

Most P1 entries here are instances of **Pattern A** (`i64 as usize` without a negativity guard)
and **Pattern B** (UTF-8 byte slicing) from the index. `list/take`, `list/drop`, and
`string/repeat` already guard correctly and can serve as the reference shape.

## P1 — High

### [STD-1] Bit shift by negative or ≥64 panics
- **Location:** `crates/sema-stdlib/src/bitwise.rs:55` (`bit/shift-left`), `:65` (`bit/shift-right`)
- **Confidence:** 97% (re-verified against source)
- **Why:** `a << n` / `a >> n` with a user `i64 n` and no range check. `(bit/shift-left 1 64)` or
  `(bit/shift-left 1 -1)` → overflow panic (debug) / masked-nonsense (release).
- **Fix:** Validate `0 <= n < 64` and return `SemaError` otherwise.

### [STD-2] `math/random-int` panics when `lo > hi`
- **Location:** `crates/sema-stdlib/src/math.rs:314`
- **Confidence:** 95%
- **Why:** `rand::rng().random_range(lo..=hi)` panics on an empty range.
  `(math/random-int 10 5)` crashes the interpreter — a common reversed-bounds mistake.
- **Fix:** Guard `lo > hi` before `random_range`.

### [STD-3] UTF-8 byte-boundary panic in `apply_overlap`
- **Location:** `crates/sema-stdlib/src/text.rs:362-365`
- **Confidence:** 92%
- **Why:** `&prev[prev.len() - overlap..]` slices a `String` by bytes; `overlap` is a char-count
  parameter, so the offset frequently lands mid-character → panic. Reached from `text/chunk` and
  `document/chunk` on any multibyte input.
- **Fix:** Find the byte offset of the char `overlap` positions from the end (e.g.
  `prev.char_indices().rev().nth(overlap-1)`).

### [STD-4] Negative `width` → huge `usize` → OOM in `string/pad-left` / `string/pad-right`
- **Location:** `crates/sema-stdlib/src/string.rs:418-421` (and `:446-447`)
- **Confidence:** 92%
- **Why:** `as_int()? as usize` with no negativity check; `-1` → `usize::MAX`, then
  `repeat_n(...)` attempts an ~18 EB allocation → OOM abort. (Pattern A.)
- **Fix:** Reject negative width before the cast, as `string/repeat` does.

### [STD-5] Negative/zero int args wrap to `usize::MAX` across list functions
- **Location:** `crates/sema-stdlib/src/list.rs` — `list/chunk` `:566-567`,
  `list/split-at` `:649-651`, `list/sliding` `:933-941`, `list/page` `:1021-1024`,
  `list/repeat`/`make-list` `:5-10`, `list/times` `:975-978`
- **Confidence:** 82-90%
- **Why:** All use `as_int()? as usize` with no negativity (and in `page`, no zero) check. Negative
  values wrap to `usize::MAX` → OOM allocation (`repeat`/`times`/`chunk`) or silent wrong result
  (`split-at` clamps to end; `sliding` produces empty). (Pattern A.)
- **Fix:** Add negativity/zero guards before each cast.

### [STD-6] Unbounded HTTP request body → DoS
- **Location:** `crates/sema-stdlib/src/server.rs:825`
- **Confidence:** 97% (re-verified against source — comment literally says "no size limit")
- **Why:** `axum::body::to_bytes(req.into_body(), usize::MAX)` buffers any body size into memory;
  a client can stream a multi-GB body and exhaust process memory.
- **Fix:** Cap (e.g. 16 MiB) and return 413 on exceed.

### [STD-7] `ws/close` drops a sender *clone* — WebSocket never closes
- **Location:** `crates/sema-stdlib/src/server.rs:1130`
- **Confidence:** 98%
- **Why:** `drop(out_tx_for_close.clone())` drops a temporary, not the captured sender (the
  `NativeFn` `Fn` bound forbids consuming it). Both senders stay live, so axum's `send_task` never
  exits and `(ws/close)` has no effect.
- **Fix:** Hold the sender in `Rc<RefCell<Option<Sender>>>` and take+drop it on first close,
  mirroring the existing `in_rx` pattern.

## P2 — Moderate

### [STD-8] Float→int `as` casts saturate / map NaN→0 silently
- **Location:** `crates/sema-stdlib/src/math.rs:27` (and `:73`, `:85`, `:364`)
- **Confidence:** 85%
- **Why:** `f.ceil() as i64` etc. saturate to `i64::MAX/MIN` on out-of-range floats and return `0`
  for NaN, with no error — e.g. `(ceil 1e300)` silently yields `i64::MAX`.
- **Fix:** Check `f.is_finite()` and range before the cast; return `SemaError` otherwise.

### [STD-9] `:port` silently truncated via `as u16`
- **Location:** `crates/sema-stdlib/src/server.rs:1194`
- **Confidence:** 90%
- **Why:** `p as u16` wraps: `{:port 70000}` binds 4464, `{:port -1}` binds 65535, and the
  `eprintln!` reports the original value, masking the mismatch. (Pattern A.)
- **Fix:** Validate `1..=65535`, error otherwise.

### [STD-10] `db/exec-batch` runs raw SQL with no parameterization
- **Location:** `crates/sema-stdlib/src/sqlite.rs:134-158`
- **Confidence:** 92% (by-design API, but an injection footgun)
- **Why:** `conn.execute_batch(sql)` takes the SQL string with no params slot; user data
  concatenated into it enables full SQL injection. (`db/exec`/`db/query` correctly parameterize.)
- **Fix:** Document static-only usage; steer user-data writes to `db/exec` with positional params.

### [STD-11] Static-file path not confirmed inside dir after `join` → symlink escape
- **Location:** `crates/sema-stdlib/src/server.rs:592-620`
- **Confidence:** 85%
- **Why:** Only a literal `".."` substring is rejected (line 579); the joined path is never
  canonicalized or prefix-checked, so a symlink inside the static dir pointing outside is followed.
- **Fix:** `canonicalize()` the joined path and verify it `starts_with` the (canonicalized) root;
  403 otherwise.

## P3 — Low

### [STD-12] `sys/set-env` calls `set_var` unsafely while a server runs
- **Location:** `crates/sema-stdlib/src/system.rs:155-158`
- **Confidence:** 83%
- **Why:** `unsafe { std::env::set_var(...) }` is a POSIX data race if tokio workers/C libs call
  `getenv` concurrently (e.g. while `http/serve` is up) — UB on glibc.
- **Fix:** Document the constraint, or guard with a process-wide lock.

## Notes (not filed as findings)
- `db/exec`/`db/query` are safe (parameterized via `params_from_iter`).
- The single-string `shell` path (system.rs ~67) routes through `sh -c` but is gated behind
  `Caps::SHELL | Caps::PROCESS` — intended design.
- `http.rs` has no SSRF restriction but is gated behind `Caps::NETWORK` (deployment concern).
- `parse_query_string` (server.rs ~683) does not percent-decode — usability, not security.
