# sema-dap & sema-fmt — Audit Findings (2026-05-29)

**Status:** Open (unfixed) · **Scope:** `crates/sema-dap/src`, `crates/sema-fmt/src` · Part of the
[2026-05-29 codebase audit](./2026-05-29-codebase-audit-index.md).
Line numbers as of commit `f609807` — re-confirm before fixing.

## P1 — High

### [DAP-1] `GetStackTrace` / `GetScopes` / `GetVariables` deadlock while the VM is running
- **Location:** `crates/sema-dap/src/server.rs:254` (and `:302`, `:341`); VM poll loop at
  `crates/sema-vm/src/vm.rs:654`
- **Confidence:** 90%
- **Why:** While the VM executes between breakpoints, the debug-poll loop (`run_inner`, every 128
  instructions) only handles `Pause`/`Disconnect`/`SetBreakpoints`; `GetStackTrace`/`GetScopes`/
  `GetVariables` fall through `_ => {}` and are dropped with no reply. The DAP server's
  `spawn_blocking(|| reply_rx.recv().unwrap_or_default())` threads then block forever, leaking
  tokio blocking-pool slots. A client that sends `stackTrace` while the program is still running
  (valid behavior) hangs the session.
- **Fix:** In the running-VM poll loop, reply to these commands (current state or an explicit
  error) instead of dropping them.

## P2 — Moderate

### [FMT-1] Trailing-whitespace cleanup corrupts CRLF inside preserved string literals
- **Location:** `crates/sema-fmt/src/formatter.rs:1694`
- **Confidence:** 85%
- **Why:** The final cleanup does `result.lines().join("\n")`. `str::lines()` splits on `\n`,
  `\r\n`, and bare `\r`; re-joining with `\n` drops the `\r`. A `\r\n` embedded inside a string /
  f-string / regex literal (preserved verbatim as `StringAtom`) gets its `\r` removed —
  **changing the program's string contents** (e.g. `"foo\r\nbar"` → `"foo\nbar"`).
- **Fix:** Replace the `lines()`-based cleanup with a sequential scan that strips trailing
  spaces/tabs only before real `\n`, without treating `\r` as a line separator.

## Notes (not filed as findings)
- `format_fstring` (formatter.rs:465) is dead code (f-strings go through the source-preserving
  `StringAtom` path) but would lose `$`-escapes if ever used.
- `sema-dap/src/server.rs:270` `frame.as_object_mut().unwrap()` is safe — `json!({...})` always
  yields an object.
- DAP `setBreakpoints` zip (server.rs:214-224) is length-safe.
