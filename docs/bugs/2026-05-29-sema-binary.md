# sema (binary) — Audit Findings (2026-05-29)

**Status:** Open (unfixed) · **Scope:** `crates/sema/src` (pkg/archive/cross_compile/main) · Part
of the [2026-05-29 codebase audit](./2026-05-29-codebase-audit-index.md).
Line numbers as of commit `f609807` — re-confirm before fixing.

## P1 — High

### [BIN-1] Registry package name used as a path component without validation → traversal
- **Location:** `crates/sema/src/pkg.rs:996` (also fed from `:389` and `:461`)
- **Confidence:** 88%
- **Why:** `registry_install(name, ...)` does `pkg_dir.join(name)` where `name` is the raw spec
  string. Git specs go through `validate_package_spec` (which rejects `..`), but registry-only
  names (no dot in the first segment) skip that guard. `sema pkg add ../../../etc/cron.d` (or a
  matching `sema.toml` `[deps]` entry) resolves `pkg_dir.join("../../../etc/cron.d")` **outside**
  `~/.sema/packages/`.
- **Fix:** Call `validate_package_spec(name)` (already in `sema-core/src/resolve.rs`), or a
  dedicated registry-name validator, at the top of `registry_install`.

## P2 — Moderate

### [BIN-2] `git checkout <ref>` without `--` → ref/flag confusion
- **Location:** `crates/sema/src/pkg.rs:117,126,155,487`
- **Confidence:** 85%
- **Why:** Refs are passed as positional args; `PackageSpec::parse` allows refs starting with `-`.
  A `sema.toml`/`sema.lock` `ref = "-f"` causes `git checkout -f` to run in the package dir
  (discarding local changes / other flag side effects).
- **Fix:** Reject refs starting with `-` in `PackageSpec::parse` and before passing to `run_git`.

## P3 — Low

### [BIN-3] `registry_download` has no response-size limit → OOM
- **Location:** `crates/sema/src/pkg.rs:1023`
- **Confidence:** 82%
- **Why:** `resp.bytes()` buffers the whole tarball with no cap; a malicious/compromised registry
  can serve a multi-GB response → OOM/disk exhaustion. (`cross_compile.rs` caps at
  `MAX_RUNTIME_SIZE` = 200 MB.)
- **Fix:** Stream to a temp file with a size limit (`response.take(MAX_PKG_SIZE)`).

### [BIN-4] Partial extraction leaves corrupt install state
- **Location:** `crates/sema/src/pkg.rs:1000` (and `registry_install_locked` at `:1069`)
- **Confidence:** 80%
- **Why:** The old package dir is removed before `extract_tarball`; a mid-extract failure leaves
  the destination neither old nor new, and later `install` calls may treat it as installed.
- **Fix:** Extract to a temp dir, then atomic-rename into place; clean the temp dir on failure.

## Notes (not filed as findings — verified clean)
- `archive.rs` (SEMAEXEC format): CRC32 validation, capacity clamping, bounds checks; paths used
  only as HashMap keys, never written to disk — no traversal risk.
- `cross_compile.rs` extraction: filename-only match, rejects symlinks/hardlinks, enforces
  `MAX_RUNTIME_SIZE`, atomic rename — clean.
- `import_tracer.rs`: `canonicalize` + `strip_prefix` confines imports to project/packages dir.
- `main.rs` CLI arg handling: no reachable panics from user input; `.expect` calls are for tokio
  setup, not user-controlled.
