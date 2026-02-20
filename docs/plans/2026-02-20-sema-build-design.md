# `sema build` — Standalone Executable Builder

**Date:** 2026-02-20
**Status:** Approved
**Issue:** https://github.com/HelgeSverre/sema/issues/9

## Overview

`sema build` compiles a sema program into a standalone executable by appending a VFS archive (compiled bytecode + imports + assets) to the sema runtime binary. The resulting binary is self-contained — no sema installation needed to run it.

```bash
sema build app.sema -o myapp          # basic
sema build app.sema --include data/   # with assets
./myapp --name hello                  # runs standalone
```

## Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Runtime binary | Use current `sema` binary (full, not stripped) | Tree-walker required at runtime for HOFs, imports, eval. See `docs/plans/vm-runtime-limitations.md` |
| Config directory | `~/.sema/` with `SEMA_HOME` env override | Simple, discoverable, matches cargo/deno/bun precedent |
| Import handling | Auto-trace and bundle all transitive imports | User expects standalone output |
| Asset handling | Explicit `--include` flag | Static analysis of `file/read` is unreliable; `sema.toml` manifest in future |
| CLI args | Pass-through `argv` as-is | Standard behavior, script uses `(sys/args)` |
| VFS implementation | Thread-local `HashMap` in sema-stdlib | Matches existing LLM provider pattern, avoids touching sema-core |
| Cross-compilation | Deferred to v2 | Should work offline; future: pre-download runtimes to `~/.sema/cache/runtimes/` |

## Binary Format

### Trailer (last 16 bytes — frozen, never changes)

```
archive_size: u64 LE     ← size of the VFS archive in bytes
magic: "SEMAEXEC"        ← 8 bytes, detection marker
```

The trailer is the only thing the loader reads first. Its format is permanent — old loaders can always detect new binaries (and reject if archive format version is too new).

### VFS Archive

```
┌─ Archive Header ──────────────────────┐
│  format_version: u16                  │  bump when archive layout changes (v1 initially)
│  flags: u16                           │  reserved bitfield
│  metadata_count: u32                  │
│  ┌─ Metadata entries ───────────────┐ │
│  │ key_len(u16) + key(utf8)         │ │
│  │ val_len(u32) + val(bytes)        │ │
│  │ ...repeats metadata_count times  │ │
│  └──────────────────────────────────┘ │
├─ TOC ─────────────────────────────────┤
│  entry_count: u32                     │
│  ┌─ TOC entries ────────────────────┐ │
│  │ path_len(u32) + path(utf8)       │ │
│  │ offset(u64) + size(u64)          │ │
│  │ ...repeats entry_count times     │ │
│  └──────────────────────────────────┘ │
├─ File data ───────────────────────────┤
│  raw bytes for all bundled files      │
│  (offsets relative to archive start)  │
└───────────────────────────────────────┘
```

### v1 Metadata Keys

| Key | Value | Description |
|-----|-------|-------------|
| `sema-version` | `"1.10.0"` | Sema version that built the executable |
| `build-timestamp` | ISO 8601 string | When it was built |
| `entry-point` | `"__main__.semac"` | VFS path of the compiled entry bytecode |
| `build-root` | absolute path string | Original project root (for path normalization) |

### VFS Entry Conventions

| VFS Path | Contents |
|----------|----------|
| `__main__.semac` | Compiled bytecode of the entry file |
| `lib/utils.sema` | Auto-traced import (relative to project root) |
| `data.json` | Asset from `--include` |
| `prompts/system.txt` | Asset from `--include prompts/` |

## CLI Interface

```
sema build <file> [options]

Arguments:
  <file>               Source file to compile and bundle

Options:
  -o, --output <path>  Output executable path (default: filename without extension)
  --include <path>...  Additional files/directories to bundle (repeatable)
  --runtime <path>     Sema binary to use as runtime base (default: current exe)
```

## Build Flow

1. Read and parse the entry file
2. Trace all `(import ...)` dependencies recursively
   - Track visited files to handle circular imports
   - Warn on dynamic imports that can't be resolved statically
   - Hard error on missing files
3. Compile the entry file to bytecode via `interpreter.compile_to_bytecode()`
4. Resolve all `--include` paths (expand directories recursively)
5. Build the VFS archive:
   - `__main__.semac` → compiled bytecode
   - Traced imports → source files (parsed by tree-walker at runtime)
   - `--include` files → raw bytes
6. Populate metadata
7. Copy the runtime binary (`std::env::current_exe()` or `--runtime`) to output path
8. Append: archive bytes + `archive_size` as u64 LE + `SEMAEXEC` magic
9. `chmod +x` on unix

## Runtime Startup Flow

In `main()`, before clap parsing:

1. Get `std::env::current_exe()`
2. Open the file, seek to last 16 bytes
3. Check for `SEMAEXEC` magic
4. If found: read `archive_size`, seek back, deserialize archive
5. Populate thread-local VFS with all entries
6. Find `entry-point` in metadata, read the bytecode from VFS
7. Initialize `Interpreter` (registers stdlib, eval callbacks, VM delegates)
8. Execute bytecode via `VM::execute()`
9. Exit with appropriate code

If no magic found, proceed with normal CLI parsing.

## VFS Interception

Thread-local VFS in `sema-stdlib/src/vfs.rs`:

```rust
thread_local! {
    static EMBEDDED_VFS: RefCell<Option<HashMap<String, Vec<u8>>>> = RefCell::new(None);
}
```

**Intercepted functions** (read-only, check VFS first then fall back to filesystem):
- `file/read`
- `file/exists?`
- `import` (via `__vm-import` and tree-walker `eval_import`)

**Not intercepted** (always real filesystem):
- `file/write`, `file/append`, `file/delete`, `file/rename`, `file/mkdir`

## `~/.sema/` Home Directory

New utility in `sema-core/src/home.rs`:

```rust
pub fn sema_home() -> PathBuf
```

Resolution order: `$SEMA_HOME` > `$HOME/.sema` > `%USERPROFILE%\.sema` > `.sema` (fallback).

Subdirectory convention:
- `~/.sema/cache/` — temp files, future runtime downloads
- `~/.sema/history` — potential future REPL history location

Also exposed as `sys/sema-home` builtin.

## Future Work (Not in v1)

- **Cross-compilation** — download pre-built runtimes for other platforms to `~/.sema/cache/runtimes/`
- **`sema.toml` manifest** — declare includes, metadata, build options in a config file
- **Runtime-only binary** — requires decoupling VM from tree-walker (see `docs/plans/vm-runtime-limitations.md`)
- **Code signing** — sign the archive for distribution trust
- **Compression** — optionally compress VFS entries (zstd or deflate)
