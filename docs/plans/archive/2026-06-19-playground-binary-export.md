# Playground → downloadable native binary (curiosity / future plan)

**Status:** idea, not scheduled. Captured 2026-06-19.

## The question

The playground runs the WASM build, not the full toolchain. Could it still let
you write code in the editor and download a *runnable native binary* for some
target (macOS / Linux / Windows) — and how hard would that be?

## The key realisation

`sema build` is **not compilation**. It's concatenation. A "compiled" sema
binary is three byte-ranges glued together:

```
[ stock sema runtime for target ]  +  [ VFS archive of your code ]  +  [ trailer ]
```

- **Runtime** — the normal `sema` interpreter binary. For cross-targets,
  `sema build --target` *downloads a pre-built one from GitHub Releases*
  (`HelgeSverre/sema`, via `crates/sema/src/cross_compile.rs`) and caches it
  under `~/.sema/cache/runtimes/v{version}/{target}/`. **No rustc / LLVM /
  linker is ever invoked.** Supported targets are the cargo-dist set:
  `aarch64-apple-darwin`, `aarch64-unknown-linux-gnu`, `x86_64-apple-darwin`,
  `x86_64-unknown-linux-gnu`, `x86_64-pc-windows-msvc`.
- **Archive** (`crates/sema/src/archive.rs`) — a dead-simple length-prefixed
  format: metadata KV pairs + TOC + raw file bytes + CRC32. Holds `.sema`
  source or `.semac` bytecode.
- **Trailer** — `archive_size: u64 LE` + magic `SEMAEXEC` (16 bytes). On
  startup the runtime reads its own tail; if it sees `SEMAEXEC` it runs the
  embedded program instead of behaving as a CLI.

So "build for Linux from a Mac" today = download the Linux runtime, append your
archive, write the trailer. The browser can do the **exact same recipe**,
producing byte-identical output, with no compiler in sight.

## Browser recipe

1. User picks a target (dropdown).
2. Fetch the stock runtime binary for that target.
3. Build the archive from the editor contents (+ any uploaded VFS files).
4. `new Blob([runtime, archive, trailer])` → trigger download.

The thing that *sounds* hard — cross-compilation / codegen in the browser —
doesn't exist in sema's model, so it isn't needed.

## Wrinkles (all modest)

1. **Generating the archive.** `archive.rs` lives in the `sema` binary crate,
   not `sema-wasm`. Either:
   - reimplement the format in JS (trivial — LE length-prefixed fields + a
     ~10-line CRC32), or
   - factor archive-writing into a lib and expose a `sema-wasm` binding that
     returns `Uint8Array` from a `{path: bytes}` map (cleaner — no format
     drift; ~1h). **Preferred.**
2. **Binary size / network.** Each runtime is tens of MB; one fetch per chosen
   target, cacheable. Heavier than the current WASM download, not a blocker.
3. **Where to fetch runtimes from.** Cleanest is to **mirror them same-origin
   on sema.run** rather than hit GitHub release assets directly — dodges any
   CORS uncertainty on the asset redirect, and they sit behind the COOP/COEP
   isolation already configured for the worker. Cost: host a handful of
   binaries per release (CI already produces them via cargo-dist).
4. **OS gatekeeping.** Downloaded Unix binaries arrive without `+x`; macOS
   quarantines unsigned binaries (`xattr -d` / right-click-open). Identical
   friction to the existing GitHub release downloads — worth a one-line UI
   note, not a technical obstacle.

## Verdict

Feasibility **high**, effort **low** (~half a day, mostly UI + hosting the
runtime mirror) — *because* sema's build model is "stock interpreter + appended
payload," not native codegen.

## Smallest proof-of-concept

1. Expose archive-building from `sema-wasm` (or reimplement in JS).
2. Hardcode one target (say `x86_64-unknown-linux-gnu`).
3. Fetch that runtime (from a same-origin mirror), concat archive + trailer,
   download.
4. Verify the resulting file actually runs natively (`./sema-out` on Linux).

## Pointers

- `crates/sema/src/archive.rs` — archive format (the layout doc-comment is the
  spec) + write/read.
- `crates/sema/src/cross_compile.rs` — target resolution, runtime download +
  cache, `SUPPORTED_TARGETS`.
- `crates/sema/src/main.rs` (`Commands::Build`, ~line 547) + `pkg.rs` — the
  `sema build` command wiring and bundling (host + `--target` paths; the actual
  `Archive::`/`cross_compile::` calls live here).
- Trailer magic: `SEMAEXEC`, `TRAILER_SIZE = 16`, `FORMAT_VERSION = 1`.
