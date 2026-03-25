# Sema on Raspberry Pi Pico — Embedded Port Investigation

**Date:** 2026-03-25
**Status:** Future / Investigation
**Depends on:** Bytecode VM (completed), `.semac` format (completed), `sema build` (completed)

## Overview

Port a minimal Sema bytecode runtime to the Raspberry Pi Pico (RP2040/RP2350), enabling Sema programs compiled on a host machine to execute on microcontroller hardware. The architecture is **host-compile, device-run**: the full Sema toolchain produces `.semac` bytecode on a desktop, and a stripped-down VM runtime on the Pico deserializes and executes it.

This is a separate `sema-pico` binary crate, not a modification of the main `sema` binary.

## Target Hardware

| | RP2040 (Pico 1) | RP2350 (Pico 2) |
|---|---|---|
| CPU | Dual Cortex-M0+ @ 133MHz | Dual Cortex-M33 @ 150MHz |
| SRAM | 264 KB | 520 KB |
| Flash | 2 MB | 16 MB |
| Rust target | `thumbv6m-none-eabi` | `thumbv8m.main-none-eabihf` |
| FPU | None | Single-precision |
| 64-bit ops | Software-emulated (slow) | Partial HW support |

**Primary target: RP2350 (Pico 2)** — double the RAM makes NaN-boxing viable without rewriting Value, and `thumbv8m` has better 64-bit arithmetic. RP2040 is a stretch goal.

## Prior Art

- **uLisp** — runs on RP2040, ~22K Lisp cells (8 bytes each) in 264KB SRAM, mark-and-sweep GC. Proves a useful Lisp fits.
- **Grift** — Rust `no_std`/`no_alloc` Lisp. Fixed-capacity arena, free-list allocation, mark-and-sweep GC. Closest Rust analog.
- **Armpit Scheme** — R5RS on Cortex-M4F in ARM assembly. Nursery-based generational GC.
- **pico-jit** — WebAssembly JIT on RP2040. 1.27x–4.38x faster than Wasm3 interpreter. Proves bytecode VMs are viable on this hardware.
- **Rhai** — Rust scripting engine with `no_std` support (requires nightly + global allocator).

## Architecture

```
┌─────────────────────────────┐     ┌──────────────────────────────┐
│        Host Machine         │     │      Raspberry Pi Pico       │
│                             │     │                              │
│  .sema ──→ sema compile    │     │  .semac ──→ deserialize      │
│          ──→ .semac file    │ ──→ │          ──→ VM::execute()   │
│                             │     │          ──→ result / I/O    │
│  (full toolchain:           │     │                              │
│   reader, lowerer, resolver,│     │  (minimal runtime:           │
│   optimizer, compiler,      │     │   sema-core, vm.rs, chunk.rs,│
│   serializer)               │     │   opcodes.rs, deserializer,  │
│                             │     │   minimal stdlib)            │
└─────────────────────────────┘     └──────────────────────────────┘
```

Bytecode transfer: embed in flash via `include_bytes!()`, load over UART/USB at runtime, or read from SD card (SPI).

## Key Blockers and Solutions

### 1. NaN-Boxing Requires 64-bit (HIGH)

Current guard in `sema-core/src/value.rs`:
```rust
#[cfg(not(any(target_pointer_width = "64", target_arch = "wasm32")))]
compile_error!("sema-core NaN-boxed Value requires a 64-bit platform (or wasm32)");
```

**Options:**

| Approach | Cells in 128KB | Effort | Code sharing |
|---|---|---|---|
| A. Keep 8-byte NaN-boxed Value | ~16K | Low (lift compile_error, add `target_arch = "arm"`) | 100% shared with desktop |
| B. 32-bit tagged word (4 bytes) | ~32K | Very high (new Value repr) | Separate Value impl behind trait |
| C. 32-bit tagged union enum (8 bytes) | ~16K | High | Partial sharing |

**Recommendation:** Start with option A. On Pico 2 (520KB), 8-byte values give ~32K+ cells which is plenty. 64-bit arithmetic on Cortex-M33 is reasonable. Revisit option B only if RP2040 memory is too tight.

### 2. String Interning — lasso::Rodeo Needs std (HIGH)

`lasso` uses thread-local storage and std allocator. Not `no_std` compatible.

**Solution:** Replace with a fixed-capacity string table for the embedded target:
```rust
#[cfg(feature = "embedded")]
struct EmbeddedInterner {
    data: Vec<u8>,          // Byte arena for string content
    table: Vec<(u32, u16)>, // (offset, length) per Spur
    lookup: HashMap<u32, Spur>, // hash → Spur for dedup
}
```

The bytecode string table already carries all strings — the deserializer just needs to build local Spur mappings. No runtime interning of new strings is needed if we don't include the parser.

### 3. thread_local! Macros (MEDIUM)

Locations that use thread_local:
- `sema-core/src/value.rs:22` — `INTERNER` (string interning)
- `sema-core/src/value.rs:59` — `GENSYM_COUNTER`
- `sema-core/src/context.rs:320` — `STDLIB_CTX`
- `sema-stdlib/http.rs` — HTTP runtime/client (dropped)
- `sema-stdlib/terminal.rs` — spinner handles (dropped)
- `sema-stdlib/kv.rs` — KV stores (dropped)

**Solution:** Pico is single-threaded. Replace with static globals behind `#[cfg(feature = "embedded")]`:
```rust
#[cfg(feature = "embedded")]
static INTERNER: RefCell<EmbeddedInterner> = ...;

#[cfg(not(feature = "embedded"))]
thread_local! { static INTERNER: RefCell<Rodeo> = ...; }
```

### 4. Allocator for Rc/Vec (MEDIUM)

`Rc`, `Vec`, `String`, `Box`, `BTreeMap`, `HashMap` all need a global allocator.

**Solution:** Use `embedded-alloc` crate (linked-list allocator for `no_std + alloc`):
```rust
#[cfg(feature = "embedded")]
use embedded_alloc::LlffHeap as Heap;

#[cfg(feature = "embedded")]
#[global_allocator]
static ALLOCATOR: Heap = Heap::empty();
```

This keeps all existing Rc/Vec code working. Budget ~200KB of SRAM as heap on Pico 2.

## What to Keep vs. Drop

### Crates

| Crate | Keep | Notes |
|---|---|---|
| **sema-core** | YES (refactored) | Value, Env, SemaError, EvalContext. Gate PathBuf/env/home behind `std` feature |
| **sema-reader** | NO | Parser not needed on device — bytecode is pre-compiled |
| **sema-vm** (runtime only) | YES | vm.rs, chunk.rs, opcodes.rs. Gate compiler/lowerer/resolver behind `#[cfg(feature = "compiler")]` |
| **sema-vm** (serialize.rs) | PARTIAL | Deserializer only. Gate serializer behind `#[cfg(feature = "compiler")]` |
| **sema-eval** | NO | Tree-walker not needed — VM only |
| **sema-stdlib** | PARTIAL | Minimal pure-compute subset |
| **sema-llm** | NO | Network I/O, tokio |
| **sema-lsp** | NO | Desktop tooling |
| **sema-dap** | NO | Desktop tooling |
| **sema-fmt** | NO | Desktop tooling |
| **sema-wasm** | NO | Browser target |

### Stdlib Modules

| Module | Lines | Keep | Reason |
|---|---|---|---|
| `arithmetic.rs` | 166 | YES | `+`, `-`, `*`, `/`, `mod` |
| `comparison.rs` | 112 | YES | `<`, `>`, `=`, `zero?`, `even?` |
| `predicates.rs` | 146 | YES | `list?`, `number?`, `string?`, etc. |
| `list.rs` | 1,221 | YES | `car`, `cdr`, `cons`, `map`, `filter`, `fold` |
| `string.rs` | 1,325 | PARTIAL | Core string ops, drop regex-dependent parts |
| `bitwise.rs` | 68 | YES | `&`, `\|`, `<<`, `>>` — useful for hardware |
| `math.rs` | 468 | PARTIAL | `sin`, `cos`, `sqrt`, `abs` — drop matrix ops |
| `bytevector.rs` | 219 | YES | Binary data — essential for hardware I/O |
| `map.rs` | 774 | MAYBE | Data structure, useful but not critical |
| `json.rs` | 35 | MAYBE | Small, serde_json has `no_std` support |
| `http.rs` | 153 | NO | Network I/O |
| `server.rs` | 1,364 | NO | HTTP server |
| `io.rs` | 706 | NO | Filesystem I/O |
| `system.rs` | 245 | NO | Process spawning, env vars |
| `terminal.rs` | 272 | NO | ANSI colors, spinners |
| `regex_ops.rs` | 118 | NO | `regex` crate too large |
| `crypto.rs` | 93 | NO | Not essential for MCU |
| `datetime.rs` | 93 | NO | Chrono dependency |
| `csv_ops.rs` | 88 | NO | Specialized |
| `toml_ops.rs` | 87 | NO | Specialized |
| `pdf.rs` | 109 | NO | PDF parsing |
| `kv.rs` | 164 | NO | Redis/SQLite |

**Total kept:** ~2,500 lines (~28% of stdlib), ~100 native functions.

### Existing Conditional Compilation

The codebase already gates heavy modules for WASM:
```rust
// sema-stdlib/src/lib.rs
#[cfg(not(target_arch = "wasm32"))]
mod http;
#[cfg(not(target_arch = "wasm32"))]
mod io;
#[cfg(not(target_arch = "wasm32"))]
mod server;
#[cfg(not(target_arch = "wasm32"))]
mod system;
#[cfg(not(target_arch = "wasm32"))]
mod terminal;
```

Extend this pattern with `#[cfg(not(feature = "embedded"))]` for additional modules.

## Embedded-Specific Stdlib (New)

For Pico hardware interaction, add a small `gpio` module:
```rust
// sema-stdlib/src/gpio.rs (behind #[cfg(feature = "embedded")])
// gpio/mode     — set pin mode (input/output/pwm)
// gpio/read     — digital read
// gpio/write    — digital write
// gpio/pwm      — PWM output
// adc/read      — analog read
// i2c/write     — I2C bus write
// i2c/read      — I2C bus read
// spi/transfer  — SPI bus transfer
// delay/ms      — millisecond delay
// timer/millis  — uptime in milliseconds
```

Example Sema program for Pico:
```scheme
;; Blink an LED on GP25 (onboard LED)
(gpio/mode 25 :output)
(while true
  (gpio/write 25 1)
  (delay/ms 500)
  (gpio/write 25 0)
  (delay/ms 500))
```

## Memory Budget

### RP2350 (Pico 2) — 520KB SRAM

| Component | Budget |
|---|---|
| Rust runtime + stack | 16 KB |
| `embedded-alloc` heap metadata | 4 KB |
| VM state (stack 256 Values, 64 frames, inline cache) | 10 KB |
| String table (from bytecode) | 8-16 KB |
| Global env + ~100 native fns | 20 KB |
| HAL / peripheral buffers | 16 KB |
| **Value heap (user programs)** | **~400 KB** |
| → At 8 bytes/cell | **~50K cells** |

### RP2040 (Pico 1) — 264KB SRAM

| Component | Budget |
|---|---|
| Rust runtime + stack | 16 KB |
| VM state + string table + env | 40 KB |
| HAL / peripheral buffers | 16 KB |
| **Value heap** | **~180 KB** |
| → At 8 bytes/cell | **~22K cells** |

## Dependency Chain (Minimal)

```
sema-pico (new binary crate, #![no_std])
  ├→ sema-core (features = ["embedded"])
  │   ├→ hashbrown (no_std compatible)
  │   └→ thiserror (no_std compatible)
  │   ❌ lasso (replaced by embedded interner)
  │   ❌ serde_json (optional, behind feature)
  │   ❌ toml (dropped)
  │
  ├→ sema-vm (features = ["embedded"], default-features = false)
  │   ├→ sema-core
  │   └→ hashbrown
  │   ❌ sema-reader (not needed without compiler)
  │   ❌ lasso (same replacement)
  │
  ├→ sema-stdlib (features = ["embedded"])
  │   └→ sema-core
  │   ❌ regex, chrono, reqwest, tokio, axum, etc.
  │
  ├→ rp2350-hal (or rp2040-hal)
  ├→ embedded-alloc
  ├→ cortex-m-rt
  └→ panic-halt (or panic-probe for debugging)
```

## Estimated Binary Size

| Component | ROM (flash) |
|---|---|
| sema-core (embedded) | ~50 KB |
| sema-vm (runtime only) | ~100 KB |
| Deserializer | ~30 KB |
| Minimal stdlib | ~30 KB |
| HAL + cortex-m-rt | ~20 KB |
| **Total runtime** | **~230 KB** |
| Remaining for bytecode | ~1.7 MB (Pico 1) / ~15.7 MB (Pico 2) |

## Implementation Phases

### Phase 1: Feature-gate sema-core for no_std (~2-3 days)

- Add `std` (default) and `embedded` features to `sema-core/Cargo.toml`
- Gate `home.rs`, VFS filesystem ops, `PathBuf` usage behind `#[cfg(feature = "std")]`
- Create `EmbeddedInterner` as lasso replacement behind `#[cfg(feature = "embedded")]`
- Convert `thread_local!` to conditional: static globals for embedded, thread_local for std
- Lift the 64-bit `compile_error!` to also allow `target_arch = "arm"`
- Ensure `Value`, `Env`, `SemaError`, `EvalContext` compile under `no_std + alloc`

### Phase 2: Feature-gate sema-vm (~1-2 days)

- Split sema-vm into runtime vs. compiler features:
  - `compiler` feature (default): lower.rs, resolve.rs, compiler.rs, optimize.rs, emit.rs, disasm.rs
  - Always included: vm.rs, chunk.rs, opcodes.rs
  - `serializer` feature (default): serialize half of serialize.rs
  - Always included: deserialize half
- Gate `sema-reader` dependency behind `compiler` feature
- Gate debug hooks (mpsc channels) behind `std` feature
- Verify VM compiles under `no_std + alloc`

### Phase 3: Minimal embedded stdlib (~1-2 days)

- Add `embedded` feature to sema-stdlib
- Feature-gate all I/O-dependent modules (http, server, io, system, terminal, etc.)
- Create `register_embedded_stdlib(env)` that registers only pure-compute functions
- Strip regex dependency from string.rs behind feature gate
- Test: all kept functions work without std

### Phase 4: sema-pico binary crate (~3-5 days)

- Create `crates/sema-pico/` with RP2350 HAL setup
- `#![no_std]`, `#![no_main]`, `embedded-alloc` global allocator
- Bytecode loading from flash (`include_bytes!`) or UART
- GPIO/I2C/SPI native functions wrapping rp2350-hal
- UART output for `println` / debug
- Basic REPL over UART (optional — requires reader on device)
- Test on real hardware with probe-rs

### Phase 5: RP2040 support (stretch goal, ~2-3 days)

- Test on RP2040 (264KB constraint)
- Profile 64-bit arithmetic performance on Cortex-M0+
- Tune stack/heap sizes for tighter memory
- Consider 32-bit Value representation if memory is insufficient

### Phase 6: Developer experience (~2-3 days)

- `sema build --target thumbv8m.main-none-eabihf` to cross-compile + bundle bytecode into Pico binary
- `sema flash` command wrapping probe-rs or picotool
- UART-based REPL for interactive development
- Example programs: blink, sensor reading, I2C display, servo control

## Rust Ecosystem Tools

| Tool | Purpose |
|---|---|
| `rp2350-hal` / `rp2040-hal` | Hardware Abstraction Layer |
| `embassy-rp` | Async HAL alternative (if async desired) |
| `embedded-alloc` | Global allocator for `no_std + alloc` |
| `cortex-m-rt` | Runtime startup (vector table, memory init) |
| `probe-rs` / `cargo-embed` | Flash and debug (RP2040; RP2350 needs picotool) |
| `defmt` + `probe-rs` | Efficient logging over debug probe |
| `panic-probe` | Panic handler that logs via probe |
| `embedded-hal` | Portable peripheral traits |

## Open Questions

1. **Reader on device?** Including `sema-reader` enables a UART REPL but adds ~50KB to the binary and requires runtime string interning. Worth it for development, not for production.
2. **Async or blocking?** Embassy-rp enables async/await for I/O (useful for concurrent sensor reads + computation). Adds complexity. Blocking HAL is simpler to start.
3. **GC?** Reference counting via Rc works but leaks cycles. For long-running embedded programs, may need a cycle detector or arena-based tracing GC. Monitor in practice first.
4. **Dual-core?** Both Pico variants have dual cores. Could run Sema VM on core 0 and hardware interrupt handling on core 1. Adds Arc requirement — breaks single-threaded Rc model.
5. **Pre-baked global env?** Could serialize the initialized global Env (with all native function bindings) into flash at build time, avoiding runtime registration overhead.

## References

- [rp-rs/rp-hal](https://github.com/rp-rs/rp-hal) — RP2040/RP2350 HAL
- [embassy-rs/embassy](https://github.com/embassy-rs/embassy) — Async embedded framework
- [embedded-alloc](https://github.com/rust-embedded/embedded-alloc) — no_std allocator
- [uLisp RP2040 port](http://www.ulisp.com/show?3KN3=) — Lisp on Pico reference
- [Grift](https://github.com/gold-silver-copper/grift) — Rust no_std Lisp
- [pico-jit](https://project-archive.inf.ed.ac.uk/ug4/20244681/ug4_proj.pdf) — WASM JIT on RP2040
- [Pico Embedded Rust Guide](https://pico.implrust.com/)
- Sema bytecode format spec: `website/docs/internals/bytecode-format.md`
