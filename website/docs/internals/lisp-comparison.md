# Lisp Dialect Benchmark

How does Sema compare to other Lisp dialects on a real-world I/O-heavy workload? We benchmarked fifteen implementations on the [1 Billion Row Challenge](https://github.com/gunnarmorling/1brc) — a data processing task that reads weather station measurements and computes min/mean/max per station. This is not a synthetic micro-benchmark; it exercises I/O, string parsing, hash table accumulation, and numeric aggregation in a tight loop.

## Results

**10 million rows (1.2 GB), best of 3 runs, single-threaded, Docker (linux/amd64):**

| Dialect | Implementation | Time (ms) | Relative | Category |
|---------|---------------|-----------|----------|----------|
| **SBCL** | Native compiler | 2,109 | 1.0x | Compiled |
| **Chez Scheme** | Native compiler | 2,817 | 1.3x | Compiled |
| **Fennel/LuaJIT** | JIT compiler | 3,308 | 1.6x | JIT-compiled |
| **Gambit** | Native compiler (gsc) | 5,539 | 2.6x | Compiled |
| **Clojure** | JVM (JIT) | 5,648 | 2.7x | JIT-compiled |
| **Chicken** | Native compiler (csc) | 7,547 | 3.6x | Compiled |
| **PicoLisp** | Interpreter | 9,506 | 4.5x | Interpreted |
| **newLISP** | Interpreter | 12,023 | 5.7x | Interpreted |
| **Emacs Lisp** | Bytecode VM | 12,837 | 6.1x | Interpreted |
| **Janet** | Bytecode VM | 13,771 | 6.5x | Interpreted |
| **ECL** | Native compiler | 14,444 | 6.8x | Compiled |
| **Guile** | Bytecode VM | 14,956 | 7.1x | Interpreted |
| **Sema** | Tree-walking interpreter | 15,392 | 7.3x | Interpreted |
| **Kawa** | JVM (JIT) | 17,479 | 8.3x | JIT-compiled |
| **Gauche** | Bytecode VM | 22,646 | 10.7x | Interpreted |

Racket was excluded — both the CS (Chez Scheme) and BC (bytecode) backends crash under x86-64 emulation on Apple Silicon. This is a Docker/emulation issue, not a Racket performance issue; Racket CS would likely land between Chez and Clojure.

::: info Compiled mode
Gambit, Chicken, and ECL are now benchmarked in compiled mode (compiling to native code via C), not interpreter mode. Previous versions of this benchmark ran them as interpreters, which was 3–6x slower. Guile now runs with bytecode auto-compilation enabled.
:::

::: info Native performance
Sema runs faster natively on Apple Silicon (~9.6s) than under x86-64 emulation (15.4s). All dialects in this table were measured under the same Docker/emulation environment for a fair comparison.
:::

## Why SBCL Wins

SBCL compiles Common Lisp to native machine code. There is no interpreter loop, no bytecode dispatch — `(+ x y)` compiles to an `ADD` instruction. Combined with `(declare (optimize (speed 3) (safety 0)))`, the benchmark's inner loop runs at near-C speed:

- **Block I/O:** Reads 1MB chunks via `read-sequence`, parsing lines from a buffer — no per-line syscall overhead
- **Custom integer parser:** Parses temperatures as integers (×10), avoiding float parsing entirely until the final division
- **Hash table with `equal` test:** SBCL's hash table implementation is highly optimized with type-specialized hashing
- **In-place struct mutation:** `station` structs are updated via `setf` with no allocation per row

SBCL's compiler has had 25+ years of optimization work (descended from CMUCL, which traces back to the 1980s). When you tell it `(safety 0)`, it trusts your type declarations and removes all runtime checks — a trade-off most interpreted languages can't make.

## Chez Scheme: The Other Native Compiler

Chez Scheme compiles to native code via a [nanopass compiler framework](https://nanopass.org/). It's 1.4x behind SBCL here, which is consistent with typical benchmarks — Chez tends to be slightly slower than SBCL on I/O-heavy workloads but competitive on computation.

The implementation uses:
- `get-line` for line reading (one syscall per line, no block I/O optimization)
- Custom character-by-character temperature parser
- Mutable vectors in a `make-hashtable` with `string-hash`

The gap to SBCL is likely explained by the per-line I/O — `get-line` allocates a fresh string per call, while SBCL's block read amortizes this.

## Fennel/LuaJIT: The JIT Surprise

Fennel compiling to LuaJIT at 1.6x is the biggest surprise — faster than both Clojure and compiled Gambit. LuaJIT's tracing JIT compiler generates native code for the hot loop after a few iterations, and Lua's table implementation (used for both hash maps and arrays) is famously efficient. The implementation is straightforward Fennel: `string.find` for semicolons, `tonumber` for parsing, Lua tables for accumulation. No special optimization tricks — LuaJIT's JIT does the heavy lifting.

## Clojure: JVM Tax + Warmup

Clojure's 2.7x result includes JVM startup and JIT warmup. The actual steady-state throughput after warmup is faster than the wall-clock time suggests, but for a single-shot script, the JVM overhead is real:

- **Startup:** ~1–2 seconds just to load the Clojure runtime
- **`line-seq` + `reduce`:** Lazy line reading with a transient map for accumulation — idiomatic but not zero-cost
- **`Double/parseDouble`:** JVM's float parser handles the full spec (scientific notation, hex floats), more work than a hand-rolled decimal parser
- **GC pauses:** The JVM's garbage collector adds latency variance

Clojure's strength is that this code is *15 lines* — the most concise implementation in the benchmark. It trades raw speed for developer productivity.

## PicoLisp: Integer Arithmetic Pays Off

PicoLisp's 4.5x result is impressive for a pure interpreter with no bytecode compilation. PicoLisp has no native floating-point — all arithmetic is integer-based. The benchmark uses temperatures multiplied by 10 (e.g., "12.3" → 123), avoiding float parsing entirely. PicoLisp's idx trees (balanced binary trees) provide O(log n) lookup and keep results sorted for free. The lack of float overhead gives it a significant edge over implementations that parse and accumulate floats on every row.

## newLISP: Simple but Effective

newLISP at 5.7x is surprisingly competitive. Its association-list-based accumulation has O(n) lookup per station, but with only 40 stations, the list stays small enough that linear search is fast. newLISP's `read-line`/`current-line` idiom and `find`/`slice` string operations are efficient C implementations. The language's simplicity — no complex type system, no numeric tower — means less overhead per operation.

## Gambit: Compiled Scheme via C

Gambit at 2.6x — virtually tied with Clojure — is the standout result among Scheme compilers. `gsc` compiles Scheme to C, then compiles C to a native binary. The result is competitive with Chez Scheme's native compiler, especially impressive given that Gambit's built-in `sort` crashes under x86-64 emulation (requiring a manual merge sort in the benchmark).

## Chicken: Compiled Scheme, I/O Bound

Chicken at 3.6x compiles Scheme to C via `csc -O3`. The remaining gap to SBCL/Chez is likely due to Chicken's `string->number` handling the full numeric tower and per-line I/O allocation. Chicken's compilation strategy (compiling to continuation-passing style C) produces correct but not maximally optimized code for this I/O-heavy workload.

## Janet: A Fair Comparison

Janet is the most architecturally comparable to Sema — both are:
- Embeddable scripting languages written in C/Rust
- Single-threaded with reference counting (Janet) / `Rc` (Sema)
- Focused on practical scripting rather than language theory
- No JIT, no native compilation

Janet compiles to bytecode and runs on a register-based VM, which should be faster than tree-walking. Under the same Docker environment, Janet is 1.1x faster than Sema (13.8s vs 15.4s) — the bytecode VM advantage is real, but Sema's hot-path optimizations (mini-eval, COW mutation, SIMD string splitting, vector mini-eval, let* flattening) keep the gap narrow.

Janet's implementation is straightforward: `file/read :line` in a loop, `string/find` + `string/slice` for parsing, mutable tables for accumulation. No special optimizations.

## Guile and Gauche: Scheme Bytecode VMs

Guile (7.1x) and Gauche (10.7x) are both R7RS-compliant Scheme implementations with bytecode VMs. Guile runs with bytecode auto-compilation enabled, which compiles source to bytecode on first execution and caches it for subsequent runs. Gauche is slower partly because its `string->number` handles the full numeric tower. Both would benefit from native compilation, but neither currently offers it as a standard option.

## Sema: The Interpreter Tax

Sema's 7.3x result (15.4s under emulation, 9.6s native) reflects the fundamental cost of tree-walking interpretation. Every operation — reading a line, splitting a string, parsing a number, updating a map — is a function call through the evaluator, with environment lookup, `Rc` reference counting, and trampoline dispatch.

Despite being a tree-walking interpreter — the simplest possible execution model — Sema lands ahead of Kawa (a JVM Scheme compiler), Gauche, and within striking distance of Guile and ECL. This is due to several hot-path optimizations:

- **`file/fold-lines`:** Reuses the lambda environment across iterations (no allocation per line)
- **Mini-eval:** Bypasses the full trampoline evaluator for hot-path operations (`+`, `=`, `min`, `max`, `assoc`, `vector`, `string/split`)
- **COW map mutation:** `assoc` mutates in-place when the `Rc` refcount is 1 (which `file/fold-lines` ensures by moving, not cloning, the accumulator)
- **`hashmap/new`:** Amortized O(1) lookup via `hashbrown` instead of O(log n) `BTreeMap`
- **memchr SIMD:** `string/split` uses SIMD-accelerated byte search for the separator
- **`let*` flattening:** Using `let*` instead of nested `let` reduces environment allocations from 3 per line to 1

Without these optimizations, Sema would be in the 30–40s range. See the [Performance Internals](./performance.md) page for the optimization journey.

## Kawa: JVM Scheme, Slower Than Expected

Kawa at 8.3x is slower than Clojure despite both running on the JVM. Kawa compiles Scheme to JVM bytecode, but its `string->number` implementation handles the full Scheme numeric tower (exact rationals, complex numbers), which is more expensive than Clojure's `Double/parseDouble`. The `java.util.HashMap` usage should be fast, but Kawa's compilation model introduces overhead for Scheme-specific features like tail-call optimization and continuations that the JVM doesn't natively support.

## ECL: Common Lisp via C

ECL at 6.8x compiles Common Lisp to C via `compile-file`, producing a native FASL. The remaining gap to SBCL is largely due to `read-from-string` for temperature parsing — ECL invokes the full Common Lisp reader for each value, which is far more expensive than a hand-rolled decimal parser like SBCL uses.

## Emacs Lisp: Buffer-Based I/O

Emacs Lisp at 6.1x loads the entire file into a buffer with `insert-file-contents`, then processes it line by line. This is actually an efficient I/O strategy (single read call), but Emacs Lisp's bytecode interpreter is optimized for interactive editing, not data processing. The `string-search` and `string-to-number` functions are written in C and fast, but the Lisp-level loop overhead (function calls, dynamic scoping checks) adds up over 10 million rows.

## What This Benchmark Doesn't Show

This is one workload. Different benchmarks would produce different orderings:

- **CPU-bound computation** (fibonacci, sorting): SBCL and Chez would dominate even more; the I/O amortizes some of the interpreter gap
- **Startup time:** Janet and Sema start in <10ms; Clojure takes 1–2s; SBCL takes ~50ms
- **Memory usage:** Janet and Sema use minimal memory (tens of MB); Clojure's JVM baseline is ~100MB+
- **Multi-threaded:** Clojure (on the JVM) and SBCL (with `lparallel`) can parallelize; Sema, Janet, and Guile are single-threaded
- **Developer experience:** Clojure's REPL, Racket's IDE (DrRacket), and SBCL's SLIME/Sly integration are far more mature than Sema's
- **Compilation flags:** SBCL's `(safety 0)` and Chicken's `-O3` are used; other compilers may have additional optimization flags not explored here

## Methodology

- **Dataset:** 10,000,000 rows, 40 weather stations, generated from the [1BRC specification](https://github.com/gunnarmorling/1brc) with fixed station statistics
- **Environment:** Docker container (`debian:bookworm-slim`, linux/amd64), running on Apple Silicon via Rosetta/QEMU
- **Measurement:** Wall-clock time via `date +%s%N`, best of 3 consecutive runs per dialect
- **Verification:** All implementations produce identical output (sorted station results, 1 decimal place rounding)
- **Code style:** Each implementation is idiomatic for its dialect — no artificial handicaps, but no heroic micro-optimization either (except SBCL's `(safety 0)` declarations, which are standard practice)
- **Compilation:** Gambit (`gsc -exe`), Chicken (`csc -O3`), and ECL (`compile-file`) are compiled to native code before benchmarking. Guile uses bytecode auto-compilation. All other dialects run in their default mode.

### Versions

| Dialect | Version | Package |
|---------|---------|---------|
| SBCL | 2.2.9 | `sbcl` (Debian bookworm) |
| Chez Scheme | 9.5.8 | `chezscheme` (Debian bookworm) |
| Fennel | 1.5.1 | Downloaded binary |
| LuaJIT | 2.1.0 | `luajit` (Debian bookworm) |
| Clojure | 1.12.0 | CLI tools |
| PicoLisp | 23.2 | `picolisp` (Debian bookworm) |
| newLISP | 10.7.5 | `newlisp` (Debian bookworm) |
| Sema | 0.9.0 | `cargo install --git` (Docker) |
| Janet | 1.37.1 | Built from source |
| Kawa | 3.1.1 | JAR from Maven Central |
| Gauche | 0.9.15 | Built from source |
| Guile | 3.0.8 | `guile-3.0` (Debian bookworm) |
| Emacs | 28.2 | `emacs-nox` (Debian bookworm) |
| Gambit | 4.9.3 | `gambc` compiled via `gsc -exe` (Debian bookworm) |
| ECL | 21.2.1 | `ecl` compiled via `compile-file` (Debian bookworm) |
| Chicken | 5.3.0 | `chicken-bin` compiled via `csc -O3` (Debian bookworm) |

### Reproducing

```bash
cd benchmarks/1brc

# Generate test data (or use existing bench-10m.txt)
python3 generate-test-data.py 10000000 measurements.txt

# Build Docker image with all runtimes
docker build --platform linux/amd64 -t sema-1brc-bench .

# Run benchmarks
docker run --platform linux/amd64 --rm \
  -v $(pwd)/../../bench-10m.txt:/data/measurements.txt:ro \
  -v $(pwd)/results:/results \
  sema-1brc-bench /data/measurements.txt

# Run Sema natively for comparison
cargo run --release -- --no-llm examples/benchmarks/1brc.sema -- bench-10m.txt
```

Source code for all implementations is in [`benchmarks/1brc/`](https://github.com/HelgeSverre/sema/tree/main/benchmarks/1brc).
