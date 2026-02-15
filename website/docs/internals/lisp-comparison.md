# Lisp Dialect Benchmark

How does Sema compare to other Lisp dialects on a real-world I/O-heavy workload? We benchmarked fifteen implementations on the [1 Billion Row Challenge](https://github.com/gunnarmorling/1brc) — a data processing task that reads weather station measurements and computes min/mean/max per station. This is not a synthetic micro-benchmark; it exercises I/O, string parsing, hash table accumulation, and numeric aggregation in a tight loop.

## Results (Optimized)

**10 million rows (1.2 GB), best of 3 runs, single-threaded, Docker (linux/amd64):**

| Dialect | Implementation | Time (ms) | Relative | Category |
|---------|---------------|-----------|----------|----------|
| **SBCL** | Native compiler | 2,108 | 1.0x | Compiled |
| **Chez Scheme** | Native compiler | 2,889 | 1.4x | Compiled |
| **Fennel/LuaJIT** | JIT compiler | 3,658 | 1.7x | JIT-compiled |
| **Gambit** | Native compiler (gsc) | 5,665 | 2.7x | Compiled |
| **Clojure** | JVM (JIT) | 5,717 | 2.7x | JIT-compiled |
| **Chicken** | Native compiler (csc) | 7,631 | 3.6x | Compiled |
| **PicoLisp** | Interpreter | 9,808 | 4.7x | Interpreted |
| **newLISP** | Interpreter | 12,481 | 5.9x | Interpreted |
| **Emacs Lisp** | Bytecode VM | 13,505 | 6.4x | Interpreted |
| **Janet** | Bytecode VM | 14,000 | 6.6x | Interpreted |
| **ECL** | Native compiler | 14,915 | 7.1x | Compiled |
| **Guile** | Bytecode VM | 15,198 | 7.2x | Interpreted |
| **Sema** | Tree-walking interpreter | 15,564 | 7.4x | Interpreted |
| **Kawa** | JVM (JIT) | 17,135 | 8.1x | JIT-compiled |
| **Gauche** | Bytecode VM | 23,082 | 10.9x | Interpreted |

Racket was excluded — both the CS (Chez Scheme) and BC (bytecode) backends crash under x86-64 emulation on Apple Silicon. This is a Docker/emulation issue, not a Racket performance issue; Racket CS would likely land between Chez and Clojure.

::: info Compiled mode
Gambit, Chicken, and ECL are now benchmarked in compiled mode (compiling to native code via C), not interpreter mode. Previous versions of this benchmark ran them as interpreters, which was 3–6x slower. Guile now runs with bytecode auto-compilation enabled.
:::

::: info Native performance
Sema runs faster natively on Apple Silicon (~9.6s) than under x86-64 emulation (15.6s). All dialects in this table were measured under the same Docker/emulation environment for a fair comparison.
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

Fennel compiling to LuaJIT at 1.7x is the biggest surprise — faster than both Clojure and compiled Gambit. LuaJIT's tracing JIT compiler generates native code for the hot loop after a few iterations, and Lua's table implementation (used for both hash maps and arrays) is famously efficient. The implementation is straightforward Fennel: `string.find` for semicolons, `tonumber` for parsing, Lua tables for accumulation. No special optimization tricks — LuaJIT's JIT does the heavy lifting.

## Clojure: JVM Tax + Warmup

Clojure's 2.7x result includes JVM startup and JIT warmup. The actual steady-state throughput after warmup is faster than the wall-clock time suggests, but for a single-shot script, the JVM overhead is real:

- **Startup:** ~1–2 seconds just to load the Clojure runtime
- **`line-seq` + `reduce`:** Lazy line reading with a transient map for accumulation — idiomatic but not zero-cost
- **`Double/parseDouble`:** JVM's float parser handles the full spec (scientific notation, hex floats), more work than a hand-rolled decimal parser
- **GC pauses:** The JVM's garbage collector adds latency variance

Clojure's strength is that this code is *15 lines* — the most concise implementation in the benchmark. It trades raw speed for developer productivity.

## PicoLisp: Integer Arithmetic Pays Off

PicoLisp's 4.7x result is impressive for a pure interpreter with no bytecode compilation. PicoLisp has no native floating-point — all arithmetic is integer-based. The benchmark uses temperatures multiplied by 10 (e.g., "12.3" → 123), avoiding float parsing entirely. PicoLisp's idx trees (balanced binary trees) provide O(log n) lookup and keep results sorted for free. The lack of float overhead gives it a significant edge over implementations that parse and accumulate floats on every row.

## newLISP: Simple but Effective

newLISP at 5.9x is surprisingly competitive. Its association-list-based accumulation has O(n) lookup per station, but with only 40 stations, the list stays small enough that linear search is fast. newLISP's `read-line`/`current-line` idiom and `find`/`slice` string operations are efficient C implementations. The language's simplicity — no complex type system, no numeric tower — means less overhead per operation.

## Gambit: Compiled Scheme via C

Gambit at 2.7x — virtually tied with Clojure — is the standout result among Scheme compilers. `gsc` compiles Scheme to C, then compiles C to a native binary. The result is competitive with Chez Scheme's native compiler, especially impressive given that Gambit's built-in `sort` crashes under x86-64 emulation (requiring a manual merge sort in the benchmark).

## Chicken: Compiled Scheme, I/O Bound

Chicken at 3.6x compiles Scheme to C via `csc -O3`. The optimized implementation uses a hand-rolled integer×10 temperature parser that avoids float parsing entirely — without it, Chicken drops to 13.6s (1.8x slower). The remaining gap to SBCL/Chez is due to per-line I/O allocation and Chicken's compilation strategy (continuation-passing style C), which produces correct but not maximally optimized code for this I/O-heavy workload.

## Janet: A Fair Comparison

Janet is the most architecturally comparable to Sema — both are:
- Embeddable scripting languages written in C/Rust
- Single-threaded with reference counting (Janet) / `Rc` (Sema)
- Focused on practical scripting rather than language theory
- No JIT, no native compilation

Janet compiles to bytecode and runs on a register-based VM, which should be faster than tree-walking. Under the same Docker environment, Janet is 0.9x faster than Sema (14.0s vs 15.6s) — the bytecode VM advantage is real, but Sema's hot-path optimizations (mini-eval, COW mutation, SIMD string splitting, vector mini-eval, let* flattening) keep the gap narrow.

Janet's implementation is straightforward: `file/read :line` in a loop, `string/find` + `string/slice` for parsing, mutable tables for accumulation. No special optimizations.

## Guile and Gauche: Scheme Bytecode VMs

Guile (7.2x) and Gauche (10.9x) are both R7RS-compliant Scheme implementations with bytecode VMs. Guile runs with bytecode auto-compilation enabled, which compiles source to bytecode on first execution and caches it for subsequent runs. Guile's optimized implementation uses a hand-rolled integer×10 parser, saving ~7% vs `string->number`. Gauche uses `string->number` in both versions — a hand-rolled char-by-char parser is actually *slower* in Gauche because `string-ref` has O(n) cost in its internal UTF-32 representation, while `string->number` is implemented in C.

## Sema: The Interpreter Tax

Sema's 7.4x result (15.6s under emulation, 9.6s native) reflects the fundamental cost of tree-walking interpretation. Every operation — reading a line, splitting a string, parsing a number, updating a map — is a function call through the evaluator, with environment lookup, `Rc` reference counting, and trampoline dispatch.

Despite being a tree-walking interpreter — the simplest possible execution model — Sema lands ahead of Kawa (a JVM Scheme compiler), Gauche, and within striking distance of Guile and ECL. This is due to several hot-path optimizations:

- **`file/fold-lines`:** Reuses the lambda environment across iterations (no allocation per line)
- **Mini-eval:** Bypasses the full trampoline evaluator for hot-path operations (`+`, `=`, `min`, `max`, `assoc`, `vector`, `string/split`)
- **COW map mutation:** `assoc` mutates in-place when the `Rc` refcount is 1 (which `file/fold-lines` ensures by moving, not cloning, the accumulator)
- **`hashmap/new`:** Amortized O(1) lookup via `hashbrown` instead of O(log n) `BTreeMap`
- **memchr SIMD:** `string/split` uses SIMD-accelerated byte search for the separator
- **`let*` flattening:** Using `let*` instead of nested `let` reduces environment allocations from 3 per line to 1

Without these optimizations, Sema would be in the 30–40s range. See the [Performance Internals](./performance.md) page for the optimization journey.

## Kawa: JVM Scheme, Slower Than Expected

Kawa at 8.1x is slower than Clojure despite both running on the JVM. Kawa compiles Scheme to JVM bytecode, but its `string->number` implementation handles the full Scheme numeric tower (exact rationals, complex numbers), which is more expensive than Clojure's `Double/parseDouble`. The `java.util.HashMap` usage should be fast, but Kawa's compilation model introduces overhead for Scheme-specific features like tail-call optimization and continuations that the JVM doesn't natively support.

## ECL: Common Lisp via C

ECL at 7.1x compiles Common Lisp to C via `compile-file`, producing a native FASL. The optimized implementation uses a hand-rolled integer×10 parser; without it (using `read-from-string` instead), ECL drops to 21.5s — a 1.4x slowdown. The remaining gap to SBCL is due to ECL's less aggressive native code generation compared to SBCL's mature optimizer.

## Emacs Lisp: Buffer-Based I/O

Emacs Lisp at 6.4x loads the entire file into a buffer with `insert-file-contents-literally`, then parses temperatures using a manual integer×10 parser that reads characters directly from the buffer without extracting substrings. Without this optimization (using `string-to-number` on extracted substrings instead), Emacs drops to 22.2s — a 1.6x slowdown. The in-buffer parsing avoids both string allocation and float overhead, which matters over 10 million rows.

## Results (Simple/Idiomatic)

To measure raw language runtime speed — independent of implementation tricks — we also benchmarked "simple" versions of each implementation. These use the language's built-in number parser (`string->number`, `string-to-number`, `tonumber`, etc.), per-line I/O, and standard data structures. No custom integer parsers, no block reads, no `(safety 0)`, no SIMD.

**10 million rows (1.2 GB), best of 3 runs, single-threaded, Docker (linux/amd64):**

| Dialect | Implementation | Time (ms) | Relative | vs Optimized |
|---------|---------------|-----------|----------|--------------|
| **Fennel/LuaJIT** | JIT compiler | 3,595 | 1.0x | ~same |
| **Chez Scheme** | Native compiler | 4,687 | 1.3x | 1.6x slower |
| **Clojure** | JVM (JIT) | 5,573 | 1.6x | ~same |
| **Gambit** | Native compiler (gsc) | 5,593 | 1.6x | ~same |
| **SBCL** | Native compiler | 7,838 | 2.2x | 3.7x slower |
| **PicoLisp** | Interpreter | 9,903 | 2.8x | ~same |
| **newLISP** | Interpreter | 12,377 | 3.4x | ~same |
| **Janet** | Bytecode VM | 13,378 | 3.7x | ~same |
| **Chicken** | Native compiler (csc) | 13,600 | 3.8x | 1.8x slower |
| **Guile** | Bytecode VM | 16,235 | 4.5x | 1.1x slower |
| **Kawa** | JVM (JIT) | 16,364 | 4.6x | ~same |
| **Sema** | Tree-walking interpreter | 16,373 | 4.6x | 1.1x slower |
| **ECL** | Native compiler | 21,459 | 6.0x | 1.4x slower |
| **Emacs Lisp** | Bytecode VM | 22,233 | 6.2x | 1.6x slower |
| **Gauche** | Bytecode VM | 22,405 | 6.2x | ~same |

The simple results are normalized to Fennel (the fastest simple implementation) rather than SBCL, since SBCL benefits the most from its optimizations.

## What the Simple Benchmarks Reveal

Comparing simple vs optimized times shows where optimization effort pays off and where the language runtime does the heavy lifting:

| Dialect | Optimized | Simple | Slowdown | What the Optimized Version Does |
|---------|----------|--------|----------|-------------------------------|
| **SBCL** | 2,108 | 7,838 | **3.7x** | Block I/O, `(safety 0)`, custom int×10 parser, typed structs |
| **Chicken** | 7,631 | 13,600 | **1.8x** | Custom int×10 parser avoids `string->number` numeric tower |
| **Chez** | 2,889 | 4,687 | **1.6x** | Custom char-by-char parser avoids `string->number` |
| **Emacs** | 13,505 | 22,233 | **1.6x** | In-buffer int×10 parser avoids string extraction + float parsing |
| **ECL** | 14,915 | 21,459 | **1.4x** | Custom int×10 parser avoids `read-from-string` (full CL reader) |
| **Guile** | 15,198 | 16,235 | **1.1x** | Custom int×10 parser, modest improvement |
| **Sema** | 15,564 | 16,373 | **1.1x** | `string->float` + hashmap vs `string->number` + sorted map |
| **Fennel** | 3,658 | 3,595 | ~same | Already simple — LuaJIT's JIT optimizes it |
| **Gambit** | 5,665 | 5,593 | ~same | Already uses `string->number` |
| **Clojure** | 5,717 | 5,573 | ~same | Only transient→persistent map optimization |
| **Janet** | 14,000 | 13,378 | ~same | Already simple |
| **Kawa** | 17,135 | 16,364 | ~same | `Double/parseDouble` → `string->number` (similar cost) |
| **PicoLisp** | 9,808 | 9,903 | ~same | No floats — int×10 is the only option |
| **newLISP** | 12,481 | 12,377 | ~same | Already simple |
| **Gauche** | 23,082 | 22,405 | ~same | `string->number` (C impl) is faster than hand-rolled Scheme |

**Key takeaways:**

- **SBCL's 3.7x optimization gain is the largest** — block I/O + `(safety 0)` + type declarations transform it from 2.2x to 1.0x relative. Without its optimizations, SBCL would rank 5th, behind Fennel, Chez, Clojure, and Gambit.
- **Number parsing is the dominant optimization** — every dialect that benefits from optimization does so primarily by replacing the language's built-in number parser with a hand-rolled integer×10 parser. This avoids the overhead of handling the full numeric tower, scientific notation, and float precision.
- **Fennel/LuaJIT is the fastest with zero optimization effort.** The simple version is actually the same code — LuaJIT's tracing JIT does all the work. This makes Fennel the clear winner in "performance per line of code."
- **Gauche's `string-ref` is O(n)** — a hand-rolled char-by-char parser is actually *slower* than `string->number` (C implementation) because Gauche uses a UTF-32 internal representation where `string-ref` must scan from the beginning.
- **Sema's optimizations provide only a 5% gain** in the simple benchmark comparison, because `file/fold-lines` and COW mutation work in both versions. The main optimization path (mini-eval, SIMD split) is baked into the runtime, not the benchmark script.

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
| Sema | 0.9.0 | Built from source (Docker) |
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

# Run optimized benchmarks
docker run --platform linux/amd64 --rm \
  -v $(pwd)/../../bench-10m.txt:/data/measurements.txt:ro \
  -v $(pwd)/results:/results \
  sema-1brc-bench /data/measurements.txt

# Run simple/idiomatic benchmarks
docker run ... sema-1brc-bench --simple /data/measurements.txt

# Run both
docker run ... sema-1brc-bench --all /data/measurements.txt

# Run Sema natively for comparison
cargo run --release -- --no-llm examples/benchmarks/1brc.sema -- bench-10m.txt
```

Source code for all implementations is in [`benchmarks/1brc/`](https://github.com/HelgeSverre/sema/tree/main/benchmarks/1brc) (optimized) and [`benchmarks/1brc/simple/`](https://github.com/HelgeSverre/sema/tree/main/benchmarks/1brc/simple) (simple/idiomatic).
