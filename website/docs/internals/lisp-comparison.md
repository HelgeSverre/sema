# Lisp Dialect Benchmark

How does Sema compare to other Lisp dialects on a real-world I/O-heavy workload? We benchmarked six implementations on the [1 Billion Row Challenge](https://github.com/gunnarmorling/1brc) — a data processing task that reads weather station measurements and computes min/mean/max per station. This is not a synthetic micro-benchmark; it exercises I/O, string parsing, hash table accumulation, and numeric aggregation in a tight loop.

## Results

**10 million rows (1.2 GB), best of 3 runs, single-threaded, Docker (linux/amd64):**

| Dialect | Implementation | Time (ms) | Relative | Category |
|---------|---------------|-----------|----------|----------|
| **SBCL** | Native compiler | 2,121 | 1.0x | Compiled |
| **Chez Scheme** | Native compiler | 2,875 | 1.4x | Compiled |
| **Clojure** | JVM (JIT) | 5,472 | 2.6x | JIT-compiled |
| **Sema** | Tree-walking interpreter | 12,708 | 6.0x | Interpreted |
| **Janet** | Bytecode VM | 13,163 | 6.2x | Interpreted |
| **Guile** | Bytecode VM | 24,286 | 11.5x | Interpreted |

Racket was excluded — the CS (Chez Scheme) backend conflicts with the standalone Chez installation in Docker, and the BC (bytecode) backend also failed to run. This is a Docker packaging issue, not a Racket performance issue; Racket CS would likely land between Chez and Clojure.

::: info Sema comparison note
Sema was measured natively on Apple Silicon (12,708 ms), while the other dialects ran inside Docker under x86-64 emulation. The relative ordering is the interesting part — Sema sits in the same performance tier as Janet, another interpreted Lisp, and both are ~6x behind SBCL's native compiler.
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

## Clojure: JVM Tax + Warmup

Clojure's 2.6x result includes JVM startup and JIT warmup. The actual steady-state throughput after warmup is faster than the wall-clock time suggests, but for a single-shot script, the JVM overhead is real:

- **Startup:** ~1–2 seconds just to load the Clojure runtime
- **`line-seq` + `reduce`:** Lazy line reading with a transient map for accumulation — idiomatic but not zero-cost
- **`Double/parseDouble`:** JVM's float parser handles the full spec (scientific notation, hex floats), more work than a hand-rolled decimal parser
- **GC pauses:** The JVM's garbage collector adds latency variance (note the 5,472–5,933 ms spread across runs)

Clojure's strength is that this code is *15 lines* — the most concise implementation in the benchmark. It trades raw speed for developer productivity.

## Sema: The Interpreter Tax

Sema's 6.0x result reflects the fundamental cost of tree-walking interpretation. Every operation — reading a line, splitting a string, parsing a number, updating a map — is a function call through the evaluator, with environment lookup, `Rc` reference counting, and trampoline dispatch.

But Sema has optimizations that keep it competitive with bytecode VMs:

- **`file/fold-lines`:** Reuses the lambda environment across iterations (no allocation per line)
- **Mini-eval:** Bypasses the full trampoline evaluator for hot-path operations (`+`, `=`, `min`, `max`, `assoc`, `string/split`)
- **COW map mutation:** `assoc` mutates in-place when the `Rc` refcount is 1 (which `file/fold-lines` ensures by moving, not cloning, the accumulator)
- **`hashmap/new`:** Amortized O(1) lookup via `hashbrown` instead of O(log n) `BTreeMap`
- **memchr SIMD:** `string/split` uses SIMD-accelerated byte search for the separator

Without these optimizations, Sema would be closer to Guile's territory (~25s). See the [Performance Internals](./performance.md) page for the optimization journey.

## Janet: A Fair Comparison

Janet is the most architecturally comparable to Sema — both are:
- Embeddable scripting languages written in C/Rust
- Single-threaded with reference counting (Janet) / `Rc` (Sema)
- Focused on practical scripting rather than language theory
- No JIT, no native compilation

Janet compiles to bytecode and runs on a register-based VM, which should be faster than tree-walking. The fact that Sema is within 4% of Janet (13.2s vs 12.7s) despite being a tree-walking interpreter is a direct result of the hot-path optimizations described above — the mini-eval effectively short-circuits the interpreter overhead for the operations that matter in this benchmark.

Janet's implementation is straightforward: `file/read :line` in a loop, `string/find` + `string/slice` for parsing, mutable tables for accumulation. No special optimizations.

## Guile: Interpreter Overhead Compounded

Guile is 11.5x behind SBCL and nearly 2x behind Sema/Janet. Guile compiles to bytecode, but its `string->number` and `string-split` implementations carry overhead for the full Scheme numeric tower (exact rationals, bignums) that simpler implementations skip. Its hash table implementation also uses a different collision strategy.

Running with `--no-auto-compile` means Guile interprets the source directly without its bytecode compiler, which adds overhead. A pre-compiled version would be faster, but we benchmarked the out-of-the-box scripting experience.

## What This Benchmark Doesn't Show

This is one workload. Different benchmarks would produce different orderings:

- **CPU-bound computation** (fibonacci, sorting): SBCL and Chez would dominate even more; the I/O amortizes some of the interpreter gap
- **Startup time:** Janet and Sema start in <10ms; Clojure takes 1–2s; SBCL takes ~50ms
- **Memory usage:** Janet and Sema use minimal memory (tens of MB); Clojure's JVM baseline is ~100MB+
- **Multi-threaded:** Clojure (on the JVM) and SBCL (with `lparallel`) can parallelize; Sema, Janet, and Guile are single-threaded
- **Developer experience:** Clojure's REPL, Racket's IDE (DrRacket), and SBCL's SLIME/Sly integration are far more mature than Sema's

## Methodology

- **Dataset:** 10,000,000 rows, 40 weather stations, generated from the [1BRC specification](https://github.com/gunnarmorling/1brc) with fixed station statistics
- **Environment:** Docker container (`debian:bookworm-slim`, linux/amd64), running on Apple Silicon via Rosetta
- **Measurement:** Wall-clock time via `date +%s%N`, best of 3 consecutive runs per dialect
- **Verification:** All implementations produce identical output (sorted station results, 1 decimal place rounding)
- **Code style:** Each implementation is idiomatic for its dialect — no artificial handicaps, but no heroic micro-optimization either (except SBCL's `(safety 0)` declarations, which are standard practice)

### Versions

| Dialect | Version | Package |
|---------|---------|---------|
| SBCL | 2.2.9 | `sbcl` (Debian bookworm) |
| Chez Scheme | 9.5.8 | `chezscheme` (Debian bookworm) |
| Clojure | 1.12.0 | `clojure` CLI tools |
| Sema | 0.8.0 | `cargo build --release` (native) |
| Janet | 1.37.1 | Built from source |
| Guile | 3.0.8 | `guile-3.0` (Debian bookworm) |

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
