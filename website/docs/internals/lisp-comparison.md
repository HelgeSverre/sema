# Lisp Dialect Benchmark

How does Sema compare to other Lisp dialects on a real-world I/O-heavy workload? We benchmarked fifteen implementations on the [1 Billion Row Challenge](https://github.com/gunnarmorling/1brc) — a data processing task that reads weather station measurements and computes min/mean/max per station. This is not a synthetic micro-benchmark; it exercises I/O, string parsing, hash table accumulation, and numeric aggregation in a tight loop.

## Results (Optimized)

**10 million rows (1.2 GB), best of 3 runs, single-threaded, Docker (linux/amd64):**

| Dialect           | Implementation           | Time (ms) | Relative | Category     |
| ----------------- | ------------------------ | --------- | -------- | ------------ |
| **SBCL**          | Native compiler          | 2,115     | 1.0x     | Compiled     |
| **Chez Scheme**   | Native compiler          | 2,873     | 1.4x     | Compiled     |
| **Fennel/LuaJIT** | JIT compiler             | 3,330     | 1.6x     | JIT-compiled |
| **Clojure**       | JVM (JIT)                | 5,487     | 2.6x     | JIT-compiled |
| **Gambit**        | Native compiler (gsc)    | 5,553     | 2.6x     | Compiled     |
| **Chicken**       | Native compiler (csc)    | 7,605     | 3.6x     | Compiled     |
| **PicoLisp**      | Interpreter              | 9,780     | 4.6x     | Interpreted  |
| **newLISP**       | Interpreter              | 12,330    | 5.8x     | Interpreted  |
| **Emacs Lisp**    | Bytecode VM              | 13,807    | 6.5x     | Interpreted  |
| **Janet**         | Bytecode VM              | 14,192    | 6.7x     | Interpreted  |
| **ECL**           | Native compiler          | 14,974    | 7.1x     | Compiled     |
| **Guile**         | Bytecode VM              | 15,487    | 7.3x     | Interpreted  |
| **Kawa**          | JVM (JIT)                | 16,611    | 7.9x     | JIT-compiled |
| **Gauche**        | Bytecode VM              | 23,252    | 11.0x    | Interpreted  |
| **Sema**          | Tree-walking interpreter | 48,042    | 22.7x    | Interpreted  |

Racket was excluded — both the CS (Chez Scheme) and BC (bytecode) backends crash under x86-64 emulation on Apple Silicon. This is a Docker/emulation issue, not a Racket performance issue; Racket CS would likely land between Chez and Clojure.

::: info Compiled mode
Gambit, Chicken, and ECL are now benchmarked in compiled mode (compiling to native code via C), not interpreter mode. Previous versions of this benchmark ran them as interpreters, which was 3–6x slower. Guile now runs with bytecode auto-compilation enabled.
:::

::: info Native performance
Sema runs significantly faster natively on Apple Silicon: ~29.6s (tree-walker) and ~17.1s (VM mode with `--vm`) on 10M rows, compared to 48.0s under x86-64 emulation. NaN-boxing (introduced in v1.5.0) adds overhead to the tree-walker that is amplified by x86-64 emulation. All dialects in this table were measured under the same Docker/emulation environment for a fair comparison.
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

Clojure's strength is that this code is _15 lines_ — the most concise implementation in the benchmark. It trades raw speed for developer productivity.

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

Janet compiles to bytecode and runs on a register-based VM, which should be faster than tree-walking. Under the same Docker environment, Janet is ~3.4x faster than Sema's tree-walker (14.2s vs 48.0s). However, Sema's bytecode VM mode (`--vm`) at ~17.1s natively is competitive with Janet's 14.2s under emulation.

Janet's implementation is straightforward: `file/read :line` in a loop, `string/find` + `string/slice` for parsing, mutable tables for accumulation. No special optimizations.

## Guile and Gauche: Scheme Bytecode VMs

Guile (7.2x) and Gauche (10.9x) are both R7RS-compliant Scheme implementations with bytecode VMs. Guile runs with bytecode auto-compilation enabled, which compiles source to bytecode on first execution and caches it for subsequent runs. Guile's optimized implementation uses a hand-rolled integer×10 parser, saving ~7% vs `string->number`. Gauche uses `string->number` in both versions — a hand-rolled char-by-char parser is actually _slower_ in Gauche because `string-ref` has O(n) cost in its internal UTF-32 representation, while `string->number` is implemented in C.

## Sema: The Interpreter Tax

Sema's 22.7x result (48.0s under emulation, ~29.6s native tree-walker, ~17.1s native VM) reflects the fundamental cost of tree-walking interpretation, amplified by NaN-boxing overhead under x86-64 emulation. Every operation — reading a line, splitting a string, parsing a number, updating a map — is a function call through the evaluator, with environment lookup, `Rc` reference counting, and trampoline dispatch.

The 22.7x relative result reflects tree-walking + NaN-boxing overhead amplified by x86-64 emulation on Apple Silicon. Natively, the tree-walker runs at ~29.6s, and the bytecode VM (`--vm`, available since v1.5.0) brings this down to ~17.1s — which would be competitive with Janet (14.2s Docker) and Guile (15.5s Docker).

Key optimizations that remain in the runtime:

- **`file/fold-lines`:** Reuses the lambda environment across iterations (no allocation per line)
- **COW map mutation:** `assoc` mutates in-place when the `Rc` refcount is 1 (which `file/fold-lines` ensures by moving, not cloning, the accumulator)
- **`hashmap/new`:** Amortized O(1) lookup via `hashbrown` instead of O(log n) `BTreeMap`
- **Bytecode VM:** The `--vm` flag compiles to bytecode before execution, eliminating tree-walking overhead and significantly improving performance

See the [Performance Internals](./performance.md) page for the optimization journey.

## Kawa: JVM Scheme, Slower Than Expected

Kawa at 8.1x is slower than Clojure despite both running on the JVM. Kawa compiles Scheme to JVM bytecode, but its `string->number` implementation handles the full Scheme numeric tower (exact rationals, complex numbers), which is more expensive than Clojure's `Double/parseDouble`. The `java.util.HashMap` usage should be fast, but Kawa's compilation model introduces overhead for Scheme-specific features like tail-call optimization and continuations that the JVM doesn't natively support.

## ECL: Common Lisp via C

ECL at 7.1x compiles Common Lisp to C via `compile-file`, producing a native FASL. The optimized implementation uses a hand-rolled integer×10 parser; without it (using `read-from-string` instead), ECL drops to 21.5s — a 1.4x slowdown. The remaining gap to SBCL is due to ECL's less aggressive native code generation compared to SBCL's mature optimizer.

## Emacs Lisp: Buffer-Based I/O

Emacs Lisp at 6.4x loads the entire file into a buffer with `insert-file-contents-literally`, then parses temperatures using a manual integer×10 parser that reads characters directly from the buffer without extracting substrings. Without this optimization (using `string-to-number` on extracted substrings instead), Emacs drops to 22.2s — a 1.6x slowdown. The in-buffer parsing avoids both string allocation and float overhead, which matters over 10 million rows.

## Results (Simple/Idiomatic)

To measure raw language runtime speed — independent of implementation tricks — we also benchmarked "simple" versions of each implementation. These use the language's built-in number parser (`string->number`, `string-to-number`, `tonumber`, etc.), per-line I/O, and standard data structures. No custom integer parsers, no block reads, no `(safety 0)`, no SIMD.

**10 million rows (1.2 GB), best of 3 runs, single-threaded, Docker (linux/amd64):**

| Dialect           | Implementation           | Time (ms) | Relative | vs Optimized |
| ----------------- | ------------------------ | --------- | -------- | ------------ |
| **Fennel/LuaJIT** | JIT compiler             | 3,640     | 1.0x     | 1.1x slower  |
| **Chez Scheme**   | Native compiler          | 4,320     | 1.2x     | 1.5x slower  |
| **Gambit**        | Native compiler (gsc)    | 5,714     | 1.6x     | ~same        |
| **Clojure**       | JVM (JIT)                | 5,913     | 1.6x     | ~same        |
| **SBCL**          | Native compiler          | 7,404     | 2.0x     | 3.5x slower  |
| **PicoLisp**      | Interpreter              | 9,839     | 2.7x     | ~same        |
| **newLISP**       | Interpreter              | 12,320    | 3.4x     | ~same        |
| **Chicken**       | Native compiler (csc)    | 13,371    | 3.7x     | 1.8x slower  |
| **Janet**         | Bytecode VM              | 14,406    | 4.0x     | ~same        |
| **Guile**         | Bytecode VM              | 16,628    | 4.6x     | 1.1x slower  |
| **Kawa**          | JVM (JIT)                | 16,874    | 4.6x     | ~same        |
| **ECL**           | Native compiler          | 22,961    | 6.3x     | 1.5x slower  |
| **Gauche**        | Bytecode VM              | 23,019    | 6.3x     | ~same        |
| **Emacs Lisp**    | Bytecode VM              | 24,293    | 6.7x     | 1.8x slower  |
| **Sema**          | Tree-walking interpreter | 52,060    | 14.3x    | 1.1x slower  |

The simple results are normalized to Fennel (the fastest simple implementation) rather than SBCL, since SBCL benefits the most from its optimizations.

## What the Simple Benchmarks Reveal

Comparing simple vs optimized times shows where optimization effort pays off and where the language runtime does the heavy lifting:

| Dialect      | Optimized | Simple | Slowdown | What the Optimized Version Does                                  |
| ------------ | --------- | ------ | -------- | ---------------------------------------------------------------- |
| **SBCL**     | 2,115     | 7,404  | **3.5x** | Block I/O, `(safety 0)`, custom int×10 parser, typed structs     |
| **Chicken**  | 7,605     | 13,371 | **1.8x** | Custom int×10 parser avoids `string->number` numeric tower       |
| **Emacs**    | 13,807    | 24,293 | **1.8x** | In-buffer int×10 parser avoids string extraction + float parsing |
| **Chez**     | 2,873     | 4,320  | **1.5x** | Custom char-by-char parser avoids `string->number`               |
| **ECL**      | 14,974    | 22,961 | **1.5x** | Custom int×10 parser avoids `read-from-string` (full CL reader)  |
| **Guile**    | 15,487    | 16,628 | **1.1x** | Custom int×10 parser, modest improvement                         |
| **Fennel**   | 3,330     | 3,640  | **1.1x** | Already simple — LuaJIT's JIT optimizes it                       |
| **Sema**     | 48,042    | 52,060 | **1.1x** | `string->float` + hashmap vs `string->number` + sorted map       |
| **Gambit**   | 5,553     | 5,714  | ~same    | Already uses `string->number`                                    |
| **Clojure**  | 5,487     | 5,913  | ~same    | Only transient→persistent map optimization                       |
| **Janet**    | 14,192    | 14,406 | ~same    | Already simple                                                   |
| **Kawa**     | 16,611    | 16,874 | ~same    | `Double/parseDouble` → `string->number` (similar cost)           |
| **PicoLisp** | 9,780     | 9,839  | ~same    | No floats — int×10 is the only option                            |
| **newLISP**  | 12,330    | 12,320 | ~same    | Already simple                                                   |
| **Gauche**   | 23,252    | 23,019 | ~same    | `string->number` (C impl) is faster than hand-rolled Scheme      |

**Key takeaways:**

- **SBCL's 3.5x optimization gain is the largest** — block I/O + `(safety 0)` + type declarations transform it from 2.0x to 1.0x relative. Without its optimizations, SBCL would rank 5th, behind Fennel, Chez, Gambit, and Clojure.
- **Number parsing is the dominant optimization** — every dialect that benefits from optimization does so primarily by replacing the language's built-in number parser with a hand-rolled integer×10 parser. This avoids the overhead of handling the full numeric tower, scientific notation, and float precision.
- **Fennel/LuaJIT is the fastest with zero optimization effort.** The simple and optimized versions are nearly identical — LuaJIT's tracing JIT does all the work. This makes Fennel the clear winner in "performance per line of code."
- **Gauche's `string-ref` is O(n)** — a hand-rolled char-by-char parser is actually _slower_ than `string->number` (C implementation) because Gauche uses a UTF-32 internal representation where `string-ref` must scan from the beginning.
- **Sema's optimization gain is very small** (48.0s vs 52.1s = 1.1x), because `file/fold-lines` and COW mutation work in both versions. The difference is just `string->float` + hashmap vs `string->number` + sorted map.

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

| Dialect     | Version | Package                                                |
| ----------- | ------- | ------------------------------------------------------ |
| SBCL        | 2.2.9   | `sbcl` (Debian bookworm)                               |
| Chez Scheme | 9.5.8   | `chezscheme` (Debian bookworm)                         |
| Fennel      | 1.5.1   | Downloaded binary                                      |
| LuaJIT      | 2.1.0   | `luajit` (Debian bookworm)                             |
| Clojure     | 1.12.0  | CLI tools                                              |
| PicoLisp    | 23.2    | `picolisp` (Debian bookworm)                           |
| newLISP     | 10.7.5  | `newlisp` (Debian bookworm)                            |
| Sema        | 1.5.0   | Built from source (Docker)                             |
| Janet       | 1.37.1  | Built from source                                      |
| Kawa        | 3.1.1   | JAR from Maven Central                                 |
| Gauche      | 0.9.15  | Built from source                                      |
| Guile       | 3.0.8   | `guile-3.0` (Debian bookworm)                          |
| Emacs       | 28.2    | `emacs-nox` (Debian bookworm)                          |
| Gambit      | 4.9.3   | `gambc` compiled via `gsc -exe` (Debian bookworm)      |
| ECL         | 21.2.1  | `ecl` compiled via `compile-file` (Debian bookworm)    |
| Chicken     | 5.3.0   | `chicken-bin` compiled via `csc -O3` (Debian bookworm) |

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

<script setup>
import { onMounted } from 'vue'

onMounted(() => {
  document.querySelectorAll('table tr').forEach(row => {
    const firstCell = row.querySelector('td:first-child')
    if (firstCell && firstCell.textContent.trim() === 'Sema') {
      row.classList.add('sema-row')
    }
  })
})
</script>

<style>
.sema-row {
  background: linear-gradient(90deg, rgba(245, 158, 11, 0.18), rgba(245, 158, 11, 0.06)) !important;
}
.sema-row td {
  font-weight: 600;
}
.sema-row td:first-child {
  border-left: 3px solid #f59e0b !important;
}
</style>
