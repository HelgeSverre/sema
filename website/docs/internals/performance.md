# Performance Internals

Sema is a tree-walking interpreter — there's no bytecode compiler or JIT. Early optimizations brought the [1 Billion Row Challenge](https://github.com/gunnarmorling/1brc) benchmark from **~25s to ~9.6s** on 10M rows using a "mini-eval" — a minimal evaluator inlined in the stdlib that bypassed the full trampoline. The mini-eval was later **removed** for architectural reasons (semantic drift from the real evaluator, and blocking the path to a bytecode VM). Fast-path optimizations in the real evaluator partially recovered performance, bringing the current benchmark to **~2,600ms on 1M rows** (vs ~960ms with the mini-eval). This page documents each optimization, its history, and measured impact.

All benchmarks were run on Apple Silicon (M-series), processing the 1BRC dataset (semicolon-delimited weather station readings, one per line).

## Benchmark Summary

| Stage | 1M rows | 10M rows | Technique | Status |
|-------|---------|----------|-----------|--------|
| Baseline | 2,501 ms | ~25,000 ms | Naive implementation | — |
| + COW assoc | 1,800 ms | ~18,000 ms | In-place map mutation | ✅ Active |
| + Env reuse | 1,626 ms | 16,059 ms | Lambda env recycling (mini-eval) | ❌ Removed |
| + Mini-eval | ~960 ms | ~9,600 ms | Inlined builtins, custom parser | ❌ Removed |
| + String interning | — | — | Spur-based dispatch | ✅ Active |
| + hashbrown | — | — | Amortized O(1) accumulator | ✅ Active |
| **Post-removal** | **~2,600 ms** | — | Callback architecture + fast paths | ✅ Current |

> **Note:** The mini-eval and its associated optimizations (env reuse, inlined builtins, custom number parser, SIMD split fast path) were removed to unblock the bytecode VM. The current architecture uses `sema_core::call_callback` to route stdlib → real evaluator. Fast-path optimizations (self-evaluating short-circuit, inline NativeFn dispatch, thread-local EvalContext, deferred cloning) partially recovered performance.

## 1. Copy-on-Write Map Mutation

**Problem:** Every `(assoc map key val)` call cloned the entire `BTreeMap`, even when no other reference existed. For the 1BRC accumulator (~400 weather stations), this was O(400) per row × millions of rows.

**Solution:** Use `Rc::try_unwrap` to check if the reference count is 1. If so, take ownership and mutate in place. Otherwise, clone.

```rust
// crates/sema-stdlib/src/map.rs
match Rc::try_unwrap(m) {
    Ok(map) => map,       // refcount == 1: we own it, mutate in place
    Err(m) => m.as_ref().clone(),  // shared: must clone
}
```

The key insight is pairing this with `Env::take()` — by *removing* the accumulator from the environment before passing it to `assoc`, the refcount drops to 1, enabling the in-place path. User code looks like:

```scheme
(file/fold-lines "data.csv"
  (lambda (acc line)
    (let ((parts (string/split line ";")))
      (assoc acc (first parts) (second parts))))
  {})
```

The `fold-lines` implementation moves (not clones) `acc` into the lambda env on each iteration, keeping the refcount at 1.

**Impact:** ~30% of the total speedup. Eliminated the O(n) full-map clone, leaving only the O(log n) BTreeMap insert per row.

**Literature:**
- This is the same copy-on-write strategy used by Swift's value types. (Clojure's persistent data structures solve a related problem — avoiding full copies — but via structural sharing rather than refcount-based COW.)
- Phil Bagwell, ["Ideal Hash Trees"](https://infoscience.epfl.ch/entities/publication/64410) (2001) — the paper behind Clojure/Scala persistent collections
- Rust's `Rc::make_mut` provides the same semantics with less ceremony

## 2. Lambda Environment Reuse *(removed)*

> **Status:** This optimization was part of the mini-eval's hot path in `io.rs`. It was removed when the mini-eval was deleted. The current `file/fold-lines` uses `sema_core::call_callback`, which routes through the real evaluator — each call creates a fresh `Env` via the standard `apply_lambda` path.

**What it was:** For simple lambdas (known arity, no rest params), the mini-eval created the lambda environment *once* and reused it across all iterations, overwriting bindings in place. Combined with a reusable `line_buf`, this eliminated per-iteration allocations for `Env`, string interning, and line buffers.

**Why it was removed:** The env reuse logic was tightly coupled to the mini-eval's direct lambda dispatch. The callback architecture routes through the real evaluator's `apply_lambda`, which always creates a fresh child `Env` — this is correct and avoids subtle bugs from env mutation leaking across calls.

**Impact when active:** ~15% speedup (2,501ms → 1,626ms combined with COW assoc).

**What remains:** The reusable `line_buf` (`String::with_capacity(64)` cleared each iteration) is still present in `file/fold-lines` — only the env reuse was lost.

## 3. Evaluator Callback Architecture *(replacing Mini-Eval)*

> **Status:** The mini-eval was deleted and replaced with a callback architecture. Stdlib now calls the real evaluator via `sema_core::call_callback`.

**What the mini-eval was:** `sema-stdlib` previously contained its own minimal evaluator (`sema_eval_value`) that handled common forms via direct recursive calls, inlining builtins like `+`, `=`, `assoc`, `string/split`, and `string->number` to skip `Env` lookup and `NativeFn` dispatch entirely.

**Why it was removed:**
1. **Semantic drift:** The mini-eval diverged from the real evaluator — new special forms, error handling, and features had to be duplicated or were silently missing.
2. **Blocking bytecode VM:** A bytecode compiler can't target two evaluators. Removing the mini-eval ensures a single evaluation path that the VM can replace.

**The callback architecture:** `sema-stdlib` cannot depend on `sema-eval` (circular dependency). Instead, `sema-eval` registers a thread-local callback (`set_call_callback`) at startup, and stdlib functions call `sema_core::call_callback` to invoke the real evaluator. A thread-local `EvalContext` (`with_stdlib_ctx`) is shared across calls to avoid per-call context allocation.

```rust
// crates/sema-stdlib/src/io.rs — file/fold-lines via callback
sema_core::with_stdlib_ctx(|ctx| {
    let mut line_buf = String::with_capacity(64);
    loop {
        line_buf.clear();
        let n = reader.read_line(&mut line_buf)?;
        if n == 0 { break; }
        // Calls the real evaluator (eval_value) via thread-local callback
        acc = sema_core::call_callback(ctx, &func, &[acc, Value::string(&line_buf)])?;
    }
    Ok(acc)
})
```

**Performance trade-off:** ~960ms → ~2,600ms on 1M rows (~2.7× regression). The overhead comes from the full trampoline evaluator: call stack management, span tracking, and `Trampoline` dispatch on every sub-expression.

**Fast-path optimizations that partially recovered performance:**
1. **Self-evaluating fast path:** `eval_value` short-circuits for integers, floats, strings, keywords, and symbols — skipping depth tracking and step limits for the most common forms.
2. **Inline NativeFn dispatch:** When the evaluator sees a `Value::NativeFn` in call position, it calls the function pointer directly without going through `call_callback` indirection.
3. **Thread-local shared EvalContext:** `with_stdlib_ctx` reuses a single `EvalContext` across all stdlib → evaluator callbacks, avoiding per-call allocation of `RefCell`/`Cell` fields.
4. **Deferred cloning:** `eval_value_inner` avoids cloning the expression and environment on the first trampoline iteration, only cloning if a tail call (`Trampoline::Eval`) is returned.

**Remaining gap:** The ~2.7× regression cannot be fully closed within the tree-walking architecture. A bytecode VM — the reason the mini-eval was removed — is the planned path to recover and exceed the original performance.

**Literature:**
- Inline caching, pioneered by Smalltalk-80 and refined in V8's hidden classes, solves the same dispatch overhead problem but at a different architectural level
- Most production Lisps (SBCL, Chez Scheme) compile to native code, making dispatch overhead negligible — Sema's callback overhead is inherent to tree-walking interpreters
- Lua 5.x's bytecode VM inlines common operations (`OP_ADD`, `OP_GETTABLE`) into the dispatch loop — this is the approach Sema's planned VM will take

## 4. String Interning (lasso)

**Problem:** Symbol/keyword equality was O(n) string comparison. Environment lookups keyed by `String` required comparing the full string on each `BTreeMap` node visit. Special form dispatch compared against 30+ string literals on every list evaluation.

**Solution:** Replace `Rc<String>` in `Value::Symbol` and `Value::Keyword` with `Spur` — a `u32` handle from the [lasso](https://crates.io/crates/lasso) string interner. Environment bindings keyed by `Spur` for direct integer lookup.

```rust
// Before: O(n) string comparison
Value::Symbol(Rc<String>)
env: BTreeMap<String, Value>

// After: O(1) integer comparison
Value::Symbol(Spur)  // u32
env: BTreeMap<Spur, Value>
```

Special form dispatch uses pre-cached `Spur` constants:

```rust
// crates/sema-eval/src/special_forms.rs
struct SpecialFormSpurs {
    quote: Spur,
    if_: Spur,
    define: Spur,
    // ... 30 more
}

// Dispatch: integer comparison, no string resolution
if head_spur == sf.if_ {
    return Some(eval_if(args, env));
}
```

**Caveat:** The initial implementation was actually *slower* (2,518ms vs 1,580ms baseline) because `resolve()` was allocating a new `String` on every symbol lookup. Fixed by adding `with_resolved(spur, |s| ...)` which provides a borrowed `&str` without allocation, and switching `Env` to use `Spur` keys directly.

**Impact:** 1,580ms → 1,400ms (11% faster) after fixing the allocation issue.

**Literature:**
- String interning is as old as Lisp itself — McCarthy's original LISP 1.5 (1962) interned atoms in the "object list" (oblist)
- Java interns all string literals and provides `String.intern()`. The JVM's `invokedynamic` uses interned method names for O(1) dispatch
- The [string-interner](https://crates.io/crates/string-interner) and [lasso](https://crates.io/crates/lasso) crates are the two main Rust options; lasso was chosen for its `Rodeo` thread-local interner which fits Sema's single-threaded architecture

## 5. hashbrown HashMap

**Problem:** The 1BRC accumulator uses a map keyed by weather station name (~400 entries). `BTreeMap` provides O(log n) lookup, but the accumulator is accessed on every row. With 10M rows, the log₂(400) ≈ 9 comparisons per lookup adds up.

**Solution:** Added a `Value::HashMap` variant backed by [hashbrown](https://crates.io/crates/hashbrown) (the same hash map used inside Rust's `std::collections::HashMap`, but exposed directly for `no_std` compatibility and raw API access).

```scheme
;; User code: opt into HashMap for the accumulator
(file/fold-lines "data.csv"
  (lambda (acc line) ...)
  (hashmap/new))  ; amortized O(1) vs O(log n)

;; Convert back to sorted BTreeMap for output
(hashmap/to-map acc)
```

`BTreeMap` remains the default for `{}` map literals because deterministic ordering matters for equality, printing, and test assertions. `hashbrown` is opt-in for performance-critical paths.

**Impact:** 1,400ms → 1,340ms (4% faster). Modest because BTreeMap with 400 entries and short string keys is already fast.

**Literature:**
- hashbrown uses SwissTable, designed by Google for their C++ `absl::flat_hash_map`. See [CppCon 2017: Matt Kulukundis "Designing a Fast, Efficient, Cache-friendly Hash Table"](https://www.youtube.com/watch?v=ncHmEUmJZf4)
- Clojure's `{:key val}` maps use HAMTs (hash array mapped tries) which provide O(~1) lookup with structural sharing. Sema's approach is simpler: full COW on the `Rc<HashMap>` rather than structural sharing, which is viable because the refcount-1 fast path almost always hits

## 6. SIMD Byte Search (memchr) *(removed)*

> **Status:** The memchr-based two-part split fast path was part of the mini-eval's inlined `string/split` and was removed with it. The current `string/split` in `sema-stdlib/src/string.rs` uses Rust's standard `str::split()` followed by `map` and `collect`. The `memchr` crate remains a dependency of `sema-stdlib` but is no longer used in the split hot path.

**What it was:** A SIMD-accelerated (SSE2/AVX2/NEON) byte search via the [memchr](https://crates.io/crates/memchr) crate, combined with a two-part split fast path that avoided `Vec` allocation when splitting on a single-byte separator with exactly one occurrence (the common case in 1BRC: `"Berlin;12.3"` → `["Berlin", "12.3"]`).

**Impact when active:** Negligible for SIMD specifically (1BRC strings are 10–30 bytes), but the two-part fast path avoided iterator/Vec overhead.

**Literature:**
- memchr is maintained by Andrew Gallant (BurntSushi), author of ripgrep. It uses the [generic SIMD](https://github.com/BurntSushi/memchr/blob/master/src/arch/all/memchr/mod.rs) framework to dispatch to the best available instruction set at runtime

## 7. Custom Number Parser *(removed)*

> **Status:** This was part of the mini-eval's inlined `string->number` and was removed with it. The current `string->number` in `sema-stdlib/src/string.rs` uses Rust's standard `str::parse::<i64>()` with fallback to `str::parse::<f64>()`.

**What it was:** A hand-rolled decimal parser that handled only `[-]digits[.digits]`, using a precomputed powers-of-10 lookup table for 1–4 fractional digits. It returned `None` for complex cases (scientific notation, infinity, NaN), falling back to the standard parser.

**Impact when active:** Part of the combined mini-eval speedup. Difficult to isolate, but avoided the overhead of Rust's [dec2flt](https://github.com/rust-lang/rust/tree/master/library/core/src/num/dec2flt) algorithm.

**Literature:**
- Rust's float parser is based on the [Eisel-Lemire algorithm](https://nigeltao.github.io/blog/2020/eisel-lemire.html) (2020), which is fast for a general-purpose parser but still does more work than necessary for simple decimals
- Daniel Lemire's [fast_float](https://github.com/fastfloat/fast_float) C++ library (and its Rust port) takes a similar "fast path for common cases" approach

## 8. Enlarged I/O Buffer

**Problem:** `BufReader`'s default 8KB buffer means frequent syscalls for large files.

**Solution:** 256KB buffer for `file/fold-lines`.

```rust
let mut reader = std::io::BufReader::with_capacity(256 * 1024, file);
```

**Impact:** Minor. CPU was the bottleneck, not I/O. But it's a free win — larger buffers amortize syscall overhead and improve sequential read throughput on modern SSDs.

## Rejected Optimizations

Not everything we tried worked:

| Approach | Result | Why |
|----------|--------|-----|
| **HashMap for Env** | Slower | `BTreeMap` is faster for the very small maps (1–3 entries) typical of `let` scopes. HashMap's hashing overhead exceeds BTreeMap's few integer comparisons at that size. |
| **im-rc / rpds (persistent collections)** | Slower | Structural sharing fights the COW optimization — the whole point is to *avoid* sharing and mutate in place when refcount is 1. |
| **bumpalo / typed-arena** | Incompatible | Values need to escape the arena (returned from functions, stored in environments). Arena allocation only works for temporaries. |
| **compact_str / smol_str** | Redundant | Once symbols/keywords are interned as `Spur`, small-string optimization for them is pointless. String *values* are still `Rc<String>` but they're not in the hot path for dispatch. |

> **Note:** "Full evaluator callback" was previously listed here as rejected (4x slower than mini-eval). It is now the **current architecture** — the ~2.7× overhead vs the mini-eval is accepted as the cost of architectural correctness. A bytecode VM is planned to eliminate this overhead entirely.

## Architecture Diagram

The hot path for `file/fold-lines` with the current callback architecture:

```
file/fold-lines
  ├── BufReader (256KB buffer, reused line_buf)
  └── Per-line loop:
        ├── read_line → reused buffer (no alloc)
        ├── call_callback → real evaluator (eval_value)
        │     ├── self-evaluating fast path (ints, floats, strings skip depth tracking)
        │     ├── NativeFn inline dispatch (direct call, no callback indirection)
        │     ├── apply_lambda → fresh Env per call (no env reuse)
        │     ├── string/split → std str::split (no SIMD fast path)
        │     ├── string->number → std parse::<i64> / parse::<f64>
        │     └── assoc → COW in-place mutation (Rc refcount == 1)
        ├── thread-local EvalContext (shared, not per-call)
        └── acc moved, not cloned → preserves refcount == 1
```
