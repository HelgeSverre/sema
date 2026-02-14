# Performance Internals

Sema is a tree-walking interpreter — there's no bytecode compiler or JIT. Despite that, targeted optimizations brought the [1 Billion Row Challenge](https://github.com/gunnarmorling/1brc) benchmark from **~25s to ~13s** on 10M rows (~1.9x speedup). This page documents each optimization, why it was chosen, and its measured impact.

All benchmarks were run on Apple Silicon (M-series), processing the 1BRC dataset (semicolon-delimited weather station readings, one per line).

## Benchmark Summary

| Stage | 1M rows | 10M rows | Technique |
|-------|---------|----------|-----------|
| Baseline | 2,501 ms | ~25,000 ms | Naive implementation |
| + COW assoc | 1,800 ms | ~18,000 ms | In-place map mutation |
| + Env reuse | 1,626 ms | 16,059 ms | Lambda env recycling |
| + String interning | 1,400 ms | ~14,000 ms | Spur-based dispatch |
| + hashbrown | 1,340 ms | ~13,400 ms | Amortized O(1) accumulator |
| **Total speedup** | **1.87x** | **~1.87x** | |

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

## 2. Lambda Environment Reuse

**Problem:** `file/fold-lines` and `file/for-each-line` are called with a lambda that executes once per line. The normal function dispatch creates a new `Env`, interns parameter names, and allocates a line `String` on every iteration.

**Solution:** For simple lambdas (known arity, no rest params), create the environment *once* and reuse it:

```rust
// crates/sema-stdlib/src/io.rs — file/fold-lines fast path
let lambda_env = Env::with_parent(Rc::new(lambda.env.clone()));
let param0_spur = intern(&lambda.params[0]);  // intern once
let param1_spur = intern(&lambda.params[1]);
lambda_env.set(param0_spur, Value::Nil);       // pre-populate
lambda_env.set(param1_spur, Value::Nil);
let mut line_buf = String::with_capacity(64);  // reuse buffer

loop {
    line_buf.clear();                          // reuse, don't reallocate
    reader.read_line(&mut line_buf)?;
    lambda_env.update(param0_spur, acc);       // overwrite in place
    lambda_env.update(param1_spur, Value::string(&line_buf));
    // ...
}
```

Three wins stacked:
1. **Env allocation:** 1 instead of N (one per line)
2. **String interning:** parameter names interned once, not per-call
3. **Line buffer:** `read_line` into a reusable `String` instead of `lines()` iterator which allocates per line

**Impact:** ~15% speedup (2,501ms → 1,626ms combined with COW assoc).

**Literature:**
- This is a manual application of escape analysis — the JVM's JIT does this automatically when it proves an allocation doesn't escape a loop
- LuaJIT's trace compiler applies similar optimizations to eliminate per-iteration allocations
- CPython's `__slots__` is a distant analogy (pre-allocated attribute storage vs. per-instance dict), though the mechanism is different

## 3. Mini-Eval (Inlined Hot-Path Builtins)

**Problem:** The full trampoline evaluator adds overhead per expression: call stack management, span tracking for error reporting, and trampoline dispatch (wrapping/unwrapping `Trampoline::Value` and `Trampoline::Eval`). For tight inner loops, this is ~4x slower than necessary.

**Solution:** `sema-stdlib` contains its own minimal evaluator (`sema_eval_value`) that handles common forms directly via recursive calls. It also inlines the most frequently called builtins — `+`, `=`, `assoc`, `get`, `min`, `max`, `first`, `nth`, `nil?`, `float`, `string/split`, `string->number` — to skip `Env` lookup and `NativeFn` dispatch entirely.

```rust
// crates/sema-stdlib/src/list.rs — inlined `+` in mini-eval
} else if head_spur == sf.plus {
    if items.len() == 3 {
        let a = sema_eval_value(&items[1], env)?;
        let b = sema_eval_value(&items[2], env)?;
        return match (&a, &b) {
            (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
            // ...
        };
    }
}
```

This also exists for a structural reason: `sema-stdlib` cannot depend on `sema-eval` (it would create a circular dependency), so it needs a local evaluator for builtins like `file/fold-lines` that invoke user-provided lambdas.

**Impact:** Using the full evaluator via callback measured 6,189ms for 1M rows vs. 1,626ms with the mini-eval — **3.8x overhead**.

**Literature:**
- Inline caching, pioneered by Smalltalk-80 and refined in V8's hidden classes, solves the same dispatch overhead problem but at a different architectural level
- Most production Lisps (SBCL, Chez Scheme) compile to native code, making dispatch overhead negligible — Sema's approach is specific to tree-walking interpreters
- Lua 5.x uses a similar technique: the VM inlines common operations (`OP_ADD`, `OP_GETTABLE`) into the bytecode dispatch loop

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

## 6. SIMD Byte Search (memchr)

**Problem:** `string/split` scans for separator bytes using a linear byte-by-byte search.

**Solution:** Use the [memchr](https://crates.io/crates/memchr) crate which provides SIMD-accelerated (SSE2/AVX2/NEON) byte search. Also added a two-part split fast path for the common case of exactly one separator occurrence (e.g., `"Berlin;12.3"` → `["Berlin", "12.3"]`), avoiding the `Vec` allocation from the full `split()` iterator.

```rust
// crates/sema-stdlib/src/list.rs — inlined string/split
if sep.len() == 1 {
    let sep_byte = sep.as_bytes()[0];
    if let Some(pos) = memchr::memchr(sep_byte, s.as_bytes()) {
        let left = &s[..pos];
        let right = &s[pos + 1..];
        if memchr::memchr(sep_byte, right.as_bytes()).is_some() {
            // 3+ parts: fall through to full split
            let parts: Vec<Value> = s.split(sep).map(Value::string).collect();
            return Ok(Value::list(parts));
        }
        // Exactly 2 parts: avoid Vec/iterator overhead
        return Ok(Value::list(vec![Value::string(left), Value::string(right)]));
    }
}
```

**Impact:** Negligible on 1BRC specifically (strings are 10–30 bytes; too short for SIMD to outperform scalar). The two-part split fast path is the bigger win here, avoiding iterator/Vec overhead. More significant for longer strings or multi-megabyte text processing.

**Literature:**
- memchr is maintained by Andrew Gallant (BurntSushi), author of ripgrep. It uses the [generic SIMD](https://github.com/BurntSushi/memchr/blob/master/src/arch/all/memchr/mod.rs) framework to dispatch to the best available instruction set at runtime
- The algorithm is based on the classic `memchr(3)` libc function, extended with SIMD by exploiting the fact that byte-equality checks vectorize trivially

## 7. Custom Number Parser

**Problem:** Rust's `str::parse::<f64>()` handles scientific notation, infinity, NaN, hex floats, and edge cases. The 1BRC data only contains simple decimals like `"-12.3"` or `"4.5"`.

**Solution:** A hand-rolled parser that handles only `[-]digits[.digits]`, returning `None` for anything more complex so callers fall back to the standard parser. Uses a precomputed powers-of-10 lookup table for 1–4 fractional digits.

```rust
// crates/sema-stdlib/src/list.rs
static POWERS: [f64; 5] = [1.0, 10.0, 100.0, 1000.0, 10000.0];
let divisor = if frac_digits < POWERS.len() {
    POWERS[frac_digits]  // table lookup, no powi()
} else {
    10f64.powi(frac_digits as i32)  // fallback
};
```

**Impact:** Part of the combined mini-eval speedup. Difficult to isolate, but avoids the overhead of Rust's [dec2flt](https://github.com/rust-lang/rust/tree/master/library/core/src/num/dec2flt) algorithm which handles the full IEEE 754 spec.

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
| **Full evaluator callback** | 4x slower | Trampoline dispatch, call stack management, and span tracking dominate at ~6,200ms vs 1,600ms for the mini-eval. |
| **im-rc / rpds (persistent collections)** | Slower | Structural sharing fights the COW optimization — the whole point is to *avoid* sharing and mutate in place when refcount is 1. |
| **bumpalo / typed-arena** | Incompatible | Values need to escape the arena (returned from functions, stored in environments). Arena allocation only works for temporaries. |
| **compact_str / smol_str** | Redundant | Once symbols/keywords are interned as `Spur`, small-string optimization for them is pointless. String *values* are still `Rc<String>` but they're not in the hot path for dispatch. |

## Architecture Diagram

The hot path for `file/fold-lines` with all optimizations:

```
file/fold-lines
  ├── BufReader (256KB buffer, reused line_buf)
  ├── Lambda env created ONCE, params interned ONCE
  └── Per-line loop:
        ├── read_line → reused buffer (no alloc)
        ├── env.update() → overwrite binding in place (no alloc)
        ├── mini-eval → direct recursive eval (no trampoline)
        │     ├── string/split → memchr SIMD + two-part fast path
        │     ├── string->number → custom decimal parser
        │     ├── assoc → COW in-place mutation (Rc refcount == 1)
        │     └── +, =, min, max → inlined (no Env lookup)
        └── acc moved, not cloned → preserves refcount == 1
```
