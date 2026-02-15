# Data Structure & Zero-Copy Crate Investigation

> **Status:** Research only — no code changes yet. Findings for review before implementation.

**Goal:** Evaluate whether popular Rust data structure / zero-copy crates could provide meaningful performance gains for Sema's interpreter hot path (1BRC benchmark: ~1620ms for 1M rows).

**Current bottleneck profile (per row, ~1.6µs):**
- 3× `Env::with_parent` scope creations (BTreeMap + Rc + RefCell alloc)
- ~15× `env.get()` symbol lookups (BTreeMap linear/tree search on small maps)
- 1× `string/split` → 2 `Rc<String>` allocations (station name + temperature string)
- 1× number parse
- 2+× `BTreeMap` lookup/insert on the accumulator (~400 stations)
- 4-key stats sub-map creation/update per row

---

## Tier 1: High-Impact Candidates

### 1. `lasso` — String Interning (⭐ Highest ROI)

**What it does:** Associates each unique string with a `Spur` (u32 integer key). O(1) intern + resolve. Single-threaded `Rodeo` interner uses `Rc`-compatible internals.

**What it speeds up:**
- **Station name dedup:** 400 unique stations across 1M rows. Currently each row allocates a new `Rc<String>` for the station name. With interning, only 400 allocations total; per-row cost becomes a hash lookup returning a `Copy` integer.
- **Keyword comparisons:** Keywords like `:min`, `:max`, `:sum`, `:count` used as BTreeMap keys. Currently compared by full string content. With interning, comparison is a single `u32 == u32`.
- **Symbol resolution:** `env.get("acc")`, `env.get("line")` etc. Currently matches `String` keys. With interned symbols, matching is integer comparison.

**Estimated improvement:** **20–40%** on the 1BRC hot path.

**Integration difficulty:** **Medium (2–3 days)**
- Add `lasso` dependency to `sema-core`.
- Option A (incremental): Intern only keywords + symbols used as map keys. Keep `Rc<String>` for general `Value::String`.
- Option B (full): Add `Value::InternedString(Spur)` variant or change `Value::Keyword`/`Value::Symbol` to store `Spur`.

**Compatibility:**
- `Spur` implements `Copy`, `Clone`, `Eq`, `Ord`, `Hash` — ✅ works with BTreeMap keys.
- Single-threaded `Rodeo` — ✅ compatible with `Rc`-based design.
- Would need a thread-local or globally-accessible `Rodeo` (consistent with existing `thread_local!` pattern for LLM provider, module cache).

**Gotchas:**
- Don't intern arbitrary user strings (file contents, large strings) — only identifiers/keywords/station-like tokens.
- Must define stable `Ord` for `Spur` (it's by integer value, not lexicographic — changes BTreeMap iteration order if used as map keys).
- Memory is never freed from the interner (acceptable for interpreter lifetime).

**Verdict: ✅ RECOMMENDED — Best single optimization. Clear, bounded integration.**

---

### 2. `hashbrown` + `rustc-hash`/`ahash` — Fast HashMap for Accumulator

**What it does:** Drop-in replacement for `std::HashMap` with better performance. `hashbrown` is the backing implementation of `std::HashMap` but exposes the raw entry API. Combined with a fast hasher like `ahash` (default in hashbrown) or `FxHash` (rustc-hash), it significantly outperforms BTreeMap for lookup-heavy workloads.

**What it speeds up:**
- **Accumulator map:** The 1BRC accumulator has ~400 station entries. BTreeMap does O(log 400) ≈ 9 comparisons per lookup with string keys. HashMap with a fast hasher does O(1) amortized with ~1 comparison.
- **Stats sub-map:** Could also benefit, but with only 4 keys the difference is smaller.

**Estimated improvement:** **10–25%** on the accumulator lookups/inserts.

**Integration difficulty:** **Medium (1–2 days for hot-path only)**
- Option A: Use `hashbrown::HashMap` only in the 1BRC fast path (specialized native fold).
- Option B: Add `Hash` impl to `Value` and change `Value::Map` to use `HashMap` globally.

**Compatibility:**
- Requires `Hash` on `Value` keys — currently only `Eq + Ord`. Adding `Hash` is straightforward (hash the discriminant + inner value).
- If combined with interning, keys become `Spur` which already implements `Hash` — optimal.

**Gotchas:**
- Changing `Value::Map` globally from BTreeMap to HashMap changes iteration order (non-deterministic). This breaks sorted output semantics and potentially tests.
- Better approach: Keep BTreeMap for user-visible maps, use HashMap internally in hot paths.
- `serde_json` uses BTreeMap and found it faster than HashMap for small maps with few keys (< 10) — so for the 4-key stats sub-map, BTreeMap may actually be faster.

**Verdict: ✅ RECOMMENDED — Especially powerful when combined with interning (integer keys + fast hash). Best applied to the ~400-entry accumulator, not the 4-key stats maps.**

---

### 3. `memchr` — Fast Delimiter Search

**What it does:** SIMD-accelerated byte search. Finds delimiter positions 2-8x faster than `str::split` or manual byte iteration.

**What it speeds up:**
- **Line splitting:** Currently uses `str::split(sep)` or manual byte search in the inlined `string/split`. `memchr` provides a single function call that's SIMD-optimized.
- Could enable zero-copy line parsing: find `;` position, take `&str` slices without allocating.

**Estimated improvement:** **5–10%** on line parsing.

**Integration difficulty:** **Small (few hours)**
- Add `memchr` dependency to `sema-stdlib`.
- Replace manual byte search in `string/split` inlined builtin with `memchr::memchr(b';', bytes)`.

**Compatibility:** ✅ No conflicts. Pure utility crate.

**Gotchas:** Minimal. Already doing manual byte search — `memchr` is strictly better.

**Verdict: ✅ RECOMMENDED — Easy win, minimal risk, small but real improvement.**

---

## Tier 2: Moderate-Impact Candidates

### 4. `compact_str` / `smol_str` — Small String Optimization

**What they do:** Store strings ≤ 22-24 bytes inline (on stack/in struct) instead of heap-allocating.

| Crate | Inline capacity | Clone cost (inline) | Mutable | `Option` optimized |
|-------|----------------|---------------------|---------|-------------------|
| `compact_str` | 24 bytes | O(n) memcpy | Yes | Yes |
| `smol_str` | 23 bytes | O(1) Rc-like | No | Yes |
| `smartstring` | 23 bytes | O(n) memcpy | Yes | No |
| `ecow` | ~22 bytes | O(1) COW | COW | Yes |

**What they speed up:**
- Eliminating heap allocation for small strings (identifiers, keywords, station names).
- Most Sema symbols/keywords are < 24 bytes: `:min`, `:max`, `:sum`, `:count`, `acc`, `line`, `name`, `temp`, `existing`, etc.

**Estimated improvement:**
- If replacing `Rc<String>` in `Value::{String,Symbol,Keyword}`: **5–15%** from reduced allocations.
- If keeping `Rc<String>` wrapper: **0–5%** (still heap-allocating the Rc).

**Integration difficulty:** **Large (3–5 days)**
- Must change `Value::String(Rc<String>)` → `Value::String(CompactString)` (or similar).
- All constructors, accessors, Display, Eq, Ord impls need updating.
- `compact_str` clone is O(n) memcpy — cheaper than heap alloc but more expensive than Rc clone. For values cloned many times, this could be worse.
- `smol_str` has O(1) clone (uses Arc internally) but is immutable and uses atomics — conflicts with single-threaded Rc philosophy.

**Compatibility:**
- `compact_str`: Implements `Eq`, `Ord`, `Hash` ✅
- `smol_str`: Implements `Eq`, `Ord`, `Hash` ✅ but uses `Arc` internally (atomic refcount)
- Would need to benchmark whether inline clone cost beats Rc clone cost for the interpreter's clone-heavy patterns.

**Gotchas:**
- **If interning is adopted (Tier 1), small string optimization becomes largely redundant** for symbols/keywords — they'd be `Spur` integers, not strings at all.
- Still useful for `Value::String` (user-created strings), but those are less hot.
- Large refactoring cost for uncertain gain.

**Verdict: ⚠️ CONDITIONAL — Only if NOT doing string interning. If interning is adopted, the benefit is marginal and doesn't justify the refactoring cost.**

---

### 5. `smallvec` / `tinyvec` — Stack-Allocated Small Vectors

**What they do:** Store up to N elements inline, spill to heap if exceeded.

**What they speed up:**
- **Env bindings:** Currently `BTreeMap<String, Value>` — could replace with `SmallVec<[(String, Value); 4]>` for linear search on tiny scopes.
- **Split results:** 2-element lists from `string/split` could use `SmallVec<[Value; 2]>`.
- **Call args:** Temporary arg vectors in `call_function` and `sema_eval_value`.

**Estimated improvement:**
- For `Env` bindings: **5–15%** if env scopes are the bottleneck (1–3 entries, linear search beats tree).
- For split results: **minimal** — still wrapped in `Rc<Vec<Value>>` for Lisp semantics.

**Integration difficulty:** **Medium (1–2 days for Env only)**
- Replace `BTreeMap<String, Value>` with `SmallVec<[(String, Value); 4]>` in `Env::bindings`.
- Change `get()`, `set()`, `update()`, `take()` to linear search.

**Compatibility:**
- `SmallVec` itself doesn't implement `Ord` but that's not needed for Env bindings.
- With interning, env keys become `Spur` (4 bytes, Copy) — linear search becomes trivially fast.

**Gotchas:**
- You already tried HashMap for Env and it was slower than BTreeMap for tiny scopes. SmallVec with linear search could be different (no hashing overhead), but needs benchmarking.
- Size of `SmallVec<[(String, Value); 4]>` on stack could be large (~256+ bytes) — may hurt if Env is cloned frequently.

**Verdict: ⚠️ WORTH BENCHMARKING — Best combined with interning (SmallVec<[(Spur, Value); 4]> would be much smaller). Moderate effort, uncertain gain without profiling.**

---

## Tier 3: Low-Impact or Poor Fit

### 6. `im-rc` / `rpds` — Persistent Immutable Data Structures

**What they do:** Provide maps/vectors with structural sharing — "cloning" a map is O(1), updates create new versions sharing unchanged nodes.

**Why they DON'T fit:**
- The 1BRC hot path **mutates** the accumulator map every row. Persistent maps allocate new tree nodes on every insert — directly fighting your existing COW optimization (`Rc::make_mut` for in-place mutation when refcount = 1).
- `im-rc::OrdMap` benchmarks show **2-3x slower** per operation than `BTreeMap` for small maps.
- Structural sharing shines when you keep old versions around — 1BRC doesn't.

**Estimated improvement:** **Negative (slower)**

**Verdict: ❌ NOT RECOMMENDED — Actively harmful for the mutation-heavy hot path.**

---

### 7. `bumpalo` / `typed-arena` — Arena Allocators

**What they do:** Batch allocations into a single arena; all freed at once when arena is dropped.

**Why they're a poor fit:**
- Sema's `Value` type is `'static` owned data via `Rc`. Arena-allocated values have a lifetime tied to the arena — can't be stored in `Value` without making it lifetime-parameterized (massive refactor).
- The hot path creates values that **escape** into the accumulator map (they survive across rows). Arena allocation only helps for truly temporary values.
- Could theoretically help for per-row temporaries (split result strings that get immediately parsed), but the integration cost is high for small gain.

**Estimated improvement:** **0–5%** (only for temporaries that don't escape)

**Integration difficulty:** **Large (lifetime infection throughout Value type)**

**Verdict: ❌ NOT RECOMMENDED — Wrong allocation pattern. The values that cost the most to allocate are the ones that persist.**

---

### 8. `indexmap` — Ordered HashMap

**What it does:** HashMap with insertion-order iteration.

**Why it's a poor fit:**
- Slower than `hashbrown` for lookups (extra indirection for order maintenance).
- Ordering semantics differ from BTreeMap (insertion order vs. sorted).
- No clear advantage over either BTreeMap (which gives sorted order) or hashbrown (which gives speed).

**Verdict: ❌ NOT RECOMMENDED — Neither fastest nor most compatible.**

---

### 9. `bytes` — Zero-Copy Buffer Type

**What it does:** Reference-counted byte buffer with zero-copy slicing.

**Why it's a poor fit:**
- Sema works with `String`/`&str`, not `Bytes`.
- Would require redesigning IO around `BytesMut` buffers — massive change for marginal benefit.
- `memchr` provides the useful part (fast search) without the buffer redesign.

**Verdict: ❌ NOT RECOMMENDED — Too invasive for the benefit.**

---

## Recommended Implementation Order

Based on ROI (impact / effort), here's the suggested sequence:

| Priority | Crate | Est. Improvement | Effort | Dependencies |
|----------|-------|-----------------|--------|-------------|
| **1** | `memchr` | 5–10% | S (hours) | None |
| **2** | `lasso` (keyword/symbol interning) | 20–40% | M (2–3 days) | None |
| **3** | `hashbrown` + `ahash` (accumulator) | 10–25% | M (1–2 days) | Better with #2 |
| **4** | `smallvec` (Env bindings) | 5–15% | M (1–2 days) | Better with #2 |

**Combined estimated improvement: 35–60%** (not all additive, but significant)

**Target: ~1000ms for 1M rows** (from current ~1620ms)

### What NOT to do:
- Don't switch to `im-rc` persistent maps — they fight the COW optimization.
- Don't add arena allocators — wrong pattern for escaping values.
- Don't adopt `compact_str`/`smol_str` if interning is planned — redundant and large refactor.
- Don't change `Value::Map` globally to HashMap — breaks deterministic ordering.

---

## Advanced Path (Beyond Crate Swaps)

If after the above optimizations the target isn't reached, the remaining overhead is in:
1. **Mini-eval dispatch:** Each expression evaluation goes through `match head.as_str()` on symbol names. With interning, this becomes integer comparison — automatically faster.
2. **Stats sub-map as a struct:** Replace the 4-key `{:min :max :sum :count}` BTreeMap with a native Rust struct. This eliminates all map overhead for the stats and is the "endgame" optimization.
3. **Pre-resolved symbol slots:** Turn `env.get("x")` into an index lookup by resolving symbols at "compile" time for the lambda body.

These are interpreter-level optimizations, not crate swaps, and would be separate investigation topics.
