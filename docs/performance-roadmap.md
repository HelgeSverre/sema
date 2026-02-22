# Performance Roadmap

Where Sema's time goes and what can be done about it. Based on analysis of the 1BRC benchmark (the most measured workload) and compute-heavy benchmarks (TAK, deriv).

## Current State

- **Tree-walker:** ~28.4s on 10M rows (native), 22.4× behind SBCL
- **Bytecode VM:** ~15.9s on 10M rows (native), 11.2× behind SBCL, ~1.7× behind Janet
- **Compute benchmarks (VM):** TAK 1,250ms, upvalue-counter 450ms, deriv 1,151ms

Janet (13.4s) is the most meaningful comparison — both are embeddable scripting languages with bytecode VMs, no JIT, no native compilation.

## Where the Time Goes

### ~60% — Stdlib call overhead

Every `string/split`, `assoc`, `string->number` goes through: VM → `CallGlobal` → hash lookup in `Env` → downcast `Rc<NativeFn>` → allocate args slice → call Rust function → wrap result back to `Value`. Janet's register VM calls C functions with a direct pointer — no `Rc` downcasting, no `ValueView` unpacking.

### ~25% — Rc refcounting

Every `Value::clone()` is an `Rc::clone()` for heap types (strings, lists, maps). Every drop decrements. The NaN-boxing is clever — small ints/bools/symbols are unboxed `u64` — but strings and lists still pay the refcount cost on every copy and drop. Janet uses a tracing GC, so copies are free (just pointer copies) and collection is batched.

### ~15% — Data structure overhead

`BTreeMap` for env lookups, `Rc<Vec<Value>>` for lists (no cons cells), `Rc<String>` for every string value. Each imposes allocation and indirection costs on hot paths.

## Tier 1: Big Wins (2–5× total speedup possible)

### 1. Inline common stdlib into VM opcodes

**Impact:** Estimated ~1.5× speedup
**Effort:** Medium (days)

The biggest single win. Instead of `CallGlobal("car")` → hash lookup → NativeFn → call, emit dedicated opcodes like `OpCar` that operate directly on the stack in the dispatch loop.

Already done for: `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `=`, `not` (the `AddInt`, `SubInt`, etc. intrinsics).

Extend to the next 20 most-called functions:
- List: `car`, `cdr`, `cons`, `length`, `null?`, `list?`, `first`, `rest`
- Map: `assoc`, `get`, `contains?`
- String: `string-length`, `string-ref`, `string-append`
- Type: `number?`, `string?`, `symbol?`
- Misc: `apply`, `not`, `display`

This is what Lua's VM does — `OP_GETTABLE`, `OP_CONCAT`, `OP_LEN` etc. are inline opcodes, not function calls.

### 2. Replace Rc with a tracing GC

**Impact:** Estimated ~1.3× speedup
**Effort:** Large (weeks)

The architectural change that would close the gap to Janet. Every `Value::clone()` and `Value::drop()` currently touches the refcount (cache-unfriendly). A simple mark-and-sweep GC (like Janet's) makes value copies free (just copy 8 bytes of NaN-boxed `u64`) and batches deallocation.

The NaN-boxed `u64` representation is already perfect for this — the 6-bit tag tells you whether the 45-bit payload is a GC pointer. The `ValueView` enum would still work for pattern matching; only the memory management changes.

Considerations:
- Sema is currently single-threaded (`Rc`, not `Arc`) — a simple non-concurrent GC is fine
- Cycles are not handled by `Rc` today (not a practical issue yet, but GC solves it for free)
- GC pause latency may matter for interactive/streaming use cases — generational GC could help
- This is the single biggest architectural bottleneck but also the most invasive change

### 3. Stack-allocated strings for short-lived intermediates

**Impact:** Estimated ~1.2× on I/O-heavy workloads
**Effort:** Medium (days)

In the 1BRC hot loop, `string/split` creates 2 temporary `Rc<String>` per line that are immediately consumed and dropped. An arena or bump allocator for strings that don't escape the current call frame would eliminate millions of `Rc` alloc/dealloc pairs.

Options:
- Bump allocator per VM `run()` invocation, reset on return
- Small-string optimization (inline strings ≤ 22 bytes in the `Value` payload directly)
- `bumpalo` for temporaries with explicit escape-to-heap promotion

## Tier 2: Moderate Wins (1.3–2× additional)

### 4. Computed goto / direct threading for VM dispatch

**Impact:** Estimated 15–30% on tight loops
**Effort:** Small–Medium

The current `match op { ... }` compiles to a jump table, but each iteration re-enters the match. With direct threading (jump directly to the next handler's address), you eliminate the central dispatch. Lua, CPython, and most production VMs do this.

Rust doesn't natively support computed goto, but options exist:
- `unsafe` with function pointer table
- C shim for the dispatch loop
- Wait for Rust's `#[feature(label_break_value)]` or similar

### 5. Constant folding and dead code elimination in the compiler

**Impact:** Varies, significant for idiomatic code
**Effort:** Medium

The lowering pass (`crates/sema-vm/src/lower.rs`) currently does minimal optimization. Standard optimizations:
- Fold `(+ 1 2)` → `3` at compile time
- Propagate known constants through `let` bindings
- Eliminate unused bindings and dead branches
- Strength reduction: `(* x 2)` → `(+ x x)`

### 6. Register-based VM instead of stack-based

**Impact:** Estimated 20–40% fewer instructions
**Effort:** Large (rewrite of VM)

Stack VMs are simpler but generate more push/pop traffic. A register VM (like Lua 5.x, Janet) encodes source/destination registers in the opcode, reducing stack manipulation. The tradeoff is wider instructions (3–4 byte operand fields instead of 0–2).

This would be a full rewrite of `crates/sema-vm/src/vm.rs` and the emitter. Consider only if the stack VM hits a ceiling after other optimizations.

## Tier 3: Smaller Wins (10–30%)

### 7. Inline caching for global lookups

**Impact:** 10–20% on function-call-heavy code
**Effort:** Small–Medium

The VM already has a 16-entry direct-mapped `global_cache`. Expanding to per-callsite inline caching (monomorphic IC) would make repeated calls to the same global function nearly free — one comparison per call instead of a hash lookup.

Store `(expected_spur, env_version, cached_value)` at each `CallGlobal` site. On hit: use cached value directly. On miss: fall back to hash lookup and update the cache.

### 8. Specialize hot higher-order functions

**Impact:** 10–20% on functional-style code
**Effort:** Medium

`file/fold-lines`, `map`, `filter`, `reduce` are the hottest higher-order functions. The VM could recognize these patterns and:
- Keep the closure's call frame alive across iterations (avoid per-call frame setup/teardown)
- Reuse the argument slots instead of pushing/popping
- Fuse map+filter chains into a single loop

### 9. String interning for string values

**Impact:** 10–15% on workloads with repeated string keys
**Effort:** Small

Currently only symbols and keywords are interned as `Spur`. In workloads with repeated string keys (like 1BRC's ~400 station names), interning string values would make `assoc`/`get` lookups O(1) integer comparison instead of O(n) string comparison.

Could be opt-in (e.g., `string/intern`) or automatic for short strings.

## What Would Close the Gap to Janet?

Janet is ~1.7× faster than Sema's VM on the 1BRC benchmark. Realistically:

| Change | Expected speedup | Cumulative |
| --- | --- | --- |
| Inline top-20 stdlib ops (#1) | ~1.5× | ~1.5× |
| Tracing GC (#2) | ~1.3× | ~1.95× |
| Direct threading (#4) | ~1.2× | ~2.3× |

Combined, Sema would be **competitive with Janet** and **ahead of Guile/Gauche**. This would move Sema from 11.2× behind SBCL to roughly 5–6× behind — solidly mid-pack among interpreted Lisps.

## What's Not Realistic

Catching SBCL (1.0×) or Chez Scheme (1.3×) is not possible without a native code compiler. Those implementations compile Lisp to machine code — `(+ x y)` becomes an `ADD` instruction. No amount of VM optimization can match that. A JIT compiler (like LuaJIT) could close the gap further, but that's a multi-year project and changes the character of the language.

## Previously Tried and Rejected

| Approach | Result | Why |
| --- | --- | --- |
| HashMap for Env | Slower | `BTreeMap` is faster for small maps (1–3 entries) typical of `let` scopes |
| im-rc / rpds (persistent collections) | Slower | Structural sharing fights COW — the point is to mutate in place when refcount is 1 |
| bumpalo / typed-arena | Incompatible | Values need to escape the arena (returned from functions, stored in envs) |
| compact_str / smol_str | Redundant | Symbols/keywords are already interned as `Spur`; string values aren't in the dispatch hot path |
| Mini-eval (inlined evaluator in stdlib) | Removed | 3× faster but caused semantic drift and blocked the bytecode VM |
