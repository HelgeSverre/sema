# VM Improvement Opportunities

Identified 2026-03-10 during a test coverage audit. Each item rated by effort,
impact, and risk. Effort is wall-clock time for a single developer who knows the
codebase.

---

## Completed

- **Global cache 16 → 256** — `vm.rs` `GLOBAL_CACHE_SIZE` increased
- **Stack depth limit (MAX_FRAMES = 2048)** — clean error on non-tail infinite recursion
- **Constant pool HashMap dedup** — `emit.rs` `add_const` now O(1) via HashMap
- **Native call borrow-not-drain** — `CallNative`, `call_value`, `call_value_with` now borrow `&self.stack[start..]` directly instead of `drain().collect::<Vec<_>>()`
- **Bytecode verification: jump target validation** — `validate_chunk_bytecode` now verifies all jump targets land on valid instruction boundaries
- **Named-let TCO bug fix** — `lower.rs` named-let body was using outer `tail` flag instead of `true`, preventing TCO for recursive named-let calls
- **Unify CoreExpr/ResolvedExpr** — single generic `Expr<V>` with `type CoreExpr = Expr<Spur>`, `type ResolvedExpr = Expr<VarRef>`. Halved the surface area for new constructs.
- **Per-instruction inline cache for globals** — each `LoadGlobal`/`CallGlobal` gets a dedicated cache slot, eliminating hash collisions. Cache entries are `(spur_bits, version, value)` for cross-VM safety. Bytecode format v2.

---

## Remaining

### 7. Switch to Lua-style open upvalues

**Files:** `crates/sema-vm/src/vm.rs`, `crates/sema-vm/src/chunk.rs`

Current model: upvalues are closed eagerly at capture time. Every `StoreLocal`
to a captured variable must write to both the stack slot and the `Rc<RefCell>`
upvalue cell (dual-write). This logic is duplicated across 8 specialized store
opcodes.

Lua's model: open upvalues hold an index into the parent's stack. They share
the stack slot directly — no dual write needed. When the parent frame exits,
open upvalues are "closed" by copying the stack value into the upvalue object.

Benefits:
- Eliminates the `has_open_upvalues` branch on every local load/store
- Eliminates dual-write fragility
- Simpler to add new specialized store opcodes

Costs:
- Need a linked list (or vec) of open upvalues per frame
- Need a `CloseUpvalues` opcode or implicit close at frame exit
- Significant refactor of `make_closure`, `LoadLocal`, `StoreLocal`, frame teardown

- **Effort:** 2-3 days
- **Impact:** Medium — correctness/maintainability improvement, minor perf win
- **Risk:** High — touching the upvalue model affects closure semantics everywhere

### 10. Cycle collector or tracing GC

**Files:** New module, touches `Value`, `Env`, all Rc usage

`Rc` everywhere means reference cycles leak permanently. Self-referential
closures, circular data structures, and mutual recursion with captured bindings
all leak. This is the most fundamental architectural gap.

Options:
- **Cycle collector** (CPython approach): periodic DFS over Rc-managed objects to find
  and break cycles. Easier to retrofit but adds GC pauses.
- **Tracing GC** (Lua approach): mark-and-sweep or copying collector. Requires
  replacing all `Rc<T>` with GC-managed `Gc<T>` pointers. Much larger change but
  better long-term.
- **Arena allocator** with scope-based deallocation: simpler but limits what
  programs can do (no long-lived closures that outlive their scope).

Already listed in `docs/vm-status.md` deferred work as "Tracing GC."

- **Effort:** 1-2 weeks (cycle collector) or 3-4 weeks (tracing GC)
- **Impact:** Critical — fixes the only true correctness bug (memory leaks)
- **Risk:** High — touches the entire value representation

---

## Summary matrix

| # | Item                              | Effort    | Impact   | Risk   | Status      |
|---|-----------------------------------|-----------|----------|--------|-------------|
| 1 | Global cache 16 → 256             | 15 min    | Medium   | None   | Done        |
| 2 | Stack depth limit                 | 20 min    | High     | None   | Done        |
| 3 | Constant pool HashMap dedup       | 30 min    | Low      | Low    | Done        |
| 4 | Native call: borrow not drain     | 2-4 hrs   | High     | Medium | Done        |
| 5 | Bytecode jump target validation   | 3-4 hrs   | Medium   | Low    | Done        |
| 6 | Document TCO limitations          | 1 hr      | Low      | None   | Won't fix   |
| 7 | Lua-style open upvalues           | 2-3 days  | Medium   | High   | Deferred    |
| 8 | Unify CoreExpr/ResolvedExpr       | 1-2 days  | Medium   | Medium | Done        |
| 9 | Per-instruction inline cache      | 2-3 days  | High     | Medium | Done        |
| 10| Cycle collector / tracing GC      | 1-4 weeks | Critical | High   | Open        |
