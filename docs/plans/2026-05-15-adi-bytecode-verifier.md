# Plan: ADI Bytecode Stack-Depth Verifier (Decision #56)

## Verdict

**Feasible. Worth doing — but as a *checked-pop* + verifier combo, not verifier-only.** Confidence: high on technical feasibility, medium on the cost/benefit framing.

**Strongest counterargument (and partially correct):** "Just replace `pop_unchecked` with checked `pop`." The hot dispatch loop already costs a u8 read + match + operand decode + Rc clones for most opcodes; a `pop().ok_or(...)` adds one `cmp + jne` plus a length read that's already in cache. On in-process bytecode this is dead code (verified by construction), so a branch predictor will erase it. **No benchmark currently exists** that proves the unchecked variant is actually load-bearing — the code is unsafe based on theoretical perf, not measured perf. Before defending the verifier as the *necessary* solution, someone needs to flip `pop_unchecked → pop` and run `cargo bench`. If the regression is < 3%, the verifier is a "nice to have" rather than a "must have".

**The honest recommendation:** ship in this order:

1. **Phase 0 (safety hotfix, ~half-day).** A `debug_assert!` already exists at line 533; promote to a runtime check gated on a `loaded_from_disk: bool` carried on `Chunk`. Bytecode from `deserialize_from_bytes` uses checked `pop_unchecked_or_error`; in-process bytecode keeps the unsafe fast path. This closes the UB hole *today* with ~30 LoC. No verifier required.
2. **Phase 1 (verifier).** Build the `Op::stack_effect()` table + abstract interpretation. This unblocks: removing the load-from-disk slowdown of Phase 0, and lets `chunk.max_stack` finally be populated (it's serialized but never set today — see "current state" below).
3. **Phase 2 (unification).** Use the same table inside the in-process compiler to compute `max_stack` and to power a `debug_assert!` against actual runtime depth during dispatch (cfg(debug_assertions)). This is the only meaningful ADI payoff.

The "many instantiations of one table" framing is **partially earned, mostly academic varnish** for this codebase today. See the ADI section below.

---

## Current state (verified, with citations)

### How stack effects are realized today

- **Opcodes are pure tags** (`crates/sema-vm/src/opcodes.rs:7-107`). The enum carries no stack-effect data. There's a `from_u8` and a parallel `pub mod op { pub const ... }` table for fast match dispatch.
- **Stack effects live exclusively in `vm.rs` match arms** (`crates/sema-vm/src/vm.rs:747-1636`). 90+ `pop_unchecked` call sites, each implicitly encoding (pops, pushes).
- **Compiler tracks `stack_height` only for exception-handler restore depth** (`crates/sema-vm/src/compiler.rs:166, 597, 603, 632, 650, 658, 662, 673, 835`). It does NOT compute `chunk.max_stack`.
- **`chunk.max_stack` is dead** (`crates/sema-vm/src/chunk.rs:11`). It is serialized (`serialize.rs:408`), deserialized (`serialize.rs:472`), and never set or read. Effectively a 16-bit hole in the format.
- **`pop_unchecked`'s safety comment explicitly names this gap** (`vm.rs:521-537`): "FIXME(C11): this is safe for in-process compilation, but `.semac` files deserialized via `crate::serialize::deserialize_compile_result` are NOT verified for stack balance."
- **`validate_bytecode` already does structural checks** (`serialize.rs:847-936`): opcode validity, operand bounds, const index, local/upvalue slot, jump target landing on instruction boundary. This is where the verifier hooks in.
- **`advance_pc`** (`serialize.rs:656-700`) is the canonical instruction-length table. The verifier should call it rather than duplicate.

### Real attack surface

- `crates/sema/src/main.rs:856`, `1899-1945`, `2141`: CLI loads `.semac` from disk if magic matches. **Real attack surface.** A user running `sema some-file.semac` from a download is the threat model.
- `crates/sema/src/archive.rs`: `sema build` produces self-extracting binaries. **Not** a verifier-relevant surface — if you run an attacker-supplied binary, the embedded bytecode is the least of your problems.

### Subtle opcodes

- **Variable-arity operand-dependent:**
  - `Call argc` (`vm.rs:902-924`): pops `argc + 1` (callee + args), pushes 1.
  - `TailCall argc` (`vm.rs:925-947`): pops `argc + 1`, pushes 0 — **frame-exiting**.
  - `CallGlobal spur argc cache`: pops `argc`, pushes 1 (no callee on stack — fused load+call) (`vm.rs:1312, 1340-1392`).
  - `CallNative native_id argc`: pops `argc`, pushes 1 (`vm.rs:975-1020`).
  - `MakeList n` / `MakeVector n`: pops `n`, pushes 1 (`vm.rs:1024-1034`).
  - `MakeMap n_pairs` / `MakeHashMap n_pairs`: pops `2 * n_pairs`, pushes 1 (`vm.rs:1036-1054`).
- **`MakeClosure`:** Despite a complex operand layout (`func_id` + `n_upvalues` + n × (is_local, idx)), it **does not consume any stack values** — upvalues are pulled from locals or parent upvalues, not the stack (`vm.rs:2012-2065`). Pops 0, pushes 1. Operand size depends on `n_upvalues` (already handled by `advance_pc`).
- **Frame-exiting ops:** `Return`, `TailCall`, `Throw`. After these, the next linear PC is unreachable from this frame. CFG: no fallthrough edge.
- **`Return` special case** (`vm.rs:948-966`): the code reads `if !self.stack.is_empty() { pop } else { Value::nil() }` — i.e. it tolerates an empty stack at return! This is a behavioral leniency that complicates the spec: the verifier could require depth >= 1 at Return (stricter than runtime) which would reject some weird-but-not-UB bytecode. Recommendation: **be strict** (require depth == 1 at Return). Document the leniency as deprecated.
- **Exception handlers as CFG edges:** Each `ExceptionEntry { try_start, try_end, handler_pc, stack_depth, catch_slot }` (`chunk.rs:38-45`) implies: from *any* PC in `[try_start, try_end)`, a non-fallthrough edge can reach `handler_pc` with operand depth = `stack_depth - n_locals + 1` (the +1 is the error value the runtime pushes at `vm.rs:2144`). The handler's first opcode is `StoreLocal catch_slot` (`compiler.rs:812-813`), which pops it, leaving operand depth = `stack_depth - n_locals`.

### CFG details

- Jumps are i32 relative offsets, relative to **the PC after the operand** (`vm.rs:861-873`, `serialize.rs:911`). All static.
- No indirect jumps, no computed branches, no `case`/jump-table opcode.
- `validate_bytecode` already enforces that jump targets land on instruction boundaries (`serialize.rs:921-933`).
- Frame transitions (`Call`, `CallGlobal`, `CallNative`, `TailCall`, `Return`, `MakeClosure`) all `continue 'dispatch`, but for the verifier they are intra-frame: `Call`/`CallGlobal`/`CallNative` have a single fallthrough successor with `depth - argc[ - 1] + 1`. `TailCall`, `Return`, `Throw` have no successors.

---

## Proposed `Op::stack_effect()` design

```rust
// crates/sema-vm/src/opcodes.rs
pub struct StackEffect { pub pops: u16, pub pushes: u16, pub exits_frame: bool }

impl Op {
    /// Stack effect of this opcode. For variable-arity opcodes,
    /// pass the decoded operand; for fixed-arity ones pass 0.
    pub fn stack_effect(self, operand: u16) -> StackEffect {
        use Op::*;
        match self {
            Const | Nil | True_ | False_
            | LoadLocal | LoadLocal0 | LoadLocal1 | LoadLocal2 | LoadLocal3
            | LoadUpvalue | LoadGlobal | Dup | MakeClosure
                => StackEffect { pops: 0, pushes: 1, exits_frame: false },
            Pop | StoreLocal | StoreLocal0..=StoreLocal3
            | StoreUpvalue | StoreGlobal | DefineGlobal | JumpIfFalse | JumpIfTrue
                => StackEffect { pops: 1, pushes: 0, exits_frame: false },
            Jump => StackEffect { pops: 0, pushes: 0, exits_frame: false },
            // Variable arity
            Call | TailCall => StackEffect {
                pops: operand + 1,
                pushes: if matches!(self, TailCall) { 0 } else { 1 },
                exits_frame: matches!(self, TailCall),
            },
            CallGlobal | CallNative
                => StackEffect { pops: operand, pushes: 1, exits_frame: false },
            MakeList | MakeVector
                => StackEffect { pops: operand, pushes: 1, exits_frame: false },
            MakeMap | MakeHashMap
                => StackEffect { pops: operand * 2, pushes: 1, exits_frame: false },
            Return | Throw
                => StackEffect { pops: 1, pushes: 0, exits_frame: true },
            // Binary
            Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge
            | AddInt | SubInt | MulInt | LtInt | EqInt
            | Cons | Append | Get | ContainsQ | Mod | Nth
                => StackEffect { pops: 2, pushes: 1, exits_frame: false },
            // Unary
            Negate | Not | Car | Cdr | Length
            | IsNull | IsPair | IsList | IsNumber | IsString | IsSymbol
                => StackEffect { pops: 1, pushes: 1, exits_frame: false },
        }
    }
}
```

(`True_` / `False_` is just escaping the keyword — actual enum uses `True`/`False`.)

The operand-reading lives in the verifier driver, not in `stack_effect`, so the function stays pure and trivially auditable. The match is exhaustive — adding a new opcode without adding a case fails to compile, same pattern as `from_u8`/`_assert_all_ops_covered` (`opcodes.rs:191-263`).

---

## Verifier algorithm

```text
verify_chunk(chunk, n_functions, n_upvalues) -> Result<u16 /* max_stack */, SemaError>

State:
  entry_depth: HashMap<pc, i32>          // operand stack depth on entry to each instruction
  worklist:    VecDeque<pc>
  max_seen:    u16

Init:
  entry_depth[0] = 0
  worklist.push_back(0)
  // Seed exception handlers as roots; their entry depth is constrained.
  for each ExceptionEntry e:
      verify_handler_entry(e, chunk.n_locals)?
      seed_or_join(e.handler_pc, e.stack_depth - chunk.n_locals + 1)

Loop:
  while let Some(pc) = worklist.pop_front():
      let depth = entry_depth[pc];
      decode op + operand at pc via advance_pc (already exists in serialize.rs)
      let effect = op.stack_effect(operand_value);
      if depth < effect.pops as i32 {
          return Err("stack underflow at pc {pc}: depth {depth}, op {op:?} requires {effect.pops}");
      }
      let after = depth - effect.pops as i32 + effect.pushes as i32;
      max_seen = max_seen.max(after.max(depth) as u16);

      if effect.exits_frame {
          if op == Return && depth != 1 { return Err("Return with depth {depth} != 1"); }
          continue;
      }

      // Successors
      let next_pc = pc + instruction_length(op, operand);
      let successors: SmallVec<[pc; 2]> = match op {
          Jump          => [pc + 5 + offset],
          JumpIfFalse | JumpIfTrue => [next_pc, pc + 5 + offset],
          _             => [next_pc],
      };
      // Exception edges: for any pc inside some [try_start, try_end), the
      // handler is already seeded as a root; no per-instruction edge needed.

      for succ in successors:
          if succ == chunk.code.len() {
              // Falling off end: only OK if previous op was Return/TailCall/Throw,
              // which we already 'continue'd above. So this is an error.
              return Err("control falls off end of chunk at pc {pc}");
          }
          seed_or_join(succ, after);

return Ok(max_seen);

seed_or_join(pc, depth):
  match entry_depth.entry(pc):
      Vacant => insert depth, push to worklist
      Occupied(old) =>
          // Lattice: depth is an integer; "join" is equality for well-formed code.
          // If they differ, the bytecode is inconsistent — reject.
          if *old != depth {
              return Err("stack depth disagreement at pc {pc}: previously {old}, now {depth}");
          }
```

**Lattice and termination.** Sema's compiler emits structured control flow only; every reachable PC has a single well-defined operand depth on entry. The lattice is `Option<i32>` with `bottom = None`. Join is "must equal" — if two predecessors disagree, that's a bug in the bytecode and we reject. This is the *strict* interpretation (Java verifier uses this too). It's stricter than "take min" but rejects strictly more bad bytecode and accepts every well-formed program. Termination: each PC's depth is set at most once before disagreement; worklist drains in O(code_len) iterations.

**Why not "min" as the ADR proposes?** Min works but it accepts bytecode that's irregular (different depths joining), which then breaks the `chunk.max_stack` calculation and any future type lattice. The Java/CLR verifiers both require equality. Recommend equality.

**Exception edges as roots.** Don't add a per-instruction edge `pc -> handler_pc` for each `pc in [try_start, try_end)` — that's O(code_len × n_handlers) edges. Instead, seed each `handler_pc` with its known entry depth (`stack_depth - n_locals + 1`) as a *root*, alongside `pc=0`. Then independently verify that for every `pc in [try_start, try_end)`, `entry_depth[pc] + max_intermediate_push <= stack_depth - n_locals` is *possible* (the runtime truncates back to `stack_depth`, so the assertion is that the handler depth is consistent — no edge needed in the worklist). Concretely: the handler is its own reachability root with depth `stack_depth - n_locals + 1`; the verifier just confirms that's non-negative.

**Output.** Returns `max_stack`. Write it into `chunk.max_stack` after verification. That field is currently dead; the verifier finally gives it meaning.

---

## ADI lens — honest accounting

The ADI framing says: factor the opcode semantics into a single denotation, then instantiate it for different abstract domains.

**What's earned:**
- The `Op::stack_effect()` table is genuine shared data between the verifier and the (currently nonexistent) `chunk.max_stack` computation in the in-process compiler. Today the compiler does *ad-hoc* `stack_height += 1; stack_height -= argc;` updates at each emission site (`compiler.rs:597, 603, 632, 650, 658, 662, 673`). After the refactor, `Compiler` can emit + call `Op::stack_effect(op, operand)` to update one counter, eliminating ~20 manually-maintained `stack_height` adjustments. That **is** an immediate DRY win.
- `debug_assert!` in dispatch: in `cfg(debug_assertions)`, after each opcode dispatch in `vm.rs`, assert `self.stack.len() - frame.base - n_locals == expected_depth_after`. This catches verifier/dispatch drift early.

**What's speculative:**
- "Cost-of-instruction abstract domain." Sema has no JIT, no inliner, no scheduler that consumes opcode cost. No short-term user.
- "Type-of-TOS lattice." Would require a much bigger refactor (Value type info is dynamic). Possibly useful for `AddInt` specialization decisions but the current compiler decides specialization syntactically (`(+ x y)` with known-int operands at compile time), not from a lattice.
- "Fuzzer oracle." `cargo fuzz` against `deserialize_from_bytes` is a 50-LoC fuzz target that doesn't need a separate oracle — the verifier *is* the oracle.

**Verdict on ADI framing:** keeping the stack-effect table as a single source of truth shared by verifier + compiler is good engineering. Calling it "definitional interpreters with abstracting interpretations" is academic varnish — there's no concrete short-term second instantiation beyond what a normal "factor out the table" refactor would yield. Frame this as "DRY for stack effects" in code review; mention ADI as inspiration in the ADR but don't gate the design on the framing.

---

## Interaction with the real VM dispatch loop

**Can the dispatch loop *use* `Op::stack_effect()` directly?** No, and shouldn't. Dispatch has many opcode-specific behaviors beyond pop/push: error handling, frame transitions, exception unwinding, intrinsic dispatch, inline-cache update. The match arms encode this irreducibly. Trying to use `stack_effect()` from dispatch would either (a) require an interpreter-style dynamic dispatch table (slow), or (b) double up the data with no win.

**What the dispatch loop *can* do:** in `cfg(debug_assertions)`, maintain a sidecar `expected_depth` counter updated via `stack_effect()` and `debug_assert_eq!(actual, expected)` after each instruction. Cost: zero in release builds, catches drift in `cargo test`. Cheap insurance.

---

## Phasing

| Phase | Scope | LoC | Days |
|-------|-------|-----|------|
| 0 | Checked-pop for loaded-from-disk chunks. Add `Chunk::from_disk: bool`. Add `pop_checked` variant. Branch in `pop_unchecked` macro. Closes UB hole today. | ~50 | 0.5 |
| 1a | `Op::stack_effect()` table in `opcodes.rs`. Unit tests asserting it matches each `vm.rs` arm by inspection (manual one-time audit). | ~120 | 0.5 |
| 1b | Verifier in new file `crates/sema-vm/src/verifier.rs`. Call from `validate_bytecode` after the existing structural checks. Write computed `max_stack` into chunk. Tests: positive (every existing `.semac` integration test still passes) + negative (hand-crafted bad chunks). | ~300 + ~200 tests | 2 |
| 1c | Remove Phase 0's checked-pop branch (verifier now guarantees safety). Single safety comment in `vm.rs` pointing at verifier. | ~30 | 0.5 |
| 2 | Compiler uses `Op::stack_effect()` to update `stack_height`. Compute `max_stack` in compiler too (so in-process chunks have non-zero `max_stack`). Pre-allocate `self.stack` with capacity from `max_stack` per frame. Possible micro-perf win. | ~80 | 0.5 |
| 3 | `cfg(debug_assertions)` sidecar depth tracker in dispatch loop. | ~40 | 0.25 |

Total: ~4 days. Phases 0 and 1 are independently shippable; everything after Phase 1c is icing.

---

## Blast radius

- **Files modified:** `crates/sema-vm/src/opcodes.rs`, `crates/sema-vm/src/serialize.rs`, `crates/sema-vm/src/compiler.rs`, `crates/sema-vm/src/lib.rs` (export `Verifier`). New file: `crates/sema-vm/src/verifier.rs`.
- **Cross-crate impact: none.** `validate_bytecode` is called from inside `deserialize_from_bytes`; callers in `sema` crate just see the same `Result`. No public API change for `sema-vm` consumers.
- **Bytecode format: unchanged.** `max_stack` is already in the format and unused; we just start populating it. No version bump.
- **Test impact:** existing `serialize_roundtrip_test.rs` keeps passing (all in-process compiler output is verifiable). Add `verifier_negative_test.rs` with hand-crafted bad chunks.

---

## Risks

1. **Drift between verifier table and dispatch.** Mitigations: (a) the `_assert_all_ops_covered` pattern in `opcodes.rs:191-263` extended to `stack_effect`; (b) `cfg(debug_assertions)` sidecar in dispatch (Phase 3); (c) a property-style test that compiles N random valid programs and verifies them.
2. **Perf cost at load.** Single linear pass + worklist, O(code_len) for structured CFGs. Smaller than the existing `validate_bytecode` two-pass scan. Negligible.
3. **Format version.** Not bumped (max_stack is already serialized). But: if `chunk.max_stack` starts being trusted by the loader (e.g., to pre-size `self.stack`), then forward-compatibility with chunks that have a *stale* `max_stack` from a future buggy compiler matters. Recommend: verifier *recomputes* `max_stack`; the value loaded from disk is a hint or ignored.
4. **Strict join vs. min.** Choosing strict equality means we may reject some legitimate-but-irregular bytecode from a future optimizer (e.g., one that emits join points with phi-style depth reconciliation). Sema doesn't have such an optimizer today. Document the choice; revisit if/when needed.
5. **`Return` with empty stack leniency.** Existing runtime silently substitutes `Value::nil()` (`vm.rs:948-953`). Verifier proposed to be stricter. Audit existing `.semac` test fixtures to confirm none rely on this. If any do, either fix them or relax the verifier rule.

---

## Alternatives considered

| Alternative | Closes UB? | Effort | Verdict |
|---|---|---|---|
| Checked `pop` everywhere | Yes | ~1 day | **Plausibly sufficient.** Requires benchmark first; if regression <3%, ship this alone and close ADR #56 as "won't do". |
| Checked `pop` only for loaded-from-disk chunks | Yes for `.semac`, no for in-process (already safe) | ~0.5 day | Phase 0 of the proposed plan. Pragmatic stopgap. |
| Refuse to load `.semac` at all | Yes | trivial | Breaks `sema some-file.semac` workflow. Probably overreaction. |
| Sandboxing (seccomp/Wasm) | Indirectly | high | Wrong layer for this bug. |
| `--untrusted` flag that forces checked pop | Yes | ~1 day | Surfaces the trust boundary to users. Worth combining with verifier in case verifier has bugs. |
| Verifier (this proposal) | Yes, and unlocks `max_stack` | ~4 days | The principled fix. |
| Guard pages on the value stack | Partially — converts UB to SIGSEGV | medium | Doesn't fix logic errors, just makes UB visible. |

The plan's bias: **start with checked-pop for disk-loaded bytecode as a same-week hotfix**, then build the verifier as the long-term answer. Don't gate the safety fix on the verifier landing.

---

## Concrete next step

Before any of this: run `cargo bench` with `pop_unchecked` replaced by `pop().ok_or_else(...)`. If the regression is under ~3%, the entire ADR can be downgraded to "won't do — just use checked pop". This single benchmark decides whether this is a 4-day project or a 1-day project.
