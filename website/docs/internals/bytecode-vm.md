# Bytecode VM

::: warning Work in Progress
The bytecode VM is under active development. The tree-walking interpreter remains the default execution path. This page documents the compilation pipeline and VM architecture as they are being built.
:::

## Overview

Sema is adding a bytecode VM alongside the existing tree-walking interpreter. The VM compiles Sema source code into stack-based bytecode for faster execution, targeting **5–15× speedup** over the tree-walker on compute-heavy workloads.

The tree-walking interpreter (`sema-eval`) is preserved as the macro expansion engine and `eval` fallback — the two runtimes coexist, sharing the global environment and `EvalContext`.

## Compilation Pipeline

```
Source text
  → Reader       (tokenize + parse → Value AST)
  → Macro expand  (tree-walker evaluates macros)
  → Lower         (Value AST → CoreExpr IR)
  → Resolve        (CoreExpr → ResolvedExpr with slot/upvalue/global analysis)
  → Compile        (ResolvedExpr → bytecode Chunks)
  → VM execution   (dispatch loop)
```

### Phase 1: Lowering (Value → CoreExpr)

The lowering pass converts the `Value` AST into `CoreExpr`, a desugared intermediate representation. All ~35 special forms are lowered to ~30 CoreExpr variants. Several forms desugar into simpler ones:

| Source Form | Lowers To |
|------------|-----------|
| `cond` | Nested `If` |
| `when` | `If` with nil else |
| `unless` | `If` with swapped branches |
| `case` | `Let` + nested `If` with `Or` comparisons |
| `defun` | `Define` + `Lambda` |

**Tail position analysis** happens during lowering. The `Call` node carries a `tail: bool` flag, set based on position:

- **Tail**: last expression in `lambda` body, `begin`, `let`/`let*`/`letrec` body, `if` branches, `cond` clauses, `and`/`or` last operand
- **Not tail**: `try` body (handler must be reachable), `do` loop body, non-last expressions

### Phase 2: Variable Resolution (CoreExpr → ResolvedExpr)

The resolver walks the CoreExpr tree and classifies every variable reference as one of:

| Resolution | Opcode | Description |
|-----------|--------|-------------|
| `Local { slot }` | `LoadLocal` / `StoreLocal` | Variable in the current function's frame |
| `Upvalue { index }` | `LoadUpvalue` / `StoreUpvalue` | Captured from an enclosing function |
| `Global { spur }` | `LoadGlobal` / `StoreGlobal` | Module-level binding |

This is the key optimization over the tree-walker: instead of hash-based environment chain lookup (O(scope depth) per access), variables are accessed by direct slot index (O(1)).

#### Upvalue Capture

Closures use the Lua/Steel upvalue model. When a lambda references a variable from an enclosing function:

1. The resolver marks the outer local as **captured**
2. An `UpvalueDesc` entry is added to the inner lambda: `ParentLocal(slot)` if capturing from the immediate parent, `ParentUpvalue(index)` if capturing through an intermediate function

```scheme
(lambda (x)           ; x = Local slot 0
  (lambda ()          ; captures x: UpvalueDesc::ParentLocal(0)
    (lambda ()        ; captures through chain: UpvalueDesc::ParentUpvalue(0)
      x)))            ; resolves to Upvalue { index: 0 }
```

### Phase 3: Bytecode Compilation

::: info Coming Soon
The bytecode compiler (ResolvedExpr → Chunk) is the next phase to be implemented.
:::

### Phase 4: VM Execution

::: info Coming Soon
The VM dispatch loop, call frames, and native function registry are planned for implementation after the compiler.
:::

## Opcode Set

The VM uses a stack-based instruction set with variable-length encoding. Each opcode is one byte, followed by operands (u16, u32, or i32).

### Constants & Stack

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Const` | u16 index | Push `constants[index]` |
| `Nil` | — | Push nil |
| `True` | — | Push #t |
| `False` | — | Push #f |
| `Pop` | — | Discard top of stack |
| `Dup` | — | Duplicate top of stack |

### Variable Access

| Opcode | Operands | Description |
|--------|----------|-------------|
| `LoadLocal` | u16 slot | Push `locals[slot]` |
| `StoreLocal` | u16 slot | `locals[slot] = pop` |
| `LoadUpvalue` | u16 index | Push `upvalues[index].get()` |
| `StoreUpvalue` | u16 index | `upvalues[index].set(pop)` |
| `LoadGlobal` | u32 spur | Push `globals[spur]` |
| `StoreGlobal` | u32 spur | `globals[spur] = pop` |
| `DefineGlobal` | u32 spur | Define new global binding |

### Control Flow

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Jump` | i32 offset | Unconditional relative jump |
| `JumpIfFalse` | i32 offset | Pop, jump if falsy |
| `JumpIfTrue` | i32 offset | Pop, jump if truthy |

### Functions

| Opcode | Operands | Description |
|--------|----------|-------------|
| `Call` | u16 argc | Call function with argc args |
| `TailCall` | u16 argc | Tail call (reuse frame for TCO) |
| `Return` | — | Return top of stack |
| `MakeClosure` | u16 func_id, u16 n_upvalues, ... | Create closure from function template |
| `CallNative` | u16 native_id, u16 argc | Direct native function call |

### Data Constructors

| Opcode | Operands | Description |
|--------|----------|-------------|
| `MakeList` | u16 n | Pop n values, push list |
| `MakeVector` | u16 n | Pop n values, push vector |
| `MakeMap` | u16 n_pairs | Pop 2n values, push sorted map |
| `MakeHashMap` | u16 n_pairs | Pop 2n values, push hash map |

### Arithmetic & Comparison

| Opcode | Description |
|--------|-------------|
| `Add`, `Sub`, `Mul`, `Div` | Generic arithmetic (int/float/string) |
| `Negate`, `Not` | Unary operators |
| `Eq`, `Lt`, `Gt`, `Le`, `Ge` | Generic comparison |
| `AddInt`, `SubInt`, `MulInt` | Specialized int fast paths |
| `LtInt`, `EqInt` | Specialized int comparison |

### Exception Handling

| Opcode | Description |
|--------|-------------|
| `Throw` | Pop value, raise as exception |

Exception handling uses an **exception table** on the Chunk rather than inline opcodes. Each entry specifies a PC range, handler address, and stack depth to restore.

## Crate Structure

The bytecode VM lives in the `sema-vm` crate, which sits between `sema-reader` and `sema-eval` in the dependency graph:

```
sema-core ← sema-reader ← sema-vm ← sema-eval
```

`sema-vm` depends on `sema-core` (for `Value`, `Spur`, `SemaError`) and `sema-reader` (for parsing in test helpers). It does **not** depend on `sema-eval` — the evaluator will depend on the VM, not the other way around.

### Source Files

| File | Purpose |
|------|---------|
| `opcodes.rs` | `Op` enum — 42 bytecode opcodes |
| `chunk.rs` | `Chunk` (bytecode + constants + spans), `Function`, `UpvalueDesc` |
| `emit.rs` | `Emitter` — bytecode builder with jump backpatching |
| `disasm.rs` | Human-readable bytecode disassembler |
| `core_expr.rs` | `CoreExpr` and `ResolvedExpr` IR enums |
| `lower.rs` | Value AST → CoreExpr lowering pass |
| `resolve.rs` | Variable resolution (local/upvalue/global analysis) |

## Design Decisions

### Why Keep the Tree-Walker?

The tree-walking interpreter remains for:

1. **Macro expansion** — macros can call `eval` at expansion time, requiring a full evaluator before compilation
2. **`eval` fallback** — dynamic `eval` needs to parse, expand, compile, and execute at runtime
3. **Debugging** — tree-walking is easier to step through and inspect

### Why Not Delete CoreExpr After Resolution?

The pipeline uses two IR types: `CoreExpr` (variables as names) and `ResolvedExpr` (variables as slots). This provides type-level safety — the compiler can only receive resolved expressions, preventing accidental use of unresolved variable references.
