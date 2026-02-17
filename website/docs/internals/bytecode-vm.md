# Bytecode VM

::: tip Opt-In (Alpha)
The bytecode VM is functional and available via the `--vm` CLI flag. The tree-walking interpreter remains the default execution path. Both runtimes coexist and share the global environment.
:::

## Overview

Sema includes a bytecode VM alongside the existing tree-walking interpreter, enabled with the `--vm` CLI flag. The VM compiles Sema source code into stack-based bytecode for faster execution, targeting **5–15× speedup** over the tree-walker on compute-heavy workloads.

The tree-walking interpreter (`sema-eval`) is preserved as the default execution path, the macro expansion engine, and `eval` fallback — the two runtimes coexist, sharing the global environment and `EvalContext`.

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

| Source Form | Lowers To                                 |
| ----------- | ----------------------------------------- |
| `cond`      | Nested `If`                               |
| `when`      | `If` with nil else                        |
| `unless`    | `If` with swapped branches                |
| `case`      | `Let` + nested `If` with `Or` comparisons |
| `defun`     | `Define` + `Lambda`                       |

**Tail position analysis** happens during lowering. The `Call` node carries a `tail: bool` flag, set based on position:

- **Tail**: last expression in `lambda` body, `begin`, `let`/`let*`/`letrec` body, `if` branches, `cond` clauses, `and`/`or` last operand
- **Not tail**: `try` body (handler must be reachable), `do` loop body, non-last expressions

### Phase 2: Variable Resolution (CoreExpr → ResolvedExpr)

The resolver walks the CoreExpr tree and classifies every variable reference as one of:

| Resolution          | Opcode                         | Description                              |
| ------------------- | ------------------------------ | ---------------------------------------- |
| `Local { slot }`    | `LoadLocal` / `StoreLocal`     | Variable in the current function's frame |
| `Upvalue { index }` | `LoadUpvalue` / `StoreUpvalue` | Captured from an enclosing function      |
| `Global { spur }`   | `LoadGlobal` / `StoreGlobal`   | Module-level binding                     |

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

### Phase 3: Bytecode Compilation (ResolvedExpr → Chunk)

The compiler (`compiler.rs`) transforms `ResolvedExpr` into bytecode `Chunk`s. The `Compiler` struct wraps an `Emitter` (bytecode builder) and collects `Function` templates for inner lambdas.

**Compilation strategies:**

- **Constants**: `Nil`, `True`, `False` get dedicated opcodes. All other constants use `Const` + constant pool.
- **Variables**: `LoadLocal`/`StoreLocal` for locals, `LoadUpvalue`/`StoreUpvalue` for captures, `LoadGlobal`/`StoreGlobal`/`DefineGlobal` for globals.
- **Control flow**: `if` uses `JumpIfFalse` + `Jump` for short-circuit. `and`/`or` use `Dup` + conditional jumps to preserve the last truthy/falsy value.
- **Lambdas**: compiled to separate `Function` templates, referenced by `MakeClosure` instruction with upvalue descriptors.
- **`do` loops**: compile to backward `Jump` with `JumpIfTrue` for exit test.
- **`try`/`catch`**: adds entries to the chunk's exception table, no inline opcodes.
- **Named let**: compiled as `MakeClosure` + `Call` — the loop body becomes a function.

**Runtime-delegated forms** — forms that can't be compiled to pure bytecode are compiled as calls to `__vm-*` global functions registered by `sema-eval`:

| Source Form                                               | Delegate                                                     |
| --------------------------------------------------------- | ------------------------------------------------------------ |
| `eval`                                                    | `__vm-eval`                                                  |
| `import`                                                  | `__vm-import`                                                |
| `load`                                                    | `__vm-load`                                                  |
| `defmacro`                                                | `__vm-defmacro-form` (passes entire form as quoted constant) |
| `define-record-type`                                      | `__vm-define-record-type`                                    |
| `delay`                                                   | `__vm-delay` (passes unevaluated body as quoted constant)    |
| `force`                                                   | `__vm-force`                                                 |
| `prompt`, `message`, `deftool`, `defagent`, `macroexpand` | Corresponding `__vm-*` delegates                             |

**Public API**: `compile()`, `compile_many()`, `compile_with_locals()`, `compile_many_with_locals()` — all return `CompileResult { chunk, functions }`.

### Phase 4: VM Execution

The VM (`vm.rs`) is a stack-based dispatch loop.

**Core structs:**

```rust
VM { stack: Vec<Value>, frames: Vec<CallFrame>, globals: Rc<Env>, functions: Vec<Rc<Function>> }
CallFrame { closure: Rc<Closure>, pc: usize, base: usize, open_upvalues: Vec<Option<Rc<UpvalueCell>>> }
```

**Key design points:**

- **Safe operations**: `pop()` and `peek()` return `Result` (no panicking on underflow). `Op::from_u8()` for opcode decoding (no unsafe transmute).
- **Closure interop**: VM closures are wrapped as `Value::NativeFn` values so the tree-walker can call them. Each NativeFn wrapper creates a fresh VM instance to execute the closure's bytecode.
- **Upvalue cells**: `UpvalueCell` with `Rc<RefCell<Value>>` for shared mutable state — `StoreLocal` syncs to open upvalue cells, `LoadLocal` reads from cells when present.
- **Exception handling**: `Throw` opcode triggers handler search via the chunk's exception table. Stack is restored to saved depth, error value pushed, PC jumps to handler.

**Entry points**: `VM::execute()` takes a closure and `EvalContext`. `eval_str()` is a convenience that parses, compiles, and runs. `compile_program()` is the shared pipeline: `Value AST → lower → resolve → compile → (Closure, Vec<Function>)`.

## Opcode Set

The VM uses a stack-based instruction set with variable-length encoding. Each opcode is one byte, followed by operands (u16, u32, or i32).

### Constants & Stack

| Opcode  | Operands  | Description             |
| ------- | --------- | ----------------------- |
| `Const` | u16 index | Push `constants[index]` |
| `Nil`   | —         | Push nil                |
| `True`  | —         | Push #t                 |
| `False` | —         | Push #f                 |
| `Pop`   | —         | Discard top of stack    |
| `Dup`   | —         | Duplicate top of stack  |

### Variable Access

| Opcode         | Operands  | Description                  |
| -------------- | --------- | ---------------------------- |
| `LoadLocal`    | u16 slot  | Push `locals[slot]`          |
| `StoreLocal`   | u16 slot  | `locals[slot] = pop`         |
| `LoadUpvalue`  | u16 index | Push `upvalues[index].get()` |
| `StoreUpvalue` | u16 index | `upvalues[index].set(pop)`   |
| `LoadGlobal`   | u32 spur  | Push `globals[spur]`         |
| `StoreGlobal`  | u32 spur  | `globals[spur] = pop`        |
| `DefineGlobal` | u32 spur  | Define new global binding    |

### Control Flow

| Opcode        | Operands   | Description                 |
| ------------- | ---------- | --------------------------- |
| `Jump`        | i32 offset | Unconditional relative jump |
| `JumpIfFalse` | i32 offset | Pop, jump if falsy          |
| `JumpIfTrue`  | i32 offset | Pop, jump if truthy         |

### Functions

| Opcode        | Operands                         | Description                           |
| ------------- | -------------------------------- | ------------------------------------- |
| `Call`        | u16 argc                         | Call function with argc args          |
| `TailCall`    | u16 argc                         | Tail call (reuse frame for TCO)       |
| `Return`      | —                                | Return top of stack                   |
| `MakeClosure` | u16 func_id, u16 n_upvalues, ... | Create closure from function template |
| `CallNative`  | u16 native_id, u16 argc          | Direct native function call           |

### Data Constructors

| Opcode        | Operands    | Description                    |
| ------------- | ----------- | ------------------------------ |
| `MakeList`    | u16 n       | Pop n values, push list        |
| `MakeVector`  | u16 n       | Pop n values, push vector      |
| `MakeMap`     | u16 n_pairs | Pop 2n values, push sorted map |
| `MakeHashMap` | u16 n_pairs | Pop 2n values, push hash map   |

### Arithmetic & Comparison

| Opcode                       | Description                           |
| ---------------------------- | ------------------------------------- |
| `Add`, `Sub`, `Mul`, `Div`   | Generic arithmetic (int/float/string) |
| `Negate`, `Not`              | Unary operators                       |
| `Eq`, `Lt`, `Gt`, `Le`, `Ge` | Generic comparison                    |
| `AddInt`, `SubInt`, `MulInt` | Specialized int fast paths            |
| `LtInt`, `EqInt`             | Specialized int comparison            |

### Exception Handling

| Opcode  | Description                   |
| ------- | ----------------------------- |
| `Throw` | Pop value, raise as exception |

Exception handling uses an **exception table** on the Chunk rather than inline opcodes. Each entry specifies a PC range, handler address, and stack depth to restore.

## Crate Structure

The bytecode VM lives in the `sema-vm` crate, which sits between `sema-reader` and `sema-eval` in the dependency graph:

```
sema-core ← sema-reader ← sema-vm ← sema-eval
```

`sema-vm` depends on `sema-core` (for `Value`, `Spur`, `SemaError`) and `sema-reader` (for parsing in test helpers). It does **not** depend on `sema-eval` — the evaluator will depend on the VM, not the other way around.

### Source Files

| File           | Purpose                                                           |
| -------------- | ----------------------------------------------------------------- |
| `opcodes.rs`   | `Op` enum — 42 bytecode opcodes                                   |
| `chunk.rs`     | `Chunk` (bytecode + constants + spans), `Function`, `UpvalueDesc` |
| `emit.rs`      | `Emitter` — bytecode builder with jump backpatching               |
| `disasm.rs`    | Human-readable bytecode disassembler                              |
| `core_expr.rs` | `CoreExpr` and `ResolvedExpr` IR enums                            |
| `lower.rs`     | Value AST → CoreExpr lowering pass                                |
| `resolve.rs`   | Variable resolution (local/upvalue/global analysis)               |
| `compiler.rs`  | Bytecode compiler (ResolvedExpr → Chunk)                          |
| `vm.rs`        | VM dispatch loop, call frames, closures, exception handling       |

## Current Limitations

- VM closures create a fresh VM per call (no true TCO across closure boundaries)
- `try`/`catch` doesn't catch runtime errors from native functions (e.g., division by zero)
- `try`/`catch` error value format may differ from tree-walker
- `define-record-type` bindings not visible to subsequent compiled code
- Generic arithmetic opcodes (`Add`, `Sub`, etc.) are defined but currently dead code — arithmetic goes through `NativeFn` globals
- `CallNative` opcode is defined but not yet used (no native function registry)
- `TailCall` falls back to regular `Call` (no frame reuse)

## CLI Usage

The `--vm` flag enables the bytecode compilation path. The tree-walker remains the default.

```bash
# Run a file with the bytecode VM
sema --vm examples/hello.sema

# REPL: toggle VM mode with ,vm
sema
> ,vm
```

Both paths share the global `Env` and `EvalContext`.

## Design Decisions

### Why Keep the Tree-Walker?

The tree-walking interpreter remains for:

1. **Macro expansion** — macros can call `eval` at expansion time, requiring a full evaluator before compilation
2. **`eval` fallback** — dynamic `eval` needs to parse, expand, compile, and execute at runtime
3. **Debugging** — tree-walking is easier to step through and inspect

### Why Not Delete CoreExpr After Resolution?

The pipeline uses two IR types: `CoreExpr` (variables as names) and `ResolvedExpr` (variables as slots). This provides type-level safety — the compiler can only receive resolved expressions, preventing accidental use of unresolved variable references.
