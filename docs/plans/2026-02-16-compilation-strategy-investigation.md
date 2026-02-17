# Compilation Strategy Investigation — Bytecode VM vs Native Code

> **Status:** Research only — no code changes yet. Findings for review before implementation.

**Date:** 2026-02-16

**Goal:** Evaluate whether Sema should add a bytecode VM, a native code compiler, or both — and which specific tooling/approach to use. Scope: full production-quality implementation, not MVP.

---

## Executive Summary & Recommendation

**Primary recommendation: custom stack-based bytecode VM.**

A bytecode VM preserves WASM support, integrates cleanly with Sema's macros/module system/LLM native calls, and can realistically deliver **5–15× speedup** over the tree-walker with manageable engineering risk. Effort: **~6–12 person-months**.

A full native-code compiler (LLVM/Cranelift) is viable but only justified if the VM proves insufficient. It requires a real GC, robust debug metadata, and likely a dual-backend strategy (native + VM for WASM). Effort: **~12–24+ person-months**.

**The GC question is the elephant in the room.** Sema's `Rc`-based memory management has no cycle collection. With closures, environments, self-referential bindings, and compiled closure graphs, reference cycles will leak. A production-quality compilation target almost certainly requires a tracing GC.

| Path                   | Speedup | Effort    | WASM              | GC Required | Risk   |
| ---------------------- | ------- | --------- | ----------------- | ----------- | ------ |
| **Bytecode VM**        | 5–15×   | 6–12 PM   | ✅                | Recommended | Medium |
| **Native (Cranelift)** | 10–30×  | 12–24+ PM | ❌ (dual backend) | Required    | High   |
| **Native (LLVM)**      | 10–30×  | 12–24+ PM | ✅ (LLVM WASM)    | Required    | High   |
| **C as IR**            | 5–20×   | 8–14 PM   | ❌                | Required    | Medium |

---

## Current Architecture Recap

Understanding what we're compiling away from:

### What we have today (~46K lines of Rust across 6 crates)

- **Tree-walking interpreter** with trampoline-based TCO (`eval_step` → `Trampoline::Value` | `Trampoline::Eval`)
- **22 `Value` variants** — dynamically typed, `Rc`-based (single-threaded)
- **`Env`**: parent-chain of `Rc<RefCell<HashMap<Spur, Value>>>` — variable lookup walks the chain
- **~350 native functions** (`NativeFn::simple` / `NativeFn::with_ctx`) across 19 stdlib modules
- **~35 special forms** dispatched via pre-interned `Spur` comparison in `try_eval_special`
- **Unhygienic `defmacro`** with expand-then-eval (macros can call `eval` at expansion time)
- **Module system** (`import`/`load` with cyclic detection, file-relative path resolution)
- **LLM primitives** as first-class Values (Prompt, Message, Conversation, ToolDef, Agent) — HTTP I/O via `tokio::block_on`
- **`EvalContext`**: module cache, call stack, span table, eval depth/step counters, sandbox
- **WASM target** support (playground at sema.run, in-memory VFS)
- **712 tests** (545 integration + 115 reader + 25 sema-llm + 18 pricing + 7 embedding + 2 sema-core)

### Current performance bottlenecks

Based on 1BRC benchmarking (~1000ms for 1M rows after perf crate integration):

1. **Env chain lookups**: `env.get(spur)` walks `Rc<RefCell<HashMap>>` parent chain — O(depth × hashmap lookup) per variable access
2. **Cloning**: every `Value` access clones an `Rc` (refcount increment); every scope creation allocates a new `HashMap`
3. **Special form dispatch**: 35 if-else comparisons per list evaluation (though Spur comparison is fast)
4. **No constant folding**: `(+ 1 2)` evaluates both operands at runtime every time
5. **No inlining**: every function call allocates a new scope, binds params by name

### What compilation eliminates

| Bottleneck             | Tree-walker cost                       | Compiled cost            |
| ---------------------- | -------------------------------------- | ------------------------ |
| Variable lookup        | HashMap chain walk per access          | Slot index (O(1))        |
| Scope creation         | `Rc<RefCell<HashMap>>` alloc per scope | Stack frame pointer bump |
| Special form dispatch  | 35 if-else per list                    | Direct bytecode/branch   |
| Constant evaluation    | Full eval per literal                  | Inline constant          |
| Function call overhead | Clone args, alloc scope, bind by name  | Copy to slots            |
| Tail call              | Trampoline loop + env clone            | Reuse frame + jump       |

---

## Strategy A: Bytecode VM

### A1. Instruction Set Design

**Recommendation: stack-based bytecode** with lexical local slots per call frame.

Stack-based VMs are simpler to implement (no register allocation needed), produce compact bytecode, and are well-suited to WASM. The major Lisp/Scheme VMs (Guile, Steel, Ketos) are all stack-based.

**Bytecode format:** `Vec<u8>` with variable-length operands. Alternative: `Vec<u32>` "wordcode" for simpler decoding at the cost of size.

#### Opcode inventory (minimum production set)

```rust
#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum Op {
    // Constants & stack
    Const,          // u16 const_index → push constants[i]
    Nil,            // push nil
    True,           // push #t
    False,          // push #f
    Pop,            // discard TOS
    Dup,            // duplicate TOS

    // Locals (slot-addressed within call frame)
    LoadLocal,      // u16 slot → push locals[slot]
    StoreLocal,     // u16 slot → locals[slot] = pop

    // Upvalues (captured variables from enclosing scopes)
    LoadUpvalue,    // u16 index → push upvalues[i].get()
    StoreUpvalue,   // u16 index → upvalues[i].set(pop)

    // Globals (module-level bindings)
    LoadGlobal,     // u32 spur → push globals[spur]
    StoreGlobal,    // u32 spur → globals[spur] = pop

    // Control flow
    Jump,           // i32 relative offset
    JumpIfFalse,    // i32 relative offset (pop condition)
    JumpIfNil,      // i32 relative offset (pop, check nil)

    // Function calls
    Call,           // u16 argc → call TOS-argc with argc args
    TailCall,       // u16 argc → tail call (reuse frame)
    Return,         // return TOS

    // Closures
    MakeClosure,    // u16 func_id, u16 n_upvalues, then n*(u8 kind, u16 idx)

    // Native function calls (fast path for known builtins)
    CallNative,     // u16 native_id, u16 argc

    // Data constructors
    MakeList,       // u16 n → pop n values, push list
    MakeVector,     // u16 n → pop n values, push vector
    MakeMap,        // u16 n_pairs → pop 2n values, push map
    MakeHashMap,    // u16 n_pairs → pop 2n values, push hashmap
    MakeRecord,     // u16 type_tag_spur, u16 n_fields

    // Exception handling
    Throw,          // pop value, throw as exception
    // Try/catch uses an exception table on the Chunk, not inline opcodes

    // Specialized arithmetic (fast paths — avoid boxing overhead)
    AddInt,         // pop 2 ints, push int (fallback to generic Add on type mismatch)
    SubInt,
    MulInt,
    LtInt,          // pop 2 ints, push bool
    EqInt,

    // Generic arithmetic
    Add,            // pop 2 values, push result (handles int/float/string)
    Sub,
    Mul,
    Div,
    Negate,
    Not,

    // Comparison
    Eq,             // pop 2, push bool
    Lt,
    Gt,
    Le,
    Ge,
}
```

> **Design note:** Specialized `AddInt`/`LtInt` opcodes avoid the overhead of dispatching through the generic `Value` type for the common case. The compiler can emit these when operand types are statically known (e.g., inside `(do ((i 0 (+ i 1))) ...)`). The VM falls back to generic ops on type mismatch.

#### Chunk and Function structs

```rust
/// A compiled code object.
#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub consts: Vec<Value>,
    pub spans: Vec<(u32, Span)>,  // sparse PC → source span mapping
    pub max_stack: u16,
    pub n_locals: u16,
    pub exception_table: Vec<ExceptionEntry>,
}

#[derive(Debug, Clone)]
pub struct ExceptionEntry {
    pub try_start: u32,   // PC range start
    pub try_end: u32,     // PC range end
    pub handler_pc: u32,  // jump target on exception
    pub stack_depth: u16, // stack depth to restore
}

/// A compiled function (template for closures).
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Spur>,
    pub chunk: Chunk,
    pub upvalue_descs: Vec<UpvalueDesc>,
    pub arity: u16,
    pub has_rest: bool,        // rest parameter (variadic)
    pub module_id: Option<u32>,
}

#[derive(Debug, Clone, Copy)]
pub enum UpvalueDesc {
    Local(u16),    // capture from parent's local slot index
    Upvalue(u16),  // capture from parent's upvalue index
}
```

### A2. Closures & Environments

**Current problem:** closures capture a full `Env` chain. Variable access is hash-lookup-per-scope-level.

**Compiled approach: lexical slots + upvalues** (the Lua/Steel model).

Each function has `n_locals` slots. The compiler resolves every variable reference to one of:

- **Local slot** — `LoadLocal slot`
- **Upvalue** — `LoadUpvalue idx` (captured from enclosing scope)
- **Global** — `LoadGlobal spur` (module-level binding)

#### How `set!` works with upvalues

If a local is both captured and mutated, it must be **boxed** (heap-allocated cell):

```rust
/// A mutable cell for variables that are both captured and mutated.
pub struct UpvalueCell {
    pub value: RefCell<Value>,
}

/// A runtime closure (function + captured upvalues).
pub struct Closure {
    pub func: Rc<Function>,
    pub upvalues: Vec<Rc<UpvalueCell>>,
}
```

When the compiler detects that a local is captured and mutated via `set!`:

1. The local slot holds an `Rc<UpvalueCell>` instead of a `Value` directly
2. `LoadLocal` / `StoreLocal` go through the cell indirection
3. `MakeClosure` captures the `Rc<UpvalueCell>` (shared reference)
4. Both parent and child see mutations

This is the standard approach used by Lua, Guile, Steel, and most modern Lisp/Scheme compilers.

#### Variable resolution pass (required new compiler phase)

```rust
#[derive(Debug, Clone)]
pub enum VarResolution {
    Local { slot: u16, mutable: bool, captured: bool },
    Upvalue { index: u16 },
    Global { spur: Spur },
}
```

The compiler performs a two-pass analysis:

1. **Resolve**: walk the AST, determine each variable's scope and whether it's captured
2. **Assign slots**: number locals sequentially, generate upvalue descriptors

This analysis pass is the **single biggest difference** between a tree-walking interpreter and a compiler. It eliminates the need for hash-based environment lookup entirely.

### A3. Value Representation

**Recommendation: keep current `Value` enum for VM v1.** Optimize later if benchmarks demand it.

The 22-variant enum works. Rc-cloning is the dominant cost, but compilation dramatically reduces clone frequency because:

- Locals live in slots (no hash map insertion/lookup)
- Constants are loaded by reference to the constant pool
- Fewer intermediate values are created

**Future optimization path (if needed):**

| Approach                      | Pros                                                 | Cons                                                               | Effort |
| ----------------------------- | ---------------------------------------------------- | ------------------------------------------------------------------ | ------ |
| **Keep `Value` enum**         | Zero migration; works now                            | Rc overhead on every access                                        | —      |
| **Tagged `u64` (NaN boxing)** | Fastest dispatch; 0 alloc for int/float/bool/nil     | Complex in Rust; WASM edge cases; heap objects still need Rc or GC | Large  |
| **Tagged pointer**            | Good middle ground; immediates for int/bool/nil/char | Still need heap for strings/lists/etc.                             | Medium |

**NaN boxing sketch (for reference — not recommended for v1):**

```
Bits 63..52 = 0x7FF8..0x7FFF → NaN space (12 bits for type tag + 48 bits payload)
Tag 0x7FF8 → nil
Tag 0x7FF9 → bool (payload bit 0 = true/false)
Tag 0x7FFA → int (48-bit signed integer, covers ±140 trillion)
Tag 0x7FFB → char (32 bits)
Tag 0x7FFC → symbol (Spur, 32 bits)
Tag 0x7FFD → keyword (Spur, 32 bits)
Tag 0x7FFE → heap pointer (48 bits — all complex types)
Normal float → f64 value directly (non-NaN)
```

This eliminates allocation for the most common types but requires careful Rust `unsafe` code and testing across platforms.

### A4. Mapping Special Forms

Special forms compile away into bytecode. They cease to exist at runtime:

| Special Form              | Compiles To                                      |
| ------------------------- | ------------------------------------------------ |
| `if`                      | `JumpIfFalse` + branches                         |
| `cond`                    | Cascaded `JumpIfFalse`                           |
| `and` / `or`              | Short-circuit jumps                              |
| `when` / `unless`         | `JumpIfFalse` / `JumpIfTrue` + body              |
| `begin`                   | Sequential instructions (no-op wrapper)          |
| `let` / `let*` / `letrec` | `StoreLocal` for bindings + body                 |
| `define` / `defun`        | `StoreGlobal` (or `StoreLocal` at non-top-level) |
| `set!`                    | `StoreLocal` / `StoreUpvalue` / `StoreGlobal`    |
| `lambda` / `fn`           | `MakeClosure`                                    |
| `quote`                   | `Const` (literal in constant pool)               |
| `quasiquote`              | Compile template with `Const` + `MakeList`       |
| `do`                      | Loop with `Jump` back + `JumpIfFalse` exit       |
| `case`                    | Cascaded comparisons or jump table               |
| `try` / `throw`           | Exception table entry + `Throw` opcode           |
| `delay` / `force`         | `MakeClosure` (thunk) / call + cache             |
| `define-record-type`      | Series of `StoreGlobal` for ctor/pred/accessors  |

**Forms that remain as runtime operations:**

| Special Form                                  | Why                     | Implementation                                          |
| --------------------------------------------- | ----------------------- | ------------------------------------------------------- |
| `eval`                                        | Inherently dynamic      | Native fn: invokes compiler + VM recursively            |
| `load` / `import`                             | File I/O + compilation  | Native fn: reads, compiles, caches module               |
| `macroexpand`                                 | Debug/meta tool         | Native fn: runs macro expander                          |
| `with-budget`                                 | Step counter management | Native fn: sets `ctx.eval_step_limit`                   |
| `prompt` / `message` / `deftool` / `defagent` | LLM data constructors   | Could compile to `MakeRecord`-like ops or remain native |

### A5. Mapping ~350 Native Functions

Native functions use a **stable calling convention**:

```rust
pub type NativeId = u16;

/// At initialization, each NativeFn gets a numeric ID.
/// The compiler emits `CallNative native_id argc`.
pub struct NativeRegistry {
    pub fns: Vec<Rc<NativeFn>>,
    pub name_to_id: HashMap<Spur, NativeId>,
}
```

The VM dispatch loop:

```rust
Op::CallNative => {
    let native_id = read_u16(&chunk.code, &mut pc);
    let argc = read_u16(&chunk.code, &mut pc);
    let args = &vm.stack[vm.sp - argc as usize .. vm.sp];
    let native = &vm.natives[native_id as usize];
    // Sandbox check is inside the native fn wrapper (unchanged from today)
    let result = (native.func)(ctx, args)?;
    vm.sp -= argc as usize;
    vm.stack[vm.sp] = result;
    vm.sp += 1;
}
```

This is a direct call with no name resolution at runtime — faster than the current approach of evaluating the head symbol, looking up the `NativeFn` in the env, then calling it.

### A6. Macros

**Compile-time macroexpansion** — same semantics as today, but explicitly separated into a phase:

```
Source text
  → Reader (tokenize + parse → Vec<Value>)
  → Macro expander (evaluate macros, produce expanded AST)
  → Compiler (lowering + slot allocation + bytecode generation)
  → VM execution
```

**The complication:** macros can call `eval`, define new macros, and run arbitrary code at expansion time. This means the macro expander needs access to an evaluator. Options:

1. **Use the tree-walking interpreter for macro expansion** (simplest, recommended for v1). Keep `sema-eval` as the "macro-time evaluator." This is what most practical Lisps do.
2. **Use the VM itself for macro expansion** (circular dependency — need to bootstrap). More complex but eventually cleaner.
3. **Restrict macros to a safe subset** (not viable — breaks existing code).

**Recommended approach:** Option 1. The tree-walker stays as the macro-expansion engine. The bytecode compiler runs after expansion. This means `sema-eval` is not deleted — it becomes the compile-time evaluator.

### A7. Module System

Modules become the **unit of compilation and caching**:

```rust
pub struct CompiledModule {
    pub functions: Vec<Rc<Function>>,  // all functions defined in this module
    pub entry: usize,                  // index of the top-level "init" function
    pub exports: Vec<(Spur, u16)>,     // exported name → global slot
    pub source_path: PathBuf,
    pub debug: DebugInfo,
}
```

**Compilation flow for `(import "path")`:**

1. Resolve path (same logic as today)
2. Check `ctx.module_cache` for compiled module
3. If not cached: read file → expand macros → compile → cache
4. Execute module's entry function to populate globals
5. Copy exported bindings into importer's env

**Cyclic import detection:** same algorithm as today (stack of paths being loaded).

### A8. Tail Call Optimization

The VM is the **easiest** place to implement real TCO:

```rust
Op::TailCall => {
    let argc = read_u16(&chunk.code, &mut pc);
    // Shift args to beginning of current frame's locals
    let args_start = vm.sp - argc as usize;
    for i in 0..argc as usize {
        vm.frames.last_mut().locals[i] = vm.stack[args_start + i].clone();
    }
    // Reset frame: new closure, PC = 0, same base
    let callee = vm.stack[args_start - 1].clone();  // the function being called
    match callee {
        Value::Closure(c) => {
            let frame = vm.frames.last_mut();
            frame.closure = c;
            frame.pc = 0;
            vm.sp = frame.base + frame.closure.func.chunk.n_locals as usize;
            // Continue dispatch loop — no new frame pushed
        }
        _ => { /* handle native calls, errors, etc. */ }
    }
}
```

The compiler identifies tail positions and emits `TailCall` instead of `Call` + `Return`.

### A9. LLM Primitive Integration

LLM primitives remain **native functions** called via `CallNative`. They are I/O-bound; compilation won't help their performance. What matters:

- Preserve sandbox capability checks (unchanged — sandbox.check() happens inside native fn)
- Preserve error context and stack traces (VM maintains call stack with spans)
- On WASM: same `cfg(not(wasm32))` gating as today

LLM-specific `Value` variants (Prompt, Message, Conversation, ToolDef, Agent) remain as-is. They're constructed by native functions and stored in the constant pool or on the stack like any other value.

### A10. GC Implications

**`Rc` without cycle collection is the biggest risk for a production-quality compiler.**

With the tree-walker, cycles are rare because environments are created and destroyed in a stack-like pattern. With compiled closures and upvalue cells, long-lived closure graphs become common:

```scheme
;; This creates a cycle: f captures env containing f
(define f (lambda () f))
```

**Options:**

| GC Strategy               | Pros                          | Cons                                      | Effort  |
| ------------------------- | ----------------------------- | ----------------------------------------- | ------- |
| **Keep `Rc` (no cycles)** | Zero effort; works today      | Leaks on closure cycles; not "production" | —       |
| **`Rc` + cycle detector** | Targeted fix; low disruption  | Periodic scan is O(n objects); limited    | Small   |
| **Tracing mark-sweep**    | Correct; standard for VMs     | Requires root enumeration; pause times    | 3–6 PM  |
| **Tracing generational**  | Best throughput; short pauses | Complex implementation                    | 6–12 PM |

**Recommendation:** For VM v1, keep `Rc` and document the cycle limitation. For production, implement a simple **mark-sweep GC** with the VM stack as roots. The VM naturally provides root enumeration (stack + global env + module cache), making tracing straightforward.

**Mark-sweep integration sketch:**

```rust
pub struct GcHeap {
    objects: Vec<Box<dyn GcObject>>,
    roots: Vec<*const dyn GcObject>,
    threshold: usize,
}

trait GcObject {
    fn trace(&self, tracer: &mut dyn FnMut(*const dyn GcObject));
    fn size(&self) -> usize;
}
```

### A11. WASM Compatibility

The bytecode VM is inherently WASM-compatible:

- Same bytecode executes identically on all platforms
- Same sandbox model
- Same debug metadata
- No platform-specific code generation
- The existing WASM playground at sema.run would use the VM directly

This is the **strongest argument** for VM over native compilation.

### A12. Debugging & Error Reporting

#### Source mapping

```rust
/// Sparse mapping from bytecode PC to source location.
pub struct DebugInfo {
    pub pc_to_span: Vec<(u32, Span)>,   // sorted by PC
    pub local_names: Vec<(u16, Spur)>,   // slot → variable name (for debugger)
    pub source_path: Option<PathBuf>,
}
```

Binary search on `pc_to_span` to find the source location for any PC. This replaces the current `SpanMap` (keyed by `Rc` pointer address, which is fragile).

#### Stack traces

The VM maintains a call stack just like `EvalContext` does today:

- Push frame on `Call` / `CallNative`
- Pop frame on `Return`
- On error: walk frames, look up PC → span for each, produce stack trace

#### Disassembler

Essential for development — a `disassemble(chunk: &Chunk) -> String` function that produces human-readable output:

```
  0000  CONST 0          ; 1
  0003  CONST 1          ; 2
  0006  ADD_INT
  0007  RETURN
```

### A13. Performance Expectations

Based on benchmarks of similar systems (Steel, Ketos, Guile):

| Workload                    | Tree-walker | Bytecode VM              | Speedup        |
| --------------------------- | ----------- | ------------------------ | -------------- |
| 1BRC (1M rows)              | ~1000ms     | ~100–200ms               | 5–10×          |
| Fibonacci(35)               | ~2s (est.)  | ~200ms                   | ~10×           |
| Tight loop (10M iterations) | ~5s (est.)  | ~400ms                   | ~12×           |
| LLM API call                | ~500ms      | ~500ms                   | 1× (I/O bound) |
| Module loading              | ~10ms       | ~15ms first, ~2ms cached | 0.7–5×         |

The biggest wins come from:

1. **Eliminating env chain lookups** (slot-based locals are O(1))
2. **Eliminating per-scope allocation** (stack frame instead of `Rc<RefCell<HashMap>>`)
3. **Reducing clone/RC overhead** (fewer intermediate `Value` clones)
4. **Specialized arithmetic** (`AddInt` avoids `Value` match dispatch)

### A14. Risks & Dealbreakers

| Risk                 | Severity | Mitigation                                      |
| -------------------- | -------- | ----------------------------------------------- |
| **GC cycles**        | High     | Document as known limitation; plan tracing GC   |
| **Macro semantics**  | Medium   | Keep tree-walker for macro expansion            |
| **`eval` builtin**   | Medium   | `eval` invokes compiler+VM recursively          |
| **Debugging parity** | Medium   | Invest in span mapping + disassembler early     |
| **Feature parity**   | High     | Must support all 35 special forms + 350 natives |
| **REPL experience**  | Medium   | Compile per-expression; cache nothing           |

### A15. Effort Estimate (Bytecode VM)

| Component                                                             | Lines of Code | Person-Months |
| --------------------------------------------------------------------- | ------------- | ------------- |
| Compiler pipeline (lowering, slot allocation, upvalues, control flow) | ~3,000        | 3–5           |
| VM runtime (dispatch loop, frames, TCO, exceptions, native ABI)       | ~2,000        | 2–4           |
| Debug metadata, stack traces, disassembler                            | ~800          | 1–2           |
| Testing (port all 712 tests to verify identical behavior)             | ~2,000        | 1–2           |
| GC (if implemented)                                                   | ~1,500        | 3–6           |
| **Total**                                                             | **~9,000**    | **~6–12**     |

---

## Strategy B: Native Code Compilation

### B1. Tooling Options

#### Cranelift (recommended if going native)

```rust
// Cargo.toml
cranelift = "0.119"
cranelift-module = "0.119"
cranelift-jit = "0.119"
cranelift-native = "0.119"
```

**Pros:**

- Rust-native integration (no C++ FFI)
- Designed for JIT use cases
- Fast compile times (~10ms per function)
- Good for x86_64 + AArch64

**Cons:**

- No WASM output target (Cranelift _consumes_ WASM, doesn't _produce_ it)
- Less optimization than LLVM (no autovectorization, limited inlining)
- Younger ecosystem; API changes between versions

**Steel uses Cranelift** — their `jit2/cgen.rs` is ~1,500 LOC for compiling bytecode to native code. This validates the approach for Scheme-like languages.

#### LLVM (via inkwell)

```rust
// Cargo.toml
inkwell = { version = "0.5", features = ["llvm17-0"] }
```

**Pros:**

- Best optimization potential
- Can target WASM (via `wasm32-unknown-unknown`)
- Can target every platform Sema might care about
- Mature; battle-tested

**Cons:**

- Massive dependency (LLVM is ~100MB+ of C++)
- Slow compile times (~100ms+ per function)
- Complex API surface
- Harder to integrate with Rust (C++ FFI via llvm-sys)
- LLVM version pinning is painful

#### GCC libgccjit

**Verdict: ❌ Not recommended.** Poor Rust ecosystem fit, limited portability, weak WASM story.

#### Custom x86_64/AArch64 codegen

**Verdict: ❌ Not recommended** unless going for a LuaJIT-style project. Enormous effort for uncertain gain over Cranelift.

#### C as intermediate language (Chicken Scheme approach)

**How it works:**

1. CPS-transform the program (all calls become tail calls)
2. Generate C source code
3. Compile with system C compiler

**Chicken's "Cheney-on-the-MTA" technique:** use the C stack as a nursery; when it overflows, copy all live data to the heap and longjmp back. This gives proper tail recursion in C.

**Verdict: ⚠️ Interesting but poor fit for Sema.** Two-stage compilation (Sema → C → binary) is slow. No JIT possible. Requires C compiler on every target. Does give excellent portability via C's ubiquity.

### B2. Native Compilation Architecture

If Cranelift is chosen:

```
Sema source
  → Reader (parse)
  → Macro expander (using tree-walker)
  → Lowering to Core IR (same as bytecode compiler)
  → Cranelift IR generation
  → Machine code (in-memory)
  → Execute
```

The **calling convention** for compiled Sema functions:

```rust
// All compiled functions use this signature
fn(ctx: *mut EvalContext, args: *const Value, argc: u32) -> Value
```

**Or** for known-arity functions with no rest params:

```rust
fn(ctx: *mut EvalContext, arg0: Value, arg1: Value) -> Value
```

### B3. Closures in Native Code

Same upvalue model as the VM, but implemented in generated machine code:

- Locals live in the machine stack frame
- Captured-and-mutated locals are boxed into heap cells
- Closure object: code pointer + array of upvalue cell pointers

### B4. TCO in Native Code

This is **harder** than in a VM:

- LLVM has `musttail` but it's restrictive (same calling convention, same return type)
- Cranelift has no built-in tail call support
- Dynamic dispatch (calling an unknown closure) makes tail calls especially hard

**Practical approach:** implement a **trampoline** in the runtime, same as today:

```rust
enum TrampolineResult {
    Done(Value),
    TailCall { closure: *const Closure, args: Vec<Value> },
}
```

This means native compilation **does not automatically solve TCO** better than a VM.

### B5. WASM Target Compatibility

**This is the critical differentiator against native compilation.**

| Backend         | x86_64 | AArch64 | WASM      |
| --------------- | ------ | ------- | --------- |
| **Bytecode VM** | ✅     | ✅      | ✅        |
| **Cranelift**   | ✅     | ✅      | ❌        |
| **LLVM**        | ✅     | ✅      | ✅ (slow) |
| **C as IR**     | ✅     | ✅      | ❌        |

If WASM support is a hard requirement (sema.run playground), native compilation via Cranelift requires a **dual backend**: VM for WASM, native for desktop. This doubles testing and maintenance burden.

### B6. Performance Expectations

| Workload      | Tree-walker | Bytecode VM | Native (Cranelift) | Native (LLVM) |
| ------------- | ----------- | ----------- | ------------------ | ------------- |
| 1BRC (1M)     | ~1000ms     | ~100–200ms  | ~50–100ms          | ~30–80ms      |
| Fibonacci(35) | ~2s         | ~200ms      | ~80ms              | ~50ms         |
| LLM API call  | ~500ms      | ~500ms      | ~500ms             | ~500ms        |

Native compilation is typically **1.5–3× faster** than a well-implemented bytecode VM. The gap narrows with specialized VM opcodes and widens with numeric-heavy workloads.

**Key insight:** unless Sema is used for compute-heavy workloads (it's primarily an LLM scripting language), the VM speedup is likely sufficient. The incremental gain from native compilation may not justify the complexity.

### B7. Effort Estimate (Native Compilation)

| Component                                           | Person-Months |
| --------------------------------------------------- | ------------- |
| Lowered IR + Cranelift codegen + calling convention | 6–10          |
| Closure/upvalue compilation                         | 2–4           |
| Debug metadata + stack traces                       | 2–4           |
| GC (required — no choice with native)               | 3–6           |
| WASM dual-backend testing                           | 3–6           |
| **Total**                                           | **~12–24+**   |

---

## Comparative Analysis: Existing Implementations

### What other Rust Lisp/Scheme implementations did

| Project            | Strategy                             | Value Variants     | GC                        | LOC      | WASM    |
| ------------------ | ------------------------------------ | ------------------ | ------------------------- | -------- | ------- |
| **Ketos**          | Stack-based bytecode                 | 18                 | Rc                        | ~2K (VM) | ❌      |
| **Steel**          | Stack-based bytecode + Cranelift JIT | 36+                | Rc + experimental tracing | ~10K     | Partial |
| **Guile 3.0**      | Stack-based bytecode + Lightning JIT | Tagged pointers    | Mark-sweep                | ~50K+    | ❌      |
| **Chez Scheme**    | Nanopass → native (register-based)   | Tagged pointers    | Generational              | ~100K+   | ❌      |
| **Chicken**        | CPS → C source                       | Tagged pointers    | Cheney-on-the-MTA         | ~30K+    | ❌      |
| **Sema (current)** | Tree-walking + trampoline            | 22 (Rc-based enum) | Rc                        | ~46K     | ✅      |

### Key lessons from each

**Ketos:** Proves that ~2K LOC is enough for a functional bytecode VM in Rust. Their instruction set has ~70 opcodes. Uses `Rc` throughout — no GC.

**Steel:** Most relevant comparison — Scheme in Rust with bytecode + Cranelift JIT. Key insight: their identifier analysis pass (determining Local/Captured/Global for each variable) is the critical compilation phase. Their JIT wrapper is ~1,500 LOC. They have 300+ native functions (similar to Sema's 350+).

**Guile 3.0:** Shows that bytecode + JIT is sufficient for production Scheme. Their transition from tree-walking to bytecode (Guile 2.0 → 3.0) took multiple years but delivered ~10× speedup.

**Chez Scheme:** Gold standard for native compilation. The nanopass framework (many small compiler passes) is elegant but extremely complex (~100K+ LOC). Shows that native compilation requires a fundamentally different GC (generational) and value representation (tagged pointers, not boxed enum).

**Chicken:** Proves that C-as-IR works for production Scheme but has poor interactive/REPL experience. The Cheney-on-the-MTA technique is clever but only applicable if you own the runtime stack.

---

## Proposed Compilation Pipeline

```
┌─────────────────────────────────────────────────────────┐
│                    Current Pipeline                      │
│  Source → Reader → Value AST → eval_step (tree-walk)    │
└─────────────────────────────────────────────────────────┘
                          ↓ (proposed)
┌─────────────────────────────────────────────────────────┐
│                   Compiled Pipeline                      │
│                                                          │
│  Source                                                  │
│    → Reader (unchanged — produces Vec<Value>)            │
│    → Macro Expander (uses tree-walker for macro bodies)  │
│    → Core Lowering (desugar special forms → core AST)    │
│    → Variable Resolver (local/upvalue/global analysis)   │
│    → Bytecode Compiler (emit Chunk)                      │
│    → VM Execution (dispatch loop)                        │
│                                                          │
│  Optional native tier:                                   │
│    → Bytecode → Cranelift IR → Machine Code              │
└─────────────────────────────────────────────────────────┘
```

### Core AST (intermediate representation after desugaring)

```rust
/// Desugared core language — no macros, no syntactic sugar.
pub enum CoreExpr {
    Const(Value),                                    // literal
    Var(VarRef),                                     // variable reference
    If(Box<CoreExpr>, Box<CoreExpr>, Box<CoreExpr>), // if-then-else
    Begin(Vec<CoreExpr>),                            // sequencing
    Set(VarRef, Box<CoreExpr>),                      // mutation
    Lambda(LambdaDef),                               // closure creation
    Call(Box<CoreExpr>, Vec<CoreExpr>),               // function application
    TailCall(Box<CoreExpr>, Vec<CoreExpr>),           // tail-position call
    Let(Vec<(u16, CoreExpr)>, Vec<CoreExpr>),         // parallel binding
    Letrec(Vec<(u16, CoreExpr)>, Vec<CoreExpr>),      // recursive binding
    Do(DoLoop),                                      // iteration
    Try(Box<CoreExpr>, u16, Box<CoreExpr>),           // try/catch
    Throw(Box<CoreExpr>),                            // throw
    MakeList(Vec<CoreExpr>),                          // list constructor
    MakeVector(Vec<CoreExpr>),                        // vector constructor
    MakeMap(Vec<(CoreExpr, CoreExpr)>),               // map constructor
    MakeRecord(Spur, Vec<CoreExpr>),                  // record constructor
    CallNative(NativeId, Vec<CoreExpr>),              // direct native call
}

pub struct VarRef {
    pub name: Spur,
    pub resolution: VarResolution,
}

pub struct LambdaDef {
    pub params: Vec<Spur>,
    pub rest: Option<Spur>,
    pub body: Vec<CoreExpr>,
    pub upvalues: Vec<UpvalueDesc>,
    pub n_locals: u16,
}

pub struct DoLoop {
    pub vars: Vec<(u16, CoreExpr, Option<CoreExpr>)>, // (slot, init, step)
    pub test: Box<CoreExpr>,
    pub result: Vec<CoreExpr>,
    pub body: Vec<CoreExpr>,
}
```

This IR serves as the boundary between the macro expander and the bytecode compiler. It can also serve as input to a future Cranelift backend.

---

## Lowering Example

```scheme
(let ((x 1) (y 2))
  (+ x y))
```

**After variable resolution:**

- `x` → Local slot 0
- `y` → Local slot 1
- `+` → Global (native fn)

**Bytecode:**

```
0000  CONST 0          ; push 1
0003  STORE_LOCAL 0     ; x = 1
0006  CONST 1          ; push 2
0009  STORE_LOCAL 1     ; y = 2
000C  LOAD_LOCAL 0      ; push x
000F  LOAD_LOCAL 1      ; push y
0012  ADD_INT            ; x + y (specialized int add)
0013  RETURN
```

Compare to tree-walker: allocates `Env::with_parent`, inserts "x" and "y" into a `HashMap`, then looks them up by hash for `(+ x y)`. The bytecode version does zero allocation and zero hashing.

### Closure Example

```scheme
(define (make-counter)
  (let ((n 0))
    (lambda () (set! n (+ n 1)) n)))
```

**Variable resolution:**

- `n` in `make-counter`: Local slot 0, **captured and mutated** → boxed
- `n` in inner lambda: Upvalue 0

**`make-counter` bytecode:**

```
0000  CONST 0          ; push 0
0003  BOX               ; wrap in UpvalueCell (because n is captured + mutated)
0004  STORE_LOCAL 0     ; n = <cell containing 0>
0007  MAKE_CLOSURE 1, 1  ; func_id=1, 1 upvalue
000A  UPVALUE_LOCAL 0    ; capture from local slot 0
000D  RETURN
```

**Inner lambda (func_id=1) bytecode:**

```
0000  LOAD_UPVALUE 0    ; push n's cell
0003  DEREF              ; get current value
0004  CONST 0            ; push 1
0007  ADD_INT
0008  LOAD_UPVALUE 0    ; push n's cell again
000B  SET_CELL           ; store new value in cell
000C  LOAD_UPVALUE 0    ; push n's cell
000F  DEREF              ; get current value (the incremented one)
0010  RETURN
```

---

## Phased Roadmap

### Phase 1: Compiler Foundation (3–4 PM)

1. **Core AST definition** (`CoreExpr` enum)
2. **Variable resolver** (local/upvalue/global analysis)
3. **Macro expansion phase** (separate from eval — extract `defmacro` + `quasiquote` expansion into its own function that uses the tree-walker)
4. **Bytecode compiler** (CoreExpr → Chunk)
5. **Disassembler** (Chunk → human-readable output)
6. **Test**: compile + disassemble all existing integration test expressions, verify output

### Phase 2: VM Runtime (2–3 PM)

1. **VM dispatch loop** (decode + execute opcodes)
2. **Call frames, stack management, TCO**
3. **Native function ABI** (`CallNative`)
4. **Exception handling** (try/throw + exception table)
5. **Test**: run all 712 existing tests through compile→VM path, verify identical results

### Phase 3: Integration (1–2 PM)

1. **Wire into `Interpreter`** — add `eval_compiled` path alongside `eval_value`
2. **Module compilation + caching**
3. **REPL integration** (compile per-expression)
4. **`eval` builtin** (invokes compiler + VM recursively)
5. **WASM playground** integration
6. **Benchmark**: 1BRC comparison

### Phase 4: Optimization (1–2 PM)

1. **Specialized arithmetic opcodes** (`AddInt`, `LtInt`, etc.)
2. **Constant folding** in the compiler
3. **Inline caching** for `CallNative` sites (if profiling shows benefit)
4. **NaN boxing or tagged values** (if profiling shows `Rc` overhead is dominant)

### Phase 5 (Optional): Native Tier (6–12 PM)

1. **Cranelift backend** (compile hot functions from bytecode to native)
2. **Profiling integration** (identify hot functions)
3. **Tracing GC** (required for long-running programs)
4. **Dual-backend testing** (VM for WASM, native for desktop)

---

## Prerequisites Before Starting

1. **Decide on `eval` environment semantics** — the single most impactful design decision. Does `eval` see lexical locals (hard: requires frame reification) or only globals (simpler: standard in many Lisps, but breaking change if current code uses `eval` with locals)?
2. **Decide on macro phase semantics** — are macros compile-time-only (language change, simplifies everything) or runtime-expanded (preserves current semantics, requires tree-walker at runtime and prevents module compilation caching)?
3. **Decide on GC strategy** — accept Rc limitations for v1, or commit to tracing GC from the start?
4. **Resolve the mini-eval problem** — the 620-line mini-evaluator in `list.rs` must be deleted or restructured before compilation work begins (see Scrutiny Findings below)
5. **Define Core AST** — agree on the desugared intermediate representation
6. **Profile the actual hot path** — use flamegraphs to quantify: % time in Env::get, % in Value clone / Rc refcount, % in list/vector construction, % in native calls. This determines whether the VM's slot-based locals are the dominant win or whether Rc/Value traffic remains the bottleneck.

---

## Scrutiny Findings

> This section was added after rigorous review by 3 parallel analysis agents + oracle consultation. It documents inaccuracies, missing considerations, and critical risks in the original research above.

### Factual Corrections

| Claim in document                        | Actual                                                                                                                                                                                                         | Severity                                                   |
| ---------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------- |
| "22 Value variants"                      | **23 variants** (Nil, Bool, Int, Float, String, Symbol, Keyword, Char, List, Vector, Map, HashMap, Lambda, Macro, NativeFn, Prompt, Message, Conversation, ToolDef, Agent, Thunk, Record, Bytevector)          | Medium                                                     |
| "~350 native functions"                  | **~380** (321 stdlib + 59 sema-llm builtins)                                                                                                                                                                   | Low                                                        |
| "~1000ms for 1BRC 1M rows"               | **~1,600ms** (codebase comment says "1.6s on 1M rows"; benchmarks/1brc/results.json shows 15,564ms for 10M rows, extrapolating to ~1,556ms for 1M)                                                             | **High** — performance projections built on wrong baseline |
| "`Env.clone()` is expensive"             | `Env.clone()` is **2 Rc bumps** (bindings + parent are both `Rc`). The real cost is `Env::with_parent` allocating a new empty HashMap per scope, not cloning                                                   | Medium                                                     |
| "EvalContext replaced all thread_locals" | **4 thread_locals remain**: `SpecialFormSpurs` cache in special_forms.rs, interner `INTERNER` in value.rs, `EVAL_FN`/`CALL_FN` callbacks in context.rs (mini-eval's `SpecialFormSpurs` in list.rs was deleted) | Low                                                        |
| "Closures capture full Env (clone)"      | Env clone is cheap (Rc bump), but both closures from the same scope **share the same bindings HashMap** via `Rc<RefCell>` — this is accidental shared mutability that the upvalue model makes explicit         | Medium                                                     |

### ~~Critical Issue 1: The Mini-Eval (620 LOC) Is a Blocker~~ — **RESOLVED**

> **Status:** Fixed. The mini-eval has been deleted and replaced with a callback architecture.

The 620-line `sema_eval_value` and `call_function` in `sema-stdlib/src/list.rs` have been replaced with thread-local `eval_callback`/`call_callback` in `sema-core`, registered by `sema-eval` during interpreter init. All 18+ callback-accepting stdlib functions now invoke the real evaluator. Net change: **-814 lines**.

**Performance trade-off:** 1BRC benchmark regressed from ~960ms → ~3050ms (3.2×) on 1M rows. The full evaluator's per-expression overhead (depth tracking, trampoline setup, Value cloning) is higher than the mini-eval's direct recursive evaluation. This is acceptable — the mini-eval had semantic drift (set! silently created bindings, define didn't parse rest params) and was the primary blocker for the bytecode VM transition.

**Future:** The evaluator's fast path can be optimized (skip depth tracking for inner expressions, avoid Value cloning for self-evaluating forms) to recover most of the lost performance.

### Critical Issue 2: Macro Phase Semantics Are Unsound

The document proposes "compile-time macroexpansion" with the tree-walker evaluating macro bodies. This has several unresolved problems:

1. **Macros are runtime in Sema today.** `Value::Macro(mac)` is invoked at runtime during `eval_step` when encountered. `apply_macro` evaluates the macro body in the _caller's_ environment. Macros can read runtime variables, perform I/O, call `eval`, and have side effects. A "compile-time-only" phase would be a **language change**.

2. **Callable interop breaks.** If a macro body calls a user-defined function, and that function exists only as compiled bytecode (not as `Value::Lambda`), the tree-walker macro expander can't call it. The tree-walker must be taught to invoke compiled closures, which means **the VM must be callable from the tree-walker** — creating circular runtime dependency.

3. **Environment bifurcation.** The tree-walker uses `Env` chains; the VM uses slots. If the macro expander runs in the tree-walker, it sees `Env`-based globals. If the VM has updated globals via `StoreGlobal`, the macro expander may see stale values unless they share a single global store.

**Required resolution:** Choose one:

- **Option A (preserve semantics):** Keep macros as runtime operations. VM detects `Value::Macro` callables and invokes the tree-walker for expansion, then compiles and executes the result. Slower but correct.
- **Option B (language change):** Restrict macros to compile-time-only. `defmacro` only allowed at top-level. Macro bodies can only reference other macros and compile-time constants. Breaks existing code that uses runtime macros.

### Critical Issue 3: `eval` Creates a Dual-Runtime Coherence Problem

The document says `eval` "invokes compiler+VM recursively." But the hard questions are unanswered:

1. **What environment does `eval` see?** Today, `eval` evaluates in the caller's `Env` (which includes lexical locals). In a compiled world, locals are in stack slots, not an `Env` map. Does `eval` see them? If yes, the VM must **reify** the current frame into a temporary `Env` (deopt bridge). If no, this is a breaking semantic change.

2. **`eval` + `defmacro` invalidates compilation caching.** If `eval` defines a macro at runtime, subsequent code depends on that macro. Module-level compilation caching becomes invalid unless you track macro environment versions.

3. **`eval` + closures.** Code evaluated via `eval` can create closures that capture the caller's bindings. With slot-based locals, the capture mechanism must work across the eval boundary — closures created by `eval` can't reference slot indices that only exist in a compiled frame.

### Critical Issue 4: Three Independent Evaluators With Semantic Drift

The codebase has **three separate eval implementations**:

1. `eval_value` in `sema-eval/src/eval.rs` — full evaluator (35 special forms, TCO, depth limits, call stack)
2. `sema_eval_value` in `sema-stdlib/src/list.rs` — mini-eval (14 special forms, no TCO, no depth limits, different `set!` semantics)
3. `call_value_fn` + `full_eval` callback in `sema-llm/src/builtins.rs` — LLM eval (delegates to tree-walker via callback, creates Lambda clones instead of Rc::clone for self-reference)

These have **known semantic differences**. Adding a VM creates a **fourth** eval path. The document must address unification, not just addition.

### Critical Issue 5: Self-Reference Creates Rc Cycles in the Upvalue Model

In `apply_lambda` (eval.rs:381-383), every named function call binds the function's own name to a `Value::Lambda(Rc::clone(lambda))`. In the current tree-walker, `Env.clone()` creates a separate Rc chain, so cycles are avoided.

In the proposed upvalue model, self-reference becomes an upvalue cell containing the closure itself: `Closure → upvalues → UpvalueCell → Value::Closure → upvalues → ...`. This **is** an Rc cycle. The document's GC section doesn't address this specific mechanism, which is the **most common** source of long-lived reference chains.

### Critical Issue 6: `Lambda.params` Is `Vec<String>`, Not `Vec<Spur>`

Every function call currently interns each parameter name: `sema_core::intern(param)` for each param on every call. For a 3-param function called 1M times, that's 3M interner lookups. The proposed `Function` struct uses `Spur` for names, but the transition requires changing `Lambda` in `sema-core` — a breaking change to the core type.

### High-Severity Missing Details

1. **Tail position analysis rules** — the document says "compiler identifies tail positions" but never specifies the rules. Needed for: `begin` (only last expr), `let/let*/letrec` (only last body expr), `cond` (only last clause result expr), `and/or` (only last operand), `when/unless` (only last body expr), `do` (only result exprs), `try` (NOT tail — exception handler must be reachable).

2. **Keyword-as-function compilation** — `(:key map)` is callable today. The opcode table has no `KeywordGet` opcode. Must either add one or compile to `(get map :key)` with identical semantics (nil default, map+hashmap dispatch).

3. **Quasiquote with `unquote-splicing`** — cannot be "compiled template with Const + MakeList." Splicing changes list structure dynamically. Must lower to `cons`/`append`/`list` combinations during expansion, not at bytecode level.

4. **`define-record-type` generates closures at runtime** — constructor, predicate, and accessor functions are created as `NativeFn` closures. In a compiled world, these should be lowered to `defun`-like forms during the lowering phase, then compiled normally.

5. **`load` vs `import` semantics** — `load` evaluates in the CURRENT env (pollutes caller's namespace, no caching). `import` evaluates in a fresh module env (isolated, cached). The document barely discusses `load`, which is semantically very different.

6. **WASM binary size impact** — adding compiler + VM crates to the WASM build increases the binary. Currently sema.run includes sema-core + sema-reader + sema-eval + sema-stdlib. Adding a compiler increases payload by an estimated 30-50%.

7. **REPL global store** — compiled REPL expressions share persistent globals via `eval_in_global`. The slot model works for locals but `define`/`defmacro` must persist in a global store shared between the VM, tree-walker (for macros), and all native functions.

### Revised Effort Estimate

| Component                                  | Original Estimate | Revised Estimate | Delta Reason                                                                          |
| ------------------------------------------ | ----------------- | ---------------- | ------------------------------------------------------------------------------------- |
| Compiler pipeline                          | 3–5 PM            | 3–5 PM           | Unchanged                                                                             |
| VM runtime                                 | 2–4 PM            | 2–4 PM           | Unchanged                                                                             |
| Debug metadata + disassembler              | 1–2 PM            | 1–2 PM           | Unchanged                                                                             |
| Testing (712 tests)                        | 1–2 PM            | 1–2 PM           | Unchanged                                                                             |
| GC (if implemented)                        | 3–6 PM            | 3–6 PM           | Unchanged                                                                             |
| **Mini-eval deletion + callback re-entry** | **Not estimated** | **1–2 PM**       | 620-line evaluator must be replaced; 18+ stdlib functions need callback restructuring |
| **Macro/eval semantics design**            | **Not estimated** | **0.5–1 PM**     | Critical design decisions + compatibility testing                                     |
| **WASM integration**                       | **Not estimated** | **0.5–1 PM**     | VFS interaction, binary size, playground wiring                                       |
| **Lambda/core type changes**               | **Not estimated** | **0.5 PM**       | `Lambda.params` Vec<String> → Vec<Spur>, etc.                                         |
| **Original total**                         | **6–12 PM**       | —                | —                                                                                     |
| **Revised total**                          | —                 | **~10–18 PM**    | More realistic for a single developer                                                 |

### Revised Performance Expectations

Based on corrected baseline (~1,600ms not ~1,000ms):

| Workload       | Tree-walker (actual) | Bytecode VM (projected) | Speedup        |
| -------------- | -------------------- | ----------------------- | -------------- |
| 1BRC (1M rows) | ~1,600ms             | ~160–320ms              | 5–10×          |
| Fibonacci(35)  | ~2s (est.)           | ~200ms                  | ~10×           |
| LLM API call   | ~500ms               | ~500ms                  | 1× (I/O bound) |

**Caveat:** The 5–15× claim may be overstated. Sema's hot path already has significant optimizations (Spur-based dispatch, hashbrown, memchr, inlined mini-eval). The remaining overhead may be dominated by `Rc` refcount traffic and `Value` cloning, which a bytecode VM doesn't automatically eliminate. **Profile before committing.**

---

## Open Questions

- **Should the tree-walker be preserved?** Required yes — it serves as the macro expander and must be able to call compiled closures (the VM). The two runtimes must interoperate.
- **What about `eval`?** Must invoke the full compiler pipeline at runtime. The compiler must be part of the runtime binary (including WASM). The hardest question: does `eval` see lexical locals or only globals?
- **Can we AOT-compile to disk?** Eventually, but module compilation caching is invalid if macros can be defined dynamically. Needs a cache invalidation strategy.
- **Thread safety?** Compilation doesn't change the single-threaded model. `Rc` stays. The interner's `thread_local!` means `Spur` values are thread-bound — relevant if parallelism is ever considered.
- **Specialized arithmetic opcodes** — the document proposes `AddInt` "when operand types are statically known," but in a dynamic Lisp, the compiler rarely knows types. Reframe as runtime-guarded fast paths, not static type inference.

---

## What NOT To Do

- **Don't build a register-based VM.** Register allocation adds ~30% complexity for marginal gains in a dynamically-typed Lisp. Stack-based is simpler and proven (Lua, Guile, Steel, CPython, JVM).
- **Don't switch to LLVM for v1.** The dependency weight, build complexity, and API surface are not justified when a VM delivers 5–10× speedup.
- **Don't implement continuations.** Sema doesn't have `call/cc`. Don't add compilation complexity for a feature that doesn't exist.
- **Don't change Value representation in the same project.** Compilation and NaN boxing are independent improvements. Do them separately to manage risk.
- **Don't delete the tree-walker.** It's the macro expander, the `eval` fallback, and the safety net. It must be taught to invoke compiled closures.
- **Don't underestimate the mini-eval.** It's 620 lines of duplicated evaluator logic in stdlib that must be dealt with before the VM can work correctly.
