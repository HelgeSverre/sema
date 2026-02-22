# VM Runtime Limitations — Tree-Walker Coupling

**Date:** 2026-02-20
**Context:** Investigated for `sema build` standalone executables (issue #9)

## Summary

The bytecode VM cannot operate independently of the tree-walking interpreter (`sema-eval`) and parser (`sema-reader`). This prevents building a slim "runtime-only" binary that excludes the compiler and tree-walker. The full `sema` binary (~10-15MB) must be used as the runtime base for `sema build`.

## Root Cause: Eval Callback Mechanism

`Interpreter::new()` registers the tree-walker as a callback via `sema_core::set_eval_callback()`. This callback is invoked by:

1. **stdlib HOFs** — `map`, `filter`, `foldl`, `for-each`, `sort` (with comparator), etc. call `call_callback()` in `sema-stdlib/src/list.rs:1200` to invoke lambda arguments.
2. **VM delegate functions** — 14 `__vm-*` native functions registered in `sema-eval/src/eval.rs:692-973` delegate runtime operations to the tree-walker.
3. **LLM tool execution** — Tools defined as lambdas execute via `full_eval()` in `sema-llm/src/builtins.rs`.

## Specific Coupling Points

### stdlib HOFs (sema-stdlib → sema-eval)

`sema-stdlib/src/list.rs` has a `call_function()` that handles native functions directly but falls back to `call_callback()` for lambdas and closures. Since `sema-stdlib` cannot depend on `sema-eval` (circular dependency), it uses the thread-local callback.

**Impact:** Every program using `map`, `filter`, `foldl`, `reduce`, `sort`, `any`, `every`, `for-each`, `partition`, `group-by`, `zip` with lambda arguments needs the tree-walker.

### VM Delegate Functions (sema-eval)

| Delegate | What it does | Tree-walker needed? |
|----------|-------------|---------------------|
| `__vm-eval` | Runtime `eval` builtin | Yes — parses and evaluates strings |
| `__vm-load` | `(load "file.sema")` | Yes — reads, parses, evaluates |
| `__vm-import` | `(import "mod.sema")` | Yes — reads, parses, evaluates, caches |
| `__vm-defmacro-form` | Full defmacro delegation | Yes |
| `__vm-define-record-type` | Record type creation | Yes |
| `__vm-force` | Force thunks | Yes (if thunk body is complex) |
| `__vm-macroexpand` | Macro expansion | Yes |
| `__vm-prompt` | Delimited continuations | Yes |
| `__vm-message` | LLM message construction | Yes |
| `__vm-deftool` | LLM tool definitions | Yes |
| `__vm-defagent` | LLM agent definitions | Yes |
| `__vm-defmacro` | Macro registration | No (just stores in env) |
| `__vm-delay` | Create thunks | No (just wraps value) |

### LLM Tool Execution (sema-llm → sema-eval)

`sema-llm/src/builtins.rs` uses `set_eval_callback()` to get access to the evaluator. When an LLM calls a tool defined as a lambda, the tool body is executed via the tree-walker. This is fundamental — tool handlers are user-defined sema code.

## What Would Be Required to Decouple

To make a runtime-only binary viable, three architectural changes would be needed:

### 1. VM-native lambda dispatch in stdlib HOFs

Rewrite `call_function()` in `sema-stdlib/src/list.rs` to handle `Value::Closure` (bytecode closures) by directly invoking the VM, not the tree-walker. The tree-walker fallback would only be needed for `Value::Lambda` (AST closures), which shouldn't exist in a fully-compiled program.

**Effort:** Medium. Requires `sema-stdlib` to gain an optional dependency on `sema-vm`, or a new callback type for VM dispatch.

### 2. Bytecode-level import resolution

Currently `import` always goes through the tree-walker (parse source → evaluate → cache module). For a runtime-only binary, the VM would need to:
- Load pre-compiled `.semac` files directly
- Merge function tables
- Execute module main chunks
- Extract and cache exports

This means all imports must be pre-compiled at build time and stored as `.semac` in the VFS.

**Effort:** High. Requires extending the bytecode format with module metadata (export lists), and implementing a bytecode module loader in the VM.

### 3. Compiled LLM tool handlers

Tool handlers defined as lambdas would need to be compiled to bytecode closures at build time. The LLM runtime would invoke them through the VM, not the tree-walker.

**Effort:** Medium. Requires the compiler to handle tool/agent definitions and the LLM runtime to support VM-based tool dispatch.

### Combined Effort Estimate

All three changes together: **3-6 weeks** of focused work. The stdlib HOF change is the most impactful (affects the most programs), the import change is the most complex, and the LLM change is the most isolated.

### Binary Size Savings

If decoupled, the runtime-only binary could exclude:
- `sema-reader` (~80KB compiled)
- `sema-eval` (~150KB compiled)
- `rustyline` (~500KB) — already excludable
- `clap` (~250KB) — already excludable

Net savings: ~1MB. The heavy dependencies (`reqwest`, `tokio`, `pdf-extract`, TLS) would remain since they're used by `sema-stdlib` and `sema-llm`.

For more significant size reduction, cargo features to gate `pdf-extract` (~500KB+), the LLM module (~200KB + shared `reqwest`), and HTTP support would have greater impact than removing the tree-walker.

## Recommendation

Defer the decoupling work. The binary size savings (~1MB) don't justify the architectural complexity. Instead:

1. Use the full sema binary as runtime for `sema build` (current approach)
2. Add cargo features to gate heavy optional dependencies (`pdf-extract`, optional LLM)
3. Consider the decoupling only if binary size becomes a real user complaint
4. If pursuing, start with stdlib HOF VM dispatch — highest value, most programs benefit
