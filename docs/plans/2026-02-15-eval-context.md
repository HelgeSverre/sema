# EvalContext: Replace Thread-Locals with Explicit Context

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace all `thread_local!` state in `sema-eval` with an explicit `EvalContext` struct threaded through the evaluator, enabling multiple independent interpreter instances per thread.

**Architecture:** Define `EvalContext` in `sema-core` so all crates can reference it. Use a dual-constructor strategy for `NativeFn` so the ~330 builtins that don't need context remain unchanged, while the ~20 that do get context access. Thread `&EvalContext` through the eval loop, special forms, and into native function calls. LLM state stays in `sema-llm` but captures `Rc<RefCell<...>>` in closures instead of using thread-locals.

**Tech Stack:** Rust 2021, `Rc<RefCell<...>>` for interior mutability (single-threaded), existing crate layering preserved.

---

## Key Design Decisions

### Scope of each current thread-local

| Variable | Current scope | New scope | Rationale |
|----------|--------------|-----------|-----------|
| `MODULE_CACHE` | Per-thread | Per-interpreter | Each interpreter should have independent modules |
| `CURRENT_FILE` | Per-thread | Per-context (stack) | Scoped to eval invocation chain |
| `MODULE_EXPORTS` | Per-thread | Per-context (stack) | Scoped to current module load |
| `CALL_STACK` | Per-thread | Per-context | Scoped to eval invocation chain |
| `SPAN_TABLE` | Per-thread | Per-interpreter | Spans persist across eval calls |
| `EVAL_DEPTH` | Per-thread | Per-context | Tracks nesting depth |
| `EVAL_STEP_LIMIT` | Per-thread | Per-context | Set once, read per eval |
| `EVAL_STEPS` | Per-thread | Per-context | Reset at top-level eval |
| `SF` (special_forms.rs) | Per-thread | **Keep as thread-local** | Pure cache of interned symbols, no state |

### NativeFn strategy: dual constructors, not 350 signature changes

The stored closure type changes to accept `&EvalContext`, but a wrapper constructor hides it from builtins that don't need it:

```rust
// In sema-core:
pub type NativeFnInner = dyn Fn(&EvalContext, &[Value]) -> Result<Value, SemaError>;

impl NativeFn {
    // For the ~330 builtins that don't need context:
    pub fn simple(name: impl Into<String>, f: impl Fn(&[Value]) -> Result<Value, SemaError> + 'static) -> Self { ... }
    // For the ~20 that do:
    pub fn with_ctx(name: impl Into<String>, f: impl Fn(&EvalContext, &[Value]) -> Result<Value, SemaError> + 'static) -> Self { ... }
}
```

### Interior mutability: `Rc<RefCell<...>>` inside EvalContext

The eval loop needs to hold `&EvalContext` while also mutating the call stack (via RAII guards). Using `RefCell` inside the struct avoids borrow-checker fights with `&mut`:

```rust
pub struct EvalContext {
    pub module_cache: RefCell<BTreeMap<PathBuf, BTreeMap<String, Value>>>,
    pub current_file: RefCell<Vec<PathBuf>>,
    pub module_exports: RefCell<Option<Vec<String>>>,
    pub call_stack: RefCell<Vec<CallFrame>>,
    pub span_table: RefCell<HashMap<usize, Span>>,
    pub eval_depth: Cell<usize>,
    pub eval_step_limit: Cell<usize>,
    pub eval_steps: Cell<usize>,
}
```

---

## Phase 1: Define EvalContext in sema-core

### Task 1: Add EvalContext struct to sema-core

**Files:**
- Create: `crates/sema-core/src/context.rs`
- Modify: `crates/sema-core/src/lib.rs`

**Step 1: Create the context module**

Create `crates/sema-core/src/context.rs`:
```rust
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use crate::{CallFrame, Span, Value};

/// Evaluation context — holds all mutable interpreter state.
///
/// Uses interior mutability (`RefCell`/`Cell`) so the eval loop
/// can hold `&EvalContext` while RAII guards mutate the call stack.
pub struct EvalContext {
    pub module_cache: RefCell<BTreeMap<PathBuf, BTreeMap<String, Value>>>,
    pub current_file: RefCell<Vec<PathBuf>>,
    pub module_exports: RefCell<Option<Vec<String>>>,
    pub call_stack: RefCell<Vec<CallFrame>>,
    pub span_table: RefCell<HashMap<usize, Span>>,
    pub eval_depth: Cell<usize>,
    pub eval_step_limit: Cell<usize>,
    pub eval_steps: Cell<usize>,
}

impl EvalContext {
    pub fn new() -> Self {
        Self {
            module_cache: RefCell::new(BTreeMap::new()),
            current_file: RefCell::new(Vec::new()),
            module_exports: RefCell::new(None),
            call_stack: RefCell::new(Vec::new()),
            span_table: RefCell::new(HashMap::new()),
            eval_depth: Cell::new(0),
            eval_step_limit: Cell::new(0),
            eval_steps: Cell::new(0),
        }
    }
}

impl Default for EvalContext {
    fn default() -> Self {
        Self::new()
    }
}
```

**Step 2: Export from lib.rs**

Add `mod context;` and `pub use context::EvalContext;` to `crates/sema-core/src/lib.rs`.

**Step 3: Verify it compiles**

Run: `cargo build -p sema-core`
Expected: compiles with no errors.

**Step 4: Commit**
```
git add -A && git commit -m "feat(core): add EvalContext struct"
```

---

### Task 2: Change NativeFn signature with dual constructors

**Files:**
- Modify: `crates/sema-core/src/value.rs`

**Step 1: Write a test for both constructor styles**

Add to the bottom of `crates/sema-core/src/value.rs` (or a test module):
```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::EvalContext;

    #[test]
    fn test_native_fn_simple() {
        let f = NativeFn::simple("add1", |args| {
            Ok(args[0].clone())
        });
        let ctx = EvalContext::new();
        assert!((f.func)(&ctx, &[Value::Int(42)]).is_ok());
    }

    #[test]
    fn test_native_fn_with_ctx() {
        let f = NativeFn::with_ctx("get-depth", |ctx, _args| {
            Ok(Value::Int(ctx.eval_depth.get() as i64))
        });
        let ctx = EvalContext::new();
        assert_eq!((f.func)(&ctx, &[]).unwrap(), Value::Int(0));
    }
}
```

**Step 2: Run tests to verify they fail**

Run: `cargo test -p sema-core`
Expected: FAIL — `NativeFn::simple` and `NativeFn::with_ctx` don't exist yet.

**Step 3: Change the NativeFn type and add constructors**

In `crates/sema-core/src/value.rs`, change:
```rust
pub type NativeFnInner = dyn Fn(&EvalContext, &[Value]) -> Result<Value, SemaError>;

pub struct NativeFn {
    pub name: String,
    pub func: Box<NativeFnInner>,
}

impl NativeFn {
    /// For builtins that don't need the evaluation context.
    pub fn simple(name: impl Into<String>, f: impl Fn(&[Value]) -> Result<Value, SemaError> + 'static) -> Self {
        Self {
            name: name.into(),
            func: Box::new(move |_ctx, args| f(args)),
        }
    }

    /// For builtins that need access to evaluation context (file paths, call stack, etc.).
    pub fn with_ctx(name: impl Into<String>, f: impl Fn(&EvalContext, &[Value]) -> Result<Value, SemaError> + 'static) -> Self {
        Self {
            name: name.into(),
            func: Box::new(f),
        }
    }
}
```

**Step 4: Run tests to verify they pass**

Run: `cargo test -p sema-core`
Expected: PASS

**Step 5: Commit**
```
git add -A && git commit -m "feat(core): change NativeFn to accept EvalContext, add dual constructors"
```

---

## Phase 2: Update sema-eval to thread EvalContext

### Task 3: Add EvalContext to Interpreter and thread through eval functions

**Files:**
- Modify: `crates/sema-eval/src/eval.rs`
- Modify: `crates/sema-eval/src/lib.rs`

**Step 1: Add EvalContext field to Interpreter**

In `eval.rs`, change `Interpreter`:
```rust
pub struct Interpreter {
    pub global_env: Rc<Env>,
    pub ctx: EvalContext,
}
```

Update `Interpreter::new()` to create the context:
```rust
pub fn new() -> Self {
    let env = Env::new();
    sema_stdlib::register_stdlib(&env);
    #[cfg(not(target_arch = "wasm32"))]
    {
        sema_llm::builtins::register_llm_builtins(&env);
        // eval callback setup deferred to after ctx exists
    }
    let ctx = EvalContext::new();
    Interpreter {
        global_env: Rc::new(env),
        ctx,
    }
}
```

**Step 2: Change eval_value / eval_value_inner / eval_step signatures**

Add `ctx: &EvalContext` parameter to:
- `pub fn eval_value(expr: &Value, env: &Env, ctx: &EvalContext) -> EvalResult`
- `fn eval_value_inner(expr: &Value, env: &Env, ctx: &EvalContext) -> EvalResult`
- `fn eval_step(expr: &Value, env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError>`
- `pub fn eval_string(input: &str, env: &Env, ctx: &EvalContext) -> EvalResult`
- `pub fn eval(expr: &Value, env: &Env, ctx: &EvalContext) -> EvalResult`

Replace all `THREAD_LOCAL.with(...)` accesses with `ctx.field` accesses.

**Step 3: Update helper functions to take &EvalContext instead of using thread-locals**

Convert the free functions (push_file_path, pop_file_path, etc.) to methods on EvalContext or to functions that take `&EvalContext`:

```rust
// These become methods on EvalContext (in sema-core/src/context.rs):
impl EvalContext {
    pub fn push_file_path(&self, path: PathBuf) {
        self.current_file.borrow_mut().push(path);
    }
    pub fn pop_file_path(&self) {
        self.current_file.borrow_mut().pop();
    }
    pub fn current_file_dir(&self) -> Option<PathBuf> {
        self.current_file.borrow().last().and_then(|p| p.parent().map(|d| d.to_path_buf()))
    }
    pub fn current_file_path(&self) -> Option<PathBuf> {
        self.current_file.borrow().last().cloned()
    }
    pub fn get_cached_module(&self, path: &PathBuf) -> Option<BTreeMap<String, Value>> {
        self.module_cache.borrow().get(path).cloned()
    }
    pub fn cache_module(&self, path: PathBuf, exports: BTreeMap<String, Value>) {
        self.module_cache.borrow_mut().insert(path, exports);
    }
    pub fn set_module_exports(&self, names: Vec<String>) {
        *self.module_exports.borrow_mut() = Some(names);
    }
    pub fn clear_module_exports(&self) {
        *self.module_exports.borrow_mut() = None;
    }
    pub fn take_module_exports(&self) -> Option<Vec<String>> {
        self.module_exports.borrow_mut().take()
    }
    pub fn push_call_frame(&self, frame: CallFrame) {
        self.call_stack.borrow_mut().push(frame);
    }
    pub fn call_stack_depth(&self) -> usize {
        self.call_stack.borrow().len()
    }
    pub fn truncate_call_stack(&self, depth: usize) {
        self.call_stack.borrow_mut().truncate(depth);
    }
    pub fn capture_stack_trace(&self) -> StackTrace {
        StackTrace(self.call_stack.borrow().iter().rev().cloned().collect())
    }
    pub fn merge_span_table(&self, spans: SpanMap) {
        self.span_table.borrow_mut().extend(spans);
    }
    pub fn lookup_span(&self, ptr: usize) -> Option<Span> {
        self.span_table.borrow().get(&ptr).cloned()
    }
    pub fn set_eval_step_limit(&self, limit: usize) {
        self.eval_step_limit.set(limit);
    }
}
```

**Step 4: Update CallStackGuard to hold &EvalContext**

```rust
struct CallStackGuard<'a> {
    ctx: &'a EvalContext,
    entry_depth: usize,
}

impl Drop for CallStackGuard<'_> {
    fn drop(&mut self) {
        self.ctx.truncate_call_stack(self.entry_depth);
    }
}
```

**Step 5: Keep old free functions as thin wrappers (temporary)**

To avoid breaking all callers at once, keep the old free functions in `eval.rs` but have them panic with a clear message or delegate to a temporary thread-local that will be removed later. Alternatively, remove them from `lib.rs` exports and fix all call sites immediately.

Recommended: **remove them from exports** and fix callers in Tasks 4-7.

**Step 6: Update Interpreter methods**

```rust
impl Interpreter {
    pub fn eval(&self, expr: &Value) -> EvalResult {
        eval_value(expr, &Env::with_parent(self.global_env.clone()), &self.ctx)
    }
    pub fn eval_str(&self, input: &str) -> EvalResult {
        eval_string(input, &Env::with_parent(self.global_env.clone()), &self.ctx)
    }
    pub fn eval_in_global(&self, expr: &Value) -> EvalResult {
        eval_value(expr, &self.global_env, &self.ctx)
    }
    pub fn eval_str_in_global(&self, input: &str) -> EvalResult {
        eval_string(input, &self.global_env, &self.ctx)
    }
}
```

**Step 7: Verify sema-eval compiles**

Run: `cargo build -p sema-eval`
Expected: compilation errors in special_forms.rs (next task).

**Step 8: Commit (WIP)**
```
git add -A && git commit -m "wip(eval): thread EvalContext through eval functions"
```

---

### Task 4: Update special_forms.rs to use EvalContext

**Files:**
- Modify: `crates/sema-eval/src/special_forms.rs`

**Step 1: Add ctx parameter to try_eval_special and all special form functions**

Change the signature:
```rust
pub fn try_eval_special(
    head_spur: Spur,
    args: &[Value],
    env: &Env,
    ctx: &EvalContext,
) -> Option<Result<Trampoline, SemaError>>
```

And propagate to every `eval_*` function in special_forms.rs. Each one that calls `eval::eval_value` now passes `ctx`.

Functions that call `eval::` helper functions (push_file_path, etc.) now call `ctx.` methods instead.

Key functions to update:
- `eval_import` — uses push_file_path, pop_file_path, current_file_dir, get_cached_module, cache_module, merge_span_table, create_module_env, set_module_exports, clear_module_exports, take_module_exports
- `eval_load` — uses push_file_path, pop_file_path, current_file_dir, merge_span_table
- `eval_module` — uses set_module_exports
- `eval_with_budget` — calls sema_llm budget functions (unchanged for now)
- All others — just forward ctx to `eval::eval_value(expr, env, ctx)`

**Step 2: Update call site in eval.rs**

In `eval_step`, update:
```rust
if let Some(result) = special_forms::try_eval_special(*spur, args, env, ctx) {
    return result;
}
```

**Step 3: Update native function calls in eval_step**

Where native functions are called:
```rust
match (native.func)(&ctx, &eval_args) { ... }
```

**Step 4: Verify sema-eval compiles and tests pass**

Run: `cargo test -p sema-eval`
Expected: should compile; tests in sema-eval should pass.

**Step 5: Commit**
```
git add -A && git commit -m "feat(eval): thread EvalContext through special forms"
```

---

## Phase 3: Update stdlib native function registrations

### Task 5: Update sema-stdlib to use NativeFn::simple()

**Files:**
- Modify: All files in `crates/sema-stdlib/src/` (arithmetic.rs, list.rs, string.rs, etc.)

**Step 1: Find-and-replace NativeFn construction pattern**

Current pattern in stdlib registration:
```rust
Value::NativeFn(Rc::new(NativeFn {
    name: name.to_string(),
    func: Box::new(f),
}))
```

Replace with:
```rust
Value::NativeFn(Rc::new(NativeFn::simple(name, f)))
```

This is a mechanical transformation. A regex replacement should handle most cases. The `register_fn` helper patterns in each module's `register()` function typically wrap the construction.

Check for any stdlib functions that use `current_file_dir()`, `current_file_path()`, or other eval-state accessors — these would need `NativeFn::with_ctx()` instead. Based on investigation, stdlib does NOT call any sema-eval functions (it has no dependency on sema-eval), so **all stdlib functions can use `NativeFn::simple()`**.

**Step 2: Verify it compiles**

Run: `cargo build -p sema-stdlib`
Expected: compiles.

**Step 3: Commit**
```
git add -A && git commit -m "refactor(stdlib): use NativeFn::simple() constructor"
```

---

### Task 6: Update sema-llm builtins

**Files:**
- Modify: `crates/sema-llm/src/builtins.rs`

**Step 1: Update NativeFn construction in sema-llm**

The `register_fn` helper in builtins.rs currently does:
```rust
fn register_fn(env: &Env, name: &str, f: impl Fn(&[Value]) -> Result<Value, SemaError> + 'static) {
    env.set(sema_core::intern(name), Value::NativeFn(Rc::new(NativeFn {
        name: name.to_string(),
        func: Box::new(f),
    })));
}
```

Change to use `NativeFn::simple()`.

**Step 2: Update the EvalCallback and thread-local strategy**

The LLM builtins have their own thread-locals (PROVIDER_REGISTRY, SESSION_USAGE, etc.). These are **separate from the eval thread-locals** and are LLM-specific. Options:

**Option A (recommended for this PR):** Keep LLM thread-locals as-is. They're self-contained within sema-llm and don't affect the eval architecture. Tackle them in a follow-up if/when needed.

**Option B (future):** Move LLM state into a separate `LlmContext` and capture it via `Rc<RefCell<...>>` in the builtins closures.

For now, only update the `EvalCallback` signature:
```rust
pub type EvalCallback = Box<dyn Fn(&EvalContext, &Value, &Env) -> Result<Value, SemaError>>;
```

And update `full_eval` and `set_eval_callback` accordingly. Any LLM builtin that calls `full_eval` will need to receive `ctx` — use `NativeFn::with_ctx()` for those.

**Step 3: Verify it compiles**

Run: `cargo build -p sema-llm`

**Step 4: Commit**
```
git add -A && git commit -m "refactor(llm): update NativeFn construction and EvalCallback signature"
```

---

## Phase 4: Update top-level crates

### Task 7: Update sema binary crate (main.rs + lib.rs)

**Files:**
- Modify: `crates/sema/src/main.rs`
- Modify: `crates/sema/src/lib.rs`

**Step 1: Update lib.rs (InterpreterBuilder / Interpreter)**

The public `sema::Interpreter` wraps `sema_eval::Interpreter`. Update:
- `InterpreterBuilder::build()` — set_eval_callback now needs to capture/pass ctx
- `Interpreter::register_fn()` — use `NativeFn::simple()`

**Step 2: Update main.rs**

Replace calls to free functions:
```rust
// Before:
sema_eval::push_file_path(canonical);
// After:
interpreter.ctx.push_file_path(canonical);
```

The `Interpreter` struct in sema-eval now has `pub ctx: EvalContext`, so main.rs accesses it via `interpreter.ctx`.

**Step 3: Verify CLI works**

Run: `cargo run -- -e "(+ 1 2)"`
Expected: `3`

Run: `cargo run -- examples/hello.sema`
Expected: runs without error.

**Step 4: Commit**
```
git add -A && git commit -m "refactor(sema): update main.rs and lib.rs for EvalContext"
```

---

### Task 8: Update WASM playground crate

**Files:**
- Modify: `playground/crate/src/lib.rs`

**Step 1: Update WasmInterpreter to pass ctx**

The WASM crate calls `sema_eval::eval_string(code, &env)` directly. Update to:
```rust
sema_eval::eval_string(code, &env, &self.inner.ctx)
```

**Step 2: Verify it compiles**

Run: `cargo build -p sema-wasm` (may need wasm target, skip if not set up)

**Step 3: Commit**
```
git add -A && git commit -m "refactor(wasm): update playground for EvalContext"
```

---

## Phase 5: Remove thread-locals and clean up

### Task 9: Remove thread_local! block and old free functions from eval.rs

**Files:**
- Modify: `crates/sema-eval/src/eval.rs`
- Modify: `crates/sema-eval/src/lib.rs`

**Step 1: Delete the thread_local! block**

Remove lines 18-36 from eval.rs (the entire `thread_local!` macro invocation).

**Step 2: Delete the old free functions**

Remove: `push_file_path`, `pop_file_path`, `current_file_dir`, `current_file_path`, `get_cached_module`, `cache_module`, `set_module_exports`, `clear_module_exports`, `take_module_exports`, `push_call_frame`, `call_stack_depth`, `truncate_call_stack`, `capture_stack_trace`, `merge_span_table`, `lookup_span`, `set_eval_step_limit`, `create_module_env`, `span_of_expr`.

Keep `create_module_env` as a method on EvalContext or as a standalone fn (it doesn't use thread-locals, just walks env parents).

**Step 3: Update lib.rs exports**

Remove all the deleted function names from the `pub use eval::{...}` line. Export `EvalContext` (re-exported from sema-core).

**Step 4: Verify everything compiles and all tests pass**

Run: `cargo test`
Expected: all tests pass.

**Step 5: Commit**
```
git add -A && git commit -m "refactor(eval): remove thread_local! state, all state now in EvalContext"
```

---

### Task 10: Run full test suite and fix any remaining issues

**Files:**
- Modify: `crates/sema/tests/integration_test.rs` (if needed)

**Step 1: Run full test suite**

Run: `cargo test`
Expected: all tests pass.

**Step 2: Run clippy**

Run: `make lint`
Expected: no warnings.

**Step 3: Test module loading (import/load)**

Run: `cargo run -- examples/` (pick an example that uses `import` or `load`)
Verify modules resolve correctly.

**Step 4: Test REPL**

Run: `cargo run` and try a few expressions, verify define persistence works.

**Step 5: Commit**
```
git add -A && git commit -m "chore: fix remaining issues after EvalContext migration"
```

---

## Out of scope (follow-up tasks)

- **LLM thread-locals**: PROVIDER_REGISTRY, SESSION_USAGE, etc. in sema-llm — keep as thread-locals for now, migrate later.
- **SPAN_TABLE unbounded growth**: Consider clearing spans at safe points or scoping per-parse.
- **MODULE_EXPORTS reentrancy**: Consider making it a stack (`Vec<Option<Vec<String>>>`) to handle nested imports safely. (May already be a stack based on recent changes.)
- **`Send + Sync` for Interpreter**: Would require switching from `Rc` to `Arc` — much larger change.
- **Mini-evaluator in list.rs**: Currently doesn't use EvalContext. If it ever needs context access, it would need to accept `&EvalContext`. For now it's fine since it only handles simple expressions.
