# Evaluator Internals

Sema is a tree-walking interpreter. There is no bytecode compiler, no JIT, no CPS transform — just an AST represented as `Value` nodes that get recursively evaluated. The key trick is a **trampoline** that turns tail calls into a loop, giving us tail-call optimization without growing the native Rust stack. This page documents how the evaluator works, from the top-level entry point down to function application and stack traces.

If you're familiar with the metacircular evaluator from [SICP](https://mitpress.mit.edu/9780262510875/structure-and-interpretation-of-computer-programs/) (Abelson & Sussman, 1996), Sema's evaluator is structurally similar — `eval` dispatches on expression type, `apply` handles function calls — but with two critical differences: the trampoline for TCO, and a mutable environment model instead of substitution.

## The Trampoline

The central abstraction is a two-variant enum:

```rust
// crates/sema-eval/src/eval.rs
pub enum Trampoline {
    Value(Value),       // done, here's the result
    Eval(Value, Env),   // tail call: evaluate this expr in this env
}
```

Every evaluation step returns a `Trampoline` instead of a `Value`. When a function wants to make a tail call — evaluate an expression as its final action — it returns `Trampoline::Eval(expr, env)` instead of recursing into `eval_value`. The caller's trampoline loop picks this up and continues iterating:

```rust
// crates/sema-eval/src/eval.rs — eval_value_inner
fn eval_value_inner(ctx: &EvalContext, expr: &Value, env: &Env) -> EvalResult {
    let entry_depth = ctx.call_stack_depth();
    let guard = CallStackGuard { ctx, entry_depth };

    // First iteration: use borrowed expr/env to avoid cloning
    match eval_step(ctx, expr, env) {
        Ok(Trampoline::Value(v)) => return Ok(v),
        Ok(Trampoline::Eval(next_expr, next_env)) => {
            // Need to continue — enter the trampoline loop with owned values
            let mut current_expr = next_expr;
            let mut current_env = next_env;
            // ... trim call stack for TCO ...
            loop {
                match eval_step(ctx, &current_expr, &current_env) {
                    Ok(Trampoline::Value(v)) => return Ok(v),
                    Ok(Trampoline::Eval(next_expr, next_env)) => {
                        // TCO: replace expr and env, continue loop
                        current_expr = next_expr;
                        current_env = next_env;
                    }
                    Err(e) => { /* attach stack trace if missing */ }
                }
            }
        }
        Err(e) => { /* attach stack trace if missing */ }
    }
}
```

The key optimization here is that `eval_value_inner` no longer clones `expr` and `env` unconditionally at entry. The first call to `eval_step` uses the borrowed references directly. Owned copies (`current_expr`, `current_env`) are only created when `eval_step` returns `Trampoline::Eval` — meaning a TCO continuation is needed. In the common case where the first `eval_step` returns `Trampoline::Value` (i.e., the expression doesn't involve a tail call), no cloning happens at all.

This is the same technique described in Guy Steele's ["Rabbit" compiler paper](https://dspace.mit.edu/handle/1721.1/6913) (1978), where he showed that tail calls in Scheme can be compiled as jumps. In a native-code compiler (like Chez Scheme), the tail call becomes an actual `jmp` instruction. In a tree-walking interpreter, we can't jump — but we can return a "please evaluate this next" token and loop. The trampoline bounces between returning tokens and evaluating them, hence the name.

### Concrete Example

Consider this tail-recursive loop:

```scheme
(define (loop n)
  (if (= n 0)
    "done"
    (loop (- n 1))))

(loop 1000000)
```

Without TCO, `(loop 1000000)` would push one million Rust stack frames and crash. With the trampoline:

1. `eval_step` sees `(loop 1000000)`, evaluates `loop` to a `Lambda`, evaluates `1000000`, calls `apply_lambda`
2. `apply_lambda` creates a new env with `n = 1000000`, evaluates the `if` body
3. The `if` special form tests `(= n 0)` → false, so it returns `Trampoline::Eval((loop (- n 1)), env)` — **not** a recursive call
4. Back in `eval_value_inner`, the loop picks up `(loop 999999)` as the new `current_expr` and continues
5. This repeats 1,000,000 times in constant Rust stack space

The key insight: `if` is a special form that returns `Trampoline::Eval` for its branches, and `apply_lambda` returns `Trampoline::Eval` for its last body expression. These are the two main sources of tail calls.

### How This Differs From Other Approaches

- **CPS (Continuation-Passing Style):** Some Scheme compilers (such as earlier versions of Orbit and Rabbit) use CPS as an intermediate representation, making every call a tail call. This supports `call/cc` naturally but requires a whole-program transformation. Modern compilers like Chez Scheme use other strategies (nanopass compilation with explicit stack frames). Sema doesn't do CPS; only calls in tail position are optimized.
- **Bytecode VM:** CPython and Lua compile to bytecode and run a `switch`-dispatch loop. The VM's instruction pointer replaces the trampoline. Bytecode is faster (smaller dispatch overhead, better cache behavior) but more complex to implement.
- **Direct recursion:** The metacircular evaluator in SICP chapter 4 just calls `eval` recursively. Simple, but blows the stack on deep tail recursion. Sema started this way and migrated to the trampoline.

## eval_step: A Single Step

`eval_step` takes an expression and an environment, and returns one `Trampoline`:

```rust
// crates/sema-eval/src/eval.rs
fn eval_step(ctx: &EvalContext, expr: &Value, env: &Env) -> Result<Trampoline, SemaError> {
    match expr {
        // Self-evaluating forms
        Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_)
        | Value::String(_) | Value::Char(_) | Value::Keyword(_)
        | Value::Thunk(_) | Value::Bytevector(_) | Value::HashMap(_)
            => Ok(Trampoline::Value(expr.clone())),

        // Symbol lookup
        Value::Symbol(spur) => env.get(*spur)
            .map(Trampoline::Value)
            .ok_or_else(|| SemaError::Unbound(resolve(*spur))),

        // Vector/Map: evaluate elements recursively
        Value::Vector(items) => { /* eval each element */ }
        Value::Map(map)      => { /* eval each key and value */ }

        // List: special forms, then function application
        Value::List(items) => { /* see below */ }
    }
}
```

The dispatch order matters:

1. **Self-evaluating:** Numbers, strings, booleans, nil, keywords, etc. return themselves immediately. These are the leaves of every evaluation.
2. **Symbols:** Look up the binding in the environment chain. If not found, return `SemaError::Unbound`.
3. **Vectors and maps:** Evaluate each element/entry recursively (these are *not* in tail position, so they call `eval_value` directly).
4. **Lists:** The interesting case — check for special forms first, then evaluate the head and dispatch on its type.

### List Evaluation

When `eval_step` encounters a list `(head arg1 arg2 ...)`:

```rust
// Check head for a special form (O(1) integer comparison)
if let Value::Symbol(spur) = head {
    if let Some(result) = special_forms::try_eval_special(*spur, args, env) {
        return result;
    }
}

// Not a special form — evaluate the head to get the callable
let func = eval_value(head, env)?;

// Dispatch on callable type
match &func {
    Value::NativeFn(native) => { /* evaluate args, call Rust fn */ }
    Value::Lambda(lambda)   => { /* evaluate args, apply_lambda */ }
    Value::Macro(mac)       => { /* pass unevaluated args, expand */ }
    Value::Keyword(spur)    => { /* keyword-as-function: (:key map) */ }
    other                   => Err("not callable")
}
```

Note: the head is checked for special forms *before* it's evaluated. This is essential — `if`, `define`, `lambda`, etc. must not evaluate their arguments eagerly.

## Special Form Dispatch

Special form dispatch is the hottest path in the evaluator. Every list expression checks whether its head symbol is one of ~39 special forms. Sema optimizes this with pre-interned `Spur` constants:

```rust
// crates/sema-eval/src/special_forms.rs
struct SpecialFormSpurs {
    quote: Spur,
    if_: Spur,
    cond: Spur,
    define: Spur,
    lambda: Spur,
    // ... 34 more
}
```

These are initialized once and leaked into a `&'static` reference via `Box::leak`:

```rust
thread_local! {
    static SF: Cell<Option<&'static SpecialFormSpurs>> = const { Cell::new(None) };
}

fn special_forms() -> &'static SpecialFormSpurs {
    SF.with(|cell| match cell.get() {
        Some(sf) => sf,
        None => {
            let sf: &'static SpecialFormSpurs = Box::leak(Box::new(SpecialFormSpurs::init()));
            cell.set(Some(sf));
            sf
        }
    })
}
```

The `Box::leak` trick converts a heap allocation into a `&'static` reference that lives for the rest of the program. This is safe because the special form names never change. The result: `try_eval_special` is a chain of integer comparisons (`head_spur == sf.if_`), with no string resolution, no hashing, and no allocation.

```rust
pub fn try_eval_special(
    head_spur: Spur,
    args: &[Value],
    env: &Env,
) -> Option<Result<Trampoline, SemaError>> {
    let sf = special_forms();
    if head_spur == sf.quote {
        Some(eval_quote(args))
    } else if head_spur == sf.if_ {
        Some(eval_if(args, env))
    } else if head_spur == sf.define {
        Some(eval_define(args, env))
    }
    // ... 36 more branches
    else {
        None  // not a special form
    }
}
```

The function returns `Option<Result<...>>` — `None` means "not a special form, proceed with function application." This two-level return type avoids the need for a separate "is this a special form?" check.

**Literature:** This is essentially the same optimization that McCarthy's LISP 1.5 used in 1962 — special forms were identified by their atom pointer, not by string comparison. The `Spur` is Sema's version of the atom pointer.

## Function Application

There are four callable types, each handled differently:

### NativeFn

Native functions are Rust closures wrapped in `Value::NativeFn`. They receive evaluated arguments and return a `Value`:

```rust
Value::NativeFn(native) => {
    let mut eval_args = Vec::with_capacity(args.len());
    for arg in args {
        eval_args.push(eval_value(ctx, arg, env)?);
    }
    ctx.push_call_frame(CallFrame { name: native.name.to_string(), ... });
    match (native.func)(ctx, &eval_args) {
        Ok(v) => {
            ctx.truncate_call_stack(ctx.call_stack_depth().saturating_sub(1));
            Ok(Trampoline::Value(v))
        }
        Err(e) => Err(e),  // leave frame for stack trace
    }
}
```

Native functions always return `Trampoline::Value` — they don't participate in TCO because their API returns `Result<Value, SemaError>` rather than `Trampoline`, so they can't request a tail-eval step. The call frame is pushed before the call and popped on success; on error, it's left in place so the stack trace captures it.

### Lambda

Lambdas are the heart of TCO. After evaluating arguments, `apply_lambda` binds parameters and returns `Trampoline::Eval` for the last body expression:

```rust
// crates/sema-eval/src/eval.rs
fn apply_lambda(ctx: &EvalContext, lambda: &Lambda, args: &[Value]) -> Result<Trampoline, SemaError> {
    let new_env = Env::with_parent(Rc::new(lambda.env.clone()));

    // Bind parameters (with rest-param support)
    for (param, arg) in lambda.params.iter().zip(args.iter()) {
        new_env.set(sema_core::intern(param), arg.clone());
    }

    // Self-reference for recursion
    if let Some(ref name) = lambda.name {
        new_env.set(sema_core::intern(name), Value::Lambda(Rc::new(lambda.clone())));
    }

    // Evaluate all body exprs except the last
    for expr in &lambda.body[..lambda.body.len() - 1] {
        eval_value(ctx, expr, &new_env)?;
    }

    // TCO: return the last body expr for the trampoline to handle
    Ok(Trampoline::Eval(lambda.body.last().unwrap().clone(), new_env))
}
```

This is where TCO actually happens. The last expression is *not* evaluated by `apply_lambda` — it's returned as `Trampoline::Eval` so the trampoline loop can evaluate it without growing the stack. Non-tail body expressions (everything except the last) are evaluated normally via `eval_value`.

Named lambdas bind themselves in the new environment, enabling direct recursion without requiring `letrec` or `Y-combinator` gymnastics.

### Macro

Macros receive unevaluated arguments, evaluate their body to produce an expansion, and then return `Trampoline::Eval` to evaluate the expansion in the caller's environment:

```rust
Value::Macro(mac) => {
    let expanded = apply_macro(mac, args, env)?;
    Ok(Trampoline::Eval(expanded, env.clone()))
}
```

This means macro expansion is always in tail position — the expanded code gets the full benefit of TCO. A macro that expands to a tail call will be optimized just like a direct tail call.

### Keyword-as-Function

Keywords can be used as functions for map lookup: `(:name person)` is equivalent to `(get person :name)`. This is syntactic sugar evaluated directly in `eval_step`, not a general callable mechanism.

## `call_value`: Calling Functions from Stdlib

The `call_value` function is the public API for calling any callable `Value` with pre-evaluated arguments. It's the entry point used by stdlib higher-order functions (`map`, `filter`, `fold`, etc.) when they need to invoke a user-supplied callback:

```rust
// crates/sema-eval/src/eval.rs
pub fn call_value(ctx: &EvalContext, func: &Value, args: &[Value]) -> EvalResult {
    match func {
        Value::NativeFn(native) => (native.func)(ctx, args),
        Value::Lambda(lambda) => {
            // Create env, bind params (with rest-param support), eval body
            let new_env = Env::with_parent(Rc::new(lambda.env.clone()));
            // ... parameter binding ...
            let mut result = Value::Nil;
            for expr in &lambda.body {
                result = eval_value(ctx, expr, &new_env)?;
            }
            Ok(result)
        }
        Value::Keyword(spur) => {
            // Keyword-as-function: (:key map) => map lookup
            // ...
        }
        other => Err(SemaError::eval("not callable")),
    }
}
```

Unlike `apply_lambda` (used by `eval_step` during normal evaluation), `call_value` does **not** use the trampoline — it evaluates the lambda body directly via `eval_value`, including the last expression. This is intentional: `call_value` is called from Rust code (stdlib native functions) that needs a `Value` result, not a `Trampoline`. The trade-off is that callbacks invoked through `call_value` don't get TCO on their own body — but in practice this doesn't matter, because stdlib HOFs like `map` and `filter` iterate externally rather than recursing.

### Callback Architecture

Stdlib functions live in `sema-stdlib`, which depends on `sema-core` but **not** on `sema-eval`. This means stdlib can't call `eval_value` or `call_value` directly. Instead, `sema-core` provides thread-local callback slots:

```rust
// sema-core: thread-local function pointers
sema_core::set_eval_callback(eval_value);   // registered by sema-eval at init
sema_core::set_call_callback(call_value);   // registered by sema-eval at init
```

When a stdlib HOF like `map` needs to call a user function, it calls `sema_core::call_callback(ctx, func, args)`, which dispatches to the registered `call_value`. This indirection preserves the dependency invariant (`core ← eval ← stdlib`) while giving stdlib full access to the evaluator.

## Stack Traces

Sema maintains a call stack in the `EvalContext` for error reporting:

```rust
// crates/sema-core/src/context.rs
pub struct EvalContext {
    // ... other fields
    pub call_stack: RefCell<Vec<CallFrame>>,
    // ...
}
```

Each `CallFrame` captures the function name, current file, and source span:

```rust
// crates/sema-core
pub struct CallFrame {
    pub name: String,
    pub file: Option<String>,
    pub span: Option<Span>,
}
```

### Source Span Tracking

Source spans are tracked using a pointer-identity trick. The reader stores `Span` information in a `HashMap<usize, Span>` keyed by the `Rc::as_ptr` address of each list's backing `Vec`:

```rust
fn span_of_expr(ctx: &EvalContext, expr: &Value) -> Option<Span> {
    match expr {
        Value::List(items) => {
            let ptr = Rc::as_ptr(items) as usize;
            ctx.lookup_span(ptr)
        }
        _ => None,
    }
}
```

This avoids storing spans inside `Value` (which would increase the size of every value). The trade-off: spans are only available for list expressions (and vectors, which the reader also registers). Spans remain valid across `Rc::clone()` calls since cloning an `Rc` preserves the inner pointer. They only become stale if a list is *rebuilt* into a new `Rc<Vec<_>>` (e.g., by `map` or `filter` constructing a fresh vector). In practice this works well because error-producing expressions are typically the original parsed forms.

### RAII Stack Management

The call stack is managed with a guard struct that truncates on drop:

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

This ensures the call stack is cleaned up even if evaluation panics or returns early via `?`. The guard is created at the top of `eval_value_inner` and persists for the duration of the trampoline loop.

### Tail Call Frame Trimming

When the trampoline handles a tail call (`Trampoline::Eval`), it trims accumulated call frames above the entry depth, keeping only the most recent one:

```rust
Ok(Trampoline::Eval(next_expr, next_env)) => {
    {
        let mut stack = ctx.call_stack.borrow_mut();
        if stack.len() > entry_depth + 1 {
            let top = stack.last().cloned();
            stack.truncate(entry_depth);
            if let Some(frame) = top {
                stack.push(frame);
            }
        }
    }
    current_expr = next_expr;
    current_env = next_env;
}
```

Without this trimming, a tail-recursive loop of 1M iterations would accumulate 1M call frames — the stack trace wouldn't overflow the Rust stack, but it would consume unbounded memory. The trim keeps exactly one frame for the current function, which is all you need to know where execution is.

## Guardrails

### MAX_EVAL_DEPTH (1024)

The evaluator tracks nesting depth via the `eval_depth` field in `EvalContext`, incremented on every `eval_value` call and decremented on return. However, `eval_value` has a **fast path** at the top that short-circuits self-evaluating forms and symbol lookups *before* any depth tracking or step counting:

```rust
const MAX_EVAL_DEPTH: usize = 1024;

pub fn eval_value(ctx: &EvalContext, expr: &Value, env: &Env) -> EvalResult {
    // Fast path: self-evaluating forms skip depth/step tracking entirely.
    match expr {
        Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_)
        | Value::String(_) | Value::Char(_) | Value::Keyword(_)
        | Value::Thunk(_) | Value::Bytevector(_) | Value::NativeFn(_)
        | Value::Lambda(_) | Value::HashMap(_) => return Ok(expr.clone()),
        Value::Symbol(spur) => {
            if let Some(val) = env.get(*spur) {
                return Ok(val);
            }
            return Err(SemaError::Unbound(resolve(*spur)));
        }
        _ => {}
    }

    // Only non-trivial expressions (lists, vectors, maps) reach here
    let depth = ctx.eval_depth.get();
    ctx.eval_depth.set(depth + 1);
    if depth > MAX_EVAL_DEPTH {
        ctx.eval_depth.set(ctx.eval_depth.get().saturating_sub(1));
        return Err(SemaError::eval("maximum eval depth exceeded (1024)"));
    }
    // ...
}
```

This fast path is a significant optimization: self-evaluating forms (numbers, strings, booleans, nil, keywords, lambdas, native functions, etc.) and symbol lookups are the most common expressions, and they never need depth tracking because they can't recurse. By returning immediately, they avoid the overhead of incrementing/decrementing the depth counter and checking the step limit. Only compound expressions — lists, vectors, and maps — fall through to the depth-tracked path.

This protects against non-tail recursion that the trampoline can't help with — for example, `(f (f (f ...)))` nests `eval_value` calls for each argument evaluation. The limit of 1024 is generous enough for real programs but prevents native stack overflow.

Note that tail-recursive loops do *not* increase the depth counter, because each iteration returns from `eval_value_inner` via the trampoline loop rather than nesting a new `eval_value` call.

### EVAL_STEP_LIMIT (Fuzzing)

For fuzz testing, an optional step counter limits the total number of trampoline iterations:

```rust
// Fields in EvalContext (crates/sema-core/src/context.rs)
pub eval_step_limit: Cell<usize>,
pub eval_steps: Cell<usize>,
```

When the limit is non-zero, every trampoline step increments `ctx.eval_steps` and checks against the limit. This catches infinite loops that the depth limit can't — a tail-recursive `(define (f) (f))` uses O(1) stack depth but infinite steps. The step counter is reset at each top-level `eval_value` call (when `ctx.eval_depth` is 0).

## Environment Model

Sema uses a linked-list scope chain, where each scope is a `BTreeMap` keyed by `Spur`:

```rust
// crates/sema-core/src/value.rs
pub struct Env {
    pub bindings: Rc<RefCell<BTreeMap<Spur, Value>>>,
    pub parent: Option<Rc<Env>>,
}
```

`Rc<RefCell<...>>` makes each scope mutable and reference-counted. `BTreeMap<Spur, Value>` provides deterministic ordering (important for printing and testing) and is faster than `HashMap` for the very small maps (1–5 entries) typical of `let` and `lambda` scopes — at that size, a few integer comparisons in the B-tree node are cheaper than computing a hash (see the [Performance Internals](performance.md#rejected-optimizations) page for the benchmark).

### Operations

| Method | Behavior |
|--------|----------|
| `get(spur)` | Walk the parent chain, return first match |
| `set(spur, val)` | Insert into the current (innermost) scope |
| `set_existing(spur, val)` | Walk the chain, update where found (for `set!`) |
| `take(spur)` | Remove from current scope only (for COW optimization) |
| `take_anywhere(spur)` | Remove from any scope in the chain |
| `update(spur, val)` | Overwrite in current scope without re-hashing (for hot loops) |

The `take` method is critical for the copy-on-write map optimization described in the Performance page — by removing a value from the environment before passing it to a function, the `Rc` reference count drops to 1, enabling in-place mutation.

**Literature:** This is the standard lexical environment model described in *Lisp in Small Pieces* (Queinnec, 1996, Chapter 6) — a chain of frames linked by static (lexical) pointers. The alternative for lexical scoping — flat closures that copy all free variables into each closure — is faster for lookup but uses more memory when closures share large environments. Sema uses the chained model because closures are pervasive and lookup cost is dominated by the `Spur` integer comparison, not chain traversal.

## Adding a Special Form

If you need to add a new special form to Sema:

1. **Add a `Spur` field** to `SpecialFormSpurs` in `crates/sema-eval/src/special_forms.rs` and initialize it in `init()` with `intern("your-form-name")`
2. **Add a match arm** in `try_eval_special()` — return `Some(eval_your_form(args, env))`
3. **Implement `eval_your_form`** — it receives `args: &[Value]` (unevaluated), `env: &Env`, and `ctx: &EvalContext`, and must return `Result<Trampoline, SemaError>`. If the form has a natural tail position (like `if`'s branches), return `Trampoline::Eval` for TCO; otherwise return `Trampoline::Value`
4. **Add an integration test** in `crates/sema/tests/integration_test.rs`
5. **Document the form** in `website/docs/language/special-forms.md`

## Further Reading

- Christian Queinnec, [*Lisp in Small Pieces*](https://www.cambridge.org/core/books/lisp-in-small-pieces/66FD2BE3EDDDC68588A4605F14A4D2A4) (Cambridge, 1996) — the canonical deep-dive into Lisp interpreter and compiler implementation, covering environment models, continuations, and compilation strategies
- Guy Lewis Steele Jr., ["Rabbit: A Compiler for Scheme"](https://dspace.mit.edu/handle/1721.1/6913) (MIT AI Memo 474, 1978) — introduces the trampoline concept and proves that tail calls can be implemented as jumps
- Abelson & Sussman, [*Structure and Interpretation of Computer Programs*](https://mitpress.mit.edu/9780262510875/structure-and-interpretation-of-computer-programs/) (MIT Press, 1996) — Chapter 4's metacircular evaluator is the template for Sema's `eval_step`/`apply_lambda` split; Chapter 5 shows how to compile to a register machine
- R. Kent Dybvig, ["Three Implementation Models for Scheme"](https://www.cs.indiana.edu/~dyb/pubs/3imp.pdf) (PhD thesis, 1987) — compares heap-based, stack-based, and string-based models; Sema uses heap-based (Rc+RefCell scopes)
