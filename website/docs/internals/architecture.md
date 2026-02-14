# Architecture Overview

Sema is a Lisp with first-class LLM primitives, implemented as a tree-walking interpreter in Rust. No bytecode, no JIT — the evaluator walks the AST directly via a trampoline loop for tail-call optimization. The runtime is single-threaded (`Rc`, not `Arc`), with deterministic destruction via reference counting instead of a garbage collector.

The entire implementation is ~15k lines of Rust spread across 6 crates, each with a clear responsibility and strict dependency ordering.

## Crate Map

```
                ┌─────────────────────────────────────┐
                │              sema                    │
                │  (binary: CLI, REPL, embedding API)  │
                └──────────┬──────────────┬────────────┘
                           │              │
              ┌────────────▼───┐    ┌─────▼──────────┐
              │  sema-stdlib   │    │    sema-llm     │
              │  ~350 native   │    │  LLM providers  │
              │  functions     │    │  + embeddings   │
              └────────┬───────┘    └──────┬──────────┘
                       │                   │
                       │    ┌──────────────┘
                       │    │
              ┌────────▼────▼──┐
              │   sema-eval    │
              │  trampoline    │
              │  evaluator     │
              └────────┬───────┘
                       │
              ┌────────▼───────┐
              │  sema-reader   │
              │  lexer/parser  │
              └────────┬───────┘
                       │
              ┌────────▼───────┐
              │   sema-core    │
              │  Value, Env,   │
              │  SemaError     │
              └────────────────┘
```

**Dependency flow:** `sema-core ← sema-reader ← sema-eval ← sema-stdlib / sema-llm ← sema`

The critical constraint: **sema-stdlib and sema-llm depend on sema-core, not on sema-eval.** This avoids circular dependencies but creates a problem — both crates sometimes need to evaluate user code. They solve it differently:

- **sema-stdlib** contains its own mini-evaluator (`sema_eval_value`) for hot paths like `file/fold-lines`
- **sema-llm** uses an eval callback (`set_eval_callback`) injected at startup by the binary crate

This is discussed in detail in [The Circular Dependency Problem](#the-circular-dependency-problem).

### Crate Responsibilities

| Crate | Role | Key types |
|-------|------|-----------|
| **sema-core** | Shared types | `Value` (23 variants), `Env`, `SemaError`, string interner, `NativeFn`, `Lambda`, `Macro`, `Record`, LLM types |
| **sema-reader** | Parsing | Hand-written `Lexer` (24 token types) + recursive descent `Parser` → `Value` AST + `SpanMap` |
| **sema-eval** | Evaluation | Trampoline-based evaluator, 33 special forms, module system, call stack + span table |
| **sema-stdlib** | Standard library | ~350 native functions across 17 modules, plus mini-evaluator for hot paths |
| **sema-llm** | LLM integration | `LlmProvider` trait, 4 native providers (Anthropic, OpenAI, Gemini, Ollama), OpenAI-compatible shim, 3 embedding providers, cost tracking |
| **sema** | Binary | clap CLI, rustyline REPL, `InterpreterBuilder` embedding API |

## The Value Type

All Sema data is represented by a single `Value` enum with 23 variants:

```rust
// crates/sema-core/src/value.rs
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(Rc<String>),
    Symbol(Spur),           // interned u32 handle
    Keyword(Spur),          // interned u32 handle
    Char(char),
    List(Rc<Vec<Value>>),
    Vector(Rc<Vec<Value>>),
    Map(Rc<BTreeMap<Value, Value>>),
    HashMap(Rc<hashbrown::HashMap<Value, Value>>),
    Lambda(Rc<Lambda>),
    Macro(Rc<Macro>),
    NativeFn(Rc<NativeFn>),
    Prompt(Rc<Prompt>),
    Message(Rc<Message>),
    Conversation(Rc<Conversation>),
    ToolDef(Rc<ToolDefinition>),
    Agent(Rc<Agent>),
    Thunk(Rc<Thunk>),
    Record(Rc<Record>),
    Bytevector(Rc<Vec<u8>>),
}
```

Several design choices here are worth examining.

### Why `Rc`, Not `Arc`

Sema is single-threaded. `Arc` adds an atomic increment/decrement on every clone/drop — unnecessary overhead when there's no cross-thread sharing. `Rc` uses ordinary (non-atomic) reference counting, which is cheaper and also means the compiler can catch accidental `Send`/`Sync` usage at compile time.

The trade-off versus a tracing garbage collector: reference counting gives deterministic destruction (values are freed the instant their last reference drops), but cannot collect cycles. In practice this is rarely a problem — Lisp closures tend to create tree-shaped reference graphs, not cycles. A lambda captures its enclosing environment, which may capture its own enclosing environment, forming a chain. Cycles are theoretically possible (e.g., named lambdas bind themselves in their own environment, and `Thunk` uses `RefCell` which could close over itself), but they don't arise in typical Sema programs. If they did, the leaked memory would be bounded by the closure's captured environment — not a growing leak.

This is a different trade-off than most production Lisps make. SBCL uses a generational garbage collector with tagged pointers (the low bits of a machine word indicate the type, avoiding a separate tag field). Clojure rides the JVM's GC. Janet uses NaN boxing — encoding values in the unused bits of IEEE 754 NaN representations to fit a tagged value in 8 bytes. Sema's `Value` enum is 16 bytes on 64-bit targets (`std::mem::size_of::<Value>()` = 16, alignment 8) — the largest variants are pointer-sized (`Rc<T>` is 8 bytes, as are `i64` and `f64`), and the compiler packs the discriminant alongside these to fit within 16 bytes. Smaller variants like `Spur` (4 bytes), `bool` (1 byte), and `char` (4 bytes) leave room to spare. Rust does not guarantee enum layout, so this measurement is empirical — but it's stable across current rustc versions on 64-bit targets. Heap types like `List`, `Map`, and `Lambda` add one level of `Rc` pointer indirection. This is larger than Janet's 8-byte NaN-boxed values and SBCL's tagged words, but simpler to implement and debug — no bit-twiddling, no platform-specific pointer tagging, and Rust's type system enforces exhaustive matching on all 23 variants.

### Why Vector-Backed Lists

`Value::List(Rc<Vec<Value>>)` stores list elements in a contiguous `Vec`, not a linked list of cons cells. This is a deliberate departure from traditional Lisp:

| Operation | Vec-backed | Cons cells |
|-----------|-----------|------------|
| Random access (`nth`) | O(1) | O(n) |
| `length` | O(1) | O(n) |
| Cache locality | Contiguous | Pointer-chasing |
| `cons` (prepend) | O(n) copy | O(1) |
| `append` | O(n) copy | O(n) |
| Pattern matching (`car`/`cdr`) | Slice indexing | Natural |

The performance win comes from cache locality — modern CPUs prefetch sequential memory, so iterating a `Vec` is dramatically faster than chasing pointers through a cons list. Random access and length are constant-time bonuses.

The cost is O(n) `cons` and `append`. Sema mitigates this with copy-on-write optimization (see [Performance Internals](./performance.md#_1-copy-on-write-map-mutation)): when the `Rc` refcount is 1, mutations happen in place instead of copying. In practice, most list construction uses `list`, `map`, `filter`, or `fold` — which build a new `Vec` directly — rather than repeated `cons`.

Clojure takes a third approach: persistent vectors backed by wide (32-way branching) array-mapped tries, giving effectively O(1) indexed access (O(log₃₂ n), which is ≤ 7 for any practical size) with structural sharing. Sema's approach is simpler and faster for small to medium lists, at the cost of no structural sharing.

### Why `BTreeMap` for Maps, `hashbrown` Opt-In

`Value::Map` uses `BTreeMap` (sorted, deterministic iteration order) rather than `HashMap`. This matters for:

- **Deterministic equality:** Two maps with the same entries compare identically via the derived `PartialEq`, and iteration order is independent of insertion order — important for consistent hashing and display
- **Printing:** `{:a 1 :b 2}` always prints in the same order, making test assertions reliable
- **Usable as keys:** Maps can be keys in other `BTreeMap`s because `Value` implements `Ord`. Since `Map` variants compare by sorted content, two maps with the same entries are always equal under `Ord`, regardless of construction order

For performance-critical code, `Value::HashMap` wraps `hashbrown::HashMap` (the SwissTable implementation used inside Rust's standard library). It's opt-in via `(hashmap/new)` — see the [Performance Internals](./performance.md#_5-hashbrown-hashmap) for benchmarks.

### Why `Spur` for Symbols and Keywords

`Symbol(Spur)` and `Keyword(Spur)` store interned `u32` handles rather than strings. A thread-local `lasso::Rodeo` interner maps strings to `Spur` values and back:

```rust
thread_local! {
    static INTERNER: RefCell<Rodeo> = RefCell::new(Rodeo::default());
}

pub fn intern(s: &str) -> Spur {
    INTERNER.with(|r| r.borrow_mut().get_or_intern(s))
}

pub fn with_resolved<F, R>(spur: Spur, f: F) -> R
where
    F: FnOnce(&str) -> R,
{
    INTERNER.with(|r| {
        let interner = r.borrow();
        f(interner.resolve(&spur))
    })
}
```

This makes symbol equality O(1) (integer comparison instead of string comparison) and environment lookup faster (integer keys in the `BTreeMap`). It also means special form dispatch — the hottest path in the evaluator — compares `u32` values against pre-cached constants rather than resolving strings.

String interning is as old as Lisp itself. McCarthy's original LISP 1.5 (1962) interned atoms in the "object list" (oblist). The key difference: Sema uses a separate interner rather than pointer identity, so interning is explicit via `intern()` rather than implicit.

### LLM Types as First-Class Values

`Prompt`, `Message`, `Conversation`, `ToolDef`, and `Agent` sit in the `Value` enum at the same level as `List` and `Map`. They're not encoded as maps-with-conventions — they're distinct types with their own constructors, pattern matching, and display representations:

```scheme
;; These are values, not strings or maps
(define msg (message :user "Hello"))    ; => <message user "Hello">
(define p (prompt msg))                 ; => <prompt 1 messages>
(define conv (conversation p :model "claude-sonnet-4-20250514")) ; => <conversation 1 messages>
```

This means the type system catches errors like passing a string where a message is expected, and tools like `complete` can dispatch on the actual type rather than checking for the presence of magic keys in a map.

## Environment Model

The environment is a linked list of scopes, each holding a `BTreeMap<Spur, Value>`:

```rust
pub struct Env {
    pub bindings: Rc<RefCell<BTreeMap<Spur, Value>>>,
    pub parent: Option<Rc<Env>>,
}
```

Variable lookup walks the parent chain until it finds a binding or reaches the root. This is the standard lexical scoping model — a closure captures a reference to its defining environment, and lookups resolve outward through enclosing scopes.

### Operations

| Operation | Behavior | Used by |
|-----------|----------|---------|
| `get(spur)` | Walk parent chain, return first match | Variable lookup |
| `set(spur, val)` | Insert in current scope | `define`, parameter binding |
| `set_existing(spur, val)` | Walk chain, update where found | `set!` (mutation) |
| `update(spur, val)` | Overwrite in current scope, no key allocation | Hot-path env reuse |
| `take(spur)` | Remove from current scope, return value | COW optimization |
| `take_anywhere(spur)` | Remove from any scope in chain | COW optimization |

`take` and `take_anywhere` exist for the copy-on-write optimization: by *removing* a value from the environment before passing it to a function, the `Rc` refcount drops to 1, enabling in-place mutation. See [Performance Internals](./performance.md#_1-copy-on-write-map-mutation).

`update` exists for the lambda environment reuse optimization: when reusing an environment across iterations of a hot loop, `update` overwrites an existing binding without the overhead of `BTreeMap::insert`'s key allocation path. See [Performance Internals](./performance.md#_2-lambda-environment-reuse).

## Error Handling

`SemaError` is a `thiserror`-derived enum with 8 variants plus a `WithTrace` wrapper:

```rust
#[derive(Debug, Clone, thiserror::Error)]
pub enum SemaError {
    Reader { message: String, span: Span },
    Eval(String),
    Type { expected: String, got: String },
    Arity { name: String, expected: String, got: usize },
    Unbound(String),
    Llm(String),
    Io(String),
    UserException(Value),

    WithTrace { inner: Box<SemaError>, trace: StackTrace },
}
```

### Constructor Helpers

Errors are created via constructor methods, never raw enum variants:

```rust
SemaError::eval("division by zero")
SemaError::type_error("int", val.type_name())
SemaError::arity("map", "2", args.len())
```

This keeps error construction concise across ~350 native functions and 33 special forms.

### Lazy Stack Traces

Stack traces are not captured at error creation time. Instead, the `WithTrace` wrapper is attached during error *propagation* — when the trampoline loop unwinds through a function call, it wraps the error with the current call stack:

```rust
pub fn with_stack_trace(self, trace: StackTrace) -> Self {
    if trace.0.is_empty() {
        return self;
    }
    match self {
        SemaError::WithTrace { .. } => self,  // already wrapped, don't double-wrap
        other => SemaError::WithTrace {
            inner: Box::new(other),
            trace,
        },
    }
}
```

This avoids the cost of capturing a stack trace for errors that are caught by `try`/`catch` — only errors that propagate to the top level pay the trace cost. The idempotence check (`WithTrace { .. } => self`) prevents double-wrapping when an error passes through multiple call frames.

## Thread-Local State

Sema uses thread-local storage extensively. This is a direct consequence of the single-threaded architecture — thread-locals are faster than `Arc<Mutex<_>>` and don't require `Send`/`Sync` bounds.

### Inventory

| Location | Thread-local | Purpose |
|----------|-------------|---------|
| `sema-core/value.rs` | `INTERNER` | String interner (`lasso::Rodeo`) |
| `sema-eval/eval.rs` | `MODULE_CACHE` | Loaded modules (path → exports) |
| `sema-eval/eval.rs` | `CURRENT_FILE` | Stack of file paths being executed |
| `sema-eval/eval.rs` | `MODULE_EXPORTS` | Exports declared by currently-loading module |
| `sema-eval/eval.rs` | `CALL_STACK` | Call frames for error traces |
| `sema-eval/eval.rs` | `SPAN_TABLE` | Rc pointer address → source span |
| `sema-eval/eval.rs` | `EVAL_DEPTH` | Recursion depth counter |
| `sema-eval/eval.rs` | `EVAL_STEP_LIMIT` | Step limit for fuzz targets |
| `sema-eval/eval.rs` | `EVAL_STEPS` | Current step counter |
| `sema-eval/special_forms.rs` | `SF` | Cached `SpecialFormSpurs` |
| `sema-stdlib/list.rs` | `SF` | Cached mini-eval `SpecialFormSpurs` |
| `sema-llm/builtins.rs` | `PROVIDER_REGISTRY` | Registered LLM providers |
| `sema-llm/builtins.rs` | `SESSION_USAGE` | Cumulative token usage |
| `sema-llm/builtins.rs` | `LAST_USAGE` | Most recent completion's usage |
| `sema-llm/builtins.rs` | `EVAL_FN` | Full evaluator callback |
| `sema-llm/builtins.rs` | `SESSION_COST` | Cumulative dollar cost |
| `sema-llm/builtins.rs` | `BUDGET_LIMIT` | Spending cap |
| `sema-llm/builtins.rs` | `BUDGET_SPENT` | Spending against cap |
| `sema-llm/pricing.rs` | `CUSTOM_PRICING` | User-defined model pricing |

### Implications for Embedding

Thread-local state means **one interpreter per thread**. If you embed Sema via `InterpreterBuilder` and spawn multiple threads, each thread gets its own interner, module cache, provider registry, etc. There's no sharing between threads, which is both a limitation (no shared state) and a feature (no synchronization overhead, no data races).

## WASM Support

Sema compiles to WebAssembly with conditional compilation gates. The `#[cfg(not(target_arch = "wasm32"))]` attribute excludes modules that depend on OS-level capabilities:

**From sema-stdlib:**
- `io` — file system access (`file/read`, `file/write`, `file/fold-lines`, etc.)
- `system` — process execution, environment variables, exit
- `http` — HTTP client (`http/get`, `http/post`, etc.)
- `terminal` — terminal control (colors, cursor, raw mode)

**From sema-eval:**
- Module `import`/`load` (depends on file system)

**sema-llm** is excluded entirely — LLM providers require network access.

The pure-computation core (arithmetic, strings, lists, maps, JSON, regex, crypto, datetime, CSV, bytevectors, predicates, math, comparison, bitwise, meta) remains available in WASM, making Sema usable as an embedded scripting language in browser-based applications.

## The LLM Subsystem

### Provider Trait

All LLM providers implement a single trait:

```rust
pub trait LlmProvider: Send + Sync {
    fn name(&self) -> &str;
    fn complete(&self, request: ChatRequest) -> Result<ChatResponse, LlmError>;
    fn default_model(&self) -> &str;

    // Optional — defaults provided
    fn stream_complete(&self, request: ChatRequest,
        on_chunk: &mut dyn FnMut(&str) -> Result<(), LlmError>,
    ) -> Result<ChatResponse, LlmError> { /* non-streaming fallback */ }

    fn batch_complete(&self, requests: Vec<ChatRequest>)
        -> Vec<Result<ChatResponse, LlmError>> { /* sequential fallback */ }

    fn embed(&self, request: EmbedRequest)
        -> Result<EmbedResponse, LlmError> { /* unsupported error */ }
}
```

Note the `Send + Sync` bound — despite the single-threaded runtime, provider implementations use `tokio::runtime::Runtime::block_on` internally to run async HTTP clients. The trait itself is synchronous; async is hidden behind the provider boundary.

### Provider Registry

The `ProviderRegistry` holds registered providers by name with a default provider slot and a separate embedding provider slot:

```rust
pub struct ProviderRegistry {
    providers: HashMap<String, Box<dyn LlmProvider>>,
    default: Option<String>,
    embedding_provider: Option<String>,
}
```

At startup, the binary crate detects available API keys and registers providers:

- `ANTHROPIC_API_KEY` → Anthropic (Claude)
- `OPENAI_API_KEY` → OpenAI (GPT)
- `GEMINI_API_KEY` → Gemini
- `OLLAMA_HOST` → Ollama (local)
- `GROQ_API_KEY`, `XAI_API_KEY`, `MISTRAL_API_KEY`, `MOONSHOT_API_KEY` → OpenAI-compatible shim

Embedding providers (Jina, Voyage, Cohere) are registered separately and selected via `(llm/set-embedding-provider)`.

### Cost Tracking

Every completion records token usage in `SESSION_USAGE` and computes dollar cost via a built-in pricing table (`pricing.rs`). The `with-budget` special form sets a spending cap:

```scheme
(with-budget 0.50
  (complete "Summarize this document..." :model "claude-sonnet-4-20250514"))
;; Raises an error if cumulative cost exceeds $0.50
```

## The Circular Dependency Problem

The most architecturally significant constraint in Sema is the dependency direction between the evaluator and the library crates.

### The Problem

Both `sema-stdlib` and `sema-llm` sometimes need to evaluate user code:

- **sema-stdlib:** `file/fold-lines` invokes a user-provided lambda on each line. `map`, `filter`, `fold`, `for-each`, `sort` all take lambda arguments.
- **sema-llm:** Tool handlers defined via `deftool` are Sema expressions that must be evaluated when an LLM invokes the tool.

But `sema-stdlib` and `sema-llm` depend on `sema-core`, not `sema-eval`. They can't call `eval_value()` because that function lives in `sema-eval`, which is upstream.

```
sema-eval ──depends-on──► sema-core
sema-stdlib ──depends-on──► sema-core
sema-stdlib ──CANNOT depend on──► sema-eval  (would create a cycle)
```

### Solution 1: Mini-Evaluator (sema-stdlib)

`sema-stdlib` contains its own minimal evaluator in `list.rs` — the `sema_eval_value` function. It handles the common forms needed in hot loops:

- `quote`, `if`, `begin`, `let`, `let*`, `cond`, `when`, `unless`, `and`, `or`
- `define`, `set!`, `lambda`/`fn`
- Inlined builtins: `+`, `=`, `assoc`, `get`, `min`, `max`, `first`, `nth`, `nil?`, `float`, `string/split`, `string->number`

This is both an architectural necessity and a performance optimization. The mini-eval skips the full evaluator's overhead (trampoline dispatch, call stack tracking, span table lookups), running **~4x faster** for tight inner loops. See [Performance Internals](./performance.md#_3-mini-eval-inlined-hot-path-builtins) for benchmarks.

The cost: any special form or builtin not handled by the mini-eval is invisible to code running inside `file/fold-lines` and similar hot-path builtins. In practice this is rarely a problem — the forms needed in tight loops (arithmetic, conditionals, let bindings, map operations) are all covered.

### Solution 2: Eval Callback (sema-llm)

`sema-llm` takes a different approach. It stores a function pointer in a thread-local:

```rust
pub type EvalCallback = Box<dyn Fn(&Value, &Env) -> Result<Value, SemaError>>;

thread_local! {
    static EVAL_FN: RefCell<Option<EvalCallback>> = RefCell::new(None);
}

pub fn set_eval_callback(f: impl Fn(&Value, &Env) -> Result<Value, SemaError> + 'static) {
    EVAL_FN.with(|eval| {
        *eval.borrow_mut() = Some(Box::new(f));
    });
}
```

At startup, the binary crate registers the full evaluator:

```rust
sema_llm::set_eval_callback(|expr, env| sema_eval::eval_value(expr, env));
```

When a tool handler needs to evaluate Sema code, it calls through this indirection:

```rust
fn full_eval(expr: &Value, env: &Env) -> Result<Value, SemaError> {
    EVAL_FN.with(|eval_fn| {
        let eval_fn = eval_fn.borrow();
        match &*eval_fn {
            Some(f) => f(expr, env),
            None => simple_eval(expr, env),  // fallback if no callback registered
        }
    })
}
```

This is the classic dependency inversion pattern — `sema-llm` depends on an *interface* (the callback signature) rather than the concrete implementation. The runtime cost is one `RefCell::borrow()` + dynamic dispatch per eval call, which is negligible for LLM tool invocations (the LLM API call dominates by orders of magnitude).

### Why Not a Trait?

An alternative would be to define an `Evaluator` trait in `sema-core` and have `sema-eval` implement it. This would work but adds complexity for little benefit — the callback is simpler, there's only one implementation, and it avoids threading a trait object through every function that might need evaluation. The callback approach also makes it easy to test `sema-llm` in isolation (register a mock evaluator).

### Architectural Lesson

The circular dependency constraint shaped Sema's architecture in a way that turned out to be beneficial. The mini-evaluator exists for correctness (stdlib can't depend on eval) but delivers a 4x performance improvement as a side effect. The eval callback exists for correctness (llm can't depend on eval) but provides clean separation of concerns as a side effect. Sometimes constraints lead to better designs than unconstrained freedom would have.
