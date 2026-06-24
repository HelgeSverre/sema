# Sema Lisp — Syntactic & Architectural Review

Comprehensive review of syntactic improvements and architectural recommendations.
Assessed against v1.27.1 on 2026-06-24.

---

## Part 1: Syntactic Improvements (Compactness & Ergonomics)

### 1. Zero-arg thunk literal `^(...)` or `#()` — HIGH IMPACT

`#(fn () ...)` boilerplate appears **88 times** in example files — benchmarks, lazy streams,
async spawn, error wrappers, generators. A zero-arg thunk literal would eliminate enormous
verbosity:

```lisp
;; current                          ;; proposed
(async (fn () (http/get url)))  →   (async ^(http/get url))
(stream-cons h (fn () (tl s)))  →   (stream-cons h ^(tl s))
(safe-op "div" (fn () (/ 10 3))) →  (safe-op "div" ^(/ 10 3))
```

The reader already handles `#(...)` for one-arg lambdas via `%`. A `^(...)` prefix (or allowing
`#(...)` with no `%` refs to be zero-arg instead of zero-arg-with-implicit-call) would be a
natural extension. Since `^` is already a valid symbol-start char, this needs a reader dispatch
choice — but `^(` as a thunk literal is unused and unambiguous.

**Implementation**: `crates/sema-reader/src/lexer.rs` (new `#`-dispatch or `^`-prefix token),
`crates/sema-reader/src/reader.rs` (desugar to `(lambda () ...)`).

---

### 2. Map/record field-access shorthand — HIGH IMPACT

`(get m :key)` is the single most repeated call in examples. Keywords are already functions
(`(:key m)` works), but this is backwards from natural reading order and doesn't compose for
nested access without `->`:

```lisp
;; current                              ;; proposed (dot-access or slash-access)
(* pi (get s :radius) (get s :radius))  →  (* pi s.radius s.radius)
(get (get config :db) :host)            →  config.db.host
;; or slash form:
(string/trim (get user :name))          →  (string/trim user/:name)
```

A reader-level `expr.field` or `expr/:field` desugaring to `(get expr :field)` would compact
record-heavy code (multimethods, JSON APIs, data pipelines) significantly. The slash form
`m/:key` is more Lisp-idiomatic and avoids ambiguity with float parsing; the dot form is more
universally readable but needs careful lexer work to distinguish from float `1.5`.

**Implementation**: `crates/sema-reader/src/lexer.rs` (detect `.field` or `/:field` after
expressions), `crates/sema-reader/src/reader.rs` (desugar to `(get ...)`).

---

### 3. List comprehensions with `:when`/`:let` — HIGH IMPACT

Sema has `dotimes` and `for-range` but no Clojure-style `for` comprehension. The
`comprehensions.sema` example **user-defines** `for/list` as a macro. This should be built-in:

```lisp
(for [x (range 10) :when (odd? x) :let [sq (* x x)]]
  (println f"${x}² = ${sq}"))

;; collects results (for/list)
(for/list [x (range 10) :when (odd? x)] (* x x))
;; → (1 9 25 49 81)
```

This combines iteration, filtering, and binding in one form — replacing nested `map`/`filter`/
`let` chains that are the current idiom.

**Implementation**: `crates/sema-vm/src/lower.rs` (lower `for`/`for/list` to nested `let` +
`if` + `cons`), or `crates/sema-eval/src/prelude.rs` (macro expansion to `foldl`/`map`+`filter`).

---

### 4. Multi-arg short lambda promotion — MEDIUM IMPACT

The reader already supports `%1` `%2` `%3` in `#(...)`, but **zero examples use them** — every
two-arg callback falls back to `(fn (a b) ...)`. This is a discoverability/documentation problem,
but also a syntax ergonomics one: `%1` `%2` are visually noisy. Consider Clojure's `%&` for rest
args and making `%` explicitly `%1` (already done) while documenting the multi-arg form more
prominently:

```lisp
;; current                           ;; already works but unknown
(sort xs (fn (a b) (- (get b 1) (get a 1))))
(sort xs #(- (get %2 1) (get %1 1)))   ;; just needs docs + examples
```

**Implementation**: No code change needed — documentation + example updates. Optionally add
`%&` rest-arg support in `crates/sema-reader/src/reader.rs` (`rewrite_percent_args`).

---

### 5. `finally` in `try` — MEDIUM IMPACT

Currently `try` supports only `(catch var handler...)` — no `finally`. Resource cleanup requires
manual `with-stream` or re-throw patterns:

```lisp
(try
  (do-stuff)
  (catch e (handle e))
  (finally (cleanup)))   ;; always runs, even on success or re-throw
```

**Implementation**: `crates/sema-vm/src/lower.rs` (lower `finally` to `try`/`catch` +
re-throw), `crates/sema-vm/src/vm.rs` (exception handling path).

---

### 6. Common combinators in stdlib — MEDIUM IMPACT

These are absent and frequently reimplemented:
- **`comp`** — function composition: `((comp f g) x)` = `(f (g x))`
- **`partial`** — partial application: `((partial + 1) 5)` = `(+ 1 5)`
- **`identity`**, **`constantly`** — trivial but essential
- **`memoize`** — cache function results
- **`complement`** — `((complement even?) 3)` = `true`

**Implementation**: `crates/sema-stdlib/src/meta.rs` or a new `crates/sema-stdlib/src/combinators.rs`,
registered in `crates/sema-stdlib/src/lib.rs::register()`.

---

### 7. Block comments `#| ... |#` — LOW IMPACT, HIGH CONVENIENCE

The reader has line comments (`;`) but no block comments. Multi-line comments are common in Lisp
code for documenting complex forms, disabling sections, and inline explanations. `#| |#`
(including nesting) is standard R7RS and would be a simple lexer addition.

**Implementation**: `crates/sema-reader/src/lexer.rs` (add `#|` dispatch, track nesting depth,
consume until matching `|#`).

---

### 8. Numeric radix prefixes `#x` `#o` `#b` — LOW IMPACT

No hex/octal/binary literals. `#xFF` `#o777` `#b1010` are standard Scheme and useful for
systems/crypto code (Sema has `bitwise` and `crypto` stdlib modules). Simple lexer dispatch
additions.

**Implementation**: `crates/sema-reader/src/lexer.rs` (add `#x`/`#o`/`#b`/`#d` dispatch, parse
radix digits).

---

### 9. Set literal `#{...}` — LOW IMPACT

`#{}` is currently an error. Sema has no set type, but if vectors are distinct from lists, a set
literal would be natural for membership-testing collections. Alternatively, `(set [1 2 3])` as a
stdlib constructor suffices without syntax.

**Implementation**: `crates/sema-reader/src/lexer.rs` + `reader.rs` (parse `#{...}` as a set
value), `crates/sema-core/src/value.rs` (new `TAG_SET` or reuse `TAG_HASHMAP` with a flag).

---

### 10. `defonce` — LOW IMPACT

No idempotent define — re-loading a file redefines everything. `defonce` defines only if not
already bound:

```lisp
(defonce *config* (load-config))   ;; won't re-run on reload
```

**Implementation**: `crates/sema-vm/src/lower.rs` (lower to `if (bound? 'name) ... (define ...)`),
or `crates/sema-eval/src/prelude.rs` (macro).

---

### 11. OR-patterns in `match` — LOW IMPACT

Currently `(match x (1 "one") (2 "two") ...)` needs separate clauses. OR-patterns would compact
this:

```lisp
(match x
  ((or 1 2 3) "small")    ;; proposed
  (_ "other"))
```

**Implementation**: `crates/sema-eval/src/destructure.rs` (add `or` pattern matching in
`soft_match`), `crates/sema-vm/src/lower.rs` (lowering for `match` with `or` patterns).

---

### 12. `:as` destructuring — LOW IMPACT

No way to bind both the whole value and its parts:

```lisp
;; proposed
(let [[a b :as whole] items] ...)   ;; `whole` = the original list
(match req ({:method m :as full} (handle m full)))
```

**Implementation**: `crates/sema-eval/src/destructure.rs` (handle `:as` keyword in vector/map
patterns), `crates/sema-vm/src/lower.rs` (destructuring lowering).

---

## Part 2: Architectural Improvements

### 1. Split the two monolith files (`builtins.rs` 6,882 lines, `vm.rs` 5,905 lines)

**`sema-llm/src/builtins.rs`** is the largest source file in the project by far — all LLM
builtins, budget tracking, caching, fallback chains, streaming, embeddings, vector store, tool
dispatch, and 16 callback call-sites in one file. This is the single biggest maintainability
concern. It should be split by concern:

```
sema-llm/src/
  builtins.rs        → mod.rs (registration wiring only)
  chat.rs            → chat completion builtins
  stream.rs          → streaming builtins
  embeddings.rs      → embedding builtins (already exists but some are in builtins.rs)
  budget.rs          → budget/cost tracking
  tools.rs           → tool definition & dispatch
  agents.rs          → agent builtins
  vector_store.rs    → vector store builtins (already exists but some are in builtins.rs)
```

**`sema-vm/src/vm.rs`** at 5,905 lines holds the execute loop, upvalue handling, debug hooks,
native dispatch, and async yield handling together. Submodules:

```
sema-vm/src/
  vm.rs              → VM struct + execute() loop core
  upvalues.rs        → upvalue open/closed/cleanup
  debug_hooks.rs     → DAP breakpoint/stepping dispatch
  native_dispatch.rs → call_value / native fn invocation
  async_yield.rs     → YieldReason handling / scheduler integration
```

Both files are at the scale where navigation, code review, and merge conflicts become real
friction.

**Risk**: Pure refactor — no behavior change. Needs comprehensive test coverage to verify
(`cargo test` across all crates).

---

### 2. Decompose `EvalContext` and reduce thread-local global state

`EvalContext` is an 18-field god-object spanning concerns: module cache, file stack, call stack,
span table, sandbox, eval limits, user/hidden context stacks, callbacks, and interactive flag.
These are orthogonal responsibilities forced into one struct because it's passed as a single
`&EvalContext` to every native function.

At minimum, group the fields into sub-structs:

```rust
pub struct EvalContext {
    pub modules: ModuleCache,          // module_cache + current_file + module_load_stack
    pub call: CallStack,               // call_stack + span_table
    pub limits: EvalLimits,            // eval_depth/steps/deadline
    pub sandbox: Sandbox,              // already a struct
    pub context: DynamicContext,       // user_context + hidden_context + context_stacks
    pub callbacks: Callbacks,          // eval_fn + call_fn
    pub interactive: bool,
}
```

Separately, the **thread-local global state** (`STDLIB_CTX`, `INTERNER`, `YIELD_SIGNAL`,
`RESUME_VALUE`, gensym counter, provider registries, session usage) creates implicit
initialization ordering — `call_callback` works only because `Interpreter::new()` ran first, and
the error message confirms this is a known footgun. Consider a `Runtime` struct that owns these
and is threaded explicitly, at least for the interner and callbacks. The `thread_local!` pattern
can remain as a fallback, but making the primary path explicit would improve testability and make
the single-threaded constraint visible in the type system.

**Risk**: Large touch surface — every `ctx.field` access changes to `ctx.substruct.field`. Can
be done incrementally with `Deref` impls or accessor methods to minimize breakage. Thread-local
migration is higher-risk and should be a separate, optional phase.

---

### 3. Consolidate the dual Scheme/Clojure naming convention

`string.rs` registers both `string-append` / `string/append`, `string-length` / `string/length`,
`substring` / `string/slice`, `char-alphabetic?` / `char/alphabetic?`. `map.rs` does the same
(`hash-ref` = `get`, `map/new` = `hash-map`). This doubles the API surface, creates decision
fatigue for users, and makes documentation/LSP completions noisy.

**Recommendation**: pick slash-namespaced names (`string/append`, `map/get`) as canonical and
register the legacy Scheme names as deprecated aliases that emit a one-time warning (or simply
document them as aliases without separate registrations). The examples already predominantly use
slash-namespaced forms. The legacy names could be moved to a `compat` module or
`#[deprecated]`-style registration that's opt-in. This also simplifies the LSP completion list
and the `veteran_hint` table (which already maps CL/Scheme names to Sema equivalents).

**Risk**: Breaking change for any code using legacy names. Mitigate with a `--compat` flag or a
`(import "sema/compat")` that registers legacy aliases. The `veteran_hint` table already provides
migration guidance.

---

## Appendix: Documentation Fix

The AGENTS.md dep-flow line states:

```
sema-core ← sema-reader ← sema-vm ← sema-eval ← sema-stdlib/sema-llm ← sema
```

This is inverted — `sema-eval` depends **on** `sema-stdlib`/`sema-llm` (to register them), not
the reverse. The runtime call direction is inverted via callbacks, but the Cargo dependency
direction is `eval → stdlib/llm`. The correct flow is:

```
sema-core ← sema-reader ← sema-vm ← sema-eval → sema-stdlib/sema-llm
                                       ↑              │
                                       └── callbacks ──┘
```

Where `sema-eval` depends on `sema-vm`, `sema-stdlib`, and `sema-llm` at the Cargo level, and
`sema-stdlib`/`sema-llm` call back into the evaluator via thread-local function pointers
registered by `sema-eval` at startup.
