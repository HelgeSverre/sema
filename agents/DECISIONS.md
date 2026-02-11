# Sema Design Decisions

## Implementation Decisions Made During Build

### 1. `prompt` special form: syntax-directed, not evaluation-first
- `(prompt (system "text") (user "text"))` — the role keywords (`system`, `user`, `assistant`, `tool`) are checked in the raw syntax BEFORE evaluation
- This prevents the evaluator from trying to call `system` as a function
- Other expression forms inside `prompt` ARE evaluated normally
- This is the same pattern as Clojure's `(defn name [args] body)` — the `name` and `[args]` are syntax, not evaluated

### 2. `message` form uses keywords, `prompt` uses bare symbols
- `(message :user "Hello")` — keyword role, fully evaluated
- `(prompt (user "Hello"))` — bare symbol role, syntax-directed
- This gives two entry points: `prompt` for ergonomic multi-message construction, `message` for dynamic single-message creation

### 3. Environment: `Rc<Env>` with parent chain, `RefCell<BTreeMap>`
- Single-threaded: `Rc` not `Arc`
- `BTreeMap` over `HashMap` for deterministic ordering (matters for printing, testing)
- `set_existing` walks the chain for `set!` semantics

### 4. Trampoline-based TCO
- `eval_step` returns `Trampoline::Value` or `Trampoline::Eval(expr, env)`
- The trampoline loop in `eval_value` drives tail calls without growing the stack
- Special forms return `Trampoline::Eval` for the last expression in bodies (`begin`, `if`, `let`, etc.)

### 5. Lambda self-reference
- Named lambdas and `define`-d functions automatically bind their own name in the closure env
- This enables recursion without `letrec` or Y-combinator
- Simple approach: inject the binding when applying the lambda

### 6. stdlib `call_function` duplication
- `sema-stdlib/src/list.rs` has its own `call_function` and mini-eval for HOF support (`map`, `filter`, `foldl`)
- This avoids a circular dependency (sema-stdlib can't depend on sema-eval)
- The mini-eval handles: symbol lookup, function application, self-evaluating literals
- Trade-off: complex expressions inside HOF callbacks may not work with the mini-eval, but lambdas work fine

### 7. LLM provider: thread_local registry
- Uses `thread_local!` for the provider registry and usage tracking
- Avoids `Arc<Mutex>` complexity since the Lisp is single-threaded
- Provider auto-configures from env vars (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`)

### 8. tokio runtime per provider
- Each provider creates its own `tokio::runtime::Runtime`
- Uses `block_on` to present a sync interface
- This keeps the Lisp evaluator synchronous while allowing async HTTP

### 9. Keywords as functions
- `:keyword` in function position works like `(get map :keyword)`: `(:name person)`
- Implemented in the evaluator's `eval_step` for `Value::Keyword` in head position

### 10. Value Ord implementation
- `Value` implements `Ord` by comparing within type, then by type_order between types
- Floats compared by bit representation (not mathematically correct but gives consistent ordering)
- Required for `BTreeMap<Value, Value>` (map keys)

## File Layout Summary
```
crates/sema-core/src/
  lib.rs            # re-exports
  value.rs          # Value enum, Env, Lambda, Macro, Message, Prompt, Conversation, etc.
  error.rs          # SemaError enum

crates/sema-reader/src/
  lib.rs            # pub fn read, read_many
  lexer.rs          # tokenize → Vec<SpannedToken>
  reader.rs         # Parser: tokens → Value (+ tests)

crates/sema-eval/src/
  lib.rs            # pub fn eval, eval_string, Interpreter
  eval.rs           # core eval loop with trampoline
  special_forms.rs  # define, if, let, lambda, begin, defmacro, quasiquote, prompt, message,
                    # deftool, defagent, load

crates/sema-stdlib/src/
  lib.rs            # register_stdlib
  arithmetic.rs     # +, -, *, /, mod
  comparison.rs     # <, >, <=, >=, =, eq?, not, zero?, even?, odd?
  list.rs           # car, cdr, cons, map, filter, foldl, range, sort, apply
  string.rs         # string-append, string/split, format, str, etc.
  predicates.rs     # null?, list?, number?, string?, etc.
  map.rs            # get, assoc, dissoc, keys, vals, merge, contains?
  io.rs             # display, println, read-file, write-file, error, load
  math.rs           # abs, min, max, floor, ceil, sqrt, pow, pi
  system.rs         # env, shell, exit, time-ms, sleep
  json.rs           # json/encode, json/decode

crates/sema-llm/src/
  lib.rs            # module declarations
  types.rs          # ChatRequest, ChatResponse, ToolCall, Usage, LlmError
  provider.rs       # LlmProvider trait, ProviderRegistry
  anthropic.rs      # AnthropicProvider (Messages API)
  openai.rs         # OpenAiProvider (chat/completions API)
  builtins.rs       # llm/configure, llm/complete, llm/chat (with tool loop), llm/extract,
                    # llm/classify, llm/pmap, agent/run, conversation/*, prompt-*,
                    # message-*, llm/last-usage, llm/session-usage, llm/reset-usage

crates/sema/src/
  main.rs           # CLI (clap) + REPL (rustyline) + file runner
```

## Phase 4 Decisions

### 11. Eval callback for tool execution
- Tool handlers (lambdas in deftool) need the full evaluator (for `let`, `if`, `cond`, etc.)
- sema-llm can't depend on sema-eval (circular dependency)
- Solution: thread_local `EvalCallback` registered by the Interpreter on init
- `set_eval_callback(eval_value)` gives the LLM builtins access to the full evaluator
- Falls back to `simple_eval` if no callback registered

### 12. Tool execution loop
- `llm/chat` with `:tools` option runs an automatic loop:
  send → check tool_calls → execute handlers → send results → repeat
- Max rounds configurable via `:max-tool-rounds` (default 10)
- Tool results sent back as user messages: `[Tool result for name]: content`
- Parameters converted from JSON to Sema values in schema-defined order

### 13. deftool / defagent as special forms
- `deftool` is a special form (not a macro) — evaluates description, params, and handler
- Creates a `ToolDef` value and binds it in the current env
- `defagent` similarly creates an `Agent` value with system, tools, max-turns, model
- Agent type added to core Value enum

### 14. Macro expansion environment
- Fixed bug: macros now expand in a child of the caller's env (not a bare env)
- This gives macros access to `list`, `cons`, and other builtins during expansion
- Macro params shadow any same-named bindings from the caller env

### 15. load as special form
- `(load "file.sema")` reads and evaluates a file in the CURRENT environment
- This means loaded definitions are available in the caller's scope
- Different from the stdlib `load` function (which just parsed and returned forms)
