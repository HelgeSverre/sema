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
  lib.rs            # pub fn eval, eval_string, Interpreter + module system re-exports
  eval.rs           # core eval loop with trampoline + module system thread-local state
  special_forms.rs  # define, if, let, let*, letrec, lambda, begin, defmacro, quasiquote,
                    # prompt, message, deftool, defagent, load, try, catch, throw,
                    # module, import, case, eval, macroexpand

crates/sema-stdlib/src/
  lib.rs            # register_stdlib
  arithmetic.rs     # +, -, *, /, mod
  comparison.rs     # <, >, <=, >=, =, eq?, not, zero?, even?, odd?
  list.rs           # car, cdr, cons, map (multi-list), filter, foldl, foldr, reduce,
                    # range, sort, apply, take, drop, last, zip, flatten, member,
                    # any, every, partition
  string.rs         # string-append, string/split, format, str, type conversions
  predicates.rs     # null?, list?, number?, string?, etc.
  map.rs            # get, assoc, dissoc, keys, vals, merge, contains?
  io.rs             # display, println, file/*, path/*, read, read-many, error
  math.rs           # abs, min, max, floor, ceil, sqrt, pow, pi, trig, log, random, clamp, sign, gcd, lcm
  bitwise.rs        # bit/and, bit/or, bit/xor, bit/not, bit/shift-left, bit/shift-right
  crypto.rs         # uuid/v4, base64/encode, base64/decode, hash/sha256
  datetime.rs       # time/now, time/format, time/parse, time/date-parts
  csv_ops.rs        # csv/parse, csv/parse-maps, csv/encode
  system.rs         # env, shell, exit, time-ms, sleep, sys/args, sys/cwd, sys/platform, sys/env-all
  json.rs           # json/encode, json/decode, json/encode-pretty
  meta.rs           # gensym
  regex_ops.rs      # regex/match?, regex/match, regex/find-all, regex/replace, regex/split
  http.rs           # http/get, http/post, http/put, http/delete, http/request

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

## Phase 6 Decisions

### 16. `try`/`catch`/`throw` over R7RS `guard`

- R7RS defines `(guard (exn (clause ...)) body)` with `cond`-style clauses
- We chose `(try body... (catch e handler...))` for several reasons:
  - Familiar to users of Java, Python, JavaScript, Clojure
  - Simpler to implement — single catch variable bound to error map
  - Error maps with `:type` keyword enable pattern matching via `cond` in the handler
  - `throw` takes any value (not just strings), stored in `:value` key
- All `SemaError` variants are catchable and converted to Sema maps:
  - `:type` — keyword like `:eval`, `:type-error`, `:unbound`, `:user`, `:llm`, `:io`, `:arity`, `:reader`
  - `:message` — human-readable string
  - Variant-specific keys: `:value` (UserException), `:expected`/`:got` (Type), `:name` (Unbound)
- `try` body is fully evaluated (loses TCO — standard behavior for exception-protected code)
- `catch` handler gets TCO on last expression

### 17. Named `let` with TCO

- Detected by checking if `args[0]` is a symbol (vs. a list of bindings)
- Creates a lambda with the loop name and binds it in the new environment
- Recursive calls resolve to the lambda, `apply_lambda` returns `Trampoline::Eval` → full TCO
- No new AST node or special dispatch — reuses existing `eval_let` function

### 18. `letrec` two-pass binding

- Pass 1: bind all names to `Nil` in new env (placeholders)
- Pass 2: evaluate init exprs in the new env, update bindings
- This allows init exprs to close over each other (mutual recursion via lambdas)
- Simpler than R7RS "locations" semantics — `Nil` placeholder is observable if read before assignment

### 19. Module system: file-path-based, not name-based

- Modules are identified by canonical file path, not by module name
- `(module name (export sym1 ...) body...)` — name is documentation only
- `(import "path.sema")` — always uses file paths
- Design rationale:
  - No module registry or search path configuration needed
  - Relative paths resolve from the importing file's directory
  - Absolute paths work too
  - Simple to understand and debug

### 20. Module isolation

- `create_module_env` walks the env parent chain to the root (global/stdlib) env
- Module env is a child of root — gets builtins but not caller's bindings
- This prevents accidental coupling between modules and callers
- Module cache stores exports by canonical path — each module loaded only once

### 21. Thread-local module state

- `MODULE_CACHE`, `CURRENT_FILE`, `MODULE_EXPORTS` are `thread_local!`
- Consistent with existing pattern (LLM provider registry uses thread_local)
- `CURRENT_FILE` is a stack — supports nested `load`/`import`
- `MODULE_EXPORTS` is `Option<Vec<String>>` — `None` means "no module form, export everything"

### 22. `load` updated for relative path resolution

- `load` now resolves relative to `current_file_dir()` (from `CURRENT_FILE` stack)
- Falls back to current working directory if no file context
- `load` also pushes/pops file path for nested resolution
- Breaking change: previously always resolved from cwd

### 23. Extended list operations in stdlib mini-eval

- All new list ops (`take`, `drop`, `zip`, etc.) are NativeFn — no evaluator needed
- Multi-list `map` reuses existing `call_function` from stdlib
- HOF-based ops (`any`, `every`, `partition`, `reduce`, `foldr`) also use `call_function`
- No changes to the mini-eval were needed

## Phase 7 Decisions

### 24. Slash-namespaced naming convention

- All new function groups use `namespace/function` naming: `file/`, `path/`, `regex/`, `http/`, `json/`, `string/`
- Legacy Scheme names (`read-file`, `write-file`, etc.) renamed to `file/read`, `file/write`, etc. for consistency
- Rationale: the slash acts as a logical namespace (like Clojure) — groups related functions into discoverable families
- Traditional Scheme names kept only for: `string-append`, `string-length`, `string-ref`, `substring` (too deeply entrenched in Scheme)
- Predicates like `null?`, `list?`, `map?` remain un-namespaced — they're universal
- Arrow conversions remain: `string->symbol`, `keyword->string`, etc. — standard Scheme convention

### 25. `case` uses PartialEq on unevaluated datums

- Datum lists are NOT evaluated — `(case x ((1 2) "match"))` compares x against literal `1` and `2`
- This matches R5RS semantics and works naturally with keywords: `(case :b ((:a :b) "match"))`
- TCO on last body expression of matching clause

### 26. `eval` as a special form (not builtin)

- `eval` evaluates its argument, then returns `Trampoline::Eval(result, env)` for TCO
- Must be a special form (not NativeFn) because it needs access to the current environment
- The evaluated expression runs in the caller's environment (not a fresh one)

### 27. HTTP client: thread-local runtime + client

- `http.rs` uses `thread_local!` for both `tokio::Runtime` and `reqwest::Client`
- Client reuse enables connection pooling across multiple requests
- Map bodies auto-serialized as JSON via `crate::json::value_to_json`
- Response is always a map: `{:status N :headers {...} :body "string"}`
- Tests marked `#[ignore]` since they require network access

### 28. `macroexpand` expands once

- `macroexpand` does a single expansion step (not recursive)
- Evaluates its argument (so you pass `'(macro-call args...)`)
- If the form starts with a macro name, expands it; otherwise returns as-is
- Uses the existing `apply_macro` function (made `pub` for this purpose)

## Phase 8 Decisions

### 29. Duplicated `call_function` in map.rs

- Map HOFs (`map/map-vals`, `map/filter`, `map/update`) need `call_function` like list.rs
- Duplicated ~60 lines of `call_function` + `sema_eval_value` rather than refactoring to shared module
- Same pattern as list.rs: handles NativeFn and Lambda, mini-eval for lambda bodies
- Rationale: avoids refactoring existing working code; both copies are stable

### 30. Bitwise ops renamed from `bit-*` to `bit/*`

- Follows the slash-namespaced convention (Decision #24)
- Old `bit-and`, `bit-or` etc. renamed to `bit/and`, `bit/or` etc.

### 31. `time/now` returns f64 seconds (not milliseconds)

- Unix timestamp as float seconds (e.g., `1707955200.123`)
- Subsecond precision via fractional part
- Different from `time-ms` which returns integer milliseconds
- Rationale: float seconds is the standard unix timestamp format, works naturally with chrono

### 32. CSV values are always strings

- `csv/parse` and `csv/parse-maps` return all fields as strings
- No automatic type coercion (CSV has no type information)
- Users can convert with `string->number`, `int`, `float` as needed

### 33. `map/filter` takes `(fn (k v) ...)` — two-argument predicate

- Unlike list `filter` which takes `(fn (item) ...)`
- Map filter needs both key and value for meaningful filtering
- Consistent with Clojure's `(filter (fn [[k v]] ...) map)` pattern

## CLI Design Decisions

### 34. CLI flag design follows Chez Scheme / Chicken Scheme conventions

- Surveyed Racket, Chez Scheme, Chicken Scheme, Clojure, Janet, Fennel, and Hy
- Core flags follow widespread Lisp conventions: `-e` (eval), `-l` (load), `-q` (quiet), `-i` (interactive), `-p` (print)
- `-p` always prints (even Nil) — useful for shell pipelines; `-e` skips Nil (standard REPL behavior)
- `-l` is repeatable: `sema -l a.sema -l b.sema` loads both before main execution
- `-i` keeps interpreter state after file/eval, then enters REPL — essential for debugging scripts
- `--no-init` / `--no-llm` skip `(llm/auto-configure)` — faster startup for scripts that don't need LLM
- `--chat-model` and `--chat-provider` set env vars (`SEMA_CHAT_MODEL`, `SEMA_CHAT_PROVIDER`) rather than reconfiguring the provider registry
  - Rationale: provider may not be configured yet; scripts can check `(env "SEMA_CHAT_MODEL")` explicitly
  - This avoids coupling CLI args to provider internals
- `sys/args` returns raw `std::env::args()` — standard behavior, user filters as needed
- `--version` uses `env!("CARGO_PKG_VERSION")` from Cargo.toml — single source of truth for version string

### 35. Multi-provider architecture: reuse OpenAiProvider for compatible APIs

- Groq, xAI, Mistral, and Moonshot all use the OpenAI chat/completions API format
- Rather than creating separate provider structs, `OpenAiProvider` was extended with `name` and `send_stream_options` fields
- Factory method `OpenAiProvider::named(name, api_key, base_url, model, send_stream_options)` creates named instances
- Mistral requires `send_stream_options=false` (rejects the `stream_options` field)
- Google Gemini and Ollama required new provider structs due to completely different APIs:
  - Gemini: auth via query param, `contents` format, `systemInstruction`, `generationConfig`, SSE streaming with `?alt=sse`
  - Ollama: no auth, NDJSON streaming (not SSE), `num_predict` instead of `max_tokens`, custom usage fields
- NDJSON parser (`ndjson.rs`) kept separate from SSE parser (`sse.rs`) — different wire protocols
- Embedding-only providers (Jina, Voyage, Cohere) implement `LlmProvider` but return errors for `complete()`
- `llm/auto-configure` registers ALL available providers, sets the first found as default (priority: Anthropic → OpenAI → Groq → xAI → Mistral → Moonshot → Gemini → Ollama)
- `llm/configure :ollama` does not require `:api-key` — the key extraction was made optional with per-arm validation

## Phase 9 Decisions

### 36. Slash-namespaced LLM accessors (legacy aliases removed)

- Renamed all LLM type accessors to use `/` namespace per Decision #24
- `tool-name` → `tool/name`, `agent-system` → `agent/system`, `prompt-messages` → `prompt/messages`, `message-role` → `message/role`, etc.
- Legacy hyphenated aliases were initially kept but later removed to avoid maintenance burden
- Only the slash-namespaced forms exist now: `tool/name`, `agent/system`, `prompt/messages`, `message/role`, etc.

### 37. Auto-retry on rate limiting

- `do_complete` now retries up to 3 times on `LlmError::RateLimited`
- Waits `min(retry_after_ms, 30000)` between retries using `std::thread::sleep`
- After 3 retries, returns a clear error: "rate limited after 3 retries"
- Only applies to `do_complete` (single completions); streaming is not retried

### 38. Gemini and Ollama tool-call support

- Gemini: sends `tools[].function_declarations` in the request, parses `functionCall` parts from response
- Ollama: sends OpenAI-compatible `tools` array, parses `message.tool_calls` from response
- Both generate synthetic IDs (`gemini-call-N`, `ollama-call-N`) since their APIs don't provide tool call IDs
- All providers now support the full tool loop via `llm/chat` with `:tools`

### 39. Provider introspection builtins

- `llm/set-default` — switch active provider at runtime (validates provider exists)
- `llm/list-providers` — returns sorted list of configured provider names as keywords
- `llm/current-provider` — returns map with `:name` and `:model` of active provider
- `llm/set-budget` / `llm/clear-budget` / `llm/budget-remaining` — expose budget control to Sema
- All provider management uses the existing `PROVIDER_REGISTRY` thread-local

### 40. HTTP timeouts on all providers

- All providers now use 120s HTTP timeout (matching Ollama's existing timeout)
- Prevents indefinite hangs on slow or unresponsive API endpoints
- Applied to: Anthropic, OpenAI, Gemini, Jina, Voyage, Cohere embedding providers

### 41. `pi` and `e` as constants, not functions

- Changed from zero-arg `NativeFn` registrations to direct `env.set()` bindings
- `pi` and `e` now evaluate as bare symbols to their float values (no parens needed)
- Rationale: mathematical constants should be values, not function calls — `(* 2 pi)` not `(* 2 (pi))`

### 42. Scheme-compat predicate aliases

- Added `pair?` (non-empty list), `boolean?` (= `bool?`), `procedure?` (= `fn?`), `equal?` (= `eq?`)
- Primary names remain `bool?`, `fn?`, `eq?` — aliases exist for Scheme compatibility
- `pair?` is new functionality: returns `#t` for non-empty lists (Sema has no dotted pairs/improper lists, so `pair?` ≡ non-empty `list?`)

## Performance Optimization Decisions

### 43. String interning for symbols and keywords (lasso)

- `Value::Symbol` and `Value::Keyword` store `Spur` (u32) instead of `Rc<String>`
- `Env::bindings` changed from `BTreeMap<String, Value>` to `BTreeMap<Spur, Value>` for direct Spur-keyed lookups
- Thread-local `Rodeo` interner, accessed via `intern()/resolve()/with_resolved()`
- `Value::String` remains `Rc<String>` — arbitrary user strings are NOT interned
- Eq comparison of symbols/keywords is now O(1) integer comparison
- Ord comparison still resolves to lexicographic for deterministic BTreeMap ordering
- Mini-eval special form dispatch uses pre-interned Spur constants for O(1) matching (no string comparison)
- Consistent with existing `thread_local!` pattern (LLM provider, module cache)

### 44. HashMap variant for performance-critical accumulation (hashbrown)

- Added `Value::HashMap(Rc<hashbrown::HashMap<Value, Value>>)` as opt-in fast map
- `hashmap/new`, `hashmap/get`, `hashmap/assoc`, `hashmap/to-map`, `hashmap/keys`, `hashmap/contains?` builtins
- Existing `get`, `assoc`, `keys`, `vals`, `contains?`, `count`, `empty?` also work on HashMap
- `Value::Map` (BTreeMap) remains the default for deterministic ordered output
- HashMap used where O(1) lookup matters more than key ordering (e.g., 1BRC accumulator with ~400 entries)
- `Hash` impl added for `Value`: hashes discriminant + inner value; functions/maps hash by discriminant only
- COW optimization (Rc::make_mut) applies to HashMap assoc just like BTreeMap assoc
- HashMap Display sorts entries for deterministic output

### 45. SIMD byte search with memchr

- `memchr` crate used in inlined `string/split` for single-byte separator search
- Replaces `bytes.iter().position()` with `memchr::memchr()` (SIMD-optimized)
- Minimal impact on short strings but beneficial for longer string processing

## WASM Playground Decisions

### 46. WASM `sys/*` returns `"web"` not host OS detection

- `sys/platform` → `"web"`, `sys/arch` → `"wasm32"`, `sys/os` → `"web"`
- Rejected parsing `navigator.userAgent` — UA strings increasingly unreliable (reduction, masquerading, privacy)
- Rejected `navigator.platform` — deprecated API
- Rationale: code runs in WASM sandbox, not natively. Reporting `"macos"` would be misleading since OS-specific APIs (filesystem paths, processes, signals) don't exist
- Matches Go (`GOOS=js`), Rust (`wasm32-unknown-unknown`), Pyodide (`sys.platform="emscripten"`)
- Future: add `web/user-agent` as a separate WASM-only function for host hints

### 47. In-memory VFS for WASM playground (session-only)

- `thread_local! BTreeMap<String, String>` for files, `BTreeSet<String>` for directories
- Enables file I/O examples (turtle-svg, modules-demo, streaming-io) without async bridges
- Session-only — data lost on reload; acceptable for a playground
- Evaluated alternatives: IndexedDB (async sync overhead), OPFS (requires Web Worker for sync access)
- OPFS identified as the ideal future upgrade path — 10-100x faster than IDB, persistent, sync access via `FileSystemSyncAccessHandle` in Workers
- See `docs/plans/2026-02-14-wasm-shims-design.md` for full comparison and roadmap

### 48. HTTP stubs over async bridge for WASM MVP

- `http/*` functions return clear error messages instead of implementing async fetch
- Fundamental constraint: `NativeFn` is synchronous, browser `fetch()` is async (Promise-based)
- Cannot synchronously wait for a Promise on the main thread without deadlocking the event loop
- Future path: `eval_async` entry point + `Value::Promise` variant or suspend/resume effect system
- Worker + Atomics.wait approach rejected for MVP due to cross-origin isolation header requirement
- See `docs/plans/2026-02-14-wasm-shims-design.md` for detailed HTTP roadmap

### 49. Terminal styling as pass-through in WASM

- All `term/*` functions return text unchanged (ANSI codes useless in browser)
- 15 color/modifier functions + `term/style`, `term/strip`, `term/rgb`
- Enables examples using terminal colors to run without error, just without visual styling
- Future: could map to HTML `<span>` elements with CSS classes if playground supports rich output

### 50. Same-VM closure execution via NativeFn payload

- VM closures are wrapped as `Value::NativeFn` with an opaque `payload: Option<Rc<dyn Any>>` field on `NativeFn`
- `VmClosurePayload` stores `Rc<Closure>` + `Vec<Rc<Function>>` (function table from compilation context)
- Inside the VM, `call_value` checks `native.payload`, downcasts to `VmClosurePayload`, and calls `call_vm_closure` which pushes a `CallFrame` on the **same VM** — zero Rust stack growth
- Outside the VM (stdlib HOFs like `map`, `filter`), the `NativeFn::func` fallback creates a fresh VM — this is the interop bridge
- This approach avoids adding a new `Value::VmClosure` variant, keeping the `Value` enum unchanged
- Trade-off: the NativeFn fallback still recurses in Rust for stdlib HOF calls, but this is bounded (stdlib doesn't do deep recursion)

### 51. True TCO for VM closures via frame reuse

- `tail_call_vm_closure` reuses the current `CallFrame`'s stack base instead of pushing a new frame
- Truncates stack to current frame's base, writes new params, replaces `closure` and resets `pc` to 0
- Enables constant-stack-space tail recursion: tested at 100,000+ depth
- `Op::TailCall` bytecode instruction emitted by compiler for calls in tail position
- Mutual recursion at 1,000+ depth also works (each call pushes a frame, but no Rust recursion)

### 52. Named-let desugared to letrec+lambda in lowering

- `(let loop ((n init) ...) body...)` lowered to `(letrec ((loop (lambda (n ...) body...))) (loop init ...))`
- Eliminates `compile_named_let` in the compiler — reuses existing `compile_letrec` + `compile_lambda` paths
- Fixed two classes of bugs: self-reference slot corruption (Bug 1) and missing upvalue/func_id support (Bug 3)
- The `NamedLet` variant still exists in `CoreExpr` but is never produced by the lowering pass
- Tail position flag propagated correctly to the initial `(loop init ...)` call
