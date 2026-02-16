# Changelog

## 1.2.0

### Added

- **`--sandbox` CLI flag** — restrict dangerous operations at runtime. Supports capability groups (`shell`, `fs-read`, `fs-write`, `network`, `env-read`, `env-write`, `process`, `llm`), presets (`--sandbox=strict`, `--sandbox=all`), and comma-separated denylists (`--sandbox=no-shell,no-network`). Sandboxed functions remain registered but return `PermissionDenied` errors when invoked.
- **`Sandbox` / `Caps` embedding API** — `InterpreterBuilder::with_sandbox(Sandbox::deny(Caps::SHELL.union(Caps::NETWORK)))` for fine-grained control when embedding Sema in Rust applications.
- **`PermissionDenied` error variant** — new structured error type for sandbox violations, catchable with `try`/`catch`.
- **REPL tab completion** — tab-complete built-in function names, special forms, user-defined bindings, and REPL commands. Powered by rustyline's `Completer` trait.
- **`,builtins` REPL command** — list all built-in function names, sorted alphabetically.
- **`llm/extract` schema validation** — new `:validate true` option checks that extracted data matches the schema (key presence and type matching).
- **`llm/extract` retry on mismatch** — new `:retries N` option re-sends the request when validation fails, feeding errors back to the LLM.

### Editor Support

- **VS Code** — TextMate grammar extension with syntax highlighting, bracket matching, comment toggling, and indentation support.
- **Vim / Neovim** — Vimscript plugin with syntax highlighting, filetype detection, and Lisp-aware indentation (`lispwords`).
- **Emacs** — `sema-mode` major mode with syntax highlighting, buffer-local indentation, REPL integration, imenu, and electric pairs.
- **Helix** — tree-sitter highlight queries (on Scheme grammar), text objects, and indentation support.
- All four editors highlight the full standard library (350+ functions), special forms, keyword literals, character literals, LLM primitives, and threading macros.

### Documentation

- **Sandbox docs** — new [CLI sandbox reference](https://sema-lang.com/docs/cli.html#sandbox) and updated [embedding guide](https://sema-lang.com/docs/embedding.html) with sandbox examples.
- **Editor support page** — new [sema-lang.com/docs/editors](https://sema-lang.com/docs/editors.html) with installation instructions for all four editors.
- **Shell completions page** — new [sema-lang.com/docs/shell-completions](https://sema-lang.com/docs/shell-completions.html) with setup instructions for bash, zsh, fish, elvish, and PowerShell.
- **Architecture decisions** — new `docs/decisions.md` documenting naming conventions, Rc cycle behavior, sandbox system, mini-evaluator design, package system plans, and LSP roadmap.
- **LSP server design** — new `docs/plans/2026-02-16-lsp-server.md` with 4-phase implementation plan using `tower-lsp`.
- **String docs reorganized** — `string/` namespaced functions now lead; legacy Scheme names grouped under "Scheme Compatibility Aliases".

## 1.1.0

### Added

- **`llm/define-provider`** — define LLM providers entirely in Sema code. The `:complete` function receives a request map (`:model`, `:messages`, `:max-tokens`, `:temperature`, `:system`, `:tools`, `:stop-sequences`) and returns a string or a response map with `:content`, `:usage`, `:tool-calls`, and `:stop-reason`. Supports closures, error propagation via `try`/`catch`, and tool-calling agents.
- **OpenAI-compatible provider fallback** — `llm/configure` with any unknown provider name plus `:api-key` and `:base-url` now registers it as an OpenAI-compatible endpoint. Works with Together AI, Azure OpenAI, Fireworks, vLLM, LiteLLM, and any other OpenAI-compatible service.
- **Tool-call responses from Lisp providers** — Lisp-defined providers can return `:tool-calls` in their response maps, enabling tool-calling agents to work with custom providers.

## 1.0.1

### Improved

- **Structured error messages** — errors now support `.with_hint()` and `.with_note()` for actionable suggestions and context. Reader errors show human-readable token names instead of Rust debug format (e.g. `expected \`)\`, got \`]\`` instead of `RParen`/`RBracket`).
- **Better error spans** — unterminated lists/vectors/maps now point to the opening delimiter instead of `0:0`.
- **Contextual hints on common errors** — unmatched delimiters, prefix operators without expressions (`'`, `` ` ``, `,`, `,@`), "not callable" errors, and bare `#` all include actionable hints.

### Changed

- **README rewritten** — leads with LLM features (coding agent example) instead of generic Lisp reference. Slimmed from ~1000 lines to ~220, deferring full reference to [sema-lang.com/docs](https://sema-lang.com/docs/).

### Added

- **Favicon** — SVG favicon for both the website and playground.
- **SEO meta tags** — Open Graph and Twitter card tags on both the website and playground.
- **Playground link** — added to website navbar.
- **Playground syntax highlighting** — state-machine tokenizer highlighting keywords, `:keyword` literals, strings, comments, numbers, booleans, and parentheses. Covers all 36 special forms, threading macros, and LLM primitives.

### Fixed

- Suppressed unused `ctx` warning in WASM `with-budget` path.

## 1.0.0

### Changed

- **Explicit `EvalContext`** — all thread-local eval state replaced with an `EvalContext` struct threaded through the evaluator. Multiple independent `Interpreter` instances per thread are now possible.
- **`NativeFn` signature** — now takes `(&EvalContext, &[Value])` with `simple`/`with_ctx` constructors.
- **`EvalCallback` in sema-llm** — updated to accept `&EvalContext`.

### Added

- `EvalContext` defined in `sema-core/src/context.rs`, owned by `Interpreter`.
- `InterpreterBuilder` defaults: `stdlib=true`, `llm=true`.

### Fixed

- Stale counts in docs: 39 special forms (was 33), 19 modules (was 17).

## 0.9.1

### Added

- **`string->float`** — new builtin for direct string-to-float conversion, avoiding the `(float (string->number ...))` roundtrip.

### Performance

- **`vector` in mini-eval** — `(vector ...)` calls now bypass the full trampoline evaluator in hot paths.
- **`string->float` in mini-eval** — fast-path evaluation for `string->float` in the mini-evaluator.
- **`let*` flattening** — using `let*` instead of nested `let` reduces environment allocations (3 per iteration → 1 in the 1BRC benchmark).
- **1BRC benchmark: 12.6s → 9.6s native** (24% faster), 17.9s → 15.4s under Docker emulation (14% faster).

## 0.9.0

### Added

- **Dynamic LLM pricing** — pricing data is now fetched from [llm-prices.com](https://www.llm-prices.com) during `(llm/auto-configure)` and cached at `~/.sema/pricing-cache.json`. Falls back to built-in estimates when offline. Custom pricing via `(llm/set-pricing)` always takes priority.
- **`llm/pricing-status`** — new builtin to inspect which pricing source is active and when it was last updated.
- **WASM playground** — browser-based Sema interpreter with categorized examples and file tree sidebar.

### Performance

- **Env bindings switched from `BTreeMap` to `hashbrown::HashMap`** — variable lookups are now O(1) amortized instead of O(log n), significantly improving performance on compute-heavy code.
- **Pre-interned special form symbols** — `else`, `catch`, and `export` symbols in `cond`/`try`/`case`/`module` are now compared as integer Spurs instead of allocating strings via `resolve()`.
- **Deferred `CallFrame` file allocation** — `CallFrame.file` now stores `PathBuf` directly instead of eagerly converting to `String` on every function call; string conversion only happens when formatting stack traces (on errors).
- **Lambda self-reference via `Rc::clone`** — recursive named lambdas no longer reconstruct the entire `Lambda` struct (params, body, env) on every call; they reuse the existing `Rc<Lambda>`.
- **Allocation-free `Display` for `Value`** — `Symbol`, `Keyword`, and `Record` display now use `with_resolved()` (borrows `&str`) instead of `resolve()` (allocates `String`).
- **Step-limit check hoisted out of trampoline loop** — `EVAL_STEP_LIMIT` TLS read moved before the loop so it's read once per eval instead of every iteration.
- **Optimized release profile** — added `lto = "thin"`, `codegen-units = 1`, `panic = "abort"` for faster release binaries; separate `release-with-debug` profile for profiling.

### Fixed

- **Unicode-safe string operations** — `string-length`, `substring`, `length`, and `count` now count characters (Unicode scalar values) instead of bytes. `string/pad-left` and `string/pad-right` use character width for padding. Previously, `(string-length "héllo")` returned 6 (bytes); now it correctly returns 5 (characters). `substring` no longer panics on multi-byte character boundaries.
- **Display panic on multi-byte strings** — Fixed `truncate` in `Value::Message` display to use character-based truncation instead of byte slicing, which could panic on messages containing emoji or non-ASCII text.
- **HashMap support in map operations** — `dissoc`, `merge`, `map/entries`, `map/map-vals`, `map/filter`, `map/select-keys`, `map/map-keys`, and `map/update` now accept both sorted maps and hashmaps, preserving the input type. Previously these functions only worked on sorted maps.
- **Stale Groq pricing** — Groq models are no longer hardcoded as free ($0.00); updated to current estimates.
- **Budget enforcement with unknown pricing** — now warns once instead of silently skipping cost tracking when pricing is unavailable for a model.

## 0.8.0

### Added

- **Embedding API** — `sema` crate now exposes a library with `Interpreter`, `InterpreterBuilder`, and `register_fn()` for embedding Sema as a scripting engine in Rust applications. Builder toggles for stdlib (`with_stdlib`) and LLM (`with_llm`) with sensible defaults.
- **Persistent defines** — `eval_str_in_global` / `eval_in_global` methods on the evaluator so that `define` persists across multiple eval calls (used by the embedding API).
- **Embedding documentation** — new docs page with quick start, builder config, native function registration, a data pipeline example, and threading model notes.

### Fixed

- **Integer overflow panics** — stdlib arithmetic (`+`, `-`, `*`), `abs`, `pow`, `math/quotient`, `math/gcd`, `math/lcm` now use wrapping operations instead of panicking on overflow.
- **"No global state" claim** — removed misleading claim from README and docs; Sema uses a thread-local string interner.

## 0.7.0

### Added

- **String escape sequences** — R7RS-style `\x<hex>;` hex escapes, `\uNNNN` (4-digit), `\UNNNNNNNN` (8-digit) Unicode escapes, and `\0` null escape in string literals. Enables producing any Unicode character including ESC (`\x1B;`) for ANSI terminal codes.
- **Agent message history** — `(agent/run agent msg {:messages history})` returns `{:response "..." :messages [...]}`, enabling multi-turn agent conversations with persistent message history.
- **VitePress documentation site** — Full documentation website with 30+ pages covering stdlib, LLM primitives, language reference, and CLI. All code examples verified against the interpreter.

### Fixed

- **Shell single-string commands** — `(shell "ls -la")` now correctly invokes the system shell for command parsing instead of treating the entire string as an executable name.
- **Tool argument ordering** — `deftool` handlers now receive arguments in lambda declaration order instead of alphabetical BTreeMap key order, fixing mismatches when parameter names aren't alphabetically sorted.

## 0.6.1

### Added

- **System introspection** — `sys/tty` (TTY device name), `sys/pid` (process ID), `sys/arch` (CPU architecture), `sys/os` (OS name), `sys/which` (find executable in PATH), `sys/elapsed` (monotonic nanosecond timer)

### Fixed

- `test_sys_interactive` no longer flaky in environments with a TTY attached

## 0.6.0

### Added

- **List operations** — `list/shuffle` (random reorder), `list/split-at` (split at index), `list/take-while` / `list/drop-while` (predicate-based prefix ops), `list/sum` (numeric sum), `list/min` / `list/max` (extrema), `list/pick` (random element), `list/repeat` / `make-list` (create n copies), `iota` (SRFI-1 integer sequence generator)
- **String operations** — `string/map` (map function over characters), `string/capitalize` (capitalize first letter), `string/reverse`, `string/title-case`
- **Math aliases** — `modulo` (alias for mod), `expt` (alias for pow), `ceiling` (alias for ceil), `truncate`
- **Type conversion** — `number->string`
- **11 new example programs** — Gabriel benchmarks, ASCII art, Perlin noise, Game of Life, lorem ipsum, maze generator, Mandelbrot set, and more
- **System introspection** — `sys/tty` (TTY device name), `sys/pid` (process ID), `sys/arch` (CPU architecture), `sys/os` (OS name), `sys/which` (find executable in PATH), `sys/elapsed` (monotonic nanosecond timer)

### Changed

- Stdlib builtin count increased from ~280 to ~350+ registered functions

## 0.5.0

### Added

- **Character comparison predicates** — R7RS `char=?`, `char<?`, `char>?`, `char<=?`, `char>=?` and case-insensitive `char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, `char-ci>=?`.
- **`define-record-type`** — R7RS record types with constructors, type predicates, and field accessors. `record?` predicate. `type` returns record type name as keyword for records.
- **Bytevectors** — `Value::Bytevector` with `#u8(1 2 3)` reader syntax. `make-bytevector`, `bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!` (COW), `bytevector-copy`, `bytevector-append`, `bytevector->list`, `list->bytevector`, `utf8->string`, `string->utf8`, `bytevector?`.

## 0.4.0

### Added

- **Character type** — First-class `#\a` syntax with named characters (`#\space`, `#\newline`, `#\tab`, `#\return`, `#\nul`). `char?`, `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char-upper-case?`, `char-lower-case?` predicates. `char-upcase`, `char-downcase` case conversion. `char->integer`, `integer->char`, `char->string`, `string->char`, `string->list`, `list->string` conversions.
- **Lazy evaluation** — `delay`/`force` with memoized promises. `promise?` and `promise-forced?` predicates. `force` on non-promise passes through (R7RS compatible).
- **Proper `do` loop** — R7RS `(do ((var init step) ...) (test result ...) body ...)` with parallel variable assignment. Replaces previous `do` alias for `begin`.
- **Car/cdr compositions** — 12 shortcut functions: `caar`, `cadr`, `cdar`, `cddr`, `caaar`, `caadr`, `cadar`, `caddr`, `cdaar`, `cdadr`, `cddar`, `cdddr`.
- **Association lists** — `assoc` now dual-purpose: `(assoc key alist)` for alist lookup, `(assoc map key val ...)` for map assoc. New `assq` and `assv` functions.

### Changed

- `string-ref` now returns `Value::Char` instead of a single-character string
- `string/chars` now returns a list of `Char` values instead of single-character strings
- `do` is no longer an alias for `begin` — it is now a proper Scheme iteration form

## 0.3.0

### Performance

- **String interning with `lasso`** — `Value::Symbol` and `Value::Keyword` now store `Spur` (u32 interned key) instead of `Rc<String>`. Symbol/keyword equality is O(1) integer comparison. `Env` bindings keyed by `Spur` for direct lookup without string allocation. Mini-eval special form dispatch uses pre-interned Spur constants — no string matching in hot path.
- **`hashbrown` HashMap variant** — New `Value::HashMap` type backed by `hashbrown::HashMap` with O(1) amortized lookups. `hashmap/new`, `hashmap/get`, `hashmap/assoc`, `hashmap/to-map`, `hashmap/keys`, `hashmap/contains?` builtins. Existing `get`, `assoc`, `keys`, `vals`, `contains?`, `count`, `empty?` also work on HashMaps. COW optimization (Rc::make_mut) applies to HashMap assoc.
- **SIMD byte search with `memchr`** — `string/split` uses SIMD-accelerated `memchr` for single-byte delimiter search.
- **1BRC benchmark: 1580ms → 1340ms** (15% faster for 1M rows)

### Added

- `Value::HashMap` data type — opt-in unordered hash map for performance-critical accumulation
- `hashmap/new`, `hashmap/get`, `hashmap/assoc`, `hashmap/to-map`, `hashmap/keys`, `hashmap/contains?` builtins
- `Hash` implementation for `Value` — enables use as `HashMap` keys
- `intern()`, `resolve()`, `with_resolved()` — string interner API in `sema-core`

### Changed

- `Value::Symbol` stores `Spur` (u32) instead of `Rc<String>` — **breaking if matching on inner type directly**
- `Value::Keyword` stores `Spur` (u32) instead of `Rc<String>` — **breaking if matching on inner type directly**
- `Env::bindings` uses `BTreeMap<Spur, Value>` instead of `BTreeMap<String, Value>`
- `as_symbol()` and `as_keyword()` now return `Option<String>` instead of `Option<&str>`

### Dependencies

- Added `lasso` 0.7 (string interning) to `sema-core`
- Added `hashbrown` 0.15 (fast HashMap) to `sema-core` and `sema-stdlib`
- Added `memchr` 2 (SIMD byte search) to `sema-stdlib`

## 0.2.1

### Performance

- **Optimized `file/fold-lines`** — reuses lambda env and moves accumulator (no Rc clone per line)
- **Optimized `file/for-each-line`** — reuses lambda env instead of creating a new one per line
- **Inlined hot-path builtins in mini-eval** — `assoc`, `get`, `nil?`, `+`, `=`, `min`, `max`, `first`, `nth`, `float`, `string/split`, `string->number` bypass Env lookup and NativeFn dispatch
- **Zero-clone `assoc`** — uses `Env::take` + `Rc::make_mut` to mutate maps in-place when refcount is 1
- **Added `Env::take()`** — removes and returns a binding from the current scope, enabling move semantics

### Internal

- Made `sema_eval_value` public in sema-stdlib for reuse by `file/fold-lines` and `file/for-each-line`

## 0.2.0

### Added

- **`defun` alias** — Common Lisp-style `(defun name (params) body)` as alias for `define`
- **`sema ast` subcommand** — Parse source and display AST as tree or JSON (`--json`)
- **Slash-namespaced LLM accessors** — `tool/name`, `agent/system`, `prompt/messages`, `message/role`, etc. (legacy names still work)
- **Provider introspection** — `llm/set-default`, `llm/list-providers`, `llm/current-provider`
- **Budget control** — `llm/set-budget`, `llm/clear-budget`, `llm/budget-remaining`
- **Gemini and Ollama tool-call support**
- **Auto rate-limit retry** — 3 attempts with exponential backoff
- **HTTP timeouts** — 120s on all provider requests
- **`conversation/say` options** — accepts optional `{:temperature :max-tokens :system}` map

### Fixed

- Website code examples now use valid, copy-pasteable Sema syntax

## 0.1.0

Initial release — Phases 1-8 complete.

- Scheme-like core with Clojure-style keywords, maps, vectors
- Trampoline-based tail-call optimization
- 226 stdlib builtins across 17 modules
- 29 LLM builtins: completion, chat, streaming, extraction, classification, tool use, agents
- 11 LLM providers: Anthropic, OpenAI, Gemini, Ollama, Groq, xAI, Mistral, Moonshot, Jina, Voyage, Cohere
- Module system with `import`/`export`
- Macros with quasiquote/unquote/splicing
- Error handling with `try`/`catch`/`throw` and stack traces
- REPL with readline, file runner, `-e`/`-p` eval modes
