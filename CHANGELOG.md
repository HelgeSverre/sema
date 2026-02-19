# Changelog

## 1.9.0

### Performance

- **VM intrinsic recognition** — the bytecode compiler now recognizes calls to common builtins (`+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `=`, `not`) and emits specialized inline opcodes instead of `CallGlobal`. This eliminates global hash lookup, `Rc` downcast, argument `Vec` allocation, and function pointer dispatch for the most frequent operations. **TAK benchmark: 4,352ms → 1,250ms (−71%). Upvalue-counter: 1,232ms → 450ms (−63%).** The `*Int` opcodes include NaN-boxed small-int fast paths that operate directly on raw `u64` bits without constructing a `Value`.
- **Peephole optimization: `(if (not X) ...)`** — the compiler pattern-matches `(if (not expr) then else)` and emits `JumpIfTrue` instead of `Not` + `JumpIfFalse`, saving one instruction per branch.
- **1BRC benchmark re-run** — all 15 Lisp dialect benchmarks re-run in Docker with Sema VM (`--vm`) included. VM result: 23.1s (11.2x vs SBCL), a 2× speedup over the tree-walker (46.3s). Natively: ~15.9s, competitive with Janet and Guile.

### Fixed

- **VM division semantics** — `vm_div` now returns float results for non-whole integer divisions (e.g., `(/ 7 2)` → `3.5`), matching the stdlib `div` function.
- **VM equality semantics** — `vm_eq` now handles mixed int/float comparison correctly (e.g., `(= 1 1.0)` → `#t`), matching the stdlib `eq` function.
- **OpenAI embedding fallback** — configuring an OpenAI embedding model no longer overwrites the chat provider setting.
- **Path denial error messages** — improved error messages when `--allowed-paths` blocks a file operation.

### Added

- **Span end positions** — `Span` now tracks `end_line` and `end_col` in addition to start positions, enabling precise range highlighting in future tooling (LSP, error reporting). Added `Span::to()` and `Span::with_end()` convenience constructors.
- **Stack traces on unbound variable errors** — the tree-walker now attaches a call stack trace to unbound variable errors for easier debugging.
- **Scoped LLM CLI flags** — CLI flags and environment variables for LLM providers are now scoped to chat vs embeddings, allowing independent configuration of each.

### Internal

- **Named constants** — replaced magic numbers across VM and value system with named constants.
- **Exported `SPECIAL_FORM_NAMES`** — canonical list of special form names exported from `sema-eval` for use by tooling.
- **`make deploy`** — combined website and playground deployment target.

## 1.8.0

### Added

- **Path sandboxing (`--allowed-paths`)** — restrict file operations to specific directories. Paths are canonicalized and lexically normalized to prevent traversal attacks (`../../etc/passwd`). Works with all file functions (`file/read`, `file/write`, `file/list`, `kv/open`, `pdf/*`, etc.) and composes with `--sandbox`. Embedding API: `Interpreter::builder().with_allowed_paths(vec![...])`.
- **WASM VFS quotas** — the browser playground virtual filesystem now enforces limits: 1 MB per file, 16 MB total, 256 files max. Prevents runaway memory usage from scripts.
- **VM compiler depth limit** — the bytecode compiler, resolver, and lowering passes now enforce a recursion depth limit (256), preventing stack overflows from deeply nested expressions. Uses RAII guard for panic-safe cleanup.
- **Benchmarks README** — `benchmarks/README.md` documents how to generate test data and run tree-walker vs VM comparisons.

### Changed

- **Benchmark data files** moved from repo root to `benchmarks/data/` (gitignored). All doc references updated.

### Security

- **Fixed path traversal bypass** — `check_path` fallback for nonexistent paths now lexically normalizes `..` segments before the `starts_with` check, closing a bypass where `allowed/nonexistent/../../escape.txt` could escape the allowed directory.
- **Fixed WASM VFS quota bypass** — `file/write-lines` was missing quota enforcement and `VFS_TOTAL_BYTES` tracking. All VFS mutators now use saturating arithmetic to prevent underflow.

## 1.7.0

### Added

- **Bytecode serialization (`.semac`)** — compile Sema source to a binary bytecode format for faster loading and source-free distribution. The format uses a 24-byte header with magic number `\x00SEM`, a deduplicated string table, function table, and main chunk section. See [Bytecode File Format](/docs/internals/bytecode-format.html) for the full spec.
- **`sema compile` subcommand** — compile `.sema` source files to `.semac` bytecode. Supports `-o` for custom output path and `--check` for validation without execution.
- **`sema disasm` subcommand** — disassemble `.semac` files to human-readable text or structured JSON (`--json`).
- **Auto-detect `.semac` files** — running `sema script.semac` automatically detects the magic number and executes via the VM, no `--vm` flag needed.
- **Embedding API: `load_file` and `preload_module`** — `Interpreter::load_file("prelude.sema")` evaluates a file with definitions persisting in the global environment. `Interpreter::preload_module("name", source)` caches a module so `(import "name")` resolves without disk access.
- **Tree-sitter grammar** — full `tree-sitter-sema` grammar with external scanner for nestable block comments, 46 tests across 5 categories. Published to `helgesverre/tree-sitter-sema` mirror repo.
- **Zed editor extension** — syntax highlighting, Go to Symbol (`define`, `defun`, `defmacro`, `defagent`, `deftool`), auto-indentation, bracket matching, and "Run Sema File" task.
- **Homebrew tap** — `brew install helgesverre/tap/sema-lang`.
- **cargo-dist** — automated multi-platform binary releases for Linux (x86_64/aarch64), macOS (Intel/Silicon), and Windows.
- **`llms.txt` and `llms-full.txt`** — LLM-friendly documentation index and full concatenated docs for context ingestion.
- **Smoke test suite** — `make smoke-bytecode` runs all 66 examples through compile → disasm → run (65/66 pass).
- **Link checker CI** — lychee-based link checking workflow and `make lint-links` target.

### Performance

- **VM dispatch loop restructuring** — two-level loop with cached frame locals, raw pointer bytecode reads, direct u8 matching, and deferred PC writeback.
- **Lazy upvalue allocation** — `open_upvalues` deferred until `MakeClosure` actually captures a local, eliminating heap allocation for non-capturing recursive functions.
- **Rc avoidance in call dispatch** — NaN-boxed tag peek (`raw_tag()`, `as_native_fn_ref()`) identifies callables without Rc refcount bumps. Eliminates 60M+ refcount operations on `tak` benchmark (31.8M calls).
- **Specialized opcodes `LoadLocal0..3`, `StoreLocal0..3`** — single-byte zero-operand instructions for the first four local variable load/store slots, eliminating 2-byte operand decode.
- **Fused `CallGlobal` opcode** — combines `LOAD_GLOBAL` + `CALL` into a single instruction for non-tail calls to global functions. Avoids pushing/popping the function value on the stack and uses a direct call path (`call_vm_closure_direct`) that skips the function-slot convention entirely.
- **Global lookup cache** — 16-entry direct-mapped cache with versioned `Env`, avoiding `RefCell` borrow and hashmap lookup on hot global reads.
- **Raw stack operations for integer arithmetic** — `AddInt`, `SubInt`, `MulInt`, `LtInt`, `EqInt` operate directly on raw u64 bits, bypassing Clone/Drop.
- **Benchmark results**: `tak` 9% faster (4.77s → 4.35s), `deriv` 6% faster (1.53s → 1.45s), `upvalue-counter` 15% faster (1.44s → 1.23s). VM remains 2–4× faster than tree-walker across all benchmarks.

### Security

- **Bytecode hardening** — safe Spur conversion (no unsafe transmute), section boundary enforcement, recursion depth limits (128), DoS allocation limits, operand bounds validation, reserved header field checks, string table index 0 validation, section payload consumption verification.

### Documentation

- **CLI reference** — documented `compile`, `disasm`, `--vm`, `--check`, `--json` flags.
- **Bytecode format spec** — updated status from "Design Phase" to "Implemented (Alpha)".
- **OG social preview images** — branded images for website, playground, and GitHub.
- **Editor support** — updated Helix config for native tree-sitter-sema grammar; documented Zed extension.

### Internal

- **crates.io publish workflow** — automated publishing of all 7 workspace crates in dependency order on version tag push.
- **Subtree split CI** — auto-syncs `editors/tree-sitter-sema/` to mirror repo on push to main.
- **Test coverage** — 36+ new unit/integration tests for error types, LLM types, JSON, KV, bytevectors, and bytecode serialization (37 serialization tests).

## 1.6.0

### Added

- **Pretty-printing** — `pprint` builtin and `pretty_print(value, max_width)` in `sema-core`. Smart line-breaking for nested maps/lists with 2-character indentation. Used by REPL and playground for result display.
- **Context module** (15 functions) — ambient key-value context that flows through execution: `context/set`, `context/get`, `context/has?`, `context/remove`, `context/pull`, `context/all`, `context/merge`, `context/clear`, `context/with` (scoped overrides), `context/push`, `context/stack`, `context/pop` (named stacks), `context/set-hidden`, `context/get-hidden`, `context/has-hidden?`. Context auto-appends to `log/info`, `log/warn`, `log/error` output.
- **PDF processing module** (4 functions) — `pdf/extract-text`, `pdf/extract-text-pages`, `pdf/page-count`, `pdf/metadata`. Pure-Rust via `pdf-extract` and `lopdf` crates, sandboxed under `FS_READ`.
- **21 new string functions** — `string/after`, `string/after-last`, `string/before`, `string/before-last`, `string/between`, `string/chop-start`, `string/chop-end`, `string/ensure-start`, `string/ensure-end`, `string/replace-first`, `string/replace-last`, `string/remove`, `string/take`, `string/snake-case`, `string/kebab-case`, `string/camel-case`, `string/pascal-case`, `string/headline`, `string/words`, `string/wrap`, `string/unwrap`.
- **Text utilities** — `text/excerpt` (snippet extraction around a search term with omission markers), `text/normalize-newlines` (convert `\r\n`/`\r` to `\n`).
- **Async HTTP in WASM playground** — `http/get`, `http/post`, `http/put`, `http/delete`, `http/request` now work in the browser playground via a replay-with-cache strategy (uses browser `fetch()` API).
- **`check_arity!` macro** — reduces boilerplate in stdlib function implementations.

### Changed

- **WASM stack tuning** — `MAX_EVAL_DEPTH` lowered to 256 for `wasm32` targets; `.cargo/config.toml` sets 64MB WASM linear memory stack.
- **Stdlib function count** — increased from ~370 to ~460+ registered functions.

### Internal

- **Website docs** — added documentation pages for Context, PDF processing, Playground & WASM HTTP support, and 21 new string functions. Updated stdlib index and quick reference tables.

## 1.5.0

### Breaking

- **`llm/with-budget` demoted from special form to function** — now takes a thunk like the other `llm/with-*` functions: `(llm/with-budget {:max-cost-usd 0.50} (lambda () ...))` instead of `(with-budget {:max-cost-usd 0.50} body...)`.

### Added

- **LLM pipeline & resilience primitives** — `llm/with-cache`, `llm/with-budget`, `llm/with-rate-limit`, `llm/with-fallback`, `llm/cache-stats`, `llm/cache-clear`, `llm/cache-key` for response caching, cost limits, rate limiting, and provider fallback chains.
- **LLM utility functions** — `llm/compare`, `llm/summarize`, `llm/token-count`, `llm/token-estimate`, `llm/default-provider`, `llm/providers`.
- **Vision support** — `llm/extract-from-image` for vision-based structured data extraction.
- **Vector store** — in-memory vector store with `vector-store/create`, `vector-store/add`, `vector-store/search`, `vector-store/delete`, `vector-store/count`, `vector-store/save`, `vector-store/open`, and vector math functions (`vector/cosine-similarity`, `vector/dot-product`, `vector/distance`, `vector/normalize`, `embedding/list->embedding`).
- **Text processing module** (15 functions) — `text/chunk`, `text/chunk-by-separator`, `text/split-sentences`, `text/clean-whitespace`, `text/strip-html`, `text/truncate`, `text/word-count`, `text/trim-indent`, `prompt/template`, `prompt/render`, `document/create`, `document/text`, `document/metadata`, `document/chunk`.
- **Key-value store** (6 functions) — persistent JSON-backed store with `kv/open`, `kv/set`, `kv/get`, `kv/delete`, `kv/keys`, `kv/close`. Sandboxed under `FS_WRITE`.
- **`retry` function** — exponential backoff retry with configurable `:max-attempts`, `:base-delay-ms`, and `:backoff` multiplier.
- **New examples** — `examples/llm/test-pipeline.sema`, `test-text-tools.sema`, `test-vector-store.sema`, `test-kv-store.sema` showcasing the new primitives.

### Changed

- **Playground build system** — examples are now separate `.sema` files in `playground/examples/` (7 categories, 46 files), injected at build time via an esbuild-based build script (`playground/build.mjs`). Replaced 2,351 lines of inline JS with a clean file-per-example structure.
- **Special forms reorganized** — sorted into logical groups (Core language, Modules, LLM primitives) with alphabetical ordering within each group.

### Internal

- **Website docs** — added documentation pages for caching, resilience, vector stores, KV stores, text processing, embeddings, and cost management.
- **Makefile** — `playground-build` now runs `node build.mjs` after wasm-pack.

## 1.4.0

### Changed

- **NaN-boxed Value type** — `Value` is now an 8-byte NaN-boxed `struct Value(u64)` instead of a 24-byte enum. All values are encoded in IEEE 754 quiet NaN payload space:
  - **Immediates** (zero heap allocation): Nil, Bool, Char, Symbol(`Spur`), Keyword(`Spur`), small integers (±17.5 trillion range)
  - **Heap types**: `Rc<T>` pointer stored in 45-bit payload (pointer >> 3, using 8-byte alignment)
  - **Floats**: stored as raw `f64` bits with canonical quiet NaN for NaN values
  - Pattern matching uses `val.view()` → `ValueView` enum; direct accessors (`as_int()`, `as_str()`, etc.) still work
  - VM mode sees **8-12% speedup** from better cache locality; tree-walker sees 9-16% regression from `view()` overhead (acceptable — VM is the future execution path)
  - Memory (RSS) reduced ~5-10% across all benchmarks

### Fixed

- **Dangling pointer UB in `as_bytevector()`/`as_record()`** — `borrow_rc()` created a stack-local `ManuallyDrop<Rc<T>>` and returned a reference into it. Fixed to use `borrow_ref()` directly.
- **Clippy lints for Rust 1.93** — fixed `manual_div_ceil` and `doc_overindented_list_items` warnings in `sema-wasm`.

### Internal

- **VM dispatch loop optimization** — tightened the main `run()` loop and fixed a bug in `call_vm_closure` argument copying.
- **Cross-language benchmark programs** — added Janet and Steel equivalents of `tak` and `nqueens` benchmarks for comparing against other Lisp implementations.
- **VM performance roadmap** — `docs/plans/2026-02-17-vm-performance-roadmap.md` analyzing the 7.8x gap vs Janet with 6-phase optimization plan.

## 1.3.0

### Added

- **Bytecode VM (preview)** — full bytecode compiler and virtual machine, opt-in via `--vm` CLI flag. The VM compiles Sema source through macro expansion → CoreExpr lowering → slot resolution → bytecode compilation → VM execution. Passes 173 unit tests, 130 integration tests, and all 44 examples. Key features:
  - **Same-VM closure execution** — VM closures carry an opaque payload on `NativeFn`; calling a closure pushes a `CallFrame` on the same VM instead of creating a fresh `VM::new()`, eliminating native stack growth.
  - **True tail-call optimization** — `tail_call_vm_closure` reuses the current frame, enabling 100K+ depth tail recursion.
  - **Named-let desugaring** — named `let` is desugared to `letrec` + `lambda` in the lowering pass, simplifying the compiler and fixing self-reference injection bugs.
  - **`delay` lowered to thunk** — `delay` compiles to a zero-arg lambda that captures the lexical environment, ensuring delayed expressions see VM locals.
  - **NativeFn fallback interop** — closures passed to stdlib higher-order functions (map, filter, etc.) go through a NativeFn wrapper, maintaining compatibility with `sema-stdlib` which depends on `sema-core`, not `sema-vm`.
- **New crate: `sema-vm`** — bytecode compiler, resolver, and stack-based virtual machine. Dependency flow: `sema-core ← sema-reader ← sema-vm ← sema-eval`.

### Fixed (VM)

- **Self-ref injection corrupting locals** — `make_closure` no longer writes NativeFn self-references into local slots for all named functions; named-let desugaring eliminates the issue entirely.
- **Missing arity checking** — NativeFn wrapper now performs strict arity validation instead of silently filling missing args with Nil.
- **Recursive inner define** — resolver allocates local slots before resolving RHS, fixing `(define (f) (define (g) (g)) (g))`.
- **`delay`/`force` not capturing lexical vars** — `delay` now lowers to a zero-arg lambda thunk that captures the lexical environment.
- **`__vm-import` selective import** — selective names list symbols are now spread individually in the reconstructed import form.

## 1.2.2

### Internal

- **Lambda/Macro params use interned `Spur` handles** — `Lambda.params`, `Lambda.rest_param`, `Lambda.name`, and the corresponding `Macro` fields changed from `String` to `Spur` (interned u32 handles). Parameter names are now interned once at lambda creation time instead of on every function call. This is a structural change in preparation for the bytecode VM, where the compiler needs `Spur` param names for local slot metadata.

## 1.2.1

### Internal

- **Eliminated mini-eval** — deleted the 620-line duplicated evaluator from `sema-stdlib/src/list.rs`. All stdlib higher-order functions (`map`, `filter`, `foldl`, `sort-by`, etc.) and file streaming functions (`file/fold-lines`, `file/for-each-line`) now call through the real evaluator via a callback architecture in `sema-core`. Net change: **-751 lines**.
- **Callback architecture** — `sema-core` provides thread-local `eval_callback` and `call_callback` functions, registered by `sema-eval` during interpreter initialization. This replaces the mini-eval while preserving the dependency constraint (`sema-stdlib` cannot depend on `sema-eval`).
- **Evaluator fast-path optimizations** — self-evaluating forms (Int, Float, String, Symbol, etc.) now skip depth tracking, step counting, and trampoline setup entirely. Deferred cloning in the trampoline avoids unnecessary `Value::clone()` and `Env::clone()` on non-TCO calls. Thread-local shared `EvalContext` eliminates per-call allocations in stdlib callbacks.
- **Public `call_value` API** — new function in `sema-eval` for calling any callable `Value` (Lambda, NativeFn, Keyword) with evaluated arguments, used by stdlib and LLM builtins.

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
- **Architecture decisions** — new `docs/decisions.md` documenting naming conventions, Rc cycle behavior, sandbox system, evaluator callback architecture, package system plans, and LSP roadmap.
- **LSP server design** — new `docs/plans/2026-02-16-lsp-server.md` with 4-phase implementation plan using `tower-lsp`.
- **String docs reorganized** — `string/` namespaced functions now lead; legacy Scheme names grouped under "Scheme Compatibility Aliases".

## 1.1.0

### Added

- **`llm/define-provider`** — define LLM providers entirely in Sema code. The `:complete` function receives a request map (`:model`, `:messages`, `:max-tokens`, `:temperature`, `:system`, `:tools`, `:stop-sequences`) and returns a string or a response map with `:content`, `:usage`, `:tool-calls`, and `:stop-reason`. Supports closures, error propagation via `try`/`catch`, and tool-calling agents.
- **OpenAI-compatible provider fallback** — `llm/configure` with any unknown provider name plus `:api-key` and `:base-url` now registers it as an OpenAI-compatible endpoint. Works with Together AI, Azure OpenAI, Fireworks, vLLM, LiteLLM, and any other OpenAI-compatible service.
- **Tool-call responses from Lisp providers** — Lisp-defined providers can return `:tool-calls` in their response maps, enabling tool-calling agents to work with custom providers.

## 1.0.1

### Improved

- **Structured error messages** — errors now support `.with_hint()` and `.with_note()` for actionable suggestions and context. Reader errors show human-readable token names instead of Rust debug format (e.g. `expected \`)\`, got \`]\``instead of`RParen`/`RBracket`).
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
