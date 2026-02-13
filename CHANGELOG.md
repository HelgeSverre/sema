# Changelog

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
