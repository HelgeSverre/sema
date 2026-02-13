# Changelog

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
