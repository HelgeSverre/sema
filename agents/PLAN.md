# Sema: A Lisp with LLM Primitives — Implementation Plan

## Core Concept
A Lisp interpreter in Rust with first-class primitives for working with LLMs. Prompts are s-expressions, conversations are persistent data structures, and LLM calls are just another form of evaluation.

**Dialect:** Scheme-like core with Clojure-style keywords (`:foo`) and map literals (`{:key val}`)
**Async model:** Sync by default, explicit `llm/parallel` for concurrency
**Strategy:** Bottom-up — solid core Lisp first, then LLM primitives on top

## Crate Structure
```
crates/
  sema-core/     # Value enum, errors, spans, Env
  sema-reader/   # lexer + s-expression reader
  sema-eval/     # evaluator, special forms
  sema-llm/      # LLM provider trait + API clients
  sema-stdlib/   # standard library builtins
  sema/          # binary: REPL + file runner
```

## Implementation Sequence

| Step | What | Crate | Status |
|------|-------|-------|--------|
| 1 | Workspace + Value enum + Error types | sema-core | Done |
| 2 | Lexer (tokenizer) | sema-reader | Done |
| 3 | Reader (tokens → Value) | sema-reader | Done |
| 4 | Environment + eval + special forms | sema-eval | Done |
| 5 | Standard library builtins | sema-stdlib | Done |
| 6 | REPL + CLI | sema | Done |
| 7 | Macros (defmacro + quasiquote eval) | sema-eval | Done |
| 8 | Provider trait + Anthropic client | sema-llm | Done |
| 9 | `llm/complete`, `llm/chat` builtins | sema-eval + sema-llm | Done |
| 10 | Prompt data type + `prompt` special form | sema-core + sema-eval | Done |
| 11 | Conversation data type | sema-core + sema-llm | Done |
| 12 | `llm/extract`, `llm/classify` | sema-llm | Done |
| 13 | `deftool` + tool execution loop | sema-eval + sema-llm | Done |
| 14 | `defagent` + agent loop | sema-eval + sema-llm | Done |
| 15 | `llm/parallel`, `llm/pmap` | sema-llm | Done |

## Key Design Decisions

### Value Type
- `Rc`-based (not `Arc`) — single-threaded Lisp, LLM concurrency at the provider level
- `BTreeMap` for maps (gives deterministic ordering + Ord for keys)
- Floats compared by bits for Ord impl (needed for BTreeMap keys)

### Environment
- `Rc<Env>` with parent chain for lexical scoping
- `RefCell<BTreeMap>` for mutable bindings within a scope
- `set_existing` walks the chain for `set!` semantics

### Reader
- Hand-written recursive descent (no parser combinator deps)
- Quasiquote desugars to `(quasiquote ...)` forms during reading
- Map literals `{:k v}` are first-class syntax

### Evaluator
- Trampoline-based for tail-call optimization
- Special forms handled in a separate module
- Macros: unhygienic `defmacro`, expand-then-eval

### LLM Integration
- `LlmProvider` trait with sync interface (internally async via tokio block_on)
- Anthropic Messages API as primary provider
- OpenAI-compatible endpoint for flexibility
- All LLM types (Prompt, Message, Conversation, ToolDef) are first-class Values

## Phase 6: Fix Critical Gaps

| Step | What | Status |
|------|------|--------|
| 1 | Extended list ops (multi-map, take, drop, zip, etc.) | Done |
| 2 | Named `let` + `letrec` | Done |
| 3 | Error handling (`try`/`catch`/`throw`) | Done |
| 4 | Module system (`module`/`import`) | Done |

76 tests total (16 reader + 60 integration), all passing.

## Phase 7: Real-World Capabilities

| Step | What | Status |
|------|------|--------|
| 1 | Metaprogramming: `case`, `eval`, `read`, `gensym`, `macroexpand`, type conversions | Done |
| 2 | File I/O + path operations (`file/` and `path/` namespaces) | Done |
| 3 | Regex (`regex/` namespace, `regex` crate) | Done |
| 4 | HTTP client (`http/` namespace, `reqwest`+`tokio`) | Done |

92 tests total (16 reader + 76 integration), all passing.

## Phase 8: Standard Library Expansion

| Step | What | Status |
|------|------|--------|
| 1 | Math: trig (tan, asin, acos, atan, atan2), logarithmic (exp, log10, log2), utility (random, random-int, clamp, sign, gcd, lcm, quotient, remainder) | Done |
| 2 | Bitwise: `bit/and`, `bit/or`, `bit/xor`, `bit/not`, `bit/shift-left`, `bit/shift-right` | Done |
| 3 | String: `string/starts-with?`, `string/ends-with?`, `string/upper`, `string/lower`, `string/pad-left`, `string/pad-right`, `string/chars`, `string/join`, `string/repeat` | Done |
| 4 | Map: `map/map-vals`, `map/filter`, `map/update`, `map/select-keys`, `map/count` | Done |
| 5 | List: `list/index-of`, `list/unique`, `list/group-by`, `list/interleave`, `list/chunk` | Done |
| 6 | System: `sys/args`, `sys/cwd`, `sys/platform`, `sys/env-all` | Done |
| 7 | Crypto/encoding: `uuid/v4`, `base64/encode`, `base64/decode`, `hash/sha256` | Done |
| 8 | Date/time: `time/now`, `time/format`, `time/parse`, `time/date-parts` | Done |
| 9 | CSV: `csv/parse`, `csv/parse-maps`, `csv/encode` | Done |
| 10 | Examples: 7 comprehensive example programs covering all features | Done |

282 tests total (16 reader + 266 integration), all passing.

## Verification Checkpoints
- **Phase 1:** `cargo build` succeeds
- **Phase 2:** REPL evaluates `(factorial 10)` → `3628800`
- **Phase 3:** `(llm/complete "Say hello")` returns LLM response
- **Phase 6:** 76 tests pass, modules import/export correctly
- **Phase 7:** 92 tests pass, `(http/get "https://httpbin.org/get")` returns `{:status 200 ...}`
- **Phase 8:** 282 tests pass, all 7 example programs run successfully
