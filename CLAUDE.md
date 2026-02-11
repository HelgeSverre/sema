# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
make build                                               # dev build
make release                                             # optimized build
make test                                                # all tests (282 tests, all passing)
make lint                                                # fmt-check + clippy -D warnings
make fmt                                                 # cargo fmt
make install                                             # install to ~/.cargo/bin
make all                                                 # lint + test + build
make run                                                 # start REPL

cargo test -p sema-reader                                # test single crate
cargo test -p sema --test integration_test -- test_name  # single integration test
cargo test -p sema -- --ignored                          # run any ignored tests
cargo run -- examples/hello.sema                         # run file
cargo run -- -e "(+ 1 2)"                                # eval expression
```

Integration tests are in `crates/sema/tests/integration_test.rs`. Reader unit tests are in `crates/sema-reader/src/reader.rs`.

## Architecture

Cargo workspace with 6 crates. Dependency flow (arrows = "depends on"):

```
sema-core  ←  sema-reader  ←  sema-eval  ←  sema (binary)
    ↑                            ↑
    ├── sema-stdlib ─────────────┘
    └── sema-llm ────────────────┘
```

- **sema-core** — `Value` enum (18 variants), `Env` (Rc + RefCell + BTreeMap), `SemaError`
- **sema-reader** — Lexer + parser producing `Value` AST
- **sema-eval** — Trampoline-based tree-walking evaluator, 29 special forms, module system
- **sema-stdlib** — 226 native functions across 17 modules registered into `Env`
- **sema-llm** — LLM provider trait + Anthropic/OpenAI clients (tokio `block_on` for sync)
- **sema** — Binary: CLI (clap) + REPL (rustyline) + integration tests

**Critical**: `sema-stdlib` and `sema-llm` depend on `sema-core` but NOT on `sema-eval` (avoids circular deps).

## Key Design Patterns

### Trampoline TCO

`eval_step` returns `Trampoline::Value(v)` (done) or `Trampoline::Eval(expr, env)` (tail call). Special forms must return `Trampoline::Eval` for tail positions to enable proper tail-call optimization.

### Circular Dependency Solutions

- **stdlib HOFs** (map, filter, foldl): `list.rs` has its own mini-eval/`call_function` that handles symbol lookup + function application without depending on sema-eval
- **LLM tool execution**: Thread-local `EvalCallback` set by `Interpreter::new()` gives sema-llm access to the full evaluator

### Module System (Thread-Local State)

`MODULE_CACHE`, `CURRENT_FILE` (stack), `MODULE_EXPORTS` are thread-local. Modules identified by canonical file path. Module env is child of root env (gets builtins, not caller bindings). Paths resolve relative to current file.

### Keywords as Functions

`(:name person)` works like `(get person :name)` — handled in `eval_step` when a `Value::Keyword` appears in head position.

## Code Conventions

### Rust

- Errors: use `SemaError::eval()`, `::type_error()`, `::arity()` constructors — never raw enum variants
- Native fns: `NativeFn` takes `&[Value]`, returns `Result<Value, SemaError>`
- Single-threaded: `Rc` everywhere, not `Arc`. `BTreeMap` for deterministic ordering.

### Sema Language Naming (Decision #24)

- **Slash-namespaced** for all new functions: `file/read`, `path/join`, `regex/match?`, `http/get`, `json/encode`, `string/split`
- **Legacy Scheme** kept: `string-append`, `string-length`, `string-ref`, `substring`
- **Arrow conversions**: `string->symbol`, `keyword->string`
- **Predicates end in `?`**: `null?`, `list?`, `file/exists?`

## Adding New Functionality

**New builtin function**: Add to appropriate `crates/sema-stdlib/src/*.rs`, register in that module's `register()` fn, add integration test.

**New special form**: Add match arm in `try_eval_special()` in `special_forms.rs`, implement handler returning `Trampoline`, add integration test.

## Design Docs

- `agents/DECISIONS.md` — 33 numbered design decisions with rationale
- `agents/PLAN.md` — implementation plan
- `agents/LIMITATIONS.md` — known gaps and limitations
