# AGENTS.md — Sema (Lisp with LLM primitives, in Rust)

## Build & Test

- Build: `cargo build` | Lint: `make lint` (fmt-check + clippy -D warnings) | All tests: `cargo test`
- Single crate: `cargo test -p sema-reader` | Single test: `cargo test -p sema --test integration_test -- test_name`
- Run file: `cargo run -- examples/hello.sema` | REPL: `cargo run` | Eval: `cargo run -- -e "(+ 1 2)"`
- Integration tests: `crates/sema/tests/integration_test.rs`. Reader unit tests: `crates/sema-reader/src/reader.rs`.

## Architecture (Cargo workspace, 6 crates)

- **sema-core** → `Value` enum, `Env` (Rc+RefCell+BTreeMap), `SemaError` (thiserror)
- **sema-reader** → Lexer + s-expression parser → `Value` AST
- **sema-eval** → Trampoline-based TCO evaluator, special forms, module system (thread-local `MODULE_CACHE`/`CURRENT_FILE`)
- **sema-stdlib** → 350+ native functions across 17 modules registered into `Env`
- **sema-llm** → LLM provider trait + Anthropic/OpenAI clients (tokio `block_on`)
- **sema** → Binary (clap CLI + rustyline REPL) + integration tests
- Dep flow: `sema-core ← sema-reader ← sema-eval ← sema-stdlib/sema-llm ← sema`. **Critical**: stdlib/llm depend on core, NOT eval.

## Code Style

- Rust 2021, single-threaded (`Rc`, not `Arc`), `BTreeMap` for deterministic ordering.
- Errors: `SemaError::eval()` / `::type_error()` / `::arity()` constructors — never raw enum variants.
- Native fns: `NativeFn` takes `&[Value]` → `Result<Value, SemaError>`. Special forms return `Trampoline`.
- Sema naming: slash-namespaced (`string/trim`, `file/read`), predicates end `?`, arrows for conversions (`string->symbol`). Legacy Scheme names kept (`string-append`, `substring`).

## Adding Functionality

- **Builtin fn**: add to `crates/sema-stdlib/src/*.rs`, register in `register()`, add integration test.
- **Special form**: add match arm in `try_eval_special()` in `special_forms.rs`, return `Trampoline`, add integration test.
