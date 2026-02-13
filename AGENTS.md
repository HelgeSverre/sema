# AGENTS.md — Sema (Lisp with LLM primitives, in Rust)

## Build & Test

- Build: `cargo build` (or `cargo build --release`)
- Test all: `cargo test`
- Test single crate: `cargo test -p sema-reader`
- Test single test: `cargo test -p sema --test integration_test -- test_name`
- Run file: `cargo run -- examples/hello.sema` | REPL: `cargo run`
- Eval expression: `cargo run -- -e "(+ 1 2)"`

Integration tests are in `crates/sema/tests/integration_test.rs`. Reader unit tests are in `crates/sema-reader/src/reader.rs`.

## Architecture (Cargo workspace)

- **sema-core** — `Value` enum, `Env` (Rc + RefCell + BTreeMap), `SemaError` (via `thiserror`)
- **sema-reader** — Lexer + s-expression parser → `Value` AST
- **sema-eval** — Trampoline-based tree-walking evaluator, special forms, module system
- **sema-stdlib** — Native functions across modules registered into `Env`
- **sema-llm** — LLM provider trait + Anthropic/OpenAI API clients (async via `tokio`/`reqwest`)
- **sema** — Binary crate: REPL (`rustyline`) and file runner; integration tests in `crates/sema/tests/`
- Dependency flow: `sema-core` ← `sema-reader` ← `sema-eval` ← `sema-stdlib` / `sema-llm` ← `sema`
- **Critical**: `sema-stdlib` and `sema-llm` depend on `sema-core` but NOT on `sema-eval` (avoids circular deps).

## Code Style

- Rust 2021 edition, workspace resolver v2. Errors use `thiserror` with `SemaError` enum.
- Use `SemaError::eval()`, `::type_error()`, `::arity()` constructors, not raw variant construction.
- Functions return `Result<Value, SemaError>`. Native builtins are `NativeFn` (takes `&[Value]`, returns `Result<Value, SemaError>`).
- Single-threaded: `Rc` everywhere, not `Arc`. `BTreeMap` for deterministic ordering.
- Sema language uses Clojure-style keywords (`:foo`), maps (`{:k v}`), vectors (`[1 2 3]`), bytevectors (`#u8(1 2 3)`), and Scheme-style `define`/`lambda`/`let`.
- Record types via `define-record-type` special form. `Value::Record` stores type tag (Spur) + positional fields (Vec<Value>).
- Stdlib functions use `/` as namespace separator (e.g. `string/trim`, `file/read`, `math/gcd`, `bit/and`), not `-`.
- Legacy Scheme names kept: `string-append`, `string-length`, `string-ref`, `substring`.
- Arrow conversions: `string->symbol`, `keyword->string`. Predicates end in `?`: `null?`, `list?`, `file/exists?`.

## Adding New Functionality

- **New builtin function**: Add to appropriate `crates/sema-stdlib/src/*.rs`, register in that module's `register()` fn, add integration test.
- **New special form**: Add match arm in `try_eval_special()` in `special_forms.rs`, implement handler returning `Trampoline`, add integration test.

## Release Procedure

To release a new version (e.g., `0.7.0`):

1. **Run tests**: `cargo test` — all must pass
2. **Bump versions** in all 6 `crates/*/Cargo.toml` files (`version = "X.Y.Z"`)
3. **Update CHANGELOG.md** — add new `## X.Y.Z` section at top with `### Added` / `### Changed` / `### Fixed`
4. **Update docs**:
   - `README.md` — add new functions to the Standard Library section, update builtin count
   - `CLAUDE.md` — update builtin count if changed
   - `examples/stdlib/*.sema` — add example tests for new functions
   - `website/index.html` — add new functions to the stdlib-card `fn-list` spans
5. **Build release**: `cargo build --release`
6. **Commit**: `git add -A && git commit -m "v0.X.0: <summary>"`
7. **Tag**: `git tag v0.X.0`
8. **Push**: `git push origin main --tags`
9. **Deploy website**: `cd website && vc --prod`

## Design Docs

- `agents/DECISIONS.md` — Numbered design decisions with rationale
- `agents/PLAN.md` — Implementation plan
- `agents/LIMITATIONS.md` — Known gaps and limitations
