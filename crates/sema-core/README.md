# sema-core

Core types and environment for the [Sema](https://sema-lang.com) programming language.

This crate provides the foundational types used across all Sema crates:

- **`Value`** — the core value enum (Int, Float, String, List, Map, Lambda, etc.)
- **`Env`** — lexical environment with `Rc<RefCell<HashMap>>` scope chains
- **`SemaError`** — error type with `eval()`, `type_error()`, and `arity()` constructors
- **`Sandbox` / `Caps`** — capability-based sandboxing for restricting script permissions
- **`intern()` / `resolve()`** — thread-local string interner for keywords and symbols

## Usage

This is an internal crate. If you want to embed Sema in your application, use [`sema-lang`](https://crates.io/crates/sema-lang) instead:

```toml
[dependencies]
sema-lang = "1.6"
```

📖 [Embedding guide](https://sema-lang.com/docs/embedding.html) · [Documentation](https://sema-lang.com/docs/) · [GitHub](https://github.com/helgesverre/sema)
