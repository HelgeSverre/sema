# sema-eval

Trampoline-based evaluator and module system for the [Sema](https://sema-lang.com) programming language.

This crate contains the core evaluation engine:

- **Trampoline-based TCO** — proper tail-call optimization without stack overflow
- **Special forms** — `define`, `lambda`, `if`, `let`, `cond`, `try/catch`, macros, and more
- **Module system** — `import`, `export`, `module` with caching
- **`EvalContext`** — holds module cache, call stack, span table, and depth counters

## Usage

This is an internal crate. If you want to embed Sema in your application, use [`sema-lang`](https://crates.io/crates/sema-lang) instead:

```toml
[dependencies]
sema-lang = "1.6"
```

📖 [Evaluator internals](https://sema-lang.com/docs/internals/evaluator.html) · [Documentation](https://sema-lang.com/docs/) · [GitHub](https://github.com/helgesverre/sema)
