# sema-vm

Bytecode compiler and virtual machine for the [Sema](https://sema-lang.com) programming language.

This crate provides an alternative execution backend:

- **Compiler** — lowers s-expression AST to bytecode via an intermediate representation
- **Virtual machine** — stack-based VM with closures, upvalues, and exception handling
- **Serialization** — `.semac` bytecode file format for ahead-of-time compilation

## Usage

This is an internal crate. If you want to embed Sema in your application, use [`sema-lang`](https://crates.io/crates/sema-lang) instead:

```toml
[dependencies]
sema-lang = "1.6"
```

📖 [Architecture](https://sema-lang.com/docs/internals/architecture.html) · [Documentation](https://sema-lang.com/docs/) · [GitHub](https://github.com/helgesverre/sema)
