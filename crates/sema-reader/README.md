# sema-reader

Lexer and s-expression parser for the [Sema](https://sema-lang.com) programming language.

Transforms source text into a `Value` AST. Supports:

- Integers, floats, strings (with escape sequences), booleans, characters
- Symbols, keywords (`:foo`), `nil`
- Lists `()`, vectors `[]`, hash maps `{}`
- Quote `'`, quasiquote `` ` ``, unquote `,`, unquote-splicing `,@`
- Line comments `;` and nestable block comments `#| ... |#`
- Byte vectors `#u8(...)`

## Usage

This is an internal crate. If you want to embed Sema in your application, use [`sema-lang`](https://crates.io/crates/sema-lang) instead:

```toml
[dependencies]
sema-lang = "1.6"
```

📖 [Documentation](https://sema-lang.com/docs/) · [GitHub](https://github.com/helgesverre/sema)
