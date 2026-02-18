# tree-sitter-sema

Tree-sitter grammar for [Sema](https://sema-lang.com), a Lisp with LLM primitives.

> **Note:** This package is auto-published as a read-only mirror from the main [Sema monorepo](https://github.com/helgesverre/sema).

## Usage

```sh
tree-sitter generate && tree-sitter test
```

## Node Types

The grammar produces the following node types:

- `list` — parenthesized list `(...)` 
- `vector` — square-bracket vector `[...]`
- `hash_map` — curly-brace map `{...}`
- `symbol` — identifiers and operators
- `keyword` — colon-prefixed keywords `:foo`
- `number` — integer and floating-point literals
- `string` — double-quoted string literals
- `boolean` — `#t` and `#f`
- `character` — character literals `#\a`
- `byte_vector` — byte vector literals `#u8(...)`
- `comment` — line comments `;`
- `block_comment` — block comments `#| ... |#`
- `quote` — `'expr`
- `quasiquote` — `` `expr ``
- `unquote` — `,expr`
- `unquote_splicing` — `,@expr`

## Links

- [Sema Language](https://sema-lang.com)
- [Sema Repository](https://github.com/helgesverre/sema)

## License

MIT
