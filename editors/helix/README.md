# Sema Support for Helix

Syntax highlighting and language configuration for [Sema](https://sema-lang.com) (`.sema` files) in the [Helix](https://helix-editor.com) editor.

- **Homepage**: [sema-lang.com](https://sema-lang.com)
- **Source**: [github.com/helgesverre/sema](https://github.com/helgesverre/sema)
- **Author**: Helge Sverre

This configuration uses the dedicated **[tree-sitter-sema](https://github.com/helgesverre/tree-sitter-sema)** grammar for parsing `.sema` files, with Sema-specific highlight queries for proper keyword, builtin, and LLM primitive highlighting.

## Features

- **Syntax highlighting** — special forms, LLM primitives (`llm/chat`, `defagent`, etc.), builtins, keyword literals (`:foo`), booleans, `nil`, strings, comments
- **Smart auto-pairs** — `()`, `[]`, `{}`, `""`
- **Comment support** — `;` line comments
- **2-space indentation**

## Installation

### 1. Copy the language configuration

Append (or merge) the contents of `languages.toml` into your Helix config:

```sh
cat editors/helix/languages.toml >> ~/.config/helix/languages.toml
```

> **Note:** If you already have a `languages.toml`, manually merge the `[[language]]` and `[[grammar]]` sections to avoid duplicates.

### 2. Copy the query files

```sh
mkdir -p ~/.config/helix/runtime/queries/sema
cp editors/helix/queries/sema/*.scm ~/.config/helix/runtime/queries/sema/
```

### 3. Fetch and build the Sema grammar

```sh
hx --grammar fetch
hx --grammar build
```

### 4. Verify

Check that Helix recognizes the language:

```sh
hx --health sema
```

Then open any `.sema` file:

```sh
hx examples/hello.sema
```

## How It Works

Helix's `grammar = "sema"` setting tells Helix to parse `.sema` files using the [tree-sitter-sema](https://github.com/helgesverre/tree-sitter-sema) grammar, which provides native support for Sema-specific syntax like keyword literals (`:name`), hash maps, and vectors. The custom query files in `queries/sema/` provide Sema-specific captures — adding highlighting for LLM primitives, slash-namespaced builtins, keyword literals, and special forms like `defagent` and `deftool`.

## Troubleshooting

- **No highlighting?** Make sure `HELIX_RUNTIME` is not overriding the query path. Helix looks for queries in `~/.config/helix/runtime/queries/` and then in its built-in runtime directory.
- **Grammar not found?** Run `hx --grammar fetch && hx --grammar build` to ensure the Sema grammar is compiled.
- **Stale config?** Restart Helix after changing `languages.toml` — it does not hot-reload language configuration.
