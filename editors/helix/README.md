# Sema Support for Helix

Syntax highlighting and language configuration for [Sema](https://sema-lang.com) (`.sema` files) in the [Helix](https://helix-editor.com) editor.

- **Homepage**: [sema-lang.com](https://sema-lang.com)
- **Source**: [github.com/helgesverre/sema](https://github.com/helgesverre/sema)
- **Author**: Helge Sverre

This configuration reuses the built-in **Scheme tree-sitter grammar** (since Sema is a Scheme-like Lisp) and layers Sema-specific highlight queries on top for proper keyword, builtin, and LLM primitive highlighting.

## Features

- **Syntax highlighting** — special forms, LLM primitives (`llm/chat`, `defagent`, etc.), builtins, keyword literals (`:foo`), booleans (`#t`/`#f`), `nil`, strings, comments
- **Smart auto-pairs** — `()`, `[]`, `{}`, `""`
- **Comment support** — `;` line comments, `#| |#` block comments
- **2-space indentation**

## Installation

### 1. Copy the language configuration

Append (or merge) the contents of `languages.toml` into your Helix config:

```sh
cat editors/helix/languages.toml >> ~/.config/helix/languages.toml
```

> **Note:** If you already have a `languages.toml`, manually merge the `[[language]]` section to avoid duplicates.

### 2. Copy the query files

```sh
mkdir -p ~/.config/helix/runtime/queries/sema
cp editors/helix/queries/sema/*.scm ~/.config/helix/runtime/queries/sema/
```

### 3. Fetch the Scheme grammar (if not already present)

Since Sema reuses the Scheme tree-sitter grammar, make sure it's installed:

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

Helix's `grammar = "scheme"` setting tells Helix to parse `.sema` files using the Scheme tree-sitter grammar. The custom query files in `queries/sema/` override the default Scheme highlights with Sema-specific captures — adding highlighting for Sema's LLM primitives, slash-namespaced builtins, keyword literals, and special forms like `defagent` and `deftool`.

## Troubleshooting

- **No highlighting?** Make sure `HELIX_RUNTIME` is not overriding the query path. Helix looks for queries in `~/.config/helix/runtime/queries/` and then in its built-in runtime directory.
- **Grammar not found?** Run `hx --grammar fetch && hx --grammar build` to ensure the Scheme grammar is compiled.
- **Stale config?** Restart Helix after changing `languages.toml` — it does not hot-reload language configuration.
