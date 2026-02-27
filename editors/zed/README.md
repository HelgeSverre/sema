# Sema for Zed

Syntax highlighting and language support for [Sema](https://sema-lang.com) (`.sema` files) in the [Zed](https://zed.dev) editor.

- **Homepage**: [sema-lang.com](https://sema-lang.com)
- **Source**: [github.com/helgesverre/sema](https://github.com/helgesverre/sema)
- **Author**: Helge Sverre

## Features

- **Syntax highlighting** — special forms, LLM primitives (`llm/chat`, `defagent`, etc.), 350+ builtins, keyword literals (`:foo`), booleans, `nil`, strings, comments
- **Smart auto-pairs** — `()`, `[]`, `{}`, `""`
- **Comment support** — `;` line comments and `#| ... |#` block comments
- **Code outline & folding** — `define`, `defun`, `defmacro`, `defagent`, `deftool`, `let`, `lambda`, `begin`, `cond`, `try`, `module`, `import`
- **Bracket matching** — parentheses, brackets, braces with rainbow coloring
- **Run file** — inline play button to run `.sema` files (requires `sema` CLI)
- **Vim textobjects** — `af`/`if` for functions, `ac`/`ic` for agents/tools
- **Secret redaction** — hides API keys in `llm/configure` calls during screen sharing
- **2-space indentation**

## Installation

### As a dev extension

1. Open Zed
2. Go to **Zed → Extensions** (or `Cmd+Shift+X`)
3. Click **Install Dev Extension**
4. Select the `editors/zed` directory from the Sema repository

### From source

Clone the Sema repository and point Zed at the extension directory:

```sh
git clone https://github.com/helgesverre/sema.git
# Then install as dev extension pointing to sema/editors/zed/
```

## Running Sema files

The extension includes a **run task** that lets you execute `.sema` files directly from the editor. A play button (▶) appears in the gutter — click it or use `cmd-shift-r` to run the current file.

This requires the `sema` CLI to be installed and available on your `PATH`:

```sh
cargo install sema-lang
```

If you need a custom command during development, override the task in your project's `.zed/tasks.json`:

```json
[
  {
    "label": "sema run (dev)",
    "command": "sema \"$ZED_FILE\"",
    "tags": ["sema-run"]
  }
]
```

## Language Server (LSP)

Sema includes a built-in language server. To enable it in Zed, add the following to your Zed settings (`Cmd+,` or **Zed → Settings → Open Settings**):

```json
{
  "lsp": {
    "sema": {
      "binary": {
        "path": "sema",
        "arguments": ["lsp"]
      }
    }
  },
  "languages": {
    "Sema": {
      "language_servers": ["sema"]
    }
  }
}
```

This requires the `sema` CLI to be installed and available on your `PATH` (see [Running Sema files](#running-sema-files) above).

## How It Works

This extension uses the dedicated **[tree-sitter-sema](https://github.com/helgesverre/tree-sitter-sema)** grammar for parsing `.sema` files. The grammar provides native support for Sema-specific syntax — keyword literals (`:name`), hash maps, vectors, and more. The query files layer Sema-specific captures on top — highlighting LLM primitives, slash-namespaced builtins (`string/trim`, `llm/chat`), and special forms like `defagent` and `deftool`.
