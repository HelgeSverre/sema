# Sema for Zed

Syntax highlighting and language configuration for [Sema](https://sema-lang.com) (`.sema` files) in the [Zed](https://zed.dev) editor.

- **Homepage**: [sema-lang.com](https://sema-lang.com)
- **Source**: [github.com/helgesverre/sema](https://github.com/helgesverre/sema)
- **Author**: Helge Sverre

## Features

- **Syntax highlighting** — special forms, LLM primitives (`llm/chat`, `defagent`, etc.), 350+ builtins, keyword literals (`:foo`), booleans, `nil`, strings, comments
- **Smart auto-pairs** — `()`, `[]`, `{}`, `""`
- **Comment support** — `;` line comments
- **Code outline** — `define`, `defun`, `defmacro`, `defagent`, `deftool`
- **Bracket matching** — parentheses, brackets, braces
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

## How It Works

This extension uses the dedicated **[tree-sitter-sema](https://github.com/helgesverre/tree-sitter-sema)** grammar for parsing `.sema` files. The grammar provides native support for Sema-specific syntax — keyword literals (`:name`), hash maps, vectors, and more. The query files layer Sema-specific captures on top — highlighting LLM primitives, slash-namespaced builtins (`string/trim`, `llm/chat`), and special forms like `defagent` and `deftool`.
