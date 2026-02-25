# Sema IntelliJ Plugin

Language support for [Sema](https://sema-lang.com), a Lisp dialect with first-class LLM primitives.

## Features

- **Syntax highlighting** — keywords, strings, numbers, comments, symbols, keywords
- **Code completion** — builtins, special forms, user-defined symbols
- **Hover documentation** — builtin docs, function signatures, import info
- **Go to definition** — user definitions, cross-module navigation, import paths
- **Diagnostics** — parse errors and compile-time warnings
- **Code lenses** — ▶ Run top-level forms with inline result display
- **Brace matching** — auto-pair `()`, `[]`, `{}`
- **Commenting** — line (`;`) and block (`#| |#`) comments
- **Run configurations** — right-click `.sema` files to run, or create from Run menu
- **File icons** — `.sema` source and `.semac` bytecode

## Requirements

- IntelliJ IDEA 2024.1+
- [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij) plugin (installed automatically as dependency)
- `sema` binary on PATH (or set `SEMA_PATH` environment variable)

## Installation

### From Source

```bash
cd editors/intellij
./gradlew buildPlugin
# Install from build/distributions/sema-intellij-*.zip via:
# Settings → Plugins → ⚙️ → Install Plugin from Disk…
```

## Configuration

Set the `SEMA_PATH` environment variable to the path of your `sema` binary if it's not on PATH.
