---
outline: [2, 3]
---

# Language Server (LSP)

Sema includes a built-in [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server that provides IDE features for any editor with LSP support. The server communicates over stdio using the standard LSP JSON-RPC protocol.

```bash
sema lsp
```

## Features

### Diagnostics

Real-time error reporting as you type. The LSP runs two analysis passes:

- **Parse diagnostics** (errors) — syntax errors like unclosed parentheses, unterminated strings, and invalid tokens. Uses error recovery to report multiple errors at once.
- **Compile diagnostics** (warnings) — deeper issues caught by the bytecode compilation pipeline, such as unbound variables, arity mismatches, and invalid special form usage. Only runs when parsing succeeds (no false positives from incomplete code).

### Completion

Context-aware autocompletion triggered by `(` and space characters. Completes from three sources:

- **Special forms** — `define`, `defun`, `lambda`, `if`, `cond`, `let`, `match`, `try`, `import`, etc.
- **Standard library** — all built-in functions including namespaced ones like `string/trim`, `file/read`, `http/get`
- **User definitions** — top-level `define`, `defun`, `defn`, `defmacro`, `defagent`, and `deftool` forms in the current document. Cached definitions survive syntax errors while typing.

### Hover

Hover over any symbol to see documentation:

- **Builtins & special forms** — shows documentation pulled from the stdlib reference docs, including descriptions and usage examples
- **User-defined functions** — shows the function signature with parameter list
- **Imported symbols** — shows the signature and which module it was imported from
- **Other builtins** — shows the symbol name and whether it's a special form or built-in function

Hover continues to work even when the file has syntax errors — the LSP uses error recovery to parse as much as possible.

### Go to Definition

Jump to the definition of symbols with precise cursor targeting:

- **User definitions** — `define`, `defun`, `defn`, `defmacro`, `defagent`, `deftool` — jumps directly to the **name** of the definition (e.g., `foo` in `(defun foo ...)`), not the entire form
- **Cross-file definitions** — if a symbol is not defined locally, the LSP follows `import` and `load` paths to find the definition in other files. Imported file parse results are cached by modification time for performance.
- **Imports** — go-to-definition on `(import "utils.sema")` or `(load "config.sema")` opens the referenced file. Supports relative paths, absolute paths, and package imports

Go-to-definition works even when the file has syntax errors.

### Find All References

Find every occurrence of a symbol across all open documents. Searches all files currently tracked by the LSP server.

### Code Lenses

Every top-level expression shows a **▶ Run** code lens above it. Clicking it evaluates all forms up to and including that expression in a sandboxed subprocess using `sema eval`, and reports the result (value, stdout, stderr, timing) back to the editor via a custom `sema/evalResult` notification.

## Editor Setup

### Helix

Add to your `~/.config/helix/languages.toml`:

```toml
[[language]]
name = "sema"
language-servers = ["sema-lsp"]

[language-server.sema-lsp]
command = "sema"
args = ["lsp"]
```

### Neovim

Using [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig):

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

if not configs.sema then
  configs.sema = {
    default_config = {
      cmd = { 'sema', 'lsp' },
      filetypes = { 'sema' },
      root_dir = lspconfig.util.root_pattern('sema.toml', '.git'),
    },
  }
end

lspconfig.sema.setup({})
```

### Zed

Zed can be configured to use the LSP by adding a language server entry. See the [Zed documentation](https://zed.dev/docs/languages) for configuring custom language servers.

### VS Code

The VS Code extension does not yet include LSP integration. For now, the extension provides syntax highlighting only. LSP support is planned.

## Architecture

The LSP server uses [tower-lsp](https://github.com/ebkalderon/tower-lsp) and a dedicated backend thread architecture:

- **Async layer** — tower-lsp handles the JSON-RPC protocol over stdio. Async handlers forward requests to the backend thread via `tokio::sync::mpsc` channels and receive responses via `tokio::sync::oneshot` channels.
- **Backend thread** — a single `std::thread` owns all `Rc`-based state (parsed ASTs, interpreter environment, document cache). This avoids `Send`/`Sync` constraints while keeping the server responsive.
- **Import cache** — parsed results for imported files are cached by file path and modification time, avoiding redundant re-parsing on every request.
- **Subprocess execution** — code lens "Run" commands spawn a separate `sema eval` process in a sandboxed environment, keeping the backend thread free for diagnostics and completions.

### Custom Notifications

The server sends a custom `sema/evalResult` notification after executing a code lens. The payload includes:

| Field       | Type    | Description                               |
| ----------- | ------- | ----------------------------------------- |
| `uri`       | string  | Document URI                              |
| `range`     | Range   | Range of the evaluated form               |
| `kind`      | string  | Always `"run"`                            |
| `value`     | string? | Return value (if successful)              |
| `stdout`    | string  | Captured stdout                           |
| `stderr`    | string  | Captured stderr                           |
| `ok`        | boolean | Whether evaluation succeeded              |
| `error`     | string? | Error message (if failed)                 |
| `elapsedMs` | number  | Execution time in milliseconds            |

## Status

The LSP server provides a complete editing experience for Sema. Planned additions include:

- Rename support
- Signature help
- VS Code extension integration
