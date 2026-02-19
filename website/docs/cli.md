---
outline: [2, 3]
---

# CLI Reference

```
sema [OPTIONS] [FILE] [-- SCRIPT_ARGS...]
```

## Flags & Options

| Flag                 | Description                                  |
| -------------------- | -------------------------------------------- |
| `-e, --eval <EXPR>`  | Evaluate expression, print result if non-nil |
| `-p, --print <EXPR>` | Evaluate expression, always print result     |
| `-l, --load <FILE>`  | Load file(s) before executing (repeatable)   |
| `-q, --quiet`        | Suppress REPL banner                         |
| `-i, --interactive`  | Enter REPL after running file or eval        |
| `--no-init`          | Skip LLM auto-configuration                  |
| `--no-llm`           | Disable LLM features (same as `--no-init`)   |
| `--chat-model <NAME>`       | Set default chat model                |
| `--chat-provider <NAME>`    | Set chat provider                     |
| `--embedding-model <NAME>`  | Set embedding model                   |
| `--embedding-provider <NAME>` | Set embedding provider              |
| `--vm`               | Use bytecode VM instead of tree-walker       |
| `--sandbox <MODE>`   | Restrict dangerous operations (see below)    |
| `-V, --version`      | Print version                                |
| `-h, --help`         | Print help                                   |

## Subcommands

### `sema ast`

Parse source into an AST tree.

```
sema ast [OPTIONS] [FILE]
```

| Flag                | Description                      |
| ------------------- | -------------------------------- |
| `-e, --eval <EXPR>` | Parse expression instead of file |
| `--json`            | Output AST as JSON               |

### `sema compile`

Compile a source file to a `.semac` bytecode file. The compiled file can be executed directly with `sema` (auto-detected via magic number). See [Bytecode File Format](./internals/bytecode-format.md) for details on the format.

```
sema compile [OPTIONS] <FILE>
```

| Flag                  | Description                                          |
| --------------------- | ---------------------------------------------------- |
| `-o, --output <FILE>` | Output file path (default: input with `.semac` extension) |
| `--check`             | Validate a `.semac` file without executing            |

```bash
# Compile to bytecode
sema compile script.sema                   # → script.semac
sema compile -o output.semac script.sema   # explicit output path

# Run the compiled bytecode (auto-detected)
sema script.semac

# Validate a bytecode file
sema compile --check script.semac
# ✓ script.semac: valid (format v1, sema 1.6.2, 3 functions, 847 bytes)
```

### `sema disasm`

Disassemble a compiled `.semac` bytecode file, printing a human-readable listing of the main chunk and all function templates.

```
sema disasm [OPTIONS] <FILE>
```

| Flag     | Description    |
| -------- | -------------- |
| `--json` | Output as JSON |

```bash
sema disasm script.semac          # human-readable text
sema disasm --json script.semac   # structured JSON output
```

### `sema completions`

Generate shell completion scripts. See [Shell Completions](./shell-completions.md) for installation instructions.

```
sema completions <SHELL>
```

Supported shells: `bash`, `zsh`, `fish`, `elvish`, `powershell`.

## Examples

```bash
# Parse a file into an AST tree
sema ast script.sema

# Parse an expression into JSON AST
sema ast -e '(+ 1 2)' --json

# Load a prelude before starting the REPL
sema -l prelude.sema

# Load helpers, then run a script
sema -l helpers.sema script.sema

# Run a script and drop into REPL to inspect state
sema -i script.sema

# Quick one-liner for shell pipelines
sema -p '(string/join (map str (range 10)) ",")'

# Run without LLM features (faster startup)
sema --no-llm script.sema

# Use the bytecode VM (faster execution for compute-heavy code)
sema --vm script.sema

# Compile to bytecode and run
sema compile script.sema
sema script.semac

# Use a specific model
sema --chat-model claude-haiku-4-5-20251001 -e '(llm/complete "Hello!")'

# Shebang support in scripts
#!/usr/bin/env sema

# Run with shell commands disabled
sema --sandbox=no-shell script.sema

# Deny multiple capabilities
sema --sandbox=no-shell,no-network,no-fs-write script.sema

# Strict mode (no shell, fs-write, network, env-write, process, llm)
sema --sandbox=strict script.sema

# Maximum restriction (deny all dangerous operations)
sema --sandbox=all script.sema

# Restrict file operations to specific directories
sema --allowed-paths=./data,./output script.sema

# Combine sandbox and path restrictions
sema --sandbox=strict --allowed-paths=./data script.sema
```

## Sandbox

The `--sandbox` flag restricts access to dangerous operations. Functions remain callable but return a `PermissionDenied` error when invoked.

### Modes

| Mode            | Description                                                            |
| --------------- | ---------------------------------------------------------------------- |
| `strict`        | Deny shell, fs-write, network, env-write, process, llm (reads allowed) |
| `all`           | Deny all capabilities                                                  |
| Comma-separated | e.g. `no-shell,no-network` — deny specific capabilities                |

### Capabilities

| Capability  | Functions affected                                                         |
| ----------- | -------------------------------------------------------------------------- |
| `shell`     | `shell`                                                                    |
| `fs-read`   | `file/read`, `file/exists?`, `file/list`, `file/info`, `load`, ...         |
| `fs-write`  | `file/write`, `file/append`, `file/delete`, `file/mkdir`, `file/copy`, ... |
| `network`   | `http/get`, `http/post`, `http/put`, `http/delete`, `http/request`         |
| `env-read`  | `env`, `sys/env-all`                                                       |
| `env-write` | `sys/set-env`                                                              |
| `process`   | `exit`, `sys/pid`, `sys/args`, `sys/which`                                 |
| `llm`       | `llm/complete`, `llm/chat`, `llm/send`                                     |

Functions not listed (arithmetic, strings, lists, maps, `println`, `path/join`, etc.) are never restricted.

### Path Restrictions

The `--allowed-paths` flag restricts all file operations to specific directories. Paths are canonicalized, so traversal attacks like `../../etc/passwd` are blocked.

```bash
# Only allow reading/writing within ./project and /tmp
sema --allowed-paths=./project,/tmp script.sema
```

When `--allowed-paths` is set, any file operation (`file/read`, `file/write`, `file/list`, etc.) targeting a path outside the allowed directories returns a `PermissionDenied` error. This works independently of `--sandbox` — you can use both together:

```bash
# Allow filesystem but only within ./data
sema --sandbox=no-shell,no-network --allowed-paths=./data script.sema
```

## Environment Variables

| Variable             | Description                                           |
| -------------------- | ----------------------------------------------------- |
| `ANTHROPIC_API_KEY`  | Anthropic API key (auto-detected)                     |
| `OPENAI_API_KEY`     | OpenAI API key (auto-detected)                        |
| `GROQ_API_KEY`       | Groq API key (auto-detected)                          |
| `XAI_API_KEY`        | xAI/Grok API key (auto-detected)                      |
| `MISTRAL_API_KEY`    | Mistral API key (auto-detected)                       |
| `MOONSHOT_API_KEY`   | Moonshot API key (auto-detected)                      |
| `GOOGLE_API_KEY`     | Google Gemini API key (auto-detected)                 |
| `OLLAMA_HOST`        | Ollama server URL (default: `http://localhost:11434`) |
| `JINA_API_KEY`       | Jina embeddings API key (auto-detected)               |
| `VOYAGE_API_KEY`     | Voyage embeddings API key (auto-detected)             |
| `COHERE_API_KEY`     | Cohere embeddings API key (auto-detected)             |
| `SEMA_CHAT_MODEL`    | Default chat model name                               |
| `SEMA_CHAT_PROVIDER` | Preferred chat provider                               |
| `SEMA_EMBEDDING_MODEL` | Default embedding model name                        |
| `SEMA_EMBEDDING_PROVIDER` | Preferred embedding provider                    |

::: tip Deprecated aliases
`SEMA_DEFAULT_MODEL` and `SEMA_LLM_PROVIDER` still work but are deprecated. Use `SEMA_CHAT_MODEL` and `SEMA_CHAT_PROVIDER` instead.
:::

## REPL Commands

| Command        | Description                 |
| -------------- | --------------------------- |
| `,quit` / `,q` | Exit the REPL               |
| `,help` / `,h` | Show help                   |
| `,env`         | Show user-defined bindings  |
| `,builtins`    | List all built-in functions |

## REPL Features

### Tab Completion

The REPL supports tab completion for:

- All 350+ built-in function names (e.g., `string/tr` → `string/trim`)
- Special forms (`def` → `define`, `defun`, `defmacro`, ...)
- User-defined bindings
- REPL commands (`,` → `,quit`, `,help`, `,env`, `,builtins`)

### Multiline Input

The REPL automatically detects incomplete expressions (unbalanced parentheses) and continues on the next line:

```
sema> (define (factorial n)
  ...   (if (= n 0)
  ...     1
  ...     (* n (factorial (- n 1)))))
sema> (factorial 10)
3628800
```

### History

Command history is saved to `~/.sema/history.txt` and persists across sessions.
