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

::: info Imports resolve at runtime
`sema compile` only compiles the specified file — it does not bundle dependencies. When you run the `.semac` file, `(import ...)` and `(load ...)` are resolved from the filesystem at runtime. All imported packages must be installed on the target machine. For a fully self-contained artifact, use [`sema build`](#sema-build) instead.
:::

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

### `sema build`

Build a standalone executable from a Sema source file. The resulting binary embeds the compiled bytecode, all transitive imports, and any explicitly included assets into a self-contained executable. See [Executable Format](./internals/executable-format.md) for details on the binary format.

```
sema build [OPTIONS] <FILE>
```

| Flag                     | Description                                               |
| ------------------------ | --------------------------------------------------------- |
| `-o, --output <PATH>`   | Output executable path (default: filename without extension) |
| `--include <PATH>...`   | Additional files or directories to bundle (repeatable)    |
| `--runtime <PATH>`      | Sema binary to use as runtime base (default: current exe) |

```bash
# Build a standalone executable
sema build script.sema                        # → ./script
sema build script.sema -o myapp              # explicit output path

# Bundle additional files
sema build script.sema --include data.json   # bundle a file
sema build script.sema --include assets/     # bundle a directory

# Run the standalone executable
./myapp --arg1 --arg2
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

### `sema pkg`

Package manager for installing, publishing, and managing Sema packages. Git-based packages work out of the box. Registry commands (`search`, `info`, `publish`, `yank`, `login`) require a running registry instance — see [Self-Hosted Registry](./packages.md#self-hosted-registry). See the full [Packages](./packages.md) documentation for details.

```
sema pkg <COMMAND>
```

| Subcommand                  | Description                                         |
| --------------------------- | --------------------------------------------------- |
| `init`                      | Initialize a new `sema.toml` in the current directory |
| `add <spec> [--registry]`   | Add a package from the registry or git              |
| `install`                   | Install all deps from `sema.toml`                   |
| `update [name]`             | Update packages (all or specific)                   |
| `remove <name>`             | Remove an installed package                         |
| `list`                      | List installed packages                             |
| `publish [--registry]`      | Publish current package to the registry             |
| `search <query> [--registry]` | Search the registry for packages                 |
| `info <name> [--registry]`  | Show package info from the registry                 |
| `yank <name@version> [--registry]` | Yank a published version                     |
| `login [--token] [--registry]` | Authenticate with a registry                     |
| `logout`                    | Remove stored registry credentials                  |
| `config [key] [value]`      | View or set package manager configuration           |

```bash
# Install a registry package
sema pkg add http-helpers@1.0.0

# Install a git package
sema pkg add github.com/user/repo@v2.0

# Publish to the registry
sema pkg login --token sema_pat_...
sema pkg publish

# Search for packages
sema pkg search json

# Set default registry
sema pkg config registry.url https://my-registry.com
```

### `sema completions`

Generate shell completion scripts. See [Shell Completions](./shell-completions.md) for installation instructions.

```
sema completions [OPTIONS] <SHELL>
```

| Flag        | Description                                                        |
| ----------- | ------------------------------------------------------------------ |
| `--install` | Auto-detect the shell's completion directory and install the script |

Supported shells: `bash`, `zsh`, `fish`, `elvish`, `powershell`.

The `--install` flag is supported for Bash, Zsh, Fish, and Elvish. For PowerShell, use `sema completions powershell` and follow the manual installation steps in [Shell Completions](./shell-completions.md).

```bash
# Print completion script to stdout
sema completions zsh

# Auto-install to the correct directory
sema completions --install zsh
```

### `sema fmt`

Format Sema source files. See [Formatter](./formatter.md) for full documentation.

```
sema fmt [OPTIONS] [FILES...]
```

| Flag | Description |
| --- | --- |
| `--check` | Check formatting without writing (exit 1 if unformatted) |
| `--diff` | Print diff of changes |
| `--width <N>` | Max line width (default: `80`) |
| `--indent <N>` | Indentation width (default: `2`) |
| `--align` | Align consecutive similar forms |

```bash
# Format all .sema files recursively
sema fmt

# Check in CI
sema fmt --check

# Preview changes
sema fmt --diff
```

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

## Shebang Scripts

Sema supports `#!` (shebang) lines, so you can write executable scripts:

```sema
#!/usr/bin/env sema
(println "Hello from a sema script!")
```

Make the file executable and run it directly:

```bash
chmod +x script.sema
./script.sema
```

The shebang line is only allowed on the first line of a file and is treated as a comment. `#!/usr/bin/env sema` uses the standard `env` lookup, so it works regardless of how sema was installed (Homebrew, Cargo, manual, etc.).

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

| Command        | Description                          |
| -------------- | ------------------------------------ |
| `,quit` / `,q` | Exit the REPL                        |
| `,help` / `,h` | Show help                            |
| `,env`         | Show user-defined bindings           |
| `,builtins`    | List all built-in functions          |
| `,type EXPR`   | Evaluate expression and show its type |
| `,time EXPR`   | Evaluate expression and show elapsed time |
| `,doc NAME`    | Show info about a binding or special form |

```
sema> ,type 42
:integer

sema> ,type '(1 2 3)
:list

sema> ,doc map
  map : native-fn

sema> ,doc if
  if : special form

sema> ,doc factorial
  factorial : lambda (n)

sema> ,time (foldl + 0 (range 100000))
4999950000
elapsed: 58.424ms
```

## REPL Features

### Tab Completion

The REPL supports tab completion for:

- All 460+ built-in function names (e.g., `string/tr` → `string/trim`)
- Special forms (`def` → `define`, `defun`, `defmacro`, ...)
- User-defined bindings
- REPL commands (`,` → `,quit`, `,help`, `,env`, `,builtins`, `,type`, `,time`, `,doc`)

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

### Shadowing Warnings

The REPL warns when you accidentally redefine a built-in function:

```
sema> (define map 42)
  warning: redefining builtin 'map'
```

This is only a warning — the redefinition still works. It helps catch accidental name collisions.

### History

Command history is saved to `~/.sema/history.txt` and persists across sessions.

## Error Messages

Sema provides detailed, colorized error messages with source context and actionable hints.

### Source Context

Errors show the offending source line with a caret pointing to the problem:

```
Error: Reader error at 1:16: unterminated string
  --> script.sema:1:16
   |
 1 | (define name "hello
   |                ^
  hint: add a closing `"` to end the string
```

### Type Errors

Type errors show the actual value that caused the problem:

```
Error: Type error: expected number, got string ("hello")
  --> <input>:1:1
   |
 1 | (+ "hello" 42)
   | ^
  at + (<input>:1:1)
```

### Arity Errors

When you pass the wrong number of arguments, the error shows what you called:

```
Error: Arity error: f expects 1 args, got 3
  --> <input>:1:18
   |
 1 | (define (f x) x) (f 1 2 3)
   |                  ^
  at f (<input>:1:18)
  note: in: (f 1 2 3)
```

### Mismatched Brackets

Mixed bracket types are caught with specific guidance:

```
Error: Reader error at 1:7: mismatched bracket: expected `]` to close `[`, found `)`
  hint: this vector was opened with `[` — close it with `]`
```

### "Did You Mean?"

Typos in function or variable names trigger fuzzy suggestions:

```
Error: Unbound variable: pritnln
  hint: Did you mean 'println'?
```

### Lisp Dialect Hints

If you use names from other Lisp dialects (Common Lisp, Clojure, Scheme), Sema provides targeted guidance:

```
Error: Unbound variable: setq
  hint: Sema uses 'set!' for variable assignment

Error: Unbound variable: funcall
  hint: In Sema, functions are called directly: (f arg ...)
```

### Stack Overflow

Infinite recursion gets a helpful hint:

```
Error: Eval error: maximum eval depth exceeded (1024)
  hint: this usually means infinite recursion; ensure recursive calls are in
        tail position for TCO, or use 'do' for iteration
```

### NO_COLOR Support

Set `NO_COLOR=1` to disable colored output, or pipe stderr to a file — Sema auto-detects non-TTY output and strips colors.
