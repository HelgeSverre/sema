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
| `--no-init`          | Skip LLM auto-configuration                 |
| `--no-llm`           | Disable LLM features (same as `--no-init`)   |
| `--model <NAME>`     | Set default LLM model                        |
| `--provider <NAME>`  | Set LLM provider                             |
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

# Use a specific model
sema --model claude-haiku-4-5-20251001 -e '(llm/complete "Hello!")'

# Shebang support in scripts
#!/usr/bin/env sema
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
| `SEMA_DEFAULT_MODEL` | Default model name                                    |
| `SEMA_LLM_PROVIDER`  | Preferred provider                                    |

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
