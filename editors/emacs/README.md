# sema-mode.el — Emacs Major Mode for Sema

An Emacs major mode for editing [Sema](https://sema-lang.com) (`.sema`) files.

- **Homepage**: [sema-lang.com](https://sema-lang.com)
- **Source**: [github.com/helgesverre/sema](https://github.com/helgesverre/sema)
- **Author**: Helge Sverre

## Features

- **Syntax highlighting** — special forms, builtins, keyword literals (`:foo`), booleans (`#t`/`#f`), numbers, strings, comments
- **Smart indentation** — Lisp-aware indentation with Sema-specific form rules
- **REPL integration** — start a Sema REPL and send code to it interactively
- **Electric pairs** — auto-close `()`, `[]`, `{}`, `""`

## Installation

### Manual

```elisp
(add-to-list 'load-path "/path/to/sema/editors/emacs")
(require 'sema-mode)
```

### use-package

```elisp
(use-package sema-mode
  :load-path "/path/to/sema/editors/emacs"
  :mode "\\.sema\\'")
```

### Doom Emacs

In `packages.el`:

```elisp
(package! sema-mode :recipe (:local-repo "/path/to/sema/editors/emacs"))
```

In `config.el`:

```elisp
(use-package! sema-mode :mode "\\.sema\\'")
```

## Key Bindings

| Key       | Command               | Description                      |
| --------- | --------------------- | -------------------------------- |
| `C-c C-z` | `sema-repl`           | Start or switch to the Sema REPL |
| `C-c C-e` | `sema-send-last-sexp` | Send sexp before point to REPL   |
| `C-c C-r` | `sema-send-region`    | Send selected region to REPL     |
| `C-c C-b` | `sema-send-buffer`    | Send entire buffer to REPL       |
| `C-c C-l` | `sema-run-file`       | Run current file with `sema`     |

## Configuration

```elisp
;; Path to the sema binary (default: "sema")
(setq sema-program "/path/to/sema")
```
