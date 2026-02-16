---
outline: [2, 3]
---

# Shell Completions

Sema can generate tab-completion scripts for your shell, giving you completions for all CLI flags, options, and subcommands.

```
sema completions <SHELL>
```

Supported shells: `bash`, `zsh`, `fish`, `elvish`, `powershell`.

## Zsh

### macOS (with oh-my-zsh or custom fpath)

```bash
mkdir -p ~/.zsh/completions
sema completions zsh > ~/.zsh/completions/_sema
```

Make sure your `~/.zshrc` includes the directory in `fpath` **before** `compinit` is called:

```bash
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit && compinit
```

If you use oh-my-zsh, add the `fpath` line before `source $ZSH/oh-my-zsh.sh` (oh-my-zsh calls `compinit` for you).

### Linux

```bash
# User-local (no sudo required)
mkdir -p ~/.zsh/completions
sema completions zsh > ~/.zsh/completions/_sema
```

Add to `~/.zshrc` (before `compinit`):

```bash
fpath=(~/.zsh/completions $fpath)
autoload -Uz compinit && compinit
```

Or install system-wide:

```bash
sudo sema completions zsh > /usr/local/share/zsh/site-functions/_sema
```

Then restart your shell or run `exec zsh`.

## Bash

### macOS

macOS ships with an older Bash. Install `bash-completion` via Homebrew:

```bash
brew install bash-completion@2
```

Then generate and install the completion script:

```bash
mkdir -p ~/.local/share/bash-completion/completions
sema completions bash > ~/.local/share/bash-completion/completions/sema
```

### Linux

```bash
# User-local
mkdir -p ~/.local/share/bash-completion/completions
sema completions bash > ~/.local/share/bash-completion/completions/sema

# Or system-wide
sudo sema completions bash > /etc/bash_completion.d/sema
```

Then restart your shell or run `source ~/.bashrc`.

## Fish

```bash
sema completions fish > ~/.config/fish/completions/sema.fish
```

This works on both macOS and Linux. Completions are picked up automatically on the next shell session.

## PowerShell

```powershell
# Create the completions directory if it doesn't exist
New-Item -ItemType Directory -Force -Path (Split-Path -Parent $PROFILE)

# Append the completion script to your profile
sema completions powershell >> $PROFILE
```

Restart PowerShell to activate.

## Elvish

```bash
sema completions elvish > ~/.config/elvish/lib/sema.elv
```

Then add `use sema` to `~/.config/elvish/rc.elv`.

## Verifying

After installing, restart your shell and type `sema ` then press <kbd>Tab</kbd>. You should see completions for flags and subcommands.
