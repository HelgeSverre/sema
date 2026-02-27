# Sema for Vim / Neovim

Syntax highlighting, filetype detection, and editor settings for [Sema](https://sema-lang.com) (`.sema` files).

- **Homepage**: [sema-lang.com](https://sema-lang.com)
- **Source**: [github.com/helgesverre/sema](https://github.com/helgesverre/sema)
- **Author**: Helge Sverre

## Features

- Full syntax highlighting (special forms, builtins, LLM primitives, keywords, strings, numbers, comments)
- Automatic filetype detection for `.sema` files
- Lisp-aware indentation with correct `lispwords`
- Comment string configured for `;`

## Installation

### vim-plug

```vim
Plug 'helgesverre/sema', { 'rtp': 'editors/vim' }
```

### lazy.nvim

```lua
{
  "helgesverre/sema",
  config = function(plugin)
    vim.opt.rtp:append(plugin.dir .. "/editors/vim")
  end,
}
```

### Pathogen

```bash
cd ~/.vim/bundle
git clone https://github.com/helgesverre/sema.git
# Then add to your vimrc:
# set runtimepath+=~/.vim/bundle/sema/editors/vim
```

Or create a symlink so Pathogen picks it up directly:

```bash
ln -s /path/to/sema/editors/vim ~/.vim/bundle/sema-vim
```

### Manual (Vim)

Copy the directories into your Vim config:

```bash
mkdir -p ~/.vim/syntax ~/.vim/ftdetect ~/.vim/ftplugin
cp editors/vim/syntax/sema.vim   ~/.vim/syntax/
cp editors/vim/ftdetect/sema.vim ~/.vim/ftdetect/
cp editors/vim/ftplugin/sema.vim ~/.vim/ftplugin/
```

### Manual (Neovim)

```bash
mkdir -p ~/.config/nvim/syntax ~/.config/nvim/ftdetect ~/.config/nvim/ftplugin
cp editors/vim/syntax/sema.vim   ~/.config/nvim/syntax/
cp editors/vim/ftdetect/sema.vim ~/.config/nvim/ftdetect/
cp editors/vim/ftplugin/sema.vim ~/.config/nvim/ftplugin/
```

### Nix / Home Manager

Add the plugin directory to your `runtimepath`:

```nix
programs.neovim.extraConfig = ''
  set runtimepath+=/path/to/sema/editors/vim
'';
```

## Highlighting Groups

| Group              | What it covers                                        |
| ------------------ | ----------------------------------------------------- |
| `semaSpecial`      | Special forms (`define`, `lambda`, `if`, `defagent`…) |
| `semaBuiltin`      | Builtin functions (`map`, `llm/chat`, `file/read`…)   |
| `semaThreading`    | Threading macros (`->`, `->>`, `as->`)                |
| `semaKeyword`      | Keyword literals (`:model`, `:temperature`…)          |
| `semaString`       | String literals                                       |
| `semaNumber`       | Integer and float literals                            |
| `semaBoolean`      | `#t`, `#f`, `true`, `false`                           |
| `semaConstant`     | `nil`                                                 |
| `semaCharacter`    | Character literals (`#\a`, `#\space`…)                |
| `semaComment`      | Line comments (`;`)                                   |
| `semaTodo`         | TODO/FIXME/XXX/HACK/NOTE in comments                  |
| `semaStringEscape` | Escape sequences in strings                           |
| `semaQuote`        | Quote (`'`) and quasiquote (`` ` ``) prefixes         |
| `semaUnquote`      | Unquote (`,`) and unquote-splicing (`,@`)             |
| `semaParens`       | Parentheses, brackets, and braces                     |

Override any group in your colorscheme to customise appearance.

## LSP Support

Sema ships with a built-in language server. Run it with `sema lsp`.

**Available features:** completions, hover docs, go-to-definition, find references, rename, signature help, diagnostics, document symbols, and code lens (run expressions).

### nvim-lspconfig (Neovim built-in LSP)

Add to your Neovim config:

```lua
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'sema',
  callback = function()
    vim.lsp.start({
      name = 'sema',
      cmd = { 'sema', 'lsp' },
      root_dir = vim.fs.dirname(vim.fs.find({ 'sema.toml', '.git' }, { upward = true })[1]),
    })
  end,
})
```

### coc.nvim

Add to `:CocConfig`:

```json
{
  "languageserver": {
    "sema": {
      "command": "sema",
      "args": ["lsp"],
      "filetypes": ["sema"]
    }
  }
}
```

### Inline Results (Advanced)

The Sema LSP server emits a custom `sema/evalResult` notification for displaying inline evaluation results. This requires a custom handler in your client — see the Sema documentation for details.
