# Sema for Visual Studio Code

Language support for [Sema](https://sema-lang.com), a Lisp dialect with first-class LLM primitives.

- **Homepage**: [sema-lang.com](https://sema-lang.com)
- **Source**: [github.com/helgesverre/sema](https://github.com/helgesverre/sema)
- **Playground**: [sema.run](https://sema.run)

## Features

- Syntax highlighting for `.sema` files
- Bracket matching and auto-closing
- Comment toggling (`Ctrl+/` / `Cmd+/`)
- Highlighting for special forms, builtins, LLM primitives, keywords, strings, numbers, and more

## Installation

### From VSIX

1. Build the VSIX package:
   ```bash
   cd editors/vscode/sema
   npx @vscode/vsce package
   ```
2. Install in VS Code:
   ```
   code --install-extension sema-0.1.0.vsix
   ```

### Manual Install

Copy the extension folder to your VS Code extensions directory:

```bash
# macOS / Linux
cp -r editors/vscode/sema ~/.vscode/extensions/helgesverre.sema-0.1.0

# Windows
xcopy editors\vscode\sema %USERPROFILE%\.vscode\extensions\helgesverre.sema-0.1.0\ /E /I
```

Restart VS Code after copying.

### Development

1. Open the `editors/vscode/sema` folder in VS Code
2. Press `F5` to launch an Extension Development Host
3. Open any `.sema` file to see syntax highlighting
