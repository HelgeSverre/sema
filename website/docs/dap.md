---
outline: [ 2, 3 ]
---

# Debugger (DAP)

Sema Lisp includes a built-in [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) (DAP) server that allows step-by-step debugging of Sema programs. By running over standard I/O (stdin/stdout) using the standard JSON-RPC DAP protocol, it integrates directly with modern editor debuggers.

```bash
sema dap
```

::: info VM-Only Feature
The debugger operates exclusively on the stack-based bytecode VM backend. The tree-walking interpreter does not support breakpoints or stepping. Programs debugged via DAP are compiled to bytecode automatically upon launch.
:::

## Features

The `sema dap` server implements the core features of the Debug Adapter Protocol:

### Launch Configuration
- **Program Target**: Specify the absolute path to the `.sema` file to debug.
- **Stop on Entry**: Set `stopOnEntry: true` to pause execution on the first bytecode instruction before any user forms run, allowing you to set up breakpoints or inspect the entry environment.

### Breakpoints
- **Dynamic Breakpoints**: Set, toggle, or clear line breakpoints before launching or in real-time while execution is paused.
- **Breakpoint Verification**: The compiler maps source file line numbers to bytecode instructions and returns verified locations back to the editor.

### Stepping Controls
- **Step Over** (`next`): Execute the current form and pause at the next sibling expression in the same frame.
- **Step Into** (`stepIn`): Follow execution inside user-defined functions or lambda expressions.
- **Step Out** (`stepOut`): Execute all remaining instructions in the active function frame and pause immediately upon returning to the caller.
- **Continue**: Resume program execution until the next breakpoint is hit or the program finishes.
- **Pause**: Pause execution of a running VM thread.

### Call Stack Inspection
- Renders the call hierarchy stack trace with frame IDs, function/closure names, active line numbers, columns, and absolute source file paths.

### Variable & Scope Inspection
- **Locals & Globals**: Separates variables into local bindings (inside `let`, function parameters) and global variables.
- **Type Inspection**: Displays the value alongside its data type (e.g., `list`, `vector`, `map`, `string`, `number`, `boolean`, `closure`).
- **Complex Objects**: Fully supports expandable nested objects (e.g., hash maps and lists) using hierarchical variable references.

### Console Output Redirection
- Program output (`stdout`) and standard error (`stderr`) prints are intercepted and redirected into standard DAP `output` event frames. This prevents user outputs from corrupting the JSON-RPC protocol transport channel while ensuring they appear in the editor's debug console.

---

## Editor Setup

### Helix

Helix supports debuggers out-of-the-box via the `lldb-dap` architecture. Merge the following configuration into your `~/.config/helix/languages.toml`:

```toml
[[language]]
name = "sema"
language-servers = ["sema-lsp"]
debugger = { name = "sema-dap", transport = "stdio" }

[debugger.sema-dap]
command = "sema"
args = ["dap"]
transport = "stdio"
templates = [
  { name = "launch", completion = [ { name = "program", completion = "filename" } ] }
]
```

To debug in Helix:
1. Open a `.sema` file.
2. Press <kbd>Space</kbd> + <kbd>g</kbd> to open the debug menu.
3. Select `launch` and press <kbd>Enter</kbd> to start.

### VS Code

Configure a debug launch task in your project's `.vscode/launch.json`:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "type": "sema",
      "request": "launch",
      "name": "Debug Sema Script",
      "program": "${file}",
      "stopOnEntry": true
    }
  ]
}
```

### Neovim

Using [nvim-dap](https://github.com/mfussenegger/nvim-dap), register the `sema` adapter and configuration:

```lua
local dap = require('dap')

dap.adapters.sema = {
  type = 'executable',
  command = 'sema',
  args = { 'dap' }
}

dap.configurations.sema = {
  {
    type = 'sema',
    request = 'launch',
    name = "Launch file",
    program = "${file}",
    stopOnEntry = true,
  },
}
```

### Emacs

Using [dap-mode](https://github.com/emacs-lsp/dap-mode), register the debugging template:

```elisp
(dap-register-adapter
  "sema-dap"
  (lambda (conf)
    (list :type "executable"
          :command "sema"
          :args '("dap"))))

(dap-register-debug-template
  "Sema Launch"
  (list :type "sema"
        :request "launch"
        :name "Sema Debug"
        :program "${file}"
        :stopOnEntry t))
```

---

## Architecture

The `sema-dap` server uses an async-synchronous bridge structure to ensure standard I/O responsiveness while handling the single-threaded nature of Lisp evaluation:

- **Frontend Async Loop**: Runs a Tokio event loop handling stdin/stdout messages. Incoming requests are parsed into DAP protocol actions, and outgoing events (like VM output prints or stop events) are serialized to stdout.
- **Backend Executor Thread**: Runs VM bytecode execution inside a dedicated OS thread.
- **VM Debug Hook**: Utilizes the `sema-vm::VM` debug hooks (`execute_debug`). Every instruction step checks if a breakpoint is hit, stepped, or paused, updating `DebugState` and sending notifications back to the async frontend.
