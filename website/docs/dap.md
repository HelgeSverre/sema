---
outline: [ 2, 3 ]
---

# Debugger (DAP)

Sema Lisp includes a built-in [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) (DAP) server that allows step-by-step debugging of Sema programs. By running over standard I/O (stdin/stdout) using the standard JSON-RPC DAP protocol, it integrates directly with modern editor debuggers.

```bash
sema dap
```

::: info Runs on the bytecode VM
The debugger operates on the stack-based bytecode VM — Sema's sole evaluator. Programs debugged via DAP are compiled to bytecode automatically upon launch.
:::

## Features

The `sema dap` server implements the core features of the Debug Adapter Protocol:

### Launch Configuration
- **Program Target**: Specify the absolute path to the `.sema` file to debug.
- **Stop on Entry**: Set `stopOnEntry: true` to pause execution on the first bytecode instruction before any user forms run, allowing you to set up breakpoints or inspect the entry environment.

### Breakpoints
- **Dynamic Breakpoints**: Set, toggle, or clear line breakpoints before launching or in real-time while execution is paused.
- **Breakpoint Verification**: The compiler maps source file line numbers to bytecode instructions and returns verified locations back to the editor.
- **Conditional Breakpoints**: Attach a `condition` expression to a breakpoint; execution only stops there when the condition evaluates truthy in the paused frame (using the same evaluator as `evaluate`/hover). A condition that errors fails open — it stops — so the problem surfaces rather than being silently swallowed.
- **Exception Breakpoints**: Enable the `uncaught` filter to stop on a runtime error that escapes to the top level (errors handled by `try`/`catch` do not trigger it). The `exceptionInfo` request reports the error message. Note: at an uncaught-exception stop the VM has already unwound its frames, so the stack/variables there are best-effort — the message is the load-bearing detail.

### Stepping Controls
- **Step Over** (`next`): Execute the current form and pause at the next sibling expression in the same frame.
- **Step Into** (`stepIn`): Follow execution inside user-defined functions or lambda expressions.
- **Step Out** (`stepOut`): Execute all remaining instructions in the active function frame and pause immediately upon returning to the caller.
- **Continue**: Resume program execution until the next breakpoint is hit or the program finishes.
- **Pause**: Pause execution of a running VM thread.

### Call Stack Inspection
- Renders the call hierarchy stack trace with frame IDs, function/closure names, active line numbers, columns, and absolute source file paths.

### Variable & Scope Inspection
- **Locals, Closure & Globals**: Separates variables into local bindings (inside `let`, function parameters), captured **upvalues** (shown by name under a *Closure* scope), and global variables. Locals are scoped to the current instruction, so bindings that are not yet in scope or whose block has already exited are not shown.
- **Type Inspection**: Displays the value alongside its data type (e.g., `list`, `vector`, `map`, `string`, `number`, `boolean`, `closure`).
- **Complex Objects**: Fully supports lazily expandable nested objects — lists, vectors, maps, hash maps, byte vectors, and named **record fields** — using hierarchical variable references.

### Evaluate & Set Values (while paused)
- **Evaluate Expressions**: Evaluate arbitrary Sema expressions in the context of the selected stack frame (editor watch expressions, hover, and the debug console REPL). A top-level `(set! …)` of an in-scope local, upvalue, or global is written back to the running program.
- **Set Variable**: Edit a local or upvalue value in place from the editor's Variables pane; the new value is written through to the live frame.

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
