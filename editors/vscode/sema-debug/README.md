# Sema Debug Adapter

Debug Sema programs using the Debug Adapter Protocol (DAP).

## Setup

The `sema dap` command starts a DAP server on stdio. Any DAP-compatible editor can use it.

### VS Code

To debug Sema programs in VS Code, you need a generic DAP extension that can launch arbitrary debug adapters. Add the following to your `.vscode/launch.json`:

```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "type": "sema",
            "request": "launch",
            "name": "Debug Sema Program",
            "program": "${file}",
            "stopOnEntry": false
        }
    ]
}
```

### Supported Features

- **Breakpoints**: Set breakpoints by line number
- **Stepping**: Step into, step over, step out
- **Stack traces**: View the call stack
- **Variable inspection**: View locals and captured variables (upvalues)
- **Pause/Continue**: Pause execution and continue

### Configuration

| Property | Type | Description |
|----------|------|-------------|
| `program` | string | Path to the `.sema` file to debug |
| `stopOnEntry` | boolean | If `true`, break on the first line (default: `false`) |
