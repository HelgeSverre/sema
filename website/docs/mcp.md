---
outline: [2, 3]
---

# Model Context Protocol (MCP)

Sema includes a built-in [Model Context Protocol](https://modelcontextprotocol.io/) server that exposes [`deftool`](/docs/llm/tools-agents.html) definitions as MCP tools. Any Sema script with tool definitions can be instantly deployed as an MCP server compatible with Claude Desktop, Cursor, and other MCP clients. The server communicates over stdio using the MCP JSON-RPC protocol.

```bash
sema mcp tools.sema
```

## Defining Tools

MCP tools are defined using Sema's `deftool` special form. Each tool has a name, description, parameter schema, and handler function:

```sema
(deftool greet
  "Greet someone by name"
  {:name {:type :string :description "The person's name"}}
  (lambda (name)
    (string-append "Hello, " name "!")))

(deftool calculate
  "Evaluate a simple arithmetic expression (a op b)"
  {:expression {:type :string :description "A math expression like '2 + 3'"}}
  (lambda (expr)
    (let ((parts (string/split expr " ")))
      (if (= (length parts) 3)
        (let ((a  (string/to-number (nth parts 0)))
              (op (nth parts 1))
              (b  (string/to-number (nth parts 2))))
          (cond
            ((= op "+") (number/to-string (+ a b)))
            ((= op "-") (number/to-string (- a b)))
            ((= op "*") (number/to-string (* a b)))
            ((= op "/") (number/to-string (/ a b)))
            (else       (string-append "Unknown operator: " op))))
        (string-append "Expected 'a op b', got: " expr)))))
```

The parameter schema maps directly to [JSON Schema](https://json-schema.org/) for MCP clients. Each parameter supports `:type`, `:description`, `:enum`, and `:optional` keys. Parameters are required by default unless `:optional #t` is set.

See [Tools & Agents](/docs/llm/tools-agents.html) for full `deftool` documentation.

## Running the Server

### From source files

```bash
sema mcp tools.sema
```

Multiple files can be loaded — all `deftool` definitions across all files are discovered:

```bash
sema mcp math-tools.sema string-tools.sema file-tools.sema
```

### From compiled bytecode

Pre-compiled `.semac` files work too, for faster startup:

```bash
sema compile tools.sema        # produces tools.semac
sema mcp tools.semac
```

### From standalone binaries

Standalone executables built with [`sema build`](/docs/cli.html#sema-build) support the `--mcp` flag. The binary runs its embedded script to define tools, then starts the MCP server:

```bash
sema build tools.sema -o my-tools
./my-tools --mcp
```

This makes it possible to distribute a single binary that works both as a CLI tool and an MCP server.

## Client Setup

### Claude Desktop

Add to your Claude Desktop configuration (`~/Library/Application Support/Claude/claude_desktop_config.json` on macOS):

```json
{
  "mcpServers": {
    "my-tools": {
      "command": "sema",
      "args": ["mcp", "path/to/tools.sema"]
    }
  }
}
```

Or with a standalone binary:

```json
{
  "mcpServers": {
    "my-tools": {
      "command": "./my-tools",
      "args": ["--mcp"]
    }
  }
}
```

### Cursor

Add to your Cursor MCP settings (`.cursor/mcp.json`):

```json
{
  "mcpServers": {
    "my-tools": {
      "command": "sema",
      "args": ["mcp", "path/to/tools.sema"]
    }
  }
}
```

### Claude Code

Add to your Claude Code MCP settings (`.claude/settings.json`):

```json
{
  "mcpServers": {
    "my-tools": {
      "command": "sema",
      "args": ["mcp", "path/to/tools.sema"]
    }
  }
}
```

## Options

| Flag                 | Description                                                     |
| -------------------- | --------------------------------------------------------------- |
| `--no-llm`           | Disable LLM auto-configuration (faster startup if tools don't use LLM) |
| `--sandbox <MODE>`   | Sandbox mode (`strict`, `all`, or comma-separated capabilities) |

```bash
# Fast startup for tools that don't call LLMs
sema mcp --no-llm tools.sema

# Restrict tool capabilities
sema mcp --sandbox=no-shell,no-network tools.sema
```

## How It Works

1. Sema evaluates the specified file(s), executing all top-level expressions
2. The environment is scanned for all `ToolDef` values (created by `deftool`)
3. Parameter schemas are converted to JSON Schema for the MCP protocol
4. The server starts on stdio, handling `initialize`, `tools/list`, and `tools/call` requests
5. When a tool is called, the JSON arguments are converted to Sema values and passed to the handler function
6. The handler's return value is converted back to a text response for the MCP client

Tool handlers run in the same interpreter context, so they can share state, access the filesystem, make HTTP requests, or call LLM APIs — whatever the tool needs.
