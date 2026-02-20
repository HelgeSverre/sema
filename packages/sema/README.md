# @sema-lang/sema

Sema Lisp interpreter for JavaScript — a client-side scripting engine powered by WebAssembly.

## Install

```bash
npm install @sema-lang/sema
```

Or use directly from a CDN:

```html
<script type="module">
  import { SemaInterpreter } from "https://cdn.jsdelivr.net/npm/@sema-lang/sema/+esm";

  const sema = await SemaInterpreter.create();
  console.log(sema.evalStr("(+ 1 2 3)").value); // "6"
</script>
```

## Quick Start

```js
import { SemaInterpreter } from "@sema-lang/sema";

const sema = await SemaInterpreter.create();

// Evaluate expressions
const r = sema.evalStr("(+ 1 2 3)");
console.log(r.value);  // "6"

// Definitions persist
sema.evalStr("(define (square x) (* x x))");
sema.evalStr("(square 7)"); // => "49"

// Register JS functions — args are native JS values
sema.registerFunction("greet", (name) => `Hello, ${name}!`);
sema.evalStr('(greet "world")'); // => "Hello, world!"

// Preload modules
sema.preloadModule("utils", "(define (double x) (* x 2))");
sema.evalStr('(import "utils")');
sema.evalStr("(double 21)"); // => "42"
```

## API

### `SemaInterpreter.create(opts?)`

Create a new interpreter. Options:

| Option | Default | Description |
|--------|---------|-------------|
| `wasmUrl` | auto | URL to the `.wasm` binary |
| `stdlib` | `true` | Include the standard library |
| `deny` | `[]` | Capabilities to deny: `"network"`, `"fs-read"`, `"fs-write"` |
| `vfs` | none | VFS backend for persistence: `MemoryBackend`, `LocalStorageBackend`, `SessionStorageBackend`, `IndexedDBBackend` |

### `evalStr(code)` → `EvalResult`

Evaluate Sema code synchronously. Returns `{ value, output, error }`.

### `evalStrAsync(code)` → `Promise<EvalResult>`

Evaluate code that may use `http/get` or other async operations.

### `registerFunction(name, fn)`

Register a JS function callable from Sema. Args are passed as native JS values.

### `preloadModule(name, source)`

Inject a virtual module for use with `(import "name")`.

### `version()` → `string`

Returns the interpreter version.

### `readFile(path)` → `string | null`

Read a file from the virtual filesystem. Returns `null` if the file doesn't exist.

### `writeFile(path, content)`

Write a file to the virtual filesystem (1 MB per file, 16 MB total, 256 files max).

### `deleteFile(path)` → `boolean`

Delete a file from the VFS. Returns `true` if the file existed.

### `listFiles(dir?)` → `string[]`

List entries in a VFS directory.

### `fileExists(path)` → `boolean`

Check if a path exists in the VFS.

### `mkdir(path)`

Create a directory (and parent directories) in the VFS.

### `isDirectory(path)` → `boolean`

Check if a path is a directory in the VFS.

### `vfsStats()` → `VFSStats`

Get VFS usage statistics: `{ files, bytes, maxFiles, maxBytes, maxFileBytes }`.

### `resetVFS()`

Clear all files and directories from the VFS.

### `flushVFS()` → `Promise<void>`

Persist VFS changes to the configured backend. No-op if no backend was provided.

### `resetVFSAndBackend()` → `Promise<void>`

Clear the VFS and the persistent backend storage.

### `dispose()`

Free WASM memory. Interpreter cannot be used after this.

## Virtual Filesystem

```js
// Seed files from JS
sema.writeFile("/lib/utils.sema", "(define (double x) (* x 2))");

// Build a file browser
const files = sema.listFiles("/");       // ["lib"]
const libFiles = sema.listFiles("/lib"); // ["utils.sema"]

// Read back
const source = sema.readFile("/lib/utils.sema");

// Check quota usage
const stats = sema.vfsStats();
// { files: 1, bytes: 28, maxFiles: 256, maxBytes: 16777216, maxFileBytes: 1048576 }

// Clean up
sema.resetVFS();
```

## VFS Persistence

By default, VFS files are lost on page reload. Use a backend to persist them:

```js
import { SemaInterpreter, IndexedDBBackend } from "@sema-lang/sema";

const sema = await SemaInterpreter.create({
  vfs: new IndexedDBBackend({ namespace: "my-project" }),
});

// Files written by Sema code are persisted after flush
await sema.evalStrAsync('(file/write "/hello.txt" "Hello!")');
await sema.flushVFS();

// On next page load, files are automatically restored
```

### Built-in Backends

| Backend | Persistence | Size Limit | Best For |
|---------|-------------|------------|----------|
| `MemoryBackend` | None (lost on reload) | WASM quota only | Testing, ephemeral sandboxes |
| `LocalStorageBackend` | Across page loads | ~5–10 MB per origin | Small projects |
| `SessionStorageBackend` | Within tab session | ~5–10 MB per origin | Scratch work, drafts |
| `IndexedDBBackend` | Across page loads | Hundreds of MB | **Production use** |

All backends accept a `{ namespace }` option (default: `"sema-vfs"`) to isolate storage between different apps or interpreter instances.

### Custom Backends

Implement the `VFSBackend` interface to use any storage mechanism:

```ts
import type { VFSBackend, VFSHost } from "@sema-lang/sema";

class MyBackend implements VFSBackend {
  async init() { /* open connections */ }
  async hydrate(host: VFSHost) { /* restore files into host */ }
  async flush(host: VFSHost) { /* save files from host */ }
  async reset() { /* clear storage */ }
}
```

## Sandbox

```js
const sema = await SemaInterpreter.create({
  deny: ["network"],  // deny HTTP access
});

sema.evalStr('(http/get "https://example.com")'); // => PermissionDenied error
sema.evalStr("(+ 1 2)");                          // => works fine
```

## Documentation

Full documentation: [sema-lang.com/docs/embedding-js](https://sema-lang.com/docs/embedding-js.html)

## License

MIT
