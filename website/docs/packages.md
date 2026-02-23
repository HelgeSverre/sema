---
outline: [2, 3]
---

# Package Manager

Sema uses Go-style URL-based packages. Packages are git repositories — there is no central registry. Any public git repo with a `mod.sema` (or a `sema.toml` with a custom entrypoint) is a valid package.

## Package Format

A package is a directory containing at minimum one of:

- **`mod.sema`** — the default entrypoint (what gets loaded on import)
- **`sema.toml`** — optional package metadata, dependencies, and custom entrypoint

### `sema.toml`

```toml
[package]
name = "my-package"
version = "0.1.0"
description = "A useful Sema library"
entrypoint = "lib.sema"

[deps]
"github.com/user/other-lib" = "v1.0.0"
"github.com/user/utils" = "main"
```

The `[package]` section defines metadata:

| Field         | Description                                       |
| ------------- | ------------------------------------------------- |
| `name`        | Package name                                      |
| `version`     | Semver version string                             |
| `description` | Short description of the package                  |
| `entrypoint`  | File loaded on import (default: `mod.sema`)       |

The `[deps]` section maps package URLs to git refs (tags, branches, or commit SHAs).

### Entrypoint Resolution

When you import a package, Sema resolves the entrypoint in this order:

1. **Direct file** — `~/.sema/packages/<spec>.sema` (for sub-module imports like `github.com/user/repo/utils`)
2. **Custom entrypoint** — if `sema.toml` exists and has an `entrypoint = "..."` field, that file is loaded
3. **Default entrypoint** — `mod.sema` in the package directory

## CLI Commands

### `sema pkg init`

Initialize a new project in the current directory. Creates a `sema.toml` with the directory name as the package name.

```bash
mkdir my-package && cd my-package
sema pkg init
```

::: tip
`sema pkg init` only creates `sema.toml`. You'll need to create `mod.sema` yourself to define your package's public API.
:::

### `sema pkg add`

Add a package to the global cache and your project's `sema.toml`.

```bash
sema pkg add github.com/user/repo          # latest default branch (main)
sema pkg add github.com/user/repo@v1.2.0   # specific tag
sema pkg add github.com/user/repo@main     # specific branch
sema pkg add github.com/user/repo@abc123   # specific commit SHA
```

Package URLs must be valid git-hostable paths (e.g., `github.com/user/repo`). Full URLs with schemes (`https://...`), Windows paths (`C:\...`), and SCP-style paths (`git@github.com:...`) are not accepted — use the bare host/path format.

If a `sema.toml` exists in the current directory, the package is automatically added to the `[deps]` section (or updated if already present).

### `sema pkg install`

Fetch all dependencies listed in `sema.toml`.

```bash
sema pkg install
```

Reads the `[deps]` section and fetches each dependency. Requires a `sema.toml` in the current directory.

### `sema pkg update`

Pull the latest changes for installed packages.

```bash
sema pkg update                       # update all installed packages
sema pkg update github.com/user/repo  # update a specific package
sema pkg update repo                  # update by short name
```

### `sema pkg remove`

Remove an installed package from the global cache and your project's `sema.toml`.

```bash
sema pkg remove github.com/user/repo  # by full path
sema pkg remove repo                  # by short name
```

If a `sema.toml` exists in the current directory and contains a matching entry in `[deps]`, it will be removed automatically.

### `sema pkg list`

List all installed packages and their current git refs.

```bash
sema pkg list
```

```
  github.com/user/repo (v1.2.0)
  github.com/user/utils (main)
```

## Importing Packages

Import a package by its URL path:

```sema
(import "github.com/user/string-utils")

(string-utils/slugify "Hello World")
; => "hello-world"
```

The package name (last segment of the URL) becomes the namespace prefix. You can also use selective imports:

```sema
(import "github.com/user/string-utils" (slugify titlecase))

(slugify "Hello World")
; => "hello-world"
```

### Sub-module Imports

You can import sub-modules from a package by appending a path:

```sema
;; Resolves to ~/.sema/packages/github.com/user/repo/utils.sema
(import "github.com/user/repo/utils")
```

### How Sema Distinguishes Package vs File Imports

An import string is treated as a **package import** when it:
- Contains `/` (path separator)
- Does **not** start with `./` or `../` (relative path)
- Does **not** end with `.sema` (explicit file)
- Is **not** an absolute path

Otherwise, it's resolved as a relative file import from the current file's directory.

```sema
;; Package imports
(import "github.com/user/repo")        ; → ~/.sema/packages/github.com/user/repo/mod.sema
(import "github.com/user/repo/utils")  ; → ~/.sema/packages/github.com/user/repo/utils.sema

;; File imports (relative to current file)
(import "./helpers.sema")              ; relative file
(import "../lib/utils.sema")           ; parent directory
```

## On-Disk Layout

Packages are cached globally at `~/.sema/packages/`, mirroring the URL structure:

```
~/.sema/packages/
  github.com/
    user/
      repo/
        sema.toml
        mod.sema
        src/
          ...
      other-lib/
        mod.sema
```

The `~/.sema/` directory is also used for REPL history (`history.txt`).

## Creating a Package

### 1. Initialize

```bash
mkdir sema-csv-utils && cd sema-csv-utils
sema pkg init
```

### 2. Write Your Code

Create `mod.sema` to define your package's public API:

```sema
;; mod.sema — package entrypoint
(defun parse-row (line)
  (map string/trim (string/split line ",")))

(defun parse-csv (text)
  (map parse-row (string/split text "\n")))
```

### 3. Add Dependencies (Optional)

```bash
sema pkg add github.com/user/csv-options@v1.0.0
```

This fetches the package and adds it to your `sema.toml` automatically. Then use it in your code:

```sema
(import "github.com/user/csv-options")

(defun parse-row (line)
  (map string/trim (string/split line (csv-options/delimiter))))
```

### 4. Publish

Push to a public git repository. Tag releases with semver:

```bash
git tag v0.1.0
git push origin main --tags
```

Others can now install your package:

```bash
sema pkg add github.com/yourname/sema-csv-utils@v0.1.0
```

## Example Workflow

```bash
# Start a new project
mkdir my-project && cd my-project
sema pkg init

# Add dependencies
sema pkg add github.com/user/http-helpers@v2.0.0
sema pkg add github.com/user/json-schema@v1.1.0

# Install everything (if cloning the project fresh)
sema pkg install

# List what's installed
sema pkg list
```

```sema
;; main.sema
(import "github.com/user/http-helpers")
(import "github.com/user/json-schema")

(def response (http-helpers/fetch "https://api.example.com/data"))
(def valid? (json-schema/validate schema (json/decode (:body response))))
(println (if valid? "Valid!" "Invalid."))
```

```bash
sema main.sema
```

## Troubleshooting

### "package not found"

```
Error: package not found: github.com/user/repo
Hint: Run: sema pkg add github.com/user/repo
```

The package hasn't been fetched yet. Run the suggested command to install it.

### "invalid package spec: URL schemes not allowed"

```
Error: invalid package spec: URL schemes not allowed: https://github.com/user/repo
```

Use the bare host/path format without `https://`:

```bash
# ✗ Wrong
sema pkg add https://github.com/user/repo

# ✓ Correct
sema pkg add github.com/user/repo
```

### "invalid package spec: path traversal not allowed"

The package path contains `..`, `.`, or empty segments. Package paths must be clean, forward-slash-separated identifiers like `github.com/user/repo`.

### "No sema.toml found"

`sema pkg install` requires a `sema.toml` in the current directory. Run `sema pkg init` to create one, or `cd` to the project root.

### "git clone/fetch failed"

The package URL couldn't be reached. Check that:
- The repository exists and is public (or you have git credentials configured)
- The git ref (tag/branch) exists on the remote
- You have network access
