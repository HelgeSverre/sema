---
outline: [2, 3]
---

# Package Manager

::: warning Registry Status
The central package registry (`pkg.sema-lang.com`) is not yet live. **Git-based packages work today** — you can install any package directly from a git repository. Registry commands (`search`, `info`, `publish`, `yank`, `login`) require a registry instance; see [Self-Hosted Registry](#self-hosted-registry) to run your own.
:::

Sema supports two package sources: a **package registry** (for published packages with semver versions) and **direct git repos** (for development branches, private code, or unregistered packages). Both can be mixed freely in the same project.

## Package Format

A package is a directory containing at minimum one of:

- **`package.sema`** — the default entrypoint (what gets loaded on import)
- **`sema.toml`** — optional package metadata, dependencies, and custom entrypoint

### `sema.toml`

```toml
[package]
name = "my-package"
version = "0.1.0"
description = "A useful Sema library"
entrypoint = "lib.sema"

[deps]
# Registry packages — short name = version
http-helpers = "1.0.0"
json-schema = "2.1.0"

# Git packages — quoted URL = git ref
"github.com/user/private-lib" = "main"
```

The `[package]` section defines metadata:

| Field         | Description                                       |
| ------------- | ------------------------------------------------- |
| `name`        | Package name                                      |
| `version`     | Semver version string (required for publishing)   |
| `description` | Short description of the package                  |
| `entrypoint`  | File loaded on import (default: `package.sema`)   |

The `[deps]` section maps package identifiers to versions or git refs:

- **Keys without `/`** are registry packages (e.g., `http-helpers`)
- **Keys with `/`** are git packages (e.g., `"github.com/user/repo"`)

### Entrypoint Resolution

When you import a package, Sema resolves the entrypoint in this order:

1. **Direct file** — `~/.sema/packages/<spec>.sema` (for sub-module imports like `github.com/user/repo/utils`)
2. **Custom entrypoint** — if `sema.toml` exists and has an `entrypoint = "..."` field, that file is loaded
3. **Default entrypoint** — `package.sema` in the package directory

## CLI Commands

### `sema pkg init`

Initialize a new project in the current directory. Creates a `sema.toml` with the directory name as the package name.

```bash
mkdir my-package && cd my-package
sema pkg init
```

::: tip
`sema pkg init` only creates `sema.toml`. You'll need to create `package.sema` yourself to define your package's public API.
:::

### `sema pkg add`

Add a package from the registry or a git repository.

```bash
# Registry packages (short names)
sema pkg add http-helpers            # latest version
sema pkg add http-helpers@1.0.0      # specific version

# Git packages (URL paths)
sema pkg add github.com/user/repo          # latest default branch (main)
sema pkg add github.com/user/repo@v1.2.0   # specific tag
sema pkg add github.com/user/repo@main     # specific branch
```

The source is auto-detected: if the first path segment contains a dot (looks like a hostname), it's treated as a git URL. Otherwise, it's looked up on the configured registry.

You can override the registry with `--registry`:

```bash
sema pkg add http-helpers --registry https://my-registry.com
```

If a `sema.toml` exists in the current directory, the package is automatically added to the `[deps]` section.

### `sema pkg install`

Fetch all dependencies listed in `sema.toml`.

```bash
sema pkg install
```

Reads the `[deps]` section and fetches each dependency — routing to the registry or git based on the key format (see [sema.toml](#sema-toml) above). Requires a `sema.toml` in the current directory.

### `sema pkg update`

Update installed packages to their latest versions.

```bash
sema pkg update                       # update all installed packages
sema pkg update http-helpers          # update a specific registry package
sema pkg update github.com/user/repo  # update a specific git package
sema pkg update repo                  # update by short name
```

- **Registry packages** check for a newer version and re-download if available
- **Git packages** run `git pull` to fetch the latest changes

### `sema pkg remove`

Remove an installed package from the global cache and your project's `sema.toml`.

```bash
sema pkg remove http-helpers          # registry package
sema pkg remove github.com/user/repo  # git package by full path
sema pkg remove repo                  # by short name
```

### `sema pkg list`

List all installed packages with their version/ref and source.

```bash
sema pkg list
```

```
  http-helpers (1.0.0) [https://pkg.sema-lang.com]
  github.com/user/repo (v1.2.0) [git]
  github.com/user/utils (main) [git]
```

### `sema pkg search`

Search the registry for packages.

```bash
sema pkg search http
sema pkg search json --registry https://my-registry.com
```

```
Found 3 packages:

  http-helpers — HTTP client utilities for Sema
  http-server — Simple HTTP server framework
  http-mock — HTTP mocking for tests
```

### `sema pkg info`

Show detailed package information from the registry.

```bash
sema pkg info http-helpers
```

```
http-helpers
  HTTP client utilities for Sema
  repo: https://github.com/user/http-helpers
  owners: alice, bob

  Versions:
    2.0.0 — 12480 bytes, 2026-02-20T10:30:00Z
    1.1.0 — 11200 bytes, 2026-01-15T08:00:00Z
    1.0.0 — 9800 bytes, 2025-12-01T12:00:00Z
```

### `sema pkg publish`

Publish the current package to the registry. Requires a `sema.toml` with `[package]` containing `name` and `version`, and an active login.

```bash
sema pkg publish
sema pkg publish --registry https://my-registry.com
```

```
Packaging...
  24576 bytes compressed
✓ Published http-helpers@1.0.0 (24576 bytes, sha256:abc123...)
```

### `sema pkg yank`

Yank a published version to prevent new installs (existing installs are unaffected).

```bash
sema pkg yank http-helpers@1.0.0
```

### `sema pkg login`

Authenticate with a package registry by providing an API token.

```bash
sema pkg login --token sema_pat_...                          # default registry
sema pkg login --token sema_pat_... --registry https://...   # self-hosted
```

Tokens are stored in `~/.sema/credentials.toml` with `0600` file permissions. You can generate a token from your registry account page.

### `sema pkg logout`

Remove stored registry credentials.

```bash
sema pkg logout
```

### `sema pkg config`

View or set package manager configuration. Currently supports `registry.url` to change the default registry.

```bash
sema pkg config                                           # show all config
sema pkg config registry.url                              # show current registry URL
sema pkg config registry.url https://my-registry.com      # set default registry
```

```
registry.url = https://pkg.sema-lang.com
registry.token = (set)

Credentials file: /Users/you/.sema/credentials.toml
```

## Importing Packages

Import a package by its URL path (git packages) or short name (registry packages):

```sema
;; Git package
(import "github.com/user/string-utils")
(string-utils/slugify "Hello World")
; => "hello-world"

;; Registry package
(import "http-helpers")
(http-helpers/fetch "https://api.example.com")
```

The package name (last segment of the URL, or the short name) becomes the namespace prefix. You can also use selective imports:

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
(import "github.com/user/repo")        ; → ~/.sema/packages/github.com/user/repo/package.sema
(import "github.com/user/repo/utils")  ; → ~/.sema/packages/github.com/user/repo/utils.sema

;; File imports (relative to current file)
(import "./helpers.sema")              ; relative file
(import "../lib/utils.sema")           ; parent directory
```

## On-Disk Layout

Packages are cached globally at `~/.sema/packages/`, with different structures for registry and git packages:

```
~/.sema/
  credentials.toml              # registry token + URL
  history.txt                   # REPL history
  packages/
    http-helpers/               # registry package (short name)
      .sema-pkg.json            # source metadata
      sema.toml
      package.sema
    github.com/                 # git packages (URL structure)
      user/
        repo/
          .git/
          sema.toml
          package.sema
```

Registry packages include a `.sema-pkg.json` metadata file that tracks the source, version, registry URL, and checksum. This file is managed automatically by the package manager.

## Creating a Package

### 1. Initialize

```bash
mkdir sema-csv-utils && cd sema-csv-utils
sema pkg init
```

### 2. Write Your Code

Create `package.sema` to define your package's public API:

```sema
;; package.sema — package entrypoint
(defun parse-row (line)
  (map string/trim (string/split line ",")))

(defun parse-csv (text)
  (map parse-row (string/split text "\n")))
```

### 3. Add Dependencies (Optional)

```bash
sema pkg add http-helpers@1.0.0
```

This fetches the package and adds it to your `sema.toml` automatically. Then use it in your code:

```sema
(import "http-helpers")

(defun fetch-csv (url)
  (parse-csv (:body (http-helpers/get url))))
```

### 4. Publish

#### To the Registry

Ensure your `sema.toml` has `name` and `version` in the `[package]` section, then:

```bash
sema pkg login --token sema_pat_...
sema pkg publish
```

Others can now install your package:

```bash
sema pkg add sema-csv-utils@0.1.0
```

#### Via Git (No Registry)

Push to a public git repository. Tag releases with semver:

```bash
git tag v0.1.0
git push origin main --tags
```

Others can install directly from git:

```bash
sema pkg add github.com/yourname/sema-csv-utils@v0.1.0
```

## Example Workflow

```bash
# Start a new project
mkdir my-project && cd my-project
sema pkg init

# Add dependencies (mix of registry and git)
sema pkg add http-helpers@2.0.0
sema pkg add github.com/user/json-schema@v1.1.0

# Install everything (if cloning the project fresh)
sema pkg install

# List what's installed
sema pkg list

# Search for packages
sema pkg search csv

# Check package details
sema pkg info csv-parser
```

```sema
;; main.sema
(import "http-helpers")
(import "github.com/user/json-schema")

(def response (http-helpers/fetch "https://api.example.com/data"))
(def valid? (json-schema/validate schema (json/decode (:body response))))
(println (if valid? "Valid!" "Invalid."))
```

```bash
sema main.sema
```

## Self-Hosted Registry

Sema's package registry is designed to be self-hostable. The registry server ships in the [`pkg/`](https://github.com/helgesverre/sema/tree/main/pkg) directory of the Sema repository — it's a single Rust binary backed by SQLite that serves both a web UI and a REST API. See its [README](https://github.com/helgesverre/sema/tree/main/pkg#readme) for build and deployment instructions.

To point the CLI at your own registry instance:

```bash
# Set as default registry
sema pkg config registry.url https://registry.mycompany.com

# Or per-command
sema pkg add my-internal-lib --registry https://registry.mycompany.com
sema pkg publish --registry https://registry.mycompany.com
```

All `sema pkg` commands that interact with the registry accept a `--registry` flag to override the default.

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

### "Not logged in"

Publishing and yanking require authentication. Run `sema pkg login --token <token>` with a token from your registry account page.

### "git clone/fetch failed"

The package URL couldn't be reached. Check that:
- The repository exists and is public (or you have git credentials configured)
- The git ref (tag/branch) exists on the remote
- You have network access
