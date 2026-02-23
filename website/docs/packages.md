---
outline: [2, 3]
---

# Package Manager

Sema uses Go-style URL-based packages. Packages are git repositories — there is no central registry. Any public git repo with a `sema.toml` and `mod.sema` is a valid package.

## Package Format

A package is a directory containing at minimum:

- **`sema.toml`** — package metadata and dependencies
- **`mod.sema`** — the package entrypoint (what gets loaded on import)

### `sema.toml`

```toml
[package]
name = "my-package"
version = "0.1.0"
description = "A useful Sema library"
entrypoint = "mod.sema"

[deps]
github.com/user/other-lib = "v1.0.0"
github.com/user/utils = "main"
```

The `[package]` section defines metadata:

| Field         | Description                                       |
| ------------- | ------------------------------------------------- |
| `name`        | Package name                                      |
| `version`     | Semver version string                             |
| `description` | Short description of the package                  |
| `entrypoint`  | File loaded on import (default: `mod.sema`)       |

The `[deps]` section maps package URLs to git refs (tags, branches, or commit SHAs).

## CLI Commands

### `sema pkg init`

Initialize a new package in the current directory. Creates a `sema.toml` and `mod.sema` if they don't exist.

```bash
mkdir my-package && cd my-package
sema pkg init
```

### `sema pkg get`

Add a dependency and fetch it.

```bash
sema pkg get github.com/user/repo          # latest default branch
sema pkg get github.com/user/repo@v1.2.0   # specific tag
sema pkg get github.com/user/repo@main     # specific branch
```

### `sema pkg install`

Fetch all dependencies listed in `sema.toml`.

```bash
sema pkg install
```

### `sema pkg update`

Update a specific dependency or all dependencies to their latest refs.

```bash
sema pkg update                    # update all
sema pkg update github.com/user/repo  # update one
```

### `sema pkg remove`

Remove a dependency from `sema.toml` and delete its cached files.

```bash
sema pkg remove github.com/user/repo
```

### `sema pkg list`

List all installed dependencies and their versions.

```bash
sema pkg list
```

## Importing Packages

Import a package by its URL:

```sema
(import "github.com/user/string-utils")

(string-utils/slugify "Hello World")
; => "hello-world"
```

The package name (last segment of the URL) becomes the namespace prefix.

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
```

## Creating a Package

### 1. Initialize

```bash
mkdir sema-csv-utils && cd sema-csv-utils
sema pkg init
```

### 2. Write Your Code

Edit `mod.sema` to export your package's public API:

```sema
;; mod.sema
(defun parse-row (line)
  (string/split line ","))

(defun parse-csv (text)
  (map parse-row (string/split-lines text)))
```

### 3. Add Dependencies (Optional)

```bash
sema pkg get github.com/user/string-utils@v1.0.0
```

Then use them in your code:

```sema
(import "github.com/user/string-utils")

(defun parse-row (line)
  (map string-utils/trim (string/split line ",")))
```

### 4. Publish

Push to a public git repository. Tag releases with semver:

```bash
git tag v0.1.0
git push origin main --tags
```

Others can now install your package:

```bash
sema pkg get github.com/yourname/sema-csv-utils@v0.1.0
```

## Example Workflow

```bash
# Start a new project
mkdir my-project && cd my-project
sema pkg init

# Add dependencies
sema pkg get github.com/user/http-helpers@v2.0.0
sema pkg get github.com/user/json-schema@v1.1.0

# Install everything
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
