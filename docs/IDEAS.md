# Sema Ideas & Feature Tracker

Consolidated from GitHub issues #7–#12 and session notes. Updated 2026-03-20.

---

## Web Server (#7)

| Feature | Status | Notes |
|---------|--------|-------|
| `http/serve` with handler fn | Done | |
| Request/response as maps | Done | `:method`, `:path`, `:headers`, `:query`, `:params`, `:body`, `:json`, `:remote` |
| Response helpers (`http/ok`, `http/html`, etc.) | Done | 8 helpers: ok, created, no-content, not-found, error, redirect, html, text |
| `http/router` data-driven routing | Done | `[:method "/path" handler]` vectors |
| Route params (`:id`) and wildcards (`*`) | Done | |
| Middleware as function composition | Done | Composable via `->` threading |
| SSE streaming (`http/stream`) | Done | |
| WebSockets (`http/websocket`, ws/send/recv/close) | Done | |
| Static file serving (`:static` routes) | Done | MIME detection, index.html, path traversal protection |
| `http/file` for serving individual files | Done | Custom content-type override |
| `route/prefix` for composing route groups | **Todo** | Simple data transform on route vectors |
| `tools->routes` auto-expose tools as REST endpoints | **Todo** | Generate POST routes from `deftool` definitions |
| Auto-generated OpenAPI spec from tools | Deferred | Low priority — tools already have name/desc/params |
| `defroute` special form | Skipped | Data-driven approach preferred |

---

## Misc Ideas (#8)

| Idea | Status | Effort | Impact | Notes |
|------|--------|--------|--------|-------|
| `defapi` — OpenAPI spec to tools | Not started | High | High | Auto-generate `deftool` from Swagger/OpenAPI. Would need HTTP client + JSON schema parsing. |
| `watch` — file/data watchers | Not started | Medium | Medium | `notify` crate for FS events, interval timers for polling. |
| `defpipe` — LLM pipelines | Not started | High | High | Typed chains with schema validation between steps. Retry/cache per stage. |
| `sema test` — test runner + LLM mocking | Not started | Medium | High | `deftest`, `assert=`, `assert`, `llm/with-mock`. CLI `sema test`. Most practical next feature. |
| `cron` — scheduled tasks | Not started | Medium | Medium | Cron expression parser + background thread pool. |
| `repl/connect` — remote REPL | Not started | High | Medium | Socket-based REPL server. nREPL-like protocol. |
| `defschema` — reusable extraction schemas | Not started | Medium | High | Named schemas with validation, transforms, defaults, composition. |
| `env` — environment profiles | Not started | Low | Medium | Profile-based config switching. `--env prod` flag. |
| `sema deploy` — single-command deployment | Not started | High | Medium | Dockerfile generation for fly.io/railway. Depends on `sema build`. |
| `bridge` — JS/Python interop | Not started | Very High | High | WASM↔JS already partial (playground). Python FFI would be new. |

---

## `sema build` (#9)

| Feature | Status | Notes |
|---------|--------|-------|
| Bytecode compilation (`sema compile`) | Done | Produces `.semac` files |
| Standalone executables (`sema build`) | Done | Appends bytecode to runtime binary |
| Cross-compilation (`--target`) | Done | Multi-target builds |
| Import tracing for bundling | Done | Static discovery of imported files |
| Disassembly (`sema disasm`) | Done | |

**This issue is fully implemented and can be closed.**

---

## Package Manager (#10)

| Feature | Status | Notes |
|---------|--------|-------|
| Module system (`import`/`export`) | Done | File-based modules with isolation |
| `sema.toml` manifest | Done | Package name, version, deps, entrypoint |
| `sema.lock` lock file | Done | SHA256 checksums, git commit SHAs |
| `sema pkg init` | Done | Scaffolds sema.toml + package.sema |
| `sema pkg add <spec>` | Done | Git repos + registry packages |
| `sema pkg install` | Done | With `--locked` CI mode |
| `sema pkg update` | Done | Single package or all |
| `sema pkg remove` | Done | Cleans cache, toml, and lock |
| `sema pkg list` | Done | Shows installed with version/source |
| `sema pkg login/logout` | Done | Credentials in ~/.sema/credentials.toml |
| `sema pkg search/info` | Done | Queries registry API |
| `sema pkg publish/yank` | Done | Tarball creation + upload |
| Registry server (`pkg/` directory) | ~80% done | Axum + SQLite/Postgres, auth, Docker/Fly.io configs |
| Central registry (`pkg.sema-lang.com`) | **Not deployed** | Server code exists, needs hosting |
| Semver range constraints | Not started | Currently exact version/ref only |
| Package signing | Not started | No cryptographic verification |
| Pre/post-install hooks | Not started | Designed but not implemented |
| Per-package sandboxing | Not started | Packages share caller's sandbox |

---

## Metaprogramming (#11)

| Feature | Status | Notes |
|---------|--------|-------|
| `defmacro` | Done | Standard hygienic macros with gensym |
| `quasiquote` / `unquote` / `unquote-splicing` | Done | |
| `macroexpand` | Done | |
| `defmacro/ai` — LLM-powered macros | Not started | Use LLM during macro expansion for code generation. Unique to Sema. |
| `derive` — auto-generate from records | Not started | Like Rust derive but for Sema records. Could be LLM-powered. |
| `deftest/ai` — LLM-generated test cases | Not started | Generate tests from function source + docstring. |
| Reader macros | Not started | Custom syntax extensions at read time. |
| Compile-time evaluation (`eval-when`) | Not started | R7RS-style phase separation. |

---

## Prompts & Conversations (#12)

| Feature | Status | Notes |
|---------|--------|-------|
| `prompt` special form | Done | Constructs prompt with `system`/`user`/`assistant` messages |
| `prompt/concat` / `prompt/append` | Done | Variadic prompt composition |
| `prompt/messages` | Done | Extract message list from prompt |
| `prompt/set-system` | Done | Replace system message |
| `prompt/fill` — template variables | Done | `{{var}}` substitution from a map |
| `prompt/slots` — list template vars | Done | Returns set of `{{var}}` names |
| `message` / `message?` / `message/role` / `message/content` | Done | Message constructors and accessors |
| `conversation/new` | Done | With optional `{:model "..."}` |
| `conversation/messages` | Done | |
| `conversation/fork` | Done | Immutable branching |
| `conversation/add-message` | Done | |
| `conversation/filter` / `conversation/map` | Done | |
| `conversation/set-system` | Done | |
| `conversation/model` / `conversation/system` | Done | Accessors |
| `defprompt` special form | Not started | Like `defn` but for parameterized prompt builders |
| Prompt versioning / diffing | Not started | Track prompt changes over time |
| Conversation branching trees | Not started | Tree structure for exploring alternatives |
| Prompt cost estimation | Not started | Token counting before sending |
| Prompt validation | Not started | Type-check message structure |

---

## Priority Recommendations

### Quick wins (do soon)
1. **`route/prefix`** — ~15 lines, pure data transform
2. **`tools->routes`** — ~40 lines, bridges tool and HTTP worlds

### High impact (next session)
3. **`sema test`** — built-in test runner. Most practical missing feature.
4. **`defschema`** — reusable extraction schemas with validation
5. **Close issues #7 and #9** — fully implemented

### Unique to Sema (demo/blog worthy)
6. **`defmacro/ai`** — LLM-powered compile-time code generation
7. **`defpipe`** — typed LLM pipelines
8. **`defapi`** — OpenAPI to tools in one line

### Infrastructure
9. **Deploy central registry** — `pkg.sema-lang.com`
10. **Semver ranges** — proper dependency constraints
