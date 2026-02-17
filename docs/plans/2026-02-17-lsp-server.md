# Sema Developer Experience — Design Document (v2)

**Date:** 2026-02-17
**Status:** Draft
**Supersedes:** `2026-02-16-lsp-server.v1.md`
**Implementation:** Not started
**Goal:** World-class developer experience — tree-sitter grammar for instant structural editing + full LSP for semantic features.

## Overview

Two complementary systems:

1. **`tree-sitter-sema`** — A dedicated tree-sitter grammar for Sema. Currently Helix piggybacks on the Scheme grammar (`grammar = "scheme"`), which doesn't understand Sema-specific syntax (keywords `:foo`, hash maps `{}`, vectors `[]`, `#t`/`#f` booleans, block comments `#| |#`, etc.). A proper grammar gives every tree-sitter-native editor (Neovim, Helix, Zed, Emacs 29+) accurate highlighting, folding, indentation, and text objects — instantly, with no server.

2. **`sema-lsp`** — A Language Server Protocol server providing diagnostics, completions, go-to-definition, and hover. Uses `sema-reader` for parsing, `sema-eval::Interpreter` for semantic analysis, and the sandbox system for safety. The bytecode VM (`sema-vm`) is not used initially — all analysis runs through the tree-walker under a restrictive sandbox.

## Prerequisites (Phase 0)

Before starting either system:

1. **Unify the special-forms list.** `SPECIAL_FORMS` in `main.rs` and `SpecialFormSpurs` in `special_forms.rs` are duplicated and out of sync. Export a canonical `SPECIAL_FORM_NAMES: &[&str]` from `sema-eval` and have both CLI completion and LSP consume it.

2. **Add end positions to `Span`** (recommended, not blocking Phase 1). Currently `Span { line, col }` is start-only. Adding `end_line`/`end_col` improves diagnostic underlines and is required for precise go-to-definition highlighting in Phase 3+.

---

## Tree-sitter Grammar (`tree-sitter-sema`)

### Why a Dedicated Grammar

Sema extends standard Scheme syntax with:
- Keywords: `:foo`, `:bar`
- Hash map literals: `{:key value}`
- Vector literals: `[1 2 3]`
- Block comments: `#| ... |#`
- String escapes: `\n`, `\t`, `\\`, `\"`
- Boolean literals: `#t`, `#f` (plus `true`/`false` as symbols)
- Dot notation in symbols: `record.field`

The Scheme tree-sitter grammar doesn't parse these correctly. A custom `tree-sitter-sema` grammar (~100-150 lines of `grammar.js`) handles all of them.

### Deliverables

- `tree-sitter-sema/` repository (or subdirectory under `editors/`)
- `grammar.js` defining: atoms (int, float, string, char, boolean, keyword, symbol), lists `()`, vectors `[]`, maps `{}`, quote/unquote/quasiquote/splice, comments (line `;` and block `#| |#`)
- Query files: `highlights.scm`, `indents.scm`, `textobjects.scm`, `folds.scm`
- Published to npm (for Neovim/Helix/Zed consumption) and as a Rust crate
- Updated editor configs: Helix switches from `grammar = "scheme"` to `grammar = "sema"`, Neovim gets tree-sitter config, Zed gets language extension

### What This Enables (No LSP Required)

| Feature | Editor Support |
|---------|---------------|
| Accurate syntax highlighting | Neovim, Helix, Zed, Emacs 29+ |
| Code folding | All tree-sitter editors |
| Smart indentation | All tree-sitter editors |
| Structural text objects (`af` = around function, `if` = inside function) | Neovim, Helix |
| Incremental select (expand/shrink selection by AST node) | Neovim, Helix, Zed |
| Syntax-aware commenting | All tree-sitter editors |

### Complexity

**Easy–Medium.** S-expression grammars are among the simplest to write for tree-sitter. Existing `tree-sitter-sexp`, `tree-sitter-commonlisp`, and `tree-sitter-clojure` grammars serve as references. The query files can be adapted from the existing Helix `.scm` files (which are already well-structured with 300+ lines of Sema-specific patterns).

---

## Crate Structure

### New Crate: `crates/sema-lsp/`

```toml
[package]
name = "sema-lsp"
version = "0.1.0"
edition = "2021"

[dependencies]
sema-core.workspace = true
sema-reader.workspace = true
sema-eval.workspace = true
tower-lsp = "0.20"
tokio = { workspace = true, features = ["rt-multi-thread", "macros", "io-std"] }
dashmap = "6"
```

Note: **no direct dependency on `sema-stdlib` or `sema-vm`**. Builtins are accessed through `Interpreter::new_with_sandbox()` which registers stdlib internally. This avoids coupling to `register_stdlib`'s signature.

### Binary Entry Point

Add an `Lsp` variant to `Commands` in `crates/sema/src/main.rs`:

```rust
#[derive(Subcommand)]
enum Commands {
    Ast { /* ... */ },
    Completions { /* ... */ },
    /// Start the Language Server Protocol server
    Lsp,
}
```

The handler calls `sema_lsp::run_server().await`.

### Dependency Flow

```
sema-core ← sema-reader
    ↑           ↑
sema-vm    sema-eval ← sema-stdlib
    ↑           ↑
    └───────────┘
                ↑
            sema-lsp
                ↑
            sema (binary)

(sema-wasm is a separate WASM target, not involved)
```

`sema-lsp` depends on `sema-core`, `sema-reader`, and `sema-eval`. It does **not** depend on `sema-vm`, `sema-stdlib`, or `sema-llm`.

### Threading Model

`tower-lsp` is async (tokio). Sema's evaluator is single-threaded (`Rc`, not `Arc`). The LSP runs a **dedicated backend thread** that owns all `Rc` state:

- One `Interpreter` instance (sandboxed, no IO/network/shell/LLM)
- Cached builtin/special-form name lists
- Parsed ASTs and span maps for open documents

Async LSP handlers send requests to the backend via a channel and await responses on a oneshot. This is cleaner than scattered `spawn_blocking` calls once multiple features share state.

```
┌─────────────────────┐     channel      ┌──────────────────────┐
│  tower-lsp async    │ ──── Request ──→ │  Backend thread      │
│  handlers           │ ←── Response ─── │  (owns Interpreter,  │
│  (Send, tokio)      │     (oneshot)    │   Rc state, caches)  │
└─────────────────────┘                  └──────────────────────┘
```

Only `Send`-safe types cross the channel: `String`, `Url`, LSP structs, `Vec<Diagnostic>`, etc.

---

## Phase 1: Parse Diagnostics

**Complexity:** Easy
**Reuses:** `sema_reader::read_many_with_spans`, `SemaError` (spans, hints, notes)

### How It Works

1. On `textDocument/didOpen` and `textDocument/didChange`, send the full document text to the backend thread.
2. Backend calls `sema_reader::read_many_with_spans(&text)`.
3. On `Ok(...)` → return empty diagnostics.
4. On `Err(e)` → convert to LSP `Diagnostic` and return.

### Document State

Track open documents in a `DashMap<Url, String>` on the async side (for quick access) and forward text to the backend for analysis.

### Span Conversion

The reader's `Span { line, col }` is 1-indexed. Convert to LSP 0-indexed:

```rust
fn span_to_lsp_range(span: &Span) -> lsp_types::Range {
    let pos = Position {
        line: span.line.saturating_sub(1) as u32,
        character: span.col.saturating_sub(1) as u32,
    };
    Range { start: pos, end: pos } // point range until end spans exist
}
```

### Error Conversion

`SemaError` supports `.hint()` and `.note()` via `WithContext`. Append these to the diagnostic message:

```rust
fn error_to_diagnostic(err: &SemaError) -> Diagnostic {
    let range = match err.span() {
        Some(span) => span_to_lsp_range(span),
        None => Range::default(),
    };
    let mut message = err.inner().to_string();
    if let Some(hint) = err.hint() {
        message.push_str(&format!("\nhint: {hint}"));
    }
    if let Some(note) = err.note() {
        message.push_str(&format!("\nnote: {note}"));
    }
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("sema".into()),
        message,
        ..Default::default()
    }
}
```

### Error Recovery

`read_many_with_spans` stops at the first parse error. For Phase 1, this means one diagnostic at a time. Consider adding `read_many_with_spans_recover` to `sema-reader` that skips to the next top-level form on error, returning `(Vec<Value>, SpanMap, Vec<SemaError>)`. This is standard for LSP servers and not blocking for Phase 1.

### `didClose` Handling

Remove document from store and publish empty diagnostics to clear stale errors.

### Deliverables

- Real-time red squiggles for unclosed parens, unterminated strings, invalid tokens
- Error messages include hints ("Did you mean...?") and notes
- Pure parse — fast, no side effects

---

## Phase 2: Completion

**Complexity:** Medium
**Reuses:** `Interpreter` (global env bindings), `SPECIAL_FORM_NAMES` (from Phase 0 prerequisite)

### Completion Sources

1. **Special forms** — from the exported `SPECIAL_FORM_NAMES` const. Kind: `Keyword`.
2. **Stdlib builtins** — enumerate `interpreter.global_env` bindings where value is `NativeFn`. Kind: `Function`.
3. **User definitions** — walk the parsed AST for top-level `define`/`defun`/`defmacro` forms. Kind: `Variable`/`Function`/`Keyword` respectively.

### Builtin Collection

On backend thread startup:

```rust
let interpreter = Interpreter::new_with_sandbox(&Sandbox::deny_all());
let builtins: Vec<String> = interpreter.global_env
    .all_binding_names()  // or manual walk of bindings HashMap
    .collect();
```

This runs once. The builtin list is static for the server's lifetime.

### Trigger and Prefix Handling

Register `completion_provider` with trigger characters `(` and space. The prefix parser must treat `/` as part of the symbol name (Sema uses `/` for namespacing: `string/trim`, `map/get`).

### Scope Limitations

Phase 2 only handles top-level definitions. `let`/`let*`/`letrec` bindings are not visible. Scope-aware completion is Phase 3+.

---

## Phase 3: Go to Definition

**Complexity:** Hard
**Reuses:** `SpanMap`, module resolution logic, `read_many_with_spans`

### 3a. Import/Load Path Resolution (Easy)

When cursor is on a string in `(import "path")` or `(load "path")`:
- Resolve path relative to current file (same logic as evaluator's module loader)
- Return `Location` pointing to the resolved file, line 0

**Note:** Path resolution logic is currently duplicated between tree-walker (`special_forms.rs`) and VM delegates (`eval.rs`). Consider extracting a shared helper before implementing this.

### 3b. User-Defined Symbols (Medium)

For `define`/`defun` symbols:
- Parse file with `read_many_with_spans`
- Walk top-level forms to find the binding
- Use `SpanMap` to look up the span (keyed by `Rc` pointer address via `Rc::as_ptr as usize`)

**Important:** The `SpanMap` requires the exact `Value` pointers from parsing — no cloning. The lookup must traverse the parsed AST and read `Rc` pointers. This is fragile but functional for single-file analysis.

### 3c. Cross-File Module Definitions (Hard)

Resolve imported symbol to its definition in another file. Requires parsing target module and tracking exports.

### Symbol-Under-Cursor

Determine what symbol the cursor is on via character-class scan (backward/forward from cursor to find symbol boundaries delimited by whitespace and parens). Simple and robust for Lisp.

### End Spans

Phase 3 strongly benefits from `Span` having `end_line`/`end_col`. Without end spans, go-to-definition returns a point position (editors highlight the word at that position, which works but is imprecise).

---

## Phase 4: Hover Documentation

**Complexity:** Medium–Hard
**Reuses:** `NativeFn` (name), `Lambda` (params), `SpanMap`

### 4a. Builtin Documentation

Ship a static `HashMap<&str, &str>` of name→doc in `sema-lsp`. Source docs from the website at `sema-lang.com/docs/`. This avoids modifying `NativeFn` or touching all 350+ registration calls across 19 stdlib modules.

Future: add `doc: Option<String>` to `NativeFn` for inline docs.

### 4b. User Function Signatures

For `(defun name (x y z) ...)`, extract and display the parameter list from the parsed AST.

### 4c. Format

Return Markdown hover content with `sema` fenced code blocks (the TextMate grammar provides syntax highlighting in VS Code hover popups).

---

## Editor Integration

### VS Code (`editors/vscode/sema/`)

The extension currently only provides TextMate grammar. Add LSP client:

1. Add `vscode-languageclient` dependency
2. Create `extension.ts` entry point that spawns `sema lsp`
3. Add `tsconfig.json` and build step (`esbuild` recommended)
4. Update `package.json`: `activationEvents`, `main` field

### Other Editors

- **Neovim:** `nvim-lspconfig` entry pointing to `sema lsp`
- **Helix:** `[[language]]` in `languages.toml` with `command = "sema"`, `args = ["lsp"]`
- **Emacs:** `eglot` config
- **Zed:** `settings.json` LSP configuration

---

## Testing Strategy

### Unit Tests (in `sema-lsp`)

- `span_to_lsp_range` conversion edge cases (line 1 col 1 → 0,0)
- `error_to_diagnostic` with Reader errors, WithContext, WithTrace variants
- Builtin name collection returns non-empty list with known names
- Definition extraction from various AST shapes

### Integration Tests

- In-process LSP via `tower_lsp::LspService::new()`: initialize → didOpen with bad source → assert diagnostics
- Completion results for partial prefixes
- Go-to-definition for top-level `defun`

### Manual Checklist

- [ ] `sema lsp` starts without crash
- [ ] VS Code shows squiggles for `(define x` (unclosed paren)
- [ ] Fixing error clears squiggles
- [ ] Completions appear for `str` → `string/trim`, `string-append`, etc.
- [ ] Multiple files open simultaneously

---

## Summary

| Phase | Feature | Complexity | Effort (agent) |
|-------|---------|------------|----------------|
| 0 | Prerequisites (special forms export, optional end spans) | Easy | ~30 min |
| T | Tree-sitter grammar + queries + editor configs | Easy–Medium | ~2–3 hours |
| 1 | LSP: Parse diagnostics | Easy | ~2 hours |
| 2 | LSP: Completion | Medium | ~1–2 hours |
| 3 | LSP: Go to definition | Hard | ~2–3 hours |
| 4 | LSP: Hover docs | Medium–Hard | ~2–3 hours |

### Implementation Order

1. **Phase 0** — unify special forms list (small, unblocks Phase 2 + tree-sitter queries)
2. **Phase T** — tree-sitter grammar (immediate payoff across all modern editors, independent of LSP)
3. **Phase 1** — LSP parse diagnostics (immediate value, validates tower-lsp plumbing)
4. **Phase 2** — LSP completion (most-requested IDE feature)
5. **Phase 3a** — import path resolution (easy, do alongside Phase 2)
6. **Phase 3b/3c + Phase 4** — incrementally as LSP matures

**Total estimated agent effort: ~10–14 hours for everything.** Phases 0+T+1+2 (~6–8 hours) deliver 90% of the daily-use value.

---

## Future Considerations

- **Compile-time diagnostics:** The VM compiler (`sema_vm::compile_program`) could detect unbound variables and invalid forms without executing code. This would be a "Phase 1b" — parse → macroexpand → compile (no execute) → report compiler errors. Requires macro expansion under sandbox and honest span propagation from the compiler. Deferred until Phase 1 is stable.

- **Eval-level diagnostics:** Running the tree-walker for deeper analysis (type mismatches, arity errors at call sites). Must use `Sandbox::deny_all()` and add timeouts. High complexity, low priority.

- **Incremental parsing:** Phase 1 re-parses the full file on every keystroke. The reader is fast enough for typical Sema files. If performance becomes an issue, add debouncing (100–200ms via `tokio::time::sleep`) first, then consider `TextDocumentSyncKind::INCREMENTAL`.

- **WASM LSP:** `sema-wasm` exists but `tower-lsp` is stdio-oriented. In-browser LSP would need a different transport (WebSocket/worker). Out of scope.

- **Tree-sitter in LSP:** Once `tree-sitter-sema` exists, the LSP could optionally use it for faster incremental parsing instead of re-running `sema-reader` on every keystroke. Low priority — `sema-reader` is already fast for typical file sizes.
