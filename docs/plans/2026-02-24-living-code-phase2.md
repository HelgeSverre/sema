# Living Code Phase 2: Self-Reading — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable Sema programs to read and analyze their own source code with `source-of`, directive comments (`;;@key value`), and `read-source`.

**Architecture:** Three layers — (1) store source text + file/line metadata during `defn`/`define`/`defmacro` evaluation via the existing `set_meta` system, (2) add a directive-aware reader variant to `sema-reader` that captures `;;@` comments, (3) a new `read-source` stdlib function that parses files into structured definition maps.

**Tech Stack:** Rust, existing `sema-reader` lexer/parser, `glob` crate (already a dependency of `sema-stdlib`), `sema-core` metadata system from Phase 1.

---

### Task 1: `source-of` — store source text in metadata during definition

**Files:**
- Modify: `crates/sema-eval/src/special_forms.rs` (eval_defun, eval_define, eval_defmacro)
- Modify: `crates/sema-eval/src/special_forms.rs` (eval_meta — add :source, :file, :line to output)
- Test: `crates/sema/tests/integration_test.rs`

**Context:** When `defn`/`define`/`defmacro` fire, they already have the original AST as `&[Value]` (the args). We reconstruct the source text by formatting the full definition form via `Value::Display`. The EvalContext has `current_file_path()` and we can get the line from the span table via `ctx.lookup_span()`.

**Step 1: Write the failing test**

Add to `crates/sema/tests/integration_test.rs`:

```rust
#[test]
fn test_source_of_via_meta() {
    // source-of should appear in (meta) output
    let result = eval_to_string(
        r#"(begin
             (defn add "Add two numbers." (a b) (+ a b))
             (get (meta add) :source))"#,
    );
    // The source should be the reconstructed defn form
    assert!(result.contains("defn"));
    assert!(result.contains("add"));
    assert!(result.contains("Add two numbers."));
}

#[test]
fn test_source_of_define_shorthand() {
    let result = eval_to_string(
        r#"(begin
             (define (square x) "Square a number." (* x x))
             (get (meta square) :source))"#,
    );
    assert!(result.contains("square"));
    assert!(result.contains("Square a number."));
}

#[test]
fn test_meta_includes_line() {
    // :line should be populated (non-nil) when we can determine it
    let result = eval_to_string(
        r#"(begin
             (defn f (x) x)
             (get (meta f) :line))"#,
    );
    // Should be an integer, not nil
    assert_ne!(result, "nil");
}
```

**Step 2: Run test to verify it fails**

Run: `cargo test -p sema --test integration_test -- test_source_of_via_meta test_source_of_define_shorthand test_meta_includes_line`
Expected: FAIL — `:source` not in metadata, `:line` is nil

**Step 3: Implement source-of storage in eval_defun**

In `eval_defun`, after `env.set_meta(name_spur, Value::keyword("doc"), ...)`, reconstruct and store the source:

```rust
// Reconstruct source text from the original AST
let mut source_parts = vec![Value::symbol("defn"), Value::symbol(&resolve(name_spur))];
if let Some(ref doc) = docstring {
    source_parts.push(Value::string(doc));
}
source_parts.push(args[params_idx].clone()); // param list
source_parts.extend_from_slice(&args[params_idx + 1..]); // body
let source_form = Value::list(source_parts);
env.set_meta(name_spur, Value::keyword("source"), Value::string(&source_form.to_string()));
```

Do the same in `eval_define` (function shorthand branch) and `eval_defmacro`.

**Step 4: Add :line and :file metadata**

In `eval_defun`, after source storage:

```rust
// Store file and line from EvalContext
if let Some(file_path) = ctx.current_file_path() {
    env.set_meta(name_spur, Value::keyword("file"), Value::string(&file_path.to_string_lossy()));
}
// Try to get line from the span table for the name symbol
if let Some(span) = ctx.lookup_span(args[0].as_ptr()) {
    env.set_meta(name_spur, Value::keyword("line"), Value::int(span.line as i64));
}
```

Same for `eval_define` and `eval_defmacro`.

**Step 5: Update eval_meta to include :source, :file, :line**

In `eval_meta`, after populating the map from existing metadata, also add `:source`, `:file`, `:line` keys from metadata (they'll already be there from set_meta — we just need to make sure the meta map returns them, which it already does since `eval_meta` returns `env.get_meta(spur)` contents).

Review `eval_meta` to confirm it already passes through all metadata keys — if it constructs the map manually, add the new keys.

**Step 6: Add (source-of name) as a convenience special form**

Add `source_of` to `SpecialFormSpurs`, `SpecialFormSpurs::init()`, `SPECIAL_FORM_NAMES`, and `try_eval_special()`. Implementation:

```rust
fn eval_source_of(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("source-of", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let source = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("source")).cloned())
        .unwrap_or_else(Value::nil);
    Ok(Trampoline::Value(source))
}
```

**Step 7: Run tests and verify they pass**

Run: `cargo test -p sema --test integration_test -- test_source_of`
Expected: PASS

**Step 8: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs crates/sema/tests/integration_test.rs
git commit -m "feat: source-of — store and retrieve source text, file, line metadata"
```

---

### Task 2: Directive comment parser in sema-reader

**Files:**
- Modify: `crates/sema-reader/src/reader.rs` (new public function)
- Modify: `crates/sema-reader/src/lib.rs` (re-export)
- Test: `crates/sema-reader/src/reader.rs` (unit tests in `#[cfg(test)]` mod)

**Context:** The lexer already captures `Token::Comment(String)` with the full text including leading `;`. The parser's `skip_trivia()` discards them. We add a new `read_many_with_directives()` that collects `;;@key value` comments and associates them with the next top-level form.

**Step 1: Define the Directive type and write failing tests**

Add to reader.rs tests:

```rust
#[test]
fn test_read_directives_basic() {
    let input = r#";;@since 1.8.0
;;@deprecated Use bar instead
(defn foo (x) x)"#;
    let (exprs, _, directives) = read_many_with_directives(input).unwrap();
    assert_eq!(exprs.len(), 1);
    assert_eq!(directives.len(), 1);
    assert_eq!(directives[0].0, 0); // associated with form index 0
    assert_eq!(directives[0].1.get("since").unwrap(), "1.8.0");
    assert_eq!(directives[0].1.get("deprecated").unwrap(), "Use bar instead");
}

#[test]
fn test_read_directives_multiple_forms() {
    let input = r#";;@since 1.0.0
(defn a () 1)

;;@since 2.0.0
;;@tags math
(defn b () 2)

(defn c () 3)"#;
    let (exprs, _, directives) = read_many_with_directives(input).unwrap();
    assert_eq!(exprs.len(), 3);
    assert_eq!(directives.len(), 2);
    assert_eq!(directives[0].0, 0);
    assert_eq!(directives[1].0, 1);
    assert_eq!(directives[1].1.get("tags").unwrap(), "math");
}

#[test]
fn test_read_directives_none() {
    let input = "(+ 1 2)";
    let (exprs, _, directives) = read_many_with_directives(input).unwrap();
    assert_eq!(exprs.len(), 1);
    assert!(directives.is_empty());
}
```

**Step 2: Run tests to verify they fail**

Run: `cargo test -p sema-reader -- test_read_directives`
Expected: FAIL — function doesn't exist

**Step 3: Implement `read_many_with_directives`**

Add to `reader.rs`:

```rust
/// Read all s-expressions, returning spans and directive comments.
///
/// Directive comments are lines starting with `;;@key value`.
/// Returns a vec of (form_index, BTreeMap<key, value>) pairs.
pub fn read_many_with_directives(
    input: &str,
) -> Result<(Vec<Value>, SpanMap, Vec<(usize, std::collections::BTreeMap<String, String>)>), SemaError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let mut exprs = Vec::new();
    let mut all_directives: Vec<(usize, std::collections::BTreeMap<String, String>)> = Vec::new();
    let mut pending_directives: std::collections::BTreeMap<String, String> = std::collections::BTreeMap::new();

    loop {
        // Manually handle trivia to capture directive comments
        while let Some(t) = parser.tokens.get(parser.pos) {
            match &t.token {
                Token::Comment(text) => {
                    if let Some(directive) = text.strip_prefix(";;@") {
                        let directive = directive.trim();
                        if let Some((key, value)) = directive.split_once(' ') {
                            pending_directives.insert(key.to_string(), value.trim().to_string());
                        } else {
                            pending_directives.insert(directive.to_string(), String::new());
                        }
                    }
                    parser.pos += 1;
                }
                Token::Newline => {
                    parser.pos += 1;
                }
                _ => break,
            }
        }
        if parser.peek().is_none() {
            break;
        }
        let form_index = exprs.len();
        exprs.push(parser.parse_expr()?);
        if !pending_directives.is_empty() {
            all_directives.push((form_index, std::mem::take(&mut pending_directives)));
        }
    }

    Ok((exprs, parser.span_map, all_directives))
}
```

**Step 4: Re-export from lib.rs**

Add to `crates/sema-reader/src/lib.rs`:
```rust
pub use reader::read_many_with_directives;
```

**Step 5: Run tests and verify they pass**

Run: `cargo test -p sema-reader -- test_read_directives`
Expected: PASS

**Step 6: Commit**

```bash
git add crates/sema-reader/src/reader.rs crates/sema-reader/src/lib.rs
git commit -m "feat: directive comment parser (;;@key value) in sema-reader"
```

---

### Task 3: `read-source` stdlib function

**Files:**
- Modify: `crates/sema-stdlib/src/io.rs` (add read-source function)
- Test: `crates/sema/tests/integration_test.rs`

**Context:** `read-source` is a stdlib function (not a special form) because it operates on files, not the calling env. It reads a `.sema` file, parses it with the directive-aware reader, and returns a list of maps describing each top-level definition.

**Step 1: Write failing tests**

Add to `integration_test.rs`:

```rust
#[test]
fn test_read_source_basic() {
    // Write a temp file, then read-source it
    let result = eval_to_string(
        r#"(begin
             (file/write "/tmp/sema-test-read-source.sema"
               "(defn greet \"Say hello.\" (name) (string-append \"Hello, \" name))\n(define x 42)")
             (let ((defs (read-source "/tmp/sema-test-read-source.sema")))
               (length defs)))"#,
    );
    assert_eq!(result, "2");
}

#[test]
fn test_read_source_defn_structure() {
    let result = eval_to_string(
        r#"(begin
             (file/write "/tmp/sema-test-rs2.sema"
               "(defn add \"Add two numbers.\" (a b) (+ a b))")
             (let ((defs (read-source "/tmp/sema-test-rs2.sema")))
               (let ((d (first defs)))
                 (list (get d :type) (get d :name) (get d :doc)))))"#,
    );
    assert_eq!(result, "(:defn \"add\" \"Add two numbers.\")");
}

#[test]
fn test_read_source_with_directives() {
    let result = eval_to_string(
        r#"(begin
             (file/write "/tmp/sema-test-rs3.sema"
               ";;@since 1.0.0\n;;@deprecated Use bar\n(defn foo (x) x)")
             (let ((defs (read-source "/tmp/sema-test-rs3.sema")))
               (let ((d (first defs)))
                 (get-in d [:directives :since]))))"#,
    );
    assert_eq!(result, "\"1.0.0\"");
}

#[test]
fn test_read_source_non_defn() {
    // Non-definition top-level forms should still appear with :type :expr
    let result = eval_to_string(
        r#"(begin
             (file/write "/tmp/sema-test-rs4.sema" "(println \"hello\")")
             (let ((defs (read-source "/tmp/sema-test-rs4.sema")))
               (get (first defs) :type)))"#,
    );
    assert_eq!(result, ":expr");
}
```

**Step 2: Run tests to verify they fail**

Run: `cargo test -p sema --test integration_test -- test_read_source`
Expected: FAIL — function doesn't exist

**Step 3: Implement `read-source` in io.rs**

Add to `crates/sema-stdlib/src/io.rs` inside `pub fn register(env, sandbox)`:

```rust
crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "read-source", &[0], |args| {
    check_arity!(args, "read-source", 1);
    let path_str = args[0]
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;

    let paths = if path_str.contains('*') {
        glob::glob(path_str)
            .map_err(|e| SemaError::eval(format!("read-source: invalid glob: {e}")))?
            .filter_map(|r| r.ok())
            .map(|p| p.to_string_lossy().to_string())
            .collect::<Vec<_>>()
    } else {
        vec![path_str.to_string()]
    };

    let mut all_defs = Vec::new();
    for path in &paths {
        let content = std::fs::read_to_string(path)
            .map_err(|e| SemaError::Io(format!("read-source {path}: {e}")))?;
        let (exprs, _spans, directives) = sema_reader::read_many_with_directives(&content)?;

        // Build directive lookup: form_index -> BTreeMap
        let mut dir_map: std::collections::HashMap<usize, &std::collections::BTreeMap<String, String>> =
            std::collections::HashMap::new();
        for (idx, dirs) in &directives {
            dir_map.insert(*idx, dirs);
        }

        // Track line numbers from source
        let lines: Vec<&str> = content.lines().collect();

        for (i, expr) in exprs.iter().enumerate() {
            let mut entry = std::collections::BTreeMap::new();
            let source_text = expr.to_string();

            // Determine the type and extract structure
            if let Some(list) = expr.as_list() {
                if let Some(head) = list.first().and_then(|v| v.as_symbol()) {
                    match head.as_str() {
                        "defn" | "defun" if list.len() >= 4 => {
                            let name = list.get(1).map(|v| v.to_string()).unwrap_or_default();
                            entry.insert(Value::keyword("type"), Value::keyword("defn"));
                            entry.insert(Value::keyword("name"), Value::string(&name));

                            // Check for docstring
                            let (doc, params_idx) = if list.get(2).and_then(|v| v.as_str()).is_some() {
                                (list[2].as_str().map(|s| s.to_string()), 3)
                            } else {
                                (None, 2)
                            };
                            if let Some(d) = &doc {
                                entry.insert(Value::keyword("doc"), Value::string(d));
                            }
                            if let Some(params) = list.get(params_idx) {
                                entry.insert(Value::keyword("params"), params.clone());
                            }
                            if list.len() > params_idx + 1 {
                                entry.insert(
                                    Value::keyword("body"),
                                    Value::list(list[params_idx + 1..].to_vec()),
                                );
                            }
                        }
                        "define" if list.len() >= 3 => {
                            // (define (f x) body) or (define x val)
                            if let Some(sig) = list.get(1).and_then(|v| v.as_list()) {
                                let name = sig.first().map(|v| v.to_string()).unwrap_or_default();
                                entry.insert(Value::keyword("type"), Value::keyword("defn"));
                                entry.insert(Value::keyword("name"), Value::string(&name));
                                entry.insert(Value::keyword("params"), Value::list(sig[1..].to_vec()));
                                // Check for docstring
                                let body_start = if list.get(2).and_then(|v| v.as_str()).is_some() {
                                    if let Some(d) = list[2].as_str() {
                                        entry.insert(Value::keyword("doc"), Value::string(d));
                                    }
                                    3
                                } else {
                                    2
                                };
                                entry.insert(
                                    Value::keyword("body"),
                                    Value::list(list[body_start..].to_vec()),
                                );
                            } else {
                                let name = list.get(1).map(|v| v.to_string()).unwrap_or_default();
                                entry.insert(Value::keyword("type"), Value::keyword("define"));
                                entry.insert(Value::keyword("name"), Value::string(&name));
                            }
                        }
                        "defmacro" if list.len() >= 4 => {
                            let name = list.get(1).map(|v| v.to_string()).unwrap_or_default();
                            entry.insert(Value::keyword("type"), Value::keyword("defmacro"));
                            entry.insert(Value::keyword("name"), Value::string(&name));
                            let (doc, params_idx) = if list.get(2).and_then(|v| v.as_str()).is_some() {
                                (list[2].as_str().map(|s| s.to_string()), 3)
                            } else {
                                (None, 2)
                            };
                            if let Some(d) = &doc {
                                entry.insert(Value::keyword("doc"), Value::string(d));
                            }
                            if let Some(params) = list.get(params_idx) {
                                entry.insert(Value::keyword("params"), params.clone());
                            }
                        }
                        _ => {
                            entry.insert(Value::keyword("type"), Value::keyword("expr"));
                        }
                    }
                } else {
                    entry.insert(Value::keyword("type"), Value::keyword("expr"));
                }
            } else {
                entry.insert(Value::keyword("type"), Value::keyword("expr"));
            }

            entry.insert(Value::keyword("source"), Value::string(&source_text));
            entry.insert(Value::keyword("file"), Value::string(path));

            // Try to find line number by matching source text in file
            // (approximate — find first line containing the start of the form)
            let source_start = source_text.chars().take(40).collect::<String>();
            let line_num = lines.iter().position(|l| l.contains(&source_start))
                .map(|n| (n + 1) as i64)
                .unwrap_or(0);
            if line_num > 0 {
                entry.insert(Value::keyword("line"), Value::int(line_num));
            }

            // Attach directives if any
            if let Some(dirs) = dir_map.get(&i) {
                let mut dir_entries = std::collections::BTreeMap::new();
                for (k, v) in *dirs {
                    dir_entries.insert(
                        Value::keyword(k),
                        Value::string(v),
                    );
                }
                entry.insert(Value::keyword("directives"), Value::map(dir_entries));
            }

            all_defs.push(Value::map(entry));
        }
    }

    Ok(Value::list(all_defs))
});
```

**Step 4: Run tests**

Run: `cargo test -p sema --test integration_test -- test_read_source`
Expected: PASS

**Step 5: Commit**

```bash
git add crates/sema-stdlib/src/io.rs crates/sema/tests/integration_test.rs
git commit -m "feat: read-source stdlib function with directive and glob support"
```

---

### Task 4: Helper functions — defs-in, undocumented-in

**Files:**
- Modify: `crates/sema-stdlib/src/io.rs`
- Test: `crates/sema/tests/integration_test.rs`

**Context:** These are simple wrappers around `read-source` but implemented as native fns for convenience.

**Step 1: Write failing tests**

```rust
#[test]
fn test_defs_in() {
    let result = eval_to_string(
        r#"(begin
             (file/write "/tmp/sema-test-defs.sema"
               "(defn a () 1)\n(defn b () 2)\n(define x 42)")
             (defs-in "/tmp/sema-test-defs.sema"))"#,
    );
    assert!(result.contains("a"));
    assert!(result.contains("b"));
    assert!(result.contains("x"));
}

#[test]
fn test_undocumented_in() {
    let result = eval_to_string(
        r#"(begin
             (file/write "/tmp/sema-test-undoc.sema"
               "(defn a \"Documented.\" () 1)\n(defn b () 2)")
             (undocumented-in "/tmp/sema-test-undoc.sema"))"#,
    );
    assert!(!result.contains("a"));
    assert!(result.contains("b"));
}
```

**Step 2: Implement**

These can be simple prelude macros in `crates/sema-eval/src/prelude.rs`, or native fns. Prelude is simplest:

```lisp
(define (defs-in path)
  (map (fn (d) (get d :name)) (read-source path)))

(define (undocumented-in path)
  (map (fn (d) (get d :name))
    (filter (fn (d) (nil? (get d :doc))) (read-source path))))
```

Alternatively, implement as native fns calling read-source internally. Prelude approach is recommended (dogfooding).

**Step 3: Run tests**

Run: `cargo test -p sema --test integration_test -- test_defs_in test_undocumented_in`

**Step 4: Commit**

```bash
git add crates/sema-eval/src/prelude.rs crates/sema/tests/integration_test.rs
git commit -m "feat: defs-in and undocumented-in prelude helpers"
```

---

### Task 5: Verification and cleanup

**Step 1: Run full test suite**

```bash
cargo test
```

**Step 2: Run lint**

```bash
make lint
```

**Step 3: Verify the example file works end-to-end**

Create/update `examples/read-source-demo.sema`:

```lisp
;; Demo: self-reading capabilities

;;@since 1.12.0
;;@tags introspection, demo
(defn greet
  "Greet someone by name.
   >>> (greet \"World\")
   \"Hello, World!\""
  (name)
  (string-append "Hello, " name "!"))

(defn add
  "Add two numbers.
   >>> (add 1 2)
   3"
  (a b)
  (+ a b))

;; Self-analysis
(println "=== Source introspection ===")
(println (source-of greet))

(println "\n=== Read-source analysis ===")
(let ((defs (read-source "examples/read-source-demo.sema")))
  (each (fn (d)
          (println f"${(get d :type)} ${(get d :name)}: ${(or (get d :doc) \"(no doc)\")}"))
        defs))
```

Run: `cargo run -- examples/read-source-demo.sema`

**Step 4: Final commit**

```bash
git add examples/read-source-demo.sema
git commit -m "feat: Living Code Phase 2 — self-reading capabilities"
```

---

## Dependency Graph

```
Task 1 (source-of) ──────┐
                          ├──→ Task 3 (read-source) ──→ Task 4 (helpers) ──→ Task 5 (verify)
Task 2 (directives) ─────┘
```

Tasks 1 and 2 are independent and can be done in parallel.
Task 3 depends on Task 2 (directive parser).
Task 4 depends on Task 3.
Task 5 depends on all.
