# Living Code Phase 1: Docstrings & Doctests

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add docstring support to `defn`/`define`/`defmacro`, runtime introspection via `(doc)`/`(meta)`, and a doctest runner that extracts `>>>` examples from docstrings and executes them as tests.

**Architecture:** Docstrings are stored as binding-level metadata on the Env (a parallel `SpurMap<Spur, BTreeMap<Value, Value>>`) alongside the existing bindings map. The `defn`/`define`/`defmacro` special forms detect an optional string literal in docstring position and store it. A new `introspect.rs` stdlib module provides `(doc sym)`, `(meta sym)`, and `(doc/search query)`. A doctest parser extracts `>>>` examples from docstrings, and a runner evaluates them and compares results. CLI gets `sema test --doctests`.

**Tech Stack:** Rust, sema-core (Env), sema-eval (special forms), sema-stdlib (new introspect module), sema (CLI)

**Worktree:** `/Users/helge/code/sema-lisp/.worktrees/living-code` (branch: `feature/living-code`)

---

## Task 1: Add metadata storage to Env

**Files:**
- Modify: `crates/sema-core/src/value.rs` (Env struct + impl, lines ~1804–1915)

**Step 1: Add metadata field to Env struct**

Add a `metadata` field to `Env` alongside `bindings`. Same pattern: `Rc<RefCell<SpurMap<Spur, BTreeMap<Value, Value>>>>`.

```rust
// In the Env struct (value.rs ~line 1804):
pub struct Env {
    pub bindings: Rc<RefCell<SpurMap<Spur, Value>>>,
    pub metadata: Rc<RefCell<SpurMap<Spur, std::collections::BTreeMap<Value, Value>>>>,
    pub parent: Option<Rc<Env>>,
    pub version: Cell<u64>,
}
```

Update `Env::new()`, `Env::with_parent()`, and `Default` impl to initialize `metadata` as empty.

**Step 2: Add metadata accessors**

Add these methods to `impl Env`:

```rust
/// Set a metadata entry for a binding.
pub fn set_meta(&self, name: Spur, key: Value, val: Value) {
    let mut meta = self.metadata.borrow_mut();
    meta.entry(name).or_default().insert(key, val);
}

/// Set the full metadata map for a binding.
pub fn set_meta_map(&self, name: Spur, map: std::collections::BTreeMap<Value, Value>) {
    self.metadata.borrow_mut().insert(name, map);
}

/// Get metadata for a binding, searching parent scopes.
pub fn get_meta(&self, name: Spur) -> Option<std::collections::BTreeMap<Value, Value>> {
    if let Some(meta) = self.metadata.borrow().get(&name) {
        Some(meta.clone())
    } else if let Some(parent) = &self.parent {
        parent.get_meta(name)
    } else {
        None
    }
}
```

**Step 3: Run existing tests to verify nothing broke**

Run: `cargo test -p sema-core`
Expected: All 140 tests pass. The new field is additive, existing code doesn't touch it.

**Step 4: Commit**

```bash
git add crates/sema-core/src/value.rs
git commit -m "feat: add metadata storage to Env for docstrings"
```

---

## Task 2: Parse docstrings in `defn`/`defun`

**Files:**
- Modify: `crates/sema-eval/src/special_forms.rs` (eval_defun ~line 411, eval_define ~line 337)

**Step 1: Write failing test**

Add to `crates/sema/tests/integration_test.rs`:

```rust
#[test]
fn test_defn_docstring() {
    let interp = Interpreter::new();
    // defn with docstring: (defn name "doc" (params) body)
    interp.eval_str(r#"(defn greet "Says hello to someone." (name) (string-append "Hello, " name))"#).unwrap();
    let result = interp.eval_str(r#"(greet "World")"#).unwrap();
    assert_eq!(result, Value::string("Hello, World"));
    // Verify docstring is stored in metadata
    let result = interp.eval_str(r#"(meta greet)"#).unwrap();
    let meta_map = result.as_map_rc().expect("meta should return a map");
    let doc = meta_map.get(&Value::keyword("doc")).expect("should have :doc");
    assert_eq!(doc.as_str().unwrap(), "Says hello to someone.");
}

#[test]
fn test_defn_no_docstring() {
    // defn without docstring still works
    let interp = Interpreter::new();
    interp.eval_str("(defn add (a b) (+ a b))").unwrap();
    let result = interp.eval_str("(add 1 2)").unwrap();
    assert_eq!(result, Value::int(3));
}

#[test]
fn test_define_fn_docstring() {
    let interp = Interpreter::new();
    // (define (f x) "doc" body)
    interp.eval_str(r#"(define (double x) "Doubles a number." (* x 2))"#).unwrap();
    let result = interp.eval_str("(double 5)").unwrap();
    assert_eq!(result, Value::int(10));
}
```

**Step 2: Run test to verify it fails**

Run: `cargo test -p sema --test integration_test -- test_defn_docstring`
Expected: FAIL (either parse error or `meta` function doesn't exist yet)

**Step 3: Modify `eval_defun` to detect docstring**

In `eval_defun` (special_forms.rs ~line 412), detect when `args[1]` is a string (docstring) instead of a list (params):

```rust
fn eval_defun(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defun", "3+", args.len()));
    }
    let name_spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;

    // Detect optional docstring: (defn name "doc" (params) body...)
    let (docstring, params_idx) = if args[1].as_str().is_some() && args.len() >= 4 {
        (args[1].as_str().map(|s| s.to_string()), 2)
    } else {
        (None, 1)
    };

    let name = args[0].clone();
    let params = args[params_idx]
        .as_list_rc()
        .ok_or_else(|| SemaError::type_error("list", args[params_idx].type_name()))?;

    // Build (name params...) signature list
    let mut sig = vec![name];
    sig.extend(params.iter().cloned());
    // Build transformed args: [(name params...), body...]
    let mut define_args = vec![Value::list(sig)];
    define_args.extend_from_slice(&args[params_idx + 1..]);
    let result = eval_define(&define_args, env, ctx)?;

    // Store docstring as metadata if present
    if let Some(doc) = docstring {
        env.set_meta(name_spur, Value::keyword("doc"), Value::string(&doc));
    }

    Ok(result)
}
```

**Step 4: Modify `eval_define` for `(define (f x) "doc" body)` form**

In `eval_define`, in the `(define (f x) body...)` branch (~line 356), detect docstring:

```rust
// Inside the `else if let Some(sig) = args[0].as_list()` branch:
// After extracting name_spur and params, before building body:

// Detect optional docstring: (define (f x) "doc" body...)
let (docstring, body_start) = if args.len() > 2 && args[1].as_str().is_some() {
    (args[1].as_str().map(|s| s.to_string()), 2)
} else {
    (None, 1)
};
let body = args[body_start..].to_vec();
// ... rest of function ...
// After env.set(name_spur, lambda):
if let Some(doc) = docstring {
    env.set_meta(name_spur, Value::keyword("doc"), Value::string(&doc));
}
```

**Step 5: Run tests**

Run: `cargo test -p sema --test integration_test -- test_defn`
Expected: `test_defn_docstring` still fails (needs `meta` function), `test_defn_no_docstring` passes.

**Step 6: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs
git commit -m "feat: parse docstrings in defn/define special forms"
```

---

## Task 3: Parse docstrings in `defmacro`

**Files:**
- Modify: `crates/sema-eval/src/special_forms.rs` (eval_defmacro)

**Step 1: Add docstring detection to `eval_defmacro`**

Same pattern: if `args[1]` is a string and `args.len() >= 4`, treat it as a docstring, shift params to `args[2]`.

```rust
fn eval_defmacro(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defmacro", "3+", args.len()));
    }
    let name_spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::eval("defmacro: name must be a symbol"))?;

    // Detect optional docstring: (defmacro name "doc" (params) body...)
    let (docstring, params_idx) = if args[1].as_str().is_some() && args.len() >= 4 {
        (args[1].as_str().map(|s| s.to_string()), 2)
    } else {
        (None, 1)
    };

    let param_list = args[params_idx]
        .as_list()
        .ok_or_else(|| SemaError::eval("defmacro: params must be a list"))?;
    // ... rest unchanged, using params_idx + 1 for body start ...
    let body = args[params_idx + 1..].to_vec();

    // After env.set(name_spur, mac):
    if let Some(doc) = docstring {
        env.set_meta(name_spur, Value::keyword("doc"), Value::string(&doc));
    }
    // ...
}
```

**Step 2: Run tests**

Run: `cargo test -p sema-eval`
Expected: All pass.

**Step 3: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs
git commit -m "feat: parse docstrings in defmacro"
```

---

## Task 4: `(doc)` and `(meta)` stdlib functions

**Files:**
- Create: `crates/sema-stdlib/src/introspect.rs`
- Modify: `crates/sema-stdlib/src/lib.rs` (add `mod introspect` + register call)

**Step 1: Create `introspect.rs`**

```rust
use sema_core::{check_arity, intern, resolve, Env, SemaError, Value};
use crate::register_fn;

pub fn register(env: &Env) {
    // (meta sym) — returns metadata map for a symbol, or nil
    register_fn(env, "meta", |args| {
        check_arity!(args, "meta", 1);
        let spur = args[0]
            .as_symbol_spur()
            .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
        // Look up metadata from the environment
        // We need env access — this will need to be register_fn_ctx or use the callback
        Err(SemaError::eval("meta: needs env access — use register_fn_ctx"))
    });
}
```

Wait — `register_fn` doesn't have env access. We need `NativeFn::with_ctx()` which gets `(&EvalContext, &[Value])`. But the EvalContext doesn't carry the env. Let me check what pattern is used for functions that need env access.

**Actually:** The `meta` function needs access to the calling environment to look up metadata. This requires a **special form**, not a stdlib function. Looking at how `eval` special form works — it receives `(args, env, ctx)`.

**Revised approach:** Implement `meta` and `doc` as **special forms** in `sema-eval`, since they need access to the calling env to look up metadata. This is the same pattern as `eval`, `define`, etc.

**Step 1: Add `meta` and `doc` special forms**

In `special_forms.rs`, add to the `SpecialForms` struct:

```rust
meta: Spur,
doc: Spur,
```

Initialize them:
```rust
meta: intern("meta"),
doc: intern("doc"),
```

Add to `ALL_SPECIAL_FORMS`:
```rust
"meta",
"doc",
```

Add dispatch in `try_eval_special()`:
```rust
} else if head_spur == sf.meta {
    Some(eval_meta(args, env, ctx))
} else if head_spur == sf.doc {
    Some(eval_doc(args, env, ctx))
}
```

**Step 2: Implement `eval_meta`**

```rust
fn eval_meta(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("meta", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;

    // Build metadata map
    let mut result = std::collections::BTreeMap::new();

    // Add stored metadata (doc, etc.)
    if let Some(meta) = env.get_meta(spur) {
        result.extend(meta);
    }

    // Add name
    result.insert(Value::keyword("name"), Value::string(&resolve(spur)));

    // Add type and arity info from the value itself
    if let Some(val) = env.get(spur) {
        result.insert(Value::keyword("type"), Value::keyword(val.type_name()));
        if let Some(lambda) = val.as_lambda_rc() {
            let param_names: Vec<Value> = lambda.params.iter().map(|s| Value::symbol(&resolve(*s))).collect();
            result.insert(Value::keyword("params"), Value::list(param_names));
            let arity = lambda.params.len() as i64;
            result.insert(Value::keyword("arity"), Value::int(arity));
            if lambda.rest_param.is_some() {
                result.insert(Value::keyword("variadic?"), Value::bool(true));
            }
        }
    }

    if result.is_empty() {
        Ok(Trampoline::Value(Value::nil()))
    } else {
        let map: hashbrown::HashMap<Value, Value> = result.into_iter().collect();
        Ok(Trampoline::Value(Value::hashmap(map.into_iter().collect())))
    }
}
```

**Step 3: Implement `eval_doc`**

```rust
fn eval_doc(args: &[Value], env: &Env, _ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("doc", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);

    let mut output = String::new();

    // Function signature
    if let Some(val) = env.get(spur) {
        if let Some(lambda) = val.as_lambda_rc() {
            let params: Vec<String> = lambda.params.iter().map(|s| resolve(*s)).collect();
            let params_str = params.join(" ");
            if let Some(rest) = &lambda.rest_param {
                output.push_str(&format!("{name} : (fn {params_str} . {})\n", resolve(*rest)));
            } else {
                output.push_str(&format!("{name} : (fn {params_str})\n"));
            }
        } else if val.as_native_fn_rc().is_some() {
            output.push_str(&format!("{name} : <native function>\n"));
        } else if val.as_macro_rc().is_some() {
            output.push_str(&format!("{name} : <macro>\n"));
        }
    }

    // Docstring
    if let Some(meta) = env.get_meta(spur) {
        if let Some(doc) = meta.get(&Value::keyword("doc")) {
            if let Some(s) = doc.as_str() {
                output.push('\n');
                output.push_str(s);
                output.push('\n');
            }
        }
    }

    if output.is_empty() {
        output = format!("No documentation for '{name}'.\n");
    }

    // Print to stderr (like Python's help()) and return nil
    eprint!("{output}");
    Ok(Trampoline::Value(Value::nil()))
}
```

**Step 4: Run the tests from Task 2**

Run: `cargo test -p sema --test integration_test -- test_defn_docstring`
Expected: PASS

**Step 5: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs
git commit -m "feat: add (doc) and (meta) special forms for introspection"
```

---

## Task 5: `(doc/search)` stdlib function

**Files:**
- Create: `crates/sema-stdlib/src/introspect.rs`
- Modify: `crates/sema-stdlib/src/lib.rs`

**Step 1: Create introspect.rs with `doc/search`**

This one CAN be a stdlib function because it uses the EvalContext's callback to access the env indirectly. But actually, it also needs env access to search metadata. So it's better as a special form too.

**Alternative:** Make it a simple function that takes a list of metadata maps (produced by calling `meta` on all names). But that's clunky.

**Simplest approach:** Add `doc/search` as another special form alongside `doc` and `meta`.

```rust
// In special_forms.rs
fn eval_doc_search(args: &[Value], env: &Env, _ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("doc/search", "1", args.len()));
    }
    let query = args[0]
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?
        .to_lowercase();

    let mut results = Vec::new();
    for spur in env.all_names() {
        let name = resolve(spur);
        let name_lower = name.to_lowercase();
        let mut matches = name_lower.contains(&query);

        // Also search docstrings
        if !matches {
            if let Some(meta) = env.get_meta(spur) {
                if let Some(doc) = meta.get(&Value::keyword("doc")) {
                    if let Some(s) = doc.as_str() {
                        matches = s.to_lowercase().contains(&query);
                    }
                }
            }
        }

        if matches {
            let mut entry = std::collections::BTreeMap::new();
            entry.insert(Value::keyword("name"), Value::string(&name));
            if let Some(meta) = env.get_meta(spur) {
                if let Some(doc) = meta.get(&Value::keyword("doc")) {
                    entry.insert(Value::keyword("doc"), doc.clone());
                }
            }
            results.push(Value::hashmap(entry.into_iter().collect()));
        }
    }

    Ok(Trampoline::Value(Value::list(results)))
}
```

Add dispatch: `doc/search` uses a Spur for `"doc/search"`.

**Step 2: Write test**

```rust
#[test]
fn test_doc_search() {
    let interp = Interpreter::new();
    interp.eval_str(r#"(defn greet "Says hello." (name) name)"#).unwrap();
    interp.eval_str(r#"(defn farewell "Says goodbye." (name) name)"#).unwrap();
    let result = interp.eval_str(r#"(length (doc/search "greet"))"#).unwrap();
    assert_eq!(result, Value::int(1));
}
```

**Step 3: Run test, verify pass**

Run: `cargo test -p sema --test integration_test -- test_doc_search`

**Step 4: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs
git commit -m "feat: add (doc/search) for searching documented functions"
```

---

## Task 6: Doctest parser

**Files:**
- Create: `crates/sema-eval/src/doctest.rs`
- Modify: `crates/sema-eval/src/lib.rs` (add `pub mod doctest`)

**Step 1: Define doctest types and parser**

```rust
//! Doctest parser and runner.
//!
//! Extracts executable examples from docstrings:
//! - `>>>` lines are expressions to evaluate
//! - The next non-blank line is the expected result
//! - `!! substring` means expect an error containing substring
//! - `>>>!` means evaluate but don't check result (setup)

/// A single doctest example
#[derive(Debug, Clone)]
pub struct DocTest {
    /// The Sema expression to evaluate
    pub input: String,
    /// What to expect
    pub expected: Expected,
    /// Line number within the docstring (for error reporting)
    pub line: usize,
}

/// Expected result of a doctest
#[derive(Debug, Clone)]
pub enum Expected {
    /// Compare return value via equal? (the expected expression as a string)
    Value(String),
    /// Expect an error containing this substring
    Error(String),
    /// Don't check the result (setup step)
    Skip,
}

/// Parse doctests from a docstring.
pub fn parse_doctests(docstring: &str) -> Vec<DocTest> {
    let mut tests = Vec::new();
    let lines: Vec<&str> = docstring.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let trimmed = lines[i].trim();

        if let Some(expr) = trimmed.strip_prefix(">>>!") {
            // Setup-only: evaluate but don't check
            let expr = expr.trim().to_string();
            if !expr.is_empty() {
                tests.push(DocTest {
                    input: expr,
                    expected: Expected::Skip,
                    line: i + 1,
                });
            }
            i += 1;
        } else if let Some(expr) = trimmed.strip_prefix(">>>") {
            let expr = expr.trim().to_string();
            if expr.is_empty() {
                i += 1;
                continue;
            }

            // Look at next non-blank line for expected result
            i += 1;
            while i < lines.len() && lines[i].trim().is_empty() {
                i += 1;
            }

            if i < lines.len() {
                let expected_line = lines[i].trim();
                if let Some(err_match) = expected_line.strip_prefix("!!") {
                    tests.push(DocTest {
                        input: expr,
                        expected: Expected::Error(err_match.trim().to_string()),
                        line: i,
                    });
                } else if expected_line.starts_with(">>>") {
                    // No expected value, next line is another test
                    tests.push(DocTest {
                        input: expr,
                        expected: Expected::Skip,
                        line: i,
                    });
                    continue; // Don't increment i, re-process this line
                } else {
                    tests.push(DocTest {
                        input: expr,
                        expected: Expected::Value(expected_line.to_string()),
                        line: i + 1,
                    });
                    i += 1;
                }
            } else {
                // No expected line — treat as skip
                tests.push(DocTest {
                    input: expr,
                    expected: Expected::Skip,
                    line: i,
                });
            }
        } else {
            i += 1;
        }
    }

    tests
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic() {
        let doc = r#"Does something.

   >>> (+ 1 2)
   3

   >>> (string/trim "  hi  ")
   "hi""#;
        let tests = parse_doctests(doc);
        assert_eq!(tests.len(), 2);
        assert_eq!(tests[0].input, "(+ 1 2)");
        assert!(matches!(tests[0].expected, Expected::Value(ref s) if s == "3"));
        assert_eq!(tests[1].input, r#"(string/trim "  hi  ")"#);
    }

    #[test]
    fn test_parse_error_expected() {
        let doc = r#"Fails on bad input.

   >>> (/ 1 0)
   !! division by zero"#;
        let tests = parse_doctests(doc);
        assert_eq!(tests.len(), 1);
        assert!(matches!(tests[0].expected, Expected::Error(ref s) if s == "division by zero"));
    }

    #[test]
    fn test_parse_setup_step() {
        let doc = r#"Needs setup.

   >>>! (define x 42)
   >>> x
   42"#;
        let tests = parse_doctests(doc);
        assert_eq!(tests.len(), 2);
        assert!(matches!(tests[0].expected, Expected::Skip));
        assert_eq!(tests[1].input, "x");
    }

    #[test]
    fn test_parse_empty_docstring() {
        let tests = parse_doctests("Just a description, no examples.");
        assert!(tests.is_empty());
    }
}
```

**Step 2: Run unit tests**

Run: `cargo test -p sema-eval -- doctest`
Expected: All 4 tests pass.

**Step 3: Commit**

```bash
git add crates/sema-eval/src/doctest.rs crates/sema-eval/src/lib.rs
git commit -m "feat: doctest parser — extracts >>> examples from docstrings"
```

---

## Task 7: Doctest runner

**Files:**
- Modify: `crates/sema-eval/src/doctest.rs` (add runner)
- Modify: `crates/sema-eval/src/lib.rs` (expose runner via Interpreter)

**Step 1: Add runner function to doctest.rs**

```rust
use sema_core::{resolve, Value, Env, SemaError};

/// Result of running a single doctest
#[derive(Debug)]
pub struct DocTestResult {
    pub input: String,
    pub passed: bool,
    pub expected: String,
    pub actual: String,
    pub line: usize,
}

/// Run all doctests for a given symbol in an environment.
/// Returns a list of results.
/// `eval_fn` is a callback that evaluates a Sema expression string and returns the result.
pub fn run_doctests<F>(
    docstring: &str,
    eval_fn: &mut F,
) -> Vec<DocTestResult>
where
    F: FnMut(&str) -> Result<Value, SemaError>,
{
    let tests = parse_doctests(docstring);
    let mut results = Vec::new();

    for test in tests {
        match &test.expected {
            Expected::Skip => {
                // Run but don't check
                let _ = eval_fn(&test.input);
                results.push(DocTestResult {
                    input: test.input,
                    passed: true,
                    expected: "(skip)".to_string(),
                    actual: "(skip)".to_string(),
                    line: test.line,
                });
            }
            Expected::Value(expected_str) => {
                match eval_fn(&test.input) {
                    Ok(actual) => {
                        // Parse the expected string as a Sema value for comparison
                        let actual_str = format!("{actual}");
                        let passed = actual_str.trim() == expected_str.trim();
                        results.push(DocTestResult {
                            input: test.input,
                            passed,
                            expected: expected_str.clone(),
                            actual: actual_str,
                            line: test.line,
                        });
                    }
                    Err(e) => {
                        results.push(DocTestResult {
                            input: test.input,
                            passed: false,
                            expected: expected_str.clone(),
                            actual: format!("ERROR: {e}"),
                            line: test.line,
                        });
                    }
                }
            }
            Expected::Error(expected_substring) => {
                match eval_fn(&test.input) {
                    Ok(val) => {
                        results.push(DocTestResult {
                            input: test.input,
                            passed: false,
                            expected: format!("!! {expected_substring}"),
                            actual: format!("{val}"),
                            line: test.line,
                        });
                    }
                    Err(e) => {
                        let err_str = e.to_string().to_lowercase();
                        let expected_lower = expected_substring.to_lowercase();
                        let passed = err_str.contains(&expected_lower);
                        results.push(DocTestResult {
                            input: test.input,
                            passed,
                            expected: format!("!! {expected_substring}"),
                            actual: format!("!! {e}"),
                            line: test.line,
                        });
                    }
                }
            }
        }
    }

    results
}
```

**Step 2: Add `run_doctests_for_symbol` to Interpreter**

In `crates/sema-eval/src/lib.rs`, add a method to `Interpreter` (or expose a public helper) that:
1. Looks up a symbol's metadata for `:doc`
2. Creates a child env
3. Runs doctests in that child env using `eval_str`

Check the existing `Interpreter` API first to determine the right pattern.

**Step 3: Write integration test**

```rust
#[test]
fn test_doctest_runner_basic() {
    let interp = Interpreter::new();
    interp.eval_str(r#"
        (defn double
          "Doubles a number.

           >>> (double 5)
           10

           >>> (double 0)
           0"
          (n) (* n 2))
    "#).unwrap();
    // Run doctests via eval
    let result = interp.eval_str(r#"(doctest double)"#).unwrap();
    // Should return a map with :passed and :total
    let map = result.as_map_rc().unwrap();
    assert_eq!(map.get(&Value::keyword("passed")), Some(&Value::int(2)));
    assert_eq!(map.get(&Value::keyword("total")), Some(&Value::int(2)));
}
```

**Step 4: Implement `(doctest sym)` special form**

Add to special_forms.rs:

```rust
fn eval_doctest(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("doctest", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);

    // Get docstring from metadata
    let docstring = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("doc")).cloned())
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .ok_or_else(|| SemaError::eval(format!("doctest: no docstring for '{name}'")))?;

    // Create child env for isolated evaluation
    let child_env = Env::with_parent(Rc::new(env.clone()));

    // Run doctests
    let results = crate::doctest::run_doctests(&docstring, &mut |input| {
        eval::eval_str_in_env(ctx, input, &child_env)
    });

    let total = results.len() as i64;
    let passed = results.iter().filter(|r| r.passed).count() as i64;

    // Print results
    for r in &results {
        if r.passed {
            eprintln!("  ✓ {} => {}", r.input, r.expected);
        } else {
            eprintln!("  ✗ {} => expected {}, got {}", r.input, r.expected, r.actual);
        }
    }

    // Return summary map
    let mut map = std::collections::BTreeMap::new();
    map.insert(Value::keyword("passed"), Value::int(passed));
    map.insert(Value::keyword("total"), Value::int(total));
    map.insert(Value::keyword("name"), Value::string(&name));
    Ok(Trampoline::Value(Value::hashmap(map.into_iter().collect())))
}
```

Note: `eval::eval_str_in_env` may not exist yet. Check `crates/sema-eval/src/lib.rs` for the right helper — likely `Interpreter::eval_str` internally calls something usable. The runner may need to be wired through the Interpreter's eval method. Adapt as needed.

**Step 5: Run tests**

Run: `cargo test -p sema --test integration_test -- test_doctest_runner`
Expected: PASS

**Step 6: Commit**

```bash
git add crates/sema-eval/src/doctest.rs crates/sema-eval/src/special_forms.rs crates/sema-eval/src/lib.rs
git commit -m "feat: doctest runner — (doctest sym) executes examples from docstrings"
```

---

## Task 8: `sema test --doctests` CLI

**Files:**
- Modify: `crates/sema/src/main.rs` (add --doctests flag to test/run command)

**Step 1: Add CLI flag**

Check if there's already a `test` subcommand. If not, add a `--doctests` flag to the main CLI or create a `Test` subcommand.

Add a `Test` subcommand:

```rust
/// Run doctests from source files
Test {
    /// Source files to scan for doctests
    files: Vec<String>,
    /// Verbose output
    #[arg(short, long)]
    verbose: bool,
},
```

**Step 2: Implement the test command**

The command should:
1. Load each file with the interpreter
2. After loading, iterate all symbols in the env that have `:doc` metadata
3. For each, extract doctests and run them
4. Print results and exit with appropriate code

```rust
Commands::Test { files, verbose } => {
    let interp = Interpreter::new();
    let mut total_passed = 0;
    let mut total_tests = 0;
    let mut any_failed = false;

    for file in &files {
        // Load the file (this defines all functions)
        if let Err(e) = interp.eval_file(file) {
            eprintln!("Error loading {file}: {e}");
            any_failed = true;
            continue;
        }

        // Find all symbols with docstrings and run their doctests
        // Use eval to call (doctest sym) for each documented symbol
        // ... implementation details depend on Interpreter API
    }

    if any_failed {
        std::process::exit(1);
    }
}
```

**Step 3: Write a sample .sema file for manual testing**

Create `examples/doctest-demo.sema`:

```lisp
(defn factorial
  "Computes factorial of n.

   >>> (factorial 0)
   1

   >>> (factorial 5)
   120

   >>> (factorial 1)
   1"
  (n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(defn fibonacci
  "Returns the nth Fibonacci number.

   >>> (fibonacci 0)
   0

   >>> (fibonacci 1)
   1

   >>> (fibonacci 10)
   55"
  (n)
  (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
```

**Step 4: Manual test**

Run: `cargo run -- test examples/doctest-demo.sema`
Expected output:
```
factorial:
  ✓ (factorial 0) => 1
  ✓ (factorial 5) => 120
  ✓ (factorial 1) => 1
fibonacci:
  ✓ (fibonacci 0) => 0
  ✓ (fibonacci 1) => 1
  ✓ (fibonacci 10) => 55
6/6 doctests passed
```

**Step 5: Commit**

```bash
git add crates/sema/src/main.rs examples/doctest-demo.sema
git commit -m "feat: sema test --doctests CLI command"
```

---

## Task 9: Dual-eval tests for doc/meta

**Files:**
- Modify: `crates/sema/tests/integration_test.rs` (tree-walker only tests for doc/meta/doctest — these are I/O-producing special forms, so tree-walker only per AGENTS.md)

**Step 1: Write comprehensive integration tests**

```rust
#[test]
fn test_meta_returns_map() {
    let interp = Interpreter::new();
    interp.eval_str(r#"(defn foo "A foo fn." (x) x)"#).unwrap();
    let meta = interp.eval_str("(meta foo)").unwrap();
    let map = meta.as_map_rc().expect("meta returns a map");
    assert_eq!(map.get(&Value::keyword("name")), Some(&Value::string("foo")));
    assert_eq!(map.get(&Value::keyword("arity")), Some(&Value::int(1)));
    assert!(map.get(&Value::keyword("doc")).is_some());
}

#[test]
fn test_meta_no_doc() {
    let interp = Interpreter::new();
    interp.eval_str("(defn bar (x) x)").unwrap();
    let meta = interp.eval_str("(meta bar)").unwrap();
    let map = meta.as_map_rc().expect("meta returns a map");
    // Should have name and params but no :doc
    assert_eq!(map.get(&Value::keyword("name")), Some(&Value::string("bar")));
    assert!(map.get(&Value::keyword("doc")).is_none());
}

#[test]
fn test_meta_undefined_returns_nil() {
    let interp = Interpreter::new();
    let result = interp.eval_str("(meta nonexistent)").unwrap();
    // Should return a map with just :name, or nil — depends on impl
    // At minimum it should not crash
    assert!(result.as_map_rc().is_some() || result.is_nil());
}

#[test]
fn test_defmacro_docstring() {
    let interp = Interpreter::new();
    interp.eval_str(r#"(defmacro my-when "Conditional execution." (test . body) `(if ,test (begin ,@body)))"#).unwrap();
    let meta = interp.eval_str("(meta my-when)").unwrap();
    let map = meta.as_map_rc().expect("meta returns a map");
    let doc = map.get(&Value::keyword("doc")).unwrap();
    assert_eq!(doc.as_str().unwrap(), "Conditional execution.");
}

#[test]
fn test_doctest_failing() {
    let interp = Interpreter::new();
    interp.eval_str(r#"
        (defn broken
          "Always returns wrong answer.

           >>> (broken 5)
           99"
          (n) n)
    "#).unwrap();
    let result = interp.eval_str("(doctest broken)").unwrap();
    let map = result.as_map_rc().unwrap();
    assert_eq!(map.get(&Value::keyword("passed")), Some(&Value::int(0)));
    assert_eq!(map.get(&Value::keyword("total")), Some(&Value::int(1)));
}

#[test]
fn test_doctest_error_expected() {
    let interp = Interpreter::new();
    interp.eval_str(r#"
        (defn safe-div
          "Divides safely.

           >>> (safe-div 10 2)
           5

           >>> (safe-div 1 0)
           !! division"
          (a b)
          (if (= b 0) (error "division by zero") (/ a b)))
    "#).unwrap();
    let result = interp.eval_str("(doctest safe-div)").unwrap();
    let map = result.as_map_rc().unwrap();
    assert_eq!(map.get(&Value::keyword("passed")), Some(&Value::int(2)));
}
```

**Step 2: Run all integration tests**

Run: `cargo test -p sema --test integration_test`
Expected: All new + existing tests pass.

**Step 3: Run full test suite**

Run: `cargo test --workspace`
Expected: All 1479+ tests pass.

**Step 4: Commit**

```bash
git add crates/sema/tests/integration_test.rs
git commit -m "test: comprehensive integration tests for doc/meta/doctest"
```

---

## Verification Checklist

Before considering Phase 1 complete:

- [ ] `cargo test --workspace` — all tests pass
- [ ] `cargo clippy --workspace -- -D warnings` — no warnings
- [ ] `cargo fmt --all --check` — formatted
- [ ] `(defn name "doc" (params) body)` stores docstring
- [ ] `(define (name params) "doc" body)` stores docstring
- [ ] `(defmacro name "doc" (params) body)` stores docstring
- [ ] `(meta sym)` returns map with `:name`, `:doc`, `:params`, `:arity`, `:type`
- [ ] `(doc sym)` prints formatted documentation
- [ ] `(doc/search "query")` searches names and docstrings
- [ ] `(doctest sym)` runs `>>>` examples and reports results
- [ ] `sema test file.sema` runs all doctests in a file
- [ ] Existing code without docstrings works exactly as before
