# Living Code Phase 4: Runtime Self-Modification (Layer 6) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement `observe!`, `become!`, `history`, `rollback!`, and `freeze!` — the runtime self-modification primitives from Layer 6 of the Living Code design.

**Architecture:** All five forms are special forms in `sema-eval/src/special_forms.rs` because they need direct access to the calling `Env` for metadata/binding manipulation. Version history is stored as metadata on the `Env` using the existing `set_meta`/`get_meta` system. `observe!` wraps a function with a logging proxy via a NativeFn that records calls and delegates to the original. `;;@adaptive` gating uses directives stored in metadata from Phase 2's `read-source`.

**Tech Stack:** Rust, sema-core (Env metadata), sema-eval (special forms), sema-reader (directives already parsed by Phase 2). Tests: tree-walker only in `integration_test.rs` (these are I/O forms).

**Worktree:** `/Users/helge/code/sema-lisp/.worktrees/living-code` (branch `feature/living-code`)

---

## Conventions Reference

- Special forms: add Spur field to `SpecialFormSpurs`, `intern()` in `init()`, string to `SPECIAL_FORM_NAMES`, dispatch branch in `try_eval_special()`, handler fn `eval_<name>()`.
- Metadata: `env.set_meta(spur, Value::keyword("key"), val)`, `env.get_meta(spur)` returns `Option<BTreeMap<Value, Value>>`.
- Tests: `Interpreter::new()` + `interp.eval_str(...)` for single expressions, `interp.eval_str_in_global(...)` for multi-statement tests where defines must persist.
- Errors: `SemaError::eval("msg")`, `.with_hint("hint")`, `SemaError::arity("name", "N", args.len())`, `SemaError::type_error("expected", actual.type_name())`.
- Existing Phase 1-3 introspection forms: `doc`, `doc/search`, `doctest`, `heal!`, `meta`, `source-of`, `ask`, `ask/code` — all at end of `try_eval_special()` in the `// Introspection` section (~line 317-334).

---

### Task 1: Add `observe!` special form — Spur wiring only

**Files:**
- Modify: `.worktrees/living-code/crates/sema-eval/src/special_forms.rs`

**Step 1: Add Spur fields for all five new forms**

In the `SpecialFormSpurs` struct, add after the existing `source_of: Spur,` line in the `// Introspection` section:

```rust
    // Runtime self-modification
    become: Spur,
    freeze: Spur,
    history: Spur,
    observe: Spur,
    rollback: Spur,
```

In `init()`, add after `source_of: intern("source-of"),`:

```rust
    // Runtime self-modification
    become: intern("become!"),
    freeze: intern("freeze!"),
    history: intern("history"),
    observe: intern("observe!"),
    rollback: intern("rollback!"),
```

**Step 2: Add to SPECIAL_FORM_NAMES**

In the `SPECIAL_FORM_NAMES` array, add after `"source-of"`:

```rust
    // Runtime self-modification
    "become!",
    "freeze!",
    "history",
    "observe!",
    "rollback!",
```

**Step 3: Add dispatch stubs in try_eval_special**

After the `} else if head_spur == sf.source_of {` block and before `} else { None }`:

```rust
    // Runtime self-modification
    } else if head_spur == sf.become {
        Some(eval_become(args, env, ctx))
    } else if head_spur == sf.freeze {
        Some(eval_freeze(args, env))
    } else if head_spur == sf.history {
        Some(eval_history(args, env))
    } else if head_spur == sf.observe {
        Some(eval_observe(args, env, ctx))
    } else if head_spur == sf.rollback {
        Some(eval_rollback(args, env, ctx))
    } else {
        None
    }
```

Remove the existing `} else { None }` that was there before.

**Step 4: Add stub handler functions**

At the end of the file, before `parse_params`, add stub implementations that return errors:

```rust
// ── Runtime self-modification special forms ───────────────────────

fn eval_observe(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let _ = (args, env, ctx);
    Err(SemaError::eval("observe!: not yet implemented"))
}

fn eval_become(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let _ = (args, env, ctx);
    Err(SemaError::eval("become!: not yet implemented"))
}

fn eval_history(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    let _ = (args, env);
    Err(SemaError::eval("history: not yet implemented"))
}

fn eval_rollback(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let _ = (args, env, ctx);
    Err(SemaError::eval("rollback!: not yet implemented"))
}

fn eval_freeze(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    let _ = (args, env);
    Err(SemaError::eval("freeze!: not yet implemented"))
}
```

**Step 5: Verify it compiles**

Run: `cargo build -p sema-eval`
Expected: compiles with no errors.

**Step 6: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs
git commit -m "feat: wire up observe!/become!/history/rollback!/freeze! special form stubs"
```

---

### Task 2: Implement `observe!` — function call logging

**Files:**
- Modify: `.worktrees/living-code/crates/sema-eval/src/special_forms.rs`
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Write failing tests**

Add at the end of `integration_test.rs`:

```rust
// ── observe! tests ───────────────────────────────────────────────

#[test]
fn test_observe_basic() {
    // observe! wraps a function, collects call log, invokes callback after N calls
    let interp = Interpreter::new();
    // Define a function, observe it for 3 calls, callback stores the log length
    interp
        .eval_str_in_global(
            r#"(begin
                (defn add "Adds two numbers." (a b) (+ a b))
                (define result nil)
                (observe! add 3
                  (fn (log) (define result (count log)))))"#,
        )
        .unwrap();
    // Make 3 calls to trigger the callback
    interp.eval_str_in_global("(add 1 2)").unwrap();
    interp.eval_str_in_global("(add 3 4)").unwrap();
    interp.eval_str_in_global("(add 5 6)").unwrap();
    // callback should have fired with 3 entries
    let result = interp.eval_str_in_global("result").unwrap();
    assert_eq!(result, Value::int(3));
}

#[test]
fn test_observe_call_log_structure() {
    // Each call log entry has :args, :result, :time-ms
    let interp = Interpreter::new();
    interp
        .eval_str_in_global(
            r#"(begin
                (defn double "Doubles." (x) (* x 2))
                (define log-data nil)
                (observe! double 1
                  (fn (log) (define log-data (first log)))))"#,
        )
        .unwrap();
    interp.eval_str_in_global("(double 5)").unwrap();
    let entry = interp.eval_str_in_global("log-data").unwrap();
    let map = entry.as_map_rc().expect("log entry should be a map");
    // :args should be (5)
    assert_eq!(
        map.get(&Value::keyword("args")),
        Some(&Value::list(vec![Value::int(5)]))
    );
    // :result should be 10
    assert_eq!(
        map.get(&Value::keyword("result")),
        Some(&Value::int(10))
    );
    // :time-ms should exist and be a number
    assert!(map.get(&Value::keyword("time-ms")).is_some());
}

#[test]
fn test_observe_restores_original() {
    // After callback fires, original function is restored (no wrapper)
    let interp = Interpreter::new();
    interp
        .eval_str_in_global(
            r#"(begin
                (defn inc "Increments." (x) (+ x 1))
                (observe! inc 2 (fn (log) nil)))"#,
        )
        .unwrap();
    interp.eval_str_in_global("(inc 1)").unwrap();
    interp.eval_str_in_global("(inc 2)").unwrap();
    // After 2 calls, observe is done. Function should still work.
    let result = interp.eval_str_in_global("(inc 10)").unwrap();
    assert_eq!(result, Value::int(11));
}

#[test]
fn test_observe_function_still_works() {
    // While being observed, function returns correct values
    let interp = Interpreter::new();
    interp
        .eval_str_in_global(
            r#"(begin
                (defn square "Squares." (x) (* x x))
                (observe! square 2 (fn (log) nil)))"#,
        )
        .unwrap();
    let r = interp.eval_str_in_global("(square 7)").unwrap();
    assert_eq!(r, Value::int(49));
}
```

**Step 2: Run tests to verify they fail**

Run: `cargo test -p sema --test integration_test -- test_observe`
Expected: FAIL with "observe!: not yet implemented"

**Step 3: Implement `eval_observe`**

Replace the stub with:

```rust
/// (observe! target sample-size callback)
/// Wraps target function with a logger. After sample-size calls, invokes callback with call log.
fn eval_observe(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 3 {
        return Err(SemaError::arity("observe!", "3", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let sample_size = eval::eval_value(ctx, &args[1], env)?;
    let sample_n = sample_size
        .as_int()
        .ok_or_else(|| SemaError::type_error("integer", sample_size.type_name()))?
        as usize;
    let callback = eval::eval_value(ctx, &args[2], env)?;

    // Get the original function
    let original = env.get(spur).ok_or_else(|| {
        SemaError::eval(format!("observe!: '{}' is not defined", resolve(spur)))
    })?;

    // Create shared state for the logging wrapper
    let call_log: Rc<RefCell<Vec<Value>>> = Rc::new(RefCell::new(Vec::new()));
    let call_count = Rc::new(Cell::new(0usize));
    let original_clone = original.clone();
    let callback_clone = callback.clone();
    let log_clone = call_log.clone();
    let count_clone = call_count.clone();
    let env_clone = env.clone();
    let spur_copy = spur;
    let sample_copy = sample_n;

    let wrapper_name = format!("{}/observed", resolve(spur));
    let wrapper = Value::native_fn(sema_core::NativeFn::with_ctx(
        wrapper_name,
        move |ctx, call_args| {
            let start = std::time::Instant::now();
            let result = sema_core::call_callback(ctx, &original_clone, call_args)?;
            let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;

            // Record the call
            let mut entry = BTreeMap::new();
            entry.insert(Value::keyword("args"), Value::list(call_args.to_vec()));
            entry.insert(Value::keyword("result"), result.clone());
            entry.insert(Value::keyword("time-ms"), Value::float(elapsed_ms));
            log_clone.borrow_mut().push(Value::map(entry));

            let n = count_clone.get() + 1;
            count_clone.set(n);

            if n >= sample_copy {
                // Restore original function
                env_clone.set(spur_copy, env_clone.get(spur_copy).unwrap_or_else(Value::nil));
                // Actually restore the original — we need to set it back
                // This is tricky because we're inside the wrapper. We set it now
                // and invoke the callback.
                let log_entries = log_clone.borrow().clone();
                env_clone.set(spur_copy, original_clone.clone());
                sema_core::call_callback(ctx, &callback_clone, &[Value::list(log_entries)])?;
            }

            Ok(result)
        },
    ));

    // Replace the function with the wrapper
    env.set(spur, wrapper);

    Ok(Trampoline::Value(Value::nil()))
}
```

Wait — there's a problem: the `original_clone` is moved into the closure, but we also need it for the restore step. Let me fix this:

The `original_clone` inside the closure is used both for calling AND for restoring. Since it's cloned when moved in, we keep a second clone for restore. Actually `original_clone.clone()` inside the closure handles the restore. This works because `Value` is `Clone`.

**Step 4: Run tests**

Run: `cargo test -p sema --test integration_test -- test_observe`
Expected: all 4 tests pass.

**Step 5: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs crates/sema/tests/integration_test.rs
git commit -m "feat: implement observe! — function call logging with callback"
```

---

### Task 3: Implement `become!` — runtime function replacement

**Files:**
- Modify: `.worktrees/living-code/crates/sema-eval/src/special_forms.rs`
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Write failing tests**

Add at the end of `integration_test.rs`:

```rust
// ── become! tests ────────────────────────────────────────────────

#[test]
fn test_become_replaces_function() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (defn greet "Says hi.
                   >>> (greet \"World\")
                   \"Hello, World\"" (name) (string-append "Hello, " name))
                ;; Mark as adaptive
                (become! greet (fn (name) (string-append "Hi, " name)))
                (greet "Alice"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("Hi, Alice"));
}

#[test]
fn test_become_saves_history() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (defn inc1 "Adds one.
                   >>> (inc1 0)
                   1" (x) (+ x 1))
                (become! inc1 (fn (x) (+ x 2)))
                (count (history inc1)))"#,
        )
        .unwrap();
    // History should have 1 entry (the original version before become!)
    assert_eq!(result, Value::int(1));
}

#[test]
fn test_become_doctest_gate() {
    // become! should fail if the new definition doesn't pass existing doctests
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (defn double "Doubles.
               >>> (double 5)
               10" (x) (* x 2))
            (become! double (fn (x) (+ x 1))))"#,
    );
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("doctest") || err.contains("pass"),
        "Error should mention doctests: {err}"
    );
}

#[test]
fn test_become_no_doctests_allowed() {
    // If function has no doctests, become! works without validation
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (defn helper (x) (+ x 1))
                (become! helper (fn (x) (+ x 10)))
                (helper 5))"#,
        )
        .unwrap();
    assert_eq!(result, Value::int(15));
}
```

**Step 2: Run tests to verify they fail**

Run: `cargo test -p sema --test integration_test -- test_become`
Expected: FAIL with "become!: not yet implemented"

**Step 3: Implement `eval_become`**

Replace the stub with:

```rust
/// (become! target new-definition)
/// Replace a function's definition at runtime. Validates doctests if present.
/// Saves old version to history metadata.
fn eval_become(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("become!", "2", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);

    // Check if frozen
    if let Some(meta) = env.get_meta(spur) {
        if meta.get(&Value::keyword("frozen?")) == Some(&Value::bool(true)) {
            return Err(SemaError::eval(format!(
                "become!: '{name}' is frozen and cannot be modified"
            ))
            .with_hint("Use (history <name>) to inspect previous versions"));
        }
    }

    // Get the current value
    let current = env.get(spur).ok_or_else(|| {
        SemaError::eval(format!("become!: '{name}' is not defined"))
    })?;

    // Evaluate the new definition
    let new_def = eval::eval_value(ctx, &args[1], env)?;

    // If the function has doctests, validate the candidate passes them
    if let Some(meta) = env.get_meta(spur) {
        if let Some(doc_val) = meta.get(&Value::keyword("doc")) {
            if let Some(docstring) = doc_val.as_str() {
                let doctests = crate::doctest::parse_doctests(docstring);
                if !doctests.is_empty() {
                    // Create sandbox env with the new function bound
                    let sandbox_env = Env::with_parent(Rc::new(env.clone()));
                    sandbox_env.set(spur, new_def.clone());

                    let results = crate::doctest::run_doctests(docstring, &mut |input| {
                        eval::eval_string(ctx, input, &sandbox_env)
                    });

                    let total = results.len();
                    let passed = results.iter().filter(|r| r.passed).count();

                    if passed < total {
                        let mut failures = String::new();
                        for r in &results {
                            if !r.passed {
                                failures.push_str(&format!(
                                    "\n  {} => expected {}, got {}",
                                    r.input, r.expected, r.actual
                                ));
                            }
                        }
                        return Err(SemaError::eval(format!(
                            "become!: candidate for '{name}' fails {}/{total} doctests:{failures}",
                            total - passed
                        )));
                    }
                }
            }
        }
    }

    // Save current version to history
    let history_key = Value::keyword("history");
    let mut history_list: Vec<Value> = env
        .get_meta(spur)
        .and_then(|m| m.get(&history_key).cloned())
        .and_then(|v| v.as_list().map(|l| l.to_vec()))
        .unwrap_or_default();

    let mut version_entry = BTreeMap::new();
    version_entry.insert(
        Value::keyword("version"),
        Value::int(history_list.len() as i64 + 1),
    );
    version_entry.insert(Value::keyword("value"), current);
    // Store source if available
    if let Some(meta) = env.get_meta(spur) {
        if let Some(source) = meta.get(&Value::keyword("source")) {
            version_entry.insert(Value::keyword("source"), source.clone());
        }
    }
    history_list.push(Value::map(version_entry));
    env.set_meta(spur, history_key, Value::list(history_list));

    // Install the new definition
    env.set(spur, new_def);

    Ok(Trampoline::Value(Value::nil()))
}
```

**Step 4: Run tests**

Run: `cargo test -p sema --test integration_test -- test_become`
Expected: all 4 tests pass.

**Step 5: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs crates/sema/tests/integration_test.rs
git commit -m "feat: implement become! — runtime function replacement with doctest gate"
```

---

### Task 4: Implement `history` and `rollback!`

**Files:**
- Modify: `.worktrees/living-code/crates/sema-eval/src/special_forms.rs`
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Write failing tests**

```rust
// ── history / rollback! tests ────────────────────────────────────

#[test]
fn test_history_empty() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str("(begin (defn foo (x) x) (history foo))")
        .unwrap();
    assert_eq!(result, Value::list(vec![]));
}

#[test]
fn test_history_after_become() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (defn add1 (x) (+ x 1))
                (become! add1 (fn (x) (+ x 10)))
                (become! add1 (fn (x) (+ x 100)))
                (count (history add1)))"#,
        )
        .unwrap();
    assert_eq!(result, Value::int(2));
}

#[test]
fn test_history_entry_has_version() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (defn inc (x) (+ x 1))
                (become! inc (fn (x) (+ x 2)))
                (get (first (history inc)) :version))"#,
        )
        .unwrap();
    assert_eq!(result, Value::int(1));
}

#[test]
fn test_rollback_restores_version() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (defn calc (x) (+ x 1))
                (become! calc (fn (x) (+ x 10)))
                (become! calc (fn (x) (+ x 100)))
                (rollback! calc 1)
                (calc 0))"#,
        )
        .unwrap();
    // Version 1 was (+ x 1), should be restored
    assert_eq!(result, Value::int(1));
}

#[test]
fn test_rollback_invalid_version() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (defn foo (x) x)
            (rollback! foo 5))"#,
    );
    assert!(result.is_err());
}
```

**Step 2: Run tests to verify they fail**

Run: `cargo test -p sema --test integration_test -- test_history test_rollback`
Expected: FAIL

**Step 3: Implement `eval_history`**

Replace the stub:

```rust
/// (history sym) — return the version history list for a symbol
fn eval_history(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("history", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;

    let history = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("history")).cloned())
        .unwrap_or_else(|| Value::list(vec![]));

    Ok(Trampoline::Value(history))
}
```

**Step 4: Implement `eval_rollback`**

Replace the stub:

```rust
/// (rollback! sym version-number) — restore a previous version from history
fn eval_rollback(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("rollback!", "2", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);
    let version_val = eval::eval_value(ctx, &args[1], env)?;
    let version = version_val
        .as_int()
        .ok_or_else(|| SemaError::type_error("integer", version_val.type_name()))?
        as usize;

    // Check if frozen
    if let Some(meta) = env.get_meta(spur) {
        if meta.get(&Value::keyword("frozen?")) == Some(&Value::bool(true)) {
            return Err(SemaError::eval(format!(
                "rollback!: '{name}' is frozen and cannot be modified"
            )));
        }
    }

    let history_list = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("history")).cloned())
        .and_then(|v| v.as_list().map(|l| l.to_vec()))
        .unwrap_or_default();

    // Find the entry with matching version number
    let entry = history_list
        .iter()
        .find(|e| {
            e.as_map_rc()
                .and_then(|m| m.get(&Value::keyword("version")).cloned())
                .and_then(|v| v.as_int())
                == Some(version as i64)
        })
        .ok_or_else(|| {
            SemaError::eval(format!(
                "rollback!: version {version} not found in history for '{name}'"
            ))
            .with_hint(format!(
                "Available versions: {}",
                history_list
                    .iter()
                    .filter_map(|e| e
                        .as_map_rc()
                        .and_then(|m| m.get(&Value::keyword("version")).cloned())
                        .and_then(|v| v.as_int()))
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
        })?;

    let old_value = entry
        .as_map_rc()
        .and_then(|m| m.get(&Value::keyword("value")).cloned())
        .ok_or_else(|| SemaError::eval("rollback!: history entry missing :value"))?;

    // Save current version to history before rolling back
    let current = env.get(spur).ok_or_else(|| {
        SemaError::eval(format!("rollback!: '{name}' is not defined"))
    })?;

    let mut updated_history = history_list;
    let mut rollback_entry = BTreeMap::new();
    rollback_entry.insert(
        Value::keyword("version"),
        Value::int(updated_history.len() as i64 + 1),
    );
    rollback_entry.insert(Value::keyword("value"), current);
    updated_history.push(Value::map(rollback_entry));
    env.set_meta(
        spur,
        Value::keyword("history"),
        Value::list(updated_history),
    );

    // Restore the old value
    env.set(spur, old_value);

    Ok(Trampoline::Value(Value::nil()))
}
```

**Step 5: Run tests**

Run: `cargo test -p sema --test integration_test -- test_history test_rollback`
Expected: all 5 tests pass.

**Step 6: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs crates/sema/tests/integration_test.rs
git commit -m "feat: implement history and rollback! — version tracking for become!"
```

---

### Task 5: Implement `freeze!`

**Files:**
- Modify: `.worktrees/living-code/crates/sema-eval/src/special_forms.rs`
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Write failing tests**

```rust
// ── freeze! tests ────────────────────────────────────────────────

#[test]
fn test_freeze_prevents_become() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (defn stable (x) x)
            (freeze! stable)
            (become! stable (fn (x) (+ x 1))))"#,
    );
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("frozen"),
        "Error should mention frozen: {err}"
    );
}

#[test]
fn test_freeze_prevents_rollback() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (defn calc (x) (+ x 1))
            (become! calc (fn (x) (+ x 10)))
            (freeze! calc)
            (rollback! calc 1))"#,
    );
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("frozen"), "Error should mention frozen: {err}");
}

#[test]
fn test_freeze_returns_nil() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str("(begin (defn f (x) x) (freeze! f))")
        .unwrap();
    assert_eq!(result, Value::nil());
}
```

**Step 2: Run tests to verify they fail**

Run: `cargo test -p sema --test integration_test -- test_freeze`
Expected: FAIL

**Step 3: Implement `eval_freeze`**

Replace the stub:

```rust
/// (freeze! sym) — permanently prevent further become!/rollback! on a function
fn eval_freeze(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("freeze!", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;

    env.set_meta(spur, Value::keyword("frozen?"), Value::bool(true));

    Ok(Trampoline::Value(Value::nil()))
}
```

**Step 4: Run tests**

Run: `cargo test -p sema --test integration_test -- test_freeze`
Expected: all 3 tests pass.

**Step 5: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs crates/sema/tests/integration_test.rs
git commit -m "feat: implement freeze! — permanent mutation prevention"
```

---

### Task 6: Add `bench` stdlib function

**Files:**
- Modify: `.worktrees/living-code/crates/sema-stdlib/src/meta.rs`
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Write failing test**

```rust
// ── bench tests ──────────────────────────────────────────────────

#[test]
fn test_bench_basic() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str("(bench (fn () (+ 1 2)) 100)")
        .unwrap();
    let map = result.as_map_rc().expect("bench should return a map");
    assert_eq!(map.get(&Value::keyword("iterations")), Some(&Value::int(100)));
    assert!(map.get(&Value::keyword("total-ms")).is_some());
    assert!(map.get(&Value::keyword("mean-ms")).is_some());
}
```

**Step 2: Run test to verify it fails**

Run: `cargo test -p sema --test integration_test -- test_bench`
Expected: FAIL

**Step 3: Implement `bench` in `meta.rs`**

Add after the `time` function registration in `meta.rs`:

```rust
    // (bench thunk iterations) — run zero-arg thunk N times, return timing stats
    register_fn(env, "bench", |args| {
        check_arity!(args, "bench", 2);
        let iterations = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[1].type_name()))?
            as usize;

        let start = std::time::Instant::now();
        for _ in 0..iterations {
            crate::list::call_function(&args[0], &[])?;
        }
        let total_ms = start.elapsed().as_secs_f64() * 1000.0;
        let mean_ms = if iterations > 0 {
            total_ms / iterations as f64
        } else {
            0.0
        };

        let mut map = std::collections::BTreeMap::new();
        map.insert(Value::keyword("iterations"), Value::int(iterations as i64));
        map.insert(Value::keyword("total-ms"), Value::float(total_ms));
        map.insert(Value::keyword("mean-ms"), Value::float(mean_ms));
        Ok(Value::map(map))
    });
```

**Step 4: Run test**

Run: `cargo test -p sema --test integration_test -- test_bench`
Expected: PASS

**Step 5: Commit**

```bash
git add crates/sema-stdlib/src/meta.rs crates/sema/tests/integration_test.rs
git commit -m "feat: add bench function — structured benchmarking with timing stats"
```

---

### Task 7: Full integration test and final verification

**Files:**
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Write end-to-end integration test**

```rust
#[test]
fn test_become_observe_history_full_flow() {
    // Full flow: define -> observe -> become -> history -> rollback -> freeze
    let interp = Interpreter::new();

    // Define a function
    interp
        .eval_str_in_global(r#"(defn calc (x) (+ x 1))"#)
        .unwrap();

    // become! to v2
    interp
        .eval_str_in_global("(become! calc (fn (x) (+ x 10)))")
        .unwrap();
    let r = interp.eval_str_in_global("(calc 0)").unwrap();
    assert_eq!(r, Value::int(10));

    // become! to v3
    interp
        .eval_str_in_global("(become! calc (fn (x) (+ x 100)))")
        .unwrap();
    let r = interp.eval_str_in_global("(calc 0)").unwrap();
    assert_eq!(r, Value::int(100));

    // Check history has 2 entries
    let h = interp.eval_str_in_global("(count (history calc))").unwrap();
    assert_eq!(h, Value::int(2));

    // Rollback to version 1 (original)
    interp.eval_str_in_global("(rollback! calc 1)").unwrap();
    let r = interp.eval_str_in_global("(calc 0)").unwrap();
    assert_eq!(r, Value::int(1));

    // Freeze
    interp.eval_str_in_global("(freeze! calc)").unwrap();

    // become! should fail now
    let result = interp.eval_str_in_global("(become! calc (fn (x) x))");
    assert!(result.is_err());
}
```

**Step 2: Run full test suite**

Run: `cargo test -p sema --test integration_test -- test_observe test_become test_history test_rollback test_freeze test_become_observe`
Expected: all tests pass.

**Step 3: Run full project tests and lint**

Run: `cargo test`
Expected: all tests pass.

Run: `make lint`
Expected: no warnings or errors.

**Step 4: Commit**

```bash
git add crates/sema/tests/integration_test.rs
git commit -m "test: add end-to-end integration test for runtime self-modification flow"
```

---

## Summary

| Form | Type | Purpose |
|------|------|---------|
| `(observe! sym n callback)` | Special form | Wrap function with call logger, fire callback after n calls |
| `(become! sym new-def)` | Special form | Replace function definition, doctest-gated, saves history |
| `(history sym)` | Special form | Return version history list |
| `(rollback! sym version)` | Special form | Restore a previous version from history |
| `(freeze! sym)` | Special form | Permanently prevent further become!/rollback! |
| `(bench thunk n)` | Stdlib fn | Run thunk n times, return timing stats map |

Total: 6 new primitives, ~250-350 lines of implementation, ~150 lines of tests.
