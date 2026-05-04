# Living Code Phase 5: `evolve` — LLM-Driven Genetic Programming

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement `(evolve ...)` — a special form that generates a population of candidate function implementations via LLM, tests them against a spec, and iteratively breeds/mutates the best ones to produce an optimized solution.

**Architecture:** `evolve` is a special form in `sema-eval` because it needs env/ctx for sandbox eval, LLM calls, and fitness callbacks. The evolution logic lives in a new `crates/sema-eval/src/evolve.rs` module. Spec can be either a quoted list of `(>>> expr expected)` forms (inline spec) or a symbol whose docstring contains doctests (symbol spec). The evolution loop is: seed population via LLM -> evaluate in sandbox -> compute fitness -> tournament select -> crossover/mutate via LLM -> repeat.

**Tech Stack:** Rust, sema-eval (special form + evolve module), sema-core (Env, Value), sema-reader (parse LLM responses). Tests: tree-walker only in `integration_test.rs`.

**Worktree:** `/Users/helge/code/sema-lisp/.worktrees/living-code` (branch `feature/living-code`)

---

## Conventions Reference

- Special forms: Spur field in `SpecialFormSpurs`, `intern()` in `init()`, string in `SPECIAL_FORM_NAMES`, dispatch in `try_eval_special()`, handler fn.
- `call_llm_complete(env, ctx, prompt, system)` — helper in special_forms.rs, looks up `llm/complete` from env.
- `sema_reader::read(code)` — parse string to Value. `sema_reader::read_many(code)` — parse to Vec<Value>.
- `eval::eval_value(ctx, &expr, &env)` — evaluate expression. `eval::eval_string(ctx, input, &env)` — parse+eval string.
- `Env::with_parent(Rc::new(env.clone()))` — sandbox child env.
- `sema_core::call_callback(ctx, &func, &[args])` — call a Sema function value from Rust.
- Keyword args in special forms: iterate raw `args` in pairs, check `as_keyword()` on odd positions, `eval::eval_value()` on even positions.
- Existing doctest runner: `crate::doctest::run_doctests(docstring, &mut eval_fn)` returns `Vec<DocTestResult>`.
- Code fence stripping pattern (from ask/code): strip ```` ```lisp ````, ```` ``` ```` prefixes/suffixes from LLM responses.
- Tests: `Interpreter::new()` + `interp.eval_str(...)`. LLM-dependent tests can't run in CI — test the non-LLM parts (spec parsing, config parsing, fitness computation).

---

### Task 1: Create `evolve.rs` module with config parsing

**Files:**
- Create: `.worktrees/living-code/crates/sema-eval/src/evolve.rs`
- Modify: `.worktrees/living-code/crates/sema-eval/src/lib.rs`

**Step 1: Create the module file with config and spec types**

Create `crates/sema-eval/src/evolve.rs`:

```rust
//! LLM-driven genetic programming: `(evolve ...)` implementation.

use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{call_callback, intern, resolve, Env, EvalContext, SemaError, Spur, Value};

use crate::eval;

/// Configuration parsed from `(evolve :name "..." :spec ... ...)` keyword args.
pub struct EvolveConfig {
    pub name: Spur,
    pub name_str: String,
    pub spec: Spec,
    pub population: usize,
    pub generations: usize,
    pub fitness_fn: Value,
    pub seed_prompt: String,
    pub verbose: bool,
    pub max_attempts: usize,
}

/// The specification: either inline `(>>> expr expected)` forms or a docstring reference.
pub enum Spec {
    /// Inline: list of (>>> expr expected) forms, stored as (input_source, expected_source) pairs.
    Inline(Vec<SpecCase>),
    /// Symbol reference: use the docstring's doctests.
    Docstring(String),
}

/// A single spec case from inline `(>>> expr expected)`.
pub struct SpecCase {
    pub input: String,
    pub expected: String,
}

/// A candidate in the population.
pub struct Candidate {
    pub source: String,
    pub fn_value: Option<Value>,
    pub fitness: f64,
    pub spec_passed: usize,
    pub spec_total: usize,
    pub time_ms: f64,
    pub error: Option<String>,
}

/// Parse keyword args from the special form into an EvolveConfig.
pub fn parse_config(
    args: &[Value],
    env: &Env,
    ctx: &EvalContext,
) -> Result<EvolveConfig, SemaError> {
    let mut name: Option<String> = None;
    let mut spec: Option<Spec> = None;
    let mut population: usize = 10;
    let mut generations: usize = 5;
    let mut fitness_fn: Option<Value> = None;
    let mut seed_prompt: Option<String> = None;
    let mut verbose = false;
    let mut max_attempts: usize = 3;

    let mut i = 0;
    while i < args.len() {
        let kw = args[i]
            .as_keyword()
            .ok_or_else(|| SemaError::eval(format!("evolve: expected keyword, got {}", args[i])))?;
        if i + 1 >= args.len() {
            return Err(SemaError::eval(format!("evolve: missing value for :{kw}")));
        }
        let val = eval::eval_value(ctx, &args[i + 1], env)?;

        match kw.as_str() {
            "name" => {
                name = Some(
                    val.as_str()
                        .ok_or_else(|| SemaError::type_error("string", val.type_name()))?
                        .to_string(),
                );
            }
            "spec" => {
                spec = Some(parse_spec(&val, env)?);
            }
            "population" => {
                population = val
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("integer", val.type_name()))?
                    as usize;
            }
            "generations" => {
                generations = val
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("integer", val.type_name()))?
                    as usize;
            }
            "fitness" => {
                fitness_fn = Some(val);
            }
            "seed-prompt" => {
                seed_prompt = Some(
                    val.as_str()
                        .ok_or_else(|| SemaError::type_error("string", val.type_name()))?
                        .to_string(),
                );
            }
            "verbose" => {
                verbose = val.is_truthy();
            }
            "max-attempts" => {
                max_attempts = val
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("integer", val.type_name()))?
                    as usize;
            }
            other => {
                return Err(SemaError::eval(format!("evolve: unknown option :{other}"))
                    .with_hint("Valid options: :name :spec :population :generations :fitness :seed-prompt :verbose :max-attempts"));
            }
        }
        i += 2;
    }

    let name_str = name.ok_or_else(|| SemaError::eval("evolve: :name is required"))?;
    let name_spur = intern(&name_str);

    Ok(EvolveConfig {
        name: name_spur,
        name_str,
        spec: spec.ok_or_else(|| SemaError::eval("evolve: :spec is required"))?,
        population,
        generations,
        fitness_fn: fitness_fn
            .ok_or_else(|| SemaError::eval("evolve: :fitness is required"))?,
        seed_prompt: seed_prompt
            .ok_or_else(|| SemaError::eval("evolve: :seed-prompt is required"))?,
        verbose,
        max_attempts,
    })
}

/// Parse the :spec value into a Spec enum.
fn parse_spec(val: &Value, env: &Env) -> Result<Spec, SemaError> {
    // If it's a list, treat as inline spec: ((>>> expr expected) ...)
    if let Some(items) = val.as_list() {
        let mut cases = Vec::new();
        for item in items {
            let form = item
                .as_list()
                .ok_or_else(|| SemaError::eval("evolve: each spec case must be a list"))?;
            if form.len() != 3 {
                return Err(SemaError::eval(
                    "evolve: spec case must be (>>> expr expected)",
                ));
            }
            let marker = form[0]
                .as_symbol()
                .ok_or_else(|| SemaError::eval("evolve: spec case must start with >>>"))?;
            if marker != ">>>" {
                return Err(SemaError::eval(format!(
                    "evolve: spec case must start with >>>, got {marker}"
                )));
            }
            cases.push(SpecCase {
                input: form[1].to_string(),
                expected: form[2].to_string(),
            });
        }
        if cases.is_empty() {
            return Err(SemaError::eval("evolve: :spec list is empty"));
        }
        Ok(Spec::Inline(cases))
    } else if val.as_lambda_rc().is_some() || val.as_native_fn_rc().is_some() {
        // It's a function — extract its docstring
        if let Some(lambda) = val.as_lambda_rc() {
            if let Some(name_spur) = lambda.name {
                if let Some(meta) = env.get_meta(name_spur) {
                    if let Some(doc) = meta.get(&Value::keyword("doc")) {
                        if let Some(s) = doc.as_str() {
                            let doctests = crate::doctest::parse_doctests(s);
                            if doctests.is_empty() {
                                return Err(SemaError::eval(
                                    "evolve: function has no doctests in its docstring",
                                )
                                .with_hint("Add >>> examples to the docstring"));
                            }
                            return Ok(Spec::Docstring(s.to_string()));
                        }
                    }
                }
                return Err(SemaError::eval(format!(
                    "evolve: function '{}' has no docstring",
                    resolve(name_spur)
                )));
            }
        }
        Err(SemaError::eval("evolve: :spec function must be a named function with doctests"))
    } else {
        Err(SemaError::type_error(
            "list of (>>> ...) forms or a function with doctests",
            val.type_name(),
        ))
    }
}

/// Run spec cases against a candidate function in a sandbox env.
/// Returns (passed, total, time_ms).
pub fn run_spec(
    config: &EvolveConfig,
    ctx: &EvalContext,
    sandbox_env: &Env,
) -> (usize, usize, f64) {
    match &config.spec {
        Spec::Inline(cases) => {
            let start = std::time::Instant::now();
            let mut passed = 0;
            let total = cases.len();
            for case in cases {
                match eval::eval_string(ctx, &case.input, sandbox_env) {
                    Ok(result) => {
                        let actual = result.to_string();
                        if actual.trim() == case.expected.trim() {
                            passed += 1;
                        }
                    }
                    Err(_) => {}
                }
            }
            let time_ms = start.elapsed().as_secs_f64() * 1000.0;
            (passed, total, time_ms)
        }
        Spec::Docstring(docstring) => {
            let start = std::time::Instant::now();
            let results = crate::doctest::run_doctests(docstring, &mut |input| {
                eval::eval_string(ctx, input, sandbox_env)
            });
            let time_ms = start.elapsed().as_secs_f64() * 1000.0;
            let passed = results.iter().filter(|r| r.passed).count();
            let total = results.len();
            (passed, total, time_ms)
        }
    }
}

/// Compute fitness by calling the user's fitness function.
pub fn compute_fitness(
    fitness_fn: &Value,
    candidate_fn: &Value,
    passed: usize,
    total: usize,
    time_ms: f64,
    ctx: &EvalContext,
) -> f64 {
    let mut spec_results = BTreeMap::new();
    spec_results.insert(Value::keyword("passed"), Value::int(passed as i64));
    spec_results.insert(Value::keyword("total"), Value::int(total as i64));
    spec_results.insert(Value::keyword("time-ms"), Value::float(time_ms));

    match call_callback(ctx, fitness_fn, &[candidate_fn.clone(), Value::map(spec_results)]) {
        Ok(val) => val.as_float().or_else(|| val.as_int().map(|i| i as f64)).unwrap_or(0.0),
        Err(_) => 0.0,
    }
}

/// Strip code fences from LLM response.
pub fn strip_code_fences(response: &str) -> &str {
    let trimmed = response.trim();
    let stripped = trimmed
        .strip_prefix("```lisp")
        .or_else(|| trimmed.strip_prefix("```sema"))
        .or_else(|| trimmed.strip_prefix("```"))
        .unwrap_or(trimmed);
    stripped.strip_suffix("```").unwrap_or(stripped).trim()
}
```

**Step 2: Register the module in lib.rs**

Add `pub mod evolve;` to `crates/sema-eval/src/lib.rs`.

**Step 3: Verify it compiles**

Run: `cargo build -p sema-eval`
Expected: compiles.

**Step 4: Commit**

```bash
git add crates/sema-eval/src/evolve.rs crates/sema-eval/src/lib.rs
git commit -m "feat: add evolve module with config parsing, spec runner, and fitness computation"
```

---

### Task 2: Write tests for config parsing and spec runner

**Files:**
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Add tests**

```rust
// ── evolve tests ─────────────────────────────────────────────────

#[test]
fn test_evolve_missing_name() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(evolve :spec '((>>> (+ 1 2) 3)) :fitness (fn (f r) 1) :seed-prompt "test")"#,
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains(":name"));
}

#[test]
fn test_evolve_missing_spec() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(evolve :name "test" :fitness (fn (f r) 1) :seed-prompt "test")"#,
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains(":spec"));
}

#[test]
fn test_evolve_missing_fitness() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(evolve :name "test" :spec '((>>> (+ 1 2) 3)) :seed-prompt "test")"#,
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains(":fitness"));
}

#[test]
fn test_evolve_missing_seed_prompt() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(evolve :name "test" :spec '((>>> (+ 1 2) 3)) :fitness (fn (f r) 1))"#,
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains(":seed-prompt"));
}

#[test]
fn test_evolve_unknown_option() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(evolve :name "t" :spec '((>>> 1 1)) :fitness (fn (f r) 1) :seed-prompt "t" :bogus 1)"#,
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("bogus"));
}

#[test]
fn test_evolve_spec_from_docstring() {
    // :spec can be a function with doctests
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (defn my-add "Adds.
               >>> (my-add 1 2)
               3" (a b) (+ a b))
            (evolve :name "my-add"
                    :spec my-add
                    :fitness (fn (f r) 1)
                    :seed-prompt "test"
                    :generations 0
                    :population 0))"#,
    );
    // With 0 generations and 0 population, it should return nil or the best (none)
    // At minimum it should NOT error on config parsing
    // It will error on "no candidates" which is fine — config parsed OK
    let is_config_error = result
        .as_ref()
        .err()
        .map(|e| {
            let s = e.to_string();
            s.contains(":name") || s.contains(":spec") || s.contains(":fitness") || s.contains(":seed-prompt")
        })
        .unwrap_or(false);
    assert!(!is_config_error, "Should not be a config parsing error");
}
```

**Step 2: Run tests (they will fail because `evolve` special form isn't wired yet)**

Run: `cargo test -p sema-lang --test integration_test -- test_evolve`
Expected: FAIL (evolve not recognized as special form)

**Step 3: Commit tests**

```bash
git add crates/sema/tests/integration_test.rs
git commit -m "test: add evolve config parsing tests"
```

---

### Task 3: Wire up `evolve` special form and implement the evolution loop

**Files:**
- Modify: `.worktrees/living-code/crates/sema-eval/src/special_forms.rs`

**Step 1: Add Spur and dispatch**

In `SpecialFormSpurs` struct, in the `// Runtime self-modification` section, add:

```rust
    evolve: Spur,
```

In `init()`, in the same section:

```rust
    evolve: intern("evolve"),
```

In `SPECIAL_FORM_NAMES`, in the same section:

```rust
    "evolve",
```

In `try_eval_special()`, add a dispatch branch before the `} else { None }`:

```rust
    } else if head_spur == sf.evolve {
        Some(eval_evolve(args, env, ctx))
```

**Step 2: Implement `eval_evolve`**

Add before the `parse_params` function:

```rust
/// (evolve :name "..." :spec ... :fitness fn :seed-prompt "..." ...)
/// LLM-driven genetic programming.
fn eval_evolve(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let config = crate::evolve::parse_config(args, env, ctx)?;

    let system_prompt = "You are writing Sema (a Lisp dialect). Write a function that satisfies \
                         the given specification. Return ONLY a (fn (params...) body...) expression. \
                         No markdown, no explanation, no code fences. Just the Sema expression.";

    let crossover_system = "You are writing Sema (a Lisp dialect). You are given two functions that \
                            solve the same problem. Create a new function that combines the best ideas \
                            from both. Return ONLY a (fn ...) expression. No markdown, no explanation.";

    let mutate_system = "You are writing Sema (a Lisp dialect). Modify this function to be \
                         faster or better while still passing all tests. Return ONLY a (fn ...) \
                         expression. No markdown, no explanation.";

    // Build spec description for prompts
    let spec_description = match &config.spec {
        crate::evolve::Spec::Inline(cases) => {
            let mut desc = String::new();
            for case in cases {
                desc.push_str(&format!("  ({}) should return {}\n", case.input, case.expected));
            }
            desc
        }
        crate::evolve::Spec::Docstring(ds) => {
            let mut desc = String::new();
            for test in crate::doctest::parse_doctests(ds) {
                match &test.expected {
                    crate::doctest::Expected::Value(v) => {
                        desc.push_str(&format!("  ({}) should return {}\n", test.input, v));
                    }
                    crate::doctest::Expected::Error(e) => {
                        desc.push_str(&format!("  ({}) should error with: {}\n", test.input, e));
                    }
                    crate::doctest::Expected::Skip => {
                        desc.push_str(&format!("  ({}) — setup step\n", test.input));
                    }
                }
            }
            desc
        }
    };

    let fn_name = &config.name_str;

    // Generation 0: seed population via LLM
    let mut population: Vec<crate::evolve::Candidate> = Vec::new();

    if config.verbose {
        eprintln!("evolve: seeding {} candidates for '{fn_name}'...", config.population);
    }

    for slot in 0..config.population {
        let prompt = format!(
            "{}\n\nThe function will be bound as '{fn_name}'. Specification:\n{spec_description}\n\
             Write a (fn ...) expression that satisfies all the above.",
            config.seed_prompt
        );
        let candidate = generate_candidate(
            env, ctx, &config, &prompt, system_prompt, slot,
        );
        population.push(candidate);
    }

    // Log generation 0
    if config.verbose {
        log_generation(0, &population);
    }

    // Evolution loop
    for gen in 1..=config.generations {
        let mut next_gen: Vec<crate::evolve::Candidate> = Vec::new();

        // Sort by fitness descending
        population.sort_by(|a, b| b.fitness.partial_cmp(&a.fitness).unwrap_or(std::cmp::Ordering::Equal));

        // Elite count: top 30%
        let elite_count = std::cmp::max(1, config.population * 30 / 100);
        for elite in population.iter().take(elite_count) {
            next_gen.push(crate::evolve::Candidate {
                source: elite.source.clone(),
                fn_value: elite.fn_value.clone(),
                fitness: elite.fitness,
                spec_passed: elite.spec_passed,
                spec_total: elite.spec_total,
                time_ms: elite.time_ms,
                error: elite.error.clone(),
            });
        }

        // Fill remaining slots with crossover/mutation
        let viable: Vec<&crate::evolve::Candidate> = population.iter().filter(|c| c.fitness > 0.0).collect();

        for slot in elite_count..config.population {
            let candidate = if viable.len() >= 2 && slot % 5 != 0 {
                // Crossover: pick two parents weighted by rank
                let parent_a = &viable[slot % viable.len()];
                let parent_b = &viable[(slot * 7 + 3) % viable.len()];

                let prompt = format!(
                    "Parent A (fitness {:.1}):\n{}\n\nParent B (fitness {:.1}):\n{}\n\n\
                     Specification:\n{spec_description}\n\
                     Create a new (fn ...) that combines the best ideas from both parents.",
                    parent_a.fitness, parent_a.source,
                    parent_b.fitness, parent_b.source,
                );
                generate_candidate(env, ctx, &config, &prompt, crossover_system, slot)
            } else if !viable.is_empty() {
                // Mutation: modify a single parent
                let parent = &viable[slot % viable.len()];
                let prompt = format!(
                    "Current function (fitness {:.1}):\n{}\n\n\
                     Specification:\n{spec_description}\n\
                     Modify this function to be faster or better.",
                    parent.fitness, parent.source,
                );
                generate_candidate(env, ctx, &config, &prompt, mutate_system, slot)
            } else {
                // No viable parents — reseed
                let prompt = format!(
                    "{}\n\nThe function will be bound as '{fn_name}'. Specification:\n{spec_description}\n\
                     Write a (fn ...) expression that satisfies all the above.",
                    config.seed_prompt
                );
                generate_candidate(env, ctx, &config, &prompt, system_prompt, slot)
            };
            next_gen.push(candidate);
        }

        population = next_gen;

        if config.verbose {
            log_generation(gen, &population);
        }
    }

    // Return the best candidate
    population.sort_by(|a, b| b.fitness.partial_cmp(&a.fitness).unwrap_or(std::cmp::Ordering::Equal));

    match population.first() {
        Some(best) if best.fn_value.is_some() && best.fitness > 0.0 => {
            let fn_val = best.fn_value.clone().unwrap();
            if config.verbose {
                eprintln!(
                    "evolve: winner for '{}' — fitness {:.1}, {}/{} spec pass, {:.2}ms",
                    fn_name, best.fitness, best.spec_passed, best.spec_total, best.time_ms
                );
            }
            Ok(Trampoline::Value(fn_val))
        }
        _ => Err(SemaError::eval(format!(
            "evolve: no viable candidate found for '{fn_name}' after {} generations",
            config.generations
        ))),
    }
}

/// Generate a single candidate: call LLM, parse, eval in sandbox, run spec, compute fitness.
/// Retries up to config.max_attempts on failure.
fn generate_candidate(
    env: &Env,
    ctx: &EvalContext,
    config: &crate::evolve::EvolveConfig,
    prompt: &str,
    system: &str,
    _slot: usize,
) -> crate::evolve::Candidate {
    for _attempt in 0..config.max_attempts {
        // Call LLM
        let response = match call_llm_complete(env, ctx, prompt, system) {
            Ok(v) => v,
            Err(e) => {
                return crate::evolve::Candidate {
                    source: String::new(),
                    fn_value: None,
                    fitness: 0.0,
                    spec_passed: 0,
                    spec_total: 0,
                    time_ms: 0.0,
                    error: Some(format!("LLM error: {e}")),
                };
            }
        };
        let response_str = match response.as_str() {
            Some(s) => s.to_string(),
            None => continue,
        };
        let code = crate::evolve::strip_code_fences(&response_str).to_string();

        // Parse
        let expr = match sema_reader::read(&code) {
            Ok(e) => e,
            Err(_) => continue,
        };

        // Eval in sandbox
        let sandbox_env = Env::with_parent(Rc::new(env.clone()));
        let fn_value = match eval::eval_value(ctx, &expr, &sandbox_env) {
            Ok(v) => v,
            Err(_) => continue,
        };

        // Bind the function by name so spec cases can call it
        sandbox_env.set(config.name, fn_value.clone());

        // Run spec
        let (passed, total, time_ms) = crate::evolve::run_spec(config, ctx, &sandbox_env);

        // Compute fitness
        let fitness = crate::evolve::compute_fitness(
            &config.fitness_fn,
            &fn_value,
            passed,
            total,
            time_ms,
            ctx,
        );

        return crate::evolve::Candidate {
            source: code,
            fn_value: Some(fn_value),
            fitness,
            spec_passed: passed,
            spec_total: total,
            time_ms,
            error: None,
        };
    }

    // Exhausted retries — zero-fitness sentinel
    crate::evolve::Candidate {
        source: String::new(),
        fn_value: None,
        fitness: 0.0,
        spec_passed: 0,
        spec_total: 0,
        time_ms: 0.0,
        error: Some("max attempts exhausted".to_string()),
    }
}

fn log_generation(gen: usize, population: &[crate::evolve::Candidate]) {
    let viable = population.iter().filter(|c| c.fitness > 0.0).count();
    let best = population
        .iter()
        .map(|c| c.fitness)
        .fold(0.0f64, f64::max);
    eprintln!(
        "  Generation {gen}: {} candidates, {viable} viable, best fitness: {best:.1}",
        population.len()
    );
}
```

**Step 3: Verify tests pass**

Run: `cargo test -p sema-lang --test integration_test -- test_evolve`
Expected: all 6 config parsing tests pass (they test error paths, no LLM needed).

**Step 4: Commit**

```bash
git add crates/sema-eval/src/special_forms.rs
git commit -m "feat: implement evolve special form — LLM-driven genetic programming"
```

---

### Task 4: Add non-LLM integration tests for evolve

The evolve loop requires an LLM, but we can test the spec runner and fitness computation directly by creating a mock scenario where we pre-define the function.

**Files:**
- Modify: `.worktrees/living-code/crates/sema/tests/integration_test.rs`

**Step 1: Add tests that exercise spec runner indirectly**

```rust
#[test]
fn test_evolve_inline_spec_bad_format() {
    let interp = Interpreter::new();
    // Spec cases must be (>>> expr expected)
    let result = interp.eval_str(
        r#"(evolve :name "t" :spec '((bad 1 2)) :fitness (fn (f r) 1) :seed-prompt "t")"#,
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains(">>>"));
}

#[test]
fn test_evolve_empty_spec() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(evolve :name "t" :spec '() :fitness (fn (f r) 1) :seed-prompt "t")"#,
    );
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("empty"));
}

#[test]
fn test_evolve_spec_fn_no_doctests() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
            (defn nodoc (x) x)
            (evolve :name "t" :spec nodoc :fitness (fn (f r) 1) :seed-prompt "t"))"#,
    );
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(err.contains("docstring") || err.contains("doctest"));
}
```

**Step 2: Run tests**

Run: `cargo test -p sema-lang --test integration_test -- test_evolve`
Expected: all 9 tests pass.

**Step 3: Commit**

```bash
git add crates/sema/tests/integration_test.rs
git commit -m "test: add evolve spec validation tests"
```

---

### Task 5: Final verification

**Step 1: Run all living-code tests**

Run: `cargo test -p sema-lang --test integration_test -- test_evolve test_observe test_become test_history test_rollback test_freeze test_bench`

**Step 2: Run full test suite**

Run: `cargo test`

**Step 3: Lint**

Run: `cargo clippy -- -D warnings`
Run: `cargo fmt --check`

**Step 4: Commit any fixups**

---

## Summary

| Component | Location | Purpose |
|-----------|----------|---------|
| `evolve.rs` | `sema-eval/src/evolve.rs` | Config parsing, spec runner, fitness computation, types |
| `eval_evolve` | `special_forms.rs` | Special form: evolution loop orchestration |
| `generate_candidate` | `special_forms.rs` | LLM call + parse + sandbox eval + spec + fitness |
| `log_generation` | `special_forms.rs` | Verbose output per generation |

`:spec` supports two modes:
- **Inline**: `'((>>> (f 1) 2) (>>> (f 0) 0))` — quoted list of test cases
- **Symbol**: `my-fn` — reads doctests from the function's docstring
