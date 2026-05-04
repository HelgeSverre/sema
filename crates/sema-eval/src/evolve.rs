//! LLM-driven genetic programming: `(evolve ...)` implementation.

use std::collections::BTreeMap;

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
                let v = val
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("integer", val.type_name()))?;
                if v < 0 {
                    return Err(SemaError::eval("evolve: :population must be non-negative"));
                }
                population = v as usize;
            }
            "generations" => {
                let v = val
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("integer", val.type_name()))?;
                if v < 0 {
                    return Err(SemaError::eval("evolve: :generations must be non-negative"));
                }
                generations = v as usize;
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
                let v = val
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("integer", val.type_name()))?;
                if v < 1 {
                    return Err(SemaError::eval("evolve: :max-attempts must be at least 1"));
                }
                max_attempts = v as usize;
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
        fitness_fn: fitness_fn.ok_or_else(|| SemaError::eval("evolve: :fitness is required"))?,
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
        Err(SemaError::eval(
            "evolve: :spec function must be a named function with doctests",
        ))
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
                if let Ok(result) = eval::eval_string(ctx, &case.input, sandbox_env) {
                    let actual = result.to_string();
                    if actual.trim() == case.expected.trim() {
                        passed += 1;
                    }
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

    match call_callback(
        ctx,
        fitness_fn,
        &[candidate_fn.clone(), Value::map(spec_results)],
    ) {
        Ok(val) => val
            .as_float()
            .or_else(|| val.as_int().map(|i| i as f64))
            .unwrap_or(0.0),
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
