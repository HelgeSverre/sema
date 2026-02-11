use std::rc::Rc;

use sema_core::{Env, Lambda, SemaError, Value};

use crate::special_forms;

/// Trampoline for tail-call optimization.
pub enum Trampoline {
    Value(Value),
    Eval(Value, Env),
}

pub type EvalResult = Result<Value, SemaError>;

/// The interpreter holds the global environment and state.
pub struct Interpreter {
    pub global_env: Rc<Env>,
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Env::new();
        // Register stdlib
        sema_stdlib::register_stdlib(&env);
        // Register LLM builtins
        sema_llm::builtins::register_llm_builtins(&env);
        // Register the full evaluator callback for LLM tool execution
        sema_llm::builtins::set_eval_callback(eval_value);
        Interpreter {
            global_env: Rc::new(env),
        }
    }

    pub fn eval(&self, expr: &Value) -> EvalResult {
        let env = Env::with_parent(self.global_env.clone());
        eval_value(expr, &env)
    }

    pub fn eval_str(&self, input: &str) -> EvalResult {
        eval_string(input, &Env::with_parent(self.global_env.clone()))
    }
}

/// Evaluate a string containing one or more expressions.
pub fn eval_string(input: &str, env: &Env) -> EvalResult {
    let exprs = sema_reader::read_many(input)?;
    let mut result = Value::Nil;
    for expr in &exprs {
        result = eval_value(expr, env)?;
    }
    Ok(result)
}

/// The core eval function: evaluate a Value in an environment.
pub fn eval(expr: &Value, env: &Env) -> EvalResult {
    eval_value(expr, env)
}

/// Evaluate with trampoline for TCO.
pub fn eval_value(expr: &Value, env: &Env) -> EvalResult {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();

    loop {
        match eval_step(&current_expr, &current_env)? {
            Trampoline::Value(v) => return Ok(v),
            Trampoline::Eval(next_expr, next_env) => {
                current_expr = next_expr;
                current_env = next_env;
            }
        }
    }
}

fn eval_step(expr: &Value, env: &Env) -> Result<Trampoline, SemaError> {
    match expr {
        // Self-evaluating forms
        Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_) | Value::String(_) => {
            Ok(Trampoline::Value(expr.clone()))
        }
        Value::Keyword(_) => Ok(Trampoline::Value(expr.clone())),
        Value::Vector(items) => {
            let mut result = Vec::with_capacity(items.len());
            for item in items.iter() {
                result.push(eval_value(item, env)?);
            }
            Ok(Trampoline::Value(Value::vector(result)))
        }
        Value::Map(map) => {
            let mut result = std::collections::BTreeMap::new();
            for (k, v) in map.iter() {
                let ek = eval_value(k, env)?;
                let ev = eval_value(v, env)?;
                result.insert(ek, ev);
            }
            Ok(Trampoline::Value(Value::Map(Rc::new(result))))
        }

        // Symbol lookup
        Value::Symbol(name) => env
            .get(name)
            .map(Trampoline::Value)
            .ok_or_else(|| SemaError::Unbound(name.to_string())),

        // Function application / special forms
        Value::List(items) => {
            if items.is_empty() {
                return Ok(Trampoline::Value(Value::Nil));
            }

            let head = &items[0];
            let args = &items[1..];

            // Check for special forms (by symbol name)
            if let Value::Symbol(name) = head {
                if let Some(result) = special_forms::try_eval_special(name, args, env) {
                    return result;
                }
            }

            // Evaluate the head to get the callable
            let func = eval_value(head, env)?;

            match &func {
                Value::NativeFn(native) => {
                    // Evaluate arguments
                    let mut eval_args = Vec::with_capacity(args.len());
                    for arg in args {
                        eval_args.push(eval_value(arg, env)?);
                    }
                    let result = (native.func)(&eval_args)?;
                    Ok(Trampoline::Value(result))
                }
                Value::Lambda(lambda) => {
                    // Evaluate arguments
                    let mut eval_args = Vec::with_capacity(args.len());
                    for arg in args {
                        eval_args.push(eval_value(arg, env)?);
                    }
                    apply_lambda(lambda, &eval_args)
                }
                Value::Macro(mac) => {
                    // Macros receive unevaluated arguments
                    let expanded = apply_macro(mac, args, env)?;
                    // Evaluate the expansion in the current env (TCO)
                    Ok(Trampoline::Eval(expanded, env.clone()))
                }
                Value::Keyword(kw) => {
                    // Keywords as functions: (:key map) => (get map :key)
                    if args.len() != 1 {
                        return Err(SemaError::arity(
                            &format!(":{kw}"),
                            "1",
                            args.len(),
                        ));
                    }
                    let map_val = eval_value(&args[0], env)?;
                    match &map_val {
                        Value::Map(map) => {
                            let key = Value::Keyword(Rc::clone(kw));
                            Ok(Trampoline::Value(
                                map.get(&key).cloned().unwrap_or(Value::Nil),
                            ))
                        }
                        _ => Err(SemaError::type_error("map", map_val.type_name())),
                    }
                }
                other => Err(SemaError::eval(format!(
                    "not callable: {} ({})",
                    other,
                    other.type_name()
                ))),
            }
        }

        other => Ok(Trampoline::Value(other.clone())),
    }
}

/// Apply a lambda to evaluated arguments with TCO.
fn apply_lambda(lambda: &Lambda, args: &[Value]) -> Result<Trampoline, SemaError> {
    let new_env = Env::with_parent(Rc::new(lambda.env.clone()));

    // Bind parameters
    if let Some(ref rest) = lambda.rest_param {
        if args.len() < lambda.params.len() {
            return Err(SemaError::arity(
                lambda.name.as_deref().unwrap_or("lambda"),
                &format!("{}+", lambda.params.len()),
                args.len(),
            ));
        }
        for (param, arg) in lambda.params.iter().zip(args.iter()) {
            new_env.set(param.clone(), arg.clone());
        }
        let rest_args = args[lambda.params.len()..].to_vec();
        new_env.set(rest.clone(), Value::list(rest_args));
    } else {
        if args.len() != lambda.params.len() {
            return Err(SemaError::arity(
                lambda.name.as_deref().unwrap_or("lambda"),
                &lambda.params.len().to_string(),
                args.len(),
            ));
        }
        for (param, arg) in lambda.params.iter().zip(args.iter()) {
            new_env.set(param.clone(), arg.clone());
        }
    }

    // Self-reference for recursion
    if let Some(ref name) = lambda.name {
        new_env.set(
            name.clone(),
            Value::Lambda(Rc::new(Lambda {
                params: lambda.params.clone(),
                rest_param: lambda.rest_param.clone(),
                body: lambda.body.clone(),
                env: lambda.env.clone(),
                name: lambda.name.clone(),
            })),
        );
    }

    // Evaluate body with TCO on last expression
    if lambda.body.is_empty() {
        return Ok(Trampoline::Value(Value::Nil));
    }
    for expr in &lambda.body[..lambda.body.len() - 1] {
        eval_value(expr, &new_env)?;
    }
    Ok(Trampoline::Eval(
        lambda.body.last().unwrap().clone(),
        new_env,
    ))
}

/// Apply a macro: bind unevaluated args, evaluate body to produce expansion.
fn apply_macro(mac: &sema_core::Macro, args: &[Value], caller_env: &Env) -> Result<Value, SemaError> {
    let env = Env::with_parent(Rc::new(caller_env.clone()));

    // Bind parameters to unevaluated forms
    if let Some(ref rest) = mac.rest_param {
        if args.len() < mac.params.len() {
            return Err(SemaError::arity(
                &mac.name,
                &format!("{}+", mac.params.len()),
                args.len(),
            ));
        }
        for (param, arg) in mac.params.iter().zip(args.iter()) {
            env.set(param.clone(), arg.clone());
        }
        let rest_args = args[mac.params.len()..].to_vec();
        env.set(rest.clone(), Value::list(rest_args));
    } else {
        if args.len() != mac.params.len() {
            return Err(SemaError::arity(
                &mac.name,
                &mac.params.len().to_string(),
                args.len(),
            ));
        }
        for (param, arg) in mac.params.iter().zip(args.iter()) {
            env.set(param.clone(), arg.clone());
        }
    }

    // Evaluate the macro body to get the expansion
    let mut result = Value::Nil;
    for expr in &mac.body {
        result = eval_value(expr, &env)?;
    }
    Ok(result)
}
