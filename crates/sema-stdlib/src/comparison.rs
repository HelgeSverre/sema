use sema_core::{SemaError, Value};

use crate::register_fn;

fn num_cmp(args: &[Value], op: &str, f: impl Fn(f64, f64) -> bool) -> Result<Value, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity(op, "2+", args.len()));
    }
    let to_f64 = |v: &Value| -> Result<f64, SemaError> {
        match v {
            Value::Int(n) => Ok(*n as f64),
            Value::Float(f) => Ok(*f),
            _ => Err(SemaError::type_error("number", v.type_name())),
        }
    };
    for pair in args.windows(2) {
        let a = to_f64(&pair[0])?;
        let b = to_f64(&pair[1])?;
        if !f(a, b) {
            return Ok(Value::Bool(false));
        }
    }
    Ok(Value::Bool(true))
}

pub fn register(env: &sema_core::Env) {
    register_fn(env, "<", |args| num_cmp(args, "<", |a, b| a < b));
    register_fn(env, ">", |args| num_cmp(args, ">", |a, b| a > b));
    register_fn(env, "<=", |args| num_cmp(args, "<=", |a, b| a <= b));
    register_fn(env, ">=", |args| num_cmp(args, ">=", |a, b| a >= b));

    register_fn(env, "=", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("=", "2+", args.len()));
        }
        for pair in args.windows(2) {
            match (&pair[0], &pair[1]) {
                (Value::Int(a), Value::Int(b)) => {
                    if a != b {
                        return Ok(Value::Bool(false));
                    }
                }
                (Value::Int(a), Value::Float(b)) | (Value::Float(b), Value::Int(a)) => {
                    if (*a as f64) != *b {
                        return Ok(Value::Bool(false));
                    }
                }
                (Value::Float(a), Value::Float(b)) => {
                    if a != b {
                        return Ok(Value::Bool(false));
                    }
                }
                (a, b) => {
                    if a != b {
                        return Ok(Value::Bool(false));
                    }
                }
            }
        }
        Ok(Value::Bool(true))
    });

    register_fn(env, "eq?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("eq?", "2", args.len()));
        }
        Ok(Value::Bool(args[0] == args[1]))
    });

    register_fn(env, "not", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("not", "1", args.len()));
        }
        Ok(Value::Bool(!args[0].is_truthy()))
    });

    register_fn(env, "zero?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("zero?", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(*n == 0)),
            Value::Float(f) => Ok(Value::Bool(*f == 0.0)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "positive?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("positive?", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(*n > 0)),
            Value::Float(f) => Ok(Value::Bool(*f > 0.0)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "negative?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("negative?", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(*n < 0)),
            Value::Float(f) => Ok(Value::Bool(*f < 0.0)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "even?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("even?", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(n % 2 == 0)),
            _ => Err(SemaError::type_error("int", args[0].type_name())),
        }
    });

    register_fn(env, "odd?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("odd?", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Bool(n % 2 != 0)),
            _ => Err(SemaError::type_error("int", args[0].type_name())),
        }
    });
}
