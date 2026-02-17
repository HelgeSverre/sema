use sema_core::{check_arity, SemaError, Value, ValueView};

use crate::register_fn;

fn num_cmp(args: &[Value], op: &str, f: impl Fn(f64, f64) -> bool) -> Result<Value, SemaError> {
    check_arity!(args, op, 2..);
    let to_f64 = |v: &Value| -> Result<f64, SemaError> {
        match v.view() {
            ValueView::Int(n) => Ok(n as f64),
            ValueView::Float(f) => Ok(f),
            _ => Err(SemaError::type_error("number", v.type_name())),
        }
    };
    for pair in args.windows(2) {
        let a = to_f64(&pair[0])?;
        let b = to_f64(&pair[1])?;
        if !f(a, b) {
            return Ok(Value::bool(false));
        }
    }
    Ok(Value::bool(true))
}

pub fn register(env: &sema_core::Env) {
    register_fn(env, "<", |args| num_cmp(args, "<", |a, b| a < b));
    register_fn(env, ">", |args| num_cmp(args, ">", |a, b| a > b));
    register_fn(env, "<=", |args| num_cmp(args, "<=", |a, b| a <= b));
    register_fn(env, ">=", |args| num_cmp(args, ">=", |a, b| a >= b));

    register_fn(env, "=", |args| {
        check_arity!(args, "=", 2..);
        for pair in args.windows(2) {
            match (pair[0].view(), pair[1].view()) {
                (ValueView::Int(a), ValueView::Int(b)) => {
                    if a != b {
                        return Ok(Value::bool(false));
                    }
                }
                (ValueView::Int(a), ValueView::Float(b))
                | (ValueView::Float(b), ValueView::Int(a)) => {
                    if (a as f64) != b {
                        return Ok(Value::bool(false));
                    }
                }
                (ValueView::Float(a), ValueView::Float(b)) => {
                    if a != b {
                        return Ok(Value::bool(false));
                    }
                }
                _ => {
                    if pair[0] != pair[1] {
                        return Ok(Value::bool(false));
                    }
                }
            }
        }
        Ok(Value::bool(true))
    });

    register_fn(env, "eq?", |args| {
        check_arity!(args, "eq?", 2);
        Ok(Value::bool(args[0] == args[1]))
    });

    register_fn(env, "not", |args| {
        check_arity!(args, "not", 1);
        Ok(Value::bool(!args[0].is_truthy()))
    });

    register_fn(env, "zero?", |args| {
        check_arity!(args, "zero?", 1);
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::bool(n == 0)),
            ValueView::Float(f) => Ok(Value::bool(f == 0.0)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "positive?", |args| {
        check_arity!(args, "positive?", 1);
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::bool(n > 0)),
            ValueView::Float(f) => Ok(Value::bool(f > 0.0)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "negative?", |args| {
        check_arity!(args, "negative?", 1);
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::bool(n < 0)),
            ValueView::Float(f) => Ok(Value::bool(f < 0.0)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "even?", |args| {
        check_arity!(args, "even?", 1);
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::bool(n % 2 == 0)),
            _ => Err(SemaError::type_error("int", args[0].type_name())),
        }
    });

    register_fn(env, "odd?", |args| {
        check_arity!(args, "odd?", 1);
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::bool(n % 2 != 0)),
            _ => Err(SemaError::type_error("int", args[0].type_name())),
        }
    });
}
