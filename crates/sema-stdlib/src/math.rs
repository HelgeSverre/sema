use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "abs", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("abs", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(n.abs())),
            Value::Float(f) => Ok(Value::Float(f.abs())),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "min", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("min", "1+", 0));
        }
        let mut result = args[0].clone();
        for arg in &args[1..] {
            let cmp_result = num_lt(&result, arg)?;
            if !cmp_result {
                result = arg.clone();
            }
        }
        Ok(result)
    });

    register_fn(env, "max", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("max", "1+", 0));
        }
        let mut result = args[0].clone();
        for arg in &args[1..] {
            let cmp_result = num_lt(arg, &result)?;
            if !cmp_result {
                result = arg.clone();
            }
        }
        Ok(result)
    });

    register_fn(env, "floor", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("floor", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(f) => Ok(Value::Int(f.floor() as i64)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "ceil", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("ceil", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(f) => Ok(Value::Int(f.ceil() as i64)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "round", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("round", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(f) => Ok(Value::Int(f.round() as i64)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "sqrt", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("sqrt", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.sqrt()))
    });

    register_fn(env, "pow", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("pow", "2", args.len()));
        }
        match (&args[0], &args[1]) {
            (Value::Int(base), Value::Int(exp)) if *exp >= 0 => {
                Ok(Value::Int(base.pow(*exp as u32)))
            }
            _ => {
                let base = args[0]
                    .as_float()
                    .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
                let exp = args[1]
                    .as_float()
                    .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
                Ok(Value::Float(base.powf(exp)))
            }
        }
    });

    register_fn(env, "log", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("log", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.ln()))
    });

    register_fn(env, "sin", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("sin", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.sin()))
    });

    register_fn(env, "cos", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("cos", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.cos()))
    });

    register_fn(env, "pi", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("pi", "0", args.len()));
        }
        Ok(Value::Float(std::f64::consts::PI))
    });

    register_fn(env, "e", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("e", "0", args.len()));
        }
        Ok(Value::Float(std::f64::consts::E))
    });

    register_fn(env, "int", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("int", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(*n)),
            Value::Float(f) => Ok(Value::Int(*f as i64)),
            Value::String(s) => s
                .parse::<i64>()
                .map(Value::Int)
                .map_err(|_| SemaError::eval(format!("cannot convert '{s}' to int"))),
            _ => Err(SemaError::type_error(
                "number or string",
                args[0].type_name(),
            )),
        }
    });

    register_fn(env, "float", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("float", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Float(*n as f64)),
            Value::Float(f) => Ok(Value::Float(*f)),
            Value::String(s) => s
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| SemaError::eval(format!("cannot convert '{s}' to float"))),
            _ => Err(SemaError::type_error(
                "number or string",
                args[0].type_name(),
            )),
        }
    });
}

fn num_lt(a: &Value, b: &Value) -> Result<bool, SemaError> {
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => Ok(a < b),
        (Value::Float(a), Value::Float(b)) => Ok(a < b),
        (Value::Int(a), Value::Float(b)) => Ok((*a as f64) < *b),
        (Value::Float(a), Value::Int(b)) => Ok(*a < (*b as f64)),
        _ => Err(SemaError::type_error("number", a.type_name())),
    }
}
