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

    // Bind pi and e as constants (bare symbol access)
    env.set_str("pi", Value::Float(std::f64::consts::PI));
    env.set_str("e", Value::Float(std::f64::consts::E));

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

    register_fn(env, "math/quotient", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/quotient", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        if b == 0 {
            return Err(SemaError::eval("math/quotient: division by zero"));
        }
        Ok(Value::Int(a / b))
    });

    register_fn(env, "math/remainder", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/remainder", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        if b == 0 {
            return Err(SemaError::eval("math/remainder: division by zero"));
        }
        Ok(Value::Int(a % b))
    });

    register_fn(env, "math/gcd", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/gcd", "2", args.len()));
        }
        let mut a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            .abs();
        let mut b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            .abs();
        while b != 0 {
            let t = b;
            b = a % b;
            a = t;
        }
        Ok(Value::Int(a))
    });

    register_fn(env, "math/lcm", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/lcm", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            .abs();
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            .abs();
        if a == 0 && b == 0 {
            return Ok(Value::Int(0));
        }
        let mut ga = a;
        let mut gb = b;
        while gb != 0 {
            let t = gb;
            gb = ga % gb;
            ga = t;
        }
        Ok(Value::Int(a / ga * b))
    });

    register_fn(env, "math/tan", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/tan", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.tan()))
    });

    register_fn(env, "math/asin", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/asin", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.asin()))
    });

    register_fn(env, "math/acos", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/acos", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.acos()))
    });

    register_fn(env, "math/atan", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/atan", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.atan()))
    });

    register_fn(env, "math/atan2", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/atan2", "2", args.len()));
        }
        let y = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        let x = args[1]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
        Ok(Value::Float(y.atan2(x)))
    });

    register_fn(env, "math/exp", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/exp", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.exp()))
    });

    register_fn(env, "math/log10", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/log10", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.log10()))
    });

    register_fn(env, "math/log2", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/log2", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::Float(f.log2()))
    });

    register_fn(env, "math/random", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("math/random", "0", args.len()));
        }
        Ok(Value::Float(rand::random::<f64>()))
    });

    register_fn(env, "math/random-int", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/random-int", "2", args.len()));
        }
        let lo = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let hi = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        use rand::RngExt;
        let val = rand::rng().random_range(lo..=hi);
        Ok(Value::Int(val))
    });

    register_fn(env, "math/clamp", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("math/clamp", "3", args.len()));
        }
        match (&args[0], &args[1], &args[2]) {
            (Value::Int(v), Value::Int(lo), Value::Int(hi)) => {
                Ok(Value::Int((*v).max(*lo).min(*hi)))
            }
            _ => {
                let v = args[0]
                    .as_float()
                    .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
                let lo = args[1]
                    .as_float()
                    .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
                let hi = args[2]
                    .as_float()
                    .ok_or_else(|| SemaError::type_error("number", args[2].type_name()))?;
                Ok(Value::Float(v.max(lo).min(hi)))
            }
        }
    });

    register_fn(env, "math/sign", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/sign", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::Int(if *n > 0 {
                1
            } else if *n < 0 {
                -1
            } else {
                0
            })),
            Value::Float(f) => Ok(Value::Int(if *f > 0.0 {
                1
            } else if *f < 0.0 {
                -1
            } else {
                0
            })),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
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
