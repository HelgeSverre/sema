use sema_core::{SemaError, Value, ValueView};

use crate::register_fn;

fn pow_impl(args: &[Value]) -> Result<Value, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("pow", "2", args.len()));
    }
    match (args[0].view(), args[1].view()) {
        (ValueView::Int(base), ValueView::Int(exp)) if exp >= 0 => {
            Ok(Value::int(base.wrapping_pow(exp as u32)))
        }
        _ => {
            let base = args[0]
                .as_float()
                .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
            let exp = args[1]
                .as_float()
                .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
            Ok(Value::float(base.powf(exp)))
        }
    }
}

fn ceil_impl(args: &[Value]) -> Result<Value, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("ceil", "1", args.len()));
    }
    match args[0].view() {
        ValueView::Int(n) => Ok(Value::int(n)),
        ValueView::Float(f) => Ok(Value::int(f.ceil() as i64)),
        _ => Err(SemaError::type_error("number", args[0].type_name())),
    }
}

pub fn register(env: &sema_core::Env) {
    register_fn(env, "abs", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("abs", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::int(n.wrapping_abs())),
            ValueView::Float(f) => Ok(Value::float(f.abs())),
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
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::int(n)),
            ValueView::Float(f) => Ok(Value::int(f.floor() as i64)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "ceil", ceil_impl);
    register_fn(env, "ceiling", ceil_impl);

    register_fn(env, "round", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("round", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::int(n)),
            ValueView::Float(f) => Ok(Value::int(f.round() as i64)),
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
        Ok(Value::float(f.sqrt()))
    });

    register_fn(env, "pow", pow_impl);
    register_fn(env, "expt", pow_impl);
    register_fn(env, "math/pow", pow_impl);

    register_fn(env, "log", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("log", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.ln()))
    });

    register_fn(env, "sin", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("sin", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.sin()))
    });

    register_fn(env, "cos", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("cos", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.cos()))
    });

    // Bind pi and e as constants (bare symbol access)
    env.set_str("pi", Value::float(std::f64::consts::PI));
    env.set_str("e", Value::float(std::f64::consts::E));

    register_fn(env, "int", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("int", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::int(n)),
            ValueView::Float(f) => Ok(Value::int(f as i64)),
            ValueView::String(s) => s
                .parse::<i64>()
                .map(Value::int)
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
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::float(n as f64)),
            ValueView::Float(f) => Ok(Value::float(f)),
            ValueView::String(s) => s
                .parse::<f64>()
                .map(Value::float)
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
        Ok(Value::int(a.wrapping_div(b)))
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
        Ok(Value::int(a % b))
    });

    register_fn(env, "math/gcd", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/gcd", "2", args.len()));
        }
        let mut a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            .wrapping_abs();
        let mut b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            .wrapping_abs();
        while b != 0 {
            let t = b;
            b = a % b;
            a = t;
        }
        Ok(Value::int(a))
    });

    register_fn(env, "math/lcm", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("math/lcm", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            .wrapping_abs();
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            .wrapping_abs();
        if a == 0 && b == 0 {
            return Ok(Value::int(0));
        }
        let mut ga = a;
        let mut gb = b;
        while gb != 0 {
            let t = gb;
            gb = ga % gb;
            ga = t;
        }
        Ok(Value::int((a / ga).wrapping_mul(b)))
    });

    register_fn(env, "math/tan", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/tan", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.tan()))
    });

    register_fn(env, "math/asin", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/asin", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.asin()))
    });

    register_fn(env, "math/acos", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/acos", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.acos()))
    });

    register_fn(env, "math/atan", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/atan", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.atan()))
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
        Ok(Value::float(y.atan2(x)))
    });

    register_fn(env, "math/exp", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/exp", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.exp()))
    });

    register_fn(env, "math/log10", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/log10", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.log10()))
    });

    register_fn(env, "math/log2", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/log2", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.log2()))
    });

    register_fn(env, "math/random", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("math/random", "0", args.len()));
        }
        Ok(Value::float(rand::random::<f64>()))
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
        Ok(Value::int(val))
    });

    register_fn(env, "math/clamp", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("math/clamp", "3", args.len()));
        }
        match (args[0].view(), args[1].view(), args[2].view()) {
            (ValueView::Int(v), ValueView::Int(lo), ValueView::Int(hi)) => {
                Ok(Value::int(v.max(lo).min(hi)))
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
                Ok(Value::float(v.max(lo).min(hi)))
            }
        }
    });

    register_fn(env, "math/sign", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/sign", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::int(if n > 0 {
                1
            } else if n < 0 {
                -1
            } else {
                0
            })),
            ValueView::Float(f) => Ok(Value::int(if f > 0.0 {
                1
            } else if f < 0.0 {
                -1
            } else {
                0
            })),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "truncate", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("truncate", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::int(n)),
            ValueView::Float(f) => Ok(Value::int(f.trunc() as i64)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "math/sinh", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/sinh", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.sinh()))
    });

    register_fn(env, "math/cosh", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/cosh", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.cosh()))
    });

    register_fn(env, "math/tanh", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/tanh", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.tanh()))
    });

    register_fn(env, "math/degrees->radians", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/degrees->radians", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.to_radians()))
    });

    register_fn(env, "math/radians->degrees", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/radians->degrees", "1", args.len()));
        }
        let f = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        Ok(Value::float(f.to_degrees()))
    });

    register_fn(env, "math/lerp", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("math/lerp", "3", args.len()));
        }
        let a = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        let b = args[1]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
        let t = args[2]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[2].type_name()))?;
        Ok(Value::float(a + (b - a) * t))
    });

    register_fn(env, "math/map-range", |args| {
        if args.len() != 5 {
            return Err(SemaError::arity("math/map-range", "5", args.len()));
        }
        let value = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        let in_min = args[1]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
        let in_max = args[2]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[2].type_name()))?;
        let out_min = args[3]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[3].type_name()))?;
        let out_max = args[4]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[4].type_name()))?;
        Ok(Value::float(
            out_min + (value - in_min) / (in_max - in_min) * (out_max - out_min),
        ))
    });

    register_fn(env, "math/nan?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/nan?", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Float(f) => Ok(Value::bool(f.is_nan())),
            _ => Ok(Value::bool(false)),
        }
    });

    register_fn(env, "math/infinite?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("math/infinite?", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Float(f) => Ok(Value::bool(f.is_infinite())),
            _ => Ok(Value::bool(false)),
        }
    });

    env.set_str("math/infinity", Value::float(f64::INFINITY));
    env.set_str("math/nan", Value::float(f64::NAN));
}

fn num_lt(a: &Value, b: &Value) -> Result<bool, SemaError> {
    match (a.view(), b.view()) {
        (ValueView::Int(a), ValueView::Int(b)) => Ok(a < b),
        (ValueView::Float(a), ValueView::Float(b)) => Ok(a < b),
        (ValueView::Int(a), ValueView::Float(b)) => Ok((a as f64) < b),
        (ValueView::Float(a), ValueView::Int(b)) => Ok(a < (b as f64)),
        _ => Err(SemaError::type_error("number", a.type_name())),
    }
}
