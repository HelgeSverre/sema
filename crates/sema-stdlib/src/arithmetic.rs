use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "+", |args| {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }
        let mut has_float = false;
        let mut int_sum: i64 = 0;
        let mut float_sum: f64 = 0.0;
        for arg in args {
            match arg {
                Value::Int(n) => {
                    if has_float {
                        float_sum += *n as f64;
                    } else {
                        int_sum += n;
                    }
                }
                Value::Float(f) => {
                    if !has_float {
                        float_sum = int_sum as f64;
                        has_float = true;
                    }
                    float_sum += f;
                }
                _ => return Err(SemaError::type_error("number", arg.type_name())),
            }
        }
        if has_float {
            Ok(Value::Float(float_sum))
        } else {
            Ok(Value::Int(int_sum))
        }
    });

    register_fn(env, "-", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("-", "1+", 0));
        }
        if args.len() == 1 {
            return match &args[0] {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(f) => Ok(Value::Float(-f)),
                _ => Err(SemaError::type_error("number", args[0].type_name())),
            };
        }
        let mut has_float = false;
        let mut result_int: i64 = 0;
        let mut result_float: f64 = 0.0;
        for (i, arg) in args.iter().enumerate() {
            match arg {
                Value::Int(n) => {
                    if i == 0 {
                        if has_float {
                            result_float = *n as f64;
                        } else {
                            result_int = *n;
                        }
                    } else if has_float {
                        result_float -= *n as f64;
                    } else {
                        result_int -= n;
                    }
                }
                Value::Float(f) => {
                    if !has_float {
                        result_float = result_int as f64;
                        has_float = true;
                    }
                    if i == 0 {
                        result_float = *f;
                    } else {
                        result_float -= f;
                    }
                }
                _ => return Err(SemaError::type_error("number", arg.type_name())),
            }
        }
        if has_float {
            Ok(Value::Float(result_float))
        } else {
            Ok(Value::Int(result_int))
        }
    });

    register_fn(env, "*", |args| {
        if args.is_empty() {
            return Ok(Value::Int(1));
        }
        let mut has_float = false;
        let mut int_prod: i64 = 1;
        let mut float_prod: f64 = 1.0;
        for arg in args {
            match arg {
                Value::Int(n) => {
                    if has_float {
                        float_prod *= *n as f64;
                    } else {
                        int_prod *= n;
                    }
                }
                Value::Float(f) => {
                    if !has_float {
                        float_prod = int_prod as f64;
                        has_float = true;
                    }
                    float_prod *= f;
                }
                _ => return Err(SemaError::type_error("number", arg.type_name())),
            }
        }
        if has_float {
            Ok(Value::Float(float_prod))
        } else {
            Ok(Value::Int(int_prod))
        }
    });

    register_fn(env, "/", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("/", "2+", args.len()));
        }
        let mut result = match &args[0] {
            Value::Int(n) => *n as f64,
            Value::Float(f) => *f,
            _ => return Err(SemaError::type_error("number", args[0].type_name())),
        };
        for arg in &args[1..] {
            let divisor = match arg {
                Value::Int(n) => *n as f64,
                Value::Float(f) => *f,
                _ => return Err(SemaError::type_error("number", arg.type_name())),
            };
            if divisor == 0.0 {
                return Err(SemaError::eval("division by zero"));
            }
            result /= divisor;
        }
        // Return int if result is a whole number and inputs were ints
        if result.fract() == 0.0 && args.iter().all(|a| matches!(a, Value::Int(_))) {
            Ok(Value::Int(result as i64))
        } else {
            Ok(Value::Float(result))
        }
    });

    register_fn(env, "mod", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("mod", "2", args.len()));
        }
        match (&args[0], &args[1]) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(SemaError::eval("modulo by zero"))
                } else {
                    Ok(Value::Int(a % b))
                }
            }
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a % b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 % b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a % *b as f64)),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });
}
