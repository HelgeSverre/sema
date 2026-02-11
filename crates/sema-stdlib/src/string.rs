use std::rc::Rc;

use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "string-append", |args| {
        let mut result = String::new();
        for arg in args {
            match arg {
                Value::String(s) => result.push_str(s),
                other => result.push_str(&other.to_string()),
            }
        }
        Ok(Value::String(Rc::new(result)))
    });

    register_fn(env, "string-length", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string-length", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::Int(s.len() as i64))
    });

    register_fn(env, "string-ref", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string-ref", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let idx = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))? as usize;
        s.chars()
            .nth(idx)
            .map(|c| Value::String(Rc::new(c.to_string())))
            .ok_or_else(|| SemaError::eval(format!("string-ref: index {idx} out of bounds")))
    });

    register_fn(env, "substring", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("substring", "2-3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let start = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))? as usize;
        let end = if args.len() == 3 {
            args[2]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?
                as usize
        } else {
            s.len()
        };
        if start > s.len() || end > s.len() || start > end {
            return Err(SemaError::eval("substring: index out of bounds"));
        }
        Ok(Value::String(Rc::new(s[start..end].to_string())))
    });

    register_fn(env, "string/split", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/split", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let sep = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let parts: Vec<Value> = s.split(sep).map(|p| Value::string(p)).collect();
        Ok(Value::list(parts))
    });

    register_fn(env, "string/trim", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/trim", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::String(Rc::new(s.trim().to_string())))
    });

    register_fn(env, "string/contains?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/contains?", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let sub = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::Bool(s.contains(sub)))
    });

    register_fn(env, "string/starts-with?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/starts-with?", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let prefix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::Bool(s.starts_with(prefix)))
    });

    register_fn(env, "string/ends-with?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/ends-with?", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let suffix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::Bool(s.ends_with(suffix)))
    });

    register_fn(env, "string/upper", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/upper", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::String(Rc::new(s.to_uppercase())))
    });

    register_fn(env, "string/lower", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/lower", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::String(Rc::new(s.to_lowercase())))
    });

    register_fn(env, "string/replace", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("string/replace", "3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let from = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let to = args[2]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
        Ok(Value::String(Rc::new(s.replace(from, to))))
    });

    register_fn(env, "string/join", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/join", "2", args.len()));
        }
        let sep = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let items = match &args[0] {
            Value::List(l) => l.as_ref(),
            Value::Vector(v) => v.as_ref(),
            _ => {
                return Err(SemaError::type_error(
                    "list or vector",
                    args[0].type_name(),
                ))
            }
        };
        let strs: Vec<String> = items
            .iter()
            .map(|v| match v {
                Value::String(s) => s.to_string(),
                other => other.to_string(),
            })
            .collect();
        Ok(Value::String(Rc::new(strs.join(sep))))
    });

    register_fn(env, "format", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("format", "1+", 0));
        }
        let fmt = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut result = String::new();
        let mut arg_idx = 1;
        let mut chars = fmt.chars();
        while let Some(ch) = chars.next() {
            if ch == '~' {
                match chars.next() {
                    Some('a') | Some('A') => {
                        // ~a: display (no quotes)
                        if arg_idx < args.len() {
                            match &args[arg_idx] {
                                Value::String(s) => result.push_str(s),
                                other => result.push_str(&other.to_string()),
                            }
                            arg_idx += 1;
                        }
                    }
                    Some('s') | Some('S') => {
                        // ~s: write (with quotes)
                        if arg_idx < args.len() {
                            result.push_str(&args[arg_idx].to_string());
                            arg_idx += 1;
                        }
                    }
                    Some('%') => result.push('\n'),
                    Some('~') => result.push('~'),
                    Some(other) => {
                        result.push('~');
                        result.push(other);
                    }
                    None => result.push('~'),
                }
            } else {
                result.push(ch);
            }
        }
        Ok(Value::String(Rc::new(result)))
    });

    register_fn(env, "str", |args| {
        let mut result = String::new();
        for arg in args {
            match arg {
                Value::String(s) => result.push_str(s),
                other => result.push_str(&other.to_string()),
            }
        }
        Ok(Value::String(Rc::new(result)))
    });

    register_fn(env, "number->string", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("number->string", "1", args.len()));
        }
        match &args[0] {
            Value::Int(n) => Ok(Value::String(Rc::new(n.to_string()))),
            Value::Float(f) => Ok(Value::String(Rc::new(f.to_string()))),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "string->number", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->number", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        if let Ok(n) = s.parse::<i64>() {
            Ok(Value::Int(n))
        } else if let Ok(f) = s.parse::<f64>() {
            Ok(Value::Float(f))
        } else {
            Err(SemaError::eval(format!("cannot parse '{s}' as number")))
        }
    });
}
