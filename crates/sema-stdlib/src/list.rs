use std::rc::Rc;

use sema_core::{Env, Lambda, SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "list", |args| Ok(Value::list(args.to_vec())));

    register_fn(env, "vector", |args| Ok(Value::vector(args.to_vec())));

    register_fn(env, "cons", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("cons", "2", args.len()));
        }
        match &args[1] {
            Value::List(list) => {
                let mut new = vec![args[0].clone()];
                new.extend(list.iter().cloned());
                Ok(Value::list(new))
            }
            Value::Nil => Ok(Value::list(vec![args[0].clone()])),
            _ => Ok(Value::list(vec![args[0].clone(), args[1].clone()])),
        }
    });

    register_fn(env, "car", first);
    register_fn(env, "first", first);

    register_fn(env, "cdr", rest);
    register_fn(env, "rest", rest);

    register_fn(env, "length", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("length", "1", args.len()));
        }
        match &args[0] {
            Value::List(l) => Ok(Value::Int(l.len() as i64)),
            Value::Vector(v) => Ok(Value::Int(v.len() as i64)),
            Value::String(s) => Ok(Value::Int(s.len() as i64)),
            Value::Map(m) => Ok(Value::Int(m.len() as i64)),
            _ => Err(SemaError::type_error("list, vector, string, or map", args[0].type_name())),
        }
    });

    register_fn(env, "append", |args| {
        let mut result = Vec::new();
        for arg in args {
            match arg {
                Value::List(l) => result.extend(l.iter().cloned()),
                Value::Vector(v) => result.extend(v.iter().cloned()),
                _ => return Err(SemaError::type_error("list or vector", arg.type_name())),
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "reverse", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("reverse", "1", args.len()));
        }
        match &args[0] {
            Value::List(l) => {
                let mut v = l.as_ref().clone();
                v.reverse();
                Ok(Value::list(v))
            }
            Value::Vector(v) => {
                let mut items = v.as_ref().clone();
                items.reverse();
                Ok(Value::vector(items))
            }
            _ => Err(SemaError::type_error("list or vector", args[0].type_name())),
        }
    });

    register_fn(env, "nth", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("nth", "2", args.len()));
        }
        let idx = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))? as usize;
        match &args[0] {
            Value::List(l) => l.get(idx).cloned().ok_or_else(|| {
                SemaError::eval(format!("index {idx} out of bounds (length {})", l.len()))
            }),
            Value::Vector(v) => v.get(idx).cloned().ok_or_else(|| {
                SemaError::eval(format!("index {idx} out of bounds (length {})", v.len()))
            }),
            _ => Err(SemaError::type_error("list or vector", args[0].type_name())),
        }
    });

    register_fn(env, "map", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map", "2", args.len()));
        }
        let items = get_sequence(&args[1], "map")?;
        let mut result = Vec::with_capacity(items.len());
        for item in &items {
            result.push(call_function(&args[0], &[item.clone()])?);
        }
        Ok(Value::list(result))
    });

    register_fn(env, "filter", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("filter", "2", args.len()));
        }
        let items = get_sequence(&args[1], "filter")?;
        let mut result = Vec::new();
        for item in &items {
            let keep = call_function(&args[0], &[item.clone()])?;
            if keep.is_truthy() {
                result.push(item.clone());
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "foldl", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("foldl", "3", args.len()));
        }
        let items = get_sequence(&args[2], "foldl")?;
        let mut acc = args[1].clone();
        for item in &items {
            acc = call_function(&args[0], &[acc, item.clone()])?;
        }
        Ok(acc)
    });

    register_fn(env, "for-each", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("for-each", "2", args.len()));
        }
        let items = get_sequence(&args[1], "for-each")?;
        for item in &items {
            call_function(&args[0], &[item.clone()])?;
        }
        Ok(Value::Nil)
    });

    register_fn(env, "range", |args| {
        let (start, end, step) = match args.len() {
            1 => (0i64, args[0].as_int().ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?, 1i64),
            2 => {
                let s = args[0].as_int().ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
                let e = args[1].as_int().ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
                (s, e, 1)
            }
            3 => {
                let s = args[0].as_int().ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
                let e = args[1].as_int().ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
                let st = args[2].as_int().ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?;
                (s, e, st)
            }
            _ => return Err(SemaError::arity("range", "1-3", args.len())),
        };
        if step == 0 {
            return Err(SemaError::eval("range: step cannot be 0"));
        }
        let mut result = Vec::new();
        let mut i = start;
        if step > 0 {
            while i < end {
                result.push(Value::Int(i));
                i += step;
            }
        } else {
            while i > end {
                result.push(Value::Int(i));
                i += step;
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "apply", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("apply", "2+", args.len()));
        }
        let func = &args[0];
        // Last arg must be a list, preceding args are prepended
        let last = &args[args.len() - 1];
        let last_items = get_sequence(last, "apply")?;
        let mut all_args: Vec<Value> = args[1..args.len() - 1].to_vec();
        all_args.extend(last_items);
        call_function(func, &all_args)
    });

    register_fn(env, "sort", |args| {
        if args.len() < 1 || args.len() > 2 {
            return Err(SemaError::arity("sort", "1-2", args.len()));
        }
        let mut items = get_sequence(&args[0], "sort")?;
        if args.len() == 1 {
            items.sort();
        } else {
            // Sort with comparator
            let mut err = None;
            items.sort_by(|a, b| {
                if err.is_some() {
                    return std::cmp::Ordering::Equal;
                }
                match call_function(&args[1], &[a.clone(), b.clone()]) {
                    Ok(Value::Int(n)) => {
                        if n < 0 {
                            std::cmp::Ordering::Less
                        } else if n > 0 {
                            std::cmp::Ordering::Greater
                        } else {
                            std::cmp::Ordering::Equal
                        }
                    }
                    Ok(Value::Bool(true)) => std::cmp::Ordering::Less,
                    Ok(Value::Bool(false)) => std::cmp::Ordering::Greater,
                    Ok(_) => std::cmp::Ordering::Equal,
                    Err(e) => {
                        err = Some(e);
                        std::cmp::Ordering::Equal
                    }
                }
            });
            if let Some(e) = err {
                return Err(e);
            }
        }
        Ok(Value::list(items))
    });
}

fn first(args: &[Value]) -> Result<Value, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("car", "1", args.len()));
    }
    match &args[0] {
        Value::List(l) => {
            if l.is_empty() {
                Ok(Value::Nil)
            } else {
                Ok(l[0].clone())
            }
        }
        Value::Vector(v) => {
            if v.is_empty() {
                Ok(Value::Nil)
            } else {
                Ok(v[0].clone())
            }
        }
        _ => Err(SemaError::type_error("list or vector", args[0].type_name())),
    }
}

fn rest(args: &[Value]) -> Result<Value, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("cdr", "1", args.len()));
    }
    match &args[0] {
        Value::List(l) => {
            if l.len() <= 1 {
                Ok(Value::list(vec![]))
            } else {
                Ok(Value::list(l[1..].to_vec()))
            }
        }
        Value::Vector(v) => {
            if v.len() <= 1 {
                Ok(Value::vector(vec![]))
            } else {
                Ok(Value::vector(v[1..].to_vec()))
            }
        }
        _ => Err(SemaError::type_error("list or vector", args[0].type_name())),
    }
}

fn get_sequence(val: &Value, ctx: &str) -> Result<Vec<Value>, SemaError> {
    match val {
        Value::List(l) => Ok(l.as_ref().clone()),
        Value::Vector(v) => Ok(v.as_ref().clone()),
        _ => Err(SemaError::type_error(
            "list or vector",
            &format!("{} in {ctx}", val.type_name()),
        )),
    }
}

/// Call a Sema function (lambda or native) with given args.
pub fn call_function(func: &Value, args: &[Value]) -> Result<Value, SemaError> {
    match func {
        Value::NativeFn(native) => (native.func)(args),
        Value::Lambda(lambda) => {
            let env = Env::with_parent(Rc::new(lambda.env.clone()));
            // Bind params
            if let Some(ref rest) = lambda.rest_param {
                if args.len() < lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda.name.as_deref().unwrap_or("lambda"),
                        &format!("{}+", lambda.params.len()),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    env.set(param.clone(), arg.clone());
                }
                let rest_args = args[lambda.params.len()..].to_vec();
                env.set(rest.clone(), Value::list(rest_args));
            } else {
                if args.len() != lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda.name.as_deref().unwrap_or("lambda"),
                        &lambda.params.len().to_string(),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    env.set(param.clone(), arg.clone());
                }
            }
            // Self-reference
            if let Some(ref name) = lambda.name {
                env.set(name.clone(), Value::Lambda(Rc::new(Lambda {
                    params: lambda.params.clone(),
                    rest_param: lambda.rest_param.clone(),
                    body: lambda.body.clone(),
                    env: lambda.env.clone(),
                    name: lambda.name.clone(),
                })));
            }
            // Evaluate body
            let mut result = Value::Nil;
            for expr in &lambda.body {
                result = sema_eval_value(&expr, &env)?;
            }
            Ok(result)
        }
        _ => Err(SemaError::eval(format!(
            "not callable: {} ({})",
            func,
            func.type_name()
        ))),
    }
}

/// Minimal eval for stdlib usage (avoids circular dependency by doing simple eval).
fn sema_eval_value(expr: &Value, env: &Env) -> Result<Value, SemaError> {
    match expr {
        Value::Symbol(name) => env
            .get(name)
            .ok_or_else(|| SemaError::Unbound(name.to_string())),
        Value::List(items) if !items.is_empty() => {
            let func_val = sema_eval_value(&items[0], env)?;
            let mut args = Vec::new();
            for arg in &items[1..] {
                args.push(sema_eval_value(arg, env)?);
            }
            call_function(&func_val, &args)
        }
        _ => Ok(expr.clone()),
    }
}
