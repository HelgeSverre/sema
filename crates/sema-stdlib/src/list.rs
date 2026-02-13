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
            _ => Err(SemaError::type_error(
                "list, vector, string, or map",
                args[0].type_name(),
            )),
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
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as usize;
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
        if args.len() < 2 {
            return Err(SemaError::arity("map", "2+", args.len()));
        }
        if args.len() == 2 {
            let items = get_sequence(&args[1], "map")?;
            let mut result = Vec::with_capacity(items.len());
            for item in &items {
                result.push(call_function(&args[0], &[item.clone()])?);
            }
            Ok(Value::list(result))
        } else {
            // Multi-list map: iterate in lockstep (shortest wins)
            let lists: Vec<Vec<Value>> = args[1..]
                .iter()
                .map(|a| get_sequence(a, "map"))
                .collect::<Result<_, _>>()?;
            let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
            let mut result = Vec::with_capacity(min_len);
            for i in 0..min_len {
                let call_args: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(call_function(&args[0], &call_args)?);
            }
            Ok(Value::list(result))
        }
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
            1 => (
                0i64,
                args[0]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?,
                1i64,
            ),
            2 => {
                let s = args[0]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
                let e = args[1]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
                (s, e, 1)
            }
            3 => {
                let s = args[0]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
                let e = args[1]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
                let st = args[2]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?;
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

    register_fn(env, "take", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("take", "2", args.len()));
        }
        let n = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            as usize;
        let items = get_sequence(&args[1], "take")?;
        let end = n.min(items.len());
        Ok(Value::list(items[..end].to_vec()))
    });

    register_fn(env, "drop", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("drop", "2", args.len()));
        }
        let n = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            as usize;
        let items = get_sequence(&args[1], "drop")?;
        let start = n.min(items.len());
        Ok(Value::list(items[start..].to_vec()))
    });

    register_fn(env, "last", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("last", "1", args.len()));
        }
        let items = get_sequence(&args[0], "last")?;
        Ok(items.last().cloned().unwrap_or(Value::Nil))
    });

    register_fn(env, "zip", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("zip", "2+", args.len()));
        }
        let lists: Vec<Vec<Value>> = args
            .iter()
            .map(|a| get_sequence(a, "zip"))
            .collect::<Result<_, _>>()?;
        let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
        let mut result = Vec::with_capacity(min_len);
        for i in 0..min_len {
            let tuple: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
            result.push(Value::list(tuple));
        }
        Ok(Value::list(result))
    });

    register_fn(env, "flatten", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("flatten", "1", args.len()));
        }
        let items = get_sequence(&args[0], "flatten")?;
        let mut result = Vec::new();
        for item in &items {
            match item {
                Value::List(l) => result.extend(l.iter().cloned()),
                Value::Vector(v) => result.extend(v.iter().cloned()),
                other => result.push(other.clone()),
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "member", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("member", "2", args.len()));
        }
        let items = get_sequence(&args[1], "member")?;
        for (i, item) in items.iter().enumerate() {
            if item == &args[0] {
                return Ok(Value::list(items[i..].to_vec()));
            }
        }
        Ok(Value::Bool(false))
    });

    register_fn(env, "any", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("any", "2", args.len()));
        }
        let items = get_sequence(&args[1], "any")?;
        for item in &items {
            if call_function(&args[0], &[item.clone()])?.is_truthy() {
                return Ok(Value::Bool(true));
            }
        }
        Ok(Value::Bool(false))
    });

    register_fn(env, "every", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("every", "2", args.len()));
        }
        let items = get_sequence(&args[1], "every")?;
        for item in &items {
            if !call_function(&args[0], &[item.clone()])?.is_truthy() {
                return Ok(Value::Bool(false));
            }
        }
        Ok(Value::Bool(true))
    });

    register_fn(env, "reduce", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("reduce", "2", args.len()));
        }
        let items = get_sequence(&args[1], "reduce")?;
        if items.is_empty() {
            return Err(SemaError::eval("reduce: empty list"));
        }
        let mut acc = items[0].clone();
        for item in &items[1..] {
            acc = call_function(&args[0], &[acc, item.clone()])?;
        }
        Ok(acc)
    });

    register_fn(env, "partition", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("partition", "2", args.len()));
        }
        let items = get_sequence(&args[1], "partition")?;
        let mut matching = Vec::new();
        let mut non_matching = Vec::new();
        for item in &items {
            if call_function(&args[0], &[item.clone()])?.is_truthy() {
                matching.push(item.clone());
            } else {
                non_matching.push(item.clone());
            }
        }
        Ok(Value::list(vec![
            Value::list(matching),
            Value::list(non_matching),
        ]))
    });

    register_fn(env, "foldr", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("foldr", "3", args.len()));
        }
        let items = get_sequence(&args[2], "foldr")?;
        let mut acc = args[1].clone();
        for item in items.iter().rev() {
            acc = call_function(&args[0], &[item.clone(), acc])?;
        }
        Ok(acc)
    });

    register_fn(env, "sort", |args| {
        if args.is_empty() || args.len() > 2 {
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

    register_fn(env, "list/index-of", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("list/index-of", "2", args.len()));
        }
        let items = get_sequence(&args[0], "list/index-of")?;
        for (i, item) in items.iter().enumerate() {
            if item == &args[1] {
                return Ok(Value::Int(i as i64));
            }
        }
        Ok(Value::Nil)
    });

    register_fn(env, "list/unique", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list/unique", "1", args.len()));
        }
        let items = get_sequence(&args[0], "list/unique")?;
        let mut seen = Vec::new();
        let mut result = Vec::new();
        for item in &items {
            if !seen.contains(item) {
                seen.push(item.clone());
                result.push(item.clone());
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "list/group-by", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("list/group-by", "2", args.len()));
        }
        let items = get_sequence(&args[1], "list/group-by")?;
        let mut groups: Vec<(Value, Vec<Value>)> = Vec::new();
        for item in &items {
            let key = call_function(&args[0], &[item.clone()])?;
            if let Some(group) = groups.iter_mut().find(|(k, _)| k == &key) {
                group.1.push(item.clone());
            } else {
                groups.push((key, vec![item.clone()]));
            }
        }
        let mut map = std::collections::BTreeMap::new();
        for (key, vals) in groups {
            map.insert(key, Value::list(vals));
        }
        Ok(Value::Map(Rc::new(map)))
    });

    register_fn(env, "list/interleave", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("list/interleave", "2+", args.len()));
        }
        let lists: Vec<Vec<Value>> = args
            .iter()
            .map(|a| get_sequence(a, "list/interleave"))
            .collect::<Result<_, _>>()?;
        let min_len = lists.iter().map(|l| l.len()).min().unwrap_or(0);
        let mut result = Vec::with_capacity(min_len * lists.len());
        for i in 0..min_len {
            for list in &lists {
                result.push(list[i].clone());
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "sort-by", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("sort-by", "2", args.len()));
        }
        let items = get_sequence(&args[1], "sort-by")?;
        // Extract keys for each element
        let mut keyed: Vec<(Value, Value)> = Vec::with_capacity(items.len());
        for item in &items {
            let key = call_function(&args[0], &[item.clone()])?;
            keyed.push((key, item.clone()));
        }
        keyed.sort_by(|(ka, _), (kb, _)| ka.cmp(kb));
        let result: Vec<Value> = keyed.into_iter().map(|(_, v)| v).collect();
        Ok(Value::list(result))
    });

    register_fn(env, "flatten-deep", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("flatten-deep", "1", args.len()));
        }
        let mut out = Vec::new();
        flatten_recursive(&args[0], &mut out);
        Ok(Value::list(out))
    });

    register_fn(env, "interpose", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("interpose", "2", args.len()));
        }
        let items = get_sequence(&args[1], "interpose")?;
        if items.is_empty() {
            return Ok(Value::list(vec![]));
        }
        let mut result = Vec::with_capacity(items.len() * 2 - 1);
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                result.push(args[0].clone());
            }
            result.push(item.clone());
        }
        Ok(Value::list(result))
    });

    register_fn(env, "frequencies", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("frequencies", "1", args.len()));
        }
        let items = get_sequence(&args[0], "frequencies")?;
        let mut counts: std::collections::BTreeMap<Value, i64> = std::collections::BTreeMap::new();
        for item in &items {
            *counts.entry(item.clone()).or_insert(0) += 1;
        }
        let map: std::collections::BTreeMap<Value, Value> = counts
            .into_iter()
            .map(|(k, v)| (k, Value::Int(v)))
            .collect();
        Ok(Value::Map(Rc::new(map)))
    });

    register_fn(env, "list->vector", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list->vector", "1", args.len()));
        }
        match &args[0] {
            Value::List(l) => Ok(Value::vector(l.as_ref().clone())),
            _ => Err(SemaError::type_error("list", args[0].type_name())),
        }
    });

    register_fn(env, "vector->list", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("vector->list", "1", args.len()));
        }
        match &args[0] {
            Value::Vector(v) => Ok(Value::list(v.as_ref().clone())),
            _ => Err(SemaError::type_error("vector", args[0].type_name())),
        }
    });

    register_fn(env, "list/chunk", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("list/chunk", "2", args.len()));
        }
        let n = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            as usize;
        if n == 0 {
            return Err(SemaError::eval("list/chunk: chunk size must be positive"));
        }
        let items = get_sequence(&args[1], "list/chunk")?;
        let mut result = Vec::new();
        for chunk in items.chunks(n) {
            result.push(Value::list(chunk.to_vec()));
        }
        Ok(Value::list(result))
    });

    // Car/cdr compositions (2-deep)
    register_fn(env, "caar", |args| first(&[first(args)?]));
    register_fn(env, "cadr", |args| first(&[rest(args)?]));
    register_fn(env, "cdar", |args| rest(&[first(args)?]));
    register_fn(env, "cddr", |args| rest(&[rest(args)?]));

    // Car/cdr compositions (3-deep)
    register_fn(env, "caaar", |args| first(&[first(&[first(args)?])?]));
    register_fn(env, "caadr", |args| first(&[first(&[rest(args)?])?]));
    register_fn(env, "cadar", |args| first(&[rest(&[first(args)?])?]));
    register_fn(env, "caddr", |args| first(&[rest(&[rest(args)?])?]));
    register_fn(env, "cdaar", |args| rest(&[first(&[first(args)?])?]));
    register_fn(env, "cdadr", |args| rest(&[first(&[rest(args)?])?]));
    register_fn(env, "cddar", |args| rest(&[rest(&[first(args)?])?]));
    register_fn(env, "cdddr", |args| rest(&[rest(&[rest(args)?])?]));

    // Association list functions (assoc is dual-purpose in map.rs)
    register_fn(env, "assq", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("assq", "2", args.len()));
        }
        let key = &args[0];
        let alist = get_sequence(&args[1], "assq")?;
        for pair in &alist {
            if let Value::List(p) = pair {
                if !p.is_empty() && &p[0] == key {
                    return Ok(pair.clone());
                }
            }
        }
        Ok(Value::Bool(false))
    });

    register_fn(env, "assv", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("assv", "2", args.len()));
        }
        let key = &args[0];
        let alist = get_sequence(&args[1], "assv")?;
        for pair in &alist {
            if let Value::List(p) = pair {
                if !p.is_empty() && &p[0] == key {
                    return Ok(pair.clone());
                }
            }
        }
        Ok(Value::Bool(false))
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
            format!("{} in {ctx}", val.type_name()),
        )),
    }
}

fn flatten_recursive(val: &Value, out: &mut Vec<Value>) {
    match val {
        Value::List(l) => {
            for item in l.iter() {
                flatten_recursive(item, out);
            }
        }
        Value::Vector(v) => {
            for item in v.iter() {
                flatten_recursive(item, out);
            }
        }
        other => out.push(other.clone()),
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
                        format!("{}+", lambda.params.len()),
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
                        lambda.params.len().to_string(),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    env.set(param.clone(), arg.clone());
                }
            }
            // Self-reference
            if let Some(ref name) = lambda.name {
                env.set(
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
            // Evaluate body
            let mut result = Value::Nil;
            for expr in &lambda.body {
                result = sema_eval_value(expr, &env)?;
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

/// Minimal eval for stdlib usage (avoids circular dependency).
/// Handles: symbols, function calls, if, begin, let, let*, cond, when, unless,
/// and, or, define, set!, quote, lambda/fn.
fn sema_eval_value(expr: &Value, env: &Env) -> Result<Value, SemaError> {
    match expr {
        Value::Symbol(name) => env
            .get(name)
            .ok_or_else(|| SemaError::Unbound(name.to_string())),
        Value::List(items) if !items.is_empty() => {
            // Check for special forms first
            if let Value::Symbol(ref head) = items[0] {
                match head.as_str() {
                    "quote" => {
                        return if items.len() == 2 {
                            Ok(items[1].clone())
                        } else {
                            Err(SemaError::eval("quote: expected 1 argument"))
                        };
                    }
                    "if" => {
                        if items.len() < 3 || items.len() > 4 {
                            return Err(SemaError::eval("if: expected 2-3 arguments"));
                        }
                        let cond = sema_eval_value(&items[1], env)?;
                        if cond.is_truthy() {
                            return sema_eval_value(&items[2], env);
                        } else if items.len() == 4 {
                            return sema_eval_value(&items[3], env);
                        } else {
                            return Ok(Value::Nil);
                        }
                    }
                    "begin" => {
                        let mut result = Value::Nil;
                        for item in &items[1..] {
                            result = sema_eval_value(item, env)?;
                        }
                        return Ok(result);
                    }
                    "let" => {
                        if items.len() < 3 {
                            return Err(SemaError::eval("let: expected bindings and body"));
                        }
                        // Named let: (let name ((var init) ...) body...)
                        if let Value::Symbol(ref loop_name) = items[1] {
                            if items.len() < 4 {
                                return Err(SemaError::eval(
                                    "named let: expected bindings and body",
                                ));
                            }
                            let bindings = match &items[2] {
                                Value::List(l) => l.as_ref(),
                                _ => {
                                    return Err(SemaError::eval("named let: expected binding list"))
                                }
                            };
                            let mut params = Vec::new();
                            let mut init_vals = Vec::new();
                            for binding in bindings {
                                match binding {
                                    Value::List(pair) if pair.len() == 2 => {
                                        if let Value::Symbol(ref p) = pair[0] {
                                            params.push(p.to_string());
                                            init_vals.push(sema_eval_value(&pair[1], env)?);
                                        } else {
                                            return Err(SemaError::eval(
                                                "let: binding name must be symbol",
                                            ));
                                        }
                                    }
                                    _ => {
                                        return Err(SemaError::eval(
                                            "let: each binding must be (name value)",
                                        ))
                                    }
                                }
                            }
                            let body: Vec<Value> = items[3..].to_vec();
                            let lambda = Value::Lambda(Rc::new(Lambda {
                                params: params.clone(),
                                rest_param: None,
                                body,
                                env: env.clone(),
                                name: Some(loop_name.to_string()),
                            }));
                            let child = Env::with_parent(Rc::new(env.clone()));
                            child.set(loop_name.to_string(), lambda.clone());
                            for (p, v) in params.iter().zip(init_vals.iter()) {
                                child.set(p.clone(), v.clone());
                            }
                            let mut result = Value::Nil;
                            for item in &items[3..] {
                                result = sema_eval_value(item, &child)?;
                            }
                            return Ok(result);
                        }
                        let bindings = match &items[1] {
                            Value::List(l) => l.as_ref(),
                            _ => return Err(SemaError::eval("let: expected binding list")),
                        };
                        let child = Env::with_parent(Rc::new(env.clone()));
                        for binding in bindings {
                            match binding {
                                Value::List(pair) if pair.len() == 2 => {
                                    if let Value::Symbol(ref name) = pair[0] {
                                        let val = sema_eval_value(&pair[1], env)?;
                                        child.set(name.to_string(), val);
                                    }
                                }
                                _ => {
                                    return Err(SemaError::eval(
                                        "let: each binding must be (name value)",
                                    ))
                                }
                            }
                        }
                        let mut result = Value::Nil;
                        for item in &items[2..] {
                            result = sema_eval_value(item, &child)?;
                        }
                        return Ok(result);
                    }
                    "let*" => {
                        if items.len() < 3 {
                            return Err(SemaError::eval("let*: expected bindings and body"));
                        }
                        let bindings = match &items[1] {
                            Value::List(l) => l.as_ref(),
                            _ => return Err(SemaError::eval("let*: expected binding list")),
                        };
                        let child = Env::with_parent(Rc::new(env.clone()));
                        for binding in bindings {
                            match binding {
                                Value::List(pair) if pair.len() == 2 => {
                                    if let Value::Symbol(ref name) = pair[0] {
                                        let val = sema_eval_value(&pair[1], &child)?;
                                        child.set(name.to_string(), val);
                                    }
                                }
                                _ => {
                                    return Err(SemaError::eval(
                                        "let*: each binding must be (name value)",
                                    ))
                                }
                            }
                        }
                        let mut result = Value::Nil;
                        for item in &items[2..] {
                            result = sema_eval_value(item, &child)?;
                        }
                        return Ok(result);
                    }
                    "cond" => {
                        for clause in &items[1..] {
                            match clause {
                                Value::List(pair) if !pair.is_empty() => {
                                    if let Value::Symbol(ref s) = pair[0] {
                                        if s.as_str() == "else" {
                                            let mut result = Value::Nil;
                                            for expr in &pair[1..] {
                                                result = sema_eval_value(expr, env)?;
                                            }
                                            return Ok(result);
                                        }
                                    }
                                    let test = sema_eval_value(&pair[0], env)?;
                                    if test.is_truthy() {
                                        let mut result = Value::Nil;
                                        for expr in &pair[1..] {
                                            result = sema_eval_value(expr, env)?;
                                        }
                                        return Ok(result);
                                    }
                                }
                                _ => return Err(SemaError::eval("cond: expected clause list")),
                            }
                        }
                        return Ok(Value::Nil);
                    }
                    "when" => {
                        if items.len() < 3 {
                            return Err(SemaError::eval("when: expected test and body"));
                        }
                        let test = sema_eval_value(&items[1], env)?;
                        if test.is_truthy() {
                            let mut result = Value::Nil;
                            for item in &items[2..] {
                                result = sema_eval_value(item, env)?;
                            }
                            return Ok(result);
                        }
                        return Ok(Value::Nil);
                    }
                    "unless" => {
                        if items.len() < 3 {
                            return Err(SemaError::eval("unless: expected test and body"));
                        }
                        let test = sema_eval_value(&items[1], env)?;
                        if !test.is_truthy() {
                            let mut result = Value::Nil;
                            for item in &items[2..] {
                                result = sema_eval_value(item, env)?;
                            }
                            return Ok(result);
                        }
                        return Ok(Value::Nil);
                    }
                    "and" => {
                        let mut result = Value::Bool(true);
                        for item in &items[1..] {
                            result = sema_eval_value(item, env)?;
                            if !result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "or" => {
                        let mut result = Value::Bool(false);
                        for item in &items[1..] {
                            result = sema_eval_value(item, env)?;
                            if result.is_truthy() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
                    }
                    "define" => {
                        if items.len() < 3 {
                            return Err(SemaError::eval("define: expected name and value"));
                        }
                        match &items[1] {
                            Value::Symbol(name) => {
                                let val = sema_eval_value(&items[2], env)?;
                                env.set(name.to_string(), val);
                                return Ok(Value::Nil);
                            }
                            Value::List(sig) if !sig.is_empty() => {
                                if let Value::Symbol(ref name) = sig[0] {
                                    let params: Vec<String> = sig[1..]
                                        .iter()
                                        .filter_map(|v| {
                                            if let Value::Symbol(s) = v {
                                                Some(s.to_string())
                                            } else {
                                                None
                                            }
                                        })
                                        .collect();
                                    let body = items[2..].to_vec();
                                    let lambda = Value::Lambda(Rc::new(Lambda {
                                        params,
                                        rest_param: None,
                                        body,
                                        env: env.clone(),
                                        name: Some(name.to_string()),
                                    }));
                                    env.set(name.to_string(), lambda);
                                    return Ok(Value::Nil);
                                }
                                return Err(SemaError::eval("define: invalid function signature"));
                            }
                            _ => {
                                return Err(SemaError::eval(
                                    "define: expected symbol or function signature",
                                ))
                            }
                        }
                    }
                    "set!" => {
                        if items.len() != 3 {
                            return Err(SemaError::eval("set!: expected name and value"));
                        }
                        if let Value::Symbol(ref name) = items[1] {
                            let val = sema_eval_value(&items[2], env)?;
                            env.set(name.to_string(), val);
                            return Ok(Value::Nil);
                        }
                        return Err(SemaError::eval("set!: expected symbol"));
                    }
                    "lambda" | "fn" => {
                        if items.len() < 3 {
                            return Err(SemaError::eval("lambda: expected params and body"));
                        }
                        let (params, rest_param) = match &items[1] {
                            Value::List(l) => {
                                let mut params = Vec::new();
                                let mut rest = None;
                                let mut saw_dot = false;
                                for p in l.iter() {
                                    if let Value::Symbol(s) = p {
                                        if s.as_str() == "." {
                                            saw_dot = true;
                                        } else if saw_dot {
                                            rest = Some(s.to_string());
                                        } else {
                                            params.push(s.to_string());
                                        }
                                    }
                                }
                                (params, rest)
                            }
                            _ => return Err(SemaError::eval("lambda: expected parameter list")),
                        };
                        let body = items[2..].to_vec();
                        return Ok(Value::Lambda(Rc::new(Lambda {
                            params,
                            rest_param,
                            body,
                            env: env.clone(),
                            name: None,
                        })));
                    }
                    _ => {} // fall through to normal function call
                }
            }
            // Keywords in function position: (:key map)
            if let Value::Keyword(_) = &items[0] {
                let key = items[0].clone();
                if items.len() >= 2 {
                    let map_val = sema_eval_value(&items[1], env)?;
                    if let Value::Map(m) = &map_val {
                        return Ok(m.get(&key).cloned().unwrap_or(Value::Nil));
                    }
                }
            }
            // Normal function call
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
