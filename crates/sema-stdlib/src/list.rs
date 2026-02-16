use std::rc::Rc;

use sema_core::{SemaError, Value};

use crate::register_fn;

fn repeat_impl(args: &[Value]) -> Result<Value, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("list/repeat", "2", args.len()));
    }
    let n = args[0]
        .as_int()
        .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))? as usize;
    let val = args[1].clone();
    Ok(Value::list(vec![val; n]))
}

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
            Value::String(s) => Ok(Value::Int(s.chars().count() as i64)),
            Value::Map(m) => Ok(Value::Int(m.len() as i64)),
            Value::HashMap(m) => Ok(Value::Int(m.len() as i64)),
            Value::Bytevector(bv) => Ok(Value::Int(bv.len() as i64)),
            _ => Err(SemaError::type_error(
                "list, vector, string, map, or bytevector",
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

    register_fn(env, "take-while", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("take-while", "2", args.len()));
        }
        let items = get_sequence(&args[1], "take-while")?;
        let mut result = Vec::new();
        for item in &items {
            if call_function(&args[0], &[item.clone()])?.is_truthy() {
                result.push(item.clone());
            } else {
                break;
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "drop-while", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("drop-while", "2", args.len()));
        }
        let items = get_sequence(&args[1], "drop-while")?;
        let mut dropping = true;
        let mut result = Vec::new();
        for item in &items {
            if dropping && call_function(&args[0], &[item.clone()])?.is_truthy() {
                continue;
            }
            dropping = false;
            result.push(item.clone());
        }
        Ok(Value::list(result))
    });

    register_fn(env, "list/dedupe", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list/dedupe", "1", args.len()));
        }
        let items = get_sequence(&args[0], "list/dedupe")?;
        let mut result = Vec::new();
        for item in &items {
            if result.last() != Some(item) {
                result.push(item.clone());
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "flat-map", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("flat-map", "2", args.len()));
        }
        let items = get_sequence(&args[1], "flat-map")?;
        let mut result = Vec::new();
        for item in &items {
            let mapped = call_function(&args[0], &[item.clone()])?;
            match mapped {
                Value::List(l) => result.extend(l.iter().cloned()),
                Value::Vector(v) => result.extend(v.iter().cloned()),
                other => result.push(other),
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "list/shuffle", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list/shuffle", "1", args.len()));
        }
        let mut items = get_sequence(&args[0], "list/shuffle")?;
        use rand::seq::SliceRandom;
        items.shuffle(&mut rand::rng());
        Ok(Value::list(items))
    });

    register_fn(env, "list/split-at", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("list/split-at", "2", args.len()));
        }
        let items = get_sequence(&args[0], "list/split-at")?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as usize;
        let n = n.min(items.len());
        let left = items[..n].to_vec();
        let right = items[n..].to_vec();
        Ok(Value::list(vec![Value::list(left), Value::list(right)]))
    });

    register_fn(env, "list/take-while", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("list/take-while", "2", args.len()));
        }
        let items = get_sequence(&args[1], "list/take-while")?;
        let mut result = Vec::new();
        for item in &items {
            let keep = call_function(&args[0], &[item.clone()])?;
            if keep.is_truthy() {
                result.push(item.clone());
            } else {
                break;
            }
        }
        Ok(Value::list(result))
    });

    register_fn(env, "list/drop-while", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("list/drop-while", "2", args.len()));
        }
        let items = get_sequence(&args[1], "list/drop-while")?;
        let mut dropping = true;
        let mut result = Vec::new();
        for item in &items {
            if dropping {
                let drop = call_function(&args[0], &[item.clone()])?;
                if drop.is_truthy() {
                    continue;
                }
                dropping = false;
            }
            result.push(item.clone());
        }
        Ok(Value::list(result))
    });

    register_fn(env, "list/sum", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list/sum", "1", args.len()));
        }
        let items = get_sequence(&args[0], "list/sum")?;
        let mut int_sum: i64 = 0;
        let mut has_float = false;
        let mut float_sum: f64 = 0.0;
        for item in &items {
            match item {
                Value::Int(n) => {
                    int_sum += n;
                    float_sum += *n as f64;
                }
                Value::Float(f) => {
                    has_float = true;
                    float_sum += f;
                }
                _ => return Err(SemaError::type_error("number", item.type_name())),
            }
        }
        if has_float {
            Ok(Value::Float(float_sum))
        } else {
            Ok(Value::Int(int_sum))
        }
    });

    register_fn(env, "list/min", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list/min", "1", args.len()));
        }
        let items = get_sequence(&args[0], "list/min")?;
        if items.is_empty() {
            return Err(SemaError::eval("list/min: empty list"));
        }
        let mut result = items[0].clone();
        for item in &items[1..] {
            if num_lt(item, &result)? {
                result = item.clone();
            }
        }
        Ok(result)
    });

    register_fn(env, "list/max", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list/max", "1", args.len()));
        }
        let items = get_sequence(&args[0], "list/max")?;
        if items.is_empty() {
            return Err(SemaError::eval("list/max: empty list"));
        }
        let mut result = items[0].clone();
        for item in &items[1..] {
            if num_lt(&result, item)? {
                result = item.clone();
            }
        }
        Ok(result)
    });

    register_fn(env, "list/pick", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list/pick", "1", args.len()));
        }
        let items = get_sequence(&args[0], "list/pick")?;
        if items.is_empty() {
            return Err(SemaError::eval("list/pick: empty list"));
        }
        use rand::seq::IndexedRandom;
        let chosen = items.choose(&mut rand::rng()).unwrap();
        Ok(chosen.clone())
    });

    register_fn(env, "list/repeat", repeat_impl);
    register_fn(env, "make-list", repeat_impl);

    register_fn(env, "iota", |args| {
        let (count, start, step) = match args.len() {
            1 => {
                let c = args[0]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
                (c, 0i64, 1i64)
            }
            2 => {
                let c = args[0]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
                let s = args[1]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
                (c, s, 1)
            }
            3 => {
                let c = args[0]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
                let s = args[1]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
                let st = args[2]
                    .as_int()
                    .ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?;
                (c, s, st)
            }
            _ => return Err(SemaError::arity("iota", "1-3", args.len())),
        };
        let mut result = Vec::with_capacity(count.max(0) as usize);
        let mut val = start;
        for _ in 0..count {
            result.push(Value::Int(val));
            val += step;
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

fn num_lt(a: &Value, b: &Value) -> Result<bool, SemaError> {
    match (a, b) {
        (Value::Int(a), Value::Int(b)) => Ok(a < b),
        (Value::Float(a), Value::Float(b)) => Ok(a < b),
        (Value::Int(a), Value::Float(b)) => Ok((*a as f64) < *b),
        (Value::Float(a), Value::Int(b)) => Ok(*a < (*b as f64)),
        _ => Err(SemaError::type_error("number", a.type_name())),
    }
}

/// Call a Sema function (lambda or native) with given args.
/// Delegates to the real evaluator via the registered callback.
pub fn call_function(func: &Value, args: &[Value]) -> Result<Value, SemaError> {
    match func {
        Value::NativeFn(native) => sema_core::with_stdlib_ctx(|ctx| (native.func)(ctx, args)),
        _ => sema_core::with_stdlib_ctx(|ctx| sema_core::call_callback(ctx, func, args)),
    }
}
