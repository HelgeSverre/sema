use std::collections::BTreeMap;
use std::rc::Rc;

use hashbrown::HashMap as HBHashMap;
use sema_core::{SemaError, Value};

use crate::list::call_function;
use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "hash-map", |args| {
        if args.len() % 2 != 0 {
            return Err(SemaError::eval(
                "hash-map: requires even number of arguments",
            ));
        }
        let mut map = BTreeMap::new();
        for pair in args.chunks(2) {
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::Map(Rc::new(map)))
    });

    register_fn(env, "get", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("get", "2-3", args.len()));
        }
        let default = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::Nil
        };
        match &args[0] {
            Value::Map(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            Value::HashMap(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "assoc", |args| {
        if args.len() < 3 || args.len() % 2 != 1 {
            return Err(SemaError::eval(
                "assoc: requires map and even number of key-value pairs",
            ));
        }
        match args[0].clone() {
            Value::Map(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                for pair in args[1..].chunks(2) {
                    map.insert(pair[0].clone(), pair[1].clone());
                }
                Ok(Value::Map(Rc::new(map)))
            }
            Value::HashMap(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                for pair in args[1..].chunks(2) {
                    map.insert(pair[0].clone(), pair[1].clone());
                }
                Ok(Value::HashMap(Rc::new(map)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "dissoc", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("dissoc", "2+", args.len()));
        }
        let mut map = match args[0].clone() {
            Value::Map(m) => match Rc::try_unwrap(m) {
                Ok(map) => map,
                Err(m) => m.as_ref().clone(),
            },
            _ => return Err(SemaError::type_error("map", args[0].type_name())),
        };
        for key in &args[1..] {
            map.remove(key);
        }
        Ok(Value::Map(Rc::new(map)))
    });

    register_fn(env, "keys", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("keys", "1", args.len()));
        }
        match &args[0] {
            Value::Map(map) => Ok(Value::list(map.keys().cloned().collect())),
            Value::HashMap(map) => Ok(Value::list(map.keys().cloned().collect())),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "vals", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("vals", "1", args.len()));
        }
        match &args[0] {
            Value::Map(map) => Ok(Value::list(map.values().cloned().collect())),
            Value::HashMap(map) => Ok(Value::list(map.values().cloned().collect())),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "merge", |args| {
        let mut result = BTreeMap::new();
        for arg in args {
            match arg {
                Value::Map(m) => {
                    for (k, v) in m.iter() {
                        result.insert(k.clone(), v.clone());
                    }
                }
                _ => return Err(SemaError::type_error("map", arg.type_name())),
            }
        }
        Ok(Value::Map(Rc::new(result)))
    });

    register_fn(env, "contains?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("contains?", "2", args.len()));
        }
        match &args[0] {
            Value::Map(map) => Ok(Value::Bool(map.contains_key(&args[1]))),
            Value::HashMap(map) => Ok(Value::Bool(map.contains_key(&args[1]))),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "count", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("count", "1", args.len()));
        }
        match &args[0] {
            Value::Map(m) => Ok(Value::Int(m.len() as i64)),
            Value::HashMap(m) => Ok(Value::Int(m.len() as i64)),
            Value::List(l) => Ok(Value::Int(l.len() as i64)),
            Value::Vector(v) => Ok(Value::Int(v.len() as i64)),
            Value::String(s) => Ok(Value::Int(s.len() as i64)),
            Value::Nil => Ok(Value::Int(0)),
            _ => Err(SemaError::type_error("collection", args[0].type_name())),
        }
    });

    register_fn(env, "empty?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("empty?", "1", args.len()));
        }
        match &args[0] {
            Value::Map(m) => Ok(Value::Bool(m.is_empty())),
            Value::HashMap(m) => Ok(Value::Bool(m.is_empty())),
            Value::List(l) => Ok(Value::Bool(l.is_empty())),
            Value::Vector(v) => Ok(Value::Bool(v.is_empty())),
            Value::String(s) => Ok(Value::Bool(s.is_empty())),
            Value::Nil => Ok(Value::Bool(true)),
            _ => Err(SemaError::type_error("collection", args[0].type_name())),
        }
    });

    register_fn(env, "map/entries", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("map/entries", "1", args.len()));
        }
        match &args[0] {
            Value::Map(map) => {
                let entries: Vec<Value> = map
                    .iter()
                    .map(|(k, v)| Value::list(vec![k.clone(), v.clone()]))
                    .collect();
                Ok(Value::list(entries))
            }
            _ => Err(SemaError::type_error("map", args[0].type_name())),
        }
    });

    register_fn(env, "map/map-vals", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/map-vals", "2", args.len()));
        }
        let map = match &args[1] {
            Value::Map(m) => m.as_ref(),
            _ => return Err(SemaError::type_error("map", args[1].type_name())),
        };
        let mut result = BTreeMap::new();
        for (k, v) in map.iter() {
            let new_v = call_function(&args[0], &[v.clone()])?;
            result.insert(k.clone(), new_v);
        }
        Ok(Value::Map(Rc::new(result)))
    });

    register_fn(env, "map/filter", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/filter", "2", args.len()));
        }
        let map = match &args[1] {
            Value::Map(m) => m.as_ref(),
            _ => return Err(SemaError::type_error("map", args[1].type_name())),
        };
        let mut result = BTreeMap::new();
        for (k, v) in map.iter() {
            let keep = call_function(&args[0], &[k.clone(), v.clone()])?;
            if keep.is_truthy() {
                result.insert(k.clone(), v.clone());
            }
        }
        Ok(Value::Map(Rc::new(result)))
    });

    register_fn(env, "map/select-keys", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/select-keys", "2", args.len()));
        }
        let map = match &args[0] {
            Value::Map(m) => m.as_ref(),
            _ => return Err(SemaError::type_error("map", args[0].type_name())),
        };
        let keys = match &args[1] {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list", args[1].type_name())),
        };
        let mut result = BTreeMap::new();
        for key in &keys {
            if let Some(val) = map.get(key) {
                result.insert(key.clone(), val.clone());
            }
        }
        Ok(Value::Map(Rc::new(result)))
    });

    register_fn(env, "map/map-keys", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/map-keys", "2", args.len()));
        }
        let map = match &args[1] {
            Value::Map(m) => m.as_ref(),
            _ => return Err(SemaError::type_error("map", args[1].type_name())),
        };
        let mut result = BTreeMap::new();
        for (k, v) in map.iter() {
            let new_k = call_function(&args[0], &[k.clone()])?;
            result.insert(new_k, v.clone());
        }
        Ok(Value::Map(Rc::new(result)))
    });

    register_fn(env, "map/from-entries", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("map/from-entries", "1", args.len()));
        }
        let entries = match &args[0] {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };
        let mut map = BTreeMap::new();
        for entry in &entries {
            let pair = match entry {
                Value::List(l) => l.as_ref().clone(),
                Value::Vector(v) => v.as_ref().clone(),
                _ => return Err(SemaError::type_error("list or vector", entry.type_name())),
            };
            if pair.len() != 2 {
                return Err(SemaError::eval(
                    "map/from-entries: each entry must be a pair (key value)",
                ));
            }
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::Map(Rc::new(map)))
    });

    register_fn(env, "map/update", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("map/update", "3", args.len()));
        }
        let mut map = match &args[0] {
            Value::Map(m) => m.as_ref().clone(),
            _ => return Err(SemaError::type_error("map", args[0].type_name())),
        };
        let key = &args[1];
        let current = map.get(key).cloned().unwrap_or(Value::Nil);
        let new_val = call_function(&args[2], &[current])?;
        map.insert(key.clone(), new_val);
        Ok(Value::Map(Rc::new(map)))
    });

    // --- hashmap builtins ---

    register_fn(env, "hashmap/new", |args| {
        if args.len() % 2 != 0 {
            return Err(SemaError::eval(
                "hashmap/new: requires even number of arguments",
            ));
        }
        let mut map = HBHashMap::with_capacity(args.len() / 2);
        for pair in args.chunks(2) {
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::HashMap(Rc::new(map)))
    });

    register_fn(env, "hashmap/get", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("hashmap/get", "2-3", args.len()));
        }
        let default = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::Nil
        };
        match &args[0] {
            Value::HashMap(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            Value::Map(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "hashmap/assoc", |args| {
        if args.len() < 3 || args.len() % 2 != 1 {
            return Err(SemaError::eval(
                "hashmap/assoc: requires hashmap and even number of key-value pairs",
            ));
        }
        let mut map = match args[0].clone() {
            Value::HashMap(m) => match Rc::try_unwrap(m) {
                Ok(map) => map,
                Err(m) => m.as_ref().clone(),
            },
            _ => return Err(SemaError::type_error("hashmap", args[0].type_name())),
        };
        for pair in args[1..].chunks(2) {
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::HashMap(Rc::new(map)))
    });

    register_fn(env, "hashmap/to-map", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("hashmap/to-map", "1", args.len()));
        }
        match &args[0] {
            Value::HashMap(hm) => {
                let map: BTreeMap<Value, Value> = hm.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                Ok(Value::Map(Rc::new(map)))
            }
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "hashmap/keys", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("hashmap/keys", "1", args.len()));
        }
        match &args[0] {
            Value::HashMap(map) => Ok(Value::list(map.keys().cloned().collect())),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "hashmap/contains?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("hashmap/contains?", "2", args.len()));
        }
        match &args[0] {
            Value::HashMap(map) => Ok(Value::Bool(map.contains_key(&args[1]))),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });
}
