use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{SemaError, Value};

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
            _ => Err(SemaError::type_error("map", args[0].type_name())),
        }
    });

    register_fn(env, "assoc", |args| {
        if args.len() < 3 || args.len() % 2 != 1 {
            return Err(SemaError::eval(
                "assoc: requires map and even number of key-value pairs",
            ));
        }
        let mut map = match &args[0] {
            Value::Map(m) => m.as_ref().clone(),
            _ => return Err(SemaError::type_error("map", args[0].type_name())),
        };
        for pair in args[1..].chunks(2) {
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::Map(Rc::new(map)))
    });

    register_fn(env, "dissoc", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("dissoc", "2+", args.len()));
        }
        let mut map = match &args[0] {
            Value::Map(m) => m.as_ref().clone(),
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
            _ => Err(SemaError::type_error("map", args[0].type_name())),
        }
    });

    register_fn(env, "vals", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("vals", "1", args.len()));
        }
        match &args[0] {
            Value::Map(map) => Ok(Value::list(map.values().cloned().collect())),
            _ => Err(SemaError::type_error("map", args[0].type_name())),
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
            _ => Err(SemaError::type_error("map", args[0].type_name())),
        }
    });

    register_fn(env, "count", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("count", "1", args.len()));
        }
        match &args[0] {
            Value::Map(m) => Ok(Value::Int(m.len() as i64)),
            Value::List(l) => Ok(Value::Int(l.len() as i64)),
            Value::Vector(v) => Ok(Value::Int(v.len() as i64)),
            Value::String(s) => Ok(Value::Int(s.len() as i64)),
            Value::Nil => Ok(Value::Int(0)),
            _ => Err(SemaError::type_error(
                "collection",
                args[0].type_name(),
            )),
        }
    });

    register_fn(env, "empty?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("empty?", "1", args.len()));
        }
        match &args[0] {
            Value::Map(m) => Ok(Value::Bool(m.is_empty())),
            Value::List(l) => Ok(Value::Bool(l.is_empty())),
            Value::Vector(v) => Ok(Value::Bool(v.is_empty())),
            Value::String(s) => Ok(Value::Bool(s.is_empty())),
            Value::Nil => Ok(Value::Bool(true)),
            _ => Err(SemaError::type_error(
                "collection",
                args[0].type_name(),
            )),
        }
    });
}
