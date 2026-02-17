use std::collections::BTreeMap;
use std::rc::Rc;

use hashbrown::HashMap as HBHashMap;
use sema_core::{SemaError, Value, ValueView};

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
        Ok(Value::map(map))
    });

    register_fn(env, "get", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("get", "2-3", args.len()));
        }
        let default = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::nil()
        };
        match args[0].view() {
            ValueView::Map(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            ValueView::HashMap(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "assoc", |args| {
        // Scheme alist lookup: (assoc key alist)
        if args.len() == 2 {
            if let Some(items) = args[1].as_list_rc() {
                let key = &args[0];
                for pair in items.iter() {
                    if let Some(p) = pair.as_list_rc() {
                        if !p.is_empty() && &p[0] == key {
                            return Ok(pair.clone());
                        }
                    }
                }
                return Ok(Value::bool(false));
            }
        }
        // Clojure-style map assoc: (assoc map key val ...)
        if args.len() < 3 || args.len() % 2 != 1 {
            return Err(SemaError::eval(
                "assoc: requires (key alist) or (map key val ...)",
            ));
        }
        // COW (copy-on-write) optimization: if this is the only reference to the map
        // (Rc refcount == 1), Rc::try_unwrap gives us ownership and we mutate in place.
        // Otherwise we clone. In practice, when used with Env::take() in accumulator
        // patterns (e.g., 1BRC's fold-lines), the refcount is almost always 1, turning
        // O(n) map clones into O(1) in-place inserts. This was the single biggest
        // optimization in the 1BRC benchmark (~30% of the total speedup).
        match args[0].view() {
            ValueView::Map(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                for pair in args[1..].chunks(2) {
                    map.insert(pair[0].clone(), pair[1].clone());
                }
                Ok(Value::map(map))
            }
            ValueView::HashMap(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                for pair in args[1..].chunks(2) {
                    map.insert(pair[0].clone(), pair[1].clone());
                }
                Ok(Value::hashmap_from_rc(Rc::new(map)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "dissoc", |args| {
        if args.len() < 2 {
            return Err(SemaError::arity("dissoc", "2+", args.len()));
        }
        match args[0].view() {
            ValueView::Map(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                for key in &args[1..] {
                    map.remove(key);
                }
                Ok(Value::map(map))
            }
            ValueView::HashMap(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                for key in &args[1..] {
                    map.remove(key);
                }
                Ok(Value::hashmap_from_rc(Rc::new(map)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "keys", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("keys", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Map(map) => Ok(Value::list(map.keys().cloned().collect())),
            ValueView::HashMap(map) => Ok(Value::list(map.keys().cloned().collect())),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "vals", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("vals", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Map(map) => Ok(Value::list(map.values().cloned().collect())),
            ValueView::HashMap(map) => Ok(Value::list(map.values().cloned().collect())),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "merge", |args| {
        if args.is_empty() {
            return Ok(Value::map(BTreeMap::new()));
        }
        match args[0].view() {
            ValueView::HashMap(_) => {
                let mut result = HBHashMap::new();
                for arg in args {
                    match arg.view() {
                        ValueView::HashMap(m) => {
                            for (k, v) in m.iter() {
                                result.insert(k.clone(), v.clone());
                            }
                        }
                        ValueView::Map(m) => {
                            for (k, v) in m.iter() {
                                result.insert(k.clone(), v.clone());
                            }
                        }
                        _ => return Err(SemaError::type_error("map or hashmap", arg.type_name())),
                    }
                }
                Ok(Value::hashmap_from_rc(Rc::new(result)))
            }
            ValueView::Map(_) => {
                let mut result = BTreeMap::new();
                for arg in args {
                    match arg.view() {
                        ValueView::Map(m) => {
                            for (k, v) in m.iter() {
                                result.insert(k.clone(), v.clone());
                            }
                        }
                        ValueView::HashMap(m) => {
                            for (k, v) in m.iter() {
                                result.insert(k.clone(), v.clone());
                            }
                        }
                        _ => return Err(SemaError::type_error("map or hashmap", arg.type_name())),
                    }
                }
                Ok(Value::map(result))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "contains?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("contains?", "2", args.len()));
        }
        match args[0].view() {
            ValueView::Map(map) => Ok(Value::bool(map.contains_key(&args[1]))),
            ValueView::HashMap(map) => Ok(Value::bool(map.contains_key(&args[1]))),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "count", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("count", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Map(m) => Ok(Value::int(m.len() as i64)),
            ValueView::HashMap(m) => Ok(Value::int(m.len() as i64)),
            ValueView::List(l) => Ok(Value::int(l.len() as i64)),
            ValueView::Vector(v) => Ok(Value::int(v.len() as i64)),
            ValueView::String(s) => Ok(Value::int(s.chars().count() as i64)),
            ValueView::Nil => Ok(Value::int(0)),
            _ => Err(SemaError::type_error("collection", args[0].type_name())),
        }
    });

    register_fn(env, "empty?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("empty?", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Map(m) => Ok(Value::bool(m.is_empty())),
            ValueView::HashMap(m) => Ok(Value::bool(m.is_empty())),
            ValueView::List(l) => Ok(Value::bool(l.is_empty())),
            ValueView::Vector(v) => Ok(Value::bool(v.is_empty())),
            ValueView::String(s) => Ok(Value::bool(s.is_empty())),
            ValueView::Nil => Ok(Value::bool(true)),
            _ => Err(SemaError::type_error("collection", args[0].type_name())),
        }
    });

    register_fn(env, "map/entries", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("map/entries", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Map(map) => {
                let entries: Vec<Value> = map
                    .iter()
                    .map(|(k, v)| Value::list(vec![k.clone(), v.clone()]))
                    .collect();
                Ok(Value::list(entries))
            }
            ValueView::HashMap(map) => {
                let mut entries: Vec<_> = map.iter().collect();
                entries.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));
                let entries: Vec<Value> = entries
                    .into_iter()
                    .map(|(k, v)| Value::list(vec![k.clone(), v.clone()]))
                    .collect();
                Ok(Value::list(entries))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "map/map-vals", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/map-vals", "2", args.len()));
        }
        match args[1].view() {
            ValueView::Map(m) => {
                let mut result = BTreeMap::new();
                for (k, v) in m.iter() {
                    let new_v = call_function(&args[0], &[v.clone()])?;
                    result.insert(k.clone(), new_v);
                }
                Ok(Value::map(result))
            }
            ValueView::HashMap(m) => {
                let mut result = HBHashMap::with_capacity(m.len());
                for (k, v) in m.iter() {
                    let new_v = call_function(&args[0], &[v.clone()])?;
                    result.insert(k.clone(), new_v);
                }
                Ok(Value::hashmap_from_rc(Rc::new(result)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[1].type_name())),
        }
    });

    register_fn(env, "map/filter", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/filter", "2", args.len()));
        }
        match args[1].view() {
            ValueView::Map(m) => {
                let mut result = BTreeMap::new();
                for (k, v) in m.iter() {
                    let keep = call_function(&args[0], &[k.clone(), v.clone()])?;
                    if keep.is_truthy() {
                        result.insert(k.clone(), v.clone());
                    }
                }
                Ok(Value::map(result))
            }
            ValueView::HashMap(m) => {
                let mut result = HBHashMap::new();
                for (k, v) in m.iter() {
                    let keep = call_function(&args[0], &[k.clone(), v.clone()])?;
                    if keep.is_truthy() {
                        result.insert(k.clone(), v.clone());
                    }
                }
                Ok(Value::hashmap_from_rc(Rc::new(result)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[1].type_name())),
        }
    });

    register_fn(env, "map/select-keys", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/select-keys", "2", args.len()));
        }
        let keys = match args[1].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list", args[1].type_name())),
        };
        match args[0].view() {
            ValueView::Map(map) => {
                let mut result = BTreeMap::new();
                for key in &keys {
                    if let Some(val) = map.get(key) {
                        result.insert(key.clone(), val.clone());
                    }
                }
                Ok(Value::map(result))
            }
            ValueView::HashMap(map) => {
                let mut result = HBHashMap::new();
                for key in &keys {
                    if let Some(val) = map.get(key) {
                        result.insert(key.clone(), val.clone());
                    }
                }
                Ok(Value::hashmap_from_rc(Rc::new(result)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "map/map-keys", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/map-keys", "2", args.len()));
        }
        match args[1].view() {
            ValueView::Map(m) => {
                let mut result = BTreeMap::new();
                for (k, v) in m.iter() {
                    let new_k = call_function(&args[0], &[k.clone()])?;
                    result.insert(new_k, v.clone());
                }
                Ok(Value::map(result))
            }
            ValueView::HashMap(m) => {
                let mut result = HBHashMap::with_capacity(m.len());
                for (k, v) in m.iter() {
                    let new_k = call_function(&args[0], &[k.clone()])?;
                    result.insert(new_k, v.clone());
                }
                Ok(Value::hashmap_from_rc(Rc::new(result)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[1].type_name())),
        }
    });

    register_fn(env, "map/from-entries", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("map/from-entries", "1", args.len()));
        }
        let entries = match args[0].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };
        let mut map = BTreeMap::new();
        for entry in &entries {
            let pair = match entry.view() {
                ValueView::List(l) => l.as_ref().clone(),
                ValueView::Vector(v) => v.as_ref().clone(),
                _ => return Err(SemaError::type_error("list or vector", entry.type_name())),
            };
            if pair.len() != 2 {
                return Err(SemaError::eval(
                    "map/from-entries: each entry must be a pair (key value)",
                ));
            }
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::map(map))
    });

    register_fn(env, "map/update", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("map/update", "3", args.len()));
        }
        match args[0].view() {
            ValueView::Map(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                let key = &args[1];
                let current = map.get(key).cloned().unwrap_or(Value::nil());
                let new_val = call_function(&args[2], &[current])?;
                map.insert(key.clone(), new_val);
                Ok(Value::map(map))
            }
            ValueView::HashMap(m) => {
                let mut map = match Rc::try_unwrap(m) {
                    Ok(map) => map,
                    Err(m) => m.as_ref().clone(),
                };
                let key = &args[1];
                let current = map.get(key).cloned().unwrap_or(Value::nil());
                let new_val = call_function(&args[2], &[current])?;
                map.insert(key.clone(), new_val);
                Ok(Value::hashmap_from_rc(Rc::new(map)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

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
        Ok(Value::hashmap_from_rc(Rc::new(map)))
    });

    register_fn(env, "hashmap/get", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("hashmap/get", "2-3", args.len()));
        }
        let default = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::nil()
        };
        match args[0].view() {
            ValueView::HashMap(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            ValueView::Map(map) => Ok(map.get(&args[1]).cloned().unwrap_or(default)),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "hashmap/assoc", |args| {
        if args.len() < 3 || args.len() % 2 != 1 {
            return Err(SemaError::eval(
                "hashmap/assoc: requires hashmap and even number of key-value pairs",
            ));
        }
        let mut map = match args[0].view() {
            ValueView::HashMap(m) => match Rc::try_unwrap(m) {
                Ok(map) => map,
                Err(m) => m.as_ref().clone(),
            },
            _ => return Err(SemaError::type_error("hashmap", args[0].type_name())),
        };
        for pair in args[1..].chunks(2) {
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::hashmap_from_rc(Rc::new(map)))
    });

    register_fn(env, "hashmap/to-map", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("hashmap/to-map", "1", args.len()));
        }
        match args[0].view() {
            ValueView::HashMap(hm) => {
                let map: BTreeMap<Value, Value> =
                    hm.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                Ok(Value::map(map))
            }
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "hashmap/keys", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("hashmap/keys", "1", args.len()));
        }
        match args[0].view() {
            ValueView::HashMap(map) => Ok(Value::list(map.keys().cloned().collect())),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "hashmap/contains?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("hashmap/contains?", "2", args.len()));
        }
        match args[0].view() {
            ValueView::HashMap(map) => Ok(Value::bool(map.contains_key(&args[1]))),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "map/sort-keys", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("map/sort-keys", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Map(m) => Ok(Value::map(m.as_ref().clone())),
            ValueView::HashMap(m) => {
                let sorted: BTreeMap<Value, Value> =
                    m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                Ok(Value::map(sorted))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "map/except", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/except", "2", args.len()));
        }
        let keys_to_remove = match args[1].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
        };
        let key_set: std::collections::BTreeSet<Value> = keys_to_remove.into_iter().collect();
        match args[0].view() {
            ValueView::Map(m) => {
                let mut result = BTreeMap::new();
                for (k, v) in m.iter() {
                    if !key_set.contains(k) {
                        result.insert(k.clone(), v.clone());
                    }
                }
                Ok(Value::map(result))
            }
            ValueView::HashMap(m) => {
                let mut result = HBHashMap::new();
                for (k, v) in m.iter() {
                    if !key_set.contains(k) {
                        result.insert(k.clone(), v.clone());
                    }
                }
                Ok(Value::hashmap_from_rc(Rc::new(result)))
            }
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "map/zip", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("map/zip", "2", args.len()));
        }
        let keys = match args[0].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };
        let vals = match args[1].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
        };
        let mut map = BTreeMap::new();
        for (k, v) in keys.into_iter().zip(vals.into_iter()) {
            map.insert(k, v);
        }
        Ok(Value::map(map))
    });
}
