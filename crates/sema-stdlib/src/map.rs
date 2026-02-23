use std::collections::BTreeMap;
use std::rc::Rc;

use hashbrown::HashMap as HBHashMap;
use sema_core::{check_arity, SemaError, Value, ValueView};

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
        check_arity!(args, "get", 2..=3);
        let default = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::nil()
        };
        if let Some(map) = args[0].as_hashmap_ref() {
            return Ok(map.get(&args[1]).cloned().unwrap_or(default));
        }
        if let Some(map) = args[0].as_map_ref() {
            return Ok(map.get(&args[1]).cloned().unwrap_or(default));
        }
        Err(SemaError::type_error("map or hashmap", args[0].type_name()))
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
        // COW fast path: if refcount==1, mutate in place without any Rc clone.
        // This avoids the refcount inflation from view()/as_*_rc() that prevented
        // try_unwrap from succeeding when args are borrowed (&[Value]).
        if let Some(()) = args[0].with_hashmap_mut_if_unique(|map| {
            for pair in args[1..].chunks(2) {
                map.insert(pair[0].clone(), pair[1].clone());
            }
        }) {
            return Ok(args[0].clone());
        }
        if let Some(()) = args[0].with_map_mut_if_unique(|map| {
            for pair in args[1..].chunks(2) {
                map.insert(pair[0].clone(), pair[1].clone());
            }
        }) {
            return Ok(args[0].clone());
        }
        // Shared path: clone the map data
        if let Some(m) = args[0].as_hashmap_ref() {
            let mut map = m.clone();
            for pair in args[1..].chunks(2) {
                map.insert(pair[0].clone(), pair[1].clone());
            }
            return Ok(Value::hashmap_from_rc(Rc::new(map)));
        }
        if let Some(m) = args[0].as_map_ref() {
            let mut map = m.clone();
            for pair in args[1..].chunks(2) {
                map.insert(pair[0].clone(), pair[1].clone());
            }
            return Ok(Value::map(map));
        }
        Err(SemaError::type_error("map or hashmap", args[0].type_name()))
    });

    register_fn(env, "dissoc", |args| {
        check_arity!(args, "dissoc", 2..);
        if let Some(()) = args[0].with_hashmap_mut_if_unique(|map| {
            for key in &args[1..] {
                map.remove(key);
            }
        }) {
            return Ok(args[0].clone());
        }
        if let Some(()) = args[0].with_map_mut_if_unique(|map| {
            for key in &args[1..] {
                map.remove(key);
            }
        }) {
            return Ok(args[0].clone());
        }
        if let Some(m) = args[0].as_hashmap_ref() {
            let mut map = m.clone();
            for key in &args[1..] {
                map.remove(key);
            }
            return Ok(Value::hashmap_from_rc(Rc::new(map)));
        }
        if let Some(m) = args[0].as_map_ref() {
            let mut map = m.clone();
            for key in &args[1..] {
                map.remove(key);
            }
            return Ok(Value::map(map));
        }
        Err(SemaError::type_error("map or hashmap", args[0].type_name()))
    });

    register_fn(env, "keys", |args| {
        check_arity!(args, "keys", 1);
        match args[0].view() {
            ValueView::Map(map) => Ok(Value::list(map.keys().cloned().collect())),
            ValueView::HashMap(map) => Ok(Value::list(map.keys().cloned().collect())),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "vals", |args| {
        check_arity!(args, "vals", 1);
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
        check_arity!(args, "contains?", 2);
        match args[0].view() {
            ValueView::Map(map) => Ok(Value::bool(map.contains_key(&args[1]))),
            ValueView::HashMap(map) => Ok(Value::bool(map.contains_key(&args[1]))),
            _ => Err(SemaError::type_error("map or hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "count", |args| {
        check_arity!(args, "count", 1);
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
        check_arity!(args, "empty?", 1);
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
        check_arity!(args, "map/entries", 1);
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
        check_arity!(args, "map/map-vals", 2);
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
        check_arity!(args, "map/filter", 2);
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
        check_arity!(args, "map/select-keys", 2);
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
        check_arity!(args, "map/map-keys", 2);
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
        check_arity!(args, "map/from-entries", 1);
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
        check_arity!(args, "map/update", 3);
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
        check_arity!(args, "hashmap/get", 2..=3);
        let default = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::nil()
        };
        if let Some(map) = args[0].as_hashmap_ref() {
            return Ok(map.get(&args[1]).cloned().unwrap_or(default));
        }
        if let Some(map) = args[0].as_map_ref() {
            return Ok(map.get(&args[1]).cloned().unwrap_or(default));
        }
        Err(SemaError::type_error("hashmap", args[0].type_name()))
    });

    register_fn(env, "hashmap/assoc", |args| {
        if args.len() < 3 || args.len() % 2 != 1 {
            return Err(SemaError::eval(
                "hashmap/assoc: requires hashmap and even number of key-value pairs",
            ));
        }
        if let Some(()) = args[0].with_hashmap_mut_if_unique(|map| {
            for pair in args[1..].chunks(2) {
                map.insert(pair[0].clone(), pair[1].clone());
            }
        }) {
            return Ok(args[0].clone());
        }
        let mut map = if let Some(m) = args[0].as_hashmap_ref() {
            m.clone()
        } else {
            return Err(SemaError::type_error("hashmap", args[0].type_name()));
        };
        for pair in args[1..].chunks(2) {
            map.insert(pair[0].clone(), pair[1].clone());
        }
        Ok(Value::hashmap_from_rc(Rc::new(map)))
    });

    register_fn(env, "hashmap/to-map", |args| {
        check_arity!(args, "hashmap/to-map", 1);
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
        check_arity!(args, "hashmap/keys", 1);
        match args[0].view() {
            ValueView::HashMap(map) => Ok(Value::list(map.keys().cloned().collect())),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "hashmap/contains?", |args| {
        check_arity!(args, "hashmap/contains?", 2);
        match args[0].view() {
            ValueView::HashMap(map) => Ok(Value::bool(map.contains_key(&args[1]))),
            _ => Err(SemaError::type_error("hashmap", args[0].type_name())),
        }
    });

    register_fn(env, "map/sort-keys", |args| {
        check_arity!(args, "map/sort-keys", 1);
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
        check_arity!(args, "map/except", 2);
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
        check_arity!(args, "map/zip", 2);
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

    register_fn(env, "get-in", |args| {
        check_arity!(args, "get-in", 2..=3);
        let path = match args[1].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
        };
        let default = if args.len() == 3 {
            args[2].clone()
        } else {
            Value::nil()
        };
        let mut current = args[0].clone();
        for key in &path {
            if let Some(map) = current.as_map_ref() {
                current = map.get(key).cloned().unwrap_or(Value::nil());
            } else if let Some(map) = current.as_hashmap_ref() {
                current = map.get(key).cloned().unwrap_or(Value::nil());
            } else {
                return Ok(default);
            }
        }
        if current.is_nil() && args.len() == 3 {
            return Ok(default);
        }
        Ok(current)
    });

    register_fn(env, "assoc-in", |args| {
        check_arity!(args, "assoc-in", 3);
        let path = match args[1].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
        };
        if path.is_empty() {
            return Ok(args[2].clone());
        }
        fn assoc_in_recursive(m: &Value, path: &[Value], val: &Value) -> Result<Value, SemaError> {
            let key = &path[0];
            if path.len() == 1 {
                // Base case: set the value at this key
                if let Some(map) = m.as_map_ref() {
                    let mut map = map.clone();
                    map.insert(key.clone(), val.clone());
                    return Ok(Value::map(map));
                }
                if let Some(map) = m.as_hashmap_ref() {
                    let mut map = map.clone();
                    map.insert(key.clone(), val.clone());
                    return Ok(Value::hashmap_from_rc(Rc::new(map)));
                }
                // If not a map, create one
                let mut map = BTreeMap::new();
                map.insert(key.clone(), val.clone());
                return Ok(Value::map(map));
            }
            // Recursive case: get nested map, recurse, then assoc back
            let nested = if let Some(map) = m.as_map_ref() {
                map.get(key)
                    .cloned()
                    .unwrap_or_else(|| Value::map(BTreeMap::new()))
            } else if let Some(map) = m.as_hashmap_ref() {
                map.get(key)
                    .cloned()
                    .unwrap_or_else(|| Value::map(BTreeMap::new()))
            } else {
                Value::map(BTreeMap::new())
            };
            let new_nested = assoc_in_recursive(&nested, &path[1..], val)?;
            if let Some(map) = m.as_map_ref() {
                let mut map = map.clone();
                map.insert(key.clone(), new_nested);
                Ok(Value::map(map))
            } else if let Some(map) = m.as_hashmap_ref() {
                let mut map = map.clone();
                map.insert(key.clone(), new_nested);
                Ok(Value::hashmap_from_rc(Rc::new(map)))
            } else {
                let mut map = BTreeMap::new();
                map.insert(key.clone(), new_nested);
                Ok(Value::map(map))
            }
        }
        assoc_in_recursive(&args[0], &path, &args[2])
    });

    register_fn(env, "update-in", |args| {
        check_arity!(args, "update-in", 3);
        let path = match args[1].view() {
            ValueView::List(l) => l.as_ref().clone(),
            ValueView::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
        };
        if path.is_empty() {
            return call_function(&args[2], &[args[0].clone()]);
        }
        fn update_in_recursive(m: &Value, path: &[Value], f: &Value) -> Result<Value, SemaError> {
            let key = &path[0];
            if path.len() == 1 {
                let current = if let Some(map) = m.as_map_ref() {
                    map.get(key).cloned().unwrap_or(Value::nil())
                } else if let Some(map) = m.as_hashmap_ref() {
                    map.get(key).cloned().unwrap_or(Value::nil())
                } else {
                    Value::nil()
                };
                let new_val = call_function(f, &[current])?;
                if let Some(map) = m.as_map_ref() {
                    let mut map = map.clone();
                    map.insert(key.clone(), new_val);
                    return Ok(Value::map(map));
                }
                if let Some(map) = m.as_hashmap_ref() {
                    let mut map = map.clone();
                    map.insert(key.clone(), new_val);
                    return Ok(Value::hashmap_from_rc(Rc::new(map)));
                }
                let mut map = BTreeMap::new();
                map.insert(key.clone(), new_val);
                return Ok(Value::map(map));
            }
            let nested = if let Some(map) = m.as_map_ref() {
                map.get(key)
                    .cloned()
                    .unwrap_or_else(|| Value::map(BTreeMap::new()))
            } else if let Some(map) = m.as_hashmap_ref() {
                map.get(key)
                    .cloned()
                    .unwrap_or_else(|| Value::map(BTreeMap::new()))
            } else {
                Value::map(BTreeMap::new())
            };
            let new_nested = update_in_recursive(&nested, &path[1..], f)?;
            if let Some(map) = m.as_map_ref() {
                let mut map = map.clone();
                map.insert(key.clone(), new_nested);
                Ok(Value::map(map))
            } else if let Some(map) = m.as_hashmap_ref() {
                let mut map = map.clone();
                map.insert(key.clone(), new_nested);
                Ok(Value::hashmap_from_rc(Rc::new(map)))
            } else {
                let mut map = BTreeMap::new();
                map.insert(key.clone(), new_nested);
                Ok(Value::map(map))
            }
        }
        update_in_recursive(&args[0], &path, &args[2])
    });

    register_fn(env, "deep-merge", |args| {
        if args.is_empty() {
            return Ok(Value::map(BTreeMap::new()));
        }
        fn is_map(v: &Value) -> bool {
            v.as_map_ref().is_some() || v.as_hashmap_ref().is_some()
        }
        fn merge_two(base: &Value, overlay: &Value) -> Result<Value, SemaError> {
            // Collect all keys from both maps
            let base_entries: Vec<(Value, Value)> = if let Some(m) = base.as_map_ref() {
                m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
            } else if let Some(m) = base.as_hashmap_ref() {
                m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
            } else {
                return Ok(overlay.clone());
            };
            let overlay_entries: Vec<(Value, Value)> = if let Some(m) = overlay.as_map_ref() {
                m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
            } else if let Some(m) = overlay.as_hashmap_ref() {
                m.iter().map(|(k, v)| (k.clone(), v.clone())).collect()
            } else {
                return Ok(overlay.clone());
            };
            let mut result = BTreeMap::new();
            for (k, v) in &base_entries {
                result.insert(k.clone(), v.clone());
            }
            for (k, v) in &overlay_entries {
                if let Some(existing) = result.get(k) {
                    if is_map(existing) && is_map(v) {
                        result.insert(k.clone(), merge_two(existing, v)?);
                    } else {
                        result.insert(k.clone(), v.clone());
                    }
                } else {
                    result.insert(k.clone(), v.clone());
                }
            }
            Ok(Value::map(result))
        }
        let mut result = args[0].clone();
        for arg in &args[1..] {
            result = merge_two(&result, arg)?;
        }
        Ok(result)
    });

    // Silent aliases for other Lisp dialects (undocumented)
    if let Some(v) = env.get(sema_core::intern("map?")) {
        env.set(sema_core::intern("hash-map?"), v);
    }
    if let Some(v) = env.get(sema_core::intern("get")) {
        env.set(sema_core::intern("hash-ref"), v);
    }
}
