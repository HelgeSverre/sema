use std::cell::RefCell;
use std::collections::HashMap;

use sema_core::{check_arity, SemaError, Value};

struct KvStore {
    path: String,
    data: serde_json::Map<String, serde_json::Value>,
}

thread_local! {
    static KV_STORES: RefCell<HashMap<String, KvStore>> = RefCell::new(HashMap::new());
}

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    crate::register_fn_gated(env, sandbox, sema_core::Caps::FS_WRITE, "kv/open", |args| {
        check_arity!(args, "kv/open", 2);
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let path = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let data = if std::path::Path::new(path).exists() {
            let content = std::fs::read_to_string(path)
                .map_err(|e| SemaError::Io(format!("kv/open: {e}")))?;
            serde_json::from_str::<serde_json::Map<String, serde_json::Value>>(&content)
                .unwrap_or_default()
        } else {
            serde_json::Map::new()
        };
        KV_STORES.with(|s| {
            s.borrow_mut().insert(
                name.to_string(),
                KvStore {
                    path: path.to_string(),
                    data,
                },
            )
        });
        Ok(Value::string(name))
    });

    crate::register_fn(env, "kv/get", |args| {
        check_arity!(args, "kv/get", 2);
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let key = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        KV_STORES.with(|s| {
            let s = s.borrow();
            let store = s
                .get(name)
                .ok_or_else(|| SemaError::eval(format!("kv store '{}' not open", name)))?;
            match store.data.get(key) {
                Some(v) => Ok(json_val_to_sema(v)),
                None => Ok(Value::nil()),
            }
        })
    });

    crate::register_fn_gated(env, sandbox, sema_core::Caps::FS_WRITE, "kv/set", |args| {
        check_arity!(args, "kv/set", 3);
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let key = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let val = sema_to_json_val(&args[2]);
        KV_STORES.with(|s| {
            let mut s = s.borrow_mut();
            let store = s
                .get_mut(name)
                .ok_or_else(|| SemaError::eval(format!("kv store '{}' not open", name)))?;
            store.data.insert(key.to_string(), val);
            flush_store(store)
        })?;
        Ok(args[2].clone())
    });

    crate::register_fn_gated(
        env,
        sandbox,
        sema_core::Caps::FS_WRITE,
        "kv/delete",
        |args| {
            check_arity!(args, "kv/delete", 2);
            let name = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let key = args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            KV_STORES.with(|s| {
                let mut s = s.borrow_mut();
                let store = s
                    .get_mut(name)
                    .ok_or_else(|| SemaError::eval(format!("kv store '{}' not open", name)))?;
                let existed = store.data.remove(key).is_some();
                flush_store(store)?;
                Ok(Value::bool(existed))
            })
        },
    );

    crate::register_fn(env, "kv/keys", |args| {
        check_arity!(args, "kv/keys", 1);
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        KV_STORES.with(|s| {
            let s = s.borrow();
            let store = s
                .get(name)
                .ok_or_else(|| SemaError::eval(format!("kv store '{}' not open", name)))?;
            Ok(Value::list(
                store.data.keys().map(|k| Value::string(k)).collect(),
            ))
        })
    });

    crate::register_fn(env, "kv/close", |args| {
        check_arity!(args, "kv/close", 1);
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        KV_STORES.with(|s| {
            let mut s = s.borrow_mut();
            if let Some(store) = s.get(name) {
                let _ = flush_store_ref(store);
            }
            s.remove(name);
        });
        Ok(Value::nil())
    });
}

fn flush_store(store: &KvStore) -> Result<(), SemaError> {
    let json = serde_json::to_string_pretty(&store.data)
        .map_err(|e| SemaError::Io(format!("kv/flush: {e}")))?;
    std::fs::write(&store.path, json).map_err(|e| SemaError::Io(format!("kv/flush: {e}")))?;
    Ok(())
}

fn flush_store_ref(store: &KvStore) -> Result<(), SemaError> {
    flush_store(store)
}

fn sema_to_json_val(val: &Value) -> serde_json::Value {
    if val.is_nil() {
        return serde_json::Value::Null;
    }
    if let Some(b) = val.as_bool() {
        return serde_json::Value::Bool(b);
    }
    if let Some(i) = val.as_int() {
        return serde_json::Value::Number(i.into());
    }
    if let Some(f) = val.as_float() {
        return serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null);
    }
    if let Some(s) = val.as_str() {
        return serde_json::Value::String(s.to_string());
    }
    if let Some(l) = val.as_list() {
        return serde_json::Value::Array(l.iter().map(sema_to_json_val).collect());
    }
    if let Some(m) = val.as_map_rc() {
        let mut obj = serde_json::Map::new();
        for (k, v) in m.iter() {
            let key = k
                .as_keyword()
                .or_else(|| k.as_str().map(|s| s.to_string()))
                .unwrap_or_else(|| k.to_string());
            obj.insert(key, sema_to_json_val(v));
        }
        return serde_json::Value::Object(obj);
    }
    serde_json::Value::String(val.to_string())
}

fn json_val_to_sema(json: &serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::nil(),
        serde_json::Value::Bool(b) => Value::bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::int(i)
            } else if let Some(f) = n.as_f64() {
                Value::float(f)
            } else {
                Value::nil()
            }
        }
        serde_json::Value::String(s) => Value::string(s),
        serde_json::Value::Array(a) => Value::list(a.iter().map(json_val_to_sema).collect()),
        serde_json::Value::Object(o) => {
            let mut map = std::collections::BTreeMap::new();
            for (k, v) in o {
                map.insert(Value::keyword(k), json_val_to_sema(v));
            }
            Value::map(map)
        }
    }
}
