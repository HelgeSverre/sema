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
    crate::register_fn_path_gated(
        env,
        sandbox,
        sema_core::Caps::FS_WRITE,
        "kv/open",
        &[1],
        |args| {
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
                // TODO: consider returning an error (or at least a warning) instead of
                // silently falling back to an empty store when the file contains malformed JSON.
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
        },
    );

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
                Some(v) => Ok(sema_core::json_to_value(v)),
                None => Ok(Value::nil()),
            }
        })
    });

    // NOTE: kv/set and kv/delete write to the path stored at kv/open time without
    // re-checking allowed_paths. This is intentional — the path was validated at open,
    // and flush_store uses std::fs::write which recreates the file if deleted (correct
    // for a KV store). If path sandboxing needs to be stricter (e.g., re-validating on
    // every write to guard against the backing file being replaced with a symlink to an
    // outside path), the stored path should be canonicalized at open time and re-checked
    // in flush_store.
    crate::register_fn_gated(env, sandbox, sema_core::Caps::FS_WRITE, "kv/set", |args| {
        check_arity!(args, "kv/set", 3);
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let key = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let val = sema_core::value_to_json_lossy(&args[2]);
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
