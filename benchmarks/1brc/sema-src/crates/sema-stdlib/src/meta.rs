use std::cell::Cell;

use sema_core::{SemaError, Value};

use crate::register_fn;

thread_local! {
    static GENSYM_COUNTER: Cell<u64> = const { Cell::new(0) };
}

pub fn register(env: &sema_core::Env) {
    register_fn(env, "gensym", |args| {
        if args.len() > 1 {
            return Err(SemaError::arity("gensym", "0 or 1", args.len()));
        }
        let prefix = if args.len() == 1 {
            args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?
                .to_string()
        } else {
            "g".to_string()
        };
        let n = GENSYM_COUNTER.with(|c| {
            let val = c.get();
            c.set(val + 1);
            val
        });
        Ok(Value::symbol(&format!("{prefix}__{n}")))
    });
}
