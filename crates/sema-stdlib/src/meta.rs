use std::cell::Cell;

use sema_core::{check_arity, SemaError, Value};

use crate::register_fn;

thread_local! {
    static GENSYM_COUNTER: Cell<u64> = const { Cell::new(0) };
}

pub fn register(env: &sema_core::Env) {
    // (retry thunk) or (retry thunk {:max-attempts 3 :base-delay-ms 100 :backoff 2.0})
    register_fn(env, "retry", |args| {
        check_arity!(args, "retry", 1..=2);
        let thunk = &args[0];
        if thunk.as_lambda_rc().is_none() && thunk.as_native_fn_rc().is_none() {
            return Err(SemaError::type_error("function", thunk.type_name()));
        }

        let mut max_attempts: u32 = 3;
        let mut base_delay_ms: u64 = 100;
        let mut backoff: f64 = 2.0;

        if let Some(opts) = args.get(1).and_then(|v| v.as_map_rc()) {
            if let Some(v) = opts
                .get(&Value::keyword("max-attempts"))
                .and_then(|v| v.as_int())
            {
                max_attempts = v.max(1) as u32;
            }
            if let Some(v) = opts
                .get(&Value::keyword("base-delay-ms"))
                .and_then(|v| v.as_int())
            {
                base_delay_ms = v.max(0) as u64;
            }
            if let Some(v) = opts
                .get(&Value::keyword("backoff"))
                .and_then(|v| v.as_float())
            {
                backoff = v;
            }
        }

        let mut last_error = None;
        for attempt in 0..max_attempts {
            match crate::list::call_function(thunk, &[]) {
                Ok(val) => return Ok(val),
                Err(e) => {
                    last_error = Some(e);
                    if attempt + 1 < max_attempts && base_delay_ms > 0 {
                        let delay = (base_delay_ms as f64 * backoff.powi(attempt as i32)) as u64;
                        std::thread::sleep(std::time::Duration::from_millis(delay));
                    }
                }
            }
        }
        Err(last_error.unwrap())
    });

    register_fn(env, "gensym", |args| {
        check_arity!(args, "gensym", 0..=1);
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
