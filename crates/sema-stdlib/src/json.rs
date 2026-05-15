use sema_core::{check_arity, SemaError, Value};

pub use sema_core::json::{json_to_value, value_to_json};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "json/encode", |args| {
        check_arity!(args, "json/encode", 1);
        let json =
            value_to_json(&args[0]).map_err(|e| SemaError::eval(format!("json/encode: {e}")))?;
        let s = serde_json::to_string(&json)
            .map_err(|e| SemaError::eval(format!("json/encode: {e}")))?;
        Ok(Value::string(&s))
    });

    register_fn(env, "json/encode-pretty", |args| {
        check_arity!(args, "json/encode-pretty", 1);
        let json = value_to_json(&args[0])
            .map_err(|e| SemaError::eval(format!("json/encode-pretty: {e}")))?;
        let s = serde_json::to_string_pretty(&json)
            .map_err(|e| SemaError::eval(format!("json/encode-pretty: {e}")))?;
        Ok(Value::string(&s))
    });

    register_fn(env, "json/decode", |args| {
        check_arity!(args, "json/decode", 1);
        let s = args[0].as_str().ok_or_else(|| {
            SemaError::type_error("string", args[0].type_name())
                .with_hint("json/decode: argument 1 must be a JSON-encoded string")
        })?;
        let json: serde_json::Value = serde_json::from_str(s).map_err(|e| {
            let l = e.line();
            let c = e.column();
            SemaError::eval(format!(
                "json/decode: parse error at line {l} column {c}: {e}"
            ))
            .with_hint(
                "json/decode expects a JSON string (note: single quotes and trailing commas are not valid JSON)",
            )
        })?;
        Ok(json_to_value(&json))
    });
}
