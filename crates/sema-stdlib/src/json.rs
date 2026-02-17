use std::collections::BTreeMap;

use sema_core::{check_arity, SemaError, Value, ValueView};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "json/encode", |args| {
        check_arity!(args, "json/encode", 1);
        let json = value_to_json(&args[0])?;
        let s = serde_json::to_string(&json)
            .map_err(|e| SemaError::eval(format!("json/encode: {e}")))?;
        Ok(Value::string(&s))
    });

    register_fn(env, "json/encode-pretty", |args| {
        check_arity!(args, "json/encode-pretty", 1);
        let json = value_to_json(&args[0])?;
        let s = serde_json::to_string_pretty(&json)
            .map_err(|e| SemaError::eval(format!("json/encode-pretty: {e}")))?;
        Ok(Value::string(&s))
    });

    register_fn(env, "json/decode", |args| {
        check_arity!(args, "json/decode", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let json: serde_json::Value =
            serde_json::from_str(s).map_err(|e| SemaError::eval(format!("json/decode: {e}")))?;
        Ok(json_to_value(&json))
    });
}

pub fn value_to_json(val: &Value) -> Result<serde_json::Value, SemaError> {
    match val.view() {
        ValueView::Nil => Ok(serde_json::Value::Null),
        ValueView::Bool(b) => Ok(serde_json::Value::Bool(b)),
        ValueView::Int(n) => Ok(serde_json::Value::Number(n.into())),
        ValueView::Float(f) => serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .ok_or_else(|| SemaError::eval("json/encode: cannot encode NaN/Infinity")),
        ValueView::String(s) => Ok(serde_json::Value::String(s.to_string())),
        ValueView::Keyword(s) => Ok(serde_json::Value::String(sema_core::resolve(s))),
        ValueView::Symbol(s) => Ok(serde_json::Value::String(sema_core::resolve(s))),
        ValueView::List(items) | ValueView::Vector(items) => {
            let arr: Result<Vec<_>, _> = items.iter().map(value_to_json).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        ValueView::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                let key = match k.view() {
                    ValueView::String(s) => s.to_string(),
                    ValueView::Keyword(s) => sema_core::resolve(s),
                    ValueView::Symbol(s) => sema_core::resolve(s),
                    _ => k.to_string(),
                };
                obj.insert(key, value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        ValueView::HashMap(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                let key = match k.view() {
                    ValueView::String(s) => s.to_string(),
                    ValueView::Keyword(s) => sema_core::resolve(s),
                    ValueView::Symbol(s) => sema_core::resolve(s),
                    _ => k.to_string(),
                };
                obj.insert(key, value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        _ => Err(SemaError::eval(format!(
            "json/encode: cannot encode {}",
            val.type_name()
        ))),
    }
}

pub fn json_to_value(json: &serde_json::Value) -> Value {
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
        serde_json::Value::Array(arr) => Value::list(arr.iter().map(json_to_value).collect()),
        serde_json::Value::Object(obj) => {
            let mut map = BTreeMap::new();
            for (k, v) in obj {
                map.insert(Value::keyword(k), json_to_value(v));
            }
            Value::map(map)
        }
    }
}
