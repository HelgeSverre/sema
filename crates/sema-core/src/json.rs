//! Canonical conversions between `Value` and `serde_json::Value`.
//!
//! Two modes:
//! - **Strict** (`value_to_json`): errors on NaN/Infinity and unsupported types.
//! - **Lossy** (`value_to_json_lossy`): NaN/Infinity→null, unsupported→string.

use std::collections::BTreeMap;

use crate::{resolve, SemaError, Value, ValueView};

/// Convert a Sema Value to a JSON value, erroring on NaN/Infinity and unsupported types.
pub fn value_to_json(val: &Value) -> Result<serde_json::Value, SemaError> {
    match val.view() {
        ValueView::Nil => Ok(serde_json::Value::Null),
        ValueView::Bool(b) => Ok(serde_json::Value::Bool(b)),
        ValueView::Int(n) => Ok(serde_json::Value::Number(n.into())),
        ValueView::Float(f) => serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .ok_or_else(|| SemaError::eval("cannot encode NaN/Infinity as JSON")),
        ValueView::String(s) => Ok(serde_json::Value::String(s.to_string())),
        ValueView::Keyword(s) => Ok(serde_json::Value::String(resolve(s))),
        ValueView::Symbol(s) => Ok(serde_json::Value::String(resolve(s))),
        ValueView::List(items) | ValueView::Vector(items) => {
            let arr: Result<Vec<_>, _> = items.iter().map(value_to_json).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        ValueView::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                obj.insert(key_to_string(k), value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        ValueView::HashMap(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                obj.insert(key_to_string(k), value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        _ => Err(SemaError::eval(format!(
            "cannot encode {} as JSON",
            val.type_name()
        ))),
    }
}

/// Convert a Sema Value to JSON without erroring. NaN/Infinity become null,
/// unsupported types become their string representation.
pub fn value_to_json_lossy(val: &Value) -> serde_json::Value {
    match val.view() {
        ValueView::Nil => serde_json::Value::Null,
        ValueView::Bool(b) => serde_json::Value::Bool(b),
        ValueView::Int(n) => serde_json::Value::Number(n.into()),
        ValueView::Float(f) => serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        ValueView::String(s) => serde_json::Value::String(s.to_string()),
        ValueView::Keyword(s) => serde_json::Value::String(resolve(s)),
        ValueView::Symbol(s) => serde_json::Value::String(resolve(s)),
        ValueView::List(items) | ValueView::Vector(items) => {
            serde_json::Value::Array(items.iter().map(value_to_json_lossy).collect())
        }
        ValueView::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                obj.insert(key_to_string(k), value_to_json_lossy(v));
            }
            serde_json::Value::Object(obj)
        }
        ValueView::HashMap(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                obj.insert(key_to_string(k), value_to_json_lossy(v));
            }
            serde_json::Value::Object(obj)
        }
        _ => serde_json::Value::String(val.to_string()),
    }
}

/// Convert a JSON value to a Sema Value.
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

/// Extract a string key from a Value for use as a JSON/TOML map key.
pub fn key_to_string(k: &Value) -> String {
    match k.view() {
        ValueView::String(s) => s.to_string(),
        ValueView::Keyword(s) => resolve(s),
        ValueView::Symbol(s) => resolve(s),
        _ => k.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lossy_preserves_map_structure_around_nan() {
        // A map with one normal value and one NaN — lossy should preserve
        // the map structure and only replace the NaN with null.
        let mut map = BTreeMap::new();
        map.insert(Value::keyword("a"), Value::int(1));
        map.insert(Value::keyword("b"), Value::float(f64::NAN));
        let val = Value::map(map);

        let json = value_to_json_lossy(&val);

        // Must be an object, not a string
        assert!(json.is_object(), "expected JSON object, got: {json}");
        let obj = json.as_object().unwrap();
        assert_eq!(obj.get("a"), Some(&serde_json::Value::Number(1.into())));
        assert_eq!(obj.get("b"), Some(&serde_json::Value::Null));
    }

    #[test]
    fn test_strict_errors_on_nan_in_map() {
        // Strict conversion should error when NaN is nested inside a map,
        // NOT stringify the whole map.
        let mut map = BTreeMap::new();
        map.insert(Value::keyword("ok"), Value::int(42));
        map.insert(Value::keyword("bad"), Value::float(f64::NAN));
        let val = Value::map(map);

        let err = value_to_json(&val).unwrap_err();
        assert!(
            err.to_string().contains("NaN"),
            "expected NaN error, got: {err}"
        );
    }

    #[test]
    fn test_strict_errors_on_nan_in_list() {
        let val = Value::list(vec![Value::int(1), Value::float(f64::NAN)]);
        let err = value_to_json(&val).unwrap_err();
        assert!(err.to_string().contains("NaN"));
    }

    #[test]
    fn test_lossy_unsupported_type_becomes_string() {
        // A native function can't be represented in JSON; lossy should
        // stringify it rather than error.
        use crate::NativeFn;
        let val = Value::native_fn(NativeFn::simple("test-fn", |_| Ok(Value::nil())));
        let json = value_to_json_lossy(&val);
        assert!(json.is_string(), "expected string, got: {json}");
    }

    #[test]
    fn test_lossy_preserves_list_structure_around_nan() {
        let val = Value::list(vec![Value::int(1), Value::float(f64::NAN), Value::int(3)]);

        let json = value_to_json_lossy(&val);

        assert!(json.is_array(), "expected JSON array, got: {json}");
        let arr = json.as_array().unwrap();
        assert_eq!(arr[0], serde_json::Value::Number(1.into()));
        assert_eq!(arr[1], serde_json::Value::Null);
        assert_eq!(arr[2], serde_json::Value::Number(3.into()));
    }
}
