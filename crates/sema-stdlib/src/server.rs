use std::collections::BTreeMap;

use sema_core::{check_arity, SemaError, Value};

use crate::register_fn;

/// Build a JSON response map: {:status N :headers {"content-type" "application/json"} :body json-string}
fn json_response(status: i64, val: &Value) -> Result<Value, SemaError> {
    let json = crate::json::value_to_json(val)?;
    let body = serde_json::to_string(&json)
        .map_err(|e| SemaError::eval(format!("http response: json encode: {e}")))?;

    let mut headers = BTreeMap::new();
    headers.insert(
        Value::string("content-type"),
        Value::string("application/json"),
    );

    let mut result = BTreeMap::new();
    result.insert(Value::keyword("status"), Value::int(status));
    result.insert(Value::keyword("headers"), Value::map(headers));
    result.insert(Value::keyword("body"), Value::string(&body));
    Ok(Value::map(result))
}

pub fn register(env: &sema_core::Env) {
    register_fn(env, "http/ok", |args| {
        check_arity!(args, "http/ok", 1);
        json_response(200, &args[0])
    });

    register_fn(env, "http/created", |args| {
        check_arity!(args, "http/created", 1);
        json_response(201, &args[0])
    });

    register_fn(env, "http/no-content", |args| {
        check_arity!(args, "http/no-content", 0);
        let mut result = BTreeMap::new();
        result.insert(Value::keyword("status"), Value::int(204));
        result.insert(Value::keyword("headers"), Value::map(BTreeMap::new()));
        result.insert(Value::keyword("body"), Value::string(""));
        Ok(Value::map(result))
    });

    register_fn(env, "http/not-found", |args| {
        check_arity!(args, "http/not-found", 1);
        json_response(404, &args[0])
    });

    register_fn(env, "http/redirect", |args| {
        check_arity!(args, "http/redirect", 1);
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;

        let mut headers = BTreeMap::new();
        headers.insert(Value::string("location"), Value::string(url));

        let mut result = BTreeMap::new();
        result.insert(Value::keyword("status"), Value::int(302));
        result.insert(Value::keyword("headers"), Value::map(headers));
        result.insert(Value::keyword("body"), Value::string(""));
        Ok(Value::map(result))
    });

    register_fn(env, "http/error", |args| {
        check_arity!(args, "http/error", 2);
        let status = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[0].type_name()))?;
        json_response(status, &args[1])
    });

    register_fn(env, "http/html", |args| {
        check_arity!(args, "http/html", 1);
        let content = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;

        let mut headers = BTreeMap::new();
        headers.insert(Value::string("content-type"), Value::string("text/html"));

        let mut result = BTreeMap::new();
        result.insert(Value::keyword("status"), Value::int(200));
        result.insert(Value::keyword("headers"), Value::map(headers));
        result.insert(Value::keyword("body"), Value::string(content));
        Ok(Value::map(result))
    });

    register_fn(env, "http/text", |args| {
        check_arity!(args, "http/text", 1);
        let content = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;

        let mut headers = BTreeMap::new();
        headers.insert(Value::string("content-type"), Value::string("text/plain"));

        let mut result = BTreeMap::new();
        result.insert(Value::keyword("status"), Value::int(200));
        result.insert(Value::keyword("headers"), Value::map(headers));
        result.insert(Value::keyword("body"), Value::string(content));
        Ok(Value::map(result))
    });
}
