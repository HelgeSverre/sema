use std::collections::BTreeMap;
use std::time::Duration;

use sema_core::{check_arity, Caps, SemaError, Value, ValueView};

thread_local! {
    static HTTP_RUNTIME: tokio::runtime::Runtime = tokio::runtime::Runtime::new()
        .expect("Failed to create HTTP tokio runtime");
    static HTTP_CLIENT: reqwest::Client = reqwest::Client::new();
}

fn http_request(
    method: &str,
    url: &str,
    body: Option<&Value>,
    opts: Option<&Value>,
) -> Result<Value, SemaError> {
    HTTP_RUNTIME.with(|rt| {
        HTTP_CLIENT.with(|client| {
            rt.block_on(async {
                let mut builder = match method {
                    "GET" => client.get(url),
                    "POST" => client.post(url),
                    "PUT" => client.put(url),
                    "DELETE" => client.delete(url),
                    "PATCH" => client.patch(url),
                    "HEAD" => client.head(url),
                    other => return Err(SemaError::eval(format!("http: unknown method {other}"))),
                };

                // Apply options
                if let Some(opts_val) = opts {
                    if let Some(opts_map) = opts_val.as_map_rc() {
                        if let Some(headers_val) = opts_map.get(&Value::keyword("headers")) {
                            if let Some(headers) = headers_val.as_map_rc() {
                                for (k, v) in headers.iter() {
                                    let key = match k.view() {
                                        ValueView::String(s) => s.to_string(),
                                        ValueView::Keyword(s) => sema_core::resolve(s),
                                        _ => k.to_string(),
                                    };
                                    let val = match v.as_str() {
                                        Some(s) => s.to_string(),
                                        None => v.to_string(),
                                    };
                                    builder = builder.header(key, val);
                                }
                            }
                        }
                        if let Some(timeout_val) = opts_map.get(&Value::keyword("timeout")) {
                            if let Some(ms) = timeout_val.as_int() {
                                builder = builder.timeout(Duration::from_millis(ms as u64));
                            }
                        }
                    }
                }

                // Apply body
                if let Some(body_val) = body {
                    if let Some(s) = body_val.as_str() {
                        builder = builder.body(s.to_string());
                    } else if body_val.as_map_rc().is_some() {
                        let json = sema_core::value_to_json_lossy(body_val);
                        let json_str = serde_json::to_string(&json)
                            .map_err(|e| SemaError::eval(format!("http: json encode: {e}")))?;
                        builder = builder
                            .header("Content-Type", "application/json")
                            .body(json_str);
                    } else {
                        builder = builder.body(body_val.to_string());
                    }
                }

                let response = builder
                    .send()
                    .await
                    .map_err(|e| SemaError::Io(format!("http {method} {url}: {e}")))?;

                let status = response.status().as_u16() as i64;
                let mut headers_map = BTreeMap::new();
                for (k, v) in response.headers() {
                    if let Ok(val) = v.to_str() {
                        headers_map.insert(Value::keyword(k.as_str()), Value::string(val));
                    }
                }
                let body_text = response
                    .text()
                    .await
                    .map_err(|e| SemaError::Io(format!("http {method} {url}: read body: {e}")))?;

                let mut result = BTreeMap::new();
                result.insert(Value::keyword("status"), Value::int(status));
                result.insert(Value::keyword("headers"), Value::map(headers_map));
                result.insert(Value::keyword("body"), Value::string(&body_text));
                Ok(Value::map(result))
            })
        })
    })
}

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/get", |args| {
        check_arity!(args, "http/get", 1..=2);
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let opts = args.get(1);
        http_request("GET", url, None, opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/post", |args| {
        check_arity!(args, "http/post", 2..=3);
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let body = &args[1];
        let opts = args.get(2);
        http_request("POST", url, Some(body), opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/put", |args| {
        check_arity!(args, "http/put", 2..=3);
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let body = &args[1];
        let opts = args.get(2);
        http_request("PUT", url, Some(body), opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/delete", |args| {
        check_arity!(args, "http/delete", 1..=2);
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let opts = args.get(1);
        http_request("DELETE", url, None, opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/request", |args| {
        check_arity!(args, "http/request", 2..=4);
        let method = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?
            .to_uppercase();
        let url = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let opts = args.get(2);
        let body = args.get(3);
        http_request(&method, url, body, opts)
    });
}
