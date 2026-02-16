use std::collections::BTreeMap;
use std::rc::Rc;
use std::time::Duration;

use sema_core::{Caps, SemaError, Value};

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
                if let Some(Value::Map(opts_map)) = opts {
                    if let Some(Value::Map(headers)) = opts_map.get(&Value::keyword("headers")) {
                        for (k, v) in headers.iter() {
                            let key = match k {
                                Value::String(s) => s.to_string(),
                                Value::Keyword(s) => sema_core::resolve(*s),
                                other => other.to_string(),
                            };
                            let val = match v {
                                Value::String(s) => s.to_string(),
                                other => other.to_string(),
                            };
                            builder = builder.header(key, val);
                        }
                    }
                    if let Some(timeout_val) = opts_map.get(&Value::keyword("timeout")) {
                        if let Some(ms) = timeout_val.as_int() {
                            builder = builder.timeout(Duration::from_millis(ms as u64));
                        }
                    }
                }

                // Apply body
                if let Some(body_val) = body {
                    match body_val {
                        Value::String(s) => {
                            builder = builder.body(s.to_string());
                        }
                        Value::Map(_) => {
                            let json = crate::json::value_to_json(body_val)?;
                            let json_str = serde_json::to_string(&json)
                                .map_err(|e| SemaError::eval(format!("http: json encode: {e}")))?;
                            builder = builder
                                .header("Content-Type", "application/json")
                                .body(json_str);
                        }
                        _ => {
                            builder = builder.body(body_val.to_string());
                        }
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
                result.insert(Value::keyword("status"), Value::Int(status));
                result.insert(Value::keyword("headers"), Value::Map(Rc::new(headers_map)));
                result.insert(Value::keyword("body"), Value::string(&body_text));
                Ok(Value::Map(Rc::new(result)))
            })
        })
    })
}

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/get", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("http/get", "1 or 2", args.len()));
        }
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let opts = args.get(1);
        http_request("GET", url, None, opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/post", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("http/post", "2 or 3", args.len()));
        }
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let body = &args[1];
        let opts = args.get(2);
        http_request("POST", url, Some(body), opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/put", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("http/put", "2 or 3", args.len()));
        }
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let body = &args[1];
        let opts = args.get(2);
        http_request("PUT", url, Some(body), opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/delete", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("http/delete", "1 or 2", args.len()));
        }
        let url = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let opts = args.get(1);
        http_request("DELETE", url, None, opts)
    });

    crate::register_fn_gated(env, sandbox, Caps::NETWORK, "http/request", |args| {
        if args.len() < 2 || args.len() > 4 {
            return Err(SemaError::arity("http/request", "2-4", args.len()));
        }
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
