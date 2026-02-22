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

    register_router(env);
}

/// Match a URL path against a route pattern, returning extracted parameters on success.
///
/// - Exact segments match literally: `/users` matches `/users`
/// - `:param` segments capture values: `/users/:id` matches `/users/42` -> `[("id","42")]`
/// - `*` wildcard captures rest of path: `/files/*` matches `/files/a/b/c` -> `[("*","a/b/c")]`
/// - Trailing slashes are normalized away before matching.
/// - Segment count must match (except for wildcard which consumes the rest).
pub fn match_path(pattern: &str, path: &str) -> Option<Vec<(String, String)>> {
    // Normalize: strip trailing slash, then split into segments.
    // Root "/" normalizes to a single empty-string segment.
    fn segments(s: &str) -> Vec<&str> {
        let trimmed = s.trim_end_matches('/');
        if trimmed.is_empty() {
            vec![""]
        } else {
            trimmed.split('/').collect()
        }
    }

    let pat_segs = segments(pattern);
    let path_segs = segments(path);

    let mut params = Vec::new();

    for (i, pat_seg) in pat_segs.iter().enumerate() {
        if *pat_seg == "*" {
            // Wildcard: capture the rest of the path from this segment onward
            let rest = if i < path_segs.len() {
                path_segs[i..].join("/")
            } else {
                String::new()
            };
            // Strip leading slash that may appear from the join of segments starting with ""
            let rest = rest.trim_start_matches('/').to_string();
            params.push(("*".to_string(), rest));
            return Some(params);
        }

        // Non-wildcard: segment count must match at this position
        if i >= path_segs.len() {
            return None;
        }

        if let Some(name) = pat_seg.strip_prefix(':') {
            // Parameter capture
            params.push((name.to_string(), path_segs[i].to_string()));
        } else if *pat_seg != path_segs[i] {
            // Literal mismatch
            return None;
        }
    }

    // After consuming all pattern segments, path must have no extra segments
    if pat_segs.len() != path_segs.len() {
        return None;
    }

    Some(params)
}

fn register_router(env: &sema_core::Env) {
    use sema_core::{call_callback, intern, EvalContext, NativeFn};
    use std::rc::Rc;

    env.set(
        intern("http/router"),
        Value::native_fn(NativeFn::with_ctx("http/router", |ctx: &EvalContext, args: &[Value]| {
            check_arity!(args, "http/router", 1);

            // Parse route table: list of [method pattern handler] vectors
            let routes_list = args[0]
                .as_list()
                .or_else(|| args[0].as_vector())
                .ok_or_else(|| SemaError::type_error("list or vector", args[0].type_name()))?;

            let mut routes: Vec<(String, String, Value)> = Vec::new();
            for route in routes_list.iter() {
                let elems = route
                    .as_vector()
                    .or_else(|| route.as_list())
                    .ok_or_else(|| {
                        SemaError::eval("http/router: each route must be a vector [method path handler]")
                    })?;
                if elems.len() != 3 {
                    return Err(SemaError::eval(
                        "http/router: each route must have exactly 3 elements [method path handler]",
                    ));
                }
                let method = elems[0]
                    .as_keyword()
                    .ok_or_else(|| SemaError::type_error("keyword", elems[0].type_name()))?;
                let pattern = elems[1]
                    .as_str()
                    .ok_or_else(|| SemaError::type_error("string", elems[1].type_name()))?
                    .to_string();
                let handler = elems[2].clone();
                routes.push((method, pattern, handler));
            }

            let routes = Rc::new(routes);
            let _ = ctx; // we don't need ctx here, but the dispatch closure does

            // Return a dispatch function
            Ok(Value::native_fn(NativeFn::with_ctx(
                "http/router/dispatch",
                move |ctx: &EvalContext, args: &[Value]| {
                    check_arity!(args, "http/router/dispatch", 1);
                    let req = &args[0];

                    // Extract method from request map
                    let req_map = req
                        .as_map_rc()
                        .ok_or_else(|| SemaError::type_error("map", req.type_name()))?;

                    let req_method = req_map
                        .get(&Value::keyword("method"))
                        .ok_or_else(|| SemaError::eval("http/router: request missing :method"))?
                        .as_keyword()
                        .ok_or_else(|| SemaError::type_error("keyword", "other"))?;

                    let req_path = req_map
                        .get(&Value::keyword("path"))
                        .ok_or_else(|| SemaError::eval("http/router: request missing :path"))?
                        .as_str()
                        .ok_or_else(|| SemaError::type_error("string", "other"))?
                        .to_string();

                    // Try each route
                    for (method, pattern, handler) in routes.iter() {
                        // Method matching: :any matches all, otherwise must match exactly
                        if method != "any" && method != &req_method {
                            continue;
                        }

                        // Path matching
                        if let Some(params) = match_path(pattern, &req_path) {
                            // Build params map (keyword keys)
                            let mut params_map = BTreeMap::new();
                            for (k, v) in &params {
                                params_map.insert(Value::keyword(k), Value::string(v));
                            }

                            // Merge params into existing :params in the request
                            let existing_params = req_map
                                .get(&Value::keyword("params"))
                                .and_then(|v| v.as_map_rc());

                            if let Some(existing) = existing_params {
                                for (k, v) in existing.iter() {
                                    params_map.entry(k.clone()).or_insert_with(|| v.clone());
                                }
                            }

                            // Build new request with merged params
                            let mut new_req = (*req_map).clone();
                            new_req.insert(Value::keyword("params"), Value::map(params_map));
                            let new_req_val = Value::map(new_req);

                            // Call handler
                            return call_callback(ctx, handler, &[new_req_val]);
                        }
                    }

                    // No route matched — return 404
                    let mut headers = BTreeMap::new();
                    headers.insert(
                        Value::string("content-type"),
                        Value::string("application/json"),
                    );
                    let mut result = BTreeMap::new();
                    result.insert(Value::keyword("status"), Value::int(404));
                    result.insert(Value::keyword("headers"), Value::map(headers));
                    result.insert(Value::keyword("body"), Value::string("\"Not Found\""));
                    Ok(Value::map(result))
                },
            )))
        })),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_match_exact_path() {
        let params = match_path("/users", "/users");
        assert!(params.is_some());
        assert!(params.unwrap().is_empty());
    }

    #[test]
    fn test_match_root() {
        assert!(match_path("/", "/").is_some());
    }

    #[test]
    fn test_no_match_different_path() {
        assert!(match_path("/users", "/posts").is_none());
    }

    #[test]
    fn test_match_param() {
        let params = match_path("/users/:id", "/users/42").unwrap();
        assert_eq!(params.len(), 1);
        assert_eq!(params[0], ("id".to_string(), "42".to_string()));
    }

    #[test]
    fn test_match_multiple_params() {
        let params = match_path("/users/:uid/posts/:pid", "/users/1/posts/99").unwrap();
        assert_eq!(params.len(), 2);
        assert_eq!(params[0], ("uid".to_string(), "1".to_string()));
        assert_eq!(params[1], ("pid".to_string(), "99".to_string()));
    }

    #[test]
    fn test_no_match_too_few_segments() {
        assert!(match_path("/users/:id", "/users").is_none());
    }

    #[test]
    fn test_no_match_too_many_segments() {
        assert!(match_path("/users", "/users/42").is_none());
    }

    #[test]
    fn test_match_wildcard() {
        let params = match_path("/files/*", "/files/a/b/c").unwrap();
        assert_eq!(params.len(), 1);
        assert_eq!(params[0], ("*".to_string(), "a/b/c".to_string()));
    }

    #[test]
    fn test_match_trailing_slash_normalized() {
        assert!(match_path("/users", "/users/").is_some());
    }
}
