use std::collections::BTreeMap;

use sema_core::{check_arity, SemaError, Value};

use crate::register_fn;

// --- Raw types for cross-thread communication (Value is !Send due to Rc) ---

/// Raw HTTP request data that is Send-safe for crossing thread boundaries.
struct RawRequest {
    method: String,
    path: String,
    headers: Vec<(String, String)>,
    query: Option<String>,
    body: String,
    content_type_is_json: bool,
}

/// Raw HTTP response data that is Send-safe for crossing thread boundaries.
struct RawResponse {
    status: u16,
    headers: Vec<(String, String)>,
    body: String,
}

/// The response type sent back from the evaluator thread to the axum handler thread.
/// Supports both normal HTTP responses and SSE streaming.
enum ServerResponse {
    /// A normal HTTP response.
    Raw(RawResponse),
    /// An SSE stream: the receiver yields event data strings.
    Sse(tokio::sync::mpsc::Receiver<String>),
    /// A WebSocket connection: bidirectional channels for message passing.
    WebSocket {
        /// Sends messages from axum (client) to the evaluator (server handler).
        incoming_tx: tokio::sync::mpsc::Sender<String>,
        /// Receives messages from the evaluator (server handler) to axum (client).
        outgoing_rx: tokio::sync::mpsc::Receiver<String>,
    },
    /// A file to serve from disk (binary-safe, read on the axum/tokio side).
    File {
        path: std::path::PathBuf,
        content_type: String,
    },
}

/// A server request sent from the axum handler thread to the main evaluator thread.
enum ServerRequest {
    Http {
        raw: RawRequest,
        respond: tokio::sync::oneshot::Sender<ServerResponse>,
    },
}

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

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
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

    register_fn(env, "http/file", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("http/file", "1-2", args.len()));
        }
        let file_path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;

        // Resolve to absolute path
        let path = std::path::Path::new(file_path);
        let abs_path = if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir()
                .map_err(|e| SemaError::eval(format!("http/file: {e}")))?
                .join(path)
        };

        // Canonicalize to resolve symlinks and ..
        let abs_path = abs_path
            .canonicalize()
            .map_err(|e| SemaError::eval(format!("http/file: {}: {e}", abs_path.display())))?;

        // Determine content type: explicit override or guess from extension
        let content_type = if args.len() == 2 {
            args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?
                .to_string()
        } else {
            mime_guess::from_path(&abs_path)
                .first_or_octet_stream()
                .to_string()
        };

        let mut map = BTreeMap::new();
        map.insert(Value::keyword("__file"), Value::bool(true));
        map.insert(
            Value::keyword("__file_path"),
            Value::string(&abs_path.to_string_lossy()),
        );
        map.insert(
            Value::keyword("__file_content_type"),
            Value::string(&content_type),
        );
        Ok(Value::map(map))
    });

    register_fn(env, "http/stream", |args| {
        check_arity!(args, "http/stream", 1);
        let mut map = BTreeMap::new();
        map.insert(Value::keyword("__stream"), Value::bool(true));
        map.insert(Value::keyword("__stream_handler"), args[0].clone());
        Ok(Value::map(map))
    });

    register_fn(env, "http/websocket", |args| {
        check_arity!(args, "http/websocket", 1);
        let mut map = BTreeMap::new();
        map.insert(Value::keyword("__websocket"), Value::bool(true));
        map.insert(Value::keyword("__ws_handler"), args[0].clone());
        Ok(Value::map(map))
    });

    register_router(env);
    register_serve(env, sandbox);
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

                // For :static routes, resolve the directory path at definition time
                // and ensure the pattern ends with /* for wildcard matching
                if method == "static" {
                    let dir_path = elems[2]
                        .as_str()
                        .ok_or_else(|| SemaError::eval(
                            "http/router: :static route directory must be a string"
                        ))?;

                    let dir = std::path::Path::new(dir_path);
                    let abs_dir = if dir.is_absolute() {
                        dir.to_path_buf()
                    } else {
                        std::env::current_dir()
                            .map_err(|e| SemaError::eval(format!("http/router: {e}")))?
                            .join(dir)
                    };
                    let abs_dir = abs_dir
                        .canonicalize()
                        .map_err(|e| SemaError::eval(format!(
                            "http/router: static directory '{}': {e}", abs_dir.display()
                        )))?;

                    // Store the resolved absolute directory path as the handler value
                    let handler = Value::string(&abs_dir.to_string_lossy());

                    // Ensure the pattern has a wildcard suffix for matching
                    let static_pattern = if pattern.ends_with("/*") || pattern.ends_with("*") {
                        pattern
                    } else {
                        format!("{}/*", pattern.trim_end_matches('/'))
                    };
                    routes.push((method, static_pattern, handler));
                    continue;
                }

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
                        // WebSocket routes match GET requests (WS upgrade starts as GET)
                        let is_ws_route = method == "ws";
                        // Static routes only match GET/HEAD requests
                        let is_static_route = method == "static";
                        if is_ws_route || is_static_route {
                            if req_method != "get" && req_method != "head" {
                                continue;
                            }
                        } else if method != "any" && method != &req_method {
                            continue;
                        }

                        // Path matching
                        if let Some(params) = match_path(pattern, &req_path) {
                            // For static routes, resolve the file and return a file marker
                            if is_static_route {
                                let dir_path = handler
                                    .as_str()
                                    .unwrap_or("");
                                let rel_path = params.iter()
                                    .find(|(k, _)| k == "*")
                                    .map(|(_, v)| v.as_str())
                                    .unwrap_or("");

                                // Security: reject path traversal
                                if rel_path.contains("..") {
                                    let mut headers = BTreeMap::new();
                                    headers.insert(
                                        Value::string("content-type"),
                                        Value::string("text/plain"),
                                    );
                                    let mut result = BTreeMap::new();
                                    result.insert(Value::keyword("status"), Value::int(400));
                                    result.insert(Value::keyword("headers"), Value::map(headers));
                                    result.insert(Value::keyword("body"), Value::string("Bad Request"));
                                    return Ok(Value::map(result));
                                }

                                let file_path = std::path::Path::new(dir_path).join(rel_path);

                                // If it's a directory, try index.html
                                let file_path = if file_path.is_dir() {
                                    file_path.join("index.html")
                                } else {
                                    file_path
                                };

                                if !file_path.exists() {
                                    // Don't match — fall through to other routes
                                    // (allows SPA fallback as a later catch-all)
                                    continue;
                                }

                                let content_type = mime_guess::from_path(&file_path)
                                    .first_or_octet_stream()
                                    .to_string();

                                let mut map = BTreeMap::new();
                                map.insert(Value::keyword("__file"), Value::bool(true));
                                map.insert(
                                    Value::keyword("__file_path"),
                                    Value::string(&file_path.to_string_lossy()),
                                );
                                map.insert(
                                    Value::keyword("__file_content_type"),
                                    Value::string(&content_type),
                                );
                                return Ok(Value::map(map));
                            }

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

                            // For WebSocket routes, return a marker map instead of calling handler
                            if is_ws_route {
                                let mut ws_map = BTreeMap::new();
                                ws_map.insert(Value::keyword("__websocket"), Value::bool(true));
                                ws_map.insert(Value::keyword("__ws_handler"), handler.clone());
                                ws_map.insert(Value::keyword("__ws_request"), new_req_val);
                                return Ok(Value::map(ws_map));
                            }

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

/// Convert an HTTP method string (e.g. "GET") to a lowercase keyword Value (e.g. :get).
fn method_keyword(method: &str) -> Value {
    Value::keyword(&method.to_ascii_lowercase())
}

/// Parse a query string like "a=1&b=2" into a Sema map {:a "1" :b "2"}.
fn parse_query_string(query: Option<&str>) -> Value {
    let mut map = BTreeMap::new();
    if let Some(qs) = query {
        for pair in qs.split('&') {
            if pair.is_empty() {
                continue;
            }
            let (key, val) = match pair.split_once('=') {
                Some((k, v)) => (k, v),
                None => (pair, ""),
            };
            map.insert(Value::keyword(key), Value::string(val));
        }
    }
    Value::map(map)
}

/// Convert a RawRequest into a Sema Value map on the main (evaluator) thread.
fn raw_request_to_value(raw: &RawRequest) -> Value {
    let mut headers_map = BTreeMap::new();
    for (k, v) in &raw.headers {
        headers_map.insert(Value::string(k), Value::string(v));
    }

    let query_val = parse_query_string(raw.query.as_deref());

    let mut req_map = BTreeMap::new();
    req_map.insert(Value::keyword("method"), method_keyword(&raw.method));
    req_map.insert(Value::keyword("path"), Value::string(&raw.path));
    req_map.insert(Value::keyword("headers"), Value::map(headers_map));
    req_map.insert(Value::keyword("query"), query_val);
    req_map.insert(Value::keyword("params"), Value::map(BTreeMap::new()));
    req_map.insert(Value::keyword("body"), Value::string(&raw.body));

    // Auto-parse JSON body if content-type indicates json
    if raw.content_type_is_json && !raw.body.is_empty() {
        if let Ok(json_val) = serde_json::from_str::<serde_json::Value>(&raw.body) {
            let sema_val = crate::json::json_to_value(&json_val);
            req_map.insert(Value::keyword("json"), sema_val);
        }
    }

    Value::map(req_map)
}

/// Convert a Sema response Value map into a RawResponse for sending back to the axum thread.
fn value_to_raw_response(val: &Value) -> RawResponse {
    let map = match val.as_map_rc() {
        Some(m) => m,
        None => {
            return RawResponse {
                status: 200,
                headers: vec![("content-type".to_string(), "text/plain".to_string())],
                body: val.to_string(),
            };
        }
    };

    let status = map
        .get(&Value::keyword("status"))
        .and_then(|v| v.as_int())
        .unwrap_or(200) as u16;

    let body = map
        .get(&Value::keyword("body"))
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_default();

    let mut headers = Vec::new();
    if let Some(h) = map
        .get(&Value::keyword("headers"))
        .and_then(|v| v.as_map_rc())
    {
        for (k, v) in h.iter() {
            let key = k
                .as_str()
                .map(|s| s.to_string())
                .unwrap_or_else(|| k.to_string());
            let val = v
                .as_str()
                .map(|s| s.to_string())
                .unwrap_or_else(|| v.to_string());
            headers.push((key, val));
        }
    }

    RawResponse {
        status,
        headers,
        body,
    }
}

/// Convert a RawResponse into an axum HTTP response.
fn raw_response_to_axum(raw: &RawResponse) -> axum::response::Response {
    use axum::http::{HeaderName, HeaderValue, StatusCode};
    use axum::response::IntoResponse;

    let status = StatusCode::from_u16(raw.status).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR);

    let mut builder = axum::http::Response::builder().status(status);
    for (k, v) in &raw.headers {
        if let (Ok(name), Ok(val)) = (
            HeaderName::try_from(k.as_str()),
            HeaderValue::try_from(v.as_str()),
        ) {
            builder = builder.header(name, val);
        }
    }

    builder
        .body(axum::body::Body::from(raw.body.clone()))
        .unwrap_or_else(|_| StatusCode::INTERNAL_SERVER_ERROR.into_response())
}

/// Handle an incoming axum request: extract metadata, forward to the evaluator, and
/// return the appropriate response (normal HTTP, SSE stream, or WebSocket upgrade).
async fn handle_axum_request(
    ws_upgrade: Option<axum::extract::ws::WebSocketUpgrade>,
    req: axum::extract::Request,
    tx: tokio::sync::mpsc::Sender<ServerRequest>,
) -> axum::response::Response {
    use axum::response::IntoResponse;

    // Extract method, URI, headers from axum request
    let method = req.method().to_string();
    let uri = req.uri().clone();
    let path = uri.path().to_string();
    let query = uri.query().map(|q| q.to_string());

    let mut headers = Vec::new();
    let mut content_type_is_json = false;
    for (name, value) in req.headers().iter() {
        let v = value.to_str().unwrap_or("").to_string();
        let n = name.as_str().to_string();
        if n == "content-type" && v.contains("json") {
            content_type_is_json = true;
        }
        headers.push((n, v));
    }

    // Read body (no size limit)
    let body_bytes = match axum::body::to_bytes(req.into_body(), usize::MAX).await {
        Ok(b) => b,
        Err(_) => {
            return raw_response_to_axum(&RawResponse {
                status: 400,
                headers: vec![("content-type".to_string(), "text/plain".to_string())],
                body: "Failed to read request body".to_string(),
            });
        }
    };
    let body = String::from_utf8_lossy(&body_bytes).to_string();

    let raw = RawRequest {
        method,
        path,
        headers,
        query,
        body,
        content_type_is_json,
    };

    // Create oneshot channel for the response
    let (resp_tx, resp_rx) = tokio::sync::oneshot::channel();

    // Send request to main thread
    if tx
        .send(ServerRequest::Http {
            raw,
            respond: resp_tx,
        })
        .await
        .is_err()
    {
        return raw_response_to_axum(&RawResponse {
            status: 503,
            headers: vec![("content-type".to_string(), "text/plain".to_string())],
            body: "Server shutting down".to_string(),
        });
    }

    // Wait for response from main thread
    match resp_rx.await {
        Ok(ServerResponse::Raw(raw_resp)) => raw_response_to_axum(&raw_resp),
        Ok(ServerResponse::Sse(rx)) => {
            use axum::response::sse::{Event, Sse};
            use futures::stream::StreamExt;
            use tokio_stream::wrappers::ReceiverStream;

            let stream = ReceiverStream::new(rx)
                .map(|data| Ok::<_, std::convert::Infallible>(Event::default().data(data)));
            Sse::new(stream).into_response()
        }
        Ok(ServerResponse::WebSocket {
            incoming_tx,
            outgoing_rx,
        }) => {
            if let Some(ws) = ws_upgrade {
                ws.on_upgrade(move |socket| bridge_websocket(socket, incoming_tx, outgoing_rx))
                    .into_response()
            } else {
                raw_response_to_axum(&RawResponse {
                    status: 400,
                    headers: vec![("content-type".to_string(), "text/plain".to_string())],
                    body: "WebSocket upgrade required".to_string(),
                })
            }
        }
        Ok(ServerResponse::File { path, content_type }) => {
            match tokio::fs::read(&path).await {
                Ok(bytes) => {
                    use axum::http::{HeaderValue, StatusCode};

                    let mut response = axum::http::Response::builder()
                        .status(StatusCode::OK)
                        .body(axum::body::Body::from(bytes))
                        .unwrap();
                    if let Ok(ct) = HeaderValue::try_from(&content_type) {
                        response.headers_mut().insert("content-type", ct);
                    }
                    // Set cache headers for static assets
                    if let Ok(val) = HeaderValue::from_str("public, max-age=3600") {
                        response.headers_mut().insert("cache-control", val);
                    }
                    response
                }
                Err(_) => raw_response_to_axum(&RawResponse {
                    status: 404,
                    headers: vec![("content-type".to_string(), "text/plain".to_string())],
                    body: "Not Found".to_string(),
                }),
            }
        }
        Err(_) => raw_response_to_axum(&RawResponse {
            status: 500,
            headers: vec![("content-type".to_string(), "text/plain".to_string())],
            body: "Handler did not respond".to_string(),
        }),
    }
}

/// Bridge an axum WebSocket to the evaluator's channels.
async fn bridge_websocket(
    socket: axum::extract::ws::WebSocket,
    incoming_tx: tokio::sync::mpsc::Sender<String>,
    mut outgoing_rx: tokio::sync::mpsc::Receiver<String>,
) {
    use axum::extract::ws::Message;
    use futures::{SinkExt, StreamExt};

    let (mut ws_sink, mut ws_stream) = socket.split();

    // Task 1: forward messages from client (WebSocket) to evaluator
    let incoming_tx_clone = incoming_tx.clone();
    let recv_task = tokio::spawn(async move {
        while let Some(Ok(msg)) = ws_stream.next().await {
            match msg {
                Message::Text(text) => {
                    if incoming_tx_clone.send(text.to_string()).await.is_err() {
                        break;
                    }
                }
                Message::Close(_) => break,
                _ => {} // ignore binary, ping, pong
            }
        }
        // Signal to the evaluator that the client disconnected by dropping the sender
        drop(incoming_tx_clone);
    });

    // Task 2: forward messages from evaluator to client (WebSocket)
    let send_task = tokio::spawn(async move {
        while let Some(msg) = outgoing_rx.recv().await {
            if ws_sink.send(Message::Text(msg.into())).await.is_err() {
                break;
            }
        }
        // Try to send a close frame
        let _ = ws_sink.send(Message::Close(None)).await;
    });

    // Wait for either task to complete, then abort the other
    tokio::select! {
        _ = recv_task => {}
        _ = send_task => {}
    }
}

/// Check if a response Value is an SSE stream marker.
fn is_stream_response(val: &Value) -> bool {
    if let Some(m) = val.as_map_rc() {
        m.get(&Value::keyword("__stream"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
    } else {
        false
    }
}

/// Check if a response Value is a WebSocket marker.
fn is_websocket_response(val: &Value) -> bool {
    if let Some(m) = val.as_map_rc() {
        m.get(&Value::keyword("__websocket"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
    } else {
        false
    }
}

/// Check if a response Value is a file response marker.
fn is_file_response(val: &Value) -> bool {
    if let Some(m) = val.as_map_rc() {
        m.get(&Value::keyword("__file"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false)
    } else {
        false
    }
}

/// Extract file path and content type from a file response marker and send to axum.
fn handle_file_response(
    response_val: &Value,
    respond: tokio::sync::oneshot::Sender<ServerResponse>,
) {
    let map = response_val.as_map_rc().unwrap();
    let path_str = map
        .get(&Value::keyword("__file_path"))
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_default();
    let content_type = map
        .get(&Value::keyword("__file_content_type"))
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_else(|| "application/octet-stream".to_string());

    let _ = respond.send(ServerResponse::File {
        path: std::path::PathBuf::from(path_str),
        content_type,
    });
}

/// Handle an SSE stream response: extract the stream handler, create channels,
/// send the SSE receiver to axum, then call the handler with a `send` function.
fn handle_sse_response(
    ctx: &sema_core::EvalContext,
    response_val: &Value,
    respond: tokio::sync::oneshot::Sender<ServerResponse>,
) {
    use sema_core::{call_callback, NativeFn};

    let map = response_val.as_map_rc().unwrap();
    let stream_handler = map
        .get(&Value::keyword("__stream_handler"))
        .cloned()
        .unwrap();

    // Create the SSE channel
    let (sse_tx, sse_rx) = tokio::sync::mpsc::channel::<String>(256);

    // Send the SSE receiver to axum so it can start streaming immediately
    let _ = respond.send(ServerResponse::Sse(sse_rx));

    // Build the `send` function for the Sema handler
    let send_fn = Value::native_fn(NativeFn::simple("http/stream/send", move |args| {
        check_arity!(args, "http/stream/send", 1);
        let msg = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        sse_tx
            .blocking_send(msg.to_string())
            .map_err(|_| SemaError::eval("SSE stream closed"))?;
        Ok(Value::nil())
    }));

    // Call the stream handler with the send function.
    // When it returns (or errors), the sse_tx is dropped, closing the stream.
    match call_callback(ctx, &stream_handler, &[send_fn]) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("http/stream handler error: {e}");
        }
    }
}

/// Handle a WebSocket response: extract the WS handler, create bidirectional channels,
/// send them to axum for bridging, then call the handler with a connection map.
fn handle_ws_response(
    ctx: &sema_core::EvalContext,
    response_val: &Value,
    respond: tokio::sync::oneshot::Sender<ServerResponse>,
) {
    use sema_core::{call_callback, NativeFn};
    use std::cell::RefCell;
    use std::rc::Rc;

    let map = response_val.as_map_rc().unwrap();
    let ws_handler = map.get(&Value::keyword("__ws_handler")).cloned().unwrap();

    // Create bidirectional channels
    let (in_tx, in_rx) = tokio::sync::mpsc::channel::<String>(256); // client -> evaluator
    let (out_tx, out_rx) = tokio::sync::mpsc::channel::<String>(256); // evaluator -> client

    // Send channels to axum for WebSocket bridging
    let _ = respond.send(ServerResponse::WebSocket {
        incoming_tx: in_tx,
        outgoing_rx: out_rx,
    });

    // Build the connection map for the Sema handler: {:send fn :recv fn :close fn}
    let out_tx_for_send = out_tx.clone();
    let send_fn = Value::native_fn(NativeFn::simple("ws/send", move |args| {
        check_arity!(args, "ws/send", 1);
        let msg = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        out_tx_for_send
            .blocking_send(msg.to_string())
            .map_err(|_| SemaError::eval("WebSocket closed"))?;
        Ok(Value::nil())
    }));

    // Wrap receiver in Rc<RefCell<Option<...>>> since NativeFn closures must be Fn (not FnOnce)
    let in_rx = Rc::new(RefCell::new(Some(in_rx)));
    let in_rx_for_recv = in_rx.clone();
    let recv_fn = Value::native_fn(NativeFn::simple("ws/recv", move |args| {
        check_arity!(args, "ws/recv", 0);
        let mut rx_opt = in_rx_for_recv.borrow_mut();
        if let Some(rx) = rx_opt.as_mut() {
            match rx.blocking_recv() {
                Some(msg) => Ok(Value::string(&msg)),
                None => {
                    // Channel closed — remove the receiver
                    *rx_opt = None;
                    Ok(Value::nil())
                }
            }
        } else {
            Ok(Value::nil())
        }
    }));

    let out_tx_for_close = out_tx;
    let in_rx_for_close = in_rx;
    let close_fn = Value::native_fn(NativeFn::simple("ws/close", move |args| {
        check_arity!(args, "ws/close", 0);
        // Drop sender to signal close to the axum side
        drop(out_tx_for_close.clone());
        // Drop receiver
        let mut rx_opt = in_rx_for_close.borrow_mut();
        *rx_opt = None;
        Ok(Value::nil())
    }));

    let mut conn_map = BTreeMap::new();
    conn_map.insert(Value::keyword("send"), send_fn);
    conn_map.insert(Value::keyword("recv"), recv_fn);
    conn_map.insert(Value::keyword("close"), close_fn);
    let conn = Value::map(conn_map);

    // Call the WebSocket handler with the connection map
    match call_callback(ctx, &ws_handler, &[conn]) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("ws handler error: {e}");
        }
    }
}

fn register_serve(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    use sema_core::{intern, Caps, EvalContext, NativeFn};

    if sandbox.is_unrestricted() {
        env.set(
            intern("http/serve"),
            Value::native_fn(NativeFn::with_ctx(
                "http/serve",
                |ctx: &EvalContext, args: &[Value]| http_serve_impl(ctx, args),
            )),
        );
    } else {
        let sandbox = sandbox.clone();
        env.set(
            intern("http/serve"),
            Value::native_fn(NativeFn::with_ctx(
                "http/serve",
                move |ctx: &EvalContext, args: &[Value]| {
                    sandbox.check(Caps::NETWORK, "http/serve")?;
                    http_serve_impl(ctx, args)
                },
            )),
        );
    }
}

fn http_serve_impl(ctx: &sema_core::EvalContext, args: &[Value]) -> Result<Value, SemaError> {
    use sema_core::call_callback;

    if args.is_empty() || args.len() > 2 {
        return Err(SemaError::arity("http/serve", "1-2", args.len()));
    }

    let handler = args[0].clone();

    // Parse options map (arg 1): {:port 3000 :host "0.0.0.0"}
    let mut port: u16 = 3000;
    let mut host = "0.0.0.0".to_string();

    if args.len() == 2 {
        if let Some(opts) = args[1].as_map_rc() {
            if let Some(p) = opts.get(&Value::keyword("port")).and_then(|v| v.as_int()) {
                port = p as u16;
            }
            if let Some(h) = opts.get(&Value::keyword("host")).and_then(|v| v.as_str()) {
                host = h.to_string();
            }
        }
    }

    // Create the mpsc channel for server requests (tokio async channel)
    let (tx, mut rx) = tokio::sync::mpsc::channel::<ServerRequest>(256);

    // Create a std sync channel for ready signal
    let (ready_tx, ready_rx) = std::sync::mpsc::channel::<Result<(), String>>();

    let bind_host = host.clone();
    let bind_port = port;

    // Spawn background thread with its own tokio runtime for axum
    std::thread::spawn(move || {
        let rt = match tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
        {
            Ok(rt) => rt,
            Err(e) => {
                let _ = ready_tx.send(Err(format!("failed to create runtime: {e}")));
                return;
            }
        };

        rt.block_on(async move {
            let tx = tx;

            // Build the axum router with a fallback handler that catches all requests.
            // We manually extract WebSocketUpgrade from request parts when needed.
            let app = axum::Router::new().fallback(move |req: axum::extract::Request| {
                let tx = tx.clone();
                async move {
                    // Try to extract WebSocketUpgrade from request parts
                    use axum::extract::FromRequestParts;
                    let (mut parts, body) = req.into_parts();
                    let ws_upgrade: Option<axum::extract::ws::WebSocketUpgrade> =
                        axum::extract::ws::WebSocketUpgrade::from_request_parts(&mut parts, &())
                            .await
                            .ok();
                    let req = axum::extract::Request::from_parts(parts, body);
                    handle_axum_request(ws_upgrade, req, tx).await
                }
            });

            // Bind TCP listener
            let addr = format!("{bind_host}:{bind_port}");
            let listener = match tokio::net::TcpListener::bind(&addr).await {
                Ok(l) => l,
                Err(e) => {
                    let _ = ready_tx.send(Err(format!("bind {addr}: {e}")));
                    return;
                }
            };

            // Signal success
            let _ = ready_tx.send(Ok(()));

            // Run the server
            let _ = axum::serve(listener, app).await;
        });
    });

    // Wait for ready signal from the background thread
    match ready_rx.recv() {
        Ok(Ok(())) => {}
        Ok(Err(e)) => {
            return Err(SemaError::Io(e));
        }
        Err(_) => {
            return Err(SemaError::eval(
                "http/serve: server thread died before binding",
            ));
        }
    }

    eprintln!("Listening on {host}:{port}");

    // Main evaluator loop: read requests from channel, call handler, send response
    while let Some(req) = rx.blocking_recv() {
        match req {
            ServerRequest::Http { raw, respond } => {
                let request_val = raw_request_to_value(&raw);
                match call_callback(ctx, &handler, &[request_val]) {
                    Ok(response_val) => {
                        // Check for SSE stream marker
                        if is_stream_response(&response_val) {
                            handle_sse_response(ctx, &response_val, respond);
                        } else if is_websocket_response(&response_val) {
                            handle_ws_response(ctx, &response_val, respond);
                        } else if is_file_response(&response_val) {
                            handle_file_response(&response_val, respond);
                        } else {
                            let raw_resp = value_to_raw_response(&response_val);
                            let _ = respond.send(ServerResponse::Raw(raw_resp));
                        }
                    }
                    Err(e) => {
                        eprintln!("http/serve handler error: {e}");
                        let _ = respond.send(ServerResponse::Raw(RawResponse {
                            status: 500,
                            headers: vec![(
                                "content-type".to_string(),
                                "application/json".to_string(),
                            )],
                            body: format!(
                                "{{\"error\":\"{}\"}}",
                                e.to_string().replace('"', "\\\"")
                            ),
                        }));
                    }
                }
            }
        }
    }

    Ok(Value::nil())
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
