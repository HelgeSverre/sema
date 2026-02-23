use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet};

use js_sys::Date;
use sema_core::{pretty_print, Env, NativeFn, SemaError, Value, ValueView};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use wasm_bindgen_futures::JsFuture;

thread_local! {
    /// Completed lines of output (flushed by println/newline)
    static OUTPUT: RefCell<Vec<String>> = const { RefCell::new(Vec::new()) };
    /// Current line being built by display/print (not yet flushed)
    static LINE_BUF: RefCell<String> = const { RefCell::new(String::new()) };
    /// Start time for sys/elapsed (milliseconds since epoch)
    static WASM_START_MS: f64 = Date::now();
    /// In-memory virtual filesystem for WASM
    static VFS: RefCell<BTreeMap<String, String>> = const { RefCell::new(BTreeMap::new()) };
    /// Virtual directories (tracked for file/mkdir, file/is-directory?)
    static VFS_DIRS: RefCell<BTreeSet<String>> = RefCell::new({
        let mut s = BTreeSet::new();
        s.insert("/".to_string());
        s
    });
    /// In-memory HTTP response cache for the replay-with-cache strategy
    static HTTP_CACHE: RefCell<BTreeMap<String, Value>> = const { RefCell::new(BTreeMap::new()) };
    /// Total bytes currently stored in the VFS
    static VFS_TOTAL_BYTES: Cell<usize> = const { Cell::new(0) };
}

const VFS_MAX_TOTAL_BYTES: usize = 16 * 1024 * 1024; // 16 MB total
const VFS_MAX_FILE_BYTES: usize = 1024 * 1024; // 1 MB per file
const VFS_MAX_FILES: usize = 256;

fn vfs_check_quota(file_name: &str, new_content_len: usize) -> Result<(), SemaError> {
    if new_content_len > VFS_MAX_FILE_BYTES {
        return Err(SemaError::eval(format!(
            "VFS quota exceeded: file '{}' is {} bytes, max {} bytes per file",
            file_name, new_content_len, VFS_MAX_FILE_BYTES
        )));
    }

    VFS.with(|vfs| {
        let map = vfs.borrow();
        let old_len = map.get(file_name).map_or(0, |s| s.len());
        let is_new_file = !map.contains_key(file_name);

        if is_new_file && map.len() >= VFS_MAX_FILES {
            return Err(SemaError::eval(format!(
                "VFS quota exceeded: max {} files",
                VFS_MAX_FILES
            )));
        }

        let total = VFS_TOTAL_BYTES.with(|t| t.get());
        let new_total = total
            .saturating_add(new_content_len)
            .saturating_sub(old_len);
        if new_total > VFS_MAX_TOTAL_BYTES {
            return Err(SemaError::eval(format!(
                "VFS quota exceeded: would use {} bytes, max {} bytes total",
                new_total, VFS_MAX_TOTAL_BYTES
            )));
        }

        Ok(())
    })
}

/// Normalize a VFS path to canonical form: always starts with "/",
/// no trailing slash (except root), collapsed "//", resolved "." segments,
/// ".." rejected (no parent traversal in sandbox).
fn normalize_path(path: &str) -> Result<String, SemaError> {
    let path = path.trim();
    if path.is_empty() || path == "/" {
        return Ok("/".to_string());
    }

    let mut segments: Vec<&str> = Vec::new();
    for seg in path.split('/') {
        match seg {
            "" | "." => continue,
            ".." => {
                return Err(SemaError::eval(
                    "VFS path error: '..' parent traversal not allowed",
                ));
            }
            s => segments.push(s),
        }
    }

    if segments.is_empty() {
        return Ok("/".to_string());
    }

    let mut result = String::with_capacity(path.len() + 1);
    for seg in &segments {
        result.push('/');
        result.push_str(seg);
    }
    Ok(result)
}

/// Append text to the current line buffer (no newline).
fn append_output(s: &str) {
    LINE_BUF.with(|b| b.borrow_mut().push_str(s));
}

/// Flush the current line buffer as a completed line.
fn flush_line() {
    LINE_BUF.with(|b| {
        let line = b.borrow().clone();
        b.borrow_mut().clear();
        OUTPUT.with(|o| o.borrow_mut().push(line));
    });
}

/// Take all completed output lines, flushing any partial line first.
fn take_output() -> Vec<String> {
    // Flush any trailing partial line
    LINE_BUF.with(|b| {
        let buf = b.borrow();
        if !buf.is_empty() {
            let line = buf.clone();
            drop(buf);
            b.borrow_mut().clear();
            OUTPUT.with(|o| o.borrow_mut().push(line));
        }
    });
    OUTPUT.with(|o| o.borrow_mut().drain(..).collect())
}

const HTTP_AWAIT_MARKER: &str = "__SEMA_WASM_HTTP__";
const MAX_REPLAYS: usize = 50;

/// Build a deterministic cache key from HTTP request parameters.
fn http_cache_key(
    method: &str,
    url: &str,
    body: Option<&str>,
    headers: &[(String, String)],
) -> String {
    use std::fmt::Write;
    let mut key = format!("{method}\n{url}\n");
    match body {
        Some(b) => {
            write!(key, "{b}").unwrap();
        }
        None => {
            key.push_str("<nil>");
        }
    }
    key.push('\n');
    for (k, v) in headers {
        writeln!(key, "{k}:{v}").unwrap();
    }
    key
}

/// Create a marker error whose message encodes an HTTP request as JSON.
fn http_await_marker(
    method: &str,
    url: &str,
    body: Option<&str>,
    headers: &[(String, String)],
    timeout_ms: Option<i64>,
) -> SemaError {
    let key = http_cache_key(method, url, body, headers);
    let body_json = match body {
        Some(b) => format!("\"{}\"", escape_json(b)),
        None => "null".to_string(),
    };
    let timeout_json = match timeout_ms {
        Some(t) => format!("{t}"),
        None => "null".to_string(),
    };
    let headers_json = headers
        .iter()
        .map(|(k, v)| format!("[\"{}\",\"{}\"]", escape_json(k), escape_json(v)))
        .collect::<Vec<_>>()
        .join(",");
    let payload = format!(
        "{}{{\"key\":\"{}\",\"method\":\"{}\",\"url\":\"{}\",\"body\":{},\"headers\":[{}],\"timeout\":{}}}",
        HTTP_AWAIT_MARKER,
        escape_json(&key),
        escape_json(method),
        escape_json(url),
        body_json,
        headers_json,
        timeout_json,
    );
    SemaError::eval(payload)
}

/// Check whether an error is an HTTP await marker.
fn is_http_await_marker(err: &SemaError) -> bool {
    match err.inner() {
        SemaError::Eval(msg) => msg.starts_with(HTTP_AWAIT_MARKER),
        _ => false,
    }
}

/// Extract the JSON payload from an HTTP await marker error.
fn parse_http_marker(err: &SemaError) -> Option<String> {
    match err.inner() {
        SemaError::Eval(msg) if msg.starts_with(HTTP_AWAIT_MARKER) => {
            Some(msg[HTTP_AWAIT_MARKER.len()..].to_string())
        }
        _ => None,
    }
}

/// Clear the HTTP response cache.
fn clear_http_cache() {
    HTTP_CACHE.with(|c| c.borrow_mut().clear());
}

/// Convert a sema Value to a serde_json::Value for body serialization.
fn value_to_json_for_body(val: &Value) -> Result<serde_json::Value, SemaError> {
    match val.view() {
        ValueView::Nil => Ok(serde_json::Value::Null),
        ValueView::Bool(b) => Ok(serde_json::Value::Bool(b)),
        ValueView::Int(n) => Ok(serde_json::Value::Number(n.into())),
        ValueView::Float(f) => serde_json::Number::from_f64(f)
            .map(serde_json::Value::Number)
            .ok_or_else(|| SemaError::eval("http: cannot encode NaN/Infinity as JSON")),
        ValueView::String(s) => Ok(serde_json::Value::String(s.to_string())),
        ValueView::Keyword(s) => Ok(serde_json::Value::String(sema_core::resolve(s))),
        ValueView::Symbol(s) => Ok(serde_json::Value::String(sema_core::resolve(s))),
        ValueView::List(items) | ValueView::Vector(items) => {
            let arr: Result<Vec<_>, _> = items.iter().map(value_to_json_for_body).collect();
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
                obj.insert(key, value_to_json_for_body(v)?);
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
                obj.insert(key, value_to_json_for_body(v)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        _ => Err(SemaError::eval(format!(
            "http: cannot serialize {} to JSON",
            val.type_name()
        ))),
    }
}

/// Perform an HTTP request via the replay-with-cache strategy.
/// On cache hit, returns the cached response. On cache miss, returns a marker error.
fn wasm_http_request(
    method: &str,
    url: &str,
    body: Option<&Value>,
    opts: Option<&Value>,
) -> Result<Value, SemaError> {
    let mut headers: Vec<(String, String)> = Vec::new();
    let mut timeout_ms: Option<i64> = None;
    let mut has_content_type = false;

    if let Some(opts_val) = opts {
        if let Some(opts_map) = opts_val.as_map_rc() {
            if let Some(headers_val) = opts_map.get(&Value::keyword("headers")) {
                if let Some(hmap) = headers_val.as_map_rc() {
                    for (k, v) in hmap.iter() {
                        let key = match k.view() {
                            ValueView::String(s) => s.to_string(),
                            ValueView::Keyword(s) => sema_core::resolve(s),
                            _ => k.to_string(),
                        };
                        let val = match v.as_str() {
                            Some(s) => s.to_string(),
                            None => v.to_string(),
                        };
                        if key.eq_ignore_ascii_case("content-type") {
                            has_content_type = true;
                        }
                        headers.push((key, val));
                    }
                }
            }
            if let Some(timeout_val) = opts_map.get(&Value::keyword("timeout")) {
                if let Some(ms) = timeout_val.as_int() {
                    timeout_ms = Some(ms);
                }
            }
        }
    }

    let body_str = match body {
        Some(val) => {
            if let Some(s) = val.as_str() {
                Some(s.to_string())
            } else if val.as_map_rc().is_some() {
                let json = value_to_json_for_body(val)?;
                let json_str = serde_json::to_string(&json)
                    .map_err(|e| SemaError::eval(format!("http: json encode: {e}")))?;
                if !has_content_type {
                    headers.push(("Content-Type".to_string(), "application/json".to_string()));
                }
                Some(json_str)
            } else if val.is_nil() {
                None
            } else {
                Some(val.to_string())
            }
        }
        None => None,
    };

    headers.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));

    let key = http_cache_key(method, url, body_str.as_deref(), &headers);

    let cached = HTTP_CACHE.with(|c| c.borrow().get(&key).cloned());
    if let Some(val) = cached {
        return Ok(val);
    }

    Err(http_await_marker(
        method,
        url,
        body_str.as_deref(),
        &headers,
        timeout_ms,
    ))
}

/// Perform an HTTP fetch via the browser's `fetch()` API.
async fn perform_fetch(
    method: &str,
    url: &str,
    body: Option<&str>,
    headers: &[(String, String)],
    timeout_ms: Option<u64>,
) -> Result<Value, SemaError> {
    let window = web_sys::window()
        .ok_or_else(|| SemaError::Io("no global `window` available".to_string()))?;

    let opts = web_sys::RequestInit::new();
    opts.set_method(method);
    opts.set_mode(web_sys::RequestMode::Cors);

    if let Some(body_str) = body {
        opts.set_body(&JsValue::from_str(body_str));
    }

    let abort_controller = if timeout_ms.is_some() {
        let controller = web_sys::AbortController::new()
            .map_err(|_| SemaError::Io("failed to create AbortController".to_string()))?;
        opts.set_signal(Some(&controller.signal()));
        Some(controller)
    } else {
        None
    };

    let request = web_sys::Request::new_with_str_and_init(url, &opts).map_err(|e| {
        SemaError::Io(format!(
            "failed to create request: {}",
            e.as_string().unwrap_or_default()
        ))
    })?;

    for (k, v) in headers {
        request.headers().set(k, v).map_err(|e| {
            SemaError::Io(format!(
                "failed to set header: {}",
                e.as_string().unwrap_or_default()
            ))
        })?;
    }

    if let (Some(ms), Some(controller)) = (timeout_ms, &abort_controller) {
        let c = controller.clone();
        let closure = wasm_bindgen::closure::Closure::once(move || {
            c.abort();
        });
        let _ = window.set_timeout_with_callback_and_timeout_and_arguments_0(
            closure.as_ref().unchecked_ref(),
            ms as i32,
        );
        closure.forget();
    }

    let resp_jsvalue = JsFuture::from(window.fetch_with_request(&request))
        .await
        .map_err(|e| {
            let msg = e
                .as_string()
                .or_else(|| {
                    js_sys::Reflect::get(&e, &JsValue::from_str("message"))
                        .ok()
                        .and_then(|m| m.as_string())
                })
                .unwrap_or_else(|| "fetch failed".to_string());
            SemaError::Io(msg)
        })?;

    let response: web_sys::Response = resp_jsvalue
        .dyn_into()
        .map_err(|_| SemaError::Io("fetch did not return a Response".to_string()))?;

    let status = response.status() as i64;

    let mut resp_headers = BTreeMap::new();
    if let Ok(Some(iter)) = js_sys::try_iter(&response.headers()) {
        for entry in iter.flatten() {
            let arr: js_sys::Array = entry.into();
            if arr.length() >= 2 {
                let k = arr.get(0).as_string().unwrap_or_default();
                let v = arr.get(1).as_string().unwrap_or_default();
                resp_headers.insert(Value::keyword(&k), Value::string(&v));
            }
        }
    }

    let body_promise = response.text().map_err(|e| {
        SemaError::Io(format!(
            "failed to read response body: {}",
            e.as_string().unwrap_or_default()
        ))
    })?;
    let body_jsvalue = JsFuture::from(body_promise).await.map_err(|e| {
        SemaError::Io(format!(
            "failed to read response body: {}",
            e.as_string().unwrap_or_default()
        ))
    })?;
    let body_text = body_jsvalue.as_string().unwrap_or_default();

    let mut result = BTreeMap::new();
    result.insert(Value::keyword("status"), Value::int(status));
    result.insert(Value::keyword("headers"), Value::map(resp_headers));
    result.insert(Value::keyword("body"), Value::string(&body_text));

    Ok(Value::map(result))
}

/// Parse an HTTP marker JSON and perform the fetch, returning (cache_key, response).
async fn perform_fetch_from_marker(json_str: &str) -> Result<(String, Value), SemaError> {
    let parsed: serde_json::Value = serde_json::from_str(json_str)
        .map_err(|e| SemaError::eval(format!("failed to parse HTTP marker JSON: {e}")))?;

    let key = parsed["key"]
        .as_str()
        .ok_or_else(|| SemaError::eval("HTTP marker missing 'key'"))?
        .to_string();
    let method = parsed["method"]
        .as_str()
        .ok_or_else(|| SemaError::eval("HTTP marker missing 'method'"))?;
    let url = parsed["url"]
        .as_str()
        .ok_or_else(|| SemaError::eval("HTTP marker missing 'url'"))?;
    let body = parsed["body"].as_str();
    let timeout_ms = parsed["timeout"].as_u64();

    let mut headers = Vec::new();
    if let Some(arr) = parsed["headers"].as_array() {
        for pair in arr {
            if let Some(pair_arr) = pair.as_array() {
                if pair_arr.len() >= 2 {
                    let k = pair_arr[0].as_str().unwrap_or_default().to_string();
                    let v = pair_arr[1].as_str().unwrap_or_default().to_string();
                    headers.push((k, v));
                }
            }
        }
    }

    let response = perform_fetch(method, url, body, &headers, timeout_ms).await?;
    Ok((key, response))
}

/// Register print/println/display/newline that write to the output buffer instead of stdout
type WasmNativeFn = Box<dyn Fn(&[Value]) -> Result<Value, SemaError>>;

fn register_wasm_io(env: &Env) {
    let register = |name: &str, f: WasmNativeFn| {
        env.set(
            sema_core::intern(name),
            Value::native_fn(NativeFn::simple(name, move |args| f(args))),
        );
    };

    // display: append to current line, no newline (like native print! without newline)
    register(
        "display",
        Box::new(|args: &[Value]| {
            let mut out = String::new();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                if let Some(s) = arg.as_str() {
                    out.push_str(s);
                } else {
                    out.push_str(&format!("{arg}"));
                }
            }
            append_output(&out);
            Ok(Value::nil())
        }),
    );

    // print: append to current line, no newline
    register(
        "print",
        Box::new(|args: &[Value]| {
            let mut out = String::new();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(&format!("{arg}"));
            }
            append_output(&out);
            Ok(Value::nil())
        }),
    );

    // println: append to current line, then flush (emit newline)
    register(
        "println",
        Box::new(|args: &[Value]| {
            let mut out = String::new();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                if let Some(s) = arg.as_str() {
                    out.push_str(s);
                } else {
                    out.push_str(&format!("{arg}"));
                }
            }
            append_output(&out);
            flush_line();
            Ok(Value::nil())
        }),
    );

    // pprint: pretty-print a value and flush
    register(
        "pprint",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("pprint", "1", args.len()));
            }
            append_output(&pretty_print(&args[0], 80));
            flush_line();
            Ok(Value::nil())
        }),
    );

    // newline: flush current line
    register(
        "newline",
        Box::new(|_args: &[Value]| {
            flush_line();
            Ok(Value::nil())
        }),
    );

    register(
        "print-error",
        Box::new(|args: &[Value]| {
            let mut out = String::new();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(&format!("{arg}"));
            }
            append_output(&format!("[error] {out}"));
            flush_line();
            Ok(Value::nil())
        }),
    );

    register(
        "println-error",
        Box::new(|args: &[Value]| {
            let mut out = String::new();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(&format!("{arg}"));
            }
            append_output(&format!("[error] {out}"));
            flush_line();
            Ok(Value::nil())
        }),
    );

    // time-ms: use Date.now() from the web platform
    register(
        "time-ms",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("time-ms", "0", args.len()));
            }
            Ok(Value::int(Date::now() as i64))
        }),
    );

    // term/* pass-through shims (ANSI codes are useless in the browser)
    for name in &[
        "term/bold",
        "term/dim",
        "term/italic",
        "term/underline",
        "term/inverse",
        "term/strikethrough",
        "term/black",
        "term/red",
        "term/green",
        "term/yellow",
        "term/blue",
        "term/magenta",
        "term/cyan",
        "term/white",
        "term/gray",
        "term/strip",
    ] {
        let fn_name = name.to_string();
        register(
            name,
            Box::new(move |args: &[Value]| {
                if args.len() != 1 {
                    return Err(SemaError::arity(&fn_name, "1", args.len()));
                }
                let text = args[0]
                    .as_str()
                    .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
                Ok(Value::string(text))
            }),
        );
    }

    // term/style: return first arg unchanged
    register(
        "term/style",
        Box::new(|args: &[Value]| {
            if args.is_empty() {
                return Err(SemaError::arity("term/style", "1+", args.len()));
            }
            let text = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::string(text))
        }),
    );

    // term/rgb: return first arg unchanged
    register(
        "term/rgb",
        Box::new(|args: &[Value]| {
            if args.len() != 4 {
                return Err(SemaError::arity("term/rgb", "4", args.len()));
            }
            let text = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::string(text))
        }),
    );

    // sys/platform
    register(
        "sys/platform",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/platform", "0", args.len()));
            }
            Ok(Value::string("web"))
        }),
    );

    // sys/arch
    register(
        "sys/arch",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/arch", "0", args.len()));
            }
            Ok(Value::string("wasm32"))
        }),
    );

    // sys/os
    register(
        "sys/os",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/os", "0", args.len()));
            }
            Ok(Value::string("web"))
        }),
    );

    // sys/elapsed: nanoseconds since WASM module load
    register(
        "sys/elapsed",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/elapsed", "0", args.len()));
            }
            let nanos = WASM_START_MS.with(|&start| ((Date::now() - start) * 1_000_000.0) as i64);
            Ok(Value::int(nanos))
        }),
    );

    // sleep: no-op in WASM
    register(
        "sleep",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("sleep", "1", args.len()));
            }
            args[0]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
            Ok(Value::nil())
        }),
    );

    // env: always nil in WASM
    register(
        "env",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("env", "1", args.len()));
            }
            args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::nil())
        }),
    );

    // sys/set-env: no-op in WASM
    register(
        "sys/set-env",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("sys/set-env", "2", args.len()));
            }
            args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            Ok(Value::nil())
        }),
    );

    // sys/env-all: empty map in WASM
    register(
        "sys/env-all",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/env-all", "0", args.len()));
            }
            Ok(Value::map(std::collections::BTreeMap::new()))
        }),
    );

    // exit: not supported in WASM
    register(
        "exit",
        Box::new(|_args: &[Value]| Err(SemaError::eval("exit not supported in WASM"))),
    );

    // sys/interactive?: always false in WASM
    register(
        "sys/interactive?",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/interactive?", "0", args.len()));
            }
            Ok(Value::bool(false))
        }),
    );

    // path/join: join path segments with "/"
    register(
        "path/join",
        Box::new(|args: &[Value]| {
            if args.is_empty() {
                return Err(SemaError::arity("path/join", "1+", 0));
            }
            let mut parts = Vec::new();
            for arg in args {
                let s = arg
                    .as_str()
                    .ok_or_else(|| SemaError::type_error("string", arg.type_name()))?;
                parts.push(s.to_string());
            }
            let joined = parts
                .iter()
                .enumerate()
                .fold(String::new(), |mut acc, (i, part)| {
                    if i > 0 && !acc.ends_with('/') && !part.starts_with('/') {
                        acc.push('/');
                    }
                    acc.push_str(part);
                    acc
                });
            Ok(Value::string(&joined))
        }),
    );

    // path/dirname: parent directory of a path
    register(
        "path/dirname",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("path/dirname", "1", args.len()));
            }
            let s = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let trimmed = s.trim_end_matches('/');
            match trimmed.rfind('/') {
                Some(0) => Ok(Value::string("/")),
                Some(pos) => Ok(Value::string(&trimmed[..pos])),
                None => Ok(Value::nil()),
            }
        }),
    );

    // path/basename: filename component of a path
    register(
        "path/basename",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("path/basename", "1", args.len()));
            }
            let s = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let trimmed = s.trim_end_matches('/');
            match trimmed.rfind('/') {
                Some(pos) => Ok(Value::string(&trimmed[pos + 1..])),
                None => Ok(Value::string(trimmed)),
            }
        }),
    );

    // path/extension: file extension (without dot)
    register(
        "path/extension",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("path/extension", "1", args.len()));
            }
            let s = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let trimmed = s.trim_end_matches('/');
            let basename = match trimmed.rfind('/') {
                Some(pos) => &trimmed[pos + 1..],
                None => trimmed,
            };
            match basename.rfind('.') {
                Some(0) | None => Ok(Value::nil()),
                Some(pos) => Ok(Value::string(&basename[pos + 1..])),
            }
        }),
    );

    // path/absolute: in WASM, just return the input unchanged
    register(
        "path/absolute",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("path/absolute", "1", args.len()));
            }
            let s = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::string(s))
        }),
    );

    // --- web/* namespace: browser environment detection (WASM-only) ---

    register(
        "web/user-agent",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("web/user-agent", "0", args.len()));
            }
            match js_sys::eval("navigator.userAgent") {
                Ok(val) => match val.as_string() {
                    Some(s) => Ok(Value::string(&s)),
                    None => Ok(Value::nil()),
                },
                Err(_) => Ok(Value::nil()),
            }
        }),
    );

    register(
        "web/user-agent-data",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("web/user-agent-data", "0", args.len()));
            }
            // navigator.userAgentData is Chromium-only; returns nil on Firefox/Safari
            let script = r#"
                (function() {
                    var d = navigator.userAgentData;
                    if (!d) return null;
                    return JSON.stringify({
                        mobile: d.mobile,
                        platform: d.platform,
                        brands: d.brands.map(function(b) { return b.brand + "/" + b.version; })
                    });
                })()
            "#;
            match js_sys::eval(script) {
                Ok(val) => match val.as_string() {
                    Some(json_str) => {
                        // Parse the JSON into a Sema map
                        match serde_json::from_str::<serde_json::Value>(&json_str) {
                            Ok(json) => Ok(json_to_value(&json)),
                            Err(_) => Ok(Value::nil()),
                        }
                    }
                    None => Ok(Value::nil()),
                },
                Err(_) => Ok(Value::nil()),
            }
        }),
    );

    // --- HTTP via replay-with-cache (async eval catches markers and performs fetch) ---

    register(
        "http/get",
        Box::new(|args: &[Value]| {
            if args.is_empty() || args.len() > 2 {
                return Err(SemaError::arity("http/get", "1 or 2", args.len()));
            }
            let url = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            wasm_http_request("GET", url, None, args.get(1))
        }),
    );

    register(
        "http/post",
        Box::new(|args: &[Value]| {
            if args.len() < 2 || args.len() > 3 {
                return Err(SemaError::arity("http/post", "2 or 3", args.len()));
            }
            let url = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            wasm_http_request("POST", url, Some(&args[1]), args.get(2))
        }),
    );

    register(
        "http/put",
        Box::new(|args: &[Value]| {
            if args.len() < 2 || args.len() > 3 {
                return Err(SemaError::arity("http/put", "2 or 3", args.len()));
            }
            let url = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            wasm_http_request("PUT", url, Some(&args[1]), args.get(2))
        }),
    );

    register(
        "http/delete",
        Box::new(|args: &[Value]| {
            if args.is_empty() || args.len() > 2 {
                return Err(SemaError::arity("http/delete", "1 or 2", args.len()));
            }
            let url = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            wasm_http_request("DELETE", url, None, args.get(1))
        }),
    );

    register(
        "http/request",
        Box::new(|args: &[Value]| {
            if args.len() < 2 || args.len() > 4 {
                return Err(SemaError::arity("http/request", "2-4", args.len()));
            }
            let method = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let url = args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            let body = args.get(2);
            let opts = args.get(3);
            wasm_http_request(method, url, body, opts)
        }),
    );

    // --- sys/* stubs for unsupported system functions ---

    register(
        "sys/args",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/args", "0", args.len()));
            }
            Ok(Value::list(Vec::new()))
        }),
    );

    register(
        "sys/cwd",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/cwd", "0", args.len()));
            }
            Ok(Value::string("/"))
        }),
    );

    register(
        "sys/home-dir",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/home-dir", "0", args.len()));
            }
            Ok(Value::nil())
        }),
    );

    register(
        "sys/temp-dir",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/temp-dir", "0", args.len()));
            }
            Ok(Value::string("/tmp"))
        }),
    );

    register(
        "sys/hostname",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/hostname", "0", args.len()));
            }
            Ok(Value::nil())
        }),
    );

    register(
        "sys/user",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/user", "0", args.len()));
            }
            Ok(Value::nil())
        }),
    );

    register(
        "sys/pid",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/pid", "0", args.len()));
            }
            Ok(Value::int(0))
        }),
    );

    register(
        "sys/which",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("sys/which", "1", args.len()));
            }
            args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::nil())
        }),
    );

    register(
        "sys/tty",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/tty", "0", args.len()));
            }
            Ok(Value::nil())
        }),
    );

    // --- VFS file operation shims ---

    register(
        "file/read",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/read", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            VFS.with(|vfs| match vfs.borrow().get(path.as_str()) {
                Some(content) => Ok(Value::string(content)),
                None => Err(SemaError::Io(format!("file/read {path}: No such file"))),
            })
        }),
    );

    register(
        "file/write",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/write", "2", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            let content = args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            vfs_check_quota(path, content.len())?;
            VFS.with(|vfs| {
                let mut map = vfs.borrow_mut();
                let old_len = map.get(path.as_str()).map_or(0, |s| s.len());
                map.insert(path.to_string(), content.to_string());
                VFS_TOTAL_BYTES.with(|t| {
                    t.set(
                        t.get()
                            .saturating_add(content.len())
                            .saturating_sub(old_len),
                    );
                });
            });
            Ok(Value::nil())
        }),
    );

    register(
        "file/exists?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/exists?", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            let in_vfs = VFS.with(|vfs| vfs.borrow().contains_key(path.as_str()));
            let in_dirs = VFS_DIRS.with(|dirs| dirs.borrow().contains(path.as_str()));
            Ok(Value::bool(in_vfs || in_dirs))
        }),
    );

    register(
        "file/delete",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/delete", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            VFS.with(|vfs| match vfs.borrow_mut().remove(path.as_str()) {
                Some(old) => {
                    VFS_TOTAL_BYTES.with(|t| t.set(t.get().saturating_sub(old.len())));
                    Ok(Value::nil())
                }
                None => Err(SemaError::Io(format!("file/delete {path}: No such file"))),
            })
        }),
    );

    register(
        "file/rename",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/rename", "2", args.len()));
            }
            let from = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let from = &normalize_path(from)?;
            let to = args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            let to = &normalize_path(to)?;
            VFS.with(|vfs| {
                let mut map = vfs.borrow_mut();
                match map.remove(from.as_str()) {
                    Some(content) => {
                        let overwritten_len = map.get(to.as_str()).map_or(0, |s| s.len());
                        map.insert(to.to_string(), content);
                        VFS_TOTAL_BYTES.with(|t| t.set(t.get().saturating_sub(overwritten_len)));
                        Ok(Value::nil())
                    }
                    None => Err(SemaError::Io(format!(
                        "file/rename {from} -> {to}: No such file"
                    ))),
                }
            })
        }),
    );

    register(
        "file/list",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/list", "1", args.len()));
            }
            let dir = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let dir = &normalize_path(dir)?;
            let prefix = if dir == "/" {
                "/".to_string()
            } else {
                format!("{dir}/")
            };
            let mut names = BTreeSet::new();
            VFS.with(|vfs| {
                for key in vfs.borrow().keys() {
                    if let Some(rest) = key.strip_prefix(&prefix) {
                        if !rest.is_empty() && !rest.contains('/') {
                            names.insert(rest.to_string());
                        }
                    }
                }
            });
            VFS_DIRS.with(|dirs| {
                for d in dirs.borrow().iter() {
                    if let Some(rest) = d.strip_prefix(&prefix) {
                        if !rest.is_empty() && !rest.contains('/') {
                            names.insert(rest.to_string());
                        }
                    }
                }
            });
            let entries: Vec<Value> = names.into_iter().map(|n| Value::string(&n)).collect();
            Ok(Value::list(entries))
        }),
    );

    register(
        "file/mkdir",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/mkdir", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            VFS_DIRS.with(|dirs| {
                let mut set = dirs.borrow_mut();
                let mut current = String::new();
                for seg in path.strip_prefix('/').unwrap_or(path).split('/') {
                    current.push('/');
                    current.push_str(seg);
                    set.insert(current.clone());
                }
            });
            Ok(Value::nil())
        }),
    );

    register(
        "file/is-directory?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/is-directory?", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            Ok(Value::bool(
                VFS_DIRS.with(|dirs| dirs.borrow().contains(path.as_str())),
            ))
        }),
    );

    register(
        "file/is-file?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/is-file?", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            Ok(Value::bool(
                VFS.with(|vfs| vfs.borrow().contains_key(path.as_str())),
            ))
        }),
    );

    register(
        "file/is-symlink?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/is-symlink?", "1", args.len()));
            }
            let _path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::bool(false))
        }),
    );

    register(
        "file/append",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/append", "2", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            let content = args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            let combined_len = VFS
                .with(|vfs| vfs.borrow().get(path.as_str()).map_or(0, |s| s.len()))
                + content.len();
            vfs_check_quota(path, combined_len)?;
            VFS.with(|vfs| {
                let mut map = vfs.borrow_mut();
                map.entry(path.to_string())
                    .and_modify(|existing| existing.push_str(content))
                    .or_insert_with(|| content.to_string());
            });
            VFS_TOTAL_BYTES.with(|t| t.set(t.get() + content.len()));
            Ok(Value::nil())
        }),
    );

    register(
        "file/copy",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/copy", "2", args.len()));
            }
            let src = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let src = &normalize_path(src)?;
            let dest = args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            let dest = &normalize_path(dest)?;
            VFS.with(|vfs| {
                let map = vfs.borrow();
                match map.get(src.as_str()) {
                    Some(content) => {
                        let content = content.clone();
                        drop(map);
                        vfs_check_quota(dest, content.len())?;
                        let mut map = vfs.borrow_mut();
                        let old_len = map.get(dest.as_str()).map_or(0, |s| s.len());
                        map.insert(dest.to_string(), content.clone());
                        VFS_TOTAL_BYTES.with(|t| {
                            t.set(
                                t.get()
                                    .saturating_add(content.len())
                                    .saturating_sub(old_len),
                            );
                        });
                        Ok(Value::nil())
                    }
                    None => Err(SemaError::Io(format!(
                        "file/copy {src} -> {dest}: No such file"
                    ))),
                }
            })
        }),
    );

    register(
        "file/read-lines",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/read-lines", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            VFS.with(|vfs| match vfs.borrow().get(path.as_str()) {
                Some(content) => {
                    let lines: Vec<Value> = content.split('\n').map(Value::string).collect();
                    Ok(Value::list(lines))
                }
                None => Err(SemaError::Io(format!(
                    "file/read-lines {path}: No such file"
                ))),
            })
        }),
    );

    register(
        "file/write-lines",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/write-lines", "2", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let path = &normalize_path(path)?;
            let lines = if let Some(l) = args[1].as_list() {
                l
            } else if let Some(v) = args[1].as_vector() {
                v
            } else {
                return Err(SemaError::type_error("list or vector", args[1].type_name()));
            };
            let strs: Vec<String> = lines
                .iter()
                .map(|v| {
                    if let Some(s) = v.as_str() {
                        s.to_string()
                    } else {
                        v.to_string()
                    }
                })
                .collect();
            let content = strs.join("\n");
            vfs_check_quota(path, content.len())?;
            let content_len = content.len();
            VFS.with(|vfs| {
                let mut map = vfs.borrow_mut();
                let old_len = map.get(path.as_str()).map_or(0, |s| s.len());
                map.insert(path.to_string(), content);
                VFS_TOTAL_BYTES.with(|t| {
                    t.set(t.get().saturating_add(content_len).saturating_sub(old_len));
                });
            });
            Ok(Value::nil())
        }),
    );

    register(
        "file/info",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/info", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let is_file = VFS.with(|vfs| vfs.borrow().contains_key(path));
            let is_dir = VFS_DIRS.with(|dirs| dirs.borrow().contains(path));
            if !is_file && !is_dir {
                return Err(SemaError::Io(format!(
                    "file/info {path}: No such file or directory"
                )));
            }
            let size = if is_file {
                VFS.with(|vfs| vfs.borrow().get(path).map(|c| c.len() as i64).unwrap_or(0))
            } else {
                0
            };
            let mut map = BTreeMap::new();
            map.insert(Value::keyword("size"), Value::int(size));
            map.insert(Value::keyword("is-dir"), Value::bool(is_dir));
            map.insert(Value::keyword("is-file"), Value::bool(is_file));
            Ok(Value::map(map))
        }),
    );

    // --- IO shims unsupported in WASM ---

    register(
        "read-line",
        Box::new(|_args: &[Value]| Err(SemaError::eval("read-line not supported in WASM"))),
    );

    register(
        "read-stdin",
        Box::new(|_args: &[Value]| Err(SemaError::eval("read-stdin not supported in WASM"))),
    );

    register(
        "shell",
        Box::new(|_args: &[Value]| Err(SemaError::eval("shell not supported in WASM"))),
    );

    // --- Reader/parser functions ---

    register(
        "load",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("load", "1", args.len()));
            }
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            VFS.with(|vfs| match vfs.borrow().get(path) {
                Some(content) => {
                    let exprs = sema_reader::read_many(content)?;
                    Ok(Value::list(exprs))
                }
                None => Err(SemaError::Io(format!("load {path}: No such file"))),
            })
        }),
    );

    register(
        "read",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("read", "1", args.len()));
            }
            let s = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            sema_reader::read(s)
        }),
    );

    register(
        "read-many",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("read-many", "1", args.len()));
            }
            let s = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let exprs = sema_reader::read_many(s)?;
            Ok(Value::list(exprs))
        }),
    );

    register(
        "error",
        Box::new(|args: &[Value]| {
            if args.is_empty() {
                return Err(SemaError::eval("error called with no message"));
            }
            let msg = if let Some(s) = args[0].as_str() {
                s.to_string()
            } else {
                args[0].to_string()
            };
            Err(SemaError::eval(msg))
        }),
    );
}

#[wasm_bindgen(js_name = SemaInterpreter)]
pub struct WasmInterpreter {
    inner: sema_eval::Interpreter,
}

impl Default for WasmInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[wasm_bindgen(js_class = SemaInterpreter)]
impl WasmInterpreter {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmInterpreter {
        let interp = sema_eval::Interpreter::new();

        // Override print/println/display with our buffer-based versions
        register_wasm_io(&interp.global_env);

        // Set eval step limit to prevent infinite loops from crashing the browser tab.
        // 10M steps is enough for complex examples but prevents runaway computation.
        interp.ctx.set_eval_step_limit(10_000_000);

        WasmInterpreter { inner: interp }
    }

    /// Evaluate code, returns JSON: {"value": "...", "output": ["...", ...], "error": null}
    /// or {"value": null, "output": [...], "error": "..."}
    pub fn eval(&self, code: &str) -> JsValue {
        OUTPUT.with(|o| o.borrow_mut().clear());
        LINE_BUF.with(|b| b.borrow_mut().clear());

        let env = sema_core::Env::with_parent(self.inner.global_env.clone());
        let json_str = match sema_eval::eval_string(&self.inner.ctx, code, &env) {
            Ok(val) => {
                let output = take_output();
                let val_str = if val.is_nil() {
                    "null".to_string()
                } else {
                    format!("\"{}\"", escape_json(&format!("{val}")))
                };
                format!(
                    "{{\"value\":{},\"output\":[{}],\"error\":null}}",
                    val_str,
                    output
                        .iter()
                        .map(|s| format!("\"{}\"", escape_json(s)))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
            Err(e) => {
                let output = take_output();
                let mut err_str = format!("{}", e.inner());
                if let Some(trace) = e.stack_trace() {
                    err_str.push_str(&format!("\n{trace}"));
                }
                if let Some(hint) = e.hint() {
                    err_str.push_str(&format!("\n  hint: {hint}"));
                }
                if let Some(note) = e.note() {
                    err_str.push_str(&format!("\n  note: {note}"));
                }
                format!(
                    "{{\"value\":null,\"output\":[{}],\"error\":\"{}\"}}",
                    output
                        .iter()
                        .map(|s| format!("\"{}\"", escape_json(s)))
                        .collect::<Vec<_>>()
                        .join(","),
                    escape_json(&err_str)
                )
            }
        };
        js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
    }

    /// Evaluate in the global env so defines persist
    #[wasm_bindgen(js_name = evalGlobal)]
    pub fn eval_global(&self, code: &str) -> JsValue {
        OUTPUT.with(|o| o.borrow_mut().clear());
        LINE_BUF.with(|b| b.borrow_mut().clear());

        let json_str = match sema_eval::eval_string(&self.inner.ctx, code, &self.inner.global_env) {
            Ok(val) => {
                let output = take_output();
                let val_str = if val.is_nil() {
                    "null".to_string()
                } else {
                    format!("\"{}\"", escape_json(&pretty_print(&val, 80)))
                };
                format!(
                    "{{\"value\":{},\"output\":[{}],\"error\":null}}",
                    val_str,
                    output
                        .iter()
                        .map(|s| format!("\"{}\"", escape_json(s)))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
            Err(e) => {
                let output = take_output();
                let mut err_str = format!("{}", e.inner());
                if let Some(trace) = e.stack_trace() {
                    err_str.push_str(&format!("\n{trace}"));
                }
                if let Some(hint) = e.hint() {
                    err_str.push_str(&format!("\n  hint: {hint}"));
                }
                if let Some(note) = e.note() {
                    err_str.push_str(&format!("\n  note: {note}"));
                }
                format!(
                    "{{\"value\":null,\"output\":[{}],\"error\":\"{}\"}}",
                    output
                        .iter()
                        .map(|s| format!("\"{}\"", escape_json(s)))
                        .collect::<Vec<_>>()
                        .join(","),
                    escape_json(&err_str)
                )
            }
        };
        js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
    }

    /// Evaluate code via the bytecode VM, returns same JSON format as eval_global
    #[wasm_bindgen(js_name = evalVM)]
    pub fn eval_vm(&self, code: &str) -> JsValue {
        OUTPUT.with(|o| o.borrow_mut().clear());
        LINE_BUF.with(|b| b.borrow_mut().clear());

        let json_str = match self.inner.eval_str_compiled(code) {
            Ok(val) => {
                let output = take_output();
                let val_str = if val.is_nil() {
                    "null".to_string()
                } else {
                    format!("\"{}\"", escape_json(&pretty_print(&val, 80)))
                };
                format!(
                    "{{\"value\":{},\"output\":[{}],\"error\":null}}",
                    val_str,
                    output
                        .iter()
                        .map(|s| format!("\"{}\"", escape_json(s)))
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }
            Err(e) => {
                let output = take_output();
                let mut err_str = format!("{}", e.inner());
                if let Some(trace) = e.stack_trace() {
                    err_str.push_str(&format!("\n{trace}"));
                }
                if let Some(hint) = e.hint() {
                    err_str.push_str(&format!("\n  hint: {hint}"));
                }
                if let Some(note) = e.note() {
                    err_str.push_str(&format!("\n  note: {note}"));
                }
                format!(
                    "{{\"value\":null,\"output\":[{}],\"error\":\"{}\"}}",
                    output
                        .iter()
                        .map(|s| format!("\"{}\"", escape_json(s)))
                        .collect::<Vec<_>>()
                        .join(","),
                    escape_json(&err_str)
                )
            }
        };
        js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
    }

    /// Evaluate code with async HTTP support (tree-walker, global env)
    #[wasm_bindgen(js_name = evalAsync)]
    pub async fn eval_async(&self, code: &str) -> JsValue {
        clear_http_cache();

        for _ in 0..MAX_REPLAYS {
            OUTPUT.with(|o| o.borrow_mut().clear());
            LINE_BUF.with(|b| b.borrow_mut().clear());

            match sema_eval::eval_string(&self.inner.ctx, code, &self.inner.global_env) {
                Ok(val) => {
                    let output = take_output();
                    let val_str = if val.is_nil() {
                        "null".to_string()
                    } else {
                        format!("\"{}\"", escape_json(&pretty_print(&val, 80)))
                    };
                    let json_str = format!(
                        "{{\"value\":{},\"output\":[{}],\"error\":null}}",
                        val_str,
                        output
                            .iter()
                            .map(|s| format!("\"{}\"", escape_json(s)))
                            .collect::<Vec<_>>()
                            .join(",")
                    );
                    return js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL);
                }
                Err(e) => {
                    if is_http_await_marker(&e) {
                        if let Some(json_str) = parse_http_marker(&e) {
                            match perform_fetch_from_marker(&json_str).await {
                                Ok((key, response)) => {
                                    HTTP_CACHE.with(|c| {
                                        c.borrow_mut().insert(key, response);
                                    });
                                    continue;
                                }
                                Err(fetch_err) => {
                                    let output = take_output();
                                    let err_str = format!("{}", fetch_err.inner());
                                    let json_str = format!(
                                        "{{\"value\":null,\"output\":[{}],\"error\":\"{}\"}}",
                                        output
                                            .iter()
                                            .map(|s| format!("\"{}\"", escape_json(s)))
                                            .collect::<Vec<_>>()
                                            .join(","),
                                        escape_json(&err_str)
                                    );
                                    return js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL);
                                }
                            }
                        }
                    }
                    let output = take_output();
                    let mut err_str = format!("{}", e.inner());
                    if let Some(trace) = e.stack_trace() {
                        err_str.push_str(&format!("\n{trace}"));
                    }
                    if let Some(hint) = e.hint() {
                        err_str.push_str(&format!("\n  hint: {hint}"));
                    }
                    if let Some(note) = e.note() {
                        err_str.push_str(&format!("\n  note: {note}"));
                    }
                    let json_str = format!(
                        "{{\"value\":null,\"output\":[{}],\"error\":\"{}\"}}",
                        output
                            .iter()
                            .map(|s| format!("\"{}\"", escape_json(s)))
                            .collect::<Vec<_>>()
                            .join(","),
                        escape_json(&err_str)
                    );
                    return js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL);
                }
            }
        }

        let json_str = format!(
            "{{\"value\":null,\"output\":[],\"error\":\"{}\"}}",
            escape_json("exceeded maximum number of HTTP requests (50)")
        );
        js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
    }

    /// Evaluate code with async HTTP support (bytecode VM)
    #[wasm_bindgen(js_name = evalVMAsync)]
    pub async fn eval_vm_async(&self, code: &str) -> JsValue {
        clear_http_cache();

        for _ in 0..MAX_REPLAYS {
            OUTPUT.with(|o| o.borrow_mut().clear());
            LINE_BUF.with(|b| b.borrow_mut().clear());

            match self.inner.eval_str_compiled(code) {
                Ok(val) => {
                    let output = take_output();
                    let val_str = if val.is_nil() {
                        "null".to_string()
                    } else {
                        format!("\"{}\"", escape_json(&pretty_print(&val, 80)))
                    };
                    let json_str = format!(
                        "{{\"value\":{},\"output\":[{}],\"error\":null}}",
                        val_str,
                        output
                            .iter()
                            .map(|s| format!("\"{}\"", escape_json(s)))
                            .collect::<Vec<_>>()
                            .join(",")
                    );
                    return js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL);
                }
                Err(e) => {
                    if is_http_await_marker(&e) {
                        if let Some(json_str) = parse_http_marker(&e) {
                            match perform_fetch_from_marker(&json_str).await {
                                Ok((key, response)) => {
                                    HTTP_CACHE.with(|c| {
                                        c.borrow_mut().insert(key, response);
                                    });
                                    continue;
                                }
                                Err(fetch_err) => {
                                    let output = take_output();
                                    let err_str = format!("{}", fetch_err.inner());
                                    let json_str = format!(
                                        "{{\"value\":null,\"output\":[{}],\"error\":\"{}\"}}",
                                        output
                                            .iter()
                                            .map(|s| format!("\"{}\"", escape_json(s)))
                                            .collect::<Vec<_>>()
                                            .join(","),
                                        escape_json(&err_str)
                                    );
                                    return js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL);
                                }
                            }
                        }
                    }
                    let output = take_output();
                    let mut err_str = format!("{}", e.inner());
                    if let Some(trace) = e.stack_trace() {
                        err_str.push_str(&format!("\n{trace}"));
                    }
                    if let Some(hint) = e.hint() {
                        err_str.push_str(&format!("\n  hint: {hint}"));
                    }
                    if let Some(note) = e.note() {
                        err_str.push_str(&format!("\n  note: {note}"));
                    }
                    let json_str = format!(
                        "{{\"value\":null,\"output\":[{}],\"error\":\"{}\"}}",
                        output
                            .iter()
                            .map(|s| format!("\"{}\"", escape_json(s)))
                            .collect::<Vec<_>>()
                            .join(","),
                        escape_json(&err_str)
                    );
                    return js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL);
                }
            }
        }

        let json_str = format!(
            "{{\"value\":null,\"output\":[],\"error\":\"{}\"}}",
            escape_json("exceeded maximum number of HTTP requests (50)")
        );
        js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
    }

    /// Create interpreter with options: {stdlib: false, deny: ["network", "fs-write"]}
    #[wasm_bindgen(js_name = createWithOptions)]
    pub fn new_with_options(opts: JsValue) -> WasmInterpreter {
        let with_stdlib = js_sys::Reflect::get(&opts, &JsValue::from_str("stdlib"))
            .ok()
            .and_then(|v| v.as_bool())
            .unwrap_or(true);

        let interp = if with_stdlib {
            sema_eval::Interpreter::new()
        } else {
            use std::rc::Rc;
            let env = sema_core::Env::new();
            let ctx = sema_core::EvalContext::new();
            sema_core::set_eval_callback(&ctx, sema_eval::eval_value);
            sema_core::set_call_callback(&ctx, sema_eval::call_value);
            let global_env = Rc::new(env);
            sema_eval::Interpreter { global_env, ctx }
        };

        register_wasm_io(&interp.global_env);
        interp.ctx.set_eval_step_limit(10_000_000);

        // Apply deny list: overwrite denied functions with PermissionDenied stubs
        if let Ok(deny_val) = js_sys::Reflect::get(&opts, &JsValue::from_str("deny")) {
            if let Some(deny_arr) = deny_val.dyn_ref::<js_sys::Array>() {
                let mut denied_caps: Vec<String> = Vec::new();
                for i in 0..deny_arr.length() {
                    if let Some(s) = deny_arr.get(i).as_string() {
                        denied_caps.push(s);
                    }
                }

                let deny_fns = |env: &Env, cap: &str, fn_names: &[&str]| {
                    for &name in fn_names {
                        let cap_name = cap.to_string();
                        let fn_name = name.to_string();
                        env.set(
                            sema_core::intern(name),
                            Value::native_fn(NativeFn::simple(name, move |_args| {
                                Err(SemaError::PermissionDenied {
                                    function: fn_name.clone(),
                                    capability: cap_name.clone(),
                                })
                            })),
                        );
                    }
                };

                for cap in &denied_caps {
                    match cap.as_str() {
                        "network" => deny_fns(
                            &interp.global_env,
                            "network",
                            &[
                                "http/get",
                                "http/post",
                                "http/put",
                                "http/delete",
                                "http/request",
                            ],
                        ),
                        "fs-read" => deny_fns(
                            &interp.global_env,
                            "fs-read",
                            &[
                                "file/read",
                                "file/exists?",
                                "file/list",
                                "file/is-directory?",
                                "file/is-file?",
                                "file/is-symlink?",
                            ],
                        ),
                        "fs-write" => deny_fns(
                            &interp.global_env,
                            "fs-write",
                            &[
                                "file/write",
                                "file/delete",
                                "file/rename",
                                "file/mkdir",
                                "file/append",
                            ],
                        ),
                        _ => {} // Unknown caps are silently ignored
                    }
                }
            }
        }

        WasmInterpreter { inner: interp }
    }

    /// Register a JavaScript function callable from Sema code.
    #[wasm_bindgen(js_name = registerFunction)]
    pub fn register_fn(&self, name: &str, callback: &js_sys::Function) {
        use sema_core::{NativeFn, SemaError, Value};

        let callback = callback.clone();
        let fn_name = name.to_string();

        let native = NativeFn::simple(&fn_name, move |args: &[Value]| {
            // Pass native JS values
            let js_array = js_sys::Array::new();
            for arg in args {
                js_array.push(&sema_value_to_jsvalue(arg));
            }

            let result = callback.apply(&JsValue::NULL, &js_array).map_err(|e| {
                let msg = e.as_string().unwrap_or_else(|| format!("{:?}", e));
                SemaError::eval(format!("JS callback error: {msg}"))
            })?;

            // Convert JS result back to Sema value
            if result.is_undefined() || result.is_null() {
                Ok(Value::nil())
            } else if let Some(b) = result.as_bool() {
                Ok(Value::bool(b))
            } else if let Some(n) = result.as_f64() {
                if n.fract() == 0.0 && n >= i64::MIN as f64 && n <= i64::MAX as f64 {
                    Ok(Value::int(n as i64))
                } else {
                    Ok(Value::float(n))
                }
            } else if let Some(s) = result.as_string() {
                // Try parsing as JSON first for structured returns
                match serde_json::from_str::<serde_json::Value>(&s) {
                    Ok(json) if !json.is_string() => Ok(json_to_value(&json)),
                    _ => Ok(Value::string(&s)),
                }
            } else {
                // Try to serialize via JSON for objects/arrays
                match js_sys::JSON::stringify(&result) {
                    Ok(json_str) => {
                        let s: String = json_str.into();
                        match serde_json::from_str::<serde_json::Value>(&s) {
                            Ok(json) => Ok(json_to_value(&json)),
                            Err(_) => Ok(Value::string(&s)),
                        }
                    }
                    Err(_) => Ok(Value::nil()),
                }
            }
        });

        self.inner
            .global_env
            .set_str(name, Value::native_fn(native));
    }

    /// Inject a virtual module so that `(import "name")` resolves without a file.
    #[wasm_bindgen(js_name = preloadModule)]
    pub fn preload_module(&self, name: &str, source: &str) -> JsValue {
        use sema_core::{intern, resolve, Env, SemaError, Value};
        use std::collections::BTreeMap;

        let result = (|| -> Result<(), SemaError> {
            let (exprs, spans) = sema_reader::read_many_with_spans(source)
                .map_err(|e| SemaError::eval(format!("{e}")))?;
            self.inner.ctx.merge_span_table(spans);

            let module_env = Env::with_parent(self.inner.global_env.clone());
            self.inner.ctx.clear_module_exports();

            for expr in &exprs {
                sema_eval::eval_value(&self.inner.ctx, expr, &module_env)?;
            }

            let declared = self.inner.ctx.take_module_exports();
            let bindings = module_env.bindings.borrow();
            let exports: BTreeMap<String, Value> = match declared {
                Some(names) => names
                    .iter()
                    .filter_map(|n| {
                        let spur = intern(n);
                        bindings.get(&spur).map(|v| (n.clone(), v.clone()))
                    })
                    .collect(),
                None => bindings
                    .iter()
                    .map(|(k, v)| (resolve(*k), v.clone()))
                    .collect(),
            };
            drop(bindings);

            self.inner
                .ctx
                .cache_module(std::path::PathBuf::from(name), exports);
            Ok(())
        })();

        let json_str = match result {
            Ok(()) => r#"{"ok":true,"error":null}"#.to_string(),
            Err(e) => format!(
                r#"{{"ok":false,"error":"{}"}}"#,
                escape_json(&format!("{}", e.inner()))
            ),
        };
        js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
    }

    /// Read a file from the virtual filesystem.
    #[wasm_bindgen(js_name = readFile)]
    pub fn read_file(&self, path: &str) -> JsValue {
        let path = match normalize_path(path) {
            Ok(p) => p,
            Err(_) => return JsValue::NULL,
        };
        VFS.with(|vfs| match vfs.borrow().get(&path) {
            Some(content) => JsValue::from_str(content),
            None => JsValue::NULL,
        })
    }

    /// Write a file to the virtual filesystem.
    #[wasm_bindgen(js_name = writeFile)]
    pub fn write_file(&self, path: &str, content: &str) -> JsValue {
        let path = match normalize_path(path) {
            Ok(p) => p,
            Err(e) => return JsValue::from_str(&format!("{}", e.inner())),
        };
        match vfs_check_quota(&path, content.len()) {
            Ok(()) => {
                VFS.with(|vfs| {
                    let mut map = vfs.borrow_mut();
                    let old_len = map.get(&path).map_or(0, |s| s.len());
                    map.insert(path.to_string(), content.to_string());
                    VFS_TOTAL_BYTES.with(|t| {
                        t.set(
                            t.get()
                                .saturating_add(content.len())
                                .saturating_sub(old_len),
                        );
                    });
                });
                JsValue::NULL
            }
            Err(e) => {
                let msg = format!("{}", e.inner());
                JsValue::from_str(&msg)
            }
        }
    }

    /// Delete a file from the virtual filesystem. Returns true if the file existed.
    #[wasm_bindgen(js_name = deleteFile)]
    pub fn delete_file(&self, path: &str) -> bool {
        let path = match normalize_path(path) {
            Ok(p) => p,
            Err(_) => return false,
        };
        VFS.with(|vfs| match vfs.borrow_mut().remove(&path) {
            Some(old) => {
                VFS_TOTAL_BYTES.with(|t| t.set(t.get().saturating_sub(old.len())));
                true
            }
            None => false,
        })
    }

    /// List files and directories in the given directory path.
    #[wasm_bindgen(js_name = listFiles)]
    pub fn list_files(&self, dir: &str) -> JsValue {
        let dir = match normalize_path(dir) {
            Ok(p) => p,
            Err(_) => return js_sys::Array::new().into(),
        };
        let prefix = if dir == "/" {
            "/".to_string()
        } else {
            format!("{dir}/")
        };
        let mut names = BTreeSet::new();
        VFS.with(|vfs| {
            for key in vfs.borrow().keys() {
                if let Some(rest) = key.strip_prefix(&prefix) {
                    if let Some(first) = rest.split('/').next() {
                        if !first.is_empty() {
                            names.insert(first.to_string());
                        }
                    }
                }
            }
        });
        VFS_DIRS.with(|dirs| {
            for d in dirs.borrow().iter() {
                if let Some(rest) = d.strip_prefix(&prefix) {
                    if let Some(first) = rest.split('/').next() {
                        if !first.is_empty() {
                            names.insert(first.to_string());
                        }
                    }
                }
            }
        });
        let arr = js_sys::Array::new();
        for name in names {
            arr.push(&JsValue::from_str(&name));
        }
        arr.into()
    }

    /// Check if a path exists in the virtual filesystem (file or directory).
    #[wasm_bindgen(js_name = fileExists)]
    pub fn file_exists(&self, path: &str) -> bool {
        let path = match normalize_path(path) {
            Ok(p) => p,
            Err(_) => return false,
        };
        let in_vfs = VFS.with(|vfs| vfs.borrow().contains_key(&path));
        let in_dirs = VFS_DIRS.with(|dirs| dirs.borrow().contains(&path));
        in_vfs || in_dirs
    }

    /// Create a directory in the virtual filesystem.
    pub fn mkdir(&self, path: &str) {
        let path = match normalize_path(path) {
            Ok(p) => p,
            Err(_) => return,
        };
        VFS_DIRS.with(|dirs| {
            let mut set = dirs.borrow_mut();
            let mut current = String::new();
            for seg in path.strip_prefix('/').unwrap_or(&path).split('/') {
                current.push('/');
                current.push_str(seg);
                set.insert(current.clone());
            }
        });
    }

    /// Check if a path is a directory in the virtual filesystem.
    #[wasm_bindgen(js_name = isDirectory)]
    pub fn is_directory(&self, path: &str) -> bool {
        let path = match normalize_path(path) {
            Ok(p) => p,
            Err(_) => return false,
        };
        VFS_DIRS.with(|dirs| dirs.borrow().contains(&path))
    }

    /// Get VFS usage statistics.
    #[wasm_bindgen(js_name = vfsStats)]
    pub fn vfs_stats(&self) -> JsValue {
        let file_count = VFS.with(|vfs| vfs.borrow().len());
        let total_bytes = VFS_TOTAL_BYTES.with(|t| t.get());
        let json_str = format!(
            "{{\"files\":{},\"bytes\":{},\"maxFiles\":{},\"maxBytes\":{},\"maxFileBytes\":{}}}",
            file_count, total_bytes, VFS_MAX_FILES, VFS_MAX_TOTAL_BYTES, VFS_MAX_FILE_BYTES
        );
        js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
    }

    /// Clear all files and directories from the virtual filesystem.
    #[wasm_bindgen(js_name = resetVFS)]
    pub fn reset_vfs(&self) {
        VFS.with(|vfs| vfs.borrow_mut().clear());
        VFS_DIRS.with(|dirs| {
            let mut set = dirs.borrow_mut();
            set.clear();
            set.insert("/".to_string());
        });
        VFS_TOTAL_BYTES.with(|t| t.set(0));
    }

    /// Get the Sema version
    pub fn version(&self) -> String {
        env!("CARGO_PKG_VERSION").to_string()
    }
}

fn sema_value_to_jsvalue(val: &Value) -> JsValue {
    match val.view() {
        ValueView::Nil => JsValue::NULL,
        ValueView::Bool(b) => JsValue::from_bool(b),
        ValueView::Int(n) => JsValue::from_f64(n as f64),
        ValueView::Float(f) => JsValue::from_f64(f),
        ValueView::String(s) => JsValue::from_str(&s),
        ValueView::Keyword(s) => JsValue::from_str(&format!(":{}", sema_core::resolve(s))),
        ValueView::Symbol(s) => JsValue::from_str(&sema_core::resolve(s)),
        _ => {
            // For complex types (lists, maps, vectors), go through JSON
            match value_to_json_for_body(val) {
                Ok(json) => {
                    let s = serde_json::to_string(&json).unwrap_or_default();
                    js_sys::JSON::parse(&s).unwrap_or(JsValue::NULL)
                }
                Err(_) => JsValue::from_str(&format!("{val}")),
            }
        }
    }
}

fn json_to_value(json: &serde_json::Value) -> Value {
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

/// Format Sema source code. Returns JSON: {"formatted": "...", "error": null}
/// or {"formatted": null, "error": "..."}
#[wasm_bindgen(js_name = formatCode)]
pub fn format_code(code: &str, width: usize, indent: usize, align: bool) -> JsValue {
    match sema_fmt::format_source_opts(code, width, indent, align) {
        Ok(formatted) => {
            let json_str = format!(
                "{{\"formatted\":\"{}\",\"error\":null}}",
                escape_json(&formatted)
            );
            js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
        }
        Err(e) => {
            let json_str = format!(
                "{{\"formatted\":null,\"error\":\"{}\"}}",
                escape_json(&format!("{}", e.inner()))
            );
            js_sys::JSON::parse(&json_str).unwrap_or(JsValue::NULL)
        }
    }
}

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}
