use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use sema_core::{Env, NativeFn, SemaError, Value};
use wasm_bindgen::prelude::*;
use js_sys::Date;

thread_local! {
    /// Completed lines of output (flushed by println/newline)
    static OUTPUT: RefCell<Vec<String>> = RefCell::new(Vec::new());
    /// Current line being built by display/print (not yet flushed)
    static LINE_BUF: RefCell<String> = RefCell::new(String::new());
    /// Start time for sys/elapsed (milliseconds since epoch)
    static WASM_START_MS: f64 = Date::now();
    /// In-memory virtual filesystem for WASM
    static VFS: RefCell<BTreeMap<String, String>> = RefCell::new(BTreeMap::new());
    /// Virtual directories (tracked for file/mkdir, file/is-directory?)
    static VFS_DIRS: RefCell<BTreeSet<String>> = RefCell::new({
        let mut s = BTreeSet::new();
        s.insert("/".to_string());
        s
    });
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

/// Register print/println/display/newline that write to the output buffer instead of stdout
fn register_wasm_io(env: &Env) {
    let register = |name: &str, f: Box<dyn Fn(&[Value]) -> Result<Value, SemaError>>| {
        env.set(
            sema_core::intern(name),
            Value::NativeFn(Rc::new(NativeFn {
                name: name.to_string(),
                func: f,
            })),
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
                match arg {
                    Value::String(s) => out.push_str(s),
                    other => out.push_str(&format!("{other}")),
                }
            }
            append_output(&out);
            Ok(Value::Nil)
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
            Ok(Value::Nil)
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
                match arg {
                    Value::String(s) => out.push_str(s),
                    other => out.push_str(&format!("{other}")),
                }
            }
            append_output(&out);
            flush_line();
            Ok(Value::Nil)
        }),
    );

    // newline: flush current line
    register(
        "newline",
        Box::new(|_args: &[Value]| {
            flush_line();
            Ok(Value::Nil)
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
            Ok(Value::Nil)
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
            Ok(Value::Nil)
        }),
    );

    // time-ms: use Date.now() from the web platform
    register(
        "time-ms",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("time-ms", "0", args.len()));
            }
            Ok(Value::Int(Date::now() as i64))
        }),
    );

    // term/* pass-through shims (ANSI codes are useless in the browser)
    for name in &[
        "term/bold", "term/dim", "term/italic", "term/underline", "term/inverse", "term/strikethrough",
        "term/black", "term/red", "term/green", "term/yellow", "term/blue", "term/magenta", "term/cyan", "term/white", "term/gray",
        "term/strip",
    ] {
        let fn_name = name.to_string();
        register(name, Box::new(move |args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity(&fn_name, "1", args.len()));
            }
            let text = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::String(Rc::new(text.to_string())))
        }));
    }

    // term/style: return first arg unchanged
    register(
        "term/style",
        Box::new(|args: &[Value]| {
            if args.is_empty() {
                return Err(SemaError::arity("term/style", "1+", args.len()));
            }
            let text = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::String(Rc::new(text.to_string())))
        }),
    );

    // term/rgb: return first arg unchanged
    register(
        "term/rgb",
        Box::new(|args: &[Value]| {
            if args.len() != 4 {
                return Err(SemaError::arity("term/rgb", "4", args.len()));
            }
            let text = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::String(Rc::new(text.to_string())))
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
            let nanos = WASM_START_MS.with(|&start| {
                ((Date::now() - start) * 1_000_000.0) as i64
            });
            Ok(Value::Int(nanos))
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
            Ok(Value::Nil)
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
            Ok(Value::Nil)
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
            Ok(Value::Nil)
        }),
    );

    // sys/env-all: empty map in WASM
    register(
        "sys/env-all",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/env-all", "0", args.len()));
            }
            Ok(Value::Map(Rc::new(std::collections::BTreeMap::new())))
        }),
    );

    // exit: not supported in WASM
    register(
        "exit",
        Box::new(|_args: &[Value]| {
            Err(SemaError::eval("exit not supported in WASM"))
        }),
    );

    // sys/interactive?: always false in WASM
    register(
        "sys/interactive?",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/interactive?", "0", args.len()));
            }
            Ok(Value::Bool(false))
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
                None => Ok(Value::Nil),
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
                Some(0) | None => Ok(Value::Nil),
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
                    Some(s) => Ok(Value::String(Rc::new(s))),
                    None => Ok(Value::Nil),
                },
                Err(_) => Ok(Value::Nil),
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
                            Err(_) => Ok(Value::Nil),
                        }
                    }
                    None => Ok(Value::Nil),
                },
                Err(_) => Ok(Value::Nil),
            }
        }),
    );

    // --- HTTP stubs (async fetch not available in synchronous WASM) ---

    register(
        "http/get",
        Box::new(|args: &[Value]| {
            if args.is_empty() || args.len() > 2 {
                return Err(SemaError::arity("http/get", "1 or 2", args.len()));
            }
            Err(SemaError::eval("http/get is not yet supported in WASM. HTTP requires async evaluation which will be available via eval_async in a future release."))
        }),
    );

    register(
        "http/post",
        Box::new(|args: &[Value]| {
            if args.len() < 2 || args.len() > 3 {
                return Err(SemaError::arity("http/post", "2 or 3", args.len()));
            }
            Err(SemaError::eval("http/post is not yet supported in WASM. HTTP requires async evaluation which will be available via eval_async in a future release."))
        }),
    );

    register(
        "http/put",
        Box::new(|args: &[Value]| {
            if args.len() < 2 || args.len() > 3 {
                return Err(SemaError::arity("http/put", "2 or 3", args.len()));
            }
            Err(SemaError::eval("http/put is not yet supported in WASM. HTTP requires async evaluation which will be available via eval_async in a future release."))
        }),
    );

    register(
        "http/delete",
        Box::new(|args: &[Value]| {
            if args.is_empty() || args.len() > 2 {
                return Err(SemaError::arity("http/delete", "1 or 2", args.len()));
            }
            Err(SemaError::eval("http/delete is not yet supported in WASM. HTTP requires async evaluation which will be available via eval_async in a future release."))
        }),
    );

    register(
        "http/request",
        Box::new(|args: &[Value]| {
            if args.len() < 2 || args.len() > 4 {
                return Err(SemaError::arity("http/request", "2-4", args.len()));
            }
            Err(SemaError::eval("http/request is not yet supported in WASM. HTTP requires async evaluation which will be available via eval_async in a future release."))
        }),
    );

    // --- sys/* stubs for unsupported system functions ---

    register(
        "sys/args",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/args", "0", args.len()));
            }
            Ok(Value::List(Rc::new(Vec::new())))
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
            Ok(Value::Nil)
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
            Ok(Value::Nil)
        }),
    );

    register(
        "sys/user",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/user", "0", args.len()));
            }
            Ok(Value::Nil)
        }),
    );

    register(
        "sys/pid",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/pid", "0", args.len()));
            }
            Ok(Value::Int(0))
        }),
    );

    register(
        "sys/which",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("sys/which", "1", args.len()));
            }
            args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::Nil)
        }),
    );

    register(
        "sys/tty",
        Box::new(|args: &[Value]| {
            if !args.is_empty() {
                return Err(SemaError::arity("sys/tty", "0", args.len()));
            }
            Ok(Value::Nil)
        }),
    );

    // --- VFS file operation shims ---

    register(
        "file/read",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/read", "1", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            VFS.with(|vfs| {
                match vfs.borrow().get(path) {
                    Some(content) => Ok(Value::String(Rc::new(content.clone()))),
                    None => Err(SemaError::Io(format!("file/read {path}: No such file"))),
                }
            })
        }),
    );

    register(
        "file/write",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/write", "2", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let content = args[1].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            VFS.with(|vfs| {
                vfs.borrow_mut().insert(path.to_string(), content.to_string());
            });
            Ok(Value::Nil)
        }),
    );

    register(
        "file/exists?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/exists?", "1", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let in_vfs = VFS.with(|vfs| vfs.borrow().contains_key(path));
            let in_dirs = VFS_DIRS.with(|dirs| dirs.borrow().contains(path));
            Ok(Value::Bool(in_vfs || in_dirs))
        }),
    );

    register(
        "file/delete",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/delete", "1", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            VFS.with(|vfs| {
                match vfs.borrow_mut().remove(path) {
                    Some(_) => Ok(Value::Nil),
                    None => Err(SemaError::Io(format!("file/delete {path}: No such file"))),
                }
            })
        }),
    );

    register(
        "file/rename",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/rename", "2", args.len()));
            }
            let from = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let to = args[1].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            VFS.with(|vfs| {
                let mut map = vfs.borrow_mut();
                match map.remove(from) {
                    Some(content) => {
                        map.insert(to.to_string(), content);
                        Ok(Value::Nil)
                    }
                    None => Err(SemaError::Io(format!("file/rename {from} -> {to}: No such file"))),
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
            let dir = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let prefix = if dir.ends_with('/') {
                dir.to_string()
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
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            VFS_DIRS.with(|dirs| {
                let mut set = dirs.borrow_mut();
                let mut current = String::new();
                for part in path.split('/') {
                    if part.is_empty() {
                        if current.is_empty() {
                            current.push('/');
                        }
                        continue;
                    }
                    if !current.ends_with('/') {
                        current.push('/');
                    }
                    current.push_str(part);
                    set.insert(current.clone());
                }
            });
            Ok(Value::Nil)
        }),
    );

    register(
        "file/is-directory?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/is-directory?", "1", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::Bool(VFS_DIRS.with(|dirs| dirs.borrow().contains(path))))
        }),
    );

    register(
        "file/is-file?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/is-file?", "1", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::Bool(VFS.with(|vfs| vfs.borrow().contains_key(path))))
        }),
    );

    register(
        "file/is-symlink?",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/is-symlink?", "1", args.len()));
            }
            let _path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::Bool(false))
        }),
    );

    register(
        "file/append",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/append", "2", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let content = args[1].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            VFS.with(|vfs| {
                let mut map = vfs.borrow_mut();
                map.entry(path.to_string())
                    .and_modify(|existing| existing.push_str(content))
                    .or_insert_with(|| content.to_string());
            });
            Ok(Value::Nil)
        }),
    );

    register(
        "file/copy",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/copy", "2", args.len()));
            }
            let src = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let dest = args[1].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            VFS.with(|vfs| {
                let map = vfs.borrow();
                match map.get(src) {
                    Some(content) => {
                        let content = content.clone();
                        drop(map);
                        vfs.borrow_mut().insert(dest.to_string(), content);
                        Ok(Value::Nil)
                    }
                    None => Err(SemaError::Io(format!("file/copy {src} -> {dest}: No such file"))),
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
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            VFS.with(|vfs| {
                match vfs.borrow().get(path) {
                    Some(content) => {
                        let lines: Vec<Value> = content.split('\n').map(Value::string).collect();
                        Ok(Value::list(lines))
                    }
                    None => Err(SemaError::Io(format!("file/read-lines {path}: No such file"))),
                }
            })
        }),
    );

    register(
        "file/write-lines",
        Box::new(|args: &[Value]| {
            if args.len() != 2 {
                return Err(SemaError::arity("file/write-lines", "2", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let lines = match &args[1] {
                Value::List(l) => l.as_ref(),
                Value::Vector(v) => v.as_ref(),
                _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
            };
            let strs: Vec<String> = lines
                .iter()
                .map(|v| match v {
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                })
                .collect();
            let content = strs.join("\n");
            VFS.with(|vfs| {
                vfs.borrow_mut().insert(path.to_string(), content);
            });
            Ok(Value::Nil)
        }),
    );

    register(
        "file/info",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("file/info", "1", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let is_file = VFS.with(|vfs| vfs.borrow().contains_key(path));
            let is_dir = VFS_DIRS.with(|dirs| dirs.borrow().contains(path));
            if !is_file && !is_dir {
                return Err(SemaError::Io(format!("file/info {path}: No such file or directory")));
            }
            let size = if is_file {
                VFS.with(|vfs| vfs.borrow().get(path).map(|c| c.len() as i64).unwrap_or(0))
            } else {
                0
            };
            let mut map = BTreeMap::new();
            map.insert(Value::keyword("size"), Value::Int(size));
            map.insert(Value::keyword("is-dir"), Value::Bool(is_dir));
            map.insert(Value::keyword("is-file"), Value::Bool(is_file));
            Ok(Value::Map(Rc::new(map)))
        }),
    );

    // --- IO shims unsupported in WASM ---

    register(
        "read-line",
        Box::new(|_args: &[Value]| {
            Err(SemaError::eval("read-line not supported in WASM"))
        }),
    );

    register(
        "read-stdin",
        Box::new(|_args: &[Value]| {
            Err(SemaError::eval("read-stdin not supported in WASM"))
        }),
    );

    register(
        "shell",
        Box::new(|_args: &[Value]| {
            Err(SemaError::eval("shell not supported in WASM"))
        }),
    );

    // --- Reader/parser functions ---

    register(
        "load",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("load", "1", args.len()));
            }
            let path = args[0].as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            VFS.with(|vfs| {
                match vfs.borrow().get(path) {
                    Some(content) => {
                        let exprs = sema_reader::read_many(content)?;
                        Ok(Value::list(exprs))
                    }
                    None => Err(SemaError::Io(format!("load {path}: No such file"))),
                }
            })
        }),
    );

    register(
        "read",
        Box::new(|args: &[Value]| {
            if args.len() != 1 {
                return Err(SemaError::arity("read", "1", args.len()));
            }
            let s = args[0].as_str()
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
            let s = args[0].as_str()
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
            let msg = match &args[0] {
                Value::String(s) => s.to_string(),
                other => other.to_string(),
            };
            Err(SemaError::eval(msg))
        }),
    );
}

#[wasm_bindgen]
pub struct WasmInterpreter {
    inner: sema_eval::Interpreter,
}

#[wasm_bindgen]
impl WasmInterpreter {
    #[wasm_bindgen(constructor)]
    pub fn new() -> WasmInterpreter {
        let interp = sema_eval::Interpreter::new();

        // Override print/println/display with our buffer-based versions
        register_wasm_io(&interp.global_env);

        WasmInterpreter { inner: interp }
    }

    /// Evaluate code, returns JSON: {"value": "...", "output": ["...", ...], "error": null}
    /// or {"value": null, "output": [...], "error": "..."}
    pub fn eval(&self, code: &str) -> String {
        OUTPUT.with(|o| o.borrow_mut().clear());
        LINE_BUF.with(|b| b.borrow_mut().clear());

        let env = sema_core::Env::with_parent(self.inner.global_env.clone());
        match sema_eval::eval_string(code, &env) {
            Ok(val) => {
                let output = take_output();
                let val_str = if matches!(val, Value::Nil) {
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
        }
    }

    /// Evaluate in the global env so defines persist
    pub fn eval_global(&self, code: &str) -> String {
        OUTPUT.with(|o| o.borrow_mut().clear());
        LINE_BUF.with(|b| b.borrow_mut().clear());

        match sema_eval::eval_string(code, &self.inner.global_env) {
            Ok(val) => {
                let output = take_output();
                let val_str = if matches!(val, Value::Nil) {
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
        }
    }

    /// Get the Sema version
    pub fn version(&self) -> String {
        env!("CARGO_PKG_VERSION").to_string()
    }
}

fn json_to_value(json: &serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(b) => Value::Bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Nil
            }
        }
        serde_json::Value::String(s) => Value::string(s),
        serde_json::Value::Array(arr) => Value::list(arr.iter().map(json_to_value).collect()),
        serde_json::Value::Object(obj) => {
            let mut map = BTreeMap::new();
            for (k, v) in obj {
                map.insert(Value::keyword(k), json_to_value(v));
            }
            Value::Map(Rc::new(map))
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
