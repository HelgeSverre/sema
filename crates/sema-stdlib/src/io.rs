use std::io::BufRead;
use std::io::Read as _;
use std::io::Write as _;

use sema_core::{check_arity, Caps, EvalContext, NativeFn, SemaError, Value, ValueView};

use crate::register_fn;

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    register_fn(env, "display", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            match arg.as_str() {
                Some(s) => print!("{s}"),
                None => print!("{arg}"),
            }
        }
        Ok(Value::nil())
    });

    register_fn(env, "print", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{arg}");
        }
        Ok(Value::nil())
    });

    register_fn(env, "println", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            match arg.as_str() {
                Some(s) => print!("{s}"),
                None => print!("{arg}"),
            }
        }
        println!();
        Ok(Value::nil())
    });

    register_fn(env, "pprint", |args| {
        check_arity!(args, "pprint", 1);
        println!("{}", sema_core::pretty_print(&args[0], 80));
        Ok(Value::nil())
    });

    register_fn(env, "newline", |args| {
        check_arity!(args, "newline", 0);
        println!();
        Ok(Value::nil())
    });

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "file/read", &[0], |args| {
        check_arity!(args, "file/read", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        if let Some(data) = sema_core::vfs::vfs_read(path) {
            return String::from_utf8(data)
                .map(|s| Value::string(&s))
                .map_err(|e| {
                    SemaError::Io(format!("file/read {path}: invalid UTF-8 in VFS: {e}"))
                });
        }
        let content = std::fs::read_to_string(path)
            .map_err(|e| SemaError::Io(format!("file/read {path}: {e}")))?;
        Ok(Value::string(&content))
    });

    crate::register_fn_path_gated(env, sandbox, Caps::FS_WRITE, "file/write", &[0], |args| {
        check_arity!(args, "file/write", 2);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        std::fs::write(path, content)
            .map_err(|e| SemaError::Io(format!("file/write {path}: {e}")))?;
        Ok(Value::nil())
    });

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "file/read-bytes",
        &[0],
        |args| {
            check_arity!(args, "file/read-bytes", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            if let Some(data) = sema_core::vfs::vfs_read(path) {
                return Ok(Value::bytevector(data));
            }
            let bytes = std::fs::read(path)
                .map_err(|e| SemaError::Io(format!("file/read-bytes {path}: {e}")))?;
            Ok(Value::bytevector(bytes))
        },
    );

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_WRITE,
        "file/write-bytes",
        &[0],
        |args| {
            check_arity!(args, "file/write-bytes", 2);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let bv = args[1]
                .as_bytevector()
                .ok_or_else(|| SemaError::type_error("bytevector", args[1].type_name()))?;
            std::fs::write(path, bv)
                .map_err(|e| SemaError::Io(format!("file/write-bytes {path}: {e}")))?;
            Ok(Value::nil())
        },
    );

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "file/exists?", &[0], |args| {
        check_arity!(args, "file/exists?", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        if let Some(exists) = sema_core::vfs::vfs_exists(path) {
            if exists {
                return Ok(Value::bool(true));
            }
        }
        Ok(Value::bool(std::path::Path::new(path).exists()))
    });

    register_fn(env, "read-line", |args| {
        check_arity!(args, "read-line", 0);
        let mut input = String::new();
        std::io::stdin()
            .read_line(&mut input)
            .map_err(|e| SemaError::Io(format!("read-line: {e}")))?;
        // Remove trailing newline
        if input.ends_with('\n') {
            input.pop();
            if input.ends_with('\r') {
                input.pop();
            }
        }
        Ok(Value::string(&input))
    });

    register_fn(env, "read", |args| {
        check_arity!(args, "read", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        sema_reader::read(s)
    });

    register_fn(env, "read-many", |args| {
        check_arity!(args, "read-many", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let exprs = sema_reader::read_many(s)?;
        Ok(Value::list(exprs))
    });

    register_fn(env, "error", |args| {
        if args.is_empty() {
            return Err(SemaError::eval("error called with no message"));
        }
        let msg = match args[0].as_str() {
            Some(s) => s.to_string(),
            None => args[0].to_string(),
        };
        Err(SemaError::eval(msg))
    });

    crate::register_fn_path_gated(env, sandbox, Caps::FS_WRITE, "file/append", &[0], |args| {
        check_arity!(args, "file/append", 2);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .map_err(|e| SemaError::Io(format!("file/append {path}: {e}")))?;
        file.write_all(content.as_bytes())
            .map_err(|e| SemaError::Io(format!("file/append {path}: {e}")))?;
        Ok(Value::nil())
    });

    crate::register_fn_path_gated(env, sandbox, Caps::FS_WRITE, "file/delete", &[0], |args| {
        check_arity!(args, "file/delete", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        std::fs::remove_file(path)
            .map_err(|e| SemaError::Io(format!("file/delete {path}: {e}")))?;
        Ok(Value::nil())
    });

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_WRITE,
        "file/rename",
        &[0, 1],
        |args| {
            check_arity!(args, "file/rename", 2);
            let from = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let to = args[1]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
            std::fs::rename(from, to)
                .map_err(|e| SemaError::Io(format!("file/rename {from} -> {to}: {e}")))?;
            Ok(Value::nil())
        },
    );

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "file/list", &[0], |args| {
        check_arity!(args, "file/list", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut entries = Vec::new();
        for entry in
            std::fs::read_dir(path).map_err(|e| SemaError::Io(format!("file/list {path}: {e}")))?
        {
            let entry = entry.map_err(|e| SemaError::Io(format!("file/list {path}: {e}")))?;
            entries.push(Value::string(&entry.file_name().to_string_lossy()));
        }
        Ok(Value::list(entries))
    });

    crate::register_fn_path_gated(env, sandbox, Caps::FS_WRITE, "file/mkdir", &[0], |args| {
        check_arity!(args, "file/mkdir", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        std::fs::create_dir_all(path)
            .map_err(|e| SemaError::Io(format!("file/mkdir {path}: {e}")))?;
        Ok(Value::nil())
    });

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "file/is-directory?",
        &[0],
        |args| {
            check_arity!(args, "file/is-directory?", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::bool(std::path::Path::new(path).is_dir()))
        },
    );

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "file/is-file?", &[0], |args| {
        check_arity!(args, "file/is-file?", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::bool(std::path::Path::new(path).is_file()))
    });

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "file/is-symlink?",
        &[0],
        |args| {
            check_arity!(args, "file/is-symlink?", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            Ok(Value::bool(std::path::Path::new(path).is_symlink()))
        },
    );

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "file/info", &[0], |args| {
        check_arity!(args, "file/info", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let meta =
            std::fs::metadata(path).map_err(|e| SemaError::Io(format!("file/info {path}: {e}")))?;
        let mut map = std::collections::BTreeMap::new();
        map.insert(Value::keyword("size"), Value::int(meta.len() as i64));
        map.insert(Value::keyword("is-dir"), Value::bool(meta.is_dir()));
        map.insert(Value::keyword("is-file"), Value::bool(meta.is_file()));
        if let Ok(modified) = meta.modified() {
            if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                map.insert(
                    Value::keyword("modified"),
                    Value::int(duration.as_millis() as i64),
                );
            }
        }
        Ok(Value::map(map))
    });

    register_fn(env, "path/join", |args| {
        check_arity!(args, "path/join", 1..);
        let mut path = std::path::PathBuf::new();
        for arg in args {
            let s = arg
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", arg.type_name()))?;
            path.push(s);
        }
        Ok(Value::string(&path.to_string_lossy()))
    });

    register_fn(env, "path/dirname", |args| {
        check_arity!(args, "path/dirname", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::path::Path::new(s).parent() {
            Some(p) => Ok(Value::string(&p.to_string_lossy())),
            None => Ok(Value::nil()),
        }
    });

    register_fn(env, "path/basename", |args| {
        check_arity!(args, "path/basename", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::path::Path::new(s).file_name() {
            Some(name) => Ok(Value::string(&name.to_string_lossy())),
            None => Ok(Value::nil()),
        }
    });

    register_fn(env, "path/extension", |args| {
        check_arity!(args, "path/extension", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::path::Path::new(s).extension() {
            Some(ext) => Ok(Value::string(&ext.to_string_lossy())),
            None => Ok(Value::nil()),
        }
    });

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "path/absolute", &[0], |args| {
        check_arity!(args, "path/absolute", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let abs = std::fs::canonicalize(s)
            .map_err(|e| SemaError::Io(format!("path/absolute {s}: {e}")))?;
        Ok(Value::string(&abs.to_string_lossy()))
    });

    crate::register_fn_gated(env, sandbox, Caps::FS_READ, "file/glob", |args| {
        check_arity!(args, "file/glob", 1);
        let pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let paths = glob::glob(pattern)
            .map_err(|e| SemaError::eval(format!("file/glob: invalid pattern: {e}")))?;
        let mut items = Vec::new();
        for entry in paths {
            let path = entry.map_err(|e| SemaError::Io(format!("file/glob: {e}")))?;
            items.push(Value::string(path.to_str().unwrap_or("")));
        }
        Ok(Value::list(items))
    });

    register_fn(env, "path/ext", |args| {
        check_arity!(args, "path/ext", 1);
        let p = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let ext = std::path::Path::new(p)
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("");
        Ok(Value::string(ext))
    });

    register_fn(env, "path/stem", |args| {
        check_arity!(args, "path/stem", 1);
        let p = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let stem = std::path::Path::new(p)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("");
        Ok(Value::string(stem))
    });

    register_fn(env, "path/dir", |args| {
        check_arity!(args, "path/dir", 1);
        let p = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let dir = std::path::Path::new(p)
            .parent()
            .and_then(|d| d.to_str())
            .unwrap_or("");
        Ok(Value::string(dir))
    });

    register_fn(env, "path/filename", |args| {
        check_arity!(args, "path/filename", 1);
        let p = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let name = std::path::Path::new(p)
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");
        Ok(Value::string(name))
    });

    register_fn(env, "path/absolute?", |args| {
        check_arity!(args, "path/absolute?", 1);
        let p = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::bool(std::path::Path::new(p).is_absolute()))
    });

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "file/read-lines",
        &[0],
        |args| {
            check_arity!(args, "file/read-lines", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let content = if let Some(data) = sema_core::vfs::vfs_read(path) {
                String::from_utf8(data).map_err(|e| {
                    SemaError::Io(format!("file/read-lines {path}: invalid UTF-8 in VFS: {e}"))
                })?
            } else {
                std::fs::read_to_string(path)
                    .map_err(|e| SemaError::Io(format!("file/read-lines {path}: {e}")))?
            };
            let lines: Vec<Value> = content.split('\n').map(Value::string).collect();
            Ok(Value::list(lines))
        },
    );

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "file/for-each-line",
        &[0],
        |args| {
            check_arity!(args, "file/for-each-line", 2);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let func = args[1].clone();
            let file = std::fs::File::open(path)
                .map_err(|e| SemaError::Io(format!("file/for-each-line {path}: {e}")))?;
            let mut reader = std::io::BufReader::new(file);

            sema_core::with_stdlib_ctx(|ctx| {
                let mut line_buf = String::with_capacity(64);
                loop {
                    line_buf.clear();
                    let n = reader
                        .read_line(&mut line_buf)
                        .map_err(|e| SemaError::Io(format!("file/for-each-line {path}: {e}")))?;
                    if n == 0 {
                        break;
                    }
                    if line_buf.ends_with('\n') {
                        line_buf.pop();
                        if line_buf.ends_with('\r') {
                            line_buf.pop();
                        }
                    }
                    sema_core::call_callback(ctx, &func, &[Value::string(&line_buf)])?;
                }
                Ok(Value::nil())
            })
        },
    );

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "file/fold-lines",
        &[0],
        |args| {
            check_arity!(args, "file/fold-lines", 3);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let func = args[1].clone();
            let mut acc = args[2].clone();
            let file = std::fs::File::open(path)
                .map_err(|e| SemaError::Io(format!("file/fold-lines {path}: {e}")))?;
            // 256KB buffer (vs default 8KB) improves throughput for large file reads.
            let mut reader = std::io::BufReader::with_capacity(256 * 1024, file);

            sema_core::with_stdlib_ctx(|ctx| {
                let mut line_buf = String::with_capacity(64);
                // Fast path: if the callback is a NativeFn, call it directly.
                // This avoids the call_callback indirection and, critically, avoids
                // the VM closure fallback wrapper's clone of args (which prevents
                // COW optimizations in functions like assoc).
                #[allow(clippy::type_complexity)]
                let native: Option<
                    &dyn Fn(&EvalContext, &[Value]) -> Result<Value, SemaError>,
                > = func.as_native_fn_ref().map(|n| &*n.func);
                loop {
                    line_buf.clear();
                    let n = reader
                        .read_line(&mut line_buf)
                        .map_err(|e| SemaError::Io(format!("file/fold-lines {path}: {e}")))?;
                    if n == 0 {
                        break;
                    }
                    if line_buf.ends_with('\n') {
                        line_buf.pop();
                        if line_buf.ends_with('\r') {
                            line_buf.pop();
                        }
                    }
                    let line_val = Value::string(&line_buf);
                    let args = [std::mem::replace(&mut acc, Value::nil()), line_val];
                    acc = if let Some(f) = native {
                        f(ctx, &args)?
                    } else {
                        sema_core::call_callback(ctx, &func, &args)?
                    };
                }
                Ok(acc)
            })
        },
    );

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_WRITE,
        "file/write-lines",
        &[0],
        |args| {
            check_arity!(args, "file/write-lines", 2);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let lines = match args[1].view() {
                ValueView::List(l) => l,
                ValueView::Vector(v) => v,
                _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
            };
            let strs: Vec<String> = lines
                .iter()
                .map(|v| match v.as_str() {
                    Some(s) => s.to_string(),
                    None => v.to_string(),
                })
                .collect();
            let content = strs.join("\n");
            std::fs::write(path, content)
                .map_err(|e| SemaError::Io(format!("file/write-lines {path}: {e}")))?;
            Ok(Value::nil())
        },
    );

    crate::register_fn_path_gated(env, sandbox, Caps::FS_WRITE, "file/copy", &[0, 1], |args| {
        check_arity!(args, "file/copy", 2);
        let src = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let dest = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        std::fs::copy(src, dest)
            .map_err(|e| SemaError::Io(format!("file/copy {src} -> {dest}: {e}")))?;
        Ok(Value::nil())
    });

    register_fn(env, "print-error", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                eprint!(" ");
            }
            match arg.as_str() {
                Some(s) => eprint!("{s}"),
                None => eprint!("{arg}"),
            }
        }
        std::io::stderr().flush().ok();
        Ok(Value::nil())
    });

    register_fn(env, "println-error", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                eprint!(" ");
            }
            match arg.as_str() {
                Some(s) => eprint!("{s}"),
                None => eprint!("{arg}"),
            }
        }
        eprintln!();
        Ok(Value::nil())
    });

    register_fn(env, "read-stdin", |args| {
        check_arity!(args, "read-stdin", 0);
        let mut buf = String::new();
        std::io::stdin()
            .read_to_string(&mut buf)
            .map_err(|e| SemaError::Io(format!("read-stdin: {e}")))?;
        Ok(Value::string(&buf))
    });

    register_log_fn(env, "log/info", "INFO");
    register_log_fn(env, "log/warn", "WARN");
    register_log_fn(env, "log/error", "ERROR");
    register_log_fn(env, "log/debug", "DEBUG");

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "load", &[0], |args| {
        check_arity!(args, "load", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = std::fs::read_to_string(path)
            .map_err(|e| SemaError::Io(format!("load {path}: {e}")))?;
        // Parse and return as a list of expressions for the caller to eval
        let exprs = sema_reader::read_many(&content)?;
        Ok(Value::list(exprs))
    });
}

fn register_log_fn(env: &sema_core::Env, name: &str, level: &'static str) {
    let fn_name = name.to_string();
    env.set(
        sema_core::intern(name),
        Value::native_fn(NativeFn::with_ctx(name, move |ctx, args| {
            check_arity!(args, &fn_name, 1..);
            let mut msg = String::new();
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    msg.push(' ');
                }
                match arg.as_str() {
                    Some(s) => msg.push_str(s),
                    None => msg.push_str(&arg.to_string()),
                }
            }
            let context = ctx.context_all();
            if context.is_empty() {
                eprintln!("[{level}] {msg}");
            } else {
                eprintln!("[{level}] {msg} {}", Value::map(context));
            }
            Ok(Value::nil())
        })),
    );
}
