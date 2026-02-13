use std::io::BufRead;
use std::io::Read as _;
use std::io::Write as _;
use std::rc::Rc;

use sema_core::{Env, SemaError, Value};

use crate::list::{call_function, sema_eval_value};
use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "display", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            match arg {
                Value::String(s) => print!("{s}"),
                other => print!("{other}"),
            }
        }
        Ok(Value::Nil)
    });

    register_fn(env, "print", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{arg}");
        }
        Ok(Value::Nil)
    });

    register_fn(env, "println", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            match arg {
                Value::String(s) => print!("{s}"),
                other => print!("{other}"),
            }
        }
        println!();
        Ok(Value::Nil)
    });

    register_fn(env, "newline", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("newline", "0", args.len()));
        }
        println!();
        Ok(Value::Nil)
    });

    register_fn(env, "file/read", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/read", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = std::fs::read_to_string(path)
            .map_err(|e| SemaError::Io(format!("file/read {path}: {e}")))?;
        Ok(Value::String(Rc::new(content)))
    });

    register_fn(env, "file/write", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("file/write", "2", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        std::fs::write(path, content)
            .map_err(|e| SemaError::Io(format!("file/write {path}: {e}")))?;
        Ok(Value::Nil)
    });

    register_fn(env, "file/exists?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/exists?", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::Bool(std::path::Path::new(path).exists()))
    });

    register_fn(env, "read-line", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("read-line", "0", args.len()));
        }
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
        Ok(Value::String(Rc::new(input)))
    });

    register_fn(env, "read", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("read", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        sema_reader::read(s)
    });

    register_fn(env, "read-many", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("read-many", "1", args.len()));
        }
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
        let msg = match &args[0] {
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };
        Err(SemaError::eval(msg))
    });

    // --- File operations ---

    register_fn(env, "file/append", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("file/append", "2", args.len()));
        }
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
        Ok(Value::Nil)
    });

    register_fn(env, "file/delete", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/delete", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        std::fs::remove_file(path)
            .map_err(|e| SemaError::Io(format!("file/delete {path}: {e}")))?;
        Ok(Value::Nil)
    });

    register_fn(env, "file/rename", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("file/rename", "2", args.len()));
        }
        let from = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let to = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        std::fs::rename(from, to)
            .map_err(|e| SemaError::Io(format!("file/rename {from} -> {to}: {e}")))?;
        Ok(Value::Nil)
    });

    register_fn(env, "file/list", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/list", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let entries: Vec<Value> = std::fs::read_dir(path)
            .map_err(|e| SemaError::Io(format!("file/list {path}: {e}")))?
            .filter_map(|entry| {
                entry
                    .ok()
                    .map(|e| Value::string(&e.file_name().to_string_lossy()))
            })
            .collect();
        Ok(Value::list(entries))
    });

    register_fn(env, "file/mkdir", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/mkdir", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        std::fs::create_dir_all(path)
            .map_err(|e| SemaError::Io(format!("file/mkdir {path}: {e}")))?;
        Ok(Value::Nil)
    });

    register_fn(env, "file/is-directory?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/is-directory?", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::Bool(std::path::Path::new(path).is_dir()))
    });

    register_fn(env, "file/is-file?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/is-file?", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::Bool(std::path::Path::new(path).is_file()))
    });

    register_fn(env, "file/is-symlink?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/is-symlink?", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::Bool(std::path::Path::new(path).is_symlink()))
    });

    register_fn(env, "file/info", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/info", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let meta =
            std::fs::metadata(path).map_err(|e| SemaError::Io(format!("file/info {path}: {e}")))?;
        let mut map = std::collections::BTreeMap::new();
        map.insert(Value::keyword("size"), Value::Int(meta.len() as i64));
        map.insert(Value::keyword("is-dir"), Value::Bool(meta.is_dir()));
        map.insert(Value::keyword("is-file"), Value::Bool(meta.is_file()));
        if let Ok(modified) = meta.modified() {
            if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                map.insert(
                    Value::keyword("modified"),
                    Value::Int(duration.as_millis() as i64),
                );
            }
        }
        Ok(Value::Map(Rc::new(map)))
    });

    // --- Path operations ---

    register_fn(env, "path/join", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("path/join", "1+", 0));
        }
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
        if args.len() != 1 {
            return Err(SemaError::arity("path/dirname", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::path::Path::new(s).parent() {
            Some(p) => Ok(Value::string(&p.to_string_lossy())),
            None => Ok(Value::Nil),
        }
    });

    register_fn(env, "path/basename", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("path/basename", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::path::Path::new(s).file_name() {
            Some(name) => Ok(Value::string(&name.to_string_lossy())),
            None => Ok(Value::Nil),
        }
    });

    register_fn(env, "path/extension", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("path/extension", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::path::Path::new(s).extension() {
            Some(ext) => Ok(Value::string(&ext.to_string_lossy())),
            None => Ok(Value::Nil),
        }
    });

    register_fn(env, "path/absolute", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("path/absolute", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let abs = std::fs::canonicalize(s)
            .map_err(|e| SemaError::Io(format!("path/absolute {s}: {e}")))?;
        Ok(Value::string(&abs.to_string_lossy()))
    });

    register_fn(env, "file/read-lines", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file/read-lines", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = std::fs::read_to_string(path)
            .map_err(|e| SemaError::Io(format!("file/read-lines {path}: {e}")))?;
        let lines: Vec<Value> = content.split('\n').map(Value::string).collect();
        Ok(Value::list(lines))
    });

    register_fn(env, "file/for-each-line", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("file/for-each-line", "2", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let func = args[1].clone();
        let file = std::fs::File::open(path)
            .map_err(|e| SemaError::Io(format!("file/for-each-line {path}: {e}")))?;
        let mut reader = std::io::BufReader::new(file);

        // Fast path: reuse env for Lambda calls
        if let Value::Lambda(ref lambda) = func {
            if lambda.params.len() == 1 && lambda.rest_param.is_none() {
                let lambda_env = Env::with_parent(Rc::new(lambda.env.clone()));
                let param0_spur = sema_core::intern(&lambda.params[0]);
                lambda_env.set(param0_spur, Value::Nil);
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
                    lambda_env.update(param0_spur, Value::String(Rc::new(line_buf.clone())));
                    for expr in &lambda.body {
                        sema_eval_value(expr, &lambda_env)?;
                    }
                }
                return Ok(Value::Nil);
            }
        }

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
            call_function(&func, &[Value::String(Rc::new(line_buf.clone()))])?;
        }
        Ok(Value::Nil)
    });

    register_fn(env, "file/fold-lines", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("file/fold-lines", "3", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let func = args[1].clone();
        let mut acc = args[2].clone();
        let file = std::fs::File::open(path)
            .map_err(|e| SemaError::Io(format!("file/fold-lines {path}: {e}")))?;
        let mut reader = std::io::BufReader::with_capacity(256 * 1024, file);

        // Fast path: reuse env for Lambda calls, move acc instead of cloning
        if let Value::Lambda(ref lambda) = func {
            if lambda.params.len() == 2 && lambda.rest_param.is_none() {
                let lambda_env = Env::with_parent(Rc::new(lambda.env.clone()));
                let param0_spur = sema_core::intern(&lambda.params[0]);
                let param1_spur = sema_core::intern(&lambda.params[1]);
                // Pre-populate keys so update() can find them
                lambda_env.set(param0_spur, Value::Nil);
                lambda_env.set(param1_spur, Value::Nil);
                let mut line_buf = String::with_capacity(64);
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
                    lambda_env.update(param0_spur, acc);
                    lambda_env.update(param1_spur, Value::String(Rc::new(line_buf.clone())));
                    let mut result = Value::Nil;
                    for expr in &lambda.body {
                        result = sema_eval_value(expr, &lambda_env)?;
                    }
                    acc = result;
                }
                return Ok(acc);
            }
        }

        let mut line_buf = String::with_capacity(64);
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
            acc = call_function(&func, &[acc, Value::String(Rc::new(line_buf.clone()))])?;
        }
        Ok(acc)
    });

    register_fn(env, "file/write-lines", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("file/write-lines", "2", args.len()));
        }
        let path = args[0]
            .as_str()
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
        std::fs::write(path, content)
            .map_err(|e| SemaError::Io(format!("file/write-lines {path}: {e}")))?;
        Ok(Value::Nil)
    });

    register_fn(env, "file/copy", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("file/copy", "2", args.len()));
        }
        let src = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let dest = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        std::fs::copy(src, dest)
            .map_err(|e| SemaError::Io(format!("file/copy {src} -> {dest}: {e}")))?;
        Ok(Value::Nil)
    });

    register_fn(env, "print-error", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                eprint!(" ");
            }
            match arg {
                Value::String(s) => eprint!("{s}"),
                other => eprint!("{other}"),
            }
        }
        std::io::stderr().flush().ok();
        Ok(Value::Nil)
    });

    register_fn(env, "println-error", |args| {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                eprint!(" ");
            }
            match arg {
                Value::String(s) => eprint!("{s}"),
                other => eprint!("{other}"),
            }
        }
        eprintln!();
        Ok(Value::Nil)
    });

    register_fn(env, "read-stdin", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("read-stdin", "0", args.len()));
        }
        let mut buf = String::new();
        std::io::stdin()
            .read_to_string(&mut buf)
            .map_err(|e| SemaError::Io(format!("read-stdin: {e}")))?;
        Ok(Value::String(Rc::new(buf)))
    });

    register_fn(env, "load", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("load", "1", args.len()));
        }
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
