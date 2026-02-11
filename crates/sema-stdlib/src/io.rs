use std::rc::Rc;

use sema_core::{SemaError, Value};

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

    register_fn(env, "read-file", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("read-file", "1", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = std::fs::read_to_string(path)
            .map_err(|e| SemaError::Io(format!("read-file {path}: {e}")))?;
        Ok(Value::String(Rc::new(content)))
    });

    register_fn(env, "write-file", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("write-file", "2", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let content = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        std::fs::write(path, content)
            .map_err(|e| SemaError::Io(format!("write-file {path}: {e}")))?;
        Ok(Value::Nil)
    });

    register_fn(env, "file-exists?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("file-exists?", "1", args.len()));
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
