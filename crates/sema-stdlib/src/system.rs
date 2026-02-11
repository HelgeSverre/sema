use std::rc::Rc;

use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "env", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("env", "1", args.len()));
        }
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::env::var(name) {
            Ok(val) => Ok(Value::String(Rc::new(val))),
            Err(_) => Ok(Value::Nil),
        }
    });

    register_fn(env, "shell", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("shell", "1+", 0));
        }
        let cmd = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let cmd_args: Vec<&str> = args[1..]
            .iter()
            .map(|a| {
                a.as_str()
                    .ok_or_else(|| SemaError::type_error("string", a.type_name()))
            })
            .collect::<Result<_, _>>()?;

        let output = std::process::Command::new(cmd)
            .args(&cmd_args)
            .output()
            .map_err(|e| SemaError::Io(format!("shell: {e}")))?;

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();

        let mut result = std::collections::BTreeMap::new();
        result.insert(Value::keyword("stdout"), Value::String(Rc::new(stdout)));
        result.insert(Value::keyword("stderr"), Value::String(Rc::new(stderr)));
        result.insert(
            Value::keyword("exit-code"),
            Value::Int(output.status.code().unwrap_or(-1) as i64),
        );
        Ok(Value::Map(Rc::new(result)))
    });

    register_fn(env, "exit", |args| {
        let code = if args.is_empty() {
            0
        } else {
            args[0].as_int().unwrap_or(0) as i32
        };
        std::process::exit(code);
    });

    register_fn(env, "time-ms", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("time-ms", "0", args.len()));
        }
        let ms = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as i64;
        Ok(Value::Int(ms))
    });

    register_fn(env, "sleep", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("sleep", "1", args.len()));
        }
        let ms = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
        Ok(Value::Nil)
    });
}
