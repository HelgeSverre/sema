use std::io::IsTerminal;

use sema_core::{Caps, SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    crate::register_fn_gated(env, sandbox, Caps::ENV_READ, "env", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("env", "1", args.len()));
        }
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::env::var(name) {
            Ok(val) => Ok(Value::string(&val)),
            Err(_) => Ok(Value::nil()),
        }
    });

    crate::register_fn_gated(env, sandbox, Caps::SHELL, "shell", |args| {
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

        let output = if cmd_args.is_empty() {
            // Single string: run through the system shell for command parsing
            let shell = if cfg!(windows) { "cmd" } else { "sh" };
            let flag = if cfg!(windows) { "/C" } else { "-c" };
            std::process::Command::new(shell).args([flag, cmd]).output()
        } else {
            // Explicit args: run the command directly
            std::process::Command::new(cmd).args(&cmd_args).output()
        }
        .map_err(|e| SemaError::Io(format!("shell: {e}")))?;

        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();

        let mut result = std::collections::BTreeMap::new();
        result.insert(Value::keyword("stdout"), Value::string(&stdout));
        result.insert(Value::keyword("stderr"), Value::string(&stderr));
        result.insert(
            Value::keyword("exit-code"),
            Value::int(output.status.code().unwrap_or(-1) as i64),
        );
        Ok(Value::map(result))
    });

    crate::register_fn_gated(env, sandbox, Caps::PROCESS, "exit", |args| {
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
        Ok(Value::int(ms))
    });

    register_fn(env, "sleep", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("sleep", "1", args.len()));
        }
        let ms = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
        Ok(Value::nil())
    });

    crate::register_fn_gated(env, sandbox, Caps::PROCESS, "sys/args", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/args", "0", args.len()));
        }
        let args_list: Vec<Value> = std::env::args().map(|a| Value::string(&a)).collect();
        Ok(Value::list(args_list))
    });

    register_fn(env, "sys/cwd", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/cwd", "0", args.len()));
        }
        let cwd = std::env::current_dir().map_err(|e| SemaError::Io(format!("sys/cwd: {e}")))?;
        Ok(Value::string(&cwd.to_string_lossy()))
    });

    register_fn(env, "sys/platform", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/platform", "0", args.len()));
        }
        let platform = if cfg!(target_os = "macos") {
            "macos"
        } else if cfg!(target_os = "linux") {
            "linux"
        } else if cfg!(target_os = "windows") {
            "windows"
        } else {
            "unknown"
        };
        Ok(Value::string(platform))
    });

    crate::register_fn_gated(env, sandbox, Caps::ENV_WRITE, "sys/set-env", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("sys/set-env", "2", args.len()));
        }
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let value = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        unsafe {
            std::env::set_var(name, value);
        }
        Ok(Value::nil())
    });

    crate::register_fn_gated(env, sandbox, Caps::ENV_READ, "sys/env-all", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/env-all", "0", args.len()));
        }
        let mut map = std::collections::BTreeMap::new();
        for (key, val) in std::env::vars() {
            map.insert(Value::keyword(&key), Value::string(&val));
        }
        Ok(Value::map(map))
    });

    register_fn(env, "sys/home-dir", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/home-dir", "0", args.len()));
        }
        match std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")) {
            Ok(home) => Ok(Value::string(&home)),
            Err(_) => Ok(Value::nil()),
        }
    });

    register_fn(env, "sys/temp-dir", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/temp-dir", "0", args.len()));
        }
        Ok(Value::string(&std::env::temp_dir().to_string_lossy()))
    });

    register_fn(env, "sys/hostname", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/hostname", "0", args.len()));
        }
        match hostname::get() {
            Ok(name) => Ok(Value::string(&name.to_string_lossy())),
            Err(_) => Ok(Value::nil()),
        }
    });

    register_fn(env, "sys/user", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/user", "0", args.len()));
        }
        match std::env::var("USER").or_else(|_| std::env::var("USERNAME")) {
            Ok(user) => Ok(Value::string(&user)),
            Err(_) => Ok(Value::nil()),
        }
    });

    register_fn(env, "sys/interactive?", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/interactive?", "0", args.len()));
        }
        Ok(Value::bool(std::io::stdin().is_terminal()))
    });

    register_fn(env, "sys/tty", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/tty", "0", args.len()));
        }
        if !std::io::stdin().is_terminal() {
            return Ok(Value::nil());
        }
        #[cfg(unix)]
        {
            use std::os::unix::io::AsRawFd;
            let fd = std::io::stdin().as_raw_fd();
            unsafe {
                let name = libc::ttyname(fd);
                if name.is_null() {
                    Ok(Value::nil())
                } else {
                    let s = std::ffi::CStr::from_ptr(name).to_string_lossy().to_string();
                    Ok(Value::string(&s))
                }
            }
        }
        #[cfg(not(unix))]
        {
            Ok(Value::nil())
        }
    });

    crate::register_fn_gated(env, sandbox, Caps::PROCESS, "sys/pid", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/pid", "0", args.len()));
        }
        Ok(Value::int(std::process::id() as i64))
    });

    register_fn(env, "sys/arch", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/arch", "0", args.len()));
        }
        Ok(Value::string(std::env::consts::ARCH))
    });

    register_fn(env, "sys/os", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/os", "0", args.len()));
        }
        Ok(Value::string(std::env::consts::OS))
    });

    crate::register_fn_gated(env, sandbox, Caps::PROCESS, "sys/which", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("sys/which", "1", args.len()));
        }
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let path_var = std::env::var("PATH").unwrap_or_default();
        let sep = if cfg!(windows) { ';' } else { ':' };
        for dir in path_var.split(sep) {
            let candidate = std::path::Path::new(dir).join(name);
            if candidate.is_file() {
                return Ok(Value::string(&candidate.to_string_lossy()));
            }
        }
        Ok(Value::nil())
    });

    register_fn(env, "sys/elapsed", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("sys/elapsed", "0", args.len()));
        }
        use std::time::Instant;
        thread_local! {
            static START: Instant = Instant::now();
        }
        let nanos = START.with(|s| s.elapsed().as_nanos()) as i64;
        Ok(Value::int(nanos))
    });
}
