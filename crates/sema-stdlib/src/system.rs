use std::cell::RefCell;
use std::collections::HashMap;
use std::io::IsTerminal;
use std::sync::atomic::{AtomicBool, Ordering};

use sema_core::{check_arity, Caps, SemaError, Value};

use crate::register_fn;

// ─── Signal pending flags (set by async signal handlers) ────────────────────
static SIGWINCH_PENDING: AtomicBool = AtomicBool::new(false);
static SIGINT_PENDING: AtomicBool = AtomicBool::new(false);
static SIGTERM_PENDING: AtomicBool = AtomicBool::new(false);

// ─── Signal callbacks (thread-local, keyed by signal number) ────────────────
// Values are Sema callables stored per-signal.
thread_local! {
    static SIGNAL_CALLBACKS: RefCell<HashMap<i32, Vec<Value>>> = RefCell::new(HashMap::new());
}

// ─── Signal handlers: only allowed to use async-signal-safe operations ───────
#[cfg(unix)]
extern "C" fn handle_sigwinch(_: libc::c_int) {
    SIGWINCH_PENDING.store(true, Ordering::Relaxed);
}

#[cfg(unix)]
extern "C" fn handle_sigint(_: libc::c_int) {
    SIGINT_PENDING.store(true, Ordering::Relaxed);
}

#[cfg(unix)]
extern "C" fn handle_sigterm(_: libc::c_int) {
    SIGTERM_PENDING.store(true, Ordering::Relaxed);
}

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    crate::register_fn_gated(env, sandbox, Caps::ENV_READ, "env", |args| {
        check_arity!(args, "env", 1);
        let name = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        match std::env::var(name) {
            Ok(val) => Ok(Value::string(&val)),
            Err(_) => Ok(Value::nil()),
        }
    });

    crate::register_fn_gated(env, sandbox, Caps::SHELL, "shell", |args| {
        check_arity!(args, "shell", 1..);
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

    fn time_ms_impl(args: &[Value]) -> Result<Value, SemaError> {
        check_arity!(args, "time-ms", 0);
        let ms = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as i64;
        Ok(Value::int(ms))
    }
    register_fn(env, "time-ms", time_ms_impl);
    // Canonical slash-namespaced alias (Decision #24)
    register_fn(env, "time/now-ms", time_ms_impl);

    register_fn(env, "sleep", |args| {
        check_arity!(args, "sleep", 1);
        let ms = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
        Ok(Value::nil())
    });

    crate::register_fn_gated(env, sandbox, Caps::PROCESS, "sys/args", |args| {
        check_arity!(args, "sys/args", 0);
        let args_list: Vec<Value> = std::env::args().map(|a| Value::string(&a)).collect();
        Ok(Value::list(args_list))
    });

    register_fn(env, "sys/cwd", |args| {
        check_arity!(args, "sys/cwd", 0);
        let cwd = std::env::current_dir().map_err(|e| SemaError::Io(format!("sys/cwd: {e}")))?;
        Ok(Value::string(&cwd.to_string_lossy()))
    });

    register_fn(env, "sys/platform", |args| {
        check_arity!(args, "sys/platform", 0);
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
        check_arity!(args, "sys/set-env", 2);
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
        check_arity!(args, "sys/env-all", 0);
        let mut map = std::collections::BTreeMap::new();
        for (key, val) in std::env::vars() {
            map.insert(Value::keyword(&key), Value::string(&val));
        }
        Ok(Value::map(map))
    });

    register_fn(env, "sys/home-dir", |args| {
        check_arity!(args, "sys/home-dir", 0);
        match std::env::var("HOME").or_else(|_| std::env::var("USERPROFILE")) {
            Ok(home) => Ok(Value::string(&home)),
            Err(_) => Ok(Value::nil()),
        }
    });

    register_fn(env, "sys/sema-home", |args| {
        check_arity!(args, "sys/sema-home", 0);
        Ok(Value::string(&sema_core::sema_home().to_string_lossy()))
    });

    register_fn(env, "sys/temp-dir", |args| {
        check_arity!(args, "sys/temp-dir", 0);
        Ok(Value::string(&std::env::temp_dir().to_string_lossy()))
    });

    register_fn(env, "sys/hostname", |args| {
        check_arity!(args, "sys/hostname", 0);
        match hostname::get() {
            Ok(name) => Ok(Value::string(&name.to_string_lossy())),
            Err(_) => Ok(Value::nil()),
        }
    });

    register_fn(env, "sys/user", |args| {
        check_arity!(args, "sys/user", 0);
        match std::env::var("USER").or_else(|_| std::env::var("USERNAME")) {
            Ok(user) => Ok(Value::string(&user)),
            Err(_) => Ok(Value::nil()),
        }
    });

    register_fn(env, "sys/interactive?", |args| {
        check_arity!(args, "sys/interactive?", 0);
        Ok(Value::bool(std::io::stdin().is_terminal()))
    });

    register_fn(env, "sys/tty", |args| {
        check_arity!(args, "sys/tty", 0);
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
        check_arity!(args, "sys/pid", 0);
        Ok(Value::int(std::process::id() as i64))
    });

    register_fn(env, "sys/arch", |args| {
        check_arity!(args, "sys/arch", 0);
        Ok(Value::string(std::env::consts::ARCH))
    });

    register_fn(env, "sys/os", |args| {
        check_arity!(args, "sys/os", 0);
        Ok(Value::string(std::env::consts::OS))
    });

    crate::register_fn_gated(env, sandbox, Caps::PROCESS, "sys/which", |args| {
        check_arity!(args, "sys/which", 1);
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

    register_fn(env, "sys/interner-stats", |args| {
        check_arity!(args, "sys/interner-stats", 0);
        let (count, bytes) = sema_core::interner_stats();
        let mut result = std::collections::BTreeMap::new();
        result.insert(Value::keyword("count"), Value::int(count as i64));
        result.insert(Value::keyword("bytes"), Value::int(bytes as i64));
        Ok(Value::map(result))
    });

    register_fn(env, "sys/elapsed", |args| {
        check_arity!(args, "sys/elapsed", 0);
        use std::time::Instant;
        thread_local! {
            static START: Instant = Instant::now();
        }
        let nanos = START.with(|s| s.elapsed().as_nanos()) as i64;
        Ok(Value::int(nanos))
    });

    // sys/term-size — returns {:rows N :cols M} or nil when not a TTY
    register_fn(env, "sys/term-size", |args| {
        check_arity!(args, "sys/term-size", 0);
        #[cfg(unix)]
        {
            let mut ws: libc::winsize = unsafe { std::mem::zeroed() };
            // Try each standard fd in order until one succeeds; stdout is most reliable
            // for terminal size since stderr is used for status lines on many setups.
            for fd in [libc::STDOUT_FILENO, libc::STDERR_FILENO, libc::STDIN_FILENO] {
                let ret = unsafe { libc::ioctl(fd, libc::TIOCGWINSZ, &mut ws) };
                if ret == 0 && ws.ws_row > 0 && ws.ws_col > 0 {
                    let mut m = std::collections::BTreeMap::new();
                    m.insert(Value::keyword("rows"), Value::int(ws.ws_row as i64));
                    m.insert(Value::keyword("cols"), Value::int(ws.ws_col as i64));
                    return Ok(Value::map(m));
                }
            }
            Ok(Value::nil())
        }
        #[cfg(not(unix))]
        Ok(Value::nil())
    });

    // ─── Signal hooks (Unix only) ────────────────────────────────────────────
    // sys/on-signal — register a Sema callback for a signal.
    // Supported signals: :winch (SIGWINCH), :int (SIGINT), :term (SIGTERM).
    // Registering a handler installs the OS signal handler the first time.
    // Call (sys/check-signals) from your event loop to dispatch pending callbacks.
    #[cfg(unix)]
    {
        use sema_core::NativeFn;

        env.set(
            sema_core::intern("sys/on-signal"),
            Value::native_fn(NativeFn::with_ctx("sys/on-signal", |_ctx, args| {
                check_arity!(args, "sys/on-signal", 2);
                let kw = args[0]
                    .as_keyword()
                    .ok_or_else(|| SemaError::type_error("keyword", args[0].type_name()))?;
                let sig_num = match kw.as_str() {
                    "winch" => libc::SIGWINCH,
                    "int" => libc::SIGINT,
                    "term" => libc::SIGTERM,
                    other => {
                        return Err(SemaError::eval(format!(
                            "sys/on-signal: unknown signal :{other}; use :winch, :int, or :term"
                        )))
                    }
                };
                let callback = args[1].clone();
                // Install the OS-level signal handler on first registration
                SIGNAL_CALLBACKS.with(|cbs| {
                    let mut map = cbs.borrow_mut();
                    let entry = map.entry(sig_num).or_default();
                    if entry.is_empty() {
                        // First callback for this signal: install handler.
                        // Cast via *const () to avoid the fn_to_numeric_cast lint.
                        let handler: libc::sighandler_t = match sig_num {
                            s if s == libc::SIGWINCH => handle_sigwinch as *const () as usize,
                            s if s == libc::SIGINT => handle_sigint as *const () as usize,
                            s if s == libc::SIGTERM => handle_sigterm as *const () as usize,
                            // Unreachable: sig_num is validated against the three above by the
                            // kw match earlier in this function.
                            _ => unreachable!("unexpected signal number {sig_num}"),
                        };
                        unsafe { libc::signal(sig_num, handler) };
                    }
                    entry.push(callback);
                });
                Ok(Value::nil())
            })),
        );

        // sys/check-signals — call all pending signal callbacks.
        // Should be called from the main event loop (e.g., after io/read-key returns).
        env.set(
            sema_core::intern("sys/check-signals"),
            Value::native_fn(NativeFn::with_ctx("sys/check-signals", |ctx, args| {
                check_arity!(args, "sys/check-signals", 0);
                let mut to_dispatch: Vec<(i32, Vec<Value>)> = Vec::new();

                if SIGWINCH_PENDING.swap(false, Ordering::Relaxed) {
                    SIGNAL_CALLBACKS.with(|cbs| {
                        if let Some(callbacks) = cbs.borrow().get(&libc::SIGWINCH) {
                            to_dispatch.push((libc::SIGWINCH, callbacks.clone()));
                        }
                    });
                }
                if SIGINT_PENDING.swap(false, Ordering::Relaxed) {
                    SIGNAL_CALLBACKS.with(|cbs| {
                        if let Some(callbacks) = cbs.borrow().get(&libc::SIGINT) {
                            to_dispatch.push((libc::SIGINT, callbacks.clone()));
                        }
                    });
                }
                if SIGTERM_PENDING.swap(false, Ordering::Relaxed) {
                    SIGNAL_CALLBACKS.with(|cbs| {
                        if let Some(callbacks) = cbs.borrow().get(&libc::SIGTERM) {
                            to_dispatch.push((libc::SIGTERM, callbacks.clone()));
                        }
                    });
                }

                for (_, callbacks) in to_dispatch {
                    for cb in &callbacks {
                        sema_core::call_callback(ctx, cb, &[])?;
                    }
                }
                Ok(Value::nil())
            })),
        );
    }
}
