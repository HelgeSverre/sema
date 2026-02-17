use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use sema_core::{SemaError, Value, ValueView};

use crate::register_fn;

fn wrap_sgr(text: &str, code: &str) -> String {
    format!("\x1b[{code}m{text}\x1b[0m")
}

fn make_style_fn(env: &sema_core::Env, name: &str, code: &str) {
    let code = code.to_string();
    let fn_name = name.to_string();
    register_fn(env, name, move |args| {
        if args.len() != 1 {
            return Err(SemaError::arity(&fn_name, "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(&wrap_sgr(text, &code)))
    });
}

const SPINNER_FRAMES: &[&str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];
const SPINNER_INTERVAL_MS: u64 = 80;

struct SpinnerHandle {
    stop_flag: Arc<AtomicBool>,
    message: Arc<Mutex<String>>,
    thread: Option<std::thread::JoinHandle<()>>,
}

thread_local! {
    static SPINNERS: RefCell<HashMap<i64, SpinnerHandle>> = RefCell::new(HashMap::new());
    static SPINNER_COUNTER: Cell<i64> = const { Cell::new(0) };
}

pub fn register(env: &sema_core::Env) {
    // Modifiers
    make_style_fn(env, "term/bold", "1");
    make_style_fn(env, "term/dim", "2");
    make_style_fn(env, "term/italic", "3");
    make_style_fn(env, "term/underline", "4");
    make_style_fn(env, "term/inverse", "7");
    make_style_fn(env, "term/strikethrough", "9");

    // Foreground colors
    make_style_fn(env, "term/black", "30");
    make_style_fn(env, "term/red", "31");
    make_style_fn(env, "term/green", "32");
    make_style_fn(env, "term/yellow", "33");
    make_style_fn(env, "term/blue", "34");
    make_style_fn(env, "term/magenta", "35");
    make_style_fn(env, "term/cyan", "36");
    make_style_fn(env, "term/white", "37");
    make_style_fn(env, "term/gray", "90");

    // (term/style "text" :bold :red ...)
    register_fn(env, "term/style", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("term/style", "1+", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;

        let mut codes: Vec<&str> = Vec::new();
        for arg in &args[1..] {
            let kw = arg
                .as_keyword()
                .ok_or_else(|| SemaError::type_error("keyword", arg.type_name()))?;
            let code = match kw.as_str() {
                // Modifiers
                "bold" => "1",
                "dim" => "2",
                "italic" => "3",
                "underline" => "4",
                "inverse" => "7",
                "strikethrough" => "9",
                // Colors
                "black" => "30",
                "red" => "31",
                "green" => "32",
                "yellow" => "33",
                "blue" => "34",
                "magenta" => "35",
                "cyan" => "36",
                "white" => "37",
                "gray" => "90",
                other => {
                    return Err(SemaError::eval(format!(
                        "term/style: unknown style keyword :{other}"
                    )))
                }
            };
            codes.push(code);
        }
        if codes.is_empty() {
            return Ok(Value::string(text));
        }
        let combined = codes.join(";");
        Ok(Value::string(&wrap_sgr(text, &combined)))
    });

    // (term/strip "ansi-string") -> plain string
    register_fn(env, "term/strip", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("term/strip", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        // Remove ANSI escape sequences: ESC[ ... m
        let mut result = String::with_capacity(text.len());
        let mut chars = text.chars();
        while let Some(ch) = chars.next() {
            if ch == '\x1b' {
                // Look for '['
                if let Some(bracket) = chars.next() {
                    if bracket == '[' {
                        // Consume until 'm'
                        for inner in chars.by_ref() {
                            if inner == 'm' {
                                break;
                            }
                        }
                    }
                    // else: not an ANSI sequence, skip the char after ESC
                }
            } else {
                result.push(ch);
            }
        }
        Ok(Value::string(&result))
    });

    // (term/rgb "text" r g b) -> 24-bit color
    register_fn(env, "term/rgb", |args| {
        if args.len() != 4 {
            return Err(SemaError::arity("term/rgb", "4", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let r = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[1].type_name()))?;
        let g = args[2]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[2].type_name()))?;
        let b = args[3]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[3].type_name()))?;
        Ok(Value::string(&format!(
            "\x1b[38;2;{r};{g};{b}m{text}\x1b[0m"
        )))
    });

    // (term/spinner-start "message") -> spinner-id
    register_fn(env, "term/spinner-start", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("term/spinner-start", "1", args.len()));
        }
        let msg = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?
            .to_string();

        let id = SPINNER_COUNTER.with(|c| {
            let id = c.get();
            c.set(id + 1);
            id
        });

        let stop_flag = Arc::new(AtomicBool::new(false));
        let message = Arc::new(Mutex::new(msg));

        let stop_clone = Arc::clone(&stop_flag);
        let msg_clone = Arc::clone(&message);

        let thread = std::thread::spawn(move || {
            let mut frame_idx = 0usize;
            loop {
                if stop_clone.load(Ordering::Relaxed) {
                    break;
                }
                let msg = msg_clone.lock().unwrap().clone();
                let frame = SPINNER_FRAMES[frame_idx % SPINNER_FRAMES.len()];
                // Write spinner frame to stderr
                let mut stderr = std::io::stderr().lock();
                let _ = write!(stderr, "\r\x1b[K{frame} {msg}");
                let _ = stderr.flush();
                drop(stderr);
                frame_idx += 1;
                std::thread::sleep(std::time::Duration::from_millis(SPINNER_INTERVAL_MS));
            }
        });

        SPINNERS.with(|spinners| {
            spinners.borrow_mut().insert(
                id,
                SpinnerHandle {
                    stop_flag,
                    message,
                    thread: Some(thread),
                },
            );
        });

        Ok(Value::int(id))
    });

    // (term/spinner-stop id) or (term/spinner-stop id {:symbol "✔" :text "Done" :color :green})
    register_fn(env, "term/spinner-stop", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("term/spinner-stop", "1-2", args.len()));
        }
        let id = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[0].type_name()))?;

        // Signal stop and wait for thread
        SPINNERS.with(|spinners| {
            let mut map = spinners.borrow_mut();
            if let Some(mut handle) = map.remove(&id) {
                handle.stop_flag.store(true, Ordering::Relaxed);
                if let Some(thread) = handle.thread.take() {
                    let _ = thread.join();
                }

                // Clear the spinner line
                let mut stderr = std::io::stderr().lock();
                let _ = write!(stderr, "\r\x1b[K");

                // Print final status if options provided
                if args.len() == 2 {
                    if let ValueView::Map(opts) = args[1].view() {
                        let symbol = opts
                            .get(&Value::keyword("symbol"))
                            .and_then(|v| v.as_str().map(|s| s.to_string()))
                            .unwrap_or_default();
                        let text = opts
                            .get(&Value::keyword("text"))
                            .and_then(|v| v.as_str().map(|s| s.to_string()))
                            .unwrap_or_default();
                        if !symbol.is_empty() || !text.is_empty() {
                            let _ = writeln!(stderr, "{symbol} {text}");
                        }
                    }
                }
                let _ = stderr.flush();
            }
        });

        Ok(Value::nil())
    });

    // (term/spinner-update id "new message")
    register_fn(env, "term/spinner-update", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("term/spinner-update", "2", args.len()));
        }
        let id = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[0].type_name()))?;
        let new_msg = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?
            .to_string();

        SPINNERS.with(|spinners| {
            let map = spinners.borrow();
            if let Some(handle) = map.get(&id) {
                *handle.message.lock().unwrap() = new_msg;
            }
        });

        Ok(Value::nil())
    });
}
