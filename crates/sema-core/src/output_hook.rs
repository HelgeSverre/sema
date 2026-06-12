use std::cell::RefCell;

type OutputHook = Option<Box<dyn Fn(&str) + Send>>;

// Thread-local output hooks for capturing program stdout/stderr.
// Used by the DAP server to redirect program output into DAP `Output` events
// instead of letting it corrupt the JSON-RPC protocol stream on stdout.
thread_local! {
    static STDOUT_HOOK: RefCell<OutputHook> = RefCell::new(None);
    static STDERR_HOOK: RefCell<OutputHook> = RefCell::new(None);
}

/// Set the thread-local stdout capture hook.
/// Pass `None` to clear.
pub fn set_stdout_hook(hook: OutputHook) {
    STDOUT_HOOK.with(|cell| *cell.borrow_mut() = hook);
}

/// Set the thread-local stderr capture hook.
/// Pass `None` to clear.
pub fn set_stderr_hook(hook: OutputHook) {
    STDERR_HOOK.with(|cell| *cell.borrow_mut() = hook);
}

/// Write a string to stdout, either through the hook (if set) or via `print!`.
pub fn write_stdout(s: &str) {
    STDOUT_HOOK.with(|cell| {
        if let Some(hook) = cell.borrow().as_ref() {
            hook(s);
        } else {
            print!("{}", s);
        }
    });
}

/// Write a string to stderr, either through the hook (if set) or via `eprint!`.
pub fn write_stderr(s: &str) {
    STDERR_HOOK.with(|cell| {
        if let Some(hook) = cell.borrow().as_ref() {
            hook(s);
        } else {
            eprint!("{}", s);
        }
    });
}
