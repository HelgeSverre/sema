use std::cell::RefCell;
use std::rc::Rc;

use sema_core::{Env, NativeFn, SemaError, Value};
use wasm_bindgen::prelude::*;

thread_local! {
    static OUTPUT: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

fn capture_output(s: String) {
    OUTPUT.with(|o| o.borrow_mut().push(s));
}

fn take_output() -> Vec<String> {
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
            capture_output(out);
            Ok(Value::Nil)
        }),
    );

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
            capture_output(out);
            Ok(Value::Nil)
        }),
    );

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
            capture_output(out);
            Ok(Value::Nil)
        }),
    );

    register(
        "newline",
        Box::new(|_args: &[Value]| {
            capture_output(String::new());
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
            capture_output(format!("[error] {out}"));
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
            capture_output(format!("[error] {out}"));
            Ok(Value::Nil)
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

fn escape_json(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}
