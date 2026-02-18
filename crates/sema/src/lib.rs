//! Sema — a Lisp with LLM primitives.
//!
//! This module provides a clean embedding API for the Sema interpreter.
//!
//! # Quick Start
//!
//! ```no_run
//! use sema::{Interpreter, InterpreterBuilder, Value};
//!
//! let interp = InterpreterBuilder::new().build();
//! let result = interp.eval_str("(+ 1 2)").unwrap();
//! assert_eq!(result, Value::int(3));
//! ```

use std::rc::Rc;

// Re-export core types.
pub use sema_core::{intern, resolve, with_resolved, Caps, Env, Sandbox, SemaError, Value};
/// Result of evaluating a Sema expression.
pub type EvalResult = Result<Value>;

pub type Result<T> = std::result::Result<T, SemaError>;

/// Builder for configuring and constructing an [`Interpreter`].
///
/// By default, both the standard library and LLM builtins are enabled.
pub struct InterpreterBuilder {
    stdlib: bool,
    llm: bool,
    sandbox: Sandbox,
}

impl Default for InterpreterBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl InterpreterBuilder {
    /// Create a new builder.
    pub fn new() -> Self {
        Self {
            stdlib: true,
            llm: true,
            sandbox: Sandbox::allow_all(),
        }
    }

    /// Enable or disable the standard library (default: `true`).
    pub fn with_stdlib(mut self, enable: bool) -> Self {
        self.stdlib = enable;
        self
    }

    /// Enable or disable the LLM builtins (default: `true`).
    pub fn with_llm(mut self, enable: bool) -> Self {
        self.llm = enable;
        self
    }

    /// Set the sandbox configuration to restrict dangerous operations.
    pub fn with_sandbox(mut self, sandbox: Sandbox) -> Self {
        self.sandbox = sandbox;
        self
    }

    /// Restrict file operations to the given directories.
    pub fn with_allowed_paths(mut self, paths: Vec<std::path::PathBuf>) -> Self {
        self.sandbox = self.sandbox.with_allowed_paths(paths);
        self
    }

    /// Disable the standard library.
    pub fn without_stdlib(self) -> Self {
        self.with_stdlib(false)
    }

    /// Disable the LLM builtins.
    pub fn without_llm(self) -> Self {
        self.with_llm(false)
    }

    /// Build the [`Interpreter`] with the configured options.
    pub fn build(self) -> Interpreter {
        sema_llm::builtins::reset_runtime_state();

        let env = Env::new();
        let ctx = sema_eval::EvalContext::new();

        sema_core::set_eval_callback(&ctx, sema_eval::eval_value);
        sema_core::set_call_callback(&ctx, sema_eval::call_value);

        if self.stdlib {
            sema_stdlib::register_stdlib(&env, &self.sandbox);
        }

        if self.llm {
            sema_llm::builtins::register_llm_builtins(&env, &self.sandbox);
            sema_llm::builtins::set_eval_callback(sema_eval::eval_value);
        }

        Interpreter {
            inner: sema_eval::Interpreter {
                global_env: Rc::new(env),
                ctx,
            },
        }
    }
}

/// A Sema Lisp interpreter instance.
///
/// Use [`InterpreterBuilder`] for fine-grained control, or call
/// [`Interpreter::new`] for a default interpreter with stdlib enabled.
pub struct Interpreter {
    inner: sema_eval::Interpreter,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        InterpreterBuilder::new().build()
    }

    /// Create an [`InterpreterBuilder`] for fine-grained configuration.
    pub fn builder() -> InterpreterBuilder {
        InterpreterBuilder::new()
    }

    /// Evaluate a single parsed [`Value`] expression.
    ///
    /// Definitions (`define`) persist across calls.
    pub fn eval(&self, expr: &Value) -> EvalResult {
        self.inner.eval_in_global(expr)
    }

    /// Parse and evaluate a string containing one or more Sema expressions.
    ///
    /// Definitions (`define`) persist across calls, so you can define a
    /// function in one call and use it in the next.
    pub fn eval_str(&self, input: &str) -> EvalResult {
        self.inner.eval_str_in_global(input)
    }

    /// Register a native function that can be called from Sema code.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use sema::{Interpreter, Value, SemaError};
    ///
    /// let interp = Interpreter::new();
    /// interp.register_fn("square", |args: &[Value]| {
    ///     if let Some(n) = args[0].as_int() {
    ///         Ok(Value::int(n * n))
    ///     } else {
    ///         Err(SemaError::type_error("integer", args[0].type_name()))
    ///     }
    /// });
    /// ```
    pub fn register_fn<F>(&self, name: &str, f: F)
    where
        F: Fn(&[Value]) -> Result<Value> + 'static,
    {
        use sema_core::NativeFn;

        let native = NativeFn::simple(name, f);
        self.inner
            .global_env
            .set_str(name, Value::native_fn(native));
    }

    /// Load and evaluate a `.sema` file.
    ///
    /// Definitions persist in the global environment, just like [`eval_str`].
    ///
    /// ```no_run
    /// # use sema::Interpreter;
    /// let interp = Interpreter::new();
    /// interp.load_file("prelude.sema").unwrap();
    /// interp.eval_str("(my-prelude-fn 42)").unwrap();
    /// ```
    pub fn load_file(&self, path: impl AsRef<std::path::Path>) -> EvalResult {
        let path = path.as_ref();
        let content = std::fs::read_to_string(path)
            .map_err(|e| SemaError::eval(format!("load_file {}: {e}", path.display())))?;
        self.eval_str(&content)
    }

    /// Pre-load a module into the module cache so that `(import "name")`
    /// resolves without reading from disk.
    ///
    /// The `name` is the string users pass to `import`. The `source` is
    /// evaluated in an isolated module environment, and all top-level
    /// bindings (or only `export`-ed ones) are cached.
    ///
    /// ```no_run
    /// # use sema::Interpreter;
    /// let interp = Interpreter::new();
    /// interp.preload_module("utils", r#"
    ///     (define (double x) (* x 2))
    /// "#).unwrap();
    ///
    /// interp.eval_str(r#"(import "utils")"#).unwrap();
    /// interp.eval_str("(double 21)").unwrap(); // => 42
    /// ```
    ///
    /// Use `(module name (export ...) ...)` to control which bindings are visible:
    ///
    /// ```no_run
    /// # use sema::Interpreter;
    /// let interp = Interpreter::new();
    /// interp.preload_module("math", r#"
    ///     (module math (export square)
    ///       (define (square x) (* x x))
    ///       (define internal 42))
    /// "#).unwrap();
    /// ```
    pub fn preload_module(&self, name: &str, source: &str) -> Result<()> {
        use sema_core::resolve;
        use std::collections::BTreeMap;

        let (exprs, spans) = sema_reader::read_many_with_spans(source)?;
        self.inner.ctx.merge_span_table(spans);

        // Evaluate in an isolated child env (like a real import does).
        let module_env = Env::with_parent(self.inner.global_env.clone());
        self.inner.ctx.clear_module_exports();

        for expr in &exprs {
            sema_eval::eval_value(&self.inner.ctx, expr, &module_env)?;
        }

        let declared = self.inner.ctx.take_module_exports();

        // Collect exports: if (export ...) was used, only those; else all bindings.
        let bindings = module_env.bindings.borrow();
        let exports: BTreeMap<String, Value> = match declared {
            Some(names) => names
                .iter()
                .filter_map(|n| {
                    let spur = intern(n);
                    bindings.get(&spur).map(|v| (n.clone(), v.clone()))
                })
                .collect(),
            None => bindings
                .iter()
                .map(|(spur, val)| (resolve(*spur), val.clone()))
                .collect(),
        };
        drop(bindings);

        // Cache under the bare name so `(import "name")` resolves it
        // before attempting to canonicalize a real file path.
        let key = std::path::PathBuf::from(name);
        self.inner.ctx.cache_module(key, exports);

        Ok(())
    }

    /// Return a reference to the global environment.
    pub fn global_env(&self) -> &Rc<Env> {
        &self.inner.global_env
    }

    pub fn env(&self) -> &Rc<Env> {
        self.global_env()
    }
}
