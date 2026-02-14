//! Sema — a Lisp with LLM primitives.
//!
//! This module provides a clean embedding API for the Sema interpreter.
//!
//! # Quick Start
//!
//! ```no_run
//! use sema::{Interpreter, InterpreterBuilder, Value};
//!
//! let interp = InterpreterBuilder::new().with_llm(true).build();
//! let result = interp.eval_str("(+ 1 2)").unwrap();
//! assert_eq!(result, Value::Int(3));
//! ```

use std::rc::Rc;

// Re-export core types.
pub use sema_core::{intern, resolve, with_resolved, Env, SemaError, Value};
/// Result of evaluating a Sema expression.
pub type EvalResult = Result<Value>;

pub type Result<T> = std::result::Result<T, SemaError>;

/// Builder for configuring and constructing an [`Interpreter`].
///
/// By default, the standard library is enabled and LLM builtins are disabled.
pub struct InterpreterBuilder {
    stdlib: bool,
    llm: bool,
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
            llm: false,
        }
    }

    /// Enable or disable the standard library (default: `true`).
    pub fn with_stdlib(mut self, enable: bool) -> Self {
        self.stdlib = enable;
        self
    }

    /// Enable or disable the LLM builtins (default: `false`).
    pub fn with_llm(mut self, enable: bool) -> Self {
        self.llm = enable;
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
        let env = Env::new();

        if self.stdlib {
            sema_stdlib::register_stdlib(&env);
        }

        if self.llm {
            sema_llm::builtins::register_llm_builtins(&env);
            sema_llm::builtins::set_eval_callback(sema_eval::eval_value);
        }

        Interpreter {
            inner: sema_eval::Interpreter {
                global_env: Rc::new(env),
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
    ///     match &args[0] {
    ///         Value::Int(n) => Ok(Value::Int(n * n)),
    ///         other => Err(SemaError::type_error("integer", format!("{other}"))),
    ///     }
    /// });
    /// ```
    pub fn register_fn<F>(&self, name: &str, f: F)
    where
        F: Fn(&[Value]) -> Result<Value> + 'static,
    {
        use sema_core::NativeFn;

        let native = NativeFn {
            name: name.to_string(),
            func: Box::new(f),
        };
        self.inner
            .global_env
            .set_str(name, Value::NativeFn(Rc::new(native)));
    }

    /// Return a reference to the global environment.
    pub fn global_env(&self) -> &Rc<Env> {
        &self.inner.global_env
    }

    pub fn env(&self) -> &Rc<Env> {
        self.global_env()
    }
}
