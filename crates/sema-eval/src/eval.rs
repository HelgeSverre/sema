use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::rc::Rc;

use sema_core::{CallFrame, Env, Lambda, SemaError, Span, SpanMap, StackTrace, Value};

use crate::special_forms;

/// Trampoline for tail-call optimization.
pub enum Trampoline {
    Value(Value),
    Eval(Value, Env),
}

pub type EvalResult = Result<Value, SemaError>;

// --- Module system thread-local state ---

thread_local! {
    /// Cache of already-loaded modules: canonical path → exported bindings
    static MODULE_CACHE: RefCell<BTreeMap<PathBuf, BTreeMap<String, Value>>> = const { RefCell::new(BTreeMap::new()) };
    /// Stack of file paths being executed (for relative path resolution)
    static CURRENT_FILE: RefCell<Vec<PathBuf>> = const { RefCell::new(Vec::new()) };
    /// Export names declared by the currently-loading module
    static MODULE_EXPORTS: RefCell<Option<Vec<String>>> = const { RefCell::new(None) };
    /// Call stack for error traces
    static CALL_STACK: RefCell<Vec<CallFrame>> = const { RefCell::new(Vec::new()) };
    /// Span table: Rc pointer address → source span
    static SPAN_TABLE: RefCell<HashMap<usize, Span>> = RefCell::new(HashMap::new());
}

pub fn push_file_path(path: PathBuf) {
    CURRENT_FILE.with(|f| f.borrow_mut().push(path));
}

pub fn pop_file_path() {
    CURRENT_FILE.with(|f| f.borrow_mut().pop());
}

pub fn current_file_dir() -> Option<PathBuf> {
    CURRENT_FILE.with(|f| {
        f.borrow()
            .last()
            .and_then(|p| p.parent().map(|d| d.to_path_buf()))
    })
}

pub fn get_cached_module(path: &PathBuf) -> Option<BTreeMap<String, Value>> {
    MODULE_CACHE.with(|c| c.borrow().get(path).cloned())
}

pub fn cache_module(path: PathBuf, exports: BTreeMap<String, Value>) {
    MODULE_CACHE.with(|c| c.borrow_mut().insert(path, exports));
}

pub fn set_module_exports(names: Vec<String>) {
    MODULE_EXPORTS.with(|e| *e.borrow_mut() = Some(names));
}

pub fn clear_module_exports() {
    MODULE_EXPORTS.with(|e| *e.borrow_mut() = None);
}

pub fn take_module_exports() -> Option<Vec<String>> {
    MODULE_EXPORTS.with(|e| e.borrow_mut().take())
}

/// Create an isolated module env: child of root (global/stdlib) env
pub fn create_module_env(env: &Env) -> Env {
    // Walk parent chain to find root
    let mut current = env.clone();
    loop {
        let parent = current.parent.clone();
        match parent {
            Some(p) => current = (*p).clone(),
            None => break,
        }
    }
    Env::with_parent(Rc::new(current))
}

// --- Call stack / span table functions ---

pub fn push_call_frame(frame: CallFrame) {
    CALL_STACK.with(|s| s.borrow_mut().push(frame));
}

pub fn call_stack_depth() -> usize {
    CALL_STACK.with(|s| s.borrow().len())
}

pub fn truncate_call_stack(depth: usize) {
    CALL_STACK.with(|s| {
        let mut stack = s.borrow_mut();
        stack.truncate(depth);
    });
}

pub fn capture_stack_trace() -> StackTrace {
    CALL_STACK.with(|s| {
        let stack = s.borrow();
        StackTrace(stack.iter().rev().cloned().collect())
    })
}

pub fn merge_span_table(spans: SpanMap) {
    SPAN_TABLE.with(|t| t.borrow_mut().extend(spans));
}

pub fn lookup_span(ptr: usize) -> Option<Span> {
    SPAN_TABLE.with(|t| t.borrow().get(&ptr).cloned())
}

/// Get the current file name (for stack frames).
pub fn current_file_name() -> Option<String> {
    CURRENT_FILE.with(|f| f.borrow().last().map(|p| p.to_string_lossy().to_string()))
}

/// Look up a span for an expression via the SPAN_TABLE.
fn span_of_expr(expr: &Value) -> Option<Span> {
    match expr {
        Value::List(items) => {
            let ptr = Rc::as_ptr(items) as usize;
            lookup_span(ptr)
        }
        _ => None,
    }
}

/// RAII guard that truncates the call stack on drop.
struct CallStackGuard {
    entry_depth: usize,
}

impl Drop for CallStackGuard {
    fn drop(&mut self) {
        truncate_call_stack(self.entry_depth);
    }
}

/// The interpreter holds the global environment and state.
pub struct Interpreter {
    pub global_env: Rc<Env>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Env::new();
        // Register stdlib
        sema_stdlib::register_stdlib(&env);
        // Register LLM builtins
        sema_llm::builtins::register_llm_builtins(&env);
        // Register the full evaluator callback for LLM tool execution
        sema_llm::builtins::set_eval_callback(eval_value);
        Interpreter {
            global_env: Rc::new(env),
        }
    }

    pub fn eval(&self, expr: &Value) -> EvalResult {
        let env = Env::with_parent(self.global_env.clone());
        eval_value(expr, &env)
    }

    pub fn eval_str(&self, input: &str) -> EvalResult {
        eval_string(input, &Env::with_parent(self.global_env.clone()))
    }
}

/// Evaluate a string containing one or more expressions.
pub fn eval_string(input: &str, env: &Env) -> EvalResult {
    let (exprs, spans) = sema_reader::read_many_with_spans(input)?;
    merge_span_table(spans);
    let mut result = Value::Nil;
    for expr in &exprs {
        result = eval_value(expr, env)?;
    }
    Ok(result)
}

/// The core eval function: evaluate a Value in an environment.
pub fn eval(expr: &Value, env: &Env) -> EvalResult {
    eval_value(expr, env)
}

/// Evaluate with trampoline for TCO.
pub fn eval_value(expr: &Value, env: &Env) -> EvalResult {
    let mut current_expr = expr.clone();
    let mut current_env = env.clone();
    let entry_depth = call_stack_depth();
    let guard = CallStackGuard { entry_depth };

    loop {
        match eval_step(&current_expr, &current_env) {
            Ok(Trampoline::Value(v)) => {
                drop(guard);
                return Ok(v);
            }
            Ok(Trampoline::Eval(next_expr, next_env)) => {
                // Tail call: replace accumulated frames with just the most recent one.
                // This keeps one frame for the current function (TCO frame replacement)
                // while preventing unbounded growth in tail-recursive loops.
                CALL_STACK.with(|s| {
                    let mut stack = s.borrow_mut();
                    if stack.len() > entry_depth + 1 {
                        let top = stack.last().cloned();
                        stack.truncate(entry_depth);
                        if let Some(frame) = top {
                            stack.push(frame);
                        }
                    }
                });
                current_expr = next_expr;
                current_env = next_env;
            }
            Err(e) => {
                if e.stack_trace().is_none() {
                    let trace = capture_stack_trace();
                    drop(guard);
                    return Err(e.with_stack_trace(trace));
                }
                drop(guard);
                return Err(e);
            }
        }
    }
}

fn eval_step(expr: &Value, env: &Env) -> Result<Trampoline, SemaError> {
    match expr {
        // Self-evaluating forms
        Value::Nil | Value::Bool(_) | Value::Int(_) | Value::Float(_) | Value::String(_) => {
            Ok(Trampoline::Value(expr.clone()))
        }
        Value::Keyword(_) => Ok(Trampoline::Value(expr.clone())),
        Value::Vector(items) => {
            let mut result = Vec::with_capacity(items.len());
            for item in items.iter() {
                result.push(eval_value(item, env)?);
            }
            Ok(Trampoline::Value(Value::vector(result)))
        }
        Value::Map(map) => {
            let mut result = std::collections::BTreeMap::new();
            for (k, v) in map.iter() {
                let ek = eval_value(k, env)?;
                let ev = eval_value(v, env)?;
                result.insert(ek, ev);
            }
            Ok(Trampoline::Value(Value::Map(Rc::new(result))))
        }

        // Symbol lookup
        Value::Symbol(name) => env
            .get(name)
            .map(Trampoline::Value)
            .ok_or_else(|| SemaError::Unbound(name.to_string())),

        // Function application / special forms
        Value::List(items) => {
            if items.is_empty() {
                return Ok(Trampoline::Value(Value::Nil));
            }

            let head = &items[0];
            let args = &items[1..];

            // Check for special forms (by symbol name)
            if let Value::Symbol(name) = head {
                if let Some(result) = special_forms::try_eval_special(name, args, env) {
                    return result;
                }
            }

            // Evaluate the head to get the callable
            let func = eval_value(head, env)?;

            // Look up the span of the call site expression
            let call_span = span_of_expr(expr);

            match &func {
                Value::NativeFn(native) => {
                    // Evaluate arguments
                    let mut eval_args = Vec::with_capacity(args.len());
                    for arg in args {
                        eval_args.push(eval_value(arg, env)?);
                    }
                    // Push frame, call native fn
                    let frame = CallFrame {
                        name: native.name.to_string(),
                        file: current_file_name(),
                        span: call_span,
                    };
                    push_call_frame(frame);
                    match (native.func)(&eval_args) {
                        Ok(v) => {
                            // Pop on success (native fns don't trampoline)
                            truncate_call_stack(call_stack_depth().saturating_sub(1));
                            Ok(Trampoline::Value(v))
                        }
                        // On error, leave frame for stack trace capture
                        Err(e) => Err(e),
                    }
                }
                Value::Lambda(lambda) => {
                    // Evaluate arguments
                    let mut eval_args = Vec::with_capacity(args.len());
                    for arg in args {
                        eval_args.push(eval_value(arg, env)?);
                    }
                    // Push frame — trampoline continues, eval_value guard handles cleanup
                    let frame = CallFrame {
                        name: lambda.name.as_deref().unwrap_or("<lambda>").to_string(),
                        file: current_file_name(),
                        span: call_span,
                    };
                    push_call_frame(frame);
                    apply_lambda(lambda, &eval_args)
                }
                Value::Macro(mac) => {
                    // Macros receive unevaluated arguments
                    let expanded = apply_macro(mac, args, env)?;
                    // Evaluate the expansion in the current env (TCO)
                    Ok(Trampoline::Eval(expanded, env.clone()))
                }
                Value::Keyword(kw) => {
                    // Keywords as functions: (:key map) => (get map :key)
                    if args.len() != 1 {
                        return Err(SemaError::arity(format!(":{kw}"), "1", args.len()));
                    }
                    let map_val = eval_value(&args[0], env)?;
                    match &map_val {
                        Value::Map(map) => {
                            let key = Value::Keyword(Rc::clone(kw));
                            Ok(Trampoline::Value(
                                map.get(&key).cloned().unwrap_or(Value::Nil),
                            ))
                        }
                        _ => Err(SemaError::type_error("map", map_val.type_name())),
                    }
                }
                other => Err(SemaError::eval(format!(
                    "not callable: {} ({})",
                    other,
                    other.type_name()
                ))),
            }
        }

        other => Ok(Trampoline::Value(other.clone())),
    }
}

/// Apply a lambda to evaluated arguments with TCO.
fn apply_lambda(lambda: &Lambda, args: &[Value]) -> Result<Trampoline, SemaError> {
    let new_env = Env::with_parent(Rc::new(lambda.env.clone()));

    // Bind parameters
    if let Some(ref rest) = lambda.rest_param {
        if args.len() < lambda.params.len() {
            return Err(SemaError::arity(
                lambda.name.as_deref().unwrap_or("lambda"),
                format!("{}+", lambda.params.len()),
                args.len(),
            ));
        }
        for (param, arg) in lambda.params.iter().zip(args.iter()) {
            new_env.set(param.clone(), arg.clone());
        }
        let rest_args = args[lambda.params.len()..].to_vec();
        new_env.set(rest.clone(), Value::list(rest_args));
    } else {
        if args.len() != lambda.params.len() {
            return Err(SemaError::arity(
                lambda.name.as_deref().unwrap_or("lambda"),
                lambda.params.len().to_string(),
                args.len(),
            ));
        }
        for (param, arg) in lambda.params.iter().zip(args.iter()) {
            new_env.set(param.clone(), arg.clone());
        }
    }

    // Self-reference for recursion
    if let Some(ref name) = lambda.name {
        new_env.set(
            name.clone(),
            Value::Lambda(Rc::new(Lambda {
                params: lambda.params.clone(),
                rest_param: lambda.rest_param.clone(),
                body: lambda.body.clone(),
                env: lambda.env.clone(),
                name: lambda.name.clone(),
            })),
        );
    }

    // Evaluate body with TCO on last expression
    if lambda.body.is_empty() {
        return Ok(Trampoline::Value(Value::Nil));
    }
    for expr in &lambda.body[..lambda.body.len() - 1] {
        eval_value(expr, &new_env)?;
    }
    Ok(Trampoline::Eval(
        lambda.body.last().unwrap().clone(),
        new_env,
    ))
}

/// Apply a macro: bind unevaluated args, evaluate body to produce expansion.
pub fn apply_macro(
    mac: &sema_core::Macro,
    args: &[Value],
    caller_env: &Env,
) -> Result<Value, SemaError> {
    let env = Env::with_parent(Rc::new(caller_env.clone()));

    // Bind parameters to unevaluated forms
    if let Some(ref rest) = mac.rest_param {
        if args.len() < mac.params.len() {
            return Err(SemaError::arity(
                &mac.name,
                format!("{}+", mac.params.len()),
                args.len(),
            ));
        }
        for (param, arg) in mac.params.iter().zip(args.iter()) {
            env.set(param.clone(), arg.clone());
        }
        let rest_args = args[mac.params.len()..].to_vec();
        env.set(rest.clone(), Value::list(rest_args));
    } else {
        if args.len() != mac.params.len() {
            return Err(SemaError::arity(
                &mac.name,
                mac.params.len().to_string(),
                args.len(),
            ));
        }
        for (param, arg) in mac.params.iter().zip(args.iter()) {
            env.set(param.clone(), arg.clone());
        }
    }

    // Evaluate the macro body to get the expansion
    let mut result = Value::Nil;
    for expr in &mac.body {
        result = eval_value(expr, &env)?;
    }
    Ok(result)
}
