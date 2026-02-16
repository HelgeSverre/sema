use std::cell::RefCell;
use std::rc::Rc;

use sema_core::{
    intern, resolve, CallFrame, Env, EvalContext, Lambda, Macro, NativeFn, SemaError, Span, Thunk,
    Value,
};

use crate::special_forms;

/// Trampoline for tail-call optimization.
pub enum Trampoline {
    Value(Value),
    Eval(Value, Env),
}

pub type EvalResult = Result<Value, SemaError>;

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

/// Look up a span for an expression via the span table in the context.
fn span_of_expr(ctx: &EvalContext, expr: &Value) -> Option<Span> {
    match expr {
        Value::List(items) => {
            let ptr = Rc::as_ptr(items) as usize;
            ctx.lookup_span(ptr)
        }
        _ => None,
    }
}

/// RAII guard that truncates the call stack on drop.
struct CallStackGuard<'a> {
    ctx: &'a EvalContext,
    entry_depth: usize,
}

impl Drop for CallStackGuard<'_> {
    fn drop(&mut self) {
        self.ctx.truncate_call_stack(self.entry_depth);
    }
}

/// The interpreter holds the global environment and state.
pub struct Interpreter {
    pub global_env: Rc<Env>,
    pub ctx: EvalContext,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let env = Env::new();
        let ctx = EvalContext::new();
        // Register eval/call callbacks so stdlib can invoke the real evaluator
        sema_core::set_eval_callback(eval_value);
        sema_core::set_call_callback(call_value);
        // Register stdlib
        sema_stdlib::register_stdlib(&env, &sema_core::Sandbox::allow_all());
        // Register LLM builtins
        #[cfg(not(target_arch = "wasm32"))]
        {
            sema_llm::builtins::reset_runtime_state();
            sema_llm::builtins::register_llm_builtins(&env, &sema_core::Sandbox::allow_all());
            sema_llm::builtins::set_eval_callback(eval_value);
        }
        let global_env = Rc::new(env);
        register_vm_delegates(&global_env);
        Interpreter { global_env, ctx }
    }

    pub fn new_with_sandbox(sandbox: &sema_core::Sandbox) -> Self {
        let env = Env::new();
        let ctx = EvalContext::new_with_sandbox(sandbox.clone());
        sema_core::set_eval_callback(eval_value);
        sema_core::set_call_callback(call_value);
        sema_stdlib::register_stdlib(&env, sandbox);
        #[cfg(not(target_arch = "wasm32"))]
        {
            sema_llm::builtins::reset_runtime_state();
            sema_llm::builtins::register_llm_builtins(&env, sandbox);
            sema_llm::builtins::set_eval_callback(eval_value);
        }
        let global_env = Rc::new(env);
        register_vm_delegates(&global_env);
        Interpreter { global_env, ctx }
    }

    pub fn eval(&self, expr: &Value) -> EvalResult {
        eval_value(&self.ctx, expr, &Env::with_parent(self.global_env.clone()))
    }

    pub fn eval_str(&self, input: &str) -> EvalResult {
        eval_string(&self.ctx, input, &Env::with_parent(self.global_env.clone()))
    }

    /// Evaluate in the global environment so that `define` persists across calls.
    pub fn eval_in_global(&self, expr: &Value) -> EvalResult {
        eval_value(&self.ctx, expr, &self.global_env)
    }

    /// Parse and evaluate in the global environment so that `define` persists across calls.
    pub fn eval_str_in_global(&self, input: &str) -> EvalResult {
        eval_string(&self.ctx, input, &self.global_env)
    }

    /// Parse, compile to bytecode, and execute via the VM.
    pub fn eval_str_compiled(&self, input: &str) -> EvalResult {
        let (exprs, spans) = sema_reader::read_many_with_spans(input)?;
        self.ctx.merge_span_table(spans);
        if exprs.is_empty() {
            return Ok(Value::Nil);
        }

        let mut expanded = Vec::new();
        for expr in &exprs {
            let exp = self.expand_for_vm(expr)?;
            expanded.push(exp);
        }

        let (closure, functions) = sema_vm::compile_program(&expanded)?;
        let mut vm = sema_vm::VM::new(self.global_env.clone(), functions);
        vm.execute(closure, &self.ctx)
    }

    /// Compile a pre-parsed Value AST to bytecode and execute via the VM.
    pub fn eval_compiled(&self, expr: &Value) -> EvalResult {
        let expanded = self.expand_for_vm(expr)?;
        let (closure, functions) = sema_vm::compile_program(std::slice::from_ref(&expanded))?;
        let mut vm = sema_vm::VM::new(self.global_env.clone(), functions);
        vm.execute(closure, &self.ctx)
    }

    /// Pre-process a top-level expression for VM compilation.
    /// Evaluates `defmacro` forms via the tree-walker to register macros,
    /// then expands macro calls in all other forms.
    fn expand_for_vm(&self, expr: &Value) -> EvalResult {
        if let Value::List(items) = expr {
            if let Some(Value::Symbol(s)) = items.first() {
                let name = resolve(*s);
                if name == "defmacro" {
                    eval_value(&self.ctx, expr, &self.global_env)?;
                    return Ok(Value::Nil);
                }
                if name == "begin" {
                    let mut new_items = vec![Value::Symbol(*s)];
                    for item in &items[1..] {
                        new_items.push(self.expand_for_vm(item)?);
                    }
                    return Ok(Value::List(Rc::new(new_items)));
                }
            }
        }
        self.expand_macros(expr)
    }

    /// Recursively expand macro calls in an expression.
    fn expand_macros(&self, expr: &Value) -> EvalResult {
        match expr {
            Value::List(items) if !items.is_empty() => {
                if let Some(Value::Symbol(s)) = items.first() {
                    let name = resolve(*s);
                    if name == "quote" {
                        return Ok(expr.clone());
                    }
                    if let Some(Value::Macro(mac)) = self.global_env.get(*s) {
                        let expanded = apply_macro(&self.ctx, &mac, &items[1..], &self.global_env)?;
                        return self.expand_macros(&expanded);
                    }
                }
                let expanded: Result<Vec<Value>, SemaError> =
                    items.iter().map(|v| self.expand_macros(v)).collect();
                Ok(Value::List(Rc::new(expanded?)))
            }
            _ => Ok(expr.clone()),
        }
    }
}

/// Evaluate a string containing one or more expressions.
pub fn eval_string(ctx: &EvalContext, input: &str, env: &Env) -> EvalResult {
    let (exprs, spans) = sema_reader::read_many_with_spans(input)?;
    ctx.merge_span_table(spans);
    let mut result = Value::Nil;
    for expr in &exprs {
        result = eval_value(ctx, expr, env)?;
    }
    Ok(result)
}

/// The core eval function: evaluate a Value in an environment.
pub fn eval(ctx: &EvalContext, expr: &Value, env: &Env) -> EvalResult {
    eval_value(ctx, expr, env)
}

/// Maximum eval nesting depth before we bail with an error.
/// This prevents native stack overflow from unbounded recursion
/// (both function calls and special form nesting like deeply nested if/let/begin).
const MAX_EVAL_DEPTH: usize = 1024;

pub fn eval_value(ctx: &EvalContext, expr: &Value, env: &Env) -> EvalResult {
    // Fast path: self-evaluating forms skip depth/step tracking entirely.
    match expr {
        Value::Nil
        | Value::Bool(_)
        | Value::Int(_)
        | Value::Float(_)
        | Value::String(_)
        | Value::Char(_)
        | Value::Keyword(_)
        | Value::Thunk(_)
        | Value::Bytevector(_)
        | Value::NativeFn(_)
        | Value::Lambda(_)
        | Value::HashMap(_) => return Ok(expr.clone()),
        Value::Symbol(spur) => {
            if let Some(val) = env.get(*spur) {
                return Ok(val);
            }
            return Err(SemaError::Unbound(resolve(*spur)));
        }
        _ => {}
    }

    let depth = ctx.eval_depth.get();
    ctx.eval_depth.set(depth + 1);
    if depth == 0 {
        ctx.eval_steps.set(0);
    }
    if depth > MAX_EVAL_DEPTH {
        ctx.eval_depth.set(ctx.eval_depth.get().saturating_sub(1));
        return Err(SemaError::eval(format!(
            "maximum eval depth exceeded ({MAX_EVAL_DEPTH})"
        )));
    }

    let result = eval_value_inner(ctx, expr, env);

    ctx.eval_depth.set(ctx.eval_depth.get().saturating_sub(1));
    result
}

/// Call a function value with already-evaluated arguments.
/// This is the public API for stdlib functions that need to invoke callbacks.
pub fn call_value(ctx: &EvalContext, func: &Value, args: &[Value]) -> EvalResult {
    match func {
        Value::NativeFn(native) => (native.func)(ctx, args),
        Value::Lambda(lambda) => {
            let new_env = Env::with_parent(Rc::new(lambda.env.clone()));

            if let Some(rest) = lambda.rest_param {
                if args.len() < lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda
                            .name
                            .map(resolve)
                            .unwrap_or_else(|| "lambda".to_string()),
                        format!("{}+", lambda.params.len()),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    new_env.set(*param, arg.clone());
                }
                let rest_args = args[lambda.params.len()..].to_vec();
                new_env.set(rest, Value::list(rest_args));
            } else {
                if args.len() != lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda
                            .name
                            .map(resolve)
                            .unwrap_or_else(|| "lambda".to_string()),
                        lambda.params.len().to_string(),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    new_env.set(*param, arg.clone());
                }
            }

            if let Some(name) = lambda.name {
                new_env.set(name, Value::Lambda(Rc::clone(lambda)));
            }

            let mut result = Value::Nil;
            for expr in &lambda.body {
                result = eval_value(ctx, expr, &new_env)?;
            }
            Ok(result)
        }
        Value::Keyword(spur) => {
            if args.len() != 1 {
                let name = resolve(*spur);
                return Err(SemaError::arity(format!(":{name}"), "1", args.len()));
            }
            let key = Value::Keyword(*spur);
            match &args[0] {
                Value::Map(map) => Ok(map.get(&key).cloned().unwrap_or(Value::Nil)),
                Value::HashMap(map) => Ok(map.get(&key).cloned().unwrap_or(Value::Nil)),
                _ => Err(SemaError::type_error("map", args[0].type_name())),
            }
        }
        other => Err(
            SemaError::eval(format!("not callable: {} ({})", other, other.type_name()))
                .with_hint("expected a function, lambda, or keyword"),
        ),
    }
}

fn eval_value_inner(ctx: &EvalContext, expr: &Value, env: &Env) -> EvalResult {
    let entry_depth = ctx.call_stack_depth();
    let guard = CallStackGuard { ctx, entry_depth };
    let limit = ctx.eval_step_limit.get();

    // First iteration: use borrowed expr/env to avoid cloning
    if limit > 0 {
        let v = ctx.eval_steps.get() + 1;
        ctx.eval_steps.set(v);
        if v > limit {
            return Err(SemaError::eval("eval step limit exceeded".to_string()));
        }
    }

    match eval_step(ctx, expr, env) {
        Ok(Trampoline::Value(v)) => {
            drop(guard);
            Ok(v)
        }
        Ok(Trampoline::Eval(next_expr, next_env)) => {
            // Need to continue — enter the trampoline loop
            let mut current_expr = next_expr;
            let mut current_env = next_env;

            // Trim call stack for TCO
            {
                let mut stack = ctx.call_stack.borrow_mut();
                if stack.len() > entry_depth + 1 {
                    let top = stack.last().cloned();
                    stack.truncate(entry_depth);
                    if let Some(frame) = top {
                        stack.push(frame);
                    }
                }
            }

            loop {
                if limit > 0 {
                    let v = ctx.eval_steps.get() + 1;
                    ctx.eval_steps.set(v);
                    if v > limit {
                        return Err(SemaError::eval("eval step limit exceeded".to_string()));
                    }
                }

                match eval_step(ctx, &current_expr, &current_env) {
                    Ok(Trampoline::Value(v)) => {
                        drop(guard);
                        return Ok(v);
                    }
                    Ok(Trampoline::Eval(next_expr, next_env)) => {
                        {
                            let mut stack = ctx.call_stack.borrow_mut();
                            if stack.len() > entry_depth + 1 {
                                let top = stack.last().cloned();
                                stack.truncate(entry_depth);
                                if let Some(frame) = top {
                                    stack.push(frame);
                                }
                            }
                        }
                        current_expr = next_expr;
                        current_env = next_env;
                    }
                    Err(e) => {
                        if e.stack_trace().is_none() {
                            let trace = ctx.capture_stack_trace();
                            drop(guard);
                            return Err(e.with_stack_trace(trace));
                        }
                        drop(guard);
                        return Err(e);
                    }
                }
            }
        }
        Err(e) => {
            if e.stack_trace().is_none() {
                let trace = ctx.capture_stack_trace();
                drop(guard);
                return Err(e.with_stack_trace(trace));
            }
            drop(guard);
            Err(e)
        }
    }
}

fn eval_step(ctx: &EvalContext, expr: &Value, env: &Env) -> Result<Trampoline, SemaError> {
    match expr {
        // Self-evaluating forms
        Value::Nil
        | Value::Bool(_)
        | Value::Int(_)
        | Value::Float(_)
        | Value::String(_)
        | Value::Char(_)
        | Value::Thunk(_)
        | Value::Bytevector(_) => Ok(Trampoline::Value(expr.clone())),
        Value::Keyword(_) => Ok(Trampoline::Value(expr.clone())),
        Value::Vector(items) => {
            let mut result = Vec::with_capacity(items.len());
            for item in items.iter() {
                result.push(eval_value(ctx, item, env)?);
            }
            Ok(Trampoline::Value(Value::vector(result)))
        }
        Value::Map(map) => {
            let mut result = std::collections::BTreeMap::new();
            for (k, v) in map.iter() {
                let ek = eval_value(ctx, k, env)?;
                let ev = eval_value(ctx, v, env)?;
                result.insert(ek, ev);
            }
            Ok(Trampoline::Value(Value::Map(Rc::new(result))))
        }
        Value::HashMap(_) => Ok(Trampoline::Value(expr.clone())),

        // Symbol lookup
        Value::Symbol(spur) => env
            .get(*spur)
            .map(Trampoline::Value)
            .ok_or_else(|| SemaError::Unbound(resolve(*spur))),

        // Function application / special forms
        Value::List(items) => {
            if items.is_empty() {
                return Ok(Trampoline::Value(Value::Nil));
            }

            let head = &items[0];
            let args = &items[1..];

            // O(1) special form dispatch: compare the symbol's Spur (u32 interned handle)
            // against cached constants, avoiding string resolution entirely.
            if let Value::Symbol(spur) = head {
                if let Some(result) = special_forms::try_eval_special(*spur, args, env, ctx) {
                    return result;
                }
            }

            // Evaluate the head to get the callable
            let func = eval_value(ctx, head, env)?;

            // Look up the span of the call site expression
            let call_span = span_of_expr(ctx, expr);

            match &func {
                Value::NativeFn(native) => {
                    // Evaluate arguments
                    let mut eval_args = Vec::with_capacity(args.len());
                    for arg in args {
                        eval_args.push(eval_value(ctx, arg, env)?);
                    }
                    // Push frame, call native fn
                    let frame = CallFrame {
                        name: native.name.to_string(),
                        file: ctx.current_file_path(),
                        span: call_span,
                    };
                    ctx.push_call_frame(frame);
                    match (native.func)(ctx, &eval_args) {
                        Ok(v) => {
                            // Pop on success (native fns don't trampoline)
                            ctx.truncate_call_stack(ctx.call_stack_depth().saturating_sub(1));
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
                        eval_args.push(eval_value(ctx, arg, env)?);
                    }
                    // Push frame — trampoline continues, eval_value guard handles cleanup
                    let frame = CallFrame {
                        name: lambda
                            .name
                            .map(resolve)
                            .unwrap_or_else(|| "<lambda>".to_string()),
                        file: ctx.current_file_path(),
                        span: call_span,
                    };
                    ctx.push_call_frame(frame);
                    apply_lambda(ctx, lambda, &eval_args)
                }
                Value::Macro(mac) => {
                    // Macros receive unevaluated arguments
                    let expanded = apply_macro(ctx, mac, args, env)?;
                    // Evaluate the expansion in the current env (TCO)
                    Ok(Trampoline::Eval(expanded, env.clone()))
                }
                Value::Keyword(spur) => {
                    // Keywords as functions: (:key map) => (get map :key)
                    if args.len() != 1 {
                        let name = resolve(*spur);
                        return Err(SemaError::arity(format!(":{name}"), "1", args.len()));
                    }
                    let map_val = eval_value(ctx, &args[0], env)?;
                    let key = Value::Keyword(*spur);
                    match &map_val {
                        Value::Map(map) => Ok(Trampoline::Value(
                            map.get(&key).cloned().unwrap_or(Value::Nil),
                        )),
                        Value::HashMap(map) => Ok(Trampoline::Value(
                            map.get(&key).cloned().unwrap_or(Value::Nil),
                        )),
                        _ => Err(SemaError::type_error("map", map_val.type_name())),
                    }
                }
                other => {
                    Err(
                        SemaError::eval(format!("not callable: {} ({})", other, other.type_name()))
                            .with_hint("the first element of a list must be a function or macro"),
                    )
                }
            }
        }

        other => Ok(Trampoline::Value(other.clone())),
    }
}

/// Apply a lambda to evaluated arguments with TCO.
fn apply_lambda(
    ctx: &EvalContext,
    lambda: &Rc<Lambda>,
    args: &[Value],
) -> Result<Trampoline, SemaError> {
    let new_env = Env::with_parent(Rc::new(lambda.env.clone()));

    // Bind parameters
    if let Some(rest) = lambda.rest_param {
        if args.len() < lambda.params.len() {
            return Err(SemaError::arity(
                lambda
                    .name
                    .map(resolve)
                    .unwrap_or_else(|| "lambda".to_string()),
                format!("{}+", lambda.params.len()),
                args.len(),
            ));
        }
        for (param, arg) in lambda.params.iter().zip(args.iter()) {
            new_env.set(*param, arg.clone());
        }
        let rest_args = args[lambda.params.len()..].to_vec();
        new_env.set(rest, Value::list(rest_args));
    } else {
        if args.len() != lambda.params.len() {
            return Err(SemaError::arity(
                lambda
                    .name
                    .map(resolve)
                    .unwrap_or_else(|| "lambda".to_string()),
                lambda.params.len().to_string(),
                args.len(),
            ));
        }
        for (param, arg) in lambda.params.iter().zip(args.iter()) {
            new_env.set(*param, arg.clone());
        }
    }

    // Self-reference for recursion — just clone the Rc pointer
    if let Some(name) = lambda.name {
        new_env.set(name, Value::Lambda(Rc::clone(lambda)));
    }

    // Evaluate body with TCO on last expression
    if lambda.body.is_empty() {
        return Ok(Trampoline::Value(Value::Nil));
    }
    for expr in &lambda.body[..lambda.body.len() - 1] {
        eval_value(ctx, expr, &new_env)?;
    }
    Ok(Trampoline::Eval(
        lambda.body.last().unwrap().clone(),
        new_env,
    ))
}

/// Apply a macro: bind unevaluated args, evaluate body to produce expansion.
pub fn apply_macro(
    ctx: &EvalContext,
    mac: &sema_core::Macro,
    args: &[Value],
    caller_env: &Env,
) -> Result<Value, SemaError> {
    let env = Env::with_parent(Rc::new(caller_env.clone()));

    // Bind parameters to unevaluated forms
    if let Some(rest) = mac.rest_param {
        if args.len() < mac.params.len() {
            return Err(SemaError::arity(
                resolve(mac.name),
                format!("{}+", mac.params.len()),
                args.len(),
            ));
        }
        for (param, arg) in mac.params.iter().zip(args.iter()) {
            env.set(*param, arg.clone());
        }
        let rest_args = args[mac.params.len()..].to_vec();
        env.set(rest, Value::list(rest_args));
    } else {
        if args.len() != mac.params.len() {
            return Err(SemaError::arity(
                resolve(mac.name),
                mac.params.len().to_string(),
                args.len(),
            ));
        }
        for (param, arg) in mac.params.iter().zip(args.iter()) {
            env.set(*param, arg.clone());
        }
    }

    // Evaluate the macro body to get the expansion
    let mut result = Value::Nil;
    for expr in &mac.body {
        result = eval_value(ctx, expr, &env)?;
    }
    Ok(result)
}

/// Register `__vm-*` native functions that the bytecode VM calls back into
/// the tree-walker for forms that cannot be fully compiled.
fn register_vm_delegates(env: &Rc<Env>) {
    // __vm-eval: evaluate an expression via the tree-walker
    let eval_env = env.clone();
    env.set(
        intern("__vm-eval"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-eval",
            move |ctx, args| {
                if args.len() != 1 {
                    return Err(SemaError::arity("eval", "1", args.len()));
                }
                sema_core::eval_callback(ctx, &args[0], &eval_env)
            },
        ))),
    );

    // __vm-load: load and evaluate a file via the tree-walker
    let load_env = env.clone();
    env.set(
        intern("__vm-load"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-load",
            move |ctx, args| {
                if args.len() != 1 {
                    return Err(SemaError::arity("load", "1", args.len()));
                }
                let path = match &args[0] {
                    Value::String(s) => s.to_string(),
                    _ => return Err(SemaError::type_error("string", args[0].type_name())),
                };
                let full_path = if let Some(dir) = ctx.current_file_dir() {
                    dir.join(&path)
                } else {
                    std::path::PathBuf::from(&path)
                };
                let content = std::fs::read_to_string(&full_path).map_err(|e| {
                    SemaError::eval(format!("load: cannot read {}: {}", full_path.display(), e))
                })?;
                ctx.push_file_path(full_path);
                let result = eval_string(ctx, &content, &load_env);
                ctx.pop_file_path();
                result
            },
        ))),
    );

    // __vm-import: import a module via the tree-walker
    let import_env = env.clone();
    env.set(
        intern("__vm-import"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-import",
            move |ctx, args| {
                if args.len() != 2 {
                    return Err(SemaError::arity("import", "2", args.len()));
                }
                let mut form = vec![Value::Symbol(intern("import")), args[0].clone()];
                if let Value::List(items) = &args[1] {
                    if !items.is_empty() {
                        for item in items.iter() {
                            form.push(item.clone());
                        }
                    }
                }
                let import_expr = Value::List(Rc::new(form));
                sema_core::eval_callback(ctx, &import_expr, &import_env)
            },
        ))),
    );

    // __vm-defmacro: register a macro in the environment
    let macro_env = env.clone();
    env.set(
        intern("__vm-defmacro"),
        Value::NativeFn(Rc::new(NativeFn::simple("__vm-defmacro", move |args| {
            if args.len() != 4 {
                return Err(SemaError::arity("defmacro", "4", args.len()));
            }
            let name = match &args[0] {
                Value::Symbol(s) => *s,
                _ => return Err(SemaError::type_error("symbol", args[0].type_name())),
            };
            let params = match &args[1] {
                Value::List(items) => items
                    .iter()
                    .map(|v| match v {
                        Value::Symbol(s) => Ok(*s),
                        _ => Err(SemaError::type_error("symbol", v.type_name())),
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                _ => return Err(SemaError::type_error("list", args[1].type_name())),
            };
            let rest_param = match &args[2] {
                Value::Symbol(s) => Some(*s),
                Value::Nil => None,
                _ => return Err(SemaError::type_error("symbol or nil", args[2].type_name())),
            };
            let body = vec![args[3].clone()];
            macro_env.set(
                name,
                Value::Macro(Rc::new(Macro {
                    params,
                    rest_param,
                    body,
                    name,
                })),
            );
            Ok(Value::Nil)
        }))),
    );

    // __vm-defmacro-form: delegate complete defmacro form to the tree-walker
    let dmf_env = env.clone();
    env.set(
        intern("__vm-defmacro-form"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-defmacro-form",
            move |ctx, args| {
                if args.len() != 1 {
                    return Err(SemaError::arity("defmacro-form", "1", args.len()));
                }
                sema_core::eval_callback(ctx, &args[0], &dmf_env)
            },
        ))),
    );

    // __vm-define-record-type: delegate to the tree-walker
    let drt_env = env.clone();
    env.set(
        intern("__vm-define-record-type"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-define-record-type",
            move |ctx, args| {
                if args.len() != 5 {
                    return Err(SemaError::arity("define-record-type", "5", args.len()));
                }
                let mut ctor_form = vec![args[1].clone()];
                if let Value::List(fields) = &args[3] {
                    ctor_form.extend(fields.iter().cloned());
                }
                let mut form = vec![
                    Value::Symbol(intern("define-record-type")),
                    args[0].clone(),
                    Value::List(ctor_form.into()),
                    args[2].clone(),
                ];
                if let Value::List(specs) = &args[4] {
                    for spec in specs.iter() {
                        form.push(spec.clone());
                    }
                }
                sema_core::eval_callback(ctx, &Value::List(Rc::new(form)), &drt_env)
            },
        ))),
    );

    // __vm-delay: create a thunk with unevaluated body
    env.set(
        intern("__vm-delay"),
        Value::NativeFn(Rc::new(NativeFn::simple("__vm-delay", |args| {
            if args.len() != 1 {
                return Err(SemaError::arity("delay", "1", args.len()));
            }
            // args[0] is the unevaluated body expression (passed as a quoted constant)
            Ok(Value::Thunk(Rc::new(Thunk {
                body: args[0].clone(),
                forced: RefCell::new(None),
            })))
        }))),
    );

    // __vm-force: force a thunk
    let force_env = env.clone();
    env.set(
        intern("__vm-force"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-force",
            move |ctx, args| {
                if args.len() != 1 {
                    return Err(SemaError::arity("force", "1", args.len()));
                }
                match &args[0] {
                    Value::Thunk(thunk) => {
                        if let Some(val) = thunk.forced.borrow().as_ref() {
                            return Ok(val.clone());
                        }
                        let val = match &thunk.body {
                            Value::NativeFn(_) | Value::Lambda(_) => {
                                sema_core::call_callback(ctx, &thunk.body, &[])?
                            }
                            other => {
                                sema_core::eval_callback(ctx, other, &force_env)?
                            }
                        };
                        *thunk.forced.borrow_mut() = Some(val.clone());
                        Ok(val)
                    }
                    other => Ok(other.clone()),
                }
            },
        ))),
    );

    // __vm-macroexpand: expand a macro form via the tree-walker
    let me_env = env.clone();
    env.set(
        intern("__vm-macroexpand"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-macroexpand",
            move |ctx, args| {
                if args.len() != 1 {
                    return Err(SemaError::arity("macroexpand", "1", args.len()));
                }
                if let Value::List(items) = &args[0] {
                    if !items.is_empty() {
                        if let Value::Symbol(spur) = &items[0] {
                            if let Some(Value::Macro(mac)) = me_env.get(*spur) {
                                return apply_macro(ctx, &mac, &items[1..], &me_env);
                            }
                        }
                    }
                }
                Ok(args[0].clone())
            },
        ))),
    );

    // __vm-prompt: delegate to tree-walker
    let prompt_env = env.clone();
    env.set(
        intern("__vm-prompt"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-prompt",
            move |ctx, args| {
                let mut form = vec![Value::Symbol(intern("prompt"))];
                form.extend(args.iter().cloned());
                sema_core::eval_callback(ctx, &Value::List(Rc::new(form)), &prompt_env)
            },
        ))),
    );

    // __vm-message: delegate to tree-walker
    let msg_env = env.clone();
    env.set(
        intern("__vm-message"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-message",
            move |ctx, args| {
                if args.len() != 2 {
                    return Err(SemaError::arity("message", "2", args.len()));
                }
                let form = Value::List(Rc::new(vec![
                    Value::Symbol(intern("message")),
                    args[0].clone(),
                    args[1].clone(),
                ]));
                sema_core::eval_callback(ctx, &form, &msg_env)
            },
        ))),
    );

    // __vm-deftool: delegate to tree-walker
    let tool_env = env.clone();
    env.set(
        intern("__vm-deftool"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-deftool",
            move |ctx, args| {
                if args.len() != 4 {
                    return Err(SemaError::arity("deftool", "4", args.len()));
                }
                let form = Value::List(Rc::new(vec![
                    Value::Symbol(intern("deftool")),
                    args[0].clone(),
                    args[1].clone(),
                    args[2].clone(),
                    args[3].clone(),
                ]));
                sema_core::eval_callback(ctx, &form, &tool_env)
            },
        ))),
    );

    // __vm-defagent: delegate to tree-walker
    let agent_env = env.clone();
    env.set(
        intern("__vm-defagent"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-defagent",
            move |ctx, args| {
                if args.len() != 2 {
                    return Err(SemaError::arity("defagent", "2", args.len()));
                }
                let form = Value::List(Rc::new(vec![
                    Value::Symbol(intern("defagent")),
                    args[0].clone(),
                    args[1].clone(),
                ]));
                sema_core::eval_callback(ctx, &form, &agent_env)
            },
        ))),
    );

    // __vm-with-budget: delegate to tree-walker
    let budget_env = env.clone();
    env.set(
        intern("__vm-with-budget"),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(
            "__vm-with-budget",
            move |ctx, args| {
                if args.len() != 2 {
                    return Err(SemaError::arity("with-budget", "2", args.len()));
                }
                let form = Value::List(Rc::new(vec![
                    Value::Symbol(intern("with-budget")),
                    args[0].clone(),
                    args[1].clone(),
                ]));
                sema_core::eval_callback(ctx, &form, &budget_env)
            },
        ))),
    );
}
