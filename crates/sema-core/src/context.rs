use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use crate::{CallFrame, Env, Sandbox, SemaError, Span, SpanMap, StackTrace, Value};

const MAX_SPAN_TABLE_ENTRIES: usize = 200_000;

/// Function-pointer type for the full evaluator callback: (ctx, expr, env) -> Result<Value, SemaError>
pub type EvalCallbackFn = fn(&EvalContext, &Value, &Env) -> Result<Value, SemaError>;

/// Function-pointer type for calling a function value with evaluated arguments: (ctx, func, args) -> Result<Value, SemaError>
pub type CallCallbackFn = fn(&EvalContext, &Value, &[Value]) -> Result<Value, SemaError>;

pub struct EvalContext {
    pub module_cache: RefCell<BTreeMap<PathBuf, BTreeMap<String, Value>>>,
    pub current_file: RefCell<Vec<PathBuf>>,
    pub module_exports: RefCell<Vec<Option<Vec<String>>>>,
    pub module_load_stack: RefCell<Vec<PathBuf>>,
    pub call_stack: RefCell<Vec<CallFrame>>,
    pub span_table: RefCell<HashMap<usize, Span>>,
    pub eval_depth: Cell<usize>,
    pub max_eval_depth: Cell<usize>,
    pub eval_step_limit: Cell<usize>,
    pub eval_steps: Cell<usize>,
    pub sandbox: Sandbox,
    pub user_context: RefCell<Vec<BTreeMap<Value, Value>>>,
    pub hidden_context: RefCell<Vec<BTreeMap<Value, Value>>>,
    pub context_stacks: RefCell<BTreeMap<Value, Vec<Value>>>,
    pub eval_fn: Cell<Option<EvalCallbackFn>>,
    pub call_fn: Cell<Option<CallCallbackFn>>,
    pub interactive: Cell<bool>,
}

impl EvalContext {
    pub fn new() -> Self {
        EvalContext {
            module_cache: RefCell::new(BTreeMap::new()),
            current_file: RefCell::new(Vec::new()),
            module_exports: RefCell::new(Vec::new()),
            module_load_stack: RefCell::new(Vec::new()),
            call_stack: RefCell::new(Vec::new()),
            span_table: RefCell::new(HashMap::new()),
            eval_depth: Cell::new(0),
            max_eval_depth: Cell::new(0),
            eval_step_limit: Cell::new(0),
            eval_steps: Cell::new(0),
            sandbox: Sandbox::allow_all(),
            user_context: RefCell::new(vec![BTreeMap::new()]),
            hidden_context: RefCell::new(vec![BTreeMap::new()]),
            context_stacks: RefCell::new(BTreeMap::new()),
            eval_fn: Cell::new(None),
            call_fn: Cell::new(None),
            interactive: Cell::new(false),
        }
    }

    pub fn new_with_sandbox(sandbox: Sandbox) -> Self {
        EvalContext {
            module_cache: RefCell::new(BTreeMap::new()),
            current_file: RefCell::new(Vec::new()),
            module_exports: RefCell::new(Vec::new()),
            module_load_stack: RefCell::new(Vec::new()),
            call_stack: RefCell::new(Vec::new()),
            span_table: RefCell::new(HashMap::new()),
            eval_depth: Cell::new(0),
            max_eval_depth: Cell::new(0),
            eval_step_limit: Cell::new(0),
            eval_steps: Cell::new(0),
            sandbox,
            user_context: RefCell::new(vec![BTreeMap::new()]),
            hidden_context: RefCell::new(vec![BTreeMap::new()]),
            context_stacks: RefCell::new(BTreeMap::new()),
            eval_fn: Cell::new(None),
            call_fn: Cell::new(None),
            interactive: Cell::new(false),
        }
    }

    pub fn push_file_path(&self, path: PathBuf) {
        self.current_file.borrow_mut().push(path);
    }

    pub fn pop_file_path(&self) {
        self.current_file.borrow_mut().pop();
    }

    pub fn current_file_dir(&self) -> Option<PathBuf> {
        self.current_file
            .borrow()
            .last()
            .and_then(|p| p.parent().map(|d| d.to_path_buf()))
    }

    pub fn current_file_path(&self) -> Option<PathBuf> {
        self.current_file.borrow().last().cloned()
    }

    pub fn get_cached_module(&self, path: &PathBuf) -> Option<BTreeMap<String, Value>> {
        self.module_cache.borrow().get(path).cloned()
    }

    pub fn cache_module(&self, path: PathBuf, exports: BTreeMap<String, Value>) {
        self.module_cache.borrow_mut().insert(path, exports);
    }

    pub fn set_module_exports(&self, names: Vec<String>) {
        let mut stack = self.module_exports.borrow_mut();
        if let Some(top) = stack.last_mut() {
            *top = Some(names);
        }
    }

    pub fn clear_module_exports(&self) {
        self.module_exports.borrow_mut().push(None);
    }

    pub fn take_module_exports(&self) -> Option<Vec<String>> {
        self.module_exports.borrow_mut().pop().flatten()
    }

    pub fn begin_module_load(&self, path: &PathBuf) -> Result<(), SemaError> {
        let mut stack = self.module_load_stack.borrow_mut();
        if let Some(pos) = stack.iter().position(|p| p == path) {
            let mut cycle: Vec<String> = stack[pos..]
                .iter()
                .map(|p| p.display().to_string())
                .collect();
            cycle.push(path.display().to_string());
            return Err(SemaError::eval(format!(
                "cyclic import detected: {}",
                cycle.join(" -> ")
            )));
        }
        stack.push(path.clone());
        Ok(())
    }

    pub fn end_module_load(&self, path: &PathBuf) {
        let mut stack = self.module_load_stack.borrow_mut();
        if matches!(stack.last(), Some(last) if last == path) {
            stack.pop();
        } else if let Some(pos) = stack.iter().rposition(|p| p == path) {
            stack.remove(pos);
        }
    }

    pub fn push_call_frame(&self, frame: CallFrame) {
        self.call_stack.borrow_mut().push(frame);
    }

    pub fn call_stack_depth(&self) -> usize {
        self.call_stack.borrow().len()
    }

    pub fn truncate_call_stack(&self, depth: usize) {
        self.call_stack.borrow_mut().truncate(depth);
    }

    pub fn capture_stack_trace(&self) -> StackTrace {
        let stack = self.call_stack.borrow();
        StackTrace(stack.iter().rev().cloned().collect())
    }

    pub fn merge_span_table(&self, spans: SpanMap) {
        let mut table = self.span_table.borrow_mut();
        if table.len() < MAX_SPAN_TABLE_ENTRIES {
            table.extend(spans);
        }
        // If table is full, skip merging new spans (preserves existing error locations)
    }

    pub fn lookup_span(&self, ptr: usize) -> Option<Span> {
        self.span_table.borrow().get(&ptr).cloned()
    }

    pub fn set_eval_step_limit(&self, limit: usize) {
        self.eval_step_limit.set(limit);
    }

    // --- User context methods ---

    pub fn context_get(&self, key: &Value) -> Option<Value> {
        let frames = self.user_context.borrow();
        for frame in frames.iter().rev() {
            if let Some(v) = frame.get(key) {
                return Some(v.clone());
            }
        }
        None
    }

    pub fn context_set(&self, key: Value, value: Value) {
        let mut frames = self.user_context.borrow_mut();
        if let Some(top) = frames.last_mut() {
            top.insert(key, value);
        }
    }

    pub fn context_has(&self, key: &Value) -> bool {
        let frames = self.user_context.borrow();
        frames.iter().any(|frame| frame.contains_key(key))
    }

    pub fn context_remove(&self, key: &Value) -> Option<Value> {
        let mut frames = self.user_context.borrow_mut();
        let mut first_found = None;
        for frame in frames.iter_mut().rev() {
            if let Some(v) = frame.remove(key) {
                if first_found.is_none() {
                    first_found = Some(v);
                }
            }
        }
        first_found
    }

    pub fn context_all(&self) -> BTreeMap<Value, Value> {
        let frames = self.user_context.borrow();
        let mut merged = BTreeMap::new();
        for frame in frames.iter() {
            for (k, v) in frame {
                merged.insert(k.clone(), v.clone());
            }
        }
        merged
    }

    pub fn context_push_frame(&self) {
        self.user_context.borrow_mut().push(BTreeMap::new());
    }

    pub fn context_push_frame_with(&self, bindings: BTreeMap<Value, Value>) {
        self.user_context.borrow_mut().push(bindings);
    }

    pub fn context_pop_frame(&self) {
        let mut frames = self.user_context.borrow_mut();
        if frames.len() > 1 {
            frames.pop();
        }
    }

    pub fn context_clear(&self) {
        let mut frames = self.user_context.borrow_mut();
        frames.clear();
        frames.push(BTreeMap::new());
    }

    // --- Hidden context methods ---

    pub fn hidden_get(&self, key: &Value) -> Option<Value> {
        let frames = self.hidden_context.borrow();
        for frame in frames.iter().rev() {
            if let Some(v) = frame.get(key) {
                return Some(v.clone());
            }
        }
        None
    }

    pub fn hidden_set(&self, key: Value, value: Value) {
        let mut frames = self.hidden_context.borrow_mut();
        if let Some(top) = frames.last_mut() {
            top.insert(key, value);
        }
    }

    pub fn hidden_has(&self, key: &Value) -> bool {
        let frames = self.hidden_context.borrow();
        frames.iter().any(|frame| frame.contains_key(key))
    }

    pub fn hidden_push_frame(&self) {
        self.hidden_context.borrow_mut().push(BTreeMap::new());
    }

    pub fn hidden_pop_frame(&self) {
        let mut frames = self.hidden_context.borrow_mut();
        if frames.len() > 1 {
            frames.pop();
        }
    }

    // --- Stack methods ---

    pub fn context_stack_push(&self, key: Value, value: Value) {
        self.context_stacks
            .borrow_mut()
            .entry(key)
            .or_default()
            .push(value);
    }

    pub fn context_stack_get(&self, key: &Value) -> Vec<Value> {
        self.context_stacks
            .borrow()
            .get(key)
            .cloned()
            .unwrap_or_default()
    }

    pub fn context_stack_pop(&self, key: &Value) -> Option<Value> {
        let mut stacks = self.context_stacks.borrow_mut();
        let stack = stacks.get_mut(key)?;
        let val = stack.pop();
        if stack.is_empty() {
            stacks.remove(key);
        }
        val
    }
}

impl Default for EvalContext {
    fn default() -> Self {
        Self::new()
    }
}

thread_local! {
    static STDLIB_CTX: EvalContext = EvalContext::new();
}

/// Get a reference to the shared stdlib EvalContext.
/// Use this for stdlib callback invocations instead of creating throwaway contexts.
pub fn with_stdlib_ctx<F, R>(f: F) -> R
where
    F: FnOnce(&EvalContext) -> R,
{
    STDLIB_CTX.with(f)
}

/// Register the full evaluator callback. Called by `sema-eval` during interpreter init.
/// Stores into both `ctx` and the shared `STDLIB_CTX` so that stdlib simple-fn closures
/// (which lack a ctx parameter) can still invoke the evaluator.
pub fn set_eval_callback(ctx: &EvalContext, f: EvalCallbackFn) {
    ctx.eval_fn.set(Some(f));
    STDLIB_CTX.with(|stdlib| stdlib.eval_fn.set(Some(f)));
}

/// Register the call-value callback. Called by `sema-eval` during interpreter init.
/// Stores into both `ctx` and the shared `STDLIB_CTX`.
pub fn set_call_callback(ctx: &EvalContext, f: CallCallbackFn) {
    ctx.call_fn.set(Some(f));
    STDLIB_CTX.with(|stdlib| stdlib.call_fn.set(Some(f)));
}

/// Evaluate an expression using the registered evaluator.
/// Panics if no evaluator has been registered (programming error).
pub fn eval_callback(ctx: &EvalContext, expr: &Value, env: &Env) -> Result<Value, SemaError> {
    let f = ctx
        .eval_fn
        .get()
        .expect("eval callback not registered — Interpreter::new() must be called first");
    f(ctx, expr, env)
}

/// Call a function value with arguments using the registered callback.
/// Panics if no callback has been registered (programming error).
pub fn call_callback(ctx: &EvalContext, func: &Value, args: &[Value]) -> Result<Value, SemaError> {
    let f = ctx
        .call_fn
        .get()
        .expect("call callback not registered — Interpreter::new() must be called first");
    f(ctx, func, args)
}
