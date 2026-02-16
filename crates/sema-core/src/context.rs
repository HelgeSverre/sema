use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use crate::{CallFrame, Sandbox, SemaError, Span, SpanMap, StackTrace, Value};

const MAX_SPAN_TABLE_ENTRIES: usize = 200_000;

pub struct EvalContext {
    pub module_cache: RefCell<BTreeMap<PathBuf, BTreeMap<String, Value>>>,
    pub current_file: RefCell<Vec<PathBuf>>,
    pub module_exports: RefCell<Vec<Option<Vec<String>>>>,
    pub module_load_stack: RefCell<Vec<PathBuf>>,
    pub call_stack: RefCell<Vec<CallFrame>>,
    pub span_table: RefCell<HashMap<usize, Span>>,
    pub eval_depth: Cell<usize>,
    pub eval_step_limit: Cell<usize>,
    pub eval_steps: Cell<usize>,
    pub sandbox: Sandbox,
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
            eval_step_limit: Cell::new(0),
            eval_steps: Cell::new(0),
            sandbox: Sandbox::allow_all(),
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
            eval_step_limit: Cell::new(0),
            eval_steps: Cell::new(0),
            sandbox,
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
        if table.len().saturating_add(spans.len()) > MAX_SPAN_TABLE_ENTRIES {
            table.clear();
        }
        table.extend(spans);
    }

    pub fn lookup_span(&self, ptr: usize) -> Option<Span> {
        self.span_table.borrow().get(&ptr).cloned()
    }

    pub fn set_eval_step_limit(&self, limit: usize) {
        self.eval_step_limit.set(limit);
    }
}

impl Default for EvalContext {
    fn default() -> Self {
        Self::new()
    }
}
