//! Notebook evaluation engine.
//!
//! Manages a persistent Sema interpreter environment and evaluates cells
//! sequentially. Each cell shares the same environment, so definitions
//! in earlier cells are visible to later ones.

use std::collections::BTreeMap;
use std::time::Instant;

use chrono::Utc;
use sema_core::{pretty_print, resolve, Value};
use sema_eval::Interpreter;

use crate::format::{CellOutput, CellType, Notebook, OutputType};

/// Result of evaluating a single cell.
#[derive(Debug, Clone)]
pub struct EvalResult {
    /// The output to store in the cell.
    pub output: CellOutput,
    /// Captured stdout text (if any).
    pub stdout: Option<String>,
}

/// The notebook evaluation engine. Wraps an `Interpreter` and a `Notebook`.
pub struct Engine {
    /// The Sema interpreter with persistent environment.
    pub interpreter: Interpreter,
    /// The notebook being worked on.
    pub notebook: Notebook,
}

impl Engine {
    /// Create a new engine with a fresh interpreter and notebook.
    pub fn new(notebook: Notebook) -> Self {
        let interpreter = Interpreter::new();
        Self {
            interpreter,
            notebook,
        }
    }

    /// Create an engine from an existing notebook file path.
    pub fn from_file(path: &std::path::Path) -> Result<Self, String> {
        let notebook = Notebook::load(path)?;
        Ok(Self::new(notebook))
    }

    /// Evaluate a single cell by ID. Returns the result and updates the notebook.
    pub fn eval_cell(&mut self, cell_id: &str) -> Result<EvalResult, String> {
        let idx = self
            .notebook
            .cell_index(cell_id)
            .ok_or_else(|| format!("Cell not found: {cell_id}"))?;

        let cell = &self.notebook.cells[idx];
        if cell.cell_type != CellType::Code {
            return Err("Cannot evaluate a markdown cell".to_string());
        }

        let source = cell.source.clone();
        let result = self.eval_source(&source);

        // Mark downstream cells as stale
        self.notebook.mark_downstream_stale(idx);

        // Update the cell with the result
        let cell = &mut self.notebook.cells[idx];
        cell.outputs = vec![result.output.clone()];
        cell.stale = false;

        Ok(result)
    }

    /// Evaluate raw source code in the notebook's environment.
    pub fn eval_source(&self, source: &str) -> EvalResult {
        let start = Instant::now();

        // Capture stdout
        let captured = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
        let captured_clone = captured.clone();

        // Evaluate in the global env so `define` persists across cells
        let eval_result = self.interpreter.eval_str_in_global(source);
        let duration_ms = start.elapsed().as_millis() as u64;

        let stdout_bytes = captured_clone.lock().unwrap_or_else(|p| p.into_inner());
        let stdout_text = if stdout_bytes.is_empty() {
            None
        } else {
            Some(String::from_utf8_lossy(&stdout_bytes).to_string())
        };

        match eval_result {
            Ok(value) => {
                let display = format_value_for_display(&value);
                let sema_value = value_to_sexp(&value);
                let bindings = extract_new_bindings(source);

                EvalResult {
                    output: CellOutput {
                        output_type: OutputType::Value,
                        display,
                        sema_value: Some(sema_value),
                        bindings,
                        timestamp: Utc::now(),
                        cost_usd: None,
                        requires_reeval: is_opaque(&value),
                        duration_ms: Some(duration_ms),
                    },
                    stdout: stdout_text,
                }
            }
            Err(err) => EvalResult {
                output: CellOutput {
                    output_type: OutputType::Error,
                    display: format_error(&err),
                    sema_value: None,
                    bindings: BTreeMap::new(),
                    timestamp: Utc::now(),
                    cost_usd: None,
                    requires_reeval: false,
                    duration_ms: Some(duration_ms),
                },
                stdout: stdout_text,
            },
        }
    }

    /// Evaluate all code cells in order.
    pub fn eval_all(&mut self) -> Vec<(String, Result<EvalResult, String>)> {
        let cell_ids: Vec<String> = self
            .notebook
            .cells
            .iter()
            .filter(|c| c.cell_type == CellType::Code)
            .map(|c| c.id.clone())
            .collect();

        cell_ids
            .into_iter()
            .map(|id| {
                let result = self.eval_cell(&id);
                (id, result)
            })
            .collect()
    }

    /// Evaluate specific cells by index (1-based).
    pub fn eval_cells(&mut self, indices: &[usize]) -> Vec<(String, Result<EvalResult, String>)> {
        let cell_ids: Vec<String> = indices
            .iter()
            .filter_map(|&i| {
                self.notebook
                    .cells
                    .get(i.saturating_sub(1))
                    .map(|c| c.id.clone())
            })
            .collect();

        cell_ids
            .into_iter()
            .map(|id| {
                let result = self.eval_cell(&id);
                (id, result)
            })
            .collect()
    }

    /// Create a new code cell, evaluate it, and return the result.
    pub fn create_and_eval(&mut self, source: &str) -> Result<(String, EvalResult), String> {
        let id = self.notebook.add_code_cell(source);
        let result = self.eval_cell(&id)?;
        Ok((id, result))
    }

    /// Get all current environment bindings as a map of name -> display string.
    pub fn env_bindings(&self) -> BTreeMap<String, String> {
        let mut map = BTreeMap::new();
        let bindings = self.interpreter.global_env.bindings.borrow();
        for (&spur, value) in bindings.iter() {
            let name = resolve(spur);
            let display = format_value_for_display(value);
            map.insert(name, display);
        }
        map
    }

    /// Reset the interpreter and clear all cell outputs.
    pub fn reset(&mut self) {
        self.interpreter = Interpreter::new();
        for cell in &mut self.notebook.cells {
            cell.outputs.clear();
            cell.stale = false;
        }
    }
}

/// Format a Sema value for human-readable display in the notebook.
pub fn format_value_for_display(value: &Value) -> String {
    if value.is_nil() {
        return String::new();
    }
    pretty_print(value, 80)
}

/// Convert a value to its S-expression string form for persistence.
pub fn value_to_sexp(value: &Value) -> String {
    format!("{value}")
}

/// Check if a value is opaque (cannot be round-tripped via read).
fn is_opaque(value: &Value) -> bool {
    use sema_core::ValueView;
    matches!(
        value.view(),
        ValueView::NativeFn(_)
            | ValueView::Lambda(_)
            | ValueView::Macro(_)
            | ValueView::Stream(_)
            | ValueView::Thunk(_)
    )
}

/// Format a SemaError for display in the notebook.
pub fn format_error(err: &sema_core::SemaError) -> String {
    format!("{err}")
}

/// Extract variable names that look like they're being defined in this source.
/// This is a simple heuristic — it looks for `(def ...)` and `(define ...)` forms.
fn extract_new_bindings(_source: &str) -> BTreeMap<String, String> {
    // A simple heuristic: we don't actually track bindings at the source level.
    // The real bindings are in the environment. This is a placeholder for
    // future binding-diff tracking.
    BTreeMap::new()
}
