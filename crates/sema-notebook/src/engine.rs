//! Notebook evaluation engine.
//!
//! Manages a persistent Sema interpreter environment and evaluates cells
//! sequentially. Each cell shares the same environment, so definitions
//! in earlier cells are visible to later ones.

use std::collections::BTreeMap;
use std::io::Read as _;
use std::time::Instant;

use chrono::Utc;
use sema_core::{pretty_print, resolve, Spur, Value};
use sema_eval::Interpreter;

use crate::format::{CellOutput, CellType, Notebook, OutputType};

/// Snapshot of the global environment bindings before a cell eval.
struct EnvSnapshot {
    /// Cloned bindings from the global env.
    bindings: Vec<(Spur, Value)>,
    /// The cell that was about to be evaluated.
    cell_id: String,
    /// The cell's outputs before evaluation (for restore).
    cell_outputs: Vec<CellOutput>,
}

/// Result of evaluating a single cell.
#[derive(Debug, Clone)]
pub struct EvalResult {
    /// The output to store in the cell.
    pub output: CellOutput,
    /// Captured stdout from println/display/print calls.
    pub stdout: String,
}

/// Result of undoing the last cell evaluation.
#[derive(Debug)]
pub struct UndoInfo {
    /// The cell whose evaluation was undone.
    pub cell_id: String,
}

/// The notebook evaluation engine. Wraps an `Interpreter` and a `Notebook`.
pub struct Engine {
    /// The Sema interpreter with persistent environment.
    pub interpreter: Interpreter,
    /// The notebook being worked on.
    pub notebook: Notebook,
    /// Snapshot from before the last cell eval (for single-step undo).
    snapshot: Option<EnvSnapshot>,
}

impl Engine {
    /// Create a new engine with a fresh interpreter and notebook.
    pub fn new(notebook: Notebook) -> Self {
        let interpreter = Interpreter::new();
        Self {
            interpreter,
            notebook,
            snapshot: None,
        }
    }

    /// Create an engine from an existing notebook file path.
    pub fn from_file(path: &std::path::Path) -> Result<Self, String> {
        let notebook = Notebook::load(path)?;
        Ok(Self::new(notebook))
    }

    /// Evaluate a single cell by ID. Returns the result and updates the notebook.
    ///
    /// Automatically snapshots the environment before evaluation so
    /// the cell can be undone with [`undo_last_cell`].
    pub fn eval_cell(&mut self, cell_id: &str) -> Result<EvalResult, String> {
        let idx = self
            .notebook
            .cell_index(cell_id)
            .ok_or_else(|| format!("Cell not found: {cell_id}"))?;

        let cell = &self.notebook.cells[idx];
        if cell.cell_type != CellType::Code {
            return Err("Cannot evaluate a markdown cell".to_string());
        }

        // Snapshot env + cell outputs before evaluation
        let mut bindings = Vec::new();
        self.interpreter
            .global_env
            .iter_bindings(|spur, value| bindings.push((spur, value.clone())));
        self.snapshot = Some(EnvSnapshot {
            bindings,
            cell_id: cell_id.to_string(),
            cell_outputs: cell.outputs.clone(),
        });

        let source = cell.source.clone();
        let result = self.eval_source(&source);

        // Mark downstream cells as stale
        self.notebook.mark_downstream_stale(idx);

        // Update the cell with outputs: stdout first (if any), then value/error
        let cell = &mut self.notebook.cells[idx];
        cell.outputs.clear();
        if !result.stdout.is_empty() {
            cell.outputs.push(CellOutput {
                output_type: OutputType::Stdout,
                display: result.stdout.clone(),
                sema_value: None,
                timestamp: Utc::now(),
                cost_usd: None,
                requires_reeval: false,
                duration_ms: None,
            });
        }
        cell.outputs.push(result.output.clone());
        cell.stale = false;

        Ok(result)
    }

    /// Evaluate raw source code in the notebook's environment.
    ///
    /// Captures stdout during evaluation so that `println`/`display`/`print`
    /// output is available in the result rather than going to the server's
    /// terminal.
    pub fn eval_source(&mut self, source: &str) -> EvalResult {
        let start = Instant::now();

        // Capture stdout during evaluation
        let mut captured = String::new();
        let eval_result = if let Ok(mut redirect) = gag::BufferRedirect::stdout() {
            let result = self.interpreter.eval_str_compiled(source);
            let _ = redirect.read_to_string(&mut captured);
            drop(redirect);
            result
        } else {
            // Fallback: eval without capture (shouldn't happen)
            self.interpreter.eval_str_compiled(source)
        };

        let duration_ms = start.elapsed().as_millis() as u64;

        match eval_result {
            Ok(value) => {
                let display = format_value_for_display(&value);
                let sema_value = value_to_sexp(&value);

                EvalResult {
                    stdout: captured,
                    output: CellOutput {
                        output_type: OutputType::Value,
                        display,
                        sema_value: Some(sema_value),
                        timestamp: Utc::now(),
                        cost_usd: None,
                        requires_reeval: is_opaque(&value),
                        duration_ms: Some(duration_ms),
                    },
                }
            }
            Err(err) => EvalResult {
                stdout: captured,
                output: CellOutput {
                    output_type: OutputType::Error,
                    display: format_error(&err),
                    sema_value: None,
                    timestamp: Utc::now(),
                    cost_usd: None,
                    requires_reeval: false,
                    duration_ms: Some(duration_ms),
                },
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
        self.interpreter.global_env.iter_bindings(|spur, value| {
            let name = resolve(spur);
            let display = format_value_for_display(value);
            map.insert(name, display);
        });
        map
    }

    /// Whether the last cell evaluation can be undone.
    pub fn can_undo(&self) -> bool {
        self.snapshot.is_some()
    }

    /// Undo the last cell evaluation, restoring the environment and cell outputs.
    pub fn undo_last_cell(&mut self) -> Result<UndoInfo, String> {
        let snapshot = self
            .snapshot
            .take()
            .ok_or_else(|| "Nothing to undo".to_string())?;

        // Restore environment bindings
        self.interpreter
            .global_env
            .replace_bindings(snapshot.bindings);

        // Restore the cell's outputs
        if let Some(cell) = self.notebook.cell_mut(&snapshot.cell_id) {
            cell.outputs = snapshot.cell_outputs;
        }

        Ok(UndoInfo {
            cell_id: snapshot.cell_id,
        })
    }

    /// Reset the interpreter and clear all cell outputs.
    pub fn reset(&mut self) {
        self.interpreter = Interpreter::new();
        self.snapshot = None;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::format::Notebook;

    fn test_engine() -> Engine {
        Engine::new(Notebook::new("Test"))
    }

    #[test]
    fn eval_simple_expression() {
        let mut engine = test_engine();
        let (id, result) = engine.create_and_eval("(+ 1 2)").unwrap();
        assert_eq!(result.output.output_type, OutputType::Value);
        assert_eq!(result.output.display, "3");
        assert!(result.output.duration_ms.is_some());
        // Cell should be stored in notebook with at least the value output
        assert!(engine.notebook.cell(&id).is_some());
        let outputs = &engine.notebook.cell(&id).unwrap().outputs;
        assert!(!outputs.is_empty());
        assert!(outputs.iter().any(|o| o.output_type == OutputType::Value));
    }

    #[test]
    fn definitions_persist_across_cells() {
        let mut engine = test_engine();
        engine.create_and_eval("(define x 42)").unwrap();
        let (_, result) = engine.create_and_eval("x").unwrap();
        assert_eq!(result.output.display, "42");
    }

    #[test]
    fn eval_error_produces_error_output() {
        let mut engine = test_engine();
        let (_, result) = engine.create_and_eval("(undefined-fn)").unwrap();
        assert_eq!(result.output.output_type, OutputType::Error);
        assert!(!result.output.display.is_empty());
    }

    #[test]
    fn eval_cell_not_found() {
        let mut engine = test_engine();
        let result = engine.eval_cell("nonexistent");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Cell not found"));
    }

    #[test]
    fn eval_markdown_cell_rejected() {
        let mut engine = test_engine();
        let id = engine.notebook.add_markdown_cell("# Hello");
        let result = engine.eval_cell(&id);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("markdown"));
    }

    #[test]
    fn eval_all_runs_all_code_cells() {
        let mut engine = test_engine();
        engine.notebook.add_code_cell("(define a 1)");
        engine.notebook.add_markdown_cell("text");
        engine.notebook.add_code_cell("(+ a 2)");

        let results = engine.eval_all();
        assert_eq!(results.len(), 2); // only code cells
        assert!(results[0].1.is_ok());
        assert!(results[1].1.is_ok());
        assert_eq!(results[1].1.as_ref().unwrap().output.display, "3");
    }

    #[test]
    fn eval_marks_downstream_stale() {
        let mut engine = test_engine();
        let id1 = engine.notebook.add_code_cell("(define x 1)");
        let id2 = engine.notebook.add_code_cell("x");

        // Eval both cells
        engine.eval_cell(&id1).unwrap();
        engine.eval_cell(&id2).unwrap();
        assert!(!engine.notebook.cell(&id2).unwrap().stale);

        // Re-eval cell 1 should mark cell 2 as stale
        engine.eval_cell(&id1).unwrap();
        assert!(engine.notebook.cell(&id2).unwrap().stale);
    }

    #[test]
    fn reset_clears_state() {
        let mut engine = test_engine();
        engine.create_and_eval("(define x 1)").unwrap();
        assert!(!engine.notebook.cells.is_empty());

        engine.reset();
        // Cells remain but outputs are cleared
        assert!(!engine.notebook.cells.is_empty());
        assert!(engine.notebook.cells[0].outputs.is_empty());

        // x should be unbound after reset
        let (_, result) = engine.create_and_eval("x").unwrap();
        assert_eq!(result.output.output_type, OutputType::Error);
    }

    #[test]
    fn nil_displays_as_empty() {
        assert_eq!(format_value_for_display(&Value::nil()), "");
    }

    #[test]
    fn opaque_values_require_reeval() {
        let mut engine = test_engine();
        let (_, result) = engine.create_and_eval("(fn (x) x)").unwrap();
        assert!(result.output.requires_reeval);
    }

    #[test]
    fn non_opaque_values_do_not_require_reeval() {
        let mut engine = test_engine();
        let (_, result) = engine.create_and_eval("42").unwrap();
        assert!(!result.output.requires_reeval);
    }

    // ── Undo tests ──────────────────────────────────────────────

    #[test]
    fn undo_restores_env_after_define() {
        let mut engine = test_engine();
        engine.create_and_eval("(define x 42)").unwrap();
        engine.undo_last_cell().unwrap();

        // x should be unbound now
        let (_, result) = engine.create_and_eval("x").unwrap();
        assert_eq!(result.output.output_type, OutputType::Error);
    }

    #[test]
    fn undo_restores_env_after_set_bang() {
        let mut engine = test_engine();
        engine.create_and_eval("(define x 1)").unwrap();

        // Overwrite x — this creates a new snapshot
        let id2 = engine.notebook.add_code_cell("(set! x 999)");
        engine.eval_cell(&id2).unwrap();

        // Undo the set! — x should be back to 1
        engine.undo_last_cell().unwrap();
        let (_, result) = engine.create_and_eval("x").unwrap();
        assert_eq!(result.output.display, "1");
    }

    #[test]
    fn undo_clears_errored_cell_output() {
        let mut engine = test_engine();
        let (id, _) = engine.create_and_eval("(bad-fn)").unwrap();
        assert!(!engine.notebook.cell(&id).unwrap().outputs.is_empty());

        engine.undo_last_cell().unwrap();
        assert!(engine.notebook.cell(&id).unwrap().outputs.is_empty());
    }

    #[test]
    fn undo_when_nothing_to_undo_errors() {
        let mut engine = test_engine();
        let result = engine.undo_last_cell();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Nothing to undo"));
    }

    #[test]
    fn undo_only_available_once() {
        let mut engine = test_engine();
        engine.create_and_eval("(define x 1)").unwrap();
        engine.undo_last_cell().unwrap();

        // Second undo should fail
        let result = engine.undo_last_cell();
        assert!(result.is_err());
    }

    #[test]
    fn new_eval_replaces_snapshot() {
        let mut engine = test_engine();
        engine.create_and_eval("(define a 1)").unwrap();
        engine.create_and_eval("(define b 2)").unwrap();

        // Undo should restore state before (define b 2), not before (define a 1)
        engine.undo_last_cell().unwrap();
        let (_, result) = engine.create_and_eval("a").unwrap();
        assert_eq!(result.output.display, "1"); // a is still defined

        // But b should be gone (after another undo for the eval we just did)
        engine.undo_last_cell().unwrap();
        let (_, result) = engine.create_and_eval("b").unwrap();
        assert_eq!(result.output.output_type, OutputType::Error);
    }

    #[test]
    fn can_undo_reflects_state() {
        let mut engine = test_engine();
        assert!(!engine.can_undo());

        engine.create_and_eval("(+ 1 2)").unwrap();
        assert!(engine.can_undo());

        engine.undo_last_cell().unwrap();
        assert!(!engine.can_undo());
    }

    #[test]
    fn reset_clears_undo() {
        let mut engine = test_engine();
        engine.create_and_eval("(define x 1)").unwrap();
        assert!(engine.can_undo());

        engine.reset();
        assert!(!engine.can_undo());
        assert!(engine.undo_last_cell().is_err());
    }
}
