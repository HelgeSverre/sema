//! Rendering helpers for notebook output.
//!
//! This module provides UI-agnostic rendering primitives that convert
//! notebook data structures into display-ready formats. These helpers
//! are designed to be reusable across different UI implementations
//! (browser, terminal, export formats, etc.).

use crate::format::{Cell, CellOutput, CellType, Notebook, OutputType};
use std::collections::BTreeMap;

// ── Structured output types for UI consumption ───────────────────

/// A fully rendered cell, ready for a UI to display.
#[derive(Debug, Clone, serde::Serialize)]
pub struct RenderedCell {
    pub id: String,
    pub cell_type: CellType,
    pub source: String,
    pub rendered_outputs: Vec<RenderedOutput>,
    pub stale: bool,
    pub cell_number: Option<usize>,
}

/// A rendered output block.
#[derive(Debug, Clone, serde::Serialize)]
pub struct RenderedOutput {
    pub output_type: OutputType,
    /// The display content (may be plain text, HTML, etc. depending on renderer).
    pub content: String,
    /// MIME type hint for the content.
    pub mime_type: String,
    /// Metadata about the output (duration, cost, etc.).
    pub meta: OutputMeta,
}

/// Metadata about a cell output.
#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct OutputMeta {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cost_usd: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timestamp: Option<String>,
    pub requires_reeval: bool,
}

// ── Rendering functions ──────────────────────────────────────────

/// Render all cells in a notebook into UI-ready structures.
pub fn render_notebook(notebook: &Notebook) -> Vec<RenderedCell> {
    let mut code_counter = 0usize;
    notebook
        .cells
        .iter()
        .map(|cell| {
            let cell_number = if cell.cell_type == CellType::Code {
                code_counter += 1;
                Some(code_counter)
            } else {
                None
            };
            render_cell(cell, cell_number)
        })
        .collect()
}

/// Render a single cell.
pub fn render_cell(cell: &Cell, cell_number: Option<usize>) -> RenderedCell {
    let rendered_outputs = cell.outputs.iter().map(render_output).collect();
    RenderedCell {
        id: cell.id.clone(),
        cell_type: cell.cell_type,
        source: cell.source.clone(),
        rendered_outputs,
        stale: cell.stale,
        cell_number,
    }
}

/// Render a single output.
pub fn render_output(output: &CellOutput) -> RenderedOutput {
    let (content, mime_type) = match output.output_type {
        OutputType::Value => {
            let content = if output.display.is_empty() {
                String::new()
            } else {
                output.display.clone()
            };
            (content, "text/plain".to_string())
        }
        OutputType::Error => (output.display.clone(), "text/x-sema-error".to_string()),
        OutputType::Stdout => (output.display.clone(), "text/plain".to_string()),
    };

    RenderedOutput {
        output_type: output.output_type,
        content,
        mime_type,
        meta: OutputMeta {
            duration_ms: output.duration_ms,
            cost_usd: output.cost_usd,
            timestamp: Some(output.timestamp.to_rfc3339()),
            requires_reeval: output.requires_reeval,
        },
    }
}

// ── Markdown export ──────────────────────────────────────────────

/// Export a notebook to Markdown format.
pub fn export_markdown(notebook: &Notebook) -> String {
    let mut out = String::new();

    if !notebook.metadata.title.is_empty() {
        out.push_str(&format!("# {}\n\n", notebook.metadata.title));
    }

    let mut code_counter = 0usize;
    for cell in &notebook.cells {
        match cell.cell_type {
            CellType::Markdown => {
                out.push_str(&cell.source);
                out.push_str("\n\n");
            }
            CellType::Code => {
                code_counter += 1;
                out.push_str(&format!("```sema\n{}\n```\n\n", cell.source));

                for output in &cell.outputs {
                    match output.output_type {
                        OutputType::Value if !output.display.is_empty() => {
                            out.push_str(&format!(
                                "**Out [{}]:**\n```\n{}\n```\n\n",
                                code_counter, output.display
                            ));
                        }
                        OutputType::Error => {
                            out.push_str(&format!(
                                "**Error [{}]:**\n```\n{}\n```\n\n",
                                code_counter, output.display
                            ));
                        }
                        OutputType::Stdout if !output.display.is_empty() => {
                            out.push_str(&format!(
                                "**Stdout [{}]:**\n```\n{}\n```\n\n",
                                code_counter, output.display
                            ));
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    out
}

// ── JSON API response types ──────────────────────────────────────

/// API response for a cell evaluation.
#[derive(Debug, Clone, serde::Serialize)]
pub struct EvalResponse {
    pub id: String,
    pub output: RenderedOutput,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stdout: Option<String>,
}

/// API response for listing environment bindings.
#[derive(Debug, Clone, serde::Serialize)]
pub struct EnvResponse {
    pub bindings: BTreeMap<String, String>,
}

/// API response for listing all cells.
#[derive(Debug, Clone, serde::Serialize)]
pub struct NotebookResponse {
    pub title: String,
    pub cells: Vec<RenderedCell>,
}

/// Produce the full notebook response for the API.
pub fn notebook_response(notebook: &Notebook) -> NotebookResponse {
    NotebookResponse {
        title: notebook.metadata.title.clone(),
        cells: render_notebook(notebook),
    }
}
