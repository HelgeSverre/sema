//! Notebook file format (.sema-nb) — JSON-based cell notebook format.
//!
//! This module defines the serializable types that make up a `.sema-nb` file.
//! The format stores both source code and evaluated outputs, enabling
//! "hydration" (restoring state) without re-evaluation.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

/// Top-level notebook document.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Notebook {
    /// Format version (currently 1).
    pub version: u32,
    /// Notebook metadata.
    pub metadata: NotebookMetadata,
    /// Ordered list of cells.
    pub cells: Vec<Cell>,
}

/// Notebook-level metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NotebookMetadata {
    /// Human-readable title.
    #[serde(default)]
    pub title: String,
    /// When the notebook was created.
    #[serde(default = "now")]
    pub created: DateTime<Utc>,
    /// Last modification time.
    #[serde(default = "now")]
    pub modified: DateTime<Utc>,
    /// Sema version used to create/last-evaluate the notebook.
    #[serde(default)]
    pub sema_version: String,
}

fn now() -> DateTime<Utc> {
    Utc::now()
}

/// A single cell in the notebook.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Cell {
    /// Unique cell identifier.
    pub id: String,
    /// Cell type.
    #[serde(rename = "type")]
    pub cell_type: CellType,
    /// Source content (Sema code or Markdown).
    pub source: String,
    /// Evaluation outputs (empty for markdown cells or unevaluated code cells).
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub outputs: Vec<CellOutput>,
    /// Whether this cell's output is stale (upstream cell was re-evaluated).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub stale: bool,
}

/// The type of a cell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CellType {
    Code,
    Markdown,
}

/// Output produced by evaluating a code cell.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellOutput {
    /// The kind of output.
    #[serde(rename = "type")]
    pub output_type: OutputType,
    /// Human-readable display string.
    pub display: String,
    /// Round-trippable S-expression form of the value.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sema_value: Option<String>,
    /// Variables defined or mutated by this cell.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub bindings: BTreeMap<String, String>,
    /// When this output was produced.
    pub timestamp: DateTime<Utc>,
    /// Estimated LLM cost in USD, if applicable.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cost_usd: Option<f64>,
    /// Whether this value requires re-evaluation (e.g. file handles).
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub requires_reeval: bool,
    /// Duration of evaluation in milliseconds.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<u64>,
}

/// Output type discriminator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum OutputType {
    Value,
    Error,
    Stdout,
}

impl Notebook {
    /// Create a new empty notebook with the given title.
    pub fn new(title: &str) -> Self {
        let now = Utc::now();
        Self {
            version: 1,
            metadata: NotebookMetadata {
                title: title.to_string(),
                created: now,
                modified: now,
                sema_version: String::new(),
            },
            cells: Vec::new(),
        }
    }

    /// Load a notebook from a `.sema-nb` JSON file.
    pub fn load(path: &Path) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read {}: {e}", path.display()))?;
        serde_json::from_str(&content)
            .map_err(|e| format!("Failed to parse {}: {e}", path.display()))
    }

    /// Save the notebook to a `.sema-nb` JSON file.
    pub fn save(&mut self, path: &Path) -> Result<(), String> {
        self.metadata.modified = Utc::now();
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| format!("Failed to serialize notebook: {e}"))?;
        std::fs::write(path, json).map_err(|e| format!("Failed to write {}: {e}", path.display()))
    }

    /// Add a new code cell and return its ID.
    pub fn add_code_cell(&mut self, source: &str) -> String {
        let id = new_cell_id();
        self.cells.push(Cell {
            id: id.clone(),
            cell_type: CellType::Code,
            source: source.to_string(),
            outputs: Vec::new(),
            stale: false,
        });
        id
    }

    /// Add a new markdown cell and return its ID.
    pub fn add_markdown_cell(&mut self, source: &str) -> String {
        let id = new_cell_id();
        self.cells.push(Cell {
            id: id.clone(),
            cell_type: CellType::Markdown,
            source: source.to_string(),
            outputs: Vec::new(),
            stale: false,
        });
        id
    }

    /// Find a cell by ID, returning a mutable reference.
    pub fn cell_mut(&mut self, id: &str) -> Option<&mut Cell> {
        self.cells.iter_mut().find(|c| c.id == id)
    }

    /// Find a cell by ID.
    pub fn cell(&self, id: &str) -> Option<&Cell> {
        self.cells.iter().find(|c| c.id == id)
    }

    /// Find the index of a cell by ID.
    pub fn cell_index(&self, id: &str) -> Option<usize> {
        self.cells.iter().position(|c| c.id == id)
    }

    /// Mark all cells after `index` as stale.
    pub fn mark_downstream_stale(&mut self, index: usize) {
        for cell in self.cells.iter_mut().skip(index + 1) {
            if cell.cell_type == CellType::Code && !cell.outputs.is_empty() {
                cell.stale = true;
            }
        }
    }
}

/// Generate a short unique cell ID.
fn new_cell_id() -> String {
    let uuid = uuid::Uuid::new_v4();
    // Use first 8 hex chars for brevity
    format!("c{}", &uuid.simple().to_string()[..8])
}
