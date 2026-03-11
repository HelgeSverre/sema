use std::path::PathBuf;

use sema_core::{Span, Spur, Value};

/// A compiled code object (bytecode + metadata).
#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub consts: Vec<Value>,
    pub spans: Vec<(u32, Span)>, // sparse PC → source span mapping (sorted by PC)
    pub max_stack: u16,
    pub n_locals: u16,
    pub exception_table: Vec<ExceptionEntry>,
    /// Number of per-instruction inline cache slots for global lookups.
    pub n_global_cache_slots: u16,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            consts: Vec::new(),
            spans: Vec::new(),
            max_stack: 0,
            n_locals: 0,
            exception_table: Vec::new(),
            n_global_cache_slots: 0,
        }
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ExceptionEntry {
    pub try_start: u32,
    pub try_end: u32,
    pub handler_pc: u32,
    pub stack_depth: u16,
    pub catch_slot: u16,
}

/// A compiled function (template for closures).
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Spur>,
    pub chunk: Chunk,
    pub upvalue_descs: Vec<UpvalueDesc>,
    pub arity: u16,
    pub has_rest: bool,
    pub local_names: Vec<(u16, Spur)>,
    pub source_file: Option<PathBuf>,
    /// Offset into the VM's inline_cache Vec where this function's cache slots begin.
    /// Assigned at VM creation time; not serialized.
    pub cache_offset: usize,
}

/// Describes how an upvalue is captured relative to the immediately enclosing function.
#[derive(Debug, Clone, Copy)]
pub enum UpvalueDesc {
    /// Capture from the parent function's local slot.
    ParentLocal(u16),
    /// Capture from the parent function's upvalue slot.
    ParentUpvalue(u16),
}
