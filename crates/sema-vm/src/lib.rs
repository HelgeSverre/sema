#![allow(clippy::mutable_key_type)]
pub mod chunk;
pub mod compiler;
pub mod core_expr;
pub mod debug;
pub mod disasm;
pub mod emit;
pub mod lower;
pub mod opcodes;
pub mod optimize;
pub mod resolve;
pub mod scheduler;
pub mod serialize;
pub mod vm;

pub use chunk::{Chunk, ExceptionEntry, Function, UpvalueDesc};
pub use compiler::{compile, CompileResult};
pub use core_expr::{
    CoreExpr, DoLoop, DoVar, Expr, LambdaDef, PromptEntry, ResolvedExpr, VarRef, VarResolution,
};
pub use debug::{
    decode_scope_ref, scope_locals_ref, scope_upvalues_ref, DebugCommand, DebugEvent, DebugState,
    ScopeKind, StepMode, StopInfo, StopReason, VmExecResult,
};
pub use disasm::disassemble;
pub use emit::Emitter;
pub use lower::lower;
pub use opcodes::Op;
pub use optimize::optimize as optimize_expr;
pub use resolve::resolve_with_locals;
pub use scheduler::init_scheduler;
pub use serialize::{deserialize_from_bytes, is_bytecode_file, serialize_to_bytes};
pub use vm::{
    compile_program, compile_program_with_spans, extract_vm_closure, snap_breakpoint_line,
    valid_breakpoint_lines, Closure, CompiledProgram, UpvalueCell, UpvalueState, VM,
};
