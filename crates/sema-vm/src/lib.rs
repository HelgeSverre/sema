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
pub mod serialize;
pub mod vm;

pub use chunk::{Chunk, ExceptionEntry, Function, UpvalueDesc};
pub use compiler::{
    compile, compile_many, compile_many_with_locals, compile_with_locals, CompileResult,
};
pub use core_expr::{
    CoreExpr, DoLoop, DoVar, LambdaDef, PromptEntry, ResolvedExpr, ResolvedLambda, VarRef,
    VarResolution,
};
pub use debug::{DebugCommand, DebugEvent, DebugState, StepMode, StopReason};
pub use disasm::disassemble;
pub use emit::Emitter;
pub use lower::{lower, lower_with_spans};
pub use opcodes::Op;
pub use optimize::optimize as optimize_expr;
pub use resolve::{resolve, resolve_with_locals};
pub use serialize::{deserialize_from_bytes, is_bytecode_file, serialize_to_bytes};
pub use vm::{compile_program, compile_program_with_spans, Closure, UpvalueCell, VM};
