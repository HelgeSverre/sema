#![allow(clippy::mutable_key_type)]
pub mod chunk;
pub mod compiler;
pub mod core_expr;
pub mod disasm;
pub mod emit;
pub mod lower;
pub mod opcodes;
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
pub use disasm::disassemble;
pub use emit::Emitter;
pub use lower::lower;
pub use opcodes::Op;
pub use resolve::{resolve, resolve_with_locals};
pub use vm::{compile_program, Closure, UpvalueCell, VM};
