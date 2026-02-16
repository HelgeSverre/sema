#![allow(clippy::mutable_key_type)]
pub mod chunk;
pub mod core_expr;
pub mod disasm;
pub mod emit;
pub mod lower;
pub mod opcodes;

pub use chunk::{Chunk, ExceptionEntry, Function, UpvalueDesc};
pub use core_expr::{CoreExpr, DoLoop, DoVar, LambdaDef, PromptEntry};
pub use disasm::disassemble;
pub use emit::Emitter;
pub use lower::lower;
pub use opcodes::Op;
