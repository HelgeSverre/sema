#![allow(clippy::mutable_key_type)]
pub mod chunk;
pub mod opcodes;

pub use chunk::{Chunk, ExceptionEntry, Function, UpvalueDesc};
pub use opcodes::Op;
