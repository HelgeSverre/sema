#![allow(clippy::mutable_key_type)]
pub mod lexer;
mod reader;

pub use reader::read;
pub use reader::read_many;
pub use reader::read_many_with_spans;
