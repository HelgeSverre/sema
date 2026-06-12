//! Code formatter for Sema source files.
//!
//! The entry point is [`format_source`], configured via [`FormatOptions`]
//! (width / indent / alignment). It takes source text and returns formatted
//! source text; it never evaluates code.

mod formatter;

pub use formatter::{format_source, FormatOptions};
