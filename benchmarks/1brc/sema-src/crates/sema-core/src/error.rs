use std::collections::HashMap;
use std::fmt;

use crate::value::Value;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// A single frame in a call stack trace.
#[derive(Debug, Clone)]
pub struct CallFrame {
    pub name: String,
    pub file: Option<std::path::PathBuf>,
    pub span: Option<Span>,
}

/// A captured stack trace (list of call frames, innermost first).
#[derive(Debug, Clone)]
pub struct StackTrace(pub Vec<CallFrame>);

impl fmt::Display for StackTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for frame in &self.0 {
            write!(f, "  at {}", frame.name)?;
            match (&frame.file, &frame.span) {
                (Some(file), Some(span)) => writeln!(f, " ({}:{span})", file.display())?,
                (Some(file), None) => writeln!(f, " ({})", file.display())?,
                (None, Some(span)) => writeln!(f, " (<input>:{span})")?,
                (None, None) => writeln!(f)?,
            }
        }
        Ok(())
    }
}

/// Maps Rc pointer addresses to source spans for expression tracking.
pub type SpanMap = HashMap<usize, Span>;

#[derive(Debug, Clone, thiserror::Error)]
pub enum SemaError {
    #[error("Reader error at {span}: {message}")]
    Reader { message: String, span: Span },

    #[error("Eval error: {0}")]
    Eval(String),

    #[error("Type error: expected {expected}, got {got}")]
    Type { expected: String, got: String },

    #[error("Arity error: {name} expects {expected} args, got {got}")]
    Arity {
        name: String,
        expected: String,
        got: usize,
    },

    #[error("Unbound variable: {0}")]
    Unbound(String),

    #[error("LLM error: {0}")]
    Llm(String),

    #[error("IO error: {0}")]
    Io(String),

    #[error("User exception: {0}")]
    UserException(Value),

    #[error("{inner}")]
    WithTrace {
        inner: Box<SemaError>,
        trace: StackTrace,
    },
}

impl SemaError {
    pub fn eval(msg: impl Into<String>) -> Self {
        SemaError::Eval(msg.into())
    }

    pub fn type_error(expected: impl Into<String>, got: impl Into<String>) -> Self {
        SemaError::Type {
            expected: expected.into(),
            got: got.into(),
        }
    }

    pub fn arity(name: impl Into<String>, expected: impl Into<String>, got: usize) -> Self {
        SemaError::Arity {
            name: name.into(),
            expected: expected.into(),
            got,
        }
    }

    /// Wrap this error with a stack trace (no-op if already wrapped).
    pub fn with_stack_trace(self, trace: StackTrace) -> Self {
        if trace.0.is_empty() {
            return self;
        }
        match self {
            SemaError::WithTrace { .. } => self,
            other => SemaError::WithTrace {
                inner: Box::new(other),
                trace,
            },
        }
    }

    pub fn stack_trace(&self) -> Option<&StackTrace> {
        match self {
            SemaError::WithTrace { trace, .. } => Some(trace),
            _ => None,
        }
    }

    pub fn inner(&self) -> &SemaError {
        match self {
            SemaError::WithTrace { inner, .. } => inner,
            other => other,
        }
    }
}
