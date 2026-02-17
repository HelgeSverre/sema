use std::collections::HashMap;
use std::fmt;

use crate::value::Value;

/// Check arity of a native function's arguments, returning `SemaError::Arity` on mismatch.
///
/// # Forms
///
/// ```ignore
/// check_arity!(args, "fn-name", 2);        // exactly 2
/// check_arity!(args, "fn-name", 1..=3);    // 1 to 3 inclusive
/// check_arity!(args, "fn-name", 2..);      // 2 or more
/// ```
#[macro_export]
macro_rules! check_arity {
    ($args:expr, $name:expr, $exact:literal) => {
        if $args.len() != $exact {
            return Err($crate::SemaError::arity(
                $name,
                stringify!($exact),
                $args.len(),
            ));
        }
    };
    ($args:expr, $name:expr, $lo:literal ..= $hi:literal) => {
        if $args.len() < $lo || $args.len() > $hi {
            return Err($crate::SemaError::arity(
                $name,
                concat!(stringify!($lo), "-", stringify!($hi)),
                $args.len(),
            ));
        }
    };
    ($args:expr, $name:expr, $lo:literal ..) => {
        if $args.len() < $lo {
            return Err($crate::SemaError::arity(
                $name,
                concat!(stringify!($lo), "+"),
                $args.len(),
            ));
        }
    };
}

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

    #[error("Permission denied: {function} requires '{capability}' capability")]
    PermissionDenied {
        function: String,
        capability: String,
    },

    #[error("User exception: {0}")]
    UserException(Value),

    #[error("{inner}")]
    WithTrace {
        inner: Box<SemaError>,
        trace: StackTrace,
    },

    #[error("{inner}")]
    WithContext {
        inner: Box<SemaError>,
        hint: Option<String>,
        note: Option<String>,
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

    /// Attach a hint (actionable suggestion) to this error.
    pub fn with_hint(self, hint: impl Into<String>) -> Self {
        match self {
            SemaError::WithContext { inner, note, .. } => SemaError::WithContext {
                inner,
                hint: Some(hint.into()),
                note,
            },
            other => SemaError::WithContext {
                inner: Box::new(other),
                hint: Some(hint.into()),
                note: None,
            },
        }
    }

    /// Attach a note (extra context) to this error.
    pub fn with_note(self, note: impl Into<String>) -> Self {
        match self {
            SemaError::WithContext { inner, hint, .. } => SemaError::WithContext {
                inner,
                hint,
                note: Some(note.into()),
            },
            other => SemaError::WithContext {
                inner: Box::new(other),
                hint: None,
                note: Some(note.into()),
            },
        }
    }

    /// Get the hint from this error, if any.
    pub fn hint(&self) -> Option<&str> {
        match self {
            SemaError::WithContext { hint, .. } => hint.as_deref(),
            SemaError::WithTrace { inner, .. } => inner.hint(),
            _ => None,
        }
    }

    /// Get the note from this error, if any.
    pub fn note(&self) -> Option<&str> {
        match self {
            SemaError::WithContext { note, .. } => note.as_deref(),
            SemaError::WithTrace { inner, .. } => inner.note(),
            _ => None,
        }
    }

    /// Wrap this error with a stack trace (no-op if already wrapped).
    pub fn with_stack_trace(self, trace: StackTrace) -> Self {
        if trace.0.is_empty() {
            return self;
        }
        match self {
            SemaError::WithTrace { .. } => self,
            SemaError::WithContext { inner, hint, note } => SemaError::WithContext {
                inner: Box::new(inner.with_stack_trace(trace)),
                hint,
                note,
            },
            other => SemaError::WithTrace {
                inner: Box::new(other),
                trace,
            },
        }
    }

    pub fn stack_trace(&self) -> Option<&StackTrace> {
        match self {
            SemaError::WithTrace { trace, .. } => Some(trace),
            SemaError::WithContext { inner, .. } => inner.stack_trace(),
            _ => None,
        }
    }

    pub fn inner(&self) -> &SemaError {
        match self {
            SemaError::WithTrace { inner, .. } => inner.inner(),
            SemaError::WithContext { inner, .. } => inner.inner(),
            other => other,
        }
    }
}
