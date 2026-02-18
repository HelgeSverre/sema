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

    #[error("Permission denied: {function} — path '{path}' is outside allowed directories")]
    PathDenied { function: String, path: String },

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Value;

    // 1. Span Display
    #[test]
    fn span_display() {
        let span = Span { line: 1, col: 5 };
        assert_eq!(span.to_string(), "1:5");
    }

    // 2. StackTrace Display — file+span, file only, span only, neither
    #[test]
    fn stack_trace_display() {
        let trace = StackTrace(vec![
            CallFrame {
                name: "foo".into(),
                file: Some("/a/b.sema".into()),
                span: Some(Span { line: 3, col: 7 }),
            },
            CallFrame {
                name: "bar".into(),
                file: Some("/c/d.sema".into()),
                span: None,
            },
            CallFrame {
                name: "baz".into(),
                file: None,
                span: Some(Span { line: 10, col: 1 }),
            },
            CallFrame {
                name: "qux".into(),
                file: None,
                span: None,
            },
        ]);
        let s = trace.to_string();
        assert!(s.contains("at foo (/a/b.sema:3:7)"));
        assert!(s.contains("at bar (/c/d.sema)"));
        assert!(s.contains("at baz (<input>:10:1)"));
        assert!(s.contains("at qux\n"));
    }

    // 3. SemaError::eval() constructor and Display
    #[test]
    fn eval_error() {
        let e = SemaError::eval("something broke");
        assert_eq!(e.to_string(), "Eval error: something broke");
    }

    // 4. SemaError::type_error() constructor and Display
    #[test]
    fn type_error() {
        let e = SemaError::type_error("string", "integer");
        assert_eq!(e.to_string(), "Type error: expected string, got integer");
    }

    // 5. SemaError::arity() constructor and Display
    #[test]
    fn arity_error() {
        let e = SemaError::arity("my-fn", "2", 5);
        assert_eq!(e.to_string(), "Arity error: my-fn expects 2 args, got 5");
    }

    // 6. with_hint attaches hint retrievable via .hint()
    #[test]
    fn with_hint() {
        let e = SemaError::eval("oops").with_hint("try this");
        assert_eq!(e.hint(), Some("try this"));
    }

    // 7. with_note attaches note retrievable via .note()
    #[test]
    fn with_note() {
        let e = SemaError::eval("oops").with_note("extra info");
        assert_eq!(e.note(), Some("extra info"));
    }

    // 8. with_hint on already-wrapped WithContext preserves note
    #[test]
    fn with_hint_preserves_note() {
        let e = SemaError::eval("oops")
            .with_note("kept note")
            .with_hint("new hint");
        assert_eq!(e.hint(), Some("new hint"));
        assert_eq!(e.note(), Some("kept note"));
    }

    // 9. with_note on already-wrapped WithContext preserves hint
    #[test]
    fn with_note_preserves_hint() {
        let e = SemaError::eval("oops")
            .with_hint("kept hint")
            .with_note("new note");
        assert_eq!(e.hint(), Some("kept hint"));
        assert_eq!(e.note(), Some("new note"));
    }

    // 10. with_stack_trace wraps in WithTrace, retrievable via .stack_trace()
    #[test]
    fn with_stack_trace() {
        let trace = StackTrace(vec![CallFrame {
            name: "f".into(),
            file: None,
            span: None,
        }]);
        let e = SemaError::eval("err").with_stack_trace(trace);
        let st = e.stack_trace().expect("should have stack trace");
        assert_eq!(st.0.len(), 1);
        assert_eq!(st.0[0].name, "f");
    }

    // 11. with_stack_trace with empty trace is no-op
    #[test]
    fn with_stack_trace_empty_is_noop() {
        let e = SemaError::eval("err").with_stack_trace(StackTrace(vec![]));
        assert!(e.stack_trace().is_none());
        assert!(matches!(e, SemaError::Eval(_)));
    }

    // 12. with_stack_trace on already-wrapped WithTrace is no-op
    #[test]
    fn with_stack_trace_already_wrapped_is_noop() {
        let frame = || CallFrame {
            name: "first".into(),
            file: None,
            span: None,
        };
        let e = SemaError::eval("err").with_stack_trace(StackTrace(vec![frame()]));
        let e2 = e.with_stack_trace(StackTrace(vec![CallFrame {
            name: "second".into(),
            file: None,
            span: None,
        }]));
        let st = e2.stack_trace().unwrap();
        assert_eq!(st.0.len(), 1);
        assert_eq!(st.0[0].name, "first");
    }

    // 13. inner() unwraps through WithTrace and WithContext
    #[test]
    fn inner_unwraps() {
        let e = SemaError::eval("root")
            .with_hint("h")
            .with_stack_trace(StackTrace(vec![CallFrame {
                name: "x".into(),
                file: None,
                span: None,
            }]));
        let inner = e.inner();
        assert!(matches!(inner, SemaError::Eval(msg) if msg == "root"));
    }

    // 14. hint() and note() return None on plain errors
    #[test]
    fn hint_note_none_on_plain() {
        let e = SemaError::eval("plain");
        assert!(e.hint().is_none());
        assert!(e.note().is_none());
    }

    // 15. check_arity! exact match passes, mismatch returns error
    #[test]
    fn check_arity_exact() {
        fn run(args: &[Value]) -> Result<(), SemaError> {
            check_arity!(args, "test-fn", 2);
            Ok(())
        }
        assert!(run(&[Value::nil(), Value::nil()]).is_ok());
        let err = run(&[Value::nil()]).unwrap_err();
        assert!(err.to_string().contains("test-fn"));
        assert!(err.to_string().contains("2"));
    }

    // 16. check_arity! range match (1..=3) passes and fails
    #[test]
    fn check_arity_range() {
        fn run(args: &[Value]) -> Result<(), SemaError> {
            check_arity!(args, "range-fn", 1..=3);
            Ok(())
        }
        assert!(run(&[Value::nil()]).is_ok());
        assert!(run(&[Value::nil(), Value::nil()]).is_ok());
        assert!(run(&[Value::nil(), Value::nil(), Value::nil()]).is_ok());
        assert!(run(&[]).is_err());
        assert!(run(&[Value::nil(), Value::nil(), Value::nil(), Value::nil()]).is_err());
    }

    // 17. check_arity! open range (2..) passes and fails
    #[test]
    fn check_arity_open_range() {
        fn run(args: &[Value]) -> Result<(), SemaError> {
            check_arity!(args, "open-fn", 2..);
            Ok(())
        }
        assert!(run(&[Value::nil(), Value::nil()]).is_ok());
        assert!(run(&[Value::nil(), Value::nil(), Value::nil()]).is_ok());
        assert!(run(&[Value::nil()]).is_err());
        assert!(run(&[]).is_err());
    }
}
