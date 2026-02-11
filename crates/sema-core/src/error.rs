use std::fmt;

#[derive(Debug, Clone)]
pub struct Span {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

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
}
