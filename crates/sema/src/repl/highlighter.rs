use nu_ansi_term::Style;
use reedline::{Highlighter, StyledText};

/// Phase-0 placeholder: returns the input unstyled. Phase 1 replaces this
/// with the real lexer-driven syntax highlighter + bracket-match logic.
pub struct SemaHighlighter;

impl SemaHighlighter {
    pub fn new() -> Self {
        Self
    }
}

impl Highlighter for SemaHighlighter {
    fn highlight(&self, line: &str, _cursor: usize) -> StyledText {
        let mut out = StyledText::new();
        out.push((Style::default(), line.to_string()));
        out
    }
}
