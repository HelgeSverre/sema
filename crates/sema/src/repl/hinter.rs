use nu_ansi_term::{Color, Style};
use reedline::{DefaultHinter, Hinter, History};

use super::completer::{collect_completions, COMPLETER_ENV};

/// Two-tier ghost-text hinter.
///
/// 1. **History-based** (primary): if any past entry shares the current
///    line as a prefix, show the unique suffix. Same behaviour as
///    `DefaultHinter`, which we delegate to.
/// 2. **Completion-based** (fallback): if history yields nothing, look
///    for a *unique* completion among env bindings / special forms /
///    `,commands`. Skip ambiguous cases entirely — ghost text that
///    changes every keystroke is noisier than no ghost text.
///
/// Reedline binds Right-arrow / Ctrl-F to accept the hint.
pub struct SemaHinter {
    inner: DefaultHinter,
    style: Style,
}

impl SemaHinter {
    pub fn new() -> Self {
        let style = Style::new().fg(Color::DarkGray);
        Self {
            inner: DefaultHinter::default().with_style(style),
            style,
        }
    }
}

impl Hinter for SemaHinter {
    fn handle(
        &mut self,
        line: &str,
        pos: usize,
        history: &dyn History,
        use_ansi_coloring: bool,
        cwd: &str,
    ) -> String {
        let hist = self
            .inner
            .handle(line, pos, history, use_ansi_coloring, cwd);
        if !hist.is_empty() {
            return hist;
        }

        // History had nothing. Try a unique completion.
        let Some(prefix) = current_word(line, pos) else {
            return String::new();
        };
        if prefix.is_empty() {
            return String::new();
        }

        let suggestions = COMPLETER_ENV.with(|cell| {
            let borrow = cell.borrow();
            borrow
                .as_ref()
                .map(|env| collect_completions(env, prefix))
                .unwrap_or_default()
        });

        if suggestions.len() != 1 {
            return String::new();
        }
        let suffix = suggestions[0].strip_prefix(prefix).unwrap_or("");
        if suffix.is_empty() {
            return String::new();
        }
        if use_ansi_coloring {
            self.style.paint(suffix).to_string()
        } else {
            suffix.to_string()
        }
    }

    fn complete_hint(&self) -> String {
        // Reedline asks for the unstyled hint text when the user presses
        // accept (Right-arrow). We can't recompute the suggestion here
        // (no `line` argument), so delegate to the inner history hinter
        // — the history path is what users hit most often anyway.
        self.inner.complete_hint()
    }

    fn next_hint_token(&self) -> String {
        self.inner.next_hint_token()
    }
}

/// Word at the cursor for hint lookup. Matches the completer's notion of
/// a "word boundary" (whitespace / opening bracket / quote).
fn current_word(line: &str, pos: usize) -> Option<&str> {
    if pos > line.len() {
        return None;
    }
    let before = &line[..pos];
    let start = before
        .rfind(|c: char| c.is_whitespace() || c == '(' || c == '[' || c == '{' || c == '\'')
        .map(|i| i + 1)
        .unwrap_or(0);
    Some(&before[start..])
}
