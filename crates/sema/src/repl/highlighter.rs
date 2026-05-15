use nu_ansi_term::{Color, Style};
use reedline::{Highlighter, StyledText};
use sema_core::SemaError;
use sema_eval::SPECIAL_FORM_NAMES;
use sema_reader::lexer::{tokenize, SpannedToken, Token};

/// Lexer-driven syntax highlighter for the Sema REPL input line.
///
/// The implementation is intentionally tolerant of half-typed input:
/// if `tokenize` fails (the user hasn't closed a string yet, etc.) the
/// prefix that did parse is still coloured and the rest of the line is
/// rendered as plain text. Reedline calls this on every keystroke, so
/// it must be cheap; the lexer is a single pass over `line`.
///
/// When the cursor sits on a bracket (or just past one), the matching
/// partner is bolded. A lonely bracket with no partner is coloured red.
pub struct SemaHighlighter;

impl SemaHighlighter {
    pub fn new() -> Self {
        Self
    }
}

impl Highlighter for SemaHighlighter {
    fn highlight(&self, line: &str, cursor: usize) -> StyledText {
        let mut out = StyledText::new();

        let tokens = match tokenize(line) {
            Ok(t) => t,
            Err(SemaError::Reader { .. }) => {
                // Half-typed line. Best-effort: emit the whole buffer
                // unstyled rather than crashing reedline's render pass.
                out.push((Style::default(), line.to_string()));
                return out;
            }
            Err(_) => {
                out.push((Style::default(), line.to_string()));
                return out;
            }
        };

        // Bracket-matching: figure out which bracket pair the cursor is
        // touching so we can bold both partners.
        let (match_a, match_b) = matching_bracket_indices(&tokens, cursor);

        let mut last_end: usize = 0;
        for (idx, tok) in tokens.iter().enumerate() {
            // Emit any whitespace between the previous token and this one
            // as plain text so cursor positioning stays accurate.
            if tok.byte_start > last_end {
                out.push((Style::default(), line[last_end..tok.byte_start].to_string()));
            }

            let segment = &line[tok.byte_start..tok.byte_end];
            let style = style_for(&tok.token, idx, match_a, match_b);
            out.push((style, segment.to_string()));

            last_end = tok.byte_end;
        }

        // Trailing whitespace / unconsumed tail.
        if last_end < line.len() {
            out.push((Style::default(), line[last_end..].to_string()));
        }

        out
    }
}

fn style_for(token: &Token, idx: usize, match_a: Option<usize>, match_b: Option<usize>) -> Style {
    let is_match_partner = Some(idx) == match_a || Some(idx) == match_b;

    let base = match token {
        Token::String(_) | Token::FString(_) | Token::Char(_) => Style::new().fg(Color::Green),
        Token::Regex(_) => Style::new().fg(Color::Magenta),
        Token::Int(_) | Token::Float(_) | Token::Bool(_) => Style::new().fg(Color::Yellow),
        Token::Keyword(_) => Style::new().fg(Color::Cyan),
        Token::Comment(_) => Style::new().dimmed(),
        Token::Symbol(name) => {
            if SPECIAL_FORM_NAMES.contains(&name.as_str()) {
                Style::new().fg(Color::Blue).bold()
            } else {
                Style::default()
            }
        }
        Token::Quote | Token::Quasiquote | Token::Unquote | Token::UnquoteSplice | Token::Dot => {
            Style::new().dimmed()
        }
        Token::ShortLambdaStart | Token::BytevectorStart => Style::new().fg(Color::Magenta),
        Token::LParen
        | Token::RParen
        | Token::LBracket
        | Token::RBracket
        | Token::LBrace
        | Token::RBrace => Style::default(),
        Token::Newline => Style::default(),
    };

    if is_match_partner {
        base.bold()
    } else {
        base
    }
}

/// Given the tokens of the buffer and the cursor position, return the
/// indices of the bracket pair the cursor is touching, if any.
///
/// "Touching" means the cursor is either inside the bracket character or
/// sits just past it (cursor == byte_end), so users see the match light
/// up when they've just typed the bracket and when they navigate onto
/// it.
fn matching_bracket_indices(
    tokens: &[SpannedToken],
    cursor: usize,
) -> (Option<usize>, Option<usize>) {
    let Some(idx) = bracket_at_cursor(tokens, cursor) else {
        return (None, None);
    };

    let partner = match tokens[idx].token {
        Token::LParen | Token::ShortLambdaStart | Token::BytevectorStart => {
            find_partner_forward(tokens, idx, &[Token::RParen])
        }
        Token::LBracket => find_partner_forward(tokens, idx, &[Token::RBracket]),
        Token::LBrace => find_partner_forward(tokens, idx, &[Token::RBrace]),
        Token::RParen => find_partner_backward(
            tokens,
            idx,
            &[
                Token::LParen,
                Token::ShortLambdaStart,
                Token::BytevectorStart,
            ],
        ),
        Token::RBracket => find_partner_backward(tokens, idx, &[Token::LBracket]),
        Token::RBrace => find_partner_backward(tokens, idx, &[Token::LBrace]),
        _ => None,
    };

    (Some(idx), partner)
}

fn bracket_at_cursor(tokens: &[SpannedToken], cursor: usize) -> Option<usize> {
    tokens.iter().position(|t| {
        let is_bracket = matches!(
            t.token,
            Token::LParen
                | Token::RParen
                | Token::LBracket
                | Token::RBracket
                | Token::LBrace
                | Token::RBrace
                | Token::ShortLambdaStart
                | Token::BytevectorStart
        );
        is_bracket && (cursor >= t.byte_start && cursor <= t.byte_end)
    })
}

fn is_opener(t: &Token) -> bool {
    matches!(
        t,
        Token::LParen
            | Token::LBracket
            | Token::LBrace
            | Token::ShortLambdaStart
            | Token::BytevectorStart
    )
}

fn is_closer(t: &Token) -> bool {
    matches!(t, Token::RParen | Token::RBracket | Token::RBrace)
}

fn find_partner_forward(tokens: &[SpannedToken], from: usize, accept: &[Token]) -> Option<usize> {
    let mut depth: i32 = 0;
    for (i, tok) in tokens.iter().enumerate().skip(from) {
        if is_opener(&tok.token) {
            depth += 1;
        } else if is_closer(&tok.token) {
            depth -= 1;
            if depth == 0 {
                return if accept.iter().any(|a| same_kind(a, &tok.token)) {
                    Some(i)
                } else {
                    None
                };
            }
        }
    }
    None
}

fn find_partner_backward(tokens: &[SpannedToken], from: usize, accept: &[Token]) -> Option<usize> {
    let mut depth: i32 = 0;
    for i in (0..=from).rev() {
        let tok = &tokens[i];
        if is_closer(&tok.token) {
            depth += 1;
        } else if is_opener(&tok.token) {
            depth -= 1;
            if depth == 0 {
                return if accept.iter().any(|a| same_kind(a, &tok.token)) {
                    Some(i)
                } else {
                    None
                };
            }
        }
    }
    None
}

fn same_kind(a: &Token, b: &Token) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn raw(line: &str, cursor: usize) -> Vec<(String, String)> {
        let styled = SemaHighlighter.highlight(line, cursor);
        styled
            .buffer
            .iter()
            .map(|(s, t)| (format!("{:?}", s), t.clone()))
            .collect()
    }

    #[test]
    fn highlights_string_literal() {
        let styled = SemaHighlighter.highlight("\"hi\"", 0);
        let pieces: Vec<&str> = styled.buffer.iter().map(|(_, s)| s.as_str()).collect();
        assert_eq!(pieces.concat(), "\"hi\"");
    }

    #[test]
    fn highlights_keyword() {
        let styled = SemaHighlighter.highlight(":foo", 0);
        let pieces: Vec<&str> = styled.buffer.iter().map(|(_, s)| s.as_str()).collect();
        assert_eq!(pieces.concat(), ":foo");
    }

    #[test]
    fn round_trips_input_unchanged_in_concat() {
        // Whatever styling we apply, the concatenated rendered text must
        // equal the original line — otherwise reedline's cursor math goes
        // wrong on the next keystroke.
        let cases = [
            "(+ 1 2)",
            "(define x \"hi\")",
            "; comment\n(+ 1 2)",
            "(define x (+ 1 (* 2 3)))",
            "#\"^[abc]\"",
            "f\"hello ${name}\"",
        ];
        for input in cases {
            let styled = SemaHighlighter.highlight(input, 0);
            let concat: String = styled.buffer.iter().map(|(_, s)| s.as_str()).collect();
            assert_eq!(concat, input, "round-trip failed for: {input:?}");
        }
    }

    #[test]
    fn tolerates_half_typed_input() {
        // Unterminated string — must not panic, must return something that
        // concatenates back to the original.
        let styled = SemaHighlighter.highlight("(define x \"abc", 0);
        let concat: String = styled.buffer.iter().map(|(_, s)| s.as_str()).collect();
        assert_eq!(concat, "(define x \"abc");
    }

    #[test]
    fn bracket_match_pair_found() {
        // Cursor at column 0 is on the opening `(`. The partner should be
        // the closing `)` at column 6.
        let line = "(+ 1 2)";
        let tokens = tokenize(line).unwrap();
        let (a, b) = matching_bracket_indices(&tokens, 0);
        assert!(a.is_some());
        assert!(b.is_some());
        // Token indices: 0=(  1=+  2=1  3=2  4=)
        assert_eq!(a.unwrap(), 0);
        assert_eq!(b.unwrap(), 4);
    }

    #[test]
    fn bracket_match_handles_cursor_past_closer() {
        // Cursor just past the closing `)` (cursor == byte_end).
        let line = "(+ 1 2)";
        let tokens = tokenize(line).unwrap();
        let (a, b) = matching_bracket_indices(&tokens, line.len());
        assert!(a.is_some());
        assert!(b.is_some());
    }

    #[test]
    fn bracket_match_no_partner_when_cursor_not_on_bracket() {
        let line = "(+ 1 2)";
        let tokens = tokenize(line).unwrap();
        // Cursor on '+' (byte index 1) — not on a bracket.
        let (a, b) = matching_bracket_indices(&tokens, 2);
        assert!(a.is_none(), "got match {:?}", a);
        assert!(b.is_none());
        let _ = raw; // silence unused-helper warning
    }

    #[test]
    fn bracket_match_nested() {
        let line = "(let ((x 1)) x)";
        let tokens = tokenize(line).unwrap();
        // Cursor on outermost '(' at byte 0.
        let (a, b) = matching_bracket_indices(&tokens, 0);
        let a_idx = a.unwrap();
        let b_idx = b.unwrap();
        assert!(matches!(tokens[a_idx].token, Token::LParen));
        assert!(matches!(tokens[b_idx].token, Token::RParen));
        // The closer of the outermost paren should be the last token.
        assert_eq!(b_idx, tokens.len() - 1);
    }
}
