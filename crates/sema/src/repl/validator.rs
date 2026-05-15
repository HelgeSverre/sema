use reedline::{ValidationResult, Validator};
use sema_core::SemaError;
use sema_reader::lexer::{tokenize, Token};

/// Decide whether the current input is complete enough to submit, or whether
/// reedline should keep reading more lines.
///
/// Uses the real lexer so strings and comments containing brackets are
/// handled correctly (the previous homegrown `is_balanced` only tracked
/// `"` and didn't know about `;` comments or `#"..."` regex literals).
pub struct SemaValidator;

impl Validator for SemaValidator {
    fn validate(&self, line: &str) -> ValidationResult {
        match tokenize(line) {
            Err(SemaError::Reader { message, .. }) if is_unterminated(&message) => {
                ValidationResult::Incomplete
            }
            // Any other reader error: submit so the evaluator can produce a
            // proper diagnostic via print_error. Blocking submission here
            // would leave the user stuck with no feedback.
            Err(_) => ValidationResult::Complete,
            Ok(tokens) => {
                let mut depth: i32 = 0;
                for t in &tokens {
                    match t.token {
                        Token::LParen
                        | Token::LBracket
                        | Token::LBrace
                        | Token::ShortLambdaStart
                        | Token::BytevectorStart => depth += 1,
                        Token::RParen | Token::RBracket | Token::RBrace => depth -= 1,
                        _ => {}
                    }
                }
                if depth > 0 {
                    ValidationResult::Incomplete
                } else {
                    ValidationResult::Complete
                }
            }
        }
    }
}

fn is_unterminated(msg: &str) -> bool {
    msg.contains("unterminated")
}

/// Shared "is this a full top-level form?" check used by both the
/// reedline-backed REPL (via the `Validator` impl above) and the
/// non-TTY headless fallback.
pub fn is_input_complete(line: &str) -> bool {
    matches!(SemaValidator.validate(line), ValidationResult::Complete)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check(input: &str) -> ValidationResult {
        SemaValidator.validate(input)
    }

    fn is_complete(r: ValidationResult) -> bool {
        matches!(r, ValidationResult::Complete)
    }

    fn is_incomplete(r: ValidationResult) -> bool {
        matches!(r, ValidationResult::Incomplete)
    }

    #[test]
    fn complete_simple() {
        assert!(is_complete(check("(+ 1 2)")));
        assert!(is_complete(check("42")));
        assert!(is_complete(check("")));
        assert!(is_complete(check("\"hello\"")));
    }

    #[test]
    fn incomplete_unbalanced() {
        assert!(is_incomplete(check("(+ 1")));
        assert!(is_incomplete(check("(define x")));
        assert!(is_incomplete(check("[1 2 3")));
        assert!(is_incomplete(check("{:a 1")));
    }

    #[test]
    fn incomplete_unterminated_string() {
        assert!(is_incomplete(check("\"abc")));
        assert!(is_incomplete(check("(define x \"hi")));
    }

    #[test]
    fn brackets_inside_string_dont_count() {
        assert!(is_complete(check("\"(((\"")));
        assert!(is_complete(check("(define x \"(no close paren here\")")));
    }

    #[test]
    fn brackets_inside_comment_dont_count() {
        assert!(is_complete(check("(+ 1 2) ; ( ( (")));
    }

    #[test]
    fn extra_closer_is_complete_let_eval_handle() {
        // Surplus `)` is invalid but not "needs more input" — submit so the
        // user gets a real error.
        assert!(is_complete(check("(+ 1 2))")));
    }
}
