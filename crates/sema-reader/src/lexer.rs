use sema_core::{SemaError, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Quote,
    Quasiquote,
    Unquote,
    UnquoteSplice,
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Keyword(String),
    Bool(bool),
    Dot,
}

#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

pub fn tokenize(input: &str) -> Result<Vec<SpannedToken>, SemaError> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    let mut line = 1;
    let mut col = 1;

    while i < chars.len() {
        let ch = chars[i];
        let span = Span { line, col };

        match ch {
            // Whitespace
            ' ' | '\t' | '\r' => {
                col += 1;
                i += 1;
            }
            '\n' => {
                line += 1;
                col = 1;
                i += 1;
            }

            // Comments
            ';' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }

            // Delimiters
            '(' => {
                tokens.push(SpannedToken {
                    token: Token::LParen,
                    span,
                });
                col += 1;
                i += 1;
            }
            ')' => {
                tokens.push(SpannedToken {
                    token: Token::RParen,
                    span,
                });
                col += 1;
                i += 1;
            }
            '[' => {
                tokens.push(SpannedToken {
                    token: Token::LBracket,
                    span,
                });
                col += 1;
                i += 1;
            }
            ']' => {
                tokens.push(SpannedToken {
                    token: Token::RBracket,
                    span,
                });
                col += 1;
                i += 1;
            }
            '{' => {
                tokens.push(SpannedToken {
                    token: Token::LBrace,
                    span,
                });
                col += 1;
                i += 1;
            }
            '}' => {
                tokens.push(SpannedToken {
                    token: Token::RBrace,
                    span,
                });
                col += 1;
                i += 1;
            }

            // Quote forms
            '\'' => {
                tokens.push(SpannedToken {
                    token: Token::Quote,
                    span,
                });
                col += 1;
                i += 1;
            }
            '`' => {
                tokens.push(SpannedToken {
                    token: Token::Quasiquote,
                    span,
                });
                col += 1;
                i += 1;
            }
            ',' => {
                if i + 1 < chars.len() && chars[i + 1] == '@' {
                    tokens.push(SpannedToken {
                        token: Token::UnquoteSplice,
                        span,
                    });
                    col += 2;
                    i += 2;
                } else {
                    tokens.push(SpannedToken {
                        token: Token::Unquote,
                        span,
                    });
                    col += 1;
                    i += 1;
                }
            }

            // Strings
            '"' => {
                let mut s = String::new();
                i += 1;
                col += 1;
                while i < chars.len() && chars[i] != '"' {
                    if chars[i] == '\\' && i + 1 < chars.len() {
                        i += 1;
                        col += 1;
                        match chars[i] {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            'r' => s.push('\r'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            other => {
                                s.push('\\');
                                s.push(other);
                            }
                        }
                    } else {
                        if chars[i] == '\n' {
                            line += 1;
                            col = 0;
                        }
                        s.push(chars[i]);
                    }
                    i += 1;
                    col += 1;
                }
                if i >= chars.len() {
                    return Err(SemaError::Reader {
                        message: "unterminated string".to_string(),
                        span,
                    });
                }
                i += 1; // closing quote
                col += 1;
                tokens.push(SpannedToken {
                    token: Token::String(s),
                    span,
                });
            }

            // #t, #f booleans
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            tokens.push(SpannedToken {
                                token: Token::Bool(true),
                                span,
                            });
                            i += 2;
                            col += 2;
                        }
                        'f' => {
                            tokens.push(SpannedToken {
                                token: Token::Bool(false),
                                span,
                            });
                            i += 2;
                            col += 2;
                        }
                        _ => {
                            return Err(SemaError::Reader {
                                message: format!(
                                    "unexpected character after #: '{}'",
                                    chars[i + 1]
                                ),
                                span,
                            });
                        }
                    }
                } else {
                    return Err(SemaError::Reader {
                        message: "unexpected end of input after #".to_string(),
                        span,
                    });
                }
            }

            // Keywords (:foo)
            ':' => {
                i += 1;
                col += 1;
                let start = i;
                while i < chars.len() && is_symbol_char(chars[i]) {
                    i += 1;
                    col += 1;
                }
                if i == start {
                    return Err(SemaError::Reader {
                        message: "expected keyword name after ':'".to_string(),
                        span,
                    });
                }
                let name: String = chars[start..i].iter().collect();
                tokens.push(SpannedToken {
                    token: Token::Keyword(name),
                    span,
                });
            }

            // Numbers and symbols
            _ => {
                if ch == '-' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    // Negative number
                    let (tok, len) = read_number(&chars[i..], &span)?;
                    tokens.push(SpannedToken { token: tok, span });
                    i += len;
                    col += len;
                } else if ch.is_ascii_digit() {
                    let (tok, len) = read_number(&chars[i..], &span)?;
                    tokens.push(SpannedToken { token: tok, span });
                    i += len;
                    col += len;
                } else if is_symbol_start(ch) {
                    let start = i;
                    while i < chars.len() && is_symbol_char(chars[i]) {
                        i += 1;
                        col += 1;
                    }
                    let name: String = chars[start..i].iter().collect();
                    // Check for special symbol names
                    match name.as_str() {
                        "true" => tokens.push(SpannedToken {
                            token: Token::Bool(true),
                            span,
                        }),
                        "false" => tokens.push(SpannedToken {
                            token: Token::Bool(false),
                            span,
                        }),
                        "nil" => tokens.push(SpannedToken {
                            token: Token::Symbol("nil".to_string()),
                            span,
                        }),
                        "." => tokens.push(SpannedToken {
                            token: Token::Dot,
                            span,
                        }),
                        _ => tokens.push(SpannedToken {
                            token: Token::Symbol(name),
                            span,
                        }),
                    }
                } else {
                    return Err(SemaError::Reader {
                        message: format!("unexpected character: '{ch}'"),
                        span,
                    });
                }
            }
        }
    }

    Ok(tokens)
}

fn read_number(chars: &[char], span: &Span) -> Result<(Token, usize), SemaError> {
    let mut i = 0;
    if chars[i] == '-' {
        i += 1;
    }
    while i < chars.len() && chars[i].is_ascii_digit() {
        i += 1;
    }
    if i < chars.len() && chars[i] == '.' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
        i += 1; // skip dot
        while i < chars.len() && chars[i].is_ascii_digit() {
            i += 1;
        }
        let s: String = chars[..i].iter().collect();
        let f: f64 = s.parse().map_err(|_| SemaError::Reader {
            message: format!("invalid float: {s}"),
            span: span.clone(),
        })?;
        Ok((Token::Float(f), i))
    } else {
        let s: String = chars[..i].iter().collect();
        let n: i64 = s.parse().map_err(|_| SemaError::Reader {
            message: format!("invalid integer: {s}"),
            span: span.clone(),
        })?;
        Ok((Token::Int(n), i))
    }
}

fn is_symbol_start(ch: char) -> bool {
    ch.is_alphabetic()
        || matches!(
            ch,
            '+' | '-' | '*' | '/' | '!' | '?' | '<' | '>' | '=' | '_' | '&' | '%' | '^' | '~' | '.'
        )
}

fn is_symbol_char(ch: char) -> bool {
    is_symbol_start(ch) || ch.is_ascii_digit() || matches!(ch, '-' | '/' | '.')
}
