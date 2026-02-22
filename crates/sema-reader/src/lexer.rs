use sema_core::{SemaError, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum FStringPart {
    Literal(String),
    Expr(String),
}

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
    FString(Vec<FStringPart>),
    ShortLambdaStart,
    Symbol(String),
    Keyword(String),
    Bool(bool),
    Char(char),
    BytevectorStart,
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
        let span = Span::point(line, col);

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
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::LParen,
                    span: span.with_end(line, col),
                });
            }
            ')' => {
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::RParen,
                    span: span.with_end(line, col),
                });
            }
            '[' => {
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::LBracket,
                    span: span.with_end(line, col),
                });
            }
            ']' => {
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::RBracket,
                    span: span.with_end(line, col),
                });
            }
            '{' => {
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::LBrace,
                    span: span.with_end(line, col),
                });
            }
            '}' => {
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::RBrace,
                    span: span.with_end(line, col),
                });
            }

            // Quote forms
            '\'' => {
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::Quote,
                    span: span.with_end(line, col),
                });
            }
            '`' => {
                col += 1;
                i += 1;
                tokens.push(SpannedToken {
                    token: Token::Quasiquote,
                    span: span.with_end(line, col),
                });
            }
            ',' => {
                if i + 1 < chars.len() && chars[i + 1] == '@' {
                    col += 2;
                    i += 2;
                    tokens.push(SpannedToken {
                        token: Token::UnquoteSplice,
                        span: span.with_end(line, col),
                    });
                } else {
                    col += 1;
                    i += 1;
                    tokens.push(SpannedToken {
                        token: Token::Unquote,
                        span: span.with_end(line, col),
                    });
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
                        read_string_escape(&chars, &mut i, &mut col, &mut s, span)?;
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
                    span: span.with_end(line, col),
                });
            }

            // #t, #f booleans
            '#' => {
                if i + 1 < chars.len() {
                    match chars[i + 1] {
                        't' => {
                            i += 2;
                            col += 2;
                            tokens.push(SpannedToken {
                                token: Token::Bool(true),
                                span: span.with_end(line, col),
                            });
                        }
                        'f' => {
                            i += 2;
                            col += 2;
                            tokens.push(SpannedToken {
                                token: Token::Bool(false),
                                span: span.with_end(line, col),
                            });
                        }
                        '\\' => {
                            // Character literal: #\a, #\space, #\newline, etc.
                            i += 2; // skip #\
                            col += 2;
                            if i >= chars.len() {
                                return Err(SemaError::Reader {
                                    message: "unexpected end of input after #\\".to_string(),
                                    span,
                                });
                            }
                            let start = i;
                            if chars[i].is_alphabetic() {
                                while i < chars.len() && is_symbol_char(chars[i]) {
                                    i += 1;
                                    col += 1;
                                }
                            } else {
                                i += 1;
                                col += 1;
                            }
                            let name: String = chars[start..i].iter().collect();
                            let c = match name.as_str() {
                                "space" => ' ',
                                "newline" => '\n',
                                "tab" => '\t',
                                "return" => '\r',
                                "nul" => '\0',
                                s if s.chars().count() == 1 => s.chars().next().unwrap(),
                                _ => {
                                    return Err(SemaError::Reader {
                                        message: format!("unknown character name: {name}"),
                                        span,
                                    });
                                }
                            };
                            tokens.push(SpannedToken {
                                token: Token::Char(c),
                                span: span.with_end(line, col),
                            });
                        }
                        'u' if i + 3 < chars.len()
                            && chars[i + 2] == '8'
                            && chars[i + 3] == '(' =>
                        {
                            i += 4;
                            col += 4;
                            tokens.push(SpannedToken {
                                token: Token::BytevectorStart,
                                span: span.with_end(line, col),
                            });
                        }
                        '(' => {
                            // Short lambda: #(+ % 1) → (lambda (%1) (+ %1 1))
                            i += 2; // skip #(
                            col += 2;
                            tokens.push(SpannedToken {
                                token: Token::ShortLambdaStart,
                                span: span.with_end(line, col),
                            });
                        }
                        '"' => {
                            // Regex literal: #"pattern" — raw string (no escape processing)
                            i += 2; // skip #"
                            col += 2;
                            let mut s = String::new();
                            while i < chars.len() && chars[i] != '"' {
                                if chars[i] == '\\' && i + 1 < chars.len() && chars[i + 1] == '"' {
                                    s.push('"');
                                    i += 2;
                                    col += 2;
                                } else {
                                    if chars[i] == '\n' {
                                        line += 1;
                                        col = 0;
                                    }
                                    s.push(chars[i]);
                                    i += 1;
                                    col += 1;
                                }
                            }
                            if i >= chars.len() {
                                return Err(SemaError::Reader {
                                    message: "unterminated regex literal".to_string(),
                                    span,
                                });
                            }
                            i += 1; // closing quote
                            col += 1;
                            tokens.push(SpannedToken {
                                token: Token::String(s),
                                span: span.with_end(line, col),
                            });
                        }
                        '!' if line == 1 && col == 1 => {
                            // Shebang line: #!/usr/bin/env sema
                            while i < chars.len() && chars[i] != '\n' {
                                i += 1;
                            }
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
                        message: "unexpected end of input after `#`".to_string(),
                        span,
                    }
                    .with_hint("# starts a special form: #t, #f, #\\char, #u8(...)"));
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
                    span: span.with_end(line, col),
                });
            }

            // Numbers, f-strings, and symbols
            _ => {
                if ch == 'f' && i + 1 < chars.len() && chars[i + 1] == '"' {
                    // f-string: f"Hello ${name}" → FString token
                    i += 1; // skip 'f'
                    col += 1;
                    i += 1; // skip opening '"'
                    col += 1;
                    let mut parts: Vec<FStringPart> = Vec::new();
                    let mut current = String::new();

                    while i < chars.len() && chars[i] != '"' {
                        if chars[i] == '\\' && i + 1 < chars.len() {
                            i += 1;
                            col += 1;
                            read_string_escape(&chars, &mut i, &mut col, &mut current, span)?;
                        } else if chars[i] == '$'
                            && i + 1 < chars.len()
                            && chars[i + 1] == '{'
                        {
                            // Start interpolation
                            if !current.is_empty() {
                                parts.push(FStringPart::Literal(std::mem::take(&mut current)));
                            }
                            i += 2; // skip "${"
                            col += 2;
                            let mut expr = String::new();
                            let mut depth = 1;
                            while i < chars.len() && depth > 0 {
                                if chars[i] == '{' {
                                    depth += 1;
                                } else if chars[i] == '}' {
                                    depth -= 1;
                                    if depth == 0 {
                                        break;
                                    }
                                }
                                if chars[i] == '\n' {
                                    line += 1;
                                    col = 0;
                                }
                                expr.push(chars[i]);
                                i += 1;
                                col += 1;
                            }
                            if depth != 0 {
                                return Err(SemaError::Reader {
                                    message: "unterminated interpolation in f-string".to_string(),
                                    span,
                                });
                            }
                            let trimmed = expr.trim().to_string();
                            if trimmed.is_empty() {
                                return Err(SemaError::Reader {
                                    message: "empty interpolation in f-string".to_string(),
                                    span,
                                });
                            }
                            parts.push(FStringPart::Expr(trimmed));
                            // i points to closing '}', outer i+=1 will skip past it
                        } else {
                            if chars[i] == '\n' {
                                line += 1;
                                col = 0;
                            }
                            current.push(chars[i]);
                        }
                        i += 1;
                        col += 1;
                    }

                    if i >= chars.len() {
                        return Err(SemaError::Reader {
                            message: "unterminated f-string".to_string(),
                            span,
                        });
                    }
                    i += 1; // closing quote
                    col += 1;

                    if !current.is_empty() {
                        parts.push(FStringPart::Literal(current));
                    }

                    tokens.push(SpannedToken {
                        token: Token::FString(parts),
                        span: span.with_end(line, col),
                    });
                } else if ch == '-' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                    // Negative number
                    let (tok, len) = read_number(&chars[i..], &span)?;
                    i += len;
                    col += len;
                    tokens.push(SpannedToken {
                        token: tok,
                        span: span.with_end(line, col),
                    });
                } else if ch.is_ascii_digit() {
                    let (tok, len) = read_number(&chars[i..], &span)?;
                    i += len;
                    col += len;
                    tokens.push(SpannedToken {
                        token: tok,
                        span: span.with_end(line, col),
                    });
                } else if is_symbol_start(ch) {
                    let start = i;
                    while i < chars.len() && is_symbol_char(chars[i]) {
                        i += 1;
                        col += 1;
                    }
                    let name: String = chars[start..i].iter().collect();
                    let token_span = span.with_end(line, col);
                    // Check for special symbol names
                    match name.as_str() {
                        "true" => tokens.push(SpannedToken {
                            token: Token::Bool(true),
                            span: token_span,
                        }),
                        "false" => tokens.push(SpannedToken {
                            token: Token::Bool(false),
                            span: token_span,
                        }),
                        "nil" => tokens.push(SpannedToken {
                            token: Token::Symbol("nil".to_string()),
                            span: token_span,
                        }),
                        "." => tokens.push(SpannedToken {
                            token: Token::Dot,
                            span: token_span,
                        }),
                        _ => tokens.push(SpannedToken {
                            token: Token::Symbol(name),
                            span: token_span,
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

/// Process a string escape sequence. `chars[*i]` is the character after `\`.
/// Pushes the decoded character(s) to `buf` and advances `*i`/`*col` for
/// multi-character escapes (hex, unicode). The caller handles the final `i += 1`.
fn read_string_escape(
    chars: &[char],
    i: &mut usize,
    col: &mut usize,
    buf: &mut String,
    span: Span,
) -> Result<(), SemaError> {
    match chars[*i] {
        'n' => buf.push('\n'),
        't' => buf.push('\t'),
        'r' => buf.push('\r'),
        '\\' => buf.push('\\'),
        '"' => buf.push('"'),
        '0' => buf.push('\0'),
        '$' => buf.push('$'),
        'x' => {
            // R7RS hex escape: \x<hex>;
            let mut hex = String::new();
            while *i + 1 < chars.len()
                && chars[*i + 1] != ';'
                && chars[*i + 1].is_ascii_hexdigit()
            {
                *i += 1;
                *col += 1;
                hex.push(chars[*i]);
            }
            if hex.is_empty() {
                return Err(SemaError::Reader {
                    message: "empty hex escape \\x;".to_string(),
                    span,
                });
            }
            if *i + 1 >= chars.len() || chars[*i + 1] != ';' {
                return Err(SemaError::Reader {
                    message: "hex escape \\x missing terminating semicolon".to_string(),
                    span,
                });
            }
            *i += 1;
            *col += 1;
            let code = u32::from_str_radix(&hex, 16).map_err(|_| SemaError::Reader {
                message: format!("invalid hex escape \\x{};", hex),
                span,
            })?;
            let ch = char::from_u32(code).ok_or_else(|| SemaError::Reader {
                message: format!("invalid unicode scalar value \\x{};", hex),
                span,
            })?;
            buf.push(ch);
        }
        'u' => {
            // \u<4 hex digits>
            let mut hex = String::new();
            for _ in 0..4 {
                if *i + 1 >= chars.len() || !chars[*i + 1].is_ascii_hexdigit() {
                    return Err(SemaError::Reader {
                        message: "\\u escape requires exactly 4 hex digits".to_string(),
                        span,
                    });
                }
                *i += 1;
                *col += 1;
                hex.push(chars[*i]);
            }
            let code = u32::from_str_radix(&hex, 16).map_err(|_| SemaError::Reader {
                message: format!("invalid hex escape \\u{}", hex),
                span,
            })?;
            let ch = char::from_u32(code).ok_or_else(|| SemaError::Reader {
                message: format!("invalid unicode scalar value \\u{}", hex),
                span,
            })?;
            buf.push(ch);
        }
        'U' => {
            // \U<8 hex digits>
            let mut hex = String::new();
            for _ in 0..8 {
                if *i + 1 >= chars.len() || !chars[*i + 1].is_ascii_hexdigit() {
                    return Err(SemaError::Reader {
                        message: "\\U escape requires exactly 8 hex digits".to_string(),
                        span,
                    });
                }
                *i += 1;
                *col += 1;
                hex.push(chars[*i]);
            }
            let code = u32::from_str_radix(&hex, 16).map_err(|_| SemaError::Reader {
                message: format!("invalid hex escape \\U{}", hex),
                span,
            })?;
            let ch = char::from_u32(code).ok_or_else(|| SemaError::Reader {
                message: format!("invalid unicode scalar value \\U{}", hex),
                span,
            })?;
            buf.push(ch);
        }
        other => {
            buf.push('\\');
            buf.push(other);
        }
    }
    Ok(())
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
            span: *span,
        })?;
        Ok((Token::Float(f), i))
    } else {
        let s: String = chars[..i].iter().collect();
        let n: i64 = s.parse().map_err(|_| SemaError::Reader {
            message: format!("invalid integer: {s}"),
            span: *span,
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
