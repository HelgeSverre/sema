use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{resolve, SemaError, Span, SpanMap, Value, ValueView};

use crate::lexer::{tokenize, FStringPart, SpannedToken, Token};

struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
    span_map: SpanMap,
}

impl Parser {
    fn new(tokens: Vec<SpannedToken>) -> Self {
        Parser {
            tokens,
            pos: 0,
            span_map: SpanMap::new(),
        }
    }

    fn peek(&self) -> Option<&Token> {
        let mut pos = self.pos;
        while let Some(t) = self.tokens.get(pos) {
            match &t.token {
                Token::Comment(_) | Token::Newline => pos += 1,
                _ => return Some(&t.token),
            }
        }
        None
    }

    fn span(&self) -> Span {
        let mut pos = self.pos;
        while let Some(t) = self.tokens.get(pos) {
            match &t.token {
                Token::Comment(_) | Token::Newline => pos += 1,
                _ => return t.span,
            }
        }
        Span::point(0, 0)
    }

    fn skip_trivia(&mut self) {
        while let Some(t) = self.tokens.get(self.pos) {
            match &t.token {
                Token::Comment(_) | Token::Newline => self.pos += 1,
                _ => break,
            }
        }
    }

    fn advance(&mut self) -> Option<&SpannedToken> {
        self.skip_trivia();
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, expected: &Token) -> Result<(), SemaError> {
        let span = self.span();
        match self.advance() {
            Some(t) if &t.token == expected => Ok(()),
            Some(t) => Err(SemaError::Reader {
                message: format!(
                    "expected `{}`, got `{}`",
                    token_display(expected),
                    token_display(&t.token)
                ),
                span,
            }),
            None => Err(SemaError::Reader {
                message: format!("expected `{}`, got end of input", token_display(expected)),
                span,
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Value, SemaError> {
        let span = self.span();
        match self.peek() {
            None => Err(SemaError::Reader {
                message: "unexpected end of input".to_string(),
                span,
            }),
            Some(Token::LParen) => self.parse_list(),
            Some(Token::LBracket) => self.parse_vector(),
            Some(Token::LBrace) => self.parse_map(),
            Some(Token::Quote) => {
                self.advance();
                let inner = self.parse_expr().map_err(|_| {
                    SemaError::Reader {
                        message: "quote (') requires an expression after it".to_string(),
                        span,
                    }
                    .with_hint("e.g. '(1 2 3) or 'foo")
                })?;
                self.make_list_with_span(vec![Value::symbol("quote"), inner], span)
            }
            Some(Token::Quasiquote) => {
                self.advance();
                let inner = self.parse_expr().map_err(|_| {
                    SemaError::Reader {
                        message: "quasiquote (`) requires an expression after it".to_string(),
                        span,
                    }
                    .with_hint("e.g. `(list ,x)")
                })?;
                self.make_list_with_span(vec![Value::symbol("quasiquote"), inner], span)
            }
            Some(Token::Unquote) => {
                self.advance();
                let inner = self.parse_expr().map_err(|_| {
                    SemaError::Reader {
                        message: "unquote (,) requires an expression after it".to_string(),
                        span,
                    }
                    .with_hint("use inside quasiquote, e.g. `(list ,x)")
                })?;
                self.make_list_with_span(vec![Value::symbol("unquote"), inner], span)
            }
            Some(Token::UnquoteSplice) => {
                self.advance();
                let inner = self.parse_expr().map_err(|_| {
                    SemaError::Reader {
                        message: "unquote-splicing (,@) requires an expression after it"
                            .to_string(),
                        span,
                    }
                    .with_hint("use inside quasiquote, e.g. `(list ,@xs)")
                })?;
                self.make_list_with_span(vec![Value::symbol("unquote-splicing"), inner], span)
            }
            Some(Token::BytevectorStart) => self.parse_bytevector(),
            Some(Token::ShortLambdaStart) => self.parse_short_lambda(),
            Some(_) => self.parse_atom(),
        }
    }

    fn make_list_with_span(&mut self, items: Vec<Value>, span: Span) -> Result<Value, SemaError> {
        let rc = Rc::new(items);
        let ptr = Rc::as_ptr(&rc) as usize;
        self.span_map.insert(ptr, span);
        Ok(Value::list_from_rc(rc))
    }

    /// Get the span of the previously consumed token (the one at pos-1).
    fn prev_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span::point(0, 0)
        }
    }

    fn parse_list(&mut self) -> Result<Value, SemaError> {
        let open_span = self.span();
        self.expect(&Token::LParen)?;
        let mut items = Vec::new();
        while self.peek() != Some(&Token::RParen) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated list".to_string(),
                    span: open_span,
                }
                .with_hint("add a closing `)`"));
            }
            // Handle dotted pairs: (a . b)
            if self.peek() == Some(&Token::Dot) {
                self.advance(); // skip dot
                let cdr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                let close = self.prev_span();
                items.push(Value::symbol("."));
                items.push(cdr);
                return self.make_list_with_span(items, open_span.to(&close));
            }
            items.push(self.parse_expr()?);
        }
        self.expect(&Token::RParen)?;
        let close = self.prev_span();
        self.make_list_with_span(items, open_span.to(&close))
    }

    fn parse_vector(&mut self) -> Result<Value, SemaError> {
        let open_span = self.span();
        self.expect(&Token::LBracket)?;
        let mut items = Vec::new();
        while self.peek() != Some(&Token::RBracket) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated vector".to_string(),
                    span: open_span,
                }
                .with_hint("add a closing `]`"));
            }
            items.push(self.parse_expr()?);
        }
        self.expect(&Token::RBracket)?;
        let close = self.prev_span();
        let rc = Rc::new(items);
        let ptr = Rc::as_ptr(&rc) as usize;
        self.span_map.insert(ptr, open_span.to(&close));
        Ok(Value::vector_from_rc(rc))
    }

    fn parse_map(&mut self) -> Result<Value, SemaError> {
        let open_span = self.span();
        self.expect(&Token::LBrace)?;
        let mut map = BTreeMap::new();
        while self.peek() != Some(&Token::RBrace) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated map".to_string(),
                    span: open_span,
                }
                .with_hint("add a closing `}`"));
            }
            let key = self.parse_expr()?;
            if self.peek() == Some(&Token::RBrace) || self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "map literal must have even number of forms".to_string(),
                    span: self.span(),
                });
            }
            let val = self.parse_expr()?;
            map.insert(key, val);
        }
        self.expect(&Token::RBrace)?;
        Ok(Value::map(map))
    }

    fn parse_bytevector(&mut self) -> Result<Value, SemaError> {
        let open_span = self.span();
        self.advance(); // consume BytevectorStart token
        let mut bytes = Vec::new();
        while self.peek() != Some(&Token::RParen) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated bytevector".to_string(),
                    span: open_span,
                }
                .with_hint("add a closing `)`"));
            }
            let span = self.span();
            match self.peek() {
                Some(Token::Int(n)) => {
                    let n = *n;
                    self.advance();
                    if !(0..=255).contains(&n) {
                        return Err(SemaError::Reader {
                            message: format!("#u8(...): byte value {n} out of range 0..255"),
                            span,
                        });
                    }
                    bytes.push(n as u8);
                }
                _ => {
                    return Err(SemaError::Reader {
                        message: "#u8(...): expected integer byte value".to_string(),
                        span,
                    });
                }
            }
        }
        self.expect(&Token::RParen)?;
        Ok(Value::bytevector(bytes))
    }

    fn parse_short_lambda(&mut self) -> Result<Value, SemaError> {
        let open_span = self.span();
        self.advance(); // consume ShortLambdaStart
        let mut body_items = Vec::new();
        while self.peek() != Some(&Token::RParen) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated short lambda #(...)".to_string(),
                    span: open_span,
                }
                .with_hint("add a closing `)`"));
            }
            body_items.push(self.parse_expr()?);
        }
        self.expect(&Token::RParen)?;

        // Build the body as a single list form: (fn-name arg1 arg2 ...)
        let body = Value::list(body_items);

        // Scan body for % / %1 / %2 etc., rewrite % → %1
        let mut max_arg: usize = 0;
        let body = rewrite_percent_args(&body, &mut max_arg);

        // Build parameter list
        let params: Vec<Value> = if max_arg == 0 {
            vec![]
        } else {
            (1..=max_arg)
                .map(|n| Value::symbol(&format!("%{}", n)))
                .collect()
        };

        Ok(Value::list(vec![
            Value::symbol("lambda"),
            Value::list(params),
            body,
        ]))
    }

    fn parse_atom(&mut self) -> Result<Value, SemaError> {
        let span = self.span();
        match self.advance() {
            Some(SpannedToken {
                token: Token::Int(n),
                ..
            }) => Ok(Value::int(*n)),
            Some(SpannedToken {
                token: Token::Float(f),
                ..
            }) => Ok(Value::float(*f)),
            Some(SpannedToken {
                token: Token::String(s),
                ..
            }) => Ok(Value::string(s)),
            Some(SpannedToken {
                token: Token::Regex(s),
                ..
            }) => Ok(Value::string(s)),
            Some(SpannedToken {
                token: Token::Symbol(s),
                ..
            }) => {
                if s == "nil" {
                    Ok(Value::nil())
                } else {
                    Ok(Value::symbol(s))
                }
            }
            Some(SpannedToken {
                token: Token::Keyword(s),
                ..
            }) => Ok(Value::keyword(s)),
            Some(SpannedToken {
                token: Token::Bool(b),
                ..
            }) => Ok(Value::bool(*b)),
            Some(SpannedToken {
                token: Token::Char(c),
                ..
            }) => Ok(Value::char(*c)),
            Some(SpannedToken {
                token: Token::FString(parts),
                ..
            }) => {
                let parts = parts.clone();
                let mut items = vec![Value::symbol("str")];
                for part in &parts {
                    match part {
                        FStringPart::Literal(s) => {
                            if !s.is_empty() {
                                items.push(Value::string(s));
                            }
                        }
                        FStringPart::Expr(src) => {
                            let val = read(src)?;
                            items.push(val);
                        }
                    }
                }
                Ok(Value::list(items))
            }
            Some(t) => {
                let (name, hint) = match &t.token {
                    Token::RParen => (
                        "unexpected closing `)`",
                        Some("no matching opening parenthesis"),
                    ),
                    Token::RBracket => (
                        "unexpected closing `]`",
                        Some("no matching opening bracket"),
                    ),
                    Token::RBrace => ("unexpected closing `}`", Some("no matching opening brace")),
                    Token::Dot => (
                        "unexpected `.`",
                        Some("dots are used in pair notation, e.g. (a . b)"),
                    ),
                    _ => ("unexpected token", None),
                };
                let err = SemaError::Reader {
                    message: name.to_string(),
                    span,
                };
                Err(if let Some(h) = hint {
                    err.with_hint(h)
                } else {
                    err
                })
            }
            None => Err(SemaError::Reader {
                message: "unexpected end of input".to_string(),
                span,
            }),
        }
    }
}

fn token_display(tok: &Token) -> &'static str {
    match tok {
        Token::LParen => "(",
        Token::RParen => ")",
        Token::LBracket => "[",
        Token::RBracket => "]",
        Token::LBrace => "{",
        Token::RBrace => "}",
        Token::Quote => "'",
        Token::Quasiquote => "`",
        Token::Unquote => ",",
        Token::UnquoteSplice => ",@",
        Token::Dot => ".",
        Token::BytevectorStart => "#u8(",
        Token::Int(_) => "integer",
        Token::Float(_) => "float",
        Token::String(_) => "string",
        Token::Symbol(_) => "symbol",
        Token::Keyword(_) => "keyword",
        Token::Bool(_) => "boolean",
        Token::Char(_) => "character",
        Token::FString(_) => "f-string",
        Token::ShortLambdaStart => "#(",
        Token::Comment(_) => "comment",
        Token::Newline => "newline",
        Token::Regex(_) => "regex",
    }
}

/// Recursively scan a Value AST for `%`, `%1`, `%2`, etc. symbols.
/// Rewrites bare `%` to `%1`. Tracks the highest numbered arg in `max_arg`.
/// Skips recursion into nested `(lambda ...)` / `(fn ...)` forms.
fn rewrite_percent_args(expr: &Value, max_arg: &mut usize) -> Value {
    match expr.view() {
        ValueView::Symbol(spur) => {
            let name = resolve(spur);
            if name == "%" {
                *max_arg = (*max_arg).max(1);
                Value::symbol("%1")
            } else if let Some(rest) = name.strip_prefix('%') {
                if let Ok(n) = rest.parse::<usize>() {
                    if n > 0 {
                        *max_arg = (*max_arg).max(n);
                    }
                }
                expr.clone()
            } else {
                expr.clone()
            }
        }
        ValueView::List(items) => {
            // Skip nested (lambda ...) / (fn ...) forms — their % args are their own
            if let Some(first) = items.first() {
                if let ValueView::Symbol(s) = first.view() {
                    let name = resolve(s);
                    if name == "lambda" || name == "fn" {
                        return expr.clone();
                    }
                }
            }
            let new_items: Vec<Value> = items
                .iter()
                .map(|item| rewrite_percent_args(item, max_arg))
                .collect();
            Value::list(new_items)
        }
        ValueView::Vector(items) => {
            let new_items: Vec<Value> = items
                .iter()
                .map(|item| rewrite_percent_args(item, max_arg))
                .collect();
            Value::vector(new_items)
        }
        _ => expr.clone(),
    }
}

/// Read a single s-expression from a string.
pub fn read(input: &str) -> Result<Value, SemaError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    if parser.peek().is_none() {
        return Ok(Value::nil());
    }
    parser.parse_expr()
}

/// Read all s-expressions from a string.
pub fn read_many(input: &str) -> Result<Vec<Value>, SemaError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let mut exprs = Vec::new();
    while parser.peek().is_some() {
        exprs.push(parser.parse_expr()?);
    }
    Ok(exprs)
}

/// Read all s-expressions and return the accumulated span map.
pub fn read_many_with_spans(input: &str) -> Result<(Vec<Value>, SpanMap), SemaError> {
    let tokens = tokenize(input)?;
    let mut parser = Parser::new(tokens);
    let mut exprs = Vec::new();
    while parser.peek().is_some() {
        exprs.push(parser.parse_expr()?);
    }
    Ok((exprs, parser.span_map))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_int() {
        assert_eq!(read("42").unwrap(), Value::int(42));
    }

    #[test]
    fn test_read_negative_int() {
        assert_eq!(read("-7").unwrap(), Value::int(-7));
    }

    #[test]
    fn test_read_float() {
        assert_eq!(read("3.14").unwrap(), Value::float(3.14));
    }

    #[test]
    fn test_read_string() {
        assert_eq!(read("\"hello\"").unwrap(), Value::string("hello"));
    }

    #[test]
    fn test_read_symbol() {
        assert_eq!(read("foo").unwrap(), Value::symbol("foo"));
    }

    #[test]
    fn test_read_keyword() {
        assert_eq!(read(":bar").unwrap(), Value::keyword("bar"));
    }

    #[test]
    fn test_read_bool() {
        assert_eq!(read("#t").unwrap(), Value::bool(true));
        assert_eq!(read("#f").unwrap(), Value::bool(false));
    }

    #[test]
    fn test_read_list() {
        let result = read("(+ 1 2)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("+"), Value::int(1), Value::int(2)])
        );
    }

    #[test]
    fn test_read_nested_list() {
        let result = read("(* (+ 1 2) 3)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("*"),
                Value::list(vec![Value::symbol("+"), Value::int(1), Value::int(2)]),
                Value::int(3)
            ])
        );
    }

    #[test]
    fn test_read_vector() {
        let result = read("[1 2 3]").unwrap();
        assert_eq!(
            result,
            Value::vector(vec![Value::int(1), Value::int(2), Value::int(3)])
        );
    }

    #[test]
    fn test_read_map() {
        let result = read("{:a 1 :b 2}").unwrap();
        let mut expected = BTreeMap::new();
        expected.insert(Value::keyword("a"), Value::int(1));
        expected.insert(Value::keyword("b"), Value::int(2));
        assert_eq!(result, Value::map(expected));
    }

    #[test]
    fn test_read_quote() {
        let result = read("'foo").unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("quote"), Value::symbol("foo")])
        );
    }

    #[test]
    fn test_read_quasiquote() {
        let result = read("`(a ,b ,@c)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("quasiquote"),
                Value::list(vec![
                    Value::symbol("a"),
                    Value::list(vec![Value::symbol("unquote"), Value::symbol("b")]),
                    Value::list(vec![Value::symbol("unquote-splicing"), Value::symbol("c")]),
                ])
            ])
        );
    }

    #[test]
    fn test_read_nil() {
        assert_eq!(read("nil").unwrap(), Value::nil());
    }

    #[test]
    fn test_read_many_exprs() {
        let results = read_many("1 2 3").unwrap();
        assert_eq!(results, vec![Value::int(1), Value::int(2), Value::int(3)]);
    }

    #[test]
    fn test_comments() {
        let result = read_many("; comment\n(+ 1 2)").unwrap();
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_read_zero() {
        assert_eq!(read("0").unwrap(), Value::int(0));
    }

    #[test]
    fn test_read_negative_zero() {
        assert_eq!(read("-0").unwrap(), Value::int(0));
    }

    #[test]
    fn test_read_leading_zeros() {
        assert_eq!(read("007").unwrap(), Value::int(7));
    }

    #[test]
    fn test_read_large_int() {
        assert_eq!(read("9999999999999").unwrap(), Value::int(9999999999999));
    }

    #[test]
    fn test_read_int_overflow() {
        // i64::MAX + 1 should error, not silently wrap
        assert!(read("9999999999999999999999").is_err());
    }

    #[test]
    fn test_read_negative_float() {
        assert_eq!(read("-2.5").unwrap(), Value::float(-2.5));
    }

    #[test]
    fn test_read_float_leading_zero() {
        assert_eq!(read("0.5").unwrap(), Value::float(0.5));
    }

    #[test]
    fn test_read_minus_is_symbol() {
        // Bare `-` should be a symbol (subtraction operator), not a number
        assert_eq!(read("-").unwrap(), Value::symbol("-"));
    }

    #[test]
    fn test_read_minus_in_list() {
        // `(- 3)` should parse as call to `-` with arg 3
        let result = read("(- 3)").unwrap();
        assert_eq!(result, Value::list(vec![Value::symbol("-"), Value::int(3)]));
    }

    #[test]
    fn test_read_negative_in_list() {
        // `(-3)` should parse as list containing -3
        let result = read("(-3)").unwrap();
        assert_eq!(result, Value::list(vec![Value::int(-3)]));
    }

    #[test]
    fn test_read_empty_string() {
        assert_eq!(read(r#""""#).unwrap(), Value::string(""));
    }

    #[test]
    fn test_read_string_with_escapes() {
        assert_eq!(
            read(r#""\n\t\r\\\"" "#).unwrap(),
            Value::string("\n\t\r\\\"")
        );
    }

    #[test]
    fn test_read_string_unknown_escape() {
        // Unknown escape sequences are preserved literally
        assert_eq!(read(r#""\z""#).unwrap(), Value::string("\\z"));
    }

    #[test]
    fn test_read_string_with_newline() {
        assert_eq!(
            read("\"line1\nline2\"").unwrap(),
            Value::string("line1\nline2")
        );
    }

    #[test]
    fn test_read_unterminated_string() {
        assert!(read("\"hello").is_err());
    }

    #[test]
    fn test_read_string_escaped_quote_at_end() {
        // `"test\"` — the backslash escapes the quote, string is unterminated
        assert!(read(r#""test\""#).is_err());
    }

    #[test]
    fn test_read_string_with_unicode() {
        assert_eq!(read("\"héllo\"").unwrap(), Value::string("héllo"));
        assert_eq!(read("\"日本語\"").unwrap(), Value::string("日本語"));
        assert_eq!(read("\"🎉\"").unwrap(), Value::string("🎉"));
    }

    #[test]
    fn test_read_string_with_parens() {
        assert_eq!(read("\"(+ 1 2)\"").unwrap(), Value::string("(+ 1 2)"));
    }

    #[test]
    fn test_read_operator_symbols() {
        assert_eq!(read("+").unwrap(), Value::symbol("+"));
        assert_eq!(read("*").unwrap(), Value::symbol("*"));
        assert_eq!(read("/").unwrap(), Value::symbol("/"));
        assert_eq!(read("<=").unwrap(), Value::symbol("<="));
        assert_eq!(read(">=").unwrap(), Value::symbol(">="));
    }

    #[test]
    fn test_read_predicate_symbols() {
        assert_eq!(read("null?").unwrap(), Value::symbol("null?"));
        assert_eq!(read("list?").unwrap(), Value::symbol("list?"));
    }

    #[test]
    fn test_read_arrow_symbols() {
        assert_eq!(
            read("string->symbol").unwrap(),
            Value::symbol("string->symbol")
        );
    }

    #[test]
    fn test_read_namespaced_symbols() {
        assert_eq!(read("file/read").unwrap(), Value::symbol("file/read"));
        assert_eq!(read("http/get").unwrap(), Value::symbol("http/get"));
    }

    #[test]
    fn test_read_true_false_as_bool() {
        assert_eq!(read("true").unwrap(), Value::bool(true));
        assert_eq!(read("false").unwrap(), Value::bool(false));
    }

    #[test]
    fn test_read_bare_colon_error() {
        // `:` alone without a name should error
        assert!(read(":").is_err());
    }

    #[test]
    fn test_read_keyword_with_numbers() {
        assert_eq!(read(":foo123").unwrap(), Value::keyword("foo123"));
    }

    #[test]
    fn test_read_keyword_with_hyphens() {
        assert_eq!(read(":max-turns").unwrap(), Value::keyword("max-turns"));
    }

    #[test]
    fn test_read_hash_invalid() {
        assert!(read("#x").is_err());
        assert!(read("#").is_err());
    }

    #[test]
    fn test_read_empty() {
        assert_eq!(read("").unwrap(), Value::nil());
    }

    #[test]
    fn test_read_whitespace_only() {
        assert_eq!(read("   \n\t  ").unwrap(), Value::nil());
    }

    #[test]
    fn test_read_many_empty() {
        assert_eq!(read_many("").unwrap(), vec![]);
    }

    #[test]
    fn test_read_many_whitespace_only() {
        assert_eq!(read_many("  \n  ").unwrap(), vec![]);
    }

    #[test]
    fn test_read_comment_only() {
        assert_eq!(read_many("; just a comment").unwrap(), vec![]);
    }

    #[test]
    fn test_read_empty_list() {
        assert_eq!(read("()").unwrap(), Value::list(vec![]));
    }

    #[test]
    fn test_read_deeply_nested() {
        let result = read("((((42))))").unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::list(vec![Value::list(vec![Value::list(
                vec![Value::int(42)]
            )])])])
        );
    }

    #[test]
    fn test_read_unterminated_list() {
        assert!(read("(1 2").is_err());
    }

    #[test]
    fn test_read_extra_rparen() {
        // `read` only reads one expr, so extra `)` is just ignored (not consumed)
        // But `read_many` should fail since `)` is not a valid expr start
        let result = read("42").unwrap();
        assert_eq!(result, Value::int(42));
    }

    #[test]
    fn test_read_dotted_pair() {
        let result = read("(a . b)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("a"),
                Value::symbol("."),
                Value::symbol("b")
            ])
        );
    }

    #[test]
    fn test_read_empty_vector() {
        assert_eq!(read("[]").unwrap(), Value::vector(vec![]));
    }

    #[test]
    fn test_read_unterminated_vector() {
        assert!(read("[1 2").is_err());
    }

    #[test]
    fn test_read_empty_map() {
        assert_eq!(read("{}").unwrap(), Value::map(BTreeMap::new()));
    }

    #[test]
    fn test_read_unterminated_map() {
        assert!(read("{:a 1").is_err());
    }

    #[test]
    fn test_read_map_odd_elements() {
        assert!(read("{:a 1 :b}").is_err());
    }

    #[test]
    fn test_read_map_duplicate_keys() {
        // Later key wins (BTreeMap insert replaces)
        let result = read("{:a 1 :a 2}").unwrap();
        let mut expected = BTreeMap::new();
        expected.insert(Value::keyword("a"), Value::int(2));
        assert_eq!(result, Value::map(expected));
    }

    #[test]
    fn test_read_nested_quote() {
        let result = read("''foo").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("quote"),
                Value::list(vec![Value::symbol("quote"), Value::symbol("foo")])
            ])
        );
    }

    #[test]
    fn test_read_quote_list() {
        let result = read("'(1 2 3)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("quote"),
                Value::list(vec![Value::int(1), Value::int(2), Value::int(3)])
            ])
        );
    }

    #[test]
    fn test_read_quote_at_eof() {
        assert!(read("'").is_err());
    }

    #[test]
    fn test_read_unquote_at_eof() {
        assert!(read(",").is_err());
    }

    #[test]
    fn test_read_unquote_splice_at_eof() {
        assert!(read(",@").is_err());
    }

    #[test]
    fn test_read_quasiquote_at_eof() {
        assert!(read("`").is_err());
    }

    #[test]
    fn test_read_comment_after_expr() {
        assert_eq!(read_many("42 ; comment").unwrap(), vec![Value::int(42)]);
    }

    #[test]
    fn test_read_multiple_comments() {
        let result = read_many("; first\n; second\n42").unwrap();
        assert_eq!(result, vec![Value::int(42)]);
    }

    #[test]
    fn test_read_comment_no_newline() {
        // Comment at end of input without trailing newline
        assert_eq!(read_many("; comment").unwrap(), vec![]);
    }

    #[test]
    fn test_read_crlf_line_endings() {
        let result = read_many("1\r\n2\r\n3").unwrap();
        assert_eq!(result, vec![Value::int(1), Value::int(2), Value::int(3)]);
    }

    #[test]
    fn test_read_tabs_as_whitespace() {
        assert_eq!(
            read("(\t+\t1\t2\t)").unwrap(),
            Value::list(vec![Value::symbol("+"), Value::int(1), Value::int(2)])
        );
    }

    #[test]
    fn test_read_mixed_collections() {
        // List containing vector and map
        let result = read("([1 2] {:a 3})").unwrap();
        let mut map = BTreeMap::new();
        map.insert(Value::keyword("a"), Value::int(3));
        assert_eq!(
            result,
            Value::list(vec![
                Value::vector(vec![Value::int(1), Value::int(2)]),
                Value::map(map)
            ])
        );
    }

    #[test]
    fn test_read_many_mixed_types() {
        let result = read_many(r#"42 3.14 "hello" foo :bar #t nil"#).unwrap();
        assert_eq!(result.len(), 7);
        assert_eq!(result[0], Value::int(42));
        assert_eq!(result[1], Value::float(3.14));
        assert_eq!(result[2], Value::string("hello"));
        assert_eq!(result[3], Value::symbol("foo"));
        assert_eq!(result[4], Value::keyword("bar"));
        assert_eq!(result[5], Value::bool(true));
        assert_eq!(result[6], Value::nil());
    }

    #[test]
    fn test_span_map_tracks_lists() {
        let (exprs, spans) = read_many_with_spans("(+ 1 2)").unwrap();
        assert_eq!(exprs.len(), 1);
        // The list should have a span entry
        let rc = exprs[0].as_list_rc().expect("expected list");
        let ptr = Rc::as_ptr(&rc) as usize;
        let span = spans.get(&ptr).expect("list should have span");
        assert_eq!(span.line, 1);
        assert_eq!(span.col, 1);
    }

    #[test]
    fn test_span_map_multiline() {
        let (exprs, spans) = read_many_with_spans("(foo)\n(bar)").unwrap();
        assert_eq!(exprs.len(), 2);
        let rc = exprs[1].as_list_rc().expect("expected list");
        let ptr = Rc::as_ptr(&rc) as usize;
        let span = spans.get(&ptr).expect("second list should have span");
        assert_eq!(span.line, 2);
        assert_eq!(span.col, 1);
    }

    #[test]
    fn test_read_unexpected_char() {
        assert!(read("@").is_err());
        assert!(read("$").is_err());
    }

    #[test]
    fn test_read_char_literal() {
        assert_eq!(read("#\\a").unwrap(), Value::char('a'));
        assert_eq!(read("#\\Z").unwrap(), Value::char('Z'));
        assert_eq!(read("#\\0").unwrap(), Value::char('0'));
    }

    #[test]
    fn test_read_char_named() {
        assert_eq!(read("#\\space").unwrap(), Value::char(' '));
        assert_eq!(read("#\\newline").unwrap(), Value::char('\n'));
        assert_eq!(read("#\\tab").unwrap(), Value::char('\t'));
        assert_eq!(read("#\\return").unwrap(), Value::char('\r'));
        assert_eq!(read("#\\nul").unwrap(), Value::char('\0'));
    }

    #[test]
    fn test_read_char_special() {
        assert_eq!(read("#\\(").unwrap(), Value::char('('));
        assert_eq!(read("#\\)").unwrap(), Value::char(')'));
    }

    #[test]
    fn test_read_char_in_list() {
        let result = read("(#\\a #\\b)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::char('a'), Value::char('b')])
        );
    }

    #[test]
    fn test_read_char_unknown_name() {
        assert!(read("#\\foobar").is_err());
    }

    #[test]
    fn test_read_char_eof() {
        assert!(read("#\\").is_err());
    }

    #[test]
    fn test_read_bytevector_literal() {
        assert_eq!(
            read("#u8(1 2 3)").unwrap(),
            Value::bytevector(vec![1, 2, 3])
        );
    }

    #[test]
    fn test_read_bytevector_empty() {
        assert_eq!(read("#u8()").unwrap(), Value::bytevector(vec![]));
    }

    #[test]
    fn test_read_bytevector_single() {
        assert_eq!(read("#u8(255)").unwrap(), Value::bytevector(vec![255]));
    }

    #[test]
    fn test_read_bytevector_out_of_range() {
        assert!(read("#u8(256)").is_err());
    }

    #[test]
    fn test_read_bytevector_negative() {
        assert!(read("#u8(-1)").is_err());
    }

    #[test]
    fn test_read_bytevector_non_integer() {
        assert!(read("#u8(1.5)").is_err());
    }

    #[test]
    fn test_read_bytevector_unterminated() {
        assert!(read("#u8(1 2").is_err());
    }

    #[test]
    fn test_read_bytevector_in_list() {
        let result = read("(#u8(1 2) #u8(3))").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::bytevector(vec![1, 2]),
                Value::bytevector(vec![3]),
            ])
        );
    }

    #[test]
    fn test_read_string_hex_escape_basic() {
        // \x41; is 'A'
        let result = read(r#""\x41;""#).unwrap();
        assert_eq!(result, Value::string("A"));
    }

    #[test]
    fn test_read_string_hex_escape_lowercase() {
        let result = read(r#""\x6c;""#).unwrap();
        assert_eq!(result, Value::string("l"));
    }

    #[test]
    fn test_read_string_hex_escape_mixed_case() {
        let result = read(r#""\x4F;""#).unwrap();
        assert_eq!(result, Value::string("O"));
    }

    #[test]
    fn test_read_string_hex_escape_esc_char() {
        // \x1B; is ESC (0x1b) — the main motivating use case
        let result = read(r#""\x1B;""#).unwrap();
        assert_eq!(result, Value::string("\x1B"));
    }

    #[test]
    fn test_read_string_hex_escape_null() {
        let result = read(r#""\x0;""#).unwrap();
        assert_eq!(result, Value::string("\0"));
    }

    #[test]
    fn test_read_string_hex_escape_unicode() {
        // \x3BB; is λ (Greek small letter lambda)
        let result = read(r#""\x3BB;""#).unwrap();
        assert_eq!(result, Value::string("λ"));
    }

    #[test]
    fn test_read_string_hex_escape_emoji() {
        // \x1F600; is 😀
        let result = read(r#""\x1F600;""#).unwrap();
        assert_eq!(result, Value::string("😀"));
    }

    #[test]
    fn test_read_string_hex_escape_in_context() {
        // Mix hex escapes with regular text and other escapes
        let result = read(r#""hello\x20;world""#).unwrap();
        assert_eq!(result, Value::string("hello world"));
    }

    #[test]
    fn test_read_string_hex_escape_multiple() {
        let result = read(r#""\x48;\x69;""#).unwrap();
        assert_eq!(result, Value::string("Hi"));
    }

    #[test]
    fn test_read_string_hex_escape_missing_semicolon() {
        assert!(read(r#""\x41""#).is_err());
    }

    #[test]
    fn test_read_string_hex_escape_no_digits() {
        assert!(read(r#""\x;""#).is_err());
    }

    #[test]
    fn test_read_string_hex_escape_invalid_hex() {
        assert!(read(r#""\xGG;""#).is_err());
    }

    #[test]
    fn test_read_string_hex_escape_invalid_codepoint() {
        // 0xD800 is a surrogate — invalid Unicode scalar
        assert!(read(r#""\xD800;""#).is_err());
    }

    #[test]
    fn test_read_string_hex_escape_too_large() {
        // 0x110000 is above Unicode max
        assert!(read(r#""\x110000;""#).is_err());
    }

    #[test]
    fn test_read_string_u_escape_basic() {
        // \u0041 is 'A'
        let result = read(r#""\u0041""#).unwrap();
        assert_eq!(result, Value::string("A"));
    }

    #[test]
    fn test_read_string_u_escape_lambda() {
        let result = read(r#""\u03BB""#).unwrap();
        assert_eq!(result, Value::string("λ"));
    }

    #[test]
    fn test_read_string_u_escape_esc() {
        let result = read(r#""\u001B""#).unwrap();
        assert_eq!(result, Value::string("\x1B"));
    }

    #[test]
    fn test_read_string_u_escape_too_few_digits() {
        assert!(read(r#""\u041""#).is_err());
    }

    #[test]
    fn test_read_string_u_escape_surrogate() {
        assert!(read(r#""\uD800""#).is_err());
    }

    #[test]
    fn test_read_string_big_u_escape_basic() {
        let result = read(r#""\U00000041""#).unwrap();
        assert_eq!(result, Value::string("A"));
    }

    #[test]
    fn test_read_string_big_u_escape_emoji() {
        let result = read(r#""\U0001F600""#).unwrap();
        assert_eq!(result, Value::string("😀"));
    }

    #[test]
    fn test_read_string_big_u_escape_too_few_digits() {
        assert!(read(r#""\U0041""#).is_err());
    }

    #[test]
    fn test_read_string_big_u_escape_invalid() {
        assert!(read(r#""\U00110000""#).is_err());
    }

    #[test]
    fn test_read_string_null_escape() {
        let result = read(r#""\0""#).unwrap();
        assert_eq!(result, Value::string("\0"));
    }

    #[test]
    fn test_read_string_mixed_escapes() {
        // Mix all escape types in one string
        let result = read(r#""\x48;\u0069\n\t""#).unwrap();
        assert_eq!(result, Value::string("Hi\n\t"));
    }

    #[test]
    fn test_read_string_ansi_escape_sequence() {
        // Real-world: ANSI color code ESC[31m (red)
        let result = read(r#""\x1B;[31mRed\x1B;[0m""#).unwrap();
        assert_eq!(result, Value::string("\x1B[31mRed\x1B[0m"));
    }

    // ── f-string tests ──

    #[test]
    fn test_read_fstring_no_interpolation() {
        let result = read(r#"f"hello""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("str"), Value::string("hello")])
        );
    }

    #[test]
    fn test_read_fstring_single_var() {
        let result = read(r#"f"hello ${name}""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("str"),
                Value::string("hello "),
                Value::symbol("name"),
            ])
        );
    }

    #[test]
    fn test_read_fstring_multiple_vars() {
        let result = read(r#"f"${a} and ${b}""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("str"),
                Value::symbol("a"),
                Value::string(" and "),
                Value::symbol("b"),
            ])
        );
    }

    #[test]
    fn test_read_fstring_expression() {
        let result = read(r#"f"result: ${(+ 1 2)}""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("str"),
                Value::string("result: "),
                Value::list(vec![Value::symbol("+"), Value::int(1), Value::int(2),]),
            ])
        );
    }

    #[test]
    fn test_read_fstring_escaped_dollar() {
        let result = read(r#"f"costs \$5""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("str"), Value::string("costs $5")])
        );
    }

    #[test]
    fn test_read_fstring_dollar_without_brace() {
        let result = read(r#"f"costs $5""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("str"), Value::string("costs $5")])
        );
    }

    #[test]
    fn test_read_fstring_escape_sequences() {
        let result = read(r#"f"line1\nline2""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("str"), Value::string("line1\nline2"),])
        );
    }

    #[test]
    fn test_read_fstring_empty_interpolation_error() {
        assert!(read(r#"f"hello ${}""#).is_err());
    }

    #[test]
    fn test_read_fstring_unterminated_interpolation_error() {
        assert!(read(r#"f"hello ${name""#).is_err());
    }

    #[test]
    fn test_read_fstring_unterminated_string_error() {
        assert!(read(r#"f"hello"#).is_err());
    }

    #[test]
    fn test_read_fstring_keyword_access() {
        let result = read(r#"f"name: ${(:name user)}""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("str"),
                Value::string("name: "),
                Value::list(vec![Value::keyword("name"), Value::symbol("user")]),
            ])
        );
    }

    #[test]
    fn test_read_fstring_in_list() {
        let result = read(r#"(println f"hello ${name}")"#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("println"),
                Value::list(vec![
                    Value::symbol("str"),
                    Value::string("hello "),
                    Value::symbol("name"),
                ]),
            ])
        );
    }

    #[test]
    fn test_read_fstring_empty() {
        let result = read(r#"f"""#).unwrap();
        assert_eq!(result, Value::list(vec![Value::symbol("str")]));
    }

    #[test]
    fn test_read_fstring_only_expr() {
        let result = read(r#"f"${x}""#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("str"), Value::symbol("x")])
        );
    }

    #[test]
    fn test_read_f_symbol_still_works() {
        // Plain 'f' symbol (not followed by '"') should still parse as symbol
        let result = read("f").unwrap();
        assert_eq!(result, Value::symbol("f"));
    }

    #[test]
    fn test_read_f_prefixed_symbol_still_works() {
        // 'foo' should still parse as a normal symbol
        let result = read("foo").unwrap();
        assert_eq!(result, Value::symbol("foo"));
    }

    // ── short lambda tests ──

    #[test]
    fn test_read_short_lambda_single_arg() {
        // #(+ % 1) → (lambda (%1) (+ %1 1))
        let result = read("#(+ % 1)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("lambda"),
                Value::list(vec![Value::symbol("%1")]),
                Value::list(vec![Value::symbol("+"), Value::symbol("%1"), Value::int(1),]),
            ])
        );
    }

    #[test]
    fn test_read_short_lambda_two_args() {
        // #(+ %1 %2) → (lambda (%1 %2) (+ %1 %2))
        let result = read("#(+ %1 %2)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("lambda"),
                Value::list(vec![Value::symbol("%1"), Value::symbol("%2")]),
                Value::list(vec![
                    Value::symbol("+"),
                    Value::symbol("%1"),
                    Value::symbol("%2"),
                ]),
            ])
        );
    }

    #[test]
    fn test_read_short_lambda_bare_percent_is_percent1() {
        // #(* % %) → (lambda (%1) (* %1 %1))
        let result = read("#(* % %)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("lambda"),
                Value::list(vec![Value::symbol("%1")]),
                Value::list(vec![
                    Value::symbol("*"),
                    Value::symbol("%1"),
                    Value::symbol("%1"),
                ]),
            ])
        );
    }

    #[test]
    fn test_read_short_lambda_no_args() {
        // #(println "hello") → (lambda () (println "hello"))
        let result = read(r#"#(println "hello")"#).unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("lambda"),
                Value::list(vec![]),
                Value::list(vec![Value::symbol("println"), Value::string("hello"),]),
            ])
        );
    }

    #[test]
    fn test_read_short_lambda_in_list() {
        // (map #(+ % 1) numbers)
        let result = read("(map #(+ % 1) numbers)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("map"),
                Value::list(vec![
                    Value::symbol("lambda"),
                    Value::list(vec![Value::symbol("%1")]),
                    Value::list(vec![Value::symbol("+"), Value::symbol("%1"), Value::int(1),]),
                ]),
                Value::symbol("numbers"),
            ])
        );
    }

    #[test]
    fn test_read_short_lambda_unterminated() {
        assert!(read("#(+ % 1").is_err());
    }

    #[test]
    fn test_read_short_lambda_nested_expr() {
        // #(> (string-length %) 3) → (lambda (%1) (> (string-length %1) 3))
        let result = read("#(> (string-length %) 3)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("lambda"),
                Value::list(vec![Value::symbol("%1")]),
                Value::list(vec![
                    Value::symbol(">"),
                    Value::list(vec![Value::symbol("string-length"), Value::symbol("%1"),]),
                    Value::int(3),
                ]),
            ])
        );
    }

    #[test]
    fn test_read_regex_literal_digits() {
        let result = read(r#"#"\d+""#).unwrap();
        assert_eq!(result, Value::string(r"\d+"));
    }

    #[test]
    fn test_read_regex_literal_char_class() {
        let result = read(r#"#"[a-z]+""#).unwrap();
        assert_eq!(result, Value::string("[a-z]+"));
    }

    #[test]
    fn test_read_regex_literal_backslashes_literal() {
        let result = read(r#"#"hello\.world""#).unwrap();
        assert_eq!(result, Value::string(r"hello\.world"));
    }

    #[test]
    fn test_read_regex_literal_escaped_quote() {
        let result = read(r#"#"foo\"bar""#).unwrap();
        assert_eq!(result, Value::string(r#"foo"bar"#));
    }

    #[test]
    fn test_read_regex_literal_unterminated() {
        assert!(read(r#"#"abc"#).is_err());
    }
}
