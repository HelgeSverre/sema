use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{SemaError, Span, Value};

use crate::lexer::{tokenize, SpannedToken, Token};

struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<SpannedToken>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|t| &t.token)
    }

    fn span(&self) -> Span {
        self.tokens
            .get(self.pos)
            .map(|t| t.span.clone())
            .unwrap_or(Span { line: 0, col: 0 })
    }

    fn advance(&mut self) -> Option<&SpannedToken> {
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
                message: format!("expected {expected:?}, got {:?}", t.token),
                span,
            }),
            None => Err(SemaError::Reader {
                message: format!("expected {expected:?}, got end of input"),
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
                let inner = self.parse_expr()?;
                Ok(Value::list(vec![Value::symbol("quote"), inner]))
            }
            Some(Token::Quasiquote) => {
                self.advance();
                let inner = self.parse_expr()?;
                Ok(Value::list(vec![Value::symbol("quasiquote"), inner]))
            }
            Some(Token::Unquote) => {
                self.advance();
                let inner = self.parse_expr()?;
                Ok(Value::list(vec![Value::symbol("unquote"), inner]))
            }
            Some(Token::UnquoteSplice) => {
                self.advance();
                let inner = self.parse_expr()?;
                Ok(Value::list(vec![Value::symbol("unquote-splicing"), inner]))
            }
            Some(_) => self.parse_atom(),
        }
    }

    fn parse_list(&mut self) -> Result<Value, SemaError> {
        self.expect(&Token::LParen)?;
        let mut items = Vec::new();
        while self.peek() != Some(&Token::RParen) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated list".to_string(),
                    span: self.span(),
                });
            }
            // Handle dotted pairs: (a . b)
            if self.peek() == Some(&Token::Dot) {
                self.advance(); // skip dot
                let cdr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                // Build a proper dotted pair using cons cells
                // For now, represent as a special list
                // In a full implementation this would be a cons cell
                // We'll use a list with a special marker for now
                items.push(Value::symbol("."));
                items.push(cdr);
                return Ok(Value::List(Rc::new(items)));
            }
            items.push(self.parse_expr()?);
        }
        self.expect(&Token::RParen)?;
        Ok(Value::List(Rc::new(items)))
    }

    fn parse_vector(&mut self) -> Result<Value, SemaError> {
        self.expect(&Token::LBracket)?;
        let mut items = Vec::new();
        while self.peek() != Some(&Token::RBracket) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated vector".to_string(),
                    span: self.span(),
                });
            }
            items.push(self.parse_expr()?);
        }
        self.expect(&Token::RBracket)?;
        Ok(Value::Vector(Rc::new(items)))
    }

    fn parse_map(&mut self) -> Result<Value, SemaError> {
        self.expect(&Token::LBrace)?;
        let mut map = BTreeMap::new();
        while self.peek() != Some(&Token::RBrace) {
            if self.peek().is_none() {
                return Err(SemaError::Reader {
                    message: "unterminated map".to_string(),
                    span: self.span(),
                });
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
        Ok(Value::Map(Rc::new(map)))
    }

    fn parse_atom(&mut self) -> Result<Value, SemaError> {
        let span = self.span();
        match self.advance() {
            Some(SpannedToken { token: Token::Int(n), .. }) => Ok(Value::Int(*n)),
            Some(SpannedToken { token: Token::Float(f), .. }) => Ok(Value::Float(*f)),
            Some(SpannedToken { token: Token::String(s), .. }) => {
                Ok(Value::String(Rc::new(s.clone())))
            }
            Some(SpannedToken { token: Token::Symbol(s), .. }) => {
                if s == "nil" {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Symbol(Rc::new(s.clone())))
                }
            }
            Some(SpannedToken { token: Token::Keyword(s), .. }) => {
                Ok(Value::Keyword(Rc::new(s.clone())))
            }
            Some(SpannedToken { token: Token::Bool(b), .. }) => Ok(Value::Bool(*b)),
            Some(t) => Err(SemaError::Reader {
                message: format!("unexpected token: {:?}", t.token),
                span,
            }),
            None => Err(SemaError::Reader {
                message: "unexpected end of input".to_string(),
                span,
            }),
        }
    }
}

/// Read a single s-expression from a string.
pub fn read(input: &str) -> Result<Value, SemaError> {
    let tokens = tokenize(input)?;
    if tokens.is_empty() {
        return Ok(Value::Nil);
    }
    let mut parser = Parser::new(tokens);
    parser.parse_expr()
}

/// Read all s-expressions from a string.
pub fn read_many(input: &str) -> Result<Vec<Value>, SemaError> {
    let tokens = tokenize(input)?;
    if tokens.is_empty() {
        return Ok(Vec::new());
    }
    let mut parser = Parser::new(tokens);
    let mut exprs = Vec::new();
    while parser.peek().is_some() {
        exprs.push(parser.parse_expr()?);
    }
    Ok(exprs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_int() {
        assert_eq!(read("42").unwrap(), Value::Int(42));
    }

    #[test]
    fn test_read_negative_int() {
        assert_eq!(read("-7").unwrap(), Value::Int(-7));
    }

    #[test]
    fn test_read_float() {
        assert_eq!(read("3.14").unwrap(), Value::Float(3.14));
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
        assert_eq!(read("#t").unwrap(), Value::Bool(true));
        assert_eq!(read("#f").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_read_list() {
        let result = read("(+ 1 2)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::symbol("+"), Value::Int(1), Value::Int(2)])
        );
    }

    #[test]
    fn test_read_nested_list() {
        let result = read("(* (+ 1 2) 3)").unwrap();
        assert_eq!(
            result,
            Value::list(vec![
                Value::symbol("*"),
                Value::list(vec![Value::symbol("+"), Value::Int(1), Value::Int(2)]),
                Value::Int(3)
            ])
        );
    }

    #[test]
    fn test_read_vector() {
        let result = read("[1 2 3]").unwrap();
        assert_eq!(
            result,
            Value::vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn test_read_map() {
        let result = read("{:a 1 :b 2}").unwrap();
        let mut expected = BTreeMap::new();
        expected.insert(Value::keyword("a"), Value::Int(1));
        expected.insert(Value::keyword("b"), Value::Int(2));
        assert_eq!(result, Value::Map(Rc::new(expected)));
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
        assert_eq!(read("nil").unwrap(), Value::Nil);
    }

    #[test]
    fn test_read_many_exprs() {
        let results = read_many("1 2 3").unwrap();
        assert_eq!(results, vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
    }

    #[test]
    fn test_comments() {
        let result = read_many("; comment\n(+ 1 2)").unwrap();
        assert_eq!(result.len(), 1);
    }
}
