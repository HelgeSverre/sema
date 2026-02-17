use sema_core::{SemaError, Value, ValueView};
use unicode_normalization::UnicodeNormalization;

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "string-append", |args| {
        let mut result = String::new();
        for arg in args {
            if let Some(s) = arg.as_str() {
                result.push_str(s);
            } else {
                result.push_str(&arg.to_string());
            }
        }
        Ok(Value::string(&result))
    });

    register_fn(env, "string-length", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string-length", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::int(s.chars().count() as i64))
    });

    register_fn(env, "string-ref", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string-ref", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let idx = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as usize;
        s.chars()
            .nth(idx)
            .map(Value::char)
            .ok_or_else(|| SemaError::eval(format!("string-ref: index {idx} out of bounds")))
    });

    register_fn(env, "substring", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("substring", "2-3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let start = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as usize;
        let char_count = s.chars().count();
        let end = if args.len() == 3 {
            args[2]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?
                as usize
        } else {
            char_count
        };
        if start > char_count || end > char_count || start > end {
            return Err(SemaError::eval("substring: index out of bounds"));
        }
        let start_byte = s
            .char_indices()
            .nth(start)
            .map(|(i, _)| i)
            .unwrap_or(s.len());
        let end_byte = if end == char_count {
            s.len()
        } else {
            s.char_indices().nth(end).map(|(i, _)| i).unwrap_or(s.len())
        };
        Ok(Value::string(&s[start_byte..end_byte]))
    });

    register_fn(env, "string/split", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/split", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let sep = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let parts: Vec<Value> = s.split(sep).map(Value::string).collect();
        Ok(Value::list(parts))
    });

    register_fn(env, "string/trim", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/trim", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(s.trim()))
    });

    register_fn(env, "string/contains?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/contains?", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let sub = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::bool(s.contains(sub)))
    });

    register_fn(env, "string/starts-with?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/starts-with?", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let prefix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::bool(s.starts_with(prefix)))
    });

    register_fn(env, "string/ends-with?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/ends-with?", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let suffix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::bool(s.ends_with(suffix)))
    });

    register_fn(env, "string/upper", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/upper", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(&s.to_uppercase()))
    });

    register_fn(env, "string/lower", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/lower", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(&s.to_lowercase()))
    });

    register_fn(env, "string/replace", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("string/replace", "3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let from = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let to = args[2]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
        Ok(Value::string(&s.replace(from, to)))
    });

    register_fn(env, "string/join", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/join", "2", args.len()));
        }
        let sep = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let items = match args[0].view() {
            ValueView::List(l) => l,
            ValueView::Vector(v) => v,
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };
        let strs: Vec<String> = items
            .iter()
            .map(|v| {
                if let Some(s) = v.as_str() {
                    s.to_string()
                } else {
                    v.to_string()
                }
            })
            .collect();
        Ok(Value::string(&strs.join(sep)))
    });

    register_fn(env, "format", |args| {
        if args.is_empty() {
            return Err(SemaError::arity("format", "1+", 0));
        }
        let fmt = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut result = String::new();
        let mut arg_idx = 1;
        let mut chars = fmt.chars();
        while let Some(ch) = chars.next() {
            if ch == '~' {
                match chars.next() {
                    Some('a') | Some('A') => {
                        // ~a: display (no quotes)
                        if arg_idx < args.len() {
                            if let Some(s) = args[arg_idx].as_str() {
                                result.push_str(s);
                            } else {
                                result.push_str(&args[arg_idx].to_string());
                            }
                            arg_idx += 1;
                        }
                    }
                    Some('s') | Some('S') => {
                        // ~s: write (with quotes)
                        if arg_idx < args.len() {
                            result.push_str(&args[arg_idx].to_string());
                            arg_idx += 1;
                        }
                    }
                    Some('%') => result.push('\n'),
                    Some('~') => result.push('~'),
                    Some(other) => {
                        result.push('~');
                        result.push(other);
                    }
                    None => result.push('~'),
                }
            } else {
                result.push(ch);
            }
        }
        Ok(Value::string(&result))
    });

    register_fn(env, "string->symbol", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->symbol", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::symbol(s))
    });

    register_fn(env, "symbol->string", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("symbol->string", "1", args.len()));
        }
        let s = args[0]
            .as_symbol()
            .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
        Ok(Value::string(&s))
    });

    register_fn(env, "string->keyword", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->keyword", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::keyword(s))
    });

    register_fn(env, "keyword->string", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("keyword->string", "1", args.len()));
        }
        let kw = args[0]
            .as_keyword()
            .ok_or_else(|| SemaError::type_error("keyword", args[0].type_name()))?;
        Ok(Value::string(&kw))
    });

    register_fn(env, "str", |args| {
        let mut result = String::new();
        for arg in args {
            if let Some(s) = arg.as_str() {
                result.push_str(s);
            } else {
                result.push_str(&arg.to_string());
            }
        }
        Ok(Value::string(&result))
    });

    register_fn(env, "number->string", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("number->string", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Int(n) => Ok(Value::string(&n.to_string())),
            ValueView::Float(f) => Ok(Value::string(&f.to_string())),
            _ => Err(SemaError::type_error("number", args[0].type_name())),
        }
    });

    register_fn(env, "string->number", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->number", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        if let Ok(n) = s.parse::<i64>() {
            Ok(Value::int(n))
        } else if let Ok(f) = s.parse::<f64>() {
            Ok(Value::float(f))
        } else {
            Err(SemaError::eval(format!("cannot parse '{s}' as number")))
        }
    });

    register_fn(env, "string->float", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->float", "1", args.len()));
        }
        match args[0].view() {
            ValueView::String(s) => s
                .parse::<f64>()
                .map(Value::float)
                .map_err(|_| SemaError::eval(format!("cannot parse '{s}' as float"))),
            ValueView::Int(n) => Ok(Value::float(n as f64)),
            ValueView::Float(_) => Ok(args[0].clone()),
            _ => Err(SemaError::type_error(
                "string or number",
                args[0].type_name(),
            )),
        }
    });

    register_fn(env, "string/index-of", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/index-of", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let sub = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.find(sub) {
            Some(idx) => Ok(Value::int(idx as i64)),
            None => Ok(Value::nil()),
        }
    });

    register_fn(env, "string/chars", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/chars", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let chars: Vec<Value> = s.chars().map(Value::char).collect();
        Ok(Value::list(chars))
    });

    register_fn(env, "string/repeat", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/repeat", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as usize;
        Ok(Value::string(&s.repeat(n)))
    });

    register_fn(env, "string/trim-left", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/trim-left", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(s.trim_start()))
    });

    register_fn(env, "string/trim-right", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/trim-right", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(s.trim_end()))
    });

    register_fn(env, "string/number?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/number?", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let is_num = s.parse::<i64>().is_ok() || s.parse::<f64>().is_ok();
        Ok(Value::bool(is_num))
    });

    register_fn(env, "string/pad-left", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("string/pad-left", "2-3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let width = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as usize;
        let pad_char = if args.len() == 3 {
            let p = args[2]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
            p.chars().next().unwrap_or(' ')
        } else {
            ' '
        };
        let char_len = s.chars().count();
        if char_len >= width {
            Ok(Value::string(s))
        } else {
            let padding: String = std::iter::repeat_n(pad_char, width - char_len).collect();
            Ok(Value::string(&format!("{}{}", padding, s)))
        }
    });

    register_fn(env, "string/pad-right", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("string/pad-right", "2-3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let width = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as usize;
        let pad_char = if args.len() == 3 {
            let p = args[2]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
            p.chars().next().unwrap_or(' ')
        } else {
            ' '
        };
        let char_len = s.chars().count();
        if char_len >= width {
            Ok(Value::string(s))
        } else {
            let padding: String = std::iter::repeat_n(pad_char, width - char_len).collect();
            Ok(Value::string(&format!("{}{}", s, padding)))
        }
    });

    register_fn(env, "string/last-index-of", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/last-index-of", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let sub = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.rfind(sub) {
            Some(idx) => Ok(Value::int(idx as i64)),
            None => Ok(Value::nil()),
        }
    });

    register_fn(env, "string/reverse", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/reverse", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(&s.chars().rev().collect::<String>()))
    });

    register_fn(env, "string/empty?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/empty?", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::bool(s.is_empty()))
    });

    register_fn(env, "string/capitalize", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/capitalize", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut chars = s.chars();
        let result = match chars.next() {
            Some(first) => {
                let mut r = first.to_uppercase().to_string();
                for c in chars {
                    r.extend(c.to_lowercase());
                }
                r
            }
            None => String::new(),
        };
        Ok(Value::string(&result))
    });

    register_fn(env, "string/title-case", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/title-case", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let result: Vec<String> = s
            .split_whitespace()
            .map(|word| {
                let mut chars = word.chars();
                match chars.next() {
                    Some(first) => {
                        let mut w = first.to_uppercase().to_string();
                        for c in chars {
                            w.extend(c.to_lowercase());
                        }
                        w
                    }
                    None => String::new(),
                }
            })
            .collect();
        Ok(Value::string(&result.join(" ")))
    });

    // Character functions

    register_fn(env, "char->integer", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char->integer", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::int(c as i64))
    });

    register_fn(env, "integer->char", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("integer->char", "1", args.len()));
        }
        let n = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let c = char::from_u32(n as u32)
            .ok_or_else(|| SemaError::eval(format!("integer->char: invalid codepoint {n}")))?;
        Ok(Value::char(c))
    });

    register_fn(env, "char-alphabetic?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char-alphabetic?", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::bool(c.is_alphabetic()))
    });

    register_fn(env, "char-numeric?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char-numeric?", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::bool(c.is_numeric()))
    });

    register_fn(env, "char-whitespace?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char-whitespace?", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::bool(c.is_whitespace()))
    });

    register_fn(env, "char-upper-case?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char-upper-case?", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::bool(c.is_uppercase()))
    });

    register_fn(env, "char-lower-case?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char-lower-case?", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::bool(c.is_lowercase()))
    });

    register_fn(env, "char-upcase", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char-upcase", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::char(c.to_uppercase().next().unwrap_or(c)))
    });

    register_fn(env, "char-downcase", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char-downcase", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::char(c.to_lowercase().next().unwrap_or(c)))
    });

    register_fn(env, "char->string", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char->string", "1", args.len()));
        }
        let c = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        Ok(Value::string(&c.to_string()))
    });

    register_fn(env, "string->char", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->char", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut chars = s.chars();
        let c = chars
            .next()
            .ok_or_else(|| SemaError::eval("string->char: empty string"))?;
        if chars.next().is_some() {
            return Err(SemaError::eval(
                "string->char: string must have exactly one character",
            ));
        }
        Ok(Value::char(c))
    });

    register_fn(env, "string->list", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->list", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let chars: Vec<Value> = s.chars().map(Value::char).collect();
        Ok(Value::list(chars))
    });

    fn two_chars(op: &str, args: &[Value]) -> Result<(char, char), SemaError> {
        if args.len() != 2 {
            return Err(SemaError::arity(op, "2", args.len()));
        }
        let a = args[0]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[0].type_name()))?;
        let b = args[1]
            .as_char()
            .ok_or_else(|| SemaError::type_error("char", args[1].type_name()))?;
        Ok((a, b))
    }

    register_fn(env, "char=?", |args| {
        let (a, b) = two_chars("char=?", args)?;
        Ok(Value::bool(a == b))
    });
    register_fn(env, "char<?", |args| {
        let (a, b) = two_chars("char<?", args)?;
        Ok(Value::bool(a < b))
    });
    register_fn(env, "char>?", |args| {
        let (a, b) = two_chars("char>?", args)?;
        Ok(Value::bool(a > b))
    });
    register_fn(env, "char<=?", |args| {
        let (a, b) = two_chars("char<=?", args)?;
        Ok(Value::bool(a <= b))
    });
    register_fn(env, "char>=?", |args| {
        let (a, b) = two_chars("char>=?", args)?;
        Ok(Value::bool(a >= b))
    });

    fn two_chars_ci(op: &str, args: &[Value]) -> Result<(char, char), SemaError> {
        let (a, b) = two_chars(op, args)?;
        let a = a.to_lowercase().next().unwrap_or(a);
        let b = b.to_lowercase().next().unwrap_or(b);
        Ok((a, b))
    }

    register_fn(env, "char-ci=?", |args| {
        let (a, b) = two_chars_ci("char-ci=?", args)?;
        Ok(Value::bool(a == b))
    });
    register_fn(env, "char-ci<?", |args| {
        let (a, b) = two_chars_ci("char-ci<?", args)?;
        Ok(Value::bool(a < b))
    });
    register_fn(env, "char-ci>?", |args| {
        let (a, b) = two_chars_ci("char-ci>?", args)?;
        Ok(Value::bool(a > b))
    });
    register_fn(env, "char-ci<=?", |args| {
        let (a, b) = two_chars_ci("char-ci<=?", args)?;
        Ok(Value::bool(a <= b))
    });
    register_fn(env, "char-ci>=?", |args| {
        let (a, b) = two_chars_ci("char-ci>=?", args)?;
        Ok(Value::bool(a >= b))
    });

    register_fn(env, "list->string", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list->string", "1", args.len()));
        }
        let items = args[0]
            .as_list()
            .ok_or_else(|| SemaError::type_error("list", args[0].type_name()))?;
        let mut s = String::with_capacity(items.len());
        for item in items {
            let c = item
                .as_char()
                .ok_or_else(|| SemaError::type_error("char", item.type_name()))?;
            s.push(c);
        }
        Ok(Value::string(&s))
    });

    register_fn(env, "string/map", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/map", "2", args.len()));
        }
        let s = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let mut result = String::with_capacity(s.len());
        for ch in s.chars() {
            let mapped = crate::list::call_function(&args[0], &[Value::char(ch)])?;
            if let Some(c) = mapped.as_char() {
                result.push(c);
            } else if let Some(s) = mapped.as_str() {
                result.push_str(s);
            } else {
                return Err(SemaError::type_error("char or string", mapped.type_name()));
            }
        }
        Ok(Value::string(&result))
    });

    register_fn(env, "string/byte-length", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/byte-length", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::int(s.len() as i64))
    });

    register_fn(env, "string/codepoints", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/codepoints", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let codepoints: Vec<Value> = s.chars().map(|c| Value::int(c as u32 as i64)).collect();
        Ok(Value::list(codepoints))
    });

    register_fn(env, "string/from-codepoints", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/from-codepoints", "1", args.len()));
        }
        let items = match args[0].view() {
            ValueView::List(l) => l,
            ValueView::Vector(v) => v,
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };
        let mut s = String::with_capacity(items.len());
        for item in items.iter() {
            let n = item
                .as_int()
                .ok_or_else(|| SemaError::type_error("integer", item.type_name()))?;
            let c = char::from_u32(n as u32).ok_or_else(|| {
                SemaError::eval(format!("string/from-codepoints: invalid codepoint {n}"))
            })?;
            s.push(c);
        }
        Ok(Value::string(&s))
    });

    register_fn(env, "string/normalize", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/normalize", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let form = args[1]
            .as_str()
            .map(|s| s.to_string())
            .or_else(|| args[1].as_keyword())
            .ok_or_else(|| SemaError::type_error("string or keyword", args[1].type_name()))?;
        let normalized = match form.to_lowercase().as_str() {
            "nfc" => s.nfc().collect::<String>(),
            "nfd" => s.nfd().collect::<String>(),
            "nfkc" => s.nfkc().collect::<String>(),
            "nfkd" => s.nfkd().collect::<String>(),
            _ => {
                return Err(SemaError::eval(format!(
                    "string/normalize: unknown form {:?}",
                    form
                )))
            }
        };
        Ok(Value::string(&normalized))
    });

    register_fn(env, "string/foldcase", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/foldcase", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(&s.to_lowercase()))
    });

    register_fn(env, "string-ci=?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string-ci=?", "2", args.len()));
        }
        let a = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let b = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::bool(a.to_lowercase() == b.to_lowercase()))
    });

    // string/after — everything after first occurrence of needle
    register_fn(env, "string/after", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/after", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let needle = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.find(needle) {
            Some(idx) => Ok(Value::string(&s[idx + needle.len()..])),
            None => Ok(Value::string(s)),
        }
    });

    // string/after-last — everything after last occurrence of needle
    register_fn(env, "string/after-last", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/after-last", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let needle = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.rfind(needle) {
            Some(idx) => Ok(Value::string(&s[idx + needle.len()..])),
            None => Ok(Value::string(s)),
        }
    });

    // string/before — everything before first occurrence of needle
    register_fn(env, "string/before", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/before", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let needle = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.find(needle) {
            Some(idx) => Ok(Value::string(&s[..idx])),
            None => Ok(Value::string(s)),
        }
    });

    // string/before-last — everything before last occurrence of needle
    register_fn(env, "string/before-last", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/before-last", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let needle = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.rfind(needle) {
            Some(idx) => Ok(Value::string(&s[..idx])),
            None => Ok(Value::string(s)),
        }
    });

    // string/between — portion between first occurrence of left and first occurrence of right after it
    register_fn(env, "string/between", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("string/between", "3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let left = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let right = args[2]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
        match s.find(left) {
            Some(l_idx) => {
                let after_left = &s[l_idx + left.len()..];
                match after_left.find(right) {
                    Some(r_idx) => Ok(Value::string(&after_left[..r_idx])),
                    None => Ok(Value::string(after_left)),
                }
            }
            None => Ok(Value::string("")),
        }
    });

    // string/chop-start — remove prefix if present
    register_fn(env, "string/chop-start", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/chop-start", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let prefix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.strip_prefix(prefix) {
            Some(rest) => Ok(Value::string(rest)),
            None => Ok(Value::string(s)),
        }
    });

    // string/chop-end — remove suffix if present
    register_fn(env, "string/chop-end", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/chop-end", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let suffix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        match s.strip_suffix(suffix) {
            Some(rest) => Ok(Value::string(rest)),
            None => Ok(Value::string(s)),
        }
    });

    // string/ensure-start — ensure string starts with prefix (add if missing)
    register_fn(env, "string/ensure-start", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/ensure-start", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let prefix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        if s.starts_with(prefix) {
            Ok(Value::string(s))
        } else {
            Ok(Value::string(&format!("{}{}", prefix, s)))
        }
    });

    // string/ensure-end — ensure string ends with suffix (add if missing)
    register_fn(env, "string/ensure-end", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/ensure-end", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let suffix = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        if s.ends_with(suffix) {
            Ok(Value::string(s))
        } else {
            Ok(Value::string(&format!("{}{}", s, suffix)))
        }
    });

    // string/replace-first — replace only first occurrence
    register_fn(env, "string/replace-first", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("string/replace-first", "3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let from = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let to = args[2]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
        match s.find(from) {
            Some(idx) => {
                let mut result = String::with_capacity(s.len());
                result.push_str(&s[..idx]);
                result.push_str(to);
                result.push_str(&s[idx + from.len()..]);
                Ok(Value::string(&result))
            }
            None => Ok(Value::string(s)),
        }
    });

    // string/replace-last — replace only last occurrence
    register_fn(env, "string/replace-last", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("string/replace-last", "3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let from = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let to = args[2]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
        match s.rfind(from) {
            Some(idx) => {
                let mut result = String::with_capacity(s.len());
                result.push_str(&s[..idx]);
                result.push_str(to);
                result.push_str(&s[idx + from.len()..]);
                Ok(Value::string(&result))
            }
            None => Ok(Value::string(s)),
        }
    });

    // string/remove — remove all occurrences of substring
    register_fn(env, "string/remove", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/remove", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let needle = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        Ok(Value::string(&s.replace(needle, "")))
    });

    // string/take — first N chars (positive) or last N chars (negative)
    register_fn(env, "string/take", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("string/take", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        let char_count = s.chars().count() as i64;
        if n >= 0 {
            let take = (n as usize).min(char_count as usize);
            Ok(Value::string(&s.chars().take(take).collect::<String>()))
        } else {
            let take = ((-n) as usize).min(char_count as usize);
            let skip = char_count as usize - take;
            Ok(Value::string(&s.chars().skip(skip).collect::<String>()))
        }
    });

    fn split_identifier_words(s: &str) -> Vec<String> {
        let mut words = Vec::new();
        let mut current = String::new();
        let mut prev_was_upper = false;
        let mut prev_was_sep = true;
        for ch in s.chars() {
            if ch == '_' || ch == '-' || ch == ' ' || ch == '.' {
                if !current.is_empty() {
                    words.push(current.clone());
                    current.clear();
                }
                prev_was_upper = false;
                prev_was_sep = true;
            } else if ch.is_uppercase() {
                if !current.is_empty() && (!prev_was_upper || prev_was_sep) {
                    words.push(current.clone());
                    current.clear();
                } else if !current.is_empty() && prev_was_upper && current.len() > 1 {
                    // Handle acronyms like "HTMLParser" -> ["HTML", "Parser"]
                    // We need to peek ahead — but since we don't have peek here,
                    // we'll handle it simply: consecutive uppercase stays together
                    // until a lowercase follows
                }
                current.push(ch);
                prev_was_upper = true;
                prev_was_sep = false;
            } else if ch.is_lowercase() && prev_was_upper && current.len() > 1 {
                // Transition from uppercase run to lowercase: split before last uppercase
                let last = current.pop().unwrap();
                if !current.is_empty() {
                    words.push(current.clone());
                    current.clear();
                }
                current.push(last);
                current.push(ch);
                prev_was_upper = false;
                prev_was_sep = false;
            } else {
                current.push(ch);
                prev_was_upper = false;
                prev_was_sep = false;
            }
        }
        if !current.is_empty() {
            words.push(current);
        }
        words
    }

    register_fn(env, "string/snake-case", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/snake-case", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let words = split_identifier_words(s);
        let result: Vec<String> = words.iter().map(|w| w.to_lowercase()).collect();
        Ok(Value::string(&result.join("_")))
    });

    register_fn(env, "string/kebab-case", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/kebab-case", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let words = split_identifier_words(s);
        let result: Vec<String> = words.iter().map(|w| w.to_lowercase()).collect();
        Ok(Value::string(&result.join("-")))
    });

    register_fn(env, "string/camel-case", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/camel-case", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let words = split_identifier_words(s);
        let mut result = String::new();
        for (i, word) in words.iter().enumerate() {
            if i == 0 {
                result.push_str(&word.to_lowercase());
            } else {
                let mut chars = word.chars();
                if let Some(first) = chars.next() {
                    result.extend(first.to_uppercase());
                    result.push_str(&chars.collect::<String>().to_lowercase());
                }
            }
        }
        Ok(Value::string(&result))
    });

    register_fn(env, "string/pascal-case", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/pascal-case", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let words = split_identifier_words(s);
        let mut result = String::new();
        for word in &words {
            let mut chars = word.chars();
            if let Some(first) = chars.next() {
                result.extend(first.to_uppercase());
                result.push_str(&chars.collect::<String>().to_lowercase());
            }
        }
        Ok(Value::string(&result))
    });

    register_fn(env, "string/headline", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/headline", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let words = split_identifier_words(s);
        let result: Vec<String> = words
            .iter()
            .map(|word| {
                let mut chars = word.chars();
                match chars.next() {
                    Some(first) => {
                        let mut w = first.to_uppercase().to_string();
                        w.push_str(&chars.collect::<String>().to_lowercase());
                        w
                    }
                    None => String::new(),
                }
            })
            .collect();
        Ok(Value::string(&result.join(" ")))
    });

    register_fn(env, "string/words", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string/words", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let words = split_identifier_words(s);
        Ok(Value::list(
            words.into_iter().map(|w| Value::string(&w)).collect(),
        ))
    });

    // string/wrap — wrap string with left and right delimiters
    register_fn(env, "string/wrap", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("string/wrap", "2-3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let left = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let right = if args.len() == 3 {
            args[2]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?
        } else {
            left
        };
        Ok(Value::string(&format!("{}{}{}", left, s, right)))
    });

    // string/unwrap — remove surrounding delimiters if both present
    register_fn(env, "string/unwrap", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("string/unwrap", "2-3", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let left = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let right = if args.len() == 3 {
            args[2]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?
        } else {
            left
        };
        if s.starts_with(left) && s.ends_with(right) && s.len() >= left.len() + right.len() {
            Ok(Value::string(&s[left.len()..s.len() - right.len()]))
        } else {
            Ok(Value::string(s))
        }
    });
}
