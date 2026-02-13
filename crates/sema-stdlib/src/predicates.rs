use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "null?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("null?", "1", args.len()));
        }
        Ok(Value::Bool(match &args[0] {
            Value::Nil => true,
            Value::List(l) => l.is_empty(),
            _ => false,
        }))
    });

    register_fn(env, "list?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::List(_))))
    });

    register_fn(env, "vector?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("vector?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Vector(_))))
    });

    register_fn(env, "number?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("number?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Int(_) | Value::Float(_)
        )))
    });

    register_fn(env, "integer?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("integer?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Int(_))))
    });

    register_fn(env, "float?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("float?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Float(_))))
    });

    register_fn(env, "string?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::String(_))))
    });

    register_fn(env, "symbol?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("symbol?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Symbol(_))))
    });

    register_fn(env, "keyword?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("keyword?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Keyword(_))))
    });

    register_fn(env, "map?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("map?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Map(_))))
    });

    register_fn(env, "bool?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("bool?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Bool(_))))
    });

    register_fn(env, "nil?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("nil?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Nil)))
    });

    register_fn(env, "fn?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("fn?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Lambda(_) | Value::NativeFn(_)
        )))
    });

    register_fn(env, "prompt?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("prompt?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Prompt(_))))
    });

    register_fn(env, "conversation?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("conversation?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Conversation(_))))
    });

    register_fn(env, "type", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("type", "1", args.len()));
        }
        Ok(Value::keyword(args[0].type_name()))
    });

    register_fn(env, "pair?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("pair?", "1", args.len()));
        }
        Ok(Value::Bool(match &args[0] {
            Value::List(l) => !l.is_empty(),
            _ => false,
        }))
    });

    register_fn(env, "boolean?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("boolean?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Bool(_))))
    });

    register_fn(env, "procedure?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("procedure?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Lambda(_) | Value::NativeFn(_)
        )))
    });

    register_fn(env, "equal?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("equal?", "2", args.len()));
        }
        Ok(Value::Bool(args[0] == args[1]))
    });

    register_fn(env, "char?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Char(_))))
    });

    register_fn(env, "promise?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("promise?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(&args[0], Value::Thunk(_))))
    });

    register_fn(env, "promise-forced?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("promise-forced?", "1", args.len()));
        }
        match &args[0] {
            Value::Thunk(t) => Ok(Value::Bool(t.forced.borrow().is_some())),
            _ => Err(SemaError::type_error("promise", args[0].type_name())),
        }
    });
}
