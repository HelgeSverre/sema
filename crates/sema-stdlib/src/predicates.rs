use sema_core::{SemaError, Value, ValueView};

use crate::register_fn;

fn bool_pred(args: &[Value]) -> Result<Value, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("bool?", "1", args.len()));
    }
    Ok(Value::bool(args[0].as_bool().is_some()))
}

fn procedure_pred(args: &[Value]) -> Result<Value, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("fn?", "1", args.len()));
    }
    Ok(Value::bool(
        args[0].as_lambda_rc().is_some() || args[0].as_native_fn_rc().is_some(),
    ))
}

pub fn register(env: &sema_core::Env) {
    register_fn(env, "null?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("null?", "1", args.len()));
        }
        Ok(Value::bool(match args[0].view() {
            ValueView::Nil => true,
            ValueView::List(l) => l.is_empty(),
            _ => false,
        }))
    });

    register_fn(env, "list?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list?", "1", args.len()));
        }
        Ok(Value::bool(args[0].is_list()))
    });

    register_fn(env, "vector?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("vector?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_vector().is_some()))
    });

    register_fn(env, "number?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("number?", "1", args.len()));
        }
        Ok(Value::bool(args[0].is_int() || args[0].is_float()))
    });

    register_fn(env, "integer?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("integer?", "1", args.len()));
        }
        Ok(Value::bool(args[0].is_int()))
    });

    register_fn(env, "float?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("float?", "1", args.len()));
        }
        Ok(Value::bool(args[0].is_float()))
    });

    register_fn(env, "string?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_str().is_some()))
    });

    register_fn(env, "symbol?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("symbol?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_symbol_spur().is_some()))
    });

    register_fn(env, "keyword?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("keyword?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_keyword_spur().is_some()))
    });

    register_fn(env, "map?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("map?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_map_rc().is_some()))
    });

    register_fn(env, "bool?", bool_pred);
    register_fn(env, "boolean?", bool_pred);

    register_fn(env, "nil?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("nil?", "1", args.len()));
        }
        Ok(Value::bool(args[0].is_nil()))
    });

    register_fn(env, "fn?", procedure_pred);
    register_fn(env, "procedure?", procedure_pred);

    register_fn(env, "prompt?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("prompt?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_prompt_rc().is_some()))
    });

    register_fn(env, "conversation?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("conversation?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_conversation_rc().is_some()))
    });

    register_fn(env, "bytevector?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("bytevector?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_bytevector().is_some()))
    });

    register_fn(env, "record?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("record?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_record_rc().is_some()))
    });

    register_fn(env, "type", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("type", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Record(r) => Ok(Value::keyword_from_spur(r.type_tag)),
            _ => Ok(Value::keyword(args[0].type_name())),
        }
    });

    register_fn(env, "pair?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("pair?", "1", args.len()));
        }
        Ok(Value::bool(match args[0].view() {
            ValueView::List(l) => !l.is_empty(),
            _ => false,
        }))
    });

    register_fn(env, "equal?", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("equal?", "2", args.len()));
        }
        Ok(Value::bool(args[0] == args[1]))
    });

    register_fn(env, "char?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("char?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_char().is_some()))
    });

    register_fn(env, "promise?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("promise?", "1", args.len()));
        }
        Ok(Value::bool(args[0].as_thunk_rc().is_some()))
    });

    register_fn(env, "promise-forced?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("promise-forced?", "1", args.len()));
        }
        match args[0].view() {
            ValueView::Thunk(t) => Ok(Value::bool(t.forced.borrow().is_some())),
            _ => Err(SemaError::type_error("promise", args[0].type_name())),
        }
    });
}
