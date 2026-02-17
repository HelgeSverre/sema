use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "bit/and", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("bit/and", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a & b))
    });

    register_fn(env, "bit/or", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("bit/or", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a | b))
    });

    register_fn(env, "bit/xor", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("bit/xor", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a ^ b))
    });

    register_fn(env, "bit/not", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("bit/not", "1", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        Ok(Value::int(!a))
    });

    register_fn(env, "bit/shift-left", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("bit/shift-left", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a << n))
    });

    register_fn(env, "bit/shift-right", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("bit/shift-right", "2", args.len()));
        }
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a >> n))
    });
}
