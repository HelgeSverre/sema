use sema_core::{check_arity, SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "bit/and", |args| {
        check_arity!(args, "bit/and", 2);
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a & b))
    });

    register_fn(env, "bit/or", |args| {
        check_arity!(args, "bit/or", 2);
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a | b))
    });

    register_fn(env, "bit/xor", |args| {
        check_arity!(args, "bit/xor", 2);
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let b = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a ^ b))
    });

    register_fn(env, "bit/not", |args| {
        check_arity!(args, "bit/not", 1);
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        Ok(Value::int(!a))
    });

    register_fn(env, "bit/shift-left", |args| {
        check_arity!(args, "bit/shift-left", 2);
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a << n))
    });

    register_fn(env, "bit/shift-right", |args| {
        check_arity!(args, "bit/shift-right", 2);
        let a = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        let n = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        Ok(Value::int(a >> n))
    });
}
