use sema_core::{EvalContext, NativeFn, SemaError, Value};

pub fn register(env: &sema_core::Env) {
    register_fn_ctx(env, "context/set", |ctx, args| {
        if args.len() != 2 {
            return Err(SemaError::arity("context/set", "2", args.len()));
        }
        ctx.context_set(args[0].clone(), args[1].clone());
        Ok(Value::nil())
    });

    register_fn_ctx(env, "context/get", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/get", "1", args.len()));
        }
        Ok(ctx.context_get(&args[0]).unwrap_or_else(Value::nil))
    });

    register_fn_ctx(env, "context/has?", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/has?", "1", args.len()));
        }
        Ok(Value::bool(ctx.context_has(&args[0])))
    });

    register_fn_ctx(env, "context/remove", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/remove", "1", args.len()));
        }
        Ok(ctx.context_remove(&args[0]).unwrap_or_else(Value::nil))
    });

    register_fn_ctx(env, "context/all", |ctx, args| {
        if !args.is_empty() {
            return Err(SemaError::arity("context/all", "0", args.len()));
        }
        Ok(Value::map(ctx.context_all()))
    });

    register_fn_ctx(env, "context/pull", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/pull", "1", args.len()));
        }
        Ok(ctx.context_remove(&args[0]).unwrap_or_else(Value::nil))
    });
}

fn register_fn_ctx(
    env: &sema_core::Env,
    name: &str,
    f: impl Fn(&EvalContext, &[Value]) -> Result<Value, SemaError> + 'static,
) {
    env.set(
        sema_core::intern(name),
        Value::native_fn(NativeFn::with_ctx(name, f)),
    );
}
