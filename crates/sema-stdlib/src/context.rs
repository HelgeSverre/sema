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

    register_fn_ctx(env, "context/push", |ctx, args| {
        if args.len() != 2 {
            return Err(SemaError::arity("context/push", "2", args.len()));
        }
        ctx.context_stack_push(args[0].clone(), args[1].clone());
        Ok(Value::nil())
    });

    register_fn_ctx(env, "context/stack", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/stack", "1", args.len()));
        }
        Ok(Value::list(ctx.context_stack_get(&args[0])))
    });

    register_fn_ctx(env, "context/pop", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/pop", "1", args.len()));
        }
        Ok(ctx.context_stack_pop(&args[0]).unwrap_or_else(Value::nil))
    });

    register_fn_ctx(env, "context/set-hidden", |ctx, args| {
        if args.len() != 2 {
            return Err(SemaError::arity("context/set-hidden", "2", args.len()));
        }
        ctx.hidden_set(args[0].clone(), args[1].clone());
        Ok(Value::nil())
    });

    register_fn_ctx(env, "context/get-hidden", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/get-hidden", "1", args.len()));
        }
        Ok(ctx.hidden_get(&args[0]).unwrap_or_else(Value::nil))
    });

    register_fn_ctx(env, "context/has-hidden?", |ctx, args| {
        if args.len() != 1 {
            return Err(SemaError::arity("context/has-hidden?", "1", args.len()));
        }
        Ok(Value::bool(ctx.hidden_has(&args[0])))
    });

    // (context/with bindings-map thunk) -> result of thunk
    register_fn_ctx(env, "context/with", |ctx, args| {
        if args.len() != 2 {
            return Err(SemaError::arity("context/with", "2", args.len()));
        }
        let bindings = args[0]
            .as_map_rc()
            .ok_or_else(|| SemaError::type_error("map", args[0].type_name()))?;
        let thunk = &args[1];
        if thunk.as_lambda_rc().is_none() && thunk.as_native_fn_rc().is_none() {
            return Err(SemaError::type_error("function", thunk.type_name()));
        }

        let frame: std::collections::BTreeMap<Value, Value> = bindings
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        ctx.context_push_frame_with(frame);
        let result = sema_core::call_callback(ctx, thunk, &[]);
        ctx.context_pop_frame();
        result
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
