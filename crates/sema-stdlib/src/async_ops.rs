use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::rc::Rc;

use sema_core::{
    call_run_scheduler, call_run_scheduler_all_of, call_run_scheduler_any_of,
    call_run_scheduler_timeout, call_spawn_callback, check_arity, in_async_context,
    set_yield_signal, take_resume_value, AsyncPromise, Channel, Env, EvalContext, NativeFn,
    PromiseState, SchedulerRunResult, SemaError, Value, ValueView, YieldReason,
};

use crate::register_fn;

// ── Helpers ──────────────────────────────────────────────────────

fn register_fn_ctx(
    env: &Env,
    name: &str,
    f: impl Fn(&EvalContext, &[Value]) -> Result<Value, SemaError> + 'static,
) {
    env.set(
        sema_core::intern(name),
        Value::native_fn(NativeFn::with_ctx(name, f)),
    );
}

fn expect_promise(args: &[Value], _name: &str, idx: usize) -> Result<Rc<AsyncPromise>, SemaError> {
    match args[idx].view() {
        ValueView::AsyncPromise(p) => Ok(p),
        _ => Err(SemaError::type_error_with_value(
            "async-promise",
            args[idx].type_name(),
            &args[idx],
        )),
    }
}

fn expect_channel(args: &[Value], _name: &str, idx: usize) -> Result<Rc<Channel>, SemaError> {
    match args[idx].view() {
        ValueView::Channel(c) => Ok(c),
        _ => Err(SemaError::type_error_with_value(
            "channel",
            args[idx].type_name(),
            &args[idx],
        )),
    }
}

/// Extract items from a list or vector, or return a type error.
fn expect_list_or_vector<'a>(val: &'a Value, name: &str) -> Result<&'a [Value], SemaError> {
    if let Some(items) = val.as_list() {
        Ok(items)
    } else if let Some(items) = val.as_vector() {
        Ok(items)
    } else {
        Err(SemaError::type_error("list or vector", val.type_name())
            .with_hint(format!("{name} expects a list or vector of promises")))
    }
}

// ── Registration ─────────────────────────────────────────────────

pub fn register(env: &Env) {
    register_predicates(env);
    register_promise_ops(env);
    register_channel_ops(env);
}

// ── Predicates ───────────────────────────────────────────────────

fn register_predicates(env: &Env) {
    register_fn(env, "async/promise?", |args| {
        check_arity!(args, "async/promise?", 1);
        Ok(Value::bool(args[0].is_async_promise()))
    });

    register_fn(env, "channel?", |args| {
        check_arity!(args, "channel?", 1);
        Ok(Value::bool(args[0].is_channel()))
    });
}

// ── Promise operations ───────────────────────────────────────────

fn register_promise_ops(env: &Env) {
    // async/spawn — spawn a thunk as an async task, returns a promise
    register_fn_ctx(env, "async/spawn", |ctx, args| {
        check_arity!(args, "async/spawn", 1);
        call_spawn_callback(ctx, args[0].clone())
    });

    // async/await — wait for a promise to resolve
    register_fn_ctx(env, "async/await", |ctx, args| {
        check_arity!(args, "async/await", 1);
        let promise = expect_promise(args, "async/await", 0)?;

        // Check for resume value first (we're resuming from a yield)
        if let Some(val) = take_resume_value() {
            return Ok(val);
        }

        // If already resolved, return immediately
        {
            let state = promise.state.borrow();
            match &*state {
                PromiseState::Resolved(v) => return Ok(v.clone()),
                PromiseState::Rejected(e) => {
                    return Err(SemaError::eval(format!("async/await: task rejected: {e}")))
                }
                PromiseState::Pending => {}
            }
        }

        // If in async context, yield
        if in_async_context() {
            set_yield_signal(YieldReason::AwaitPromise(promise));
            return Ok(Value::nil()); // placeholder, VM catches the signal
        }

        // At top level, run the scheduler inline
        call_run_scheduler(ctx, Some(promise.clone()))?;
        let state = promise.state.borrow();
        match &*state {
            PromiseState::Resolved(v) => Ok(v.clone()),
            PromiseState::Rejected(e) => {
                Err(SemaError::eval(format!("async/await: task rejected: {e}")))
            }
            PromiseState::Pending => Err(SemaError::eval(
                "async/await: still pending after scheduler run",
            )),
        }
    });

    // async/run — run all pending tasks to completion
    register_fn_ctx(env, "async/run", |ctx, args| {
        check_arity!(args, "async/run", 0);
        call_run_scheduler(ctx, None)?;
        Ok(Value::nil())
    });

    // async/resolved — create an already-resolved promise
    register_fn(env, "async/resolved", |args| {
        check_arity!(args, "async/resolved", 1);
        Ok(Value::async_promise(AsyncPromise {
            state: RefCell::new(PromiseState::Resolved(args[0].clone())),
            task_id: Cell::new(0),
        }))
    });

    // async/rejected — create an already-rejected promise
    register_fn(env, "async/rejected", |args| {
        check_arity!(args, "async/rejected", 1);
        let msg = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?
            .to_string();
        Ok(Value::async_promise(AsyncPromise {
            state: RefCell::new(PromiseState::Rejected(msg)),
            task_id: Cell::new(0),
        }))
    });

    // async/resolved? — check if promise is resolved
    register_fn(env, "async/resolved?", |args| {
        check_arity!(args, "async/resolved?", 1);
        let promise = expect_promise(args, "async/resolved?", 0)?;
        let resolved = matches!(&*promise.state.borrow(), PromiseState::Resolved(_));
        Ok(Value::bool(resolved))
    });

    // async/rejected? — check if promise is rejected
    register_fn(env, "async/rejected?", |args| {
        check_arity!(args, "async/rejected?", 1);
        let promise = expect_promise(args, "async/rejected?", 0)?;
        let rejected = matches!(&*promise.state.borrow(), PromiseState::Rejected(_));
        Ok(Value::bool(rejected))
    });

    // async/pending? — check if promise is still pending
    register_fn(env, "async/pending?", |args| {
        check_arity!(args, "async/pending?", 1);
        let promise = expect_promise(args, "async/pending?", 0)?;
        let pending = matches!(&*promise.state.borrow(), PromiseState::Pending);
        Ok(Value::bool(pending))
    });

    // async/cancel — cancel a spawned async task
    register_fn(env, "async/cancel", |args| {
        check_arity!(args, "async/cancel", 1);
        let promise = expect_promise(args, "async/cancel", 0)?;
        let task_id = promise.task_id.get();
        if task_id == 0 {
            return Err(SemaError::eval(
                "async/cancel: cannot cancel a non-spawned promise".to_string(),
            ));
        }
        sema_core::call_cancel_callback(task_id)?;
        Ok(Value::nil())
    });

    // async/cancelled? — check if promise was cancelled
    register_fn(env, "async/cancelled?", |args| {
        check_arity!(args, "async/cancelled?", 1);
        let promise = expect_promise(args, "async/cancelled?", 0)?;
        let state = promise.state.borrow();
        let is_cancelled = matches!(&*state, PromiseState::Rejected(msg) if msg == "cancelled");
        Ok(Value::bool(is_cancelled))
    });

    // async/all — run scheduler and collect results from all promises
    register_fn_ctx(env, "async/all", |ctx, args| {
        check_arity!(args, "async/all", 1);
        let items = expect_list_or_vector(&args[0], "async/all")?;

        let promises: Vec<Rc<AsyncPromise>> = items
            .iter()
            .map(|item| expect_promise(std::slice::from_ref(item), "async/all", 0))
            .collect::<Result<_, _>>()?;

        // Run scheduler until the requested promises settle. Unrelated
        // background tasks must not make this combinator report deadlock.
        call_run_scheduler_all_of(ctx, promises.clone())?;

        // Collect results
        let mut results = Vec::with_capacity(items.len());
        if let Some(err) = promises.iter().find_map(|p| match &*p.state.borrow() {
            PromiseState::Rejected(e) => Some(e.clone()),
            _ => None,
        }) {
            return Err(SemaError::eval(format!("async/all: task rejected: {err}")));
        }
        for p in promises {
            let state = p.state.borrow();
            match &*state {
                PromiseState::Resolved(v) => results.push(v.clone()),
                PromiseState::Rejected(_) => unreachable!("rejections handled above"),
                PromiseState::Pending => {
                    return Err(SemaError::eval("async/all: task still pending"))
                }
            }
        }
        Ok(Value::list(results))
    });

    // async/race — run scheduler and return the first resolved promise's value
    register_fn_ctx(env, "async/race", |ctx, args| {
        check_arity!(args, "async/race", 1);
        let items = expect_list_or_vector(&args[0], "async/race")?;

        if items.is_empty() {
            return Err(SemaError::eval("async/race: requires at least one promise"));
        }

        // Collect promises
        let promises: Vec<Rc<AsyncPromise>> = items
            .iter()
            .map(|item| expect_promise(std::slice::from_ref(item), "async/race", 0))
            .collect::<Result<_, _>>()?;

        // Check if any already resolved
        for p in &promises {
            if let PromiseState::Resolved(v) = &*p.state.borrow() {
                return Ok(v.clone());
            }
        }

        // Run scheduler until one requested promise settles. Unrelated
        // background tasks must not make this combinator report deadlock.
        call_run_scheduler_any_of(ctx, promises.clone())?;

        // Find first resolved
        for p in &promises {
            if let PromiseState::Resolved(v) = &*p.state.borrow() {
                return Ok(v.clone());
            }
        }

        // Check for rejections
        for p in &promises {
            if let PromiseState::Rejected(e) = &*p.state.borrow() {
                return Err(SemaError::eval(format!("async/race: task rejected: {e}")));
            }
        }

        Err(SemaError::eval("async/race: no promise resolved"))
    });

    // async/timeout — race a promise against a deadline
    register_fn_ctx(env, "async/timeout", |ctx, args| {
        check_arity!(args, "async/timeout", 2);
        let ms = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        if ms < 0 {
            return Err(SemaError::eval(
                "async/timeout: duration must be non-negative",
            ));
        }
        let promise = expect_promise(args, "async/timeout", 1)?;

        // If already resolved/rejected, return immediately
        {
            let state = promise.state.borrow();
            match &*state {
                PromiseState::Resolved(v) => return Ok(v.clone()),
                PromiseState::Rejected(e) => {
                    return Err(SemaError::eval(format!(
                        "async/timeout: task rejected: {e}"
                    )))
                }
                PromiseState::Pending => {}
            }
        }

        // Run scheduler until the promise resolves or the timeout elapses.
        if call_run_scheduler_timeout(ctx, promise.clone(), ms as u64)?
            == SchedulerRunResult::TimedOut
        {
            return Err(SemaError::eval("async/timeout: operation timed out"));
        }

        // Check if resolved
        {
            let state = promise.state.borrow();
            match &*state {
                PromiseState::Resolved(v) => return Ok(v.clone()),
                PromiseState::Rejected(e) => {
                    return Err(SemaError::eval(format!(
                        "async/timeout: task rejected: {e}"
                    )))
                }
                PromiseState::Pending => {}
            }
        }

        Err(SemaError::eval(
            "async/timeout: operation is still pending after scheduler run",
        ))
    });

    // async/sleep — yield for a duration in milliseconds
    register_fn(env, "async/sleep", |args| {
        check_arity!(args, "async/sleep", 1);
        let ms = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        if ms < 0 {
            return Err(SemaError::eval(
                "async/sleep: duration must be non-negative",
            ));
        }
        if in_async_context() {
            if let Some(cached) = take_resume_value() {
                return Ok(cached);
            }
            set_yield_signal(YieldReason::Sleep(ms as u64));
            return Ok(Value::nil());
        }
        // Outside async, actually sleep
        #[cfg(not(target_arch = "wasm32"))]
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
        Ok(Value::nil())
    });
}

// ── Channel operations ───────────────────────────────────────────

fn register_channel_ops(env: &Env) {
    // channel/new — create a bounded channel
    register_fn(env, "channel/new", |args| {
        check_arity!(args, "channel/new", 0..=1);
        let capacity = if args.is_empty() {
            1
        } else {
            let n = args[0]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
            if n <= 0 {
                return Err(SemaError::eval("channel/new: capacity must be at least 1"));
            }
            n as usize
        };
        Ok(Value::channel(Channel {
            buffer: RefCell::new(VecDeque::with_capacity(capacity)),
            capacity,
            closed: Cell::new(false),
        }))
    });

    // channel/send — send a value to a channel (yields if full in async context)
    register_fn(env, "channel/send", |args| {
        check_arity!(args, "channel/send", 2);
        let ch = expect_channel(args, "channel/send", 0)?;
        if ch.closed.get() {
            return Err(SemaError::eval(format!(
                "channel/send: channel is closed; value {} was dropped",
                args[1]
            )));
        }
        if in_async_context() {
            if let Some(cached) = take_resume_value() {
                return Ok(cached);
            }
        }
        let mut buf = ch.buffer.borrow_mut();
        if buf.len() >= ch.capacity {
            drop(buf);
            if in_async_context() {
                set_yield_signal(YieldReason::ChannelSend(ch, args[1].clone()));
                return Ok(Value::nil());
            }
            return Err(
                SemaError::eval("channel/send: channel is full").with_hint(
                    "Use async to run in an async context where send will yield until space is available",
                ),
            );
        }
        buf.push_back(args[1].clone());
        Ok(Value::nil())
    });

    // channel/recv — receive a value from a channel (yields if empty in async context)
    register_fn(env, "channel/recv", |args| {
        check_arity!(args, "channel/recv", 1);
        let ch = expect_channel(args, "channel/recv", 0)?;
        if in_async_context() {
            if let Some(cached) = take_resume_value() {
                return Ok(cached);
            }
        }
        let mut buf = ch.buffer.borrow_mut();
        if let Some(v) = buf.pop_front() {
            return Ok(v);
        }
        drop(buf);
        if ch.closed.get() {
            return Ok(Value::nil());
        }
        if in_async_context() {
            set_yield_signal(YieldReason::ChannelRecv(ch));
            return Ok(Value::nil());
        }
        Err(SemaError::eval("channel/recv: channel is empty"))
    });

    // channel/try-recv — non-blocking receive (returns nil if empty)
    register_fn(env, "channel/try-recv", |args| {
        check_arity!(args, "channel/try-recv", 1);
        let ch = expect_channel(args, "channel/try-recv", 0)?;
        let val = ch.buffer.borrow_mut().pop_front().unwrap_or(Value::nil());
        Ok(val)
    });

    // channel/close — close a channel
    register_fn(env, "channel/close", |args| {
        check_arity!(args, "channel/close", 1);
        let ch = expect_channel(args, "channel/close", 0)?;
        ch.closed.set(true);
        Ok(Value::nil())
    });

    // channel/closed? — check if a channel is closed
    register_fn(env, "channel/closed?", |args| {
        check_arity!(args, "channel/closed?", 1);
        let ch = expect_channel(args, "channel/closed?", 0)?;
        Ok(Value::bool(ch.closed.get()))
    });

    // channel/count — number of items currently in the buffer
    register_fn(env, "channel/count", |args| {
        check_arity!(args, "channel/count", 1);
        let ch = expect_channel(args, "channel/count", 0)?;
        let len = ch.buffer.borrow().len();
        Ok(Value::int(len as i64))
    });

    // channel/empty? — check if the channel buffer is empty
    register_fn(env, "channel/empty?", |args| {
        check_arity!(args, "channel/empty?", 1);
        let ch = expect_channel(args, "channel/empty?", 0)?;
        let empty = ch.buffer.borrow().is_empty();
        Ok(Value::bool(empty))
    });

    // channel/full? — check if the channel buffer is at capacity
    register_fn(env, "channel/full?", |args| {
        check_arity!(args, "channel/full?", 1);
        let ch = expect_channel(args, "channel/full?", 0)?;
        let buf = ch.buffer.borrow();
        Ok(Value::bool(buf.len() >= ch.capacity))
    });
}
