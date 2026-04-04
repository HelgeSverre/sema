use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::rc::Rc;

use sema_core::{
    call_callback, check_arity, AsyncPromise, Channel, Env, EvalContext, NativeFn, PromiseState,
    SemaError, Value, ValueView,
};

use crate::register_fn;

// ── Task scheduler ──────────────────────────────────────────────

/// A task in the cooperative scheduler.
struct Task {
    #[allow(dead_code)]
    id: u64,
    /// The promise this task will resolve.
    promise: Rc<AsyncPromise>,
    /// The function to call (a thunk/lambda).
    func: Value,
    /// Whether this task has started (the func has been called).
    started: bool,
}

/// Thread-local task scheduler for cooperative concurrency.
struct Scheduler {
    tasks: RefCell<VecDeque<Task>>,
    next_id: Cell<u64>,
}

impl Scheduler {
    fn new() -> Self {
        Scheduler {
            tasks: RefCell::new(VecDeque::new()),
            next_id: Cell::new(1),
        }
    }

    fn spawn(&self, func: Value, env: &Env) -> Rc<AsyncPromise> {
        let id = self.next_id.get();
        self.next_id.set(id + 1);

        let promise = Rc::new(AsyncPromise {
            state: RefCell::new(PromiseState::Pending),
            body: Value::nil(),
            env: env.clone(),
            task_id: Cell::new(id),
        });

        self.tasks.borrow_mut().push_back(Task {
            id,
            promise: promise.clone(),
            func,
            started: false,
        });

        promise
    }

    /// Run all tasks until the given promise is resolved, or all tasks finish.
    fn run_until(
        &self,
        ctx: &EvalContext,
        target: Option<&Rc<AsyncPromise>>,
    ) -> Result<(), SemaError> {
        // Guard against re-entrant scheduling
        let max_iterations = 100_000;
        let mut iterations = 0;

        loop {
            // Check if target is resolved
            if let Some(t) = target {
                match &*t.state.borrow() {
                    PromiseState::Pending => {}
                    _ => return Ok(()),
                }
            }

            // Pop next task
            let task = self.tasks.borrow_mut().pop_front();
            let Some(mut task) = task else {
                // No more tasks
                return Ok(());
            };

            iterations += 1;
            if iterations > max_iterations {
                return Err(SemaError::eval(
                    "async scheduler: exceeded maximum iterations (possible deadlock)".to_string(),
                ));
            }

            if !task.started {
                task.started = true;
                // Execute the thunk — call with no arguments
                match call_callback(ctx, &task.func, &[]) {
                    Ok(val) => {
                        *task.promise.state.borrow_mut() = PromiseState::Resolved(val);
                    }
                    Err(e) => {
                        *task.promise.state.borrow_mut() = PromiseState::Rejected(format!("{e}"));
                    }
                }
                // Task is done, don't re-enqueue
            }
        }
    }

    /// Run all pending tasks to completion.
    fn run_all(&self, ctx: &EvalContext) -> Result<(), SemaError> {
        self.run_until(ctx, None)
    }
}

thread_local! {
    static SCHEDULER: Scheduler = Scheduler::new();
}

// ── Helper functions ────────────────────────────────────────────

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

// ── Registration ────────────────────────────────────────────────

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

pub fn register(env: &sema_core::Env) {
    // ── Predicates ──────────────────────────────────────────────

    register_fn(env, "async/promise?", |args| {
        check_arity!(args, "async/promise?", 1);
        Ok(Value::bool(args[0].is_async_promise()))
    });

    register_fn(env, "channel?", |args| {
        check_arity!(args, "channel?", 1);
        Ok(Value::bool(args[0].is_channel()))
    });

    // ── async/spawn — spawn a thunk as a concurrent task ────────

    register_fn_ctx(env, "async/spawn", |_ctx, args| {
        check_arity!(args, "async/spawn", 1);
        // args[0] should be a callable (lambda or native-fn)
        let func = args[0].clone();
        let env = Env::new();
        let promise = SCHEDULER.with(|s| s.spawn(func, &env));
        Ok(Value::async_promise_from_rc(promise))
    });

    // ── async/await — block until promise resolves ──────────────

    register_fn_ctx(env, "async/await", |ctx, args| {
        check_arity!(args, "async/await", 1);
        let promise = expect_promise(args, "async/await", 0)?;

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

        // Run scheduler until this promise resolves
        SCHEDULER.with(|s| s.run_until(ctx, Some(&promise)))?;

        let state = promise.state.borrow();
        match &*state {
            PromiseState::Resolved(v) => Ok(v.clone()),
            PromiseState::Rejected(e) => {
                Err(SemaError::eval(format!("async/await: task rejected: {e}")))
            }
            PromiseState::Pending => Err(SemaError::eval(
                "async/await: task still pending after scheduler run (deadlock?)".to_string(),
            )),
        }
    });

    // ── async/run — run all pending tasks to completion ──────────

    register_fn_ctx(env, "async/run", |ctx, args| {
        check_arity!(args, "async/run", 0);
        SCHEDULER.with(|s| s.run_all(ctx))?;
        Ok(Value::nil())
    });

    // ── async/resolved — create an already-resolved promise ─────

    register_fn(env, "async/resolved", |args| {
        check_arity!(args, "async/resolved", 1);
        let promise = AsyncPromise {
            state: RefCell::new(PromiseState::Resolved(args[0].clone())),
            body: Value::nil(),
            env: Env::new(),
            task_id: Cell::new(0),
        };
        Ok(Value::async_promise(promise))
    });

    // ── async/rejected — create an already-rejected promise ─────

    register_fn(env, "async/rejected", |args| {
        check_arity!(args, "async/rejected", 1);
        let msg = args[0]
            .as_str()
            .map(|s| s.to_string())
            .unwrap_or_else(|| format!("{}", args[0]));
        let promise = AsyncPromise {
            state: RefCell::new(PromiseState::Rejected(msg)),
            body: Value::nil(),
            env: Env::new(),
            task_id: Cell::new(0),
        };
        Ok(Value::async_promise(promise))
    });

    // ── async/resolved? — check if a promise has resolved ───────

    register_fn(env, "async/resolved?", |args| {
        check_arity!(args, "async/resolved?", 1);
        let promise = expect_promise(args, "async/resolved?", 0)?;
        let state = promise.state.borrow();
        let result = matches!(&*state, PromiseState::Resolved(_));
        drop(state);
        Ok(Value::bool(result))
    });

    // ── async/rejected? — check if a promise was rejected ───────

    register_fn(env, "async/rejected?", |args| {
        check_arity!(args, "async/rejected?", 1);
        let promise = expect_promise(args, "async/rejected?", 0)?;
        let state = promise.state.borrow();
        let result = matches!(&*state, PromiseState::Rejected(_));
        drop(state);
        Ok(Value::bool(result))
    });

    // ── async/pending? — check if a promise is still pending ────

    register_fn(env, "async/pending?", |args| {
        check_arity!(args, "async/pending?", 1);
        let promise = expect_promise(args, "async/pending?", 0)?;
        let state = promise.state.borrow();
        let result = matches!(&*state, PromiseState::Pending);
        drop(state);
        Ok(Value::bool(result))
    });

    // ── async/all — await multiple promises, return list of results

    register_fn_ctx(env, "async/all", |ctx, args| {
        check_arity!(args, "async/all", 1);
        let promises_val = &args[0];
        let items = match promises_val.view() {
            ValueView::List(items) => items,
            ValueView::Vector(items) => items,
            _ => {
                return Err(SemaError::type_error_with_value(
                    "list or vector",
                    promises_val.type_name(),
                    promises_val,
                ))
            }
        };

        // Run scheduler to resolve all promises
        SCHEDULER.with(|s| s.run_all(ctx))?;

        // Collect results
        let mut results = Vec::with_capacity(items.len());
        for item in items.iter() {
            let promise = match item.view() {
                ValueView::AsyncPromise(p) => p,
                _ => {
                    return Err(SemaError::type_error_with_value(
                        "async-promise",
                        item.type_name(),
                        item,
                    ))
                }
            };
            let state = promise.state.borrow();
            match &*state {
                PromiseState::Resolved(v) => results.push(v.clone()),
                PromiseState::Rejected(e) => {
                    return Err(SemaError::eval(format!("async/all: task rejected: {e}")))
                }
                PromiseState::Pending => {
                    return Err(SemaError::eval(
                        "async/all: task still pending after scheduler run".to_string(),
                    ))
                }
            }
            drop(state);
        }
        Ok(Value::list(results))
    });

    // ── Channel operations ──────────────────────────────────────

    register_fn(env, "channel/new", |args| {
        check_arity!(args, "channel/new", 0..=1);
        let capacity = if args.is_empty() {
            1
        } else {
            args[0]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
                as usize
        };
        if capacity == 0 {
            return Err(SemaError::eval(
                "channel/new: capacity must be at least 1".to_string(),
            ));
        }
        Ok(Value::channel(Channel {
            buffer: RefCell::new(VecDeque::with_capacity(capacity)),
            capacity,
            closed: Cell::new(false),
        }))
    });

    register_fn(env, "channel/send", |args| {
        check_arity!(args, "channel/send", 2);
        let ch = expect_channel(args, "channel/send", 0)?;
        if ch.closed.get() {
            return Err(SemaError::eval(
                "channel/send: channel is closed".to_string(),
            ));
        }
        let mut buf = ch.buffer.borrow_mut();
        if buf.len() >= ch.capacity {
            return Err(
                SemaError::eval("channel/send: channel is full".to_string()).with_hint(
                    "Consider increasing channel capacity or consuming values with channel/recv",
                ),
            );
        }
        buf.push_back(args[1].clone());
        Ok(Value::nil())
    });

    register_fn(env, "channel/recv", |args| {
        check_arity!(args, "channel/recv", 1);
        let ch = expect_channel(args, "channel/recv", 0)?;
        let mut buf = ch.buffer.borrow_mut();
        match buf.pop_front() {
            Some(v) => Ok(v),
            None => {
                if ch.closed.get() {
                    Ok(Value::nil())
                } else {
                    Err(SemaError::eval(
                        "channel/recv: channel is empty".to_string(),
                    ))
                }
            }
        }
    });

    register_fn(env, "channel/try-recv", |args| {
        check_arity!(args, "channel/try-recv", 1);
        let ch = expect_channel(args, "channel/try-recv", 0)?;
        let mut buf = ch.buffer.borrow_mut();
        match buf.pop_front() {
            Some(v) => Ok(v),
            None => Ok(Value::nil()),
        }
    });

    register_fn(env, "channel/close", |args| {
        check_arity!(args, "channel/close", 1);
        let ch = expect_channel(args, "channel/close", 0)?;
        ch.closed.set(true);
        Ok(Value::nil())
    });

    register_fn(env, "channel/closed?", |args| {
        check_arity!(args, "channel/closed?", 1);
        let ch = expect_channel(args, "channel/closed?", 0)?;
        Ok(Value::bool(ch.closed.get()))
    });

    register_fn(env, "channel/count", |args| {
        check_arity!(args, "channel/count", 1);
        let ch = expect_channel(args, "channel/count", 0)?;
        let count = ch.buffer.borrow().len() as i64;
        Ok(Value::int(count))
    });

    register_fn(env, "channel/empty?", |args| {
        check_arity!(args, "channel/empty?", 1);
        let ch = expect_channel(args, "channel/empty?", 0)?;
        let empty = ch.buffer.borrow().is_empty();
        Ok(Value::bool(empty))
    });

    register_fn(env, "channel/full?", |args| {
        check_arity!(args, "channel/full?", 1);
        let ch = expect_channel(args, "channel/full?", 0)?;
        let full = ch.buffer.borrow().len() >= ch.capacity;
        Ok(Value::bool(full))
    });
}
