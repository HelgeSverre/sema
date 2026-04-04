use std::cell::{Cell, RefCell};
use std::collections::VecDeque;
use std::rc::Rc;

use sema_core::{
    call_callback, check_arity, AsyncPromise, Channel, Env, EvalContext, NativeFn, PromiseState,
    SemaError, Value, ValueView, YieldReason,
};

use crate::register_fn;

// ── Async execution context (thread-local) ──────────────────────

/// Per-task replay state used to implement cooperative yield/resume
/// in the tree-walker. When a task yields (e.g. channel/recv on empty),
/// we record all yieldable-operation results seen so far. On resume,
/// we re-evaluate the task from scratch but replay cached results for
/// operations that already completed, and suppress side-effect output
/// during replay.
struct ReplayState {
    /// Cached results from previous runs (one per yieldable operation).
    log: Vec<Value>,
    /// Current index into the log during replay.
    index: usize,
    /// If true, we are replaying past a yield point — suppress output.
    replaying: bool,
}

// Thread-local flag: are we currently inside an async task?
// This lets NativeFns (channel/recv, etc.) decide whether to yield
// or error when they cannot proceed.
thread_local! {
    static IN_ASYNC_TASK: Cell<bool> = const { Cell::new(false) };
    static REPLAY: RefCell<Option<ReplayState>> = const { RefCell::new(None) };
    static SUPPRESS_OUTPUT: Cell<bool> = const { Cell::new(false) };
}

/// Check if we're in replay mode and should return a cached result
/// instead of actually executing. Returns Some(cached_value) if
/// replaying, None if at the execution frontier.
fn replay_check() -> Option<Value> {
    REPLAY.with(|r| {
        let mut replay = r.borrow_mut();
        let replay = replay.as_mut()?;
        if replay.index < replay.log.len() {
            let val = replay.log[replay.index].clone();
            replay.index += 1;
            Some(val)
        } else {
            None
        }
    })
}

/// Record a result in the replay log (at the execution frontier).
fn replay_record(val: &Value) {
    REPLAY.with(|r| {
        let mut replay = r.borrow_mut();
        if let Some(ref mut replay) = *replay {
            replay.log.push(val.clone());
            replay.index += 1;
        }
    });
}

/// Check if output should be suppressed (during replay).
/// Returns true when async replay is in progress and output should be suppressed.
#[allow(dead_code)]
pub fn is_output_suppressed() -> bool {
    SUPPRESS_OUTPUT.with(|s| s.get())
}

// ── Task states ─────────────────────────────────────────────────

enum TaskState {
    /// Ready to run (fresh or resumed).
    Ready,
    /// Blocked on a yield reason, with its replay log for resume.
    Blocked {
        reason: YieldReason,
        replay_log: Vec<Value>,
    },
    /// Completed successfully.
    Done,
    /// Failed with error.
    Failed,
}

struct Task {
    id: u64,
    promise: Rc<AsyncPromise>,
    func: Value,
    state: TaskState,
}

// ── Scheduler ───────────────────────────────────────────────────

struct Scheduler {
    tasks: RefCell<Vec<Task>>,
    next_id: Cell<u64>,
}

impl Scheduler {
    fn new() -> Self {
        Scheduler {
            tasks: RefCell::new(Vec::new()),
            next_id: Cell::new(1),
        }
    }

    fn spawn(&self, func: Value, _env: &Env) -> Rc<AsyncPromise> {
        let id = self.next_id.get();
        self.next_id.set(id + 1);

        let promise = Rc::new(AsyncPromise {
            state: RefCell::new(PromiseState::Pending),
            body: Value::nil(),
            env: Env::new(),
            task_id: Cell::new(id),
        });

        self.tasks.borrow_mut().push(Task {
            id,
            promise: promise.clone(),
            func,
            state: TaskState::Ready,
        });

        promise
    }

    /// Try to unblock tasks whose conditions are now met.
    /// When waking a task, compute the resume value and append it to the
    /// replay log so the yieldable operation returns the correct result
    /// on replay instead of yielding again.
    fn wake_blocked_tasks(&self) {
        let mut tasks = self.tasks.borrow_mut();
        for task in tasks.iter_mut() {
            let resume_value = if let TaskState::Blocked { ref reason, .. } = task.state {
                match reason {
                    YieldReason::AwaitPromise(p) => {
                        let state = p.state.borrow();
                        match &*state {
                            PromiseState::Resolved(v) => Some(v.clone()),
                            PromiseState::Rejected(_) => Some(Value::nil()), // will error on replay
                            PromiseState::Pending => None,
                        }
                    }
                    YieldReason::ChannelRecv(ch) => {
                        let mut buf = ch.buffer.borrow_mut();
                        if let Some(v) = buf.pop_front() {
                            Some(v)
                        } else if ch.closed.get() {
                            Some(Value::nil())
                        } else {
                            None
                        }
                    }
                    YieldReason::ChannelSend(ch, val) => {
                        let mut buf = ch.buffer.borrow_mut();
                        if buf.len() < ch.capacity {
                            buf.push_back(val.clone());
                            Some(Value::nil())
                        } else {
                            None
                        }
                    }
                    YieldReason::Sleep(_) => Some(Value::nil()),
                }
            } else {
                None
            };

            if let Some(resume_val) = resume_value {
                let old = std::mem::replace(&mut task.state, TaskState::Ready);
                if let TaskState::Blocked { mut replay_log, .. } = old {
                    // Append the resume value so the yieldable op returns it on replay
                    replay_log.push(resume_val);
                    PENDING_REPLAY.with(|pr| {
                        pr.borrow_mut().insert(task.id, replay_log);
                    });
                }
            }
        }
    }

    /// Run a single task. Returns true if the task made progress.
    fn run_task(&self, ctx: &EvalContext, task_idx: usize) -> Result<bool, SemaError> {
        let (id, func, promise) = {
            let tasks = self.tasks.borrow();
            let task = &tasks[task_idx];
            (task.id, task.func.clone(), task.promise.clone())
        };

        // Set up replay state if we have one from a previous yield
        let replay_log = PENDING_REPLAY.with(|pr| pr.borrow_mut().remove(&id));
        let replay = replay_log.map(|log| {
            let len = log.len();
            ReplayState {
                log,
                index: 0,
                replaying: len > 0,
            }
        });

        // Install async context
        IN_ASYNC_TASK.with(|f| f.set(true));
        REPLAY.with(|r| *r.borrow_mut() = replay);
        SUPPRESS_OUTPUT.with(|s| {
            let is_replay = REPLAY.with(|r| r.borrow().as_ref().is_some_and(|rs| rs.replaying));
            s.set(is_replay);
        });

        // Execute the thunk
        let result = call_callback(ctx, &func, &[]);

        // Tear down async context
        let final_replay = REPLAY.with(|r| r.borrow_mut().take());
        IN_ASYNC_TASK.with(|f| f.set(false));
        SUPPRESS_OUTPUT.with(|s| s.set(false));

        // Process result
        let mut tasks = self.tasks.borrow_mut();
        let task = &mut tasks[task_idx];

        match result {
            Ok(val) => {
                *promise.state.borrow_mut() = PromiseState::Resolved(val);
                task.state = TaskState::Done;
                Ok(true)
            }
            Err(SemaError::Yield(reason)) => {
                let replay_log = final_replay.map_or_else(Vec::new, |rs| rs.log);
                task.state = TaskState::Blocked { reason, replay_log };
                Ok(true)
            }
            Err(e) => {
                *promise.state.borrow_mut() = PromiseState::Rejected(format!("{e}"));
                task.state = TaskState::Failed;
                Ok(true)
            }
        }
    }

    /// Run the event loop until the target promise resolves or all tasks finish.
    fn run_until(
        &self,
        ctx: &EvalContext,
        target: Option<&Rc<AsyncPromise>>,
    ) -> Result<(), SemaError> {
        let max_ticks = 1_000_000;

        for _ in 0..max_ticks {
            // Check if target is resolved
            if let Some(t) = target {
                if !matches!(&*t.state.borrow(), PromiseState::Pending) {
                    return Ok(());
                }
            }

            // Wake blocked tasks
            self.wake_blocked_tasks();

            // Find next ready task
            let ready_idx = {
                let tasks = self.tasks.borrow();
                tasks
                    .iter()
                    .position(|t| matches!(t.state, TaskState::Ready))
            };

            let Some(idx) = ready_idx else {
                // No ready tasks. Check if any are blocked.
                let has_blocked = self
                    .tasks
                    .borrow()
                    .iter()
                    .any(|t| matches!(t.state, TaskState::Blocked { .. }));
                if has_blocked {
                    // All tasks are blocked — deadlock
                    return Err(SemaError::eval(
                        "async scheduler: all tasks blocked (deadlock detected)".to_string(),
                    ));
                }
                // All tasks are done
                return Ok(());
            };

            self.run_task(ctx, idx)?;
        }

        Err(SemaError::eval(
            "async scheduler: exceeded maximum ticks (possible infinite loop)".to_string(),
        ))
    }

    fn run_all(&self, ctx: &EvalContext) -> Result<(), SemaError> {
        self.run_until(ctx, None)
    }
}

thread_local! {
    static SCHEDULER: Scheduler = Scheduler::new();
    /// Side-channel for passing replay logs from wake_blocked_tasks to run_task.
    static PENDING_REPLAY: RefCell<std::collections::HashMap<u64, Vec<Value>>> =
        RefCell::new(std::collections::HashMap::new());
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

fn is_in_async_task() -> bool {
    IN_ASYNC_TASK.with(|f| f.get())
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
        let func = args[0].clone();
        let env = Env::new();
        let promise = SCHEDULER.with(|s| s.spawn(func, &env));
        Ok(Value::async_promise_from_rc(promise))
    });

    // ── async/await — yield until promise resolves ──────────────

    register_fn_ctx(env, "async/await", |ctx, args| {
        check_arity!(args, "async/await", 1);
        let promise = expect_promise(args, "async/await", 0)?;

        // If already resolved, return immediately (and record in replay log)
        {
            let state = promise.state.borrow();
            match &*state {
                PromiseState::Resolved(v) => {
                    let v = v.clone();
                    drop(state);
                    replay_record(&v);
                    return Ok(v);
                }
                PromiseState::Rejected(e) => {
                    return Err(SemaError::eval(format!("async/await: task rejected: {e}")))
                }
                PromiseState::Pending => {}
            }
        }

        // If we're in an async task, yield to the scheduler
        if is_in_async_task() {
            // Check replay first — maybe we already resolved this in a prior run
            if let Some(cached) = replay_check() {
                return Ok(cached);
            }
            return Err(SemaError::Yield(YieldReason::AwaitPromise(promise)));
        }

        // If we're at the top level, run the scheduler inline
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

    // ── async/race — return first promise to resolve ────────────

    register_fn_ctx(env, "async/race", |ctx, args| {
        check_arity!(args, "async/race", 1);
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

        if items.is_empty() {
            return Err(SemaError::eval(
                "async/race: requires at least one promise".to_string(),
            ));
        }

        // Collect promises
        let promises: Vec<Rc<AsyncPromise>> = items
            .iter()
            .map(|item| match item.view() {
                ValueView::AsyncPromise(p) => Ok(p),
                _ => Err(SemaError::type_error_with_value(
                    "async-promise",
                    item.type_name(),
                    item,
                )),
            })
            .collect::<Result<_, _>>()?;

        // Check if any already resolved
        for p in &promises {
            let state = p.state.borrow();
            if let PromiseState::Resolved(v) = &*state {
                return Ok(v.clone());
            }
        }

        // Run scheduler, checking after each tick if any resolved
        let max_ticks = 1_000_000;
        for _ in 0..max_ticks {
            SCHEDULER.with(|s| {
                s.wake_blocked_tasks();
                let ready_idx = {
                    let tasks = s.tasks.borrow();
                    tasks
                        .iter()
                        .position(|t| matches!(t.state, TaskState::Ready))
                };
                if let Some(idx) = ready_idx {
                    s.run_task(ctx, idx)
                } else {
                    Ok(false)
                }
            })?;

            // Check if any promise resolved
            for p in &promises {
                let state = p.state.borrow();
                match &*state {
                    PromiseState::Resolved(v) => return Ok(v.clone()),
                    PromiseState::Rejected(e) => {
                        return Err(SemaError::eval(format!("async/race: task rejected: {e}")))
                    }
                    PromiseState::Pending => {}
                }
            }

            // Check if all tasks are done with none resolved (shouldn't happen but guard)
            let any_pending = promises
                .iter()
                .any(|p| matches!(&*p.state.borrow(), PromiseState::Pending));
            if !any_pending {
                break;
            }
        }

        Err(SemaError::eval(
            "async/race: no promise resolved".to_string(),
        ))
    });

    // ── async/sleep — yield for a number of milliseconds ────────

    register_fn(env, "async/sleep", |args| {
        check_arity!(args, "async/sleep", 1);
        let ms = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        if ms < 0 {
            return Err(SemaError::eval(
                "async/sleep: duration must be non-negative".to_string(),
            ));
        }

        if is_in_async_task() {
            if let Some(cached) = replay_check() {
                return Ok(cached);
            }
            return Err(SemaError::Yield(YieldReason::Sleep(ms as u64)));
        }

        // Outside async context, actually sleep
        #[cfg(not(target_arch = "wasm32"))]
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
        Ok(Value::nil())
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

        // Check replay first
        if is_in_async_task() {
            if let Some(cached) = replay_check() {
                return Ok(cached);
            }
        }

        let mut buf = ch.buffer.borrow_mut();
        if buf.len() >= ch.capacity {
            drop(buf);
            if is_in_async_task() {
                // Yield — will resume when channel has space
                return Err(SemaError::Yield(YieldReason::ChannelSend(
                    ch,
                    args[1].clone(),
                )));
            }
            return Err(
                SemaError::eval("channel/send: channel is full".to_string()).with_hint(
                    "Use async/spawn to run in an async context where send will yield until space is available",
                ),
            );
        }
        buf.push_back(args[1].clone());
        drop(buf);

        // Record nil in replay log
        replay_record(&Value::nil());
        Ok(Value::nil())
    });

    register_fn(env, "channel/recv", |args| {
        check_arity!(args, "channel/recv", 1);
        let ch = expect_channel(args, "channel/recv", 0)?;

        // Check replay first
        if is_in_async_task() {
            if let Some(cached) = replay_check() {
                return Ok(cached);
            }
        }

        let mut buf = ch.buffer.borrow_mut();
        match buf.pop_front() {
            Some(v) => {
                drop(buf);
                replay_record(&v);
                Ok(v)
            }
            None => {
                drop(buf);
                if ch.closed.get() {
                    let v = Value::nil();
                    replay_record(&v);
                    Ok(v)
                } else if is_in_async_task() {
                    // Yield — will resume when channel has data
                    Err(SemaError::Yield(YieldReason::ChannelRecv(ch)))
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
