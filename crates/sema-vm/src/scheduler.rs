//! Cooperative task scheduler for async concurrency.
//!
//! Manages multiple VM instances, each running an async task. Tasks yield
//! cooperatively via the `YieldReason` signal mechanism defined in `sema_core::async_signal`.
//!
//! # Architecture
//!
//! Each async task gets its own VM instance sharing globals and functions
//! with the parent. Native functions signal yield via `set_yield_signal(reason)`
//! and return `Ok(Value::nil())`. The VM checks `take_yield_signal()` after
//! native calls and returns `VmExecResult::AsyncYield(reason)`. On resume,
//! the scheduler sets `set_resume_value(val)` before re-running the VM.

use std::cell::RefCell;
use std::rc::Rc;

use sema_core::{
    in_async_context, set_async_context, set_cancel_callback, set_run_scheduler_callback,
    set_spawn_callback, AsyncPromise, Env, EvalContext, PromiseState, SemaError, Spur, Value,
    YieldReason,
};

use crate::debug::VmExecResult;
use crate::vm::{self, Closure, VM};

// ── Task types ─────────────────────────────────────────────────────

/// Current state of an async task.
enum TaskState {
    /// Ready to run (or resume).
    Ready,
    /// Blocked waiting on an external event.
    Blocked(YieldReason),
    /// Completed successfully.
    Done,
    /// Completed with an error.
    Failed,
}

/// A single async task managed by the scheduler.
#[allow(dead_code)]
struct Task {
    id: u64,
    vm: VM,
    closure: Rc<Closure>,
    promise: Rc<AsyncPromise>,
    state: TaskState,
    /// Whether `execute_async` has been called (false = first run).
    started: bool,
    /// Whether this task has been cancelled.
    cancelled: bool,
    /// Value to pass to the VM on resume (set by `wake_blocked_tasks`).
    resume_value: Option<Value>,
    /// Deadline for sleep-blocked tasks (None if not sleeping).
    #[cfg(not(target_arch = "wasm32"))]
    sleep_deadline: Option<std::time::Instant>,
}

// ── Scheduler ──────────────────────────────────────────────────────

/// Cooperative task scheduler managing multiple VM-backed async tasks.
pub struct Scheduler {
    tasks: Vec<Task>,
    next_id: u64,
    /// Shared global environment for spawning child VMs.
    globals: Rc<Env>,
    /// Native function spurs for resolving the native dispatch table in child VMs.
    native_spurs: Vec<Spur>,
}

impl Scheduler {
    /// Create a new scheduler with shared state from the parent VM.
    pub fn new(globals: Rc<Env>, native_spurs: Vec<Spur>) -> Self {
        Scheduler {
            tasks: Vec::new(),
            next_id: 1,
            globals,
            native_spurs,
        }
    }

    /// Spawn a new async task from a thunk (zero-argument VM closure).
    ///
    /// Extracts the compiled closure from the thunk value, creates a
    /// dedicated VM for the task, and returns a promise that will be
    /// resolved when the task completes.
    pub fn spawn(
        &mut self,
        thunk: Value,
        _ctx: &EvalContext,
    ) -> Result<Rc<AsyncPromise>, SemaError> {
        let (closure, functions) = vm::extract_vm_closure(&thunk).ok_or_else(|| {
            SemaError::eval("async/spawn: argument must be a function (compiled VM closure)")
        })?;

        let id = self.next_id;
        self.next_id += 1;

        let promise = Rc::new(AsyncPromise {
            state: RefCell::new(PromiseState::Pending),
            body: Value::nil(),
            env: Env::new(),
            task_id: std::cell::Cell::new(id),
        });

        // Use the function table from the thunk's own compilation context,
        // not the scheduler's — each eval_str_compiled produces different functions.
        let vm = VM::new_for_task(self.globals.clone(), functions, &self.native_spurs)?;

        self.tasks.push(Task {
            id,
            vm,
            closure,
            promise: promise.clone(),
            state: TaskState::Ready,
            started: false,
            cancelled: false,
            resume_value: None,
            #[cfg(not(target_arch = "wasm32"))]
            sleep_deadline: None,
        });

        Ok(promise)
    }

    /// Check blocked tasks and transition them to Ready if their
    /// blocking condition has been satisfied.
    fn wake_blocked_tasks(&mut self) {
        /// Result of checking a blocked task's wake condition.
        enum WakeAction {
            /// Still blocked — no change.
            Pending,
            /// Resume the task with this value.
            Resume(Value),
            /// Fail the task with this rejection message.
            Fail(String),
        }

        for task in &mut self.tasks {
            if task.cancelled {
                if !matches!(task.state, TaskState::Done | TaskState::Failed) {
                    *task.promise.state.borrow_mut() = PromiseState::Rejected("cancelled".to_string());
                    task.state = TaskState::Failed;
                }
                continue;
            }
            if let TaskState::Blocked(ref reason) = task.state {
                let action = match reason {
                    YieldReason::AwaitPromise(p) => {
                        let state = p.state.borrow();
                        match &*state {
                            PromiseState::Resolved(v) => WakeAction::Resume(v.clone()),
                            PromiseState::Rejected(e) => WakeAction::Fail(e.clone()),
                            PromiseState::Pending => WakeAction::Pending,
                        }
                    }
                    YieldReason::ChannelRecv(ch) => {
                        let mut buf = ch.buffer.borrow_mut();
                        if let Some(v) = buf.pop_front() {
                            WakeAction::Resume(v)
                        } else if ch.closed.get() {
                            WakeAction::Resume(Value::nil())
                        } else {
                            WakeAction::Pending
                        }
                    }
                    YieldReason::ChannelSend(ch, val) => {
                        if ch.closed.get() {
                            WakeAction::Fail(
                                "channel/send: channel closed while send was pending".to_string(),
                            )
                        } else {
                            let mut buf = ch.buffer.borrow_mut();
                            if buf.len() < ch.capacity {
                                buf.push_back(val.clone());
                                WakeAction::Resume(Value::nil())
                            } else {
                                WakeAction::Pending
                            }
                        }
                    }
                    YieldReason::Sleep(_ms) => {
                        #[cfg(not(target_arch = "wasm32"))]
                        {
                            // Set deadline on first check if not already set
                            if task.sleep_deadline.is_none() {
                                task.sleep_deadline = Some(
                                    std::time::Instant::now()
                                        + std::time::Duration::from_millis(*_ms),
                                );
                            }
                            if task
                                .sleep_deadline
                                .map_or(true, |d| std::time::Instant::now() >= d)
                            {
                                task.sleep_deadline = None;
                                WakeAction::Resume(Value::nil())
                            } else {
                                WakeAction::Pending
                            }
                        }
                        #[cfg(target_arch = "wasm32")]
                        {
                            WakeAction::Resume(Value::nil())
                        }
                    }
                };

                match action {
                    WakeAction::Resume(val) => {
                        task.resume_value = Some(val);
                        task.state = TaskState::Ready;
                    }
                    WakeAction::Fail(msg) => {
                        *task.promise.state.borrow_mut() = PromiseState::Rejected(msg);
                        task.state = TaskState::Failed;
                    }
                    WakeAction::Pending => {}
                }
            }
        }
    }

    /// Mark a task as cancelled and immediately reject its promise.
    /// No-op if the task is already Done or Failed.
    fn cancel_task(&mut self, task_id: u64) -> Result<(), SemaError> {
        let task = self.tasks.iter_mut().find(|t| t.id == task_id).ok_or_else(|| {
            SemaError::eval(format!("async/cancel: no task with id {task_id}"))
        })?;
        // No-op for already completed or already cancelled tasks
        if matches!(task.state, TaskState::Done | TaskState::Failed) || task.cancelled {
            return Ok(());
        }
        task.cancelled = true;
        *task.promise.state.borrow_mut() = PromiseState::Rejected("cancelled".to_string());
        task.state = TaskState::Failed;
        Ok(())
    }
}

// ── Thread-local scheduler ─────────────────────────────────────────

thread_local! {
    static SCHEDULER: RefCell<Option<Scheduler>> = const { RefCell::new(None) };
}

/// Take the scheduler out of the thread-local temporarily.
/// The caller MUST put it back via `put_scheduler`.
fn take_scheduler() -> Result<Scheduler, SemaError> {
    SCHEDULER.with(|s| s.borrow_mut().take()).ok_or_else(|| {
        SemaError::eval("async scheduler not initialized (call init_scheduler first)")
    })
}

/// Put the scheduler back into the thread-local.
fn put_scheduler(sched: Scheduler) {
    SCHEDULER.with(|s| *s.borrow_mut() = Some(sched));
}

/// Spawn callback registered via `sema_core::set_spawn_callback`.
///
/// Called by the `async/spawn` stdlib function. Takes the scheduler
/// briefly to add the task, then puts it back immediately.
fn spawn_callback(ctx: &EvalContext, thunk: Value) -> Result<Value, SemaError> {
    let mut sched = take_scheduler()?;
    let result = sched.spawn(thunk, ctx);
    put_scheduler(sched);
    let promise = result?;
    Ok(Value::async_promise_from_rc(promise))
}

/// Run-scheduler callback registered via `sema_core::set_run_scheduler_callback`.
///
/// Takes the scheduler out of the thread-local, runs it, puts it back.
/// During task execution, the scheduler is put back temporarily so that
/// re-entrant calls (nested async/spawn) can access it.
fn run_scheduler_callback(
    ctx: &EvalContext,
    target: Option<Rc<AsyncPromise>>,
) -> Result<(), SemaError> {
    let mut sched = take_scheduler()?;
    let result = run_until_reentrant(&mut sched, ctx, target.as_ref());
    put_scheduler(sched);
    result
}

/// Run the scheduler event loop with re-entrant safety.
///
/// Before each task step, the scheduler is put back into the thread-local
/// so that nested `async/spawn` and `async/await` calls from within
/// task VMs can access it. After each step, the scheduler is taken back
/// out (it may have new tasks added by the step).
fn run_until_reentrant(
    sched: &mut Scheduler,
    ctx: &EvalContext,
    target: Option<&Rc<AsyncPromise>>,
) -> Result<(), SemaError> {
    const MAX_TICKS: u64 = 1_000_000;

    for _ in 0..MAX_TICKS {
        if let Some(t) = target {
            if !matches!(&*t.state.borrow(), PromiseState::Pending) {
                return Ok(());
            }
        }

        sched.wake_blocked_tasks();

        let ready_idx = sched
            .tasks
            .iter()
            .position(|t| matches!(t.state, TaskState::Ready));

        let Some(idx) = ready_idx else {
            let has_blocked = sched
                .tasks
                .iter()
                .any(|t| matches!(t.state, TaskState::Blocked(_)));
            if has_blocked {
                // Check if all blocked tasks are sleeping — if so, wait for
                // the nearest deadline instead of reporting deadlock.
                let all_sleeping = sched
                    .tasks
                    .iter()
                    .filter(|t| matches!(t.state, TaskState::Blocked(_)))
                    .all(|t| matches!(t.state, TaskState::Blocked(YieldReason::Sleep(_))));
                if all_sleeping {
                    #[cfg(not(target_arch = "wasm32"))]
                    {
                        // Find the nearest sleep deadline and wait
                        let nearest = sched
                            .tasks
                            .iter()
                            .filter_map(|t| t.sleep_deadline)
                            .min();
                        if let Some(deadline) = nearest {
                            let now = std::time::Instant::now();
                            if deadline > now {
                                std::thread::sleep(deadline - now);
                            }
                        }
                    }
                    continue; // Re-check after sleeping
                }
                return Err(SemaError::eval(
                    "async scheduler: all tasks blocked (deadlock detected)",
                ));
            }
            return Ok(());
        };

        // Extract the task from the scheduler, put the scheduler back
        // into the thread-local, then run the task. This allows nested
        // async/spawn and async/await inside the task VM to access the
        // scheduler via the thread-local.
        let mut task = sched.tasks.swap_remove(idx);

        // Check if task was cancelled before running it
        if task.cancelled {
            *task.promise.state.borrow_mut() = PromiseState::Rejected("cancelled".to_string());
            task.state = TaskState::Failed;
            sched.tasks.push(task);
            continue;
        }

        let taken = std::mem::replace(sched, Scheduler::new(Rc::new(Env::new()), Vec::new()));
        put_scheduler(taken);

        // Run the extracted task
        if let Some(val) = task.resume_value.take() {
            task.vm.replace_stack_top(val);
        }
        let prev_async = in_async_context();
        set_async_context(true);
        let result = if !task.started {
            task.started = true;
            task.vm.execute_async(task.closure.clone(), ctx)
        } else {
            task.vm.run_async(ctx)
        };
        set_async_context(prev_async);

        match result {
            Ok(VmExecResult::Finished(val)) => {
                *task.promise.state.borrow_mut() = PromiseState::Resolved(val);
                task.state = TaskState::Done;
            }
            Ok(VmExecResult::AsyncYield(reason)) => {
                task.state = TaskState::Blocked(reason);
            }
            Ok(_) => {}
            Err(e) => {
                *task.promise.state.borrow_mut() = PromiseState::Rejected(format!("{e}"));
                task.state = TaskState::Failed;
            }
        }

        // Take scheduler back, put the task back in
        let mut s = take_scheduler()?;
        s.tasks.push(task);
        *sched = s;
    }

    Err(SemaError::eval(
        "async scheduler: exceeded maximum ticks (possible infinite loop)",
    ))
}

/// Cancel callback registered via `sema_core::set_cancel_callback`.
///
/// Called by the `async/cancel` stdlib function. Takes the scheduler
/// briefly to cancel the task, then puts it back immediately.
fn cancel_callback(task_id: u64) -> Result<(), SemaError> {
    let mut sched = take_scheduler()?;
    let result = sched.cancel_task(task_id);
    put_scheduler(sched);
    result
}

/// Initialize the thread-local scheduler and register the spawn/run callbacks.
///
/// Must be called before any async operations. Typically called once
/// during VM startup with the global environment and function table
/// from the compiled program.
pub fn init_scheduler(globals: Rc<Env>, native_spurs: Vec<Spur>) {
    SCHEDULER.with(|s| {
        *s.borrow_mut() = Some(Scheduler::new(globals, native_spurs));
    });
    set_spawn_callback(spawn_callback);
    set_run_scheduler_callback(run_scheduler_callback);
    set_cancel_callback(cancel_callback);
}
