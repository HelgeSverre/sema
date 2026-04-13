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
    set_async_context, set_run_scheduler_callback, set_spawn_callback, AsyncPromise, Env,
    EvalContext, PromiseState, SemaError, Spur, Value, YieldReason,
};

use crate::chunk::Function;
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
    /// Value to pass to the VM on resume (set by `wake_blocked_tasks`).
    resume_value: Option<Value>,
}

// ── Scheduler ──────────────────────────────────────────────────────

/// Cooperative task scheduler managing multiple VM-backed async tasks.
pub struct Scheduler {
    tasks: Vec<Task>,
    next_id: u64,
    /// Shared global environment for spawning child VMs.
    globals: Rc<Env>,
    /// Shared function table from the parent compilation.
    functions: Rc<Vec<Rc<Function>>>,
    /// Native function spurs for resolving the native dispatch table in child VMs.
    native_spurs: Vec<Spur>,
}

impl Scheduler {
    /// Create a new scheduler with shared state from the parent VM.
    pub fn new(
        globals: Rc<Env>,
        functions: Rc<Vec<Rc<Function>>>,
        native_spurs: Vec<Spur>,
    ) -> Self {
        Scheduler {
            tasks: Vec::new(),
            next_id: 1,
            globals,
            functions,
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
            SemaError::eval(
                "async/spawn: argument must be a function (compiled VM closure)",
            )
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
        let vm = VM::new_for_task(
            self.globals.clone(),
            functions,
            &self.native_spurs,
        )?;

        self.tasks.push(Task {
            id,
            vm,
            closure,
            promise: promise.clone(),
            state: TaskState::Ready,
            started: false,
            resume_value: None,
        });

        Ok(promise)
    }

    /// Check blocked tasks and transition them to Ready if their
    /// blocking condition has been satisfied.
    fn wake_blocked_tasks(&mut self) {
        for task in &mut self.tasks {
            if let TaskState::Blocked(ref reason) = task.state {
                let resume = match reason {
                    YieldReason::AwaitPromise(p) => {
                        let state = p.state.borrow();
                        match &*state {
                            PromiseState::Resolved(v) => Some(v.clone()),
                            PromiseState::Rejected(_) => Some(Value::nil()),
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
                    // Sleep yields resume immediately — the scheduler does not
                    // track real time, so sleeps are effectively zero-duration.
                    YieldReason::Sleep(_) => Some(Value::nil()),
                };

                if let Some(val) = resume {
                    task.resume_value = Some(val);
                    task.state = TaskState::Ready;
                }
            }
        }
    }

    /// Execute one step of a single task.
    ///
    /// Returns `Ok(true)` if progress was made, `Ok(false)` if the task
    /// produced an unexpected result (should not happen in practice).
    fn run_task(&mut self, idx: usize, ctx: &EvalContext) -> Result<bool, SemaError> {
        let task = &mut self.tasks[idx];

        // Supply the resume value (if any) by replacing the nil placeholder
        // that the VM left on the stack when it yielded.
        if let Some(val) = task.resume_value.take() {
            task.vm.replace_stack_top(val);
        }

        set_async_context(true);

        let result = if !task.started {
            task.started = true;
            task.vm.execute_async(task.closure.clone(), ctx)
        } else {
            task.vm.run_async(ctx)
        };

        set_async_context(false);

        match result {
            Ok(VmExecResult::Finished(val)) => {
                *task.promise.state.borrow_mut() = PromiseState::Resolved(val);
                task.state = TaskState::Done;
                Ok(true)
            }
            Ok(VmExecResult::AsyncYield(reason)) => {
                task.state = TaskState::Blocked(reason);
                Ok(true)
            }
            // Stopped (debug breakpoint) or Yielded (budget exhausted) should
            // not occur during async scheduling.
            Ok(_) => Ok(false),
            Err(e) => {
                *task.promise.state.borrow_mut() =
                    PromiseState::Rejected(format!("{e}"));
                task.state = TaskState::Failed;
                Ok(true)
            }
        }
    }

    /// Run the scheduler event loop until the target promise resolves
    /// (or all tasks complete if `target` is `None`).
    ///
    /// Uses round-robin scheduling with a hard upper bound on total
    /// ticks to prevent infinite loops.
    pub fn run_until(
        &mut self,
        ctx: &EvalContext,
        target: Option<&Rc<AsyncPromise>>,
    ) -> Result<(), SemaError> {
        const MAX_TICKS: u64 = 1_000_000;

        for _ in 0..MAX_TICKS {
            // Check if the target promise has been resolved/rejected.
            if let Some(t) = target {
                if !matches!(&*t.state.borrow(), PromiseState::Pending) {
                    return Ok(());
                }
            }

            // Try to wake any blocked tasks whose conditions are met.
            self.wake_blocked_tasks();

            // Find the next ready task (round-robin by position).
            let ready_idx = self
                .tasks
                .iter()
                .position(|t| matches!(t.state, TaskState::Ready));

            let Some(idx) = ready_idx else {
                // No ready tasks — check if any are still blocked.
                let has_blocked = self
                    .tasks
                    .iter()
                    .any(|t| matches!(t.state, TaskState::Blocked(_)));
                if has_blocked {
                    return Err(SemaError::eval(
                        "async scheduler: all tasks blocked (deadlock detected)",
                    ));
                }
                // All tasks are Done or Failed — nothing left to do.
                return Ok(());
            };

            self.run_task(idx, ctx)?;
        }

        Err(SemaError::eval(
            "async scheduler: exceeded maximum ticks (possible infinite loop)",
        ))
    }

    /// Run all tasks to completion (convenience wrapper around `run_until`).
    pub fn run_all(&mut self, ctx: &EvalContext) -> Result<(), SemaError> {
        self.run_until(ctx, None)
    }
}

// ── Thread-local scheduler ─────────────────────────────────────────

thread_local! {
    static SCHEDULER: RefCell<Option<Scheduler>> = const { RefCell::new(None) };
}

/// Access the thread-local scheduler. Panics if no scheduler is initialized.
fn with_scheduler<F, R>(f: F) -> R
where
    F: FnOnce(&mut Scheduler) -> R,
{
    SCHEDULER.with(|s| {
        let mut borrow = s.borrow_mut();
        let sched = borrow
            .as_mut()
            .expect("async scheduler not initialized (call init_scheduler first)");
        f(sched)
    })
}

/// Spawn callback registered via `sema_core::set_spawn_callback`.
///
/// Called by the `async/spawn` stdlib function. Delegates to the
/// thread-local scheduler's `spawn` method and returns the promise
/// wrapped as a `Value`.
fn spawn_callback(ctx: &EvalContext, thunk: Value) -> Result<Value, SemaError> {
    let promise = with_scheduler(|sched| sched.spawn(thunk, ctx))?;
    Ok(Value::async_promise_from_rc(promise))
}

/// Run-scheduler callback registered via `sema_core::set_run_scheduler_callback`.
///
/// Called by the `async/await` and `async/run` stdlib functions.
/// Runs the scheduler event loop until the target promise resolves
/// (or all tasks complete if `target` is `None`).
fn run_scheduler_callback(
    ctx: &EvalContext,
    target: Option<Rc<AsyncPromise>>,
) -> Result<(), SemaError> {
    SCHEDULER.with(|s| {
        let mut borrow = s.borrow_mut();
        let sched = borrow.as_mut().ok_or_else(|| {
            SemaError::eval(
                "async scheduler not initialized (call init_scheduler first)",
            )
        })?;
        sched.run_until(ctx, target.as_ref())
    })
}

/// Initialize the thread-local scheduler and register the spawn/run callbacks.
///
/// Must be called before any async operations. Typically called once
/// during VM startup with the global environment and function table
/// from the compiled program.
pub fn init_scheduler(
    globals: Rc<Env>,
    functions: Rc<Vec<Rc<Function>>>,
    native_spurs: Vec<Spur>,
) {
    SCHEDULER.with(|s| {
        *s.borrow_mut() = Some(Scheduler::new(globals, functions, native_spurs));
    });
    set_spawn_callback(spawn_callback);
    set_run_scheduler_callback(run_scheduler_callback);
}
