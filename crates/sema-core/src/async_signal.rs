//! Async yield/resume signaling infrastructure.
//!
//! Thread-local signals for cooperative async scheduling. Native functions
//! (channel/recv, async/await, etc.) set `YIELD_SIGNAL` when they need to
//! suspend; the VM checks it after each native call. On resume, the scheduler
//! sets `RESUME_VALUE` so the native function can return the resolved value.
//!
//! Lives in sema-core (not sema-vm) so sema-stdlib can use it without
//! depending on sema-vm. Follows the same pattern as `set_eval_callback`.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use crate::value::{AsyncPromise, Channel, Value};
use crate::{EvalContext, SemaError};

/// Reason a task is yielding control back to the scheduler.
#[derive(Debug, Clone)]
pub enum YieldReason {
    /// Waiting for a promise to resolve.
    AwaitPromise(Rc<AsyncPromise>),
    /// Waiting to receive from an empty channel.
    ChannelRecv(Rc<Channel>),
    /// Waiting to send to a full channel (carries the value to send).
    ChannelSend(Rc<Channel>, Value),
    /// Sleeping for a duration in milliseconds.
    Sleep(u64),
}

thread_local! {
    /// Set by native functions that need to yield. Checked by the VM after
    /// each native call. If set, the VM suspends the current task.
    static YIELD_SIGNAL: RefCell<Option<YieldReason>> = const { RefCell::new(None) };

    /// Set by the scheduler before resuming a yielded task. The native
    /// function that previously yielded checks this first and returns it
    /// instead of re-executing the operation.
    static RESUME_VALUE: RefCell<Option<Value>> = const { RefCell::new(None) };

    /// Whether we are currently executing inside an async task.
    /// Native functions check this to decide between yielding and erroring.
    static IN_ASYNC_CONTEXT: Cell<bool> = const { Cell::new(false) };
}

// ── Yield signal ────────────────────────────────────────────────

/// Set the yield signal. Called by native functions that need to suspend.
pub fn set_yield_signal(reason: YieldReason) {
    YIELD_SIGNAL.with(|s| *s.borrow_mut() = Some(reason));
}

/// Take the yield signal (clearing it). Called by the VM after native calls.
pub fn take_yield_signal() -> Option<YieldReason> {
    YIELD_SIGNAL.with(|s| s.borrow_mut().take())
}

// ── Resume value ────────────────────────────────────────────────

/// Set the resume value. Called by the scheduler before resuming a task.
pub fn set_resume_value(val: Value) {
    RESUME_VALUE.with(|r| *r.borrow_mut() = Some(val));
}

/// Take the resume value (clearing it). Called by the native function
/// that previously yielded, returning this instead of re-executing.
pub fn take_resume_value() -> Option<Value> {
    RESUME_VALUE.with(|r| r.borrow_mut().take())
}

// ── Async context ───────────────────────────────────────────────

/// Check if we are currently inside an async task.
pub fn in_async_context() -> bool {
    IN_ASYNC_CONTEXT.with(|c| c.get())
}

/// Set whether we are inside an async task.
pub fn set_async_context(val: bool) {
    IN_ASYNC_CONTEXT.with(|c| c.set(val));
}

// ── Spawn callback ──────────────────────────────────────────────

/// Callback type for spawning async tasks.
/// Takes the thunk (zero-arg function) and returns the promise value.
/// Registered by the scheduler in sema-vm at startup.
pub type SpawnCallbackFn = fn(&EvalContext, Value) -> Result<Value, SemaError>;

thread_local! {
    static SPAWN_CALLBACK: Cell<Option<SpawnCallbackFn>> = const { Cell::new(None) };
}

/// Register the spawn callback. Called by the scheduler during init.
pub fn set_spawn_callback(f: SpawnCallbackFn) {
    SPAWN_CALLBACK.with(|cb| cb.set(Some(f)));
}

/// Spawn an async task via the registered callback.
/// Returns an error if no scheduler has been registered.
pub fn call_spawn_callback(ctx: &EvalContext, thunk: Value) -> Result<Value, SemaError> {
    let f = SPAWN_CALLBACK.with(|cb| cb.get()).ok_or_else(|| {
        SemaError::eval(
            "async/spawn: no async scheduler registered (async requires the VM backend)"
                .to_string(),
        )
    })?;
    f(ctx, thunk)
}

// ── Run-scheduler callback ──────────────────────────────────────

/// Callback type for running the scheduler until a promise resolves.
/// Takes an optional promise to wait for (None = run all tasks).
pub type RunSchedulerCallbackFn =
    fn(&EvalContext, Option<Rc<AsyncPromise>>) -> Result<(), SemaError>;

thread_local! {
    static RUN_SCHEDULER_CALLBACK: Cell<Option<RunSchedulerCallbackFn>> = const { Cell::new(None) };
}

/// Register the run-scheduler callback.
pub fn set_run_scheduler_callback(f: RunSchedulerCallbackFn) {
    RUN_SCHEDULER_CALLBACK.with(|cb| cb.set(Some(f)));
}

/// Run the scheduler, optionally waiting for a specific promise.
pub fn call_run_scheduler(
    ctx: &EvalContext,
    target: Option<Rc<AsyncPromise>>,
) -> Result<(), SemaError> {
    let f = RUN_SCHEDULER_CALLBACK.with(|cb| cb.get()).ok_or_else(|| {
        SemaError::eval(
            "async: no async scheduler registered (async requires the VM backend)".to_string(),
        )
    })?;
    f(ctx, target)
}
