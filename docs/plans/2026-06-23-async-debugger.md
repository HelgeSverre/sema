# Plan — Breakpoints & stepping inside async tasks

**Status:** Scoped, not started (2026-06-23). Bug write-up: `docs/bugs/async-breakpoints.md`.
**Goal:** breakpoints, stepping, and pause/inspect work for code running inside the
cooperative scheduler (`async`/`async/spawn`/`async/map`/`pool-map`/channels), in both
the WASM playground and the native DAP — to the same fidelity as synchronous code.

## Why it's not a one-liner

Today the breakpoint/step machinery only runs when `VM::run_inner` is called with
`Some(&mut DebugState)` (`vm.rs:875`). The scheduler steps every task with
`run_async`/`execute_async` → `run_inner(ctx, None)` (`vm.rs:785,815`; `scheduler.rs:894`),
so async tasks execute in non-debug mode. Three things must change together:

1. **Reach the `DebugState` from the scheduler.** The scheduler (`sema-vm`) currently
   takes no debug context. Either thread `Option<&mut DebugState>` down
   `run_scheduler_callback → run_until_reentrant → task step`, or stash it in a
   thread-local the scheduler reads at each step. Thread-local is likely cleaner
   because the scheduler is reached via the `RUN_SCHEDULER_CALLBACK` fn-pointer seam
   (`async_signal.rs`) that can't easily carry a borrowed `&mut DebugState`.
2. **Run task steps in debug mode.** Add `run_async_debug`/`execute_async_debug`
   (or a flag) that call `run_inner(ctx, Some(debug))`, and have the scheduler use
   them when a debug session is active. Each per-task VM (`VM::new_for_task`) shares
   the SAME `DebugState` (breakpoints are global to the session, not per-VM).
3. **Stop/resume across the scheduler.** A breakpoint hit mid-task surfaces as
   `VmExecResult::Stopped`. The VM's debug loop already parks waiting for a
   `DebugCommand` inside one `run_inner` call — for the **native DAP** (threaded,
   blocking) that may "just work": the task step blocks on the command channel, which
   pauses the scheduler thread, exactly the desired behavior. For the **WASM
   playground** the model is step-driven (`debug_start`/`debug_continue`/`debug_step`
   return control to JS between stops — `sema-wasm/src/lib.rs:2017+`), so a `Stopped`
   from inside the scheduler must unwind back out to JS *with the scheduler's state
   intact* and resume INTO the scheduler on the next `debug_continue`. This is the
   real design work: the scheduler must be suspendable at a breakpoint and resumable,
   not just run-to-completion.

## Acceptance gate

- **Native DAP:** a breakpoint inside `(async/spawn (fn () …))` stops with a correct
  stack/locals view; continue/step/step-over/step-out behave; the repro in the bug
  doc stops. A DAP integration test (model on existing DAP tests) covers it.
- **Playground (WASM):** `debug_start` with a breakpoint inside an async task stops and
  reports frames; `debug_continue`/`debug_step` resume correctly through the scheduler;
  a Playwright/WASM test asserts the stop.
- **No regression:** synchronous-code debugging unchanged; non-debug async runs
  unchanged (zero overhead when no session is active — gate on a cheap `is_debugging`
  check so the hot async path stays identical when not debugging).

## Open design questions (decide first)

- Multi-task stepping semantics: when stopped in task A, does "step" step A only, or
  the whole scheduler? (Likely: step the stopped task; siblings stay parked.)
- Frame/scope reporting across the per-task VM boundary (the DAP's stack/variables
  requests must target the stopped task's VM, not the main VM).
- Whether to show the scheduler/other parked tasks in the call-stack / threads view
  (DAP "threads" could map to scheduler tasks — a nice-to-have, not v1).

## Rough sequencing

1. Native DAP first (simpler stop/resume — blocking command channel). Thread/stash
   `DebugState`, add debug task-step path, prove the bug-doc repro stops. 
2. Then the WASM playground stop/resume-through-the-scheduler (the harder half).
3. Optional: map scheduler tasks → DAP threads for a multi-task debugging view.
