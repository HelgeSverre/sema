# Plan — Breakpoints & stepping inside async tasks

**Status:** Slice 1 (native DAP STOP+CONTINUE) + Slice 2 (WASM playground cooperative STOP+CONTINUE) SHIPPED 2026-06-23. Slice 3 (the three Slice-1/2 follow-ups) SHIPPED 2026-06-24. Bug write-up: `docs/bugs/async-breakpoints.md`.

## Slice 3 — shipped (the deferred follow-ups)

The three items parked after Slices 1–2 are now done:

1. **Async-stop inspection is verified and works in both debuggers.**
   - Native DAP: `handle_debug_stop` already runs on `task.vm`, so
     GetStackTrace/GetScopes/GetVariables target the stopped task's frames. Pinned
     by `dap_async_breakpoint_test::async_task_breakpoint_inspects_task_frame_locals`
     (task-local `n=42` visible, top frame at the breakpoint line).
   - WASM cooperative: the main VM is parked at the `await`, so inspection has to
     reach the paused task's per-task VM. `set_coop_task_stop` now records the
     paused task **id**; `scheduler::with_coop_paused_task_vm` relocates that task
     by id (no raw pointers across the JS boundary) and hands its VM to the
     inspection calls. `debug_get_stack_trace`/`debug_get_locals` route through it.
     Gate: `wasm_async_debug_test::coop_async_stop_inspects_paused_task_locals`
     (task VM sees `n=42`; main VM does NOT — proving the routing was required).

2. **Cooperative step depth is task-correct.** `debug_resume` measured
   `step_frame_depth` against the main VM, but a cooperative step re-drives the
   PAUSED TASK's VM — so when the main thread awaited from a deeper frame than the
   task, StepOut/StepOver compared against the wrong depth and StepOut wrongly
   stopped *inside* the task. Fixed to measure depth via `with_coop_paused_task_vm`.
   Gate: `wasm_async_debug_test::coop_async_step_over_and_out_use_task_depth`
   (mutation-verified: the old main-VM depth makes StepOut stop at line 3 inside
   the task). The native DAP path was already correct (`handle_debug_stop` sets the
   depth from `task.vm.frames.len()`).

3. **Missing-`DebugCoopResume` guard.** `surface_coop_task_stop` now enforces that
   a cooperative task stop only surfaces to JS when the scheduler-driving
   combinator registered a `DebugCoopResume` — a new combinator that paused the
   scheduler without one fails loudly instead of silently wedging the next resume.
   Unit test: `vm::tests::surface_coop_task_stop_requires_a_pending_resume`.

**Adversarial pass (2026-06-24)** found and fixed three session-boundary bugs the
unit gates missed: (a) a breakpoint inside a HOF callback (`map`/`filter`/…)
running in an async task drove the scheduler through `run_closure_as_inline_task`
— the 6th, previously-unhandled scheduler-driving path — and the cooperative
`DebugPaused` was swallowed into a spurious "HOF callback did not complete" error
that also bypassed the guard; (b) abandoning a session (Stop) while paused at an
async breakpoint leaked `DEBUG_COOP_RESUME` into the next session, whose first
Continue would re-drive the dead target and clobber the new VM's stack; (c) the
abandoned task survived `Ready` in the reused scheduler and could run under the
next program. Fixes: `run_closure_as_inline_task` auto-continues through
cooperative stops (forcing `Continue`, restored after) so the HOF completes;
`start_cooperative` scrubs `DEBUG_COOP_RESUME`/`COOP_TASK_STOP`/`COOP_PAUSED_TASK_ID`
and resets leftover scheduler tasks. Both regressions are mutation-verified
(`coop_breakpoint_in_hof_callback_in_async_task_completes`,
`coop_abandoned_async_session_does_not_poison_next_session`).

**Known cooperative limitation:** a breakpoint inside a HOF callback running in an
async task does NOT pause in the WASM playground (it auto-continues) — the nested
inline-task drive runs synchronously inside the owning task's native call and
cannot suspend back to JS without clobbering the outer resume target. The native
DAP debugger DOES pause there (blocking path). Revisit only if needed.

**Still deferred (genuinely out of scope):** stepping that follows control INTO a
concurrently-scheduled SIBLING task. (Adversarially confirmed safe — no panic/
wedge; a sibling can pause spuriously and Continue escapes — but the session-global
step state would need per-task scoping to contain it.) Step-within-a-task and step-out-of-a-task are
correct; making a `Step` interleave the stepper across independent sibling tasks
is confusing semantics (most debuggers don't), low-value, and would require
rebasing the session-global step state across per-task VM boundaries. Left parked
deliberately; revisit only if a concrete need appears.

## Slice 2 — shipped (WASM playground cooperative stop+continue)

The playground debugger is **cooperative** (`start_cooperative`/`run_cooperative`
return to JS between stops; `DebugState::new_headless()` has a disconnected command
channel) — so the blocking native `handle_debug_stop` can't be used. Design
*debug-pause-as-cooperative-yield*, reusing the existing `AsyncYield`→JS→`debug_poll`
resume plumbing:
- `DebugState` gained `headless`/`is_headless()`; `start_cooperative`/`run_cooperative`
  register the session via `ActiveDebugGuard` (so the scheduler's `step_task_debug`
  engages), the guard dropping when control returns to JS.
- On a task `Stopped` under a headless session, `step_task_debug` records the location
  (`vm::set_coop_task_stop`), leaves the task **Ready/paused (frames intact, not
  reaped)**, and `run_until_reentrant` returns `SchedulerRunResult::DebugPaused`. The
  scheduler-driving natives (`async/await`/`all`/`timeout`/`race`/`run`) yield the
  main VM and record a `DebugCoopResume`; `start_cooperative`/`run_cooperative`
  surface a `VmExecResult::Stopped(info)` (info.line = the task's breakpoint line) to JS.
- On the next `run_cooperative` (Continue): re-drive the scheduler (resume the paused
  task; a nested breakpoint surfaces as another `Stopped`), reconstruct the native's
  value from `DebugCoopResume`, resume the main VM.
- Gates: `crates/sema/tests/wasm_async_debug_test.rs` (cooperative STOP+CONTINUE,
  single/two-task/first-task) + `playground/tests/async-debugger.spec.ts` (e2e,
  injected program, verified headed). Non-debug async hot path byte-identical (gated
  on `is_debug_session_active()`/`is_headless()`).

Deferred (both slices): stepping across the scheduler into sibling tasks; full
stack/variable inspection targeting the paused task's VM (cooperative async stop
targets the main VM frame). One maintenance note: a new async combinator must add a
`DebugCoopResume` arm or its debug-resume value would be wrong.

## Slice 1 — shipped (native DAP stop+continue)

A breakpoint on a line that runs only inside an async task
(`async/spawn`/`async`/`async/map`/`pool-map`/`async/all`/channels) now STOPS under
the native DAP debugger and `Continue` resumes the task + scheduler to completion.

How:
- `ACTIVE_DEBUG` thread-local (`*mut DebugState`) in `crates/sema-vm/src/vm.rs`,
  mirroring `CURRENT_VM`. `execute_debug` registers the active session via an
  `ActiveDebugGuard` (popped on return/panic). The scheduler — reached through the
  `RUN_SCHEDULER_CALLBACK` fn-pointer seam, which can't carry a borrowed
  `&mut DebugState` — reborrows it via `with_active_debug(...)`, gated on the cheap
  `is_debug_session_active()` so the **non-debug async hot path is byte-identical**.
- `VM::execute_async_debug` / `run_async_debug` run the task step through
  `run_inner(ctx, Some(debug))`.
- The `Stopped` command loop was extracted from `execute_debug` into the reusable
  `VM::handle_debug_stop(ctx, debug, info) -> DebugStopResume`. The scheduler calls
  it on `task.vm`, so GetStackTrace/GetScopes/GetVariables target the **stopped
  task's VM** (its frames), not the main VM.
- Gate test: `crates/sema/tests/dap_async_breakpoint_test.rs` (async-task breakpoint
  stops+continues; sync control proves the harness).

Verified vs deferred (Slice 1):
- VERIFIED: stop on a breakpoint inside an async task; `Continue` resumes the task
  and the scheduler to completion. Workspace tests, `make examples` (81/0), lint all
  green; existing DAP tests unregressed.
- DEFERRED follow-ups (documented, not done in Slice 1):
  - **Stepping across the scheduler.** `Continue` is correct. `Step*` set the
    stopped task VM's step mode and stop again on the next line *within that task*,
    but stepping does not follow control across the scheduler boundary (into sibling
    tasks or back to the main VM); siblings stay parked. See the code comment on
    `scheduler::step_task_debug`.
  - **Full frame/scope inspection at an async stop.** `handle_debug_stop` targets
    `task.vm` so inspection requests are wired correctly, but inspection at an async
    stop is not yet covered by an integration test — left as a follow-up (Slice 1
    lands STOP+CONTINUE solidly).
  - **WASM playground** (the harder suspend/resume-through-the-scheduler half) is a
    separate later slice — native DAP only here.

---

## Original scoping (below)
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
