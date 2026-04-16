mod common;

use common::eval_vm;
use sema_core::{Caps, Sandbox, Value};

fn eval_vm_err(input: &str) -> String {
    let interp = sema_eval::Interpreter::new();
    interp.eval_str_compiled(input).unwrap_err().to_string()
}

// === Basic async/spawn + await ===

#[test]
fn async_spawn_await() {
    assert_eq!(
        eval_vm(r#"(let ((p (async/spawn (fn () (+ 1 2))))) (async/await p))"#),
        Value::int(3)
    );
}

// === async special form ===

#[test]
fn async_special_form() {
    assert_eq!(
        eval_vm(r#"(let ((p (async (+ 10 20)))) (await p))"#),
        Value::int(30)
    );
}

// === async with multiple expressions in body ===

#[test]
fn async_multi_body() {
    assert_eq!(
        eval_vm(r#"(let ((p (async (define x 10) (define y 20) (+ x y)))) (await p))"#),
        Value::int(30)
    );
}

// === async/all ===

#[test]
fn async_all() {
    assert_eq!(
        eval_vm(
            r#"(let ((p1 (async (+ 1 1))) (p2 (async (+ 2 2))) (p3 (async (+ 3 3)))) (async/all (list p1 p2 p3)))"#
        ),
        Value::list(vec![Value::int(2), Value::int(4), Value::int(6)])
    );
}

// === async/resolved and async/rejected ===

#[test]
fn async_resolved() {
    assert_eq!(eval_vm("(async/await (async/resolved 42))"), Value::int(42));
}

#[test]
fn async_rejected() {
    let err = eval_vm_err(r#"(async/await (async/rejected "oops"))"#);
    assert!(
        err.contains("rejected"),
        "expected rejection error, got: {err}"
    );
}

// === Promise predicates ===

#[test]
fn async_promise_predicate() {
    assert_eq!(
        eval_vm("(async/promise? (async/resolved 1))"),
        Value::bool(true)
    );
}

#[test]
fn async_promise_predicate_false() {
    assert_eq!(eval_vm("(async/promise? 42)"), Value::bool(false));
}

#[test]
fn async_resolved_predicate() {
    assert_eq!(
        eval_vm("(async/resolved? (async/resolved 1))"),
        Value::bool(true)
    );
}

#[test]
fn async_rejected_predicate() {
    assert_eq!(
        eval_vm(r#"(async/rejected? (async/rejected "x"))"#),
        Value::bool(true)
    );
}

#[test]
fn async_pending_predicate() {
    // A freshly spawned task is pending until awaited
    assert_eq!(
        eval_vm("(async/pending? (async (+ 1 2)))"),
        Value::bool(true)
    );
}

// === Channel basics ===

#[test]
fn channel_send_recv() {
    assert_eq!(
        eval_vm("(let ((ch (channel/new 3))) (channel/send ch 10) (channel/send ch 20) (channel/recv ch))"),
        Value::int(10)
    );
}

#[test]
fn channel_fifo() {
    assert_eq!(
        eval_vm("(let ((ch (channel/new 3))) (channel/send ch :a) (channel/send ch :b) (channel/recv ch) (channel/recv ch))"),
        Value::keyword("b")
    );
}

#[test]
fn channel_count() {
    assert_eq!(
        eval_vm("(let ((ch (channel/new 5))) (channel/send ch 1) (channel/send ch 2) (channel/count ch))"),
        Value::int(2)
    );
}

#[test]
fn channel_empty() {
    assert_eq!(
        eval_vm("(channel/empty? (channel/new 1))"),
        Value::bool(true)
    );
}

#[test]
fn channel_predicate() {
    assert_eq!(eval_vm("(channel? (channel/new 1))"), Value::bool(true));
}

#[test]
fn channel_predicate_false() {
    assert_eq!(eval_vm("(channel? 42)"), Value::bool(false));
}

#[test]
fn channel_close() {
    assert_eq!(
        eval_vm("(let ((ch (channel/new 1))) (channel/close ch) (channel/closed? ch))"),
        Value::bool(true)
    );
}

#[test]
fn channel_try_recv_empty() {
    assert_eq!(eval_vm("(channel/try-recv (channel/new 1))"), Value::nil());
}

#[test]
fn channel_full() {
    assert_eq!(
        eval_vm("(let ((ch (channel/new 1))) (channel/send ch 42) (channel/full? ch))"),
        Value::bool(true)
    );
}

// === Async producer/consumer with channels ===

#[test]
fn async_producer_consumer() {
    assert_eq!(
        eval_vm(
            r#"(let ((ch (channel/new 1)))
          (let ((producer (async (channel/send ch 42)))
                (consumer (async (channel/recv ch))))
            (await consumer)))"#
        ),
        Value::int(42)
    );
}

#[test]
fn async_producer_consumer_multi() {
    assert_eq!(
        eval_vm(
            r#"(let ((ch (channel/new 2)))
          (let ((producer (async
                  (channel/send ch 10)
                  (channel/send ch 20)))
                (consumer (async
                  (let ((a (channel/recv ch))
                        (b (channel/recv ch)))
                    (+ a b)))))
            (await consumer)))"#
        ),
        Value::int(30)
    );
}

// === async/race ===

#[test]
fn async_race_first_wins() {
    assert_eq!(
        eval_vm(
            r#"(let ((fast (async/resolved 1))
              (slow (async (+ 2 2))))
          (async/race (list fast slow)))"#
        ),
        Value::int(1)
    );
}

#[test]
fn async_race_returns_first_resolved_in_list_order() {
    // With real race semantics, the sender settles first after sending. The
    // blocked receiver is only woken on a later scheduler turn.
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1)))
              (let ((first (async (channel/recv ch)))
                    (second (async
                              (channel/send ch :sent)
                              :sender-done)))
                (async/race (list first second))))
        "#
        ),
        Value::keyword("sender-done")
    );
}

// === async/sleep ===

#[test]
fn async_sleep_returns_nil() {
    assert_eq!(
        eval_vm("(let ((p (async (async/sleep 0)))) (await p))"),
        Value::nil()
    );
}

// === Error cases ===

#[test]
fn channel_send_closed_error() {
    let err = eval_vm_err("(let ((ch (channel/new 1))) (channel/close ch) (channel/send ch 1))");
    assert!(err.contains("closed"), "expected closed error, got: {err}");
}

#[test]
fn channel_recv_empty_error() {
    let err = eval_vm_err("(channel/recv (channel/new 1))");
    assert!(err.contains("empty"), "expected empty error, got: {err}");
}

#[test]
fn channel_send_full_error() {
    let err = eval_vm_err("(let ((ch (channel/new 1))) (channel/send ch 1) (channel/send ch 2))");
    assert!(err.contains("full"), "expected full error, got: {err}");
}

#[test]
fn channel_zero_capacity_error() {
    let err = eval_vm_err("(channel/new 0)");
    assert!(
        err.contains("capacity"),
        "expected capacity error, got: {err}"
    );
}

// === Tree-walker rejects async ===

#[test]
fn tree_walker_rejects_async() {
    let interp = sema_eval::Interpreter::new();
    let err = interp.eval_str("(async (+ 1 2))").unwrap_err().to_string();
    assert!(
        err.contains("VM backend"),
        "expected VM backend error, got: {err}"
    );
}

// ── Nested async ──────────────────────────────────────────────────

#[test]
fn nested_async_await() {
    assert_eq!(eval_vm("(await (async (await (async 7))))"), Value::int(7),);
}

#[test]
fn nested_async_multiple_awaits() {
    assert_eq!(
        eval_vm("(await (async (+ (await (async 3)) (await (async 4)))))"),
        Value::int(7),
    );
}

#[test]
fn triple_nested_async() {
    assert_eq!(
        eval_vm("(await (async (await (async (await (async 42))))))"),
        Value::int(42),
    );
}

// === Bug regression tests ===

#[test]
fn await_rejected_propagates() {
    let err = eval_vm_err(r#"(await (async (await (async (/ 1 0)))))"#);
    assert!(!err.is_empty(), "should propagate inner rejection");
}

#[test]
fn async_context_preserved_after_nested_run() {
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1)))
              (await (async
                (async/run)
                (channel/send ch 42)
                (channel/recv ch))))
        "#
        ),
        Value::int(42)
    );
}

#[test]
fn channel_close_rejects_pending_send() {
    let err = eval_vm_err(
        r#"
        (let ((ch (channel/new 1)))
          (channel/send ch :fill)
          (let ((sender (async (channel/send ch :blocked))))
            (channel/close ch)
            (channel/recv ch)
            (await sender)))
    "#,
    );
    assert!(
        err.contains("closed") || err.contains("rejected"),
        "should reject pending send on closed channel, got: {err}"
    );
}

#[test]
fn nested_async_with_channel() {
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1)))
              (await (async
                (let ((inner (async (channel/recv ch))))
                  (channel/send ch 99)
                  (await inner)))))
        "#
        ),
        Value::int(99),
    );
}

// === async/timeout ===

#[test]
fn timeout_resolved_in_time() {
    assert_eq!(
        eval_vm("(async/timeout 1000 (async/resolved 42))"),
        Value::int(42),
    );
}

#[test]
fn timeout_task_completes_in_time() {
    assert_eq!(
        eval_vm("(async/timeout 1000 (async (+ 1 2)))"),
        Value::int(3),
    );
}

#[test]
fn timeout_already_rejected() {
    let err = eval_vm_err(r#"(async/timeout 1000 (async/rejected "oops"))"#);
    assert!(err.contains("rejected"), "expected rejection, got: {err}");
}

#[test]
fn timeout_negative_duration_error() {
    let err = eval_vm_err("(async/timeout -1 (async/resolved 1))");
    assert!(
        err.contains("non-negative"),
        "expected non-negative error, got: {err}"
    );
}

#[test]
fn timeout_expires() {
    let err = eval_vm_err(
        r#"
        (let ((ch (channel/new 1)))
          (async/timeout 50 (async (channel/recv ch))))
    "#,
    );
    assert!(err.contains("timed out"), "expected timeout, got: {err}");
}

#[test]
fn timeout_beats_sleeping_task() {
    let err = eval_vm_err("(async/timeout 10 (async (async/sleep 100) 42))");
    assert!(
        err.contains("timed out"),
        "expected timeout before sleep completion, got: {err}"
    );
}

#[test]
fn async_race_returns_first_to_settle_not_list_order() {
    assert_eq!(
        eval_vm("(async/race (list (async (async/sleep 100) :slow) (async :fast)))"),
        Value::keyword("fast"),
    );
}

#[test]
fn async_all_ignores_unrelated_blocked_task() {
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1)))
              (let ((bg (async (channel/recv ch)))
                    (p (async 1)))
                (async/all (list p))))
        "#
        ),
        Value::list(vec![Value::int(1)]),
    );
}

#[test]
fn async_task_survives_separate_vm_evals() {
    let interp = sema_eval::Interpreter::new();
    interp
        .eval_str_compiled("(define p (async (async/sleep 1) 42))")
        .unwrap();
    assert_eq!(
        interp.eval_str_compiled("(await p)").unwrap(),
        Value::int(42)
    );
}

#[test]
fn serial_list_respects_sandbox() {
    let sandbox = Sandbox::deny(Caps::SERIAL);
    let interp = sema_eval::Interpreter::new_with_sandbox(&sandbox);
    let err = interp
        .eval_str_compiled("(serial/list)")
        .unwrap_err()
        .to_string();
    assert!(
        err.contains("Permission denied") && err.contains("serial"),
        "expected serial sandbox denial, got: {err}"
    );
}

// === Task cancellation ===

#[test]
fn cancel_pending_task() {
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1)))
              (let ((p (async (channel/recv ch))))
                (async/cancel p)
                (async/cancelled? p)))
        "#
        ),
        Value::bool(true),
    );
}

#[test]
fn cancel_awaited_task_rejects() {
    let err = eval_vm_err(
        r#"
        (let ((ch (channel/new 1)))
          (let ((p (async (channel/recv ch))))
            (async/cancel p)
            (await p)))
    "#,
    );
    assert!(
        err.contains("cancelled"),
        "expected cancellation error, got: {err}"
    );
}

#[test]
fn cancel_completed_task_is_noop() {
    assert_eq!(
        eval_vm(
            r#"
            (let ((p (async 42)))
              (await p)
              (async/cancel p)
              (async/resolved? p))
        "#
        ),
        Value::bool(true),
    );
}

// === Bug regression: yield signal through op::CALL ===

#[test]
fn channel_recv_via_local_variable_yields_correctly() {
    // channel/recv called through a local variable binding goes through
    // op::CALL (not CALL_NATIVE or CALL_GLOBAL). The yield signal must
    // still be checked after call_value returns.
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1))
                  (recv channel/recv))
              (let ((producer (async (channel/send ch 42)))
                    (consumer (async (recv ch))))
                (await consumer)))
        "#
        ),
        Value::int(42),
    );
}

#[test]
fn channel_send_via_local_variable_yields_correctly() {
    // Same bug but for channel/send through a local binding
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1))
                  (send channel/send))
              (let ((consumer (async (channel/recv ch))))
                (send ch 99)
                (await consumer)))
        "#
        ),
        Value::int(99),
    );
}

// === Bug regression: false deadlock with mixed blocked tasks ===

#[test]
fn sleeping_task_unblocks_channel_recv() {
    // A sleeping task will eventually send to a channel. The scheduler
    // must not report deadlock when one task sleeps and another waits
    // on channel/recv.
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1)))
              (async/spawn (fn () (async/sleep 1) (channel/send ch 42)))
              (let ((consumer (async (channel/recv ch))))
                (await consumer)))
        "#
        ),
        Value::int(42),
    );
}

// === async/all error handling ===

#[test]
fn async_all_rejects_on_any_failure() {
    let err = eval_vm_err(
        r#"(async/all (list (async 1) (async (/ 1 0)) (async 3)))"#,
    );
    assert!(
        err.contains("rejected") || err.contains("division") || err.contains("zero"),
        "expected rejection from division error, got: {err}"
    );
}

#[test]
fn async_all_empty_list() {
    assert_eq!(eval_vm("(async/all (list))"), Value::list(vec![]));
}

// === async/race edge cases ===

#[test]
fn async_race_empty_list_errors() {
    let err = eval_vm_err("(async/race (list))");
    assert!(
        err.contains("requires at least one"),
        "expected arity error, got: {err}"
    );
}

#[test]
fn async_race_all_rejected() {
    let err = eval_vm_err(
        r#"(async/race (list (async (error "a")) (async (error "b"))))"#,
    );
    assert!(!err.is_empty(), "expected rejection error, got empty");
}

// === Channel close semantics ===

#[test]
fn channel_recv_closed_empty_returns_nil() {
    assert_eq!(
        eval_vm("(let ((ch (channel/new 1))) (channel/close ch) (channel/recv ch))"),
        Value::nil(),
    );
}

#[test]
fn channel_recv_wakes_on_close_with_nil() {
    // An async task blocked on recv should be woken with nil when
    // the channel is closed.
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 1)))
              (let ((consumer (async (channel/recv ch))))
                (channel/close ch)
                (await consumer)))
        "#
        ),
        Value::nil(),
    );
}

// === Timeout edge cases ===

#[test]
fn timeout_zero_expires_immediately() {
    let err = eval_vm_err(
        r#"(async/timeout 0 (async (channel/recv (channel/new 1))))"#,
    );
    assert!(
        err.contains("timed out"),
        "expected timeout error, got: {err}"
    );
}

// === Deadlock detection ===

#[test]
fn deadlock_detected_two_tasks_waiting() {
    let err = eval_vm_err(
        r#"
        (let ((ch1 (channel/new 1))
              (ch2 (channel/new 1)))
          (let ((t1 (async (channel/recv ch1)))
                (t2 (async (channel/recv ch2))))
            (async/all (list t1 t2))))
    "#,
    );
    assert!(
        err.contains("deadlock") || err.contains("blocked"),
        "expected deadlock error, got: {err}"
    );
}

// === Strengthen weak assertion ===

#[test]
fn await_rejected_propagates_division_error() {
    let err = eval_vm_err(r#"(await (async (await (async (/ 1 0)))))"#);
    assert!(
        err.contains("division") || err.contains("zero"),
        "should propagate division-by-zero cause, got: {err}"
    );
}

// === Multiple senders ===

#[test]
fn channel_two_senders_one_receiver() {
    assert_eq!(
        eval_vm(
            r#"
            (let ((ch (channel/new 2)))
              (let ((s1 (async (channel/send ch 10)))
                    (s2 (async (channel/send ch 20)))
                    (r  (async (+ (channel/recv ch) (channel/recv ch)))))
                (await r)))
        "#
        ),
        Value::int(30),
    );
}
