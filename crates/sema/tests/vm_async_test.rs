mod common;

use common::eval_vm;
use sema_core::Value;

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
