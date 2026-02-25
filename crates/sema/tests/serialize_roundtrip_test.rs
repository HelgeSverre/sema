//! End-to-end serialize round-trip tests for the bytecode format.
//!
//! Each test: compile → serialize → deserialize → run in VM → compare to tree-walker.

mod common;

use sema_core::Value;
use sema_eval::Interpreter;
use sema_vm::{deserialize_from_bytes, serialize_to_bytes, Closure, Function, VM};
use std::rc::Rc;

/// Compile source, serialize to bytes, deserialize back, execute, and return the result.
fn eval_roundtrip(input: &str) -> Value {
    let interp = Interpreter::new();

    // 1. Compile to bytecode
    let compiled = interp
        .compile_to_bytecode(input)
        .unwrap_or_else(|e| panic!("compile failed for `{input}`: {e}"));

    // 2. Serialize to bytes
    let bytes = serialize_to_bytes(&compiled, 0)
        .unwrap_or_else(|e| panic!("serialize failed for `{input}`: {e}"));

    // 3. Deserialize back
    let deserialized = deserialize_from_bytes(&bytes)
        .unwrap_or_else(|e| panic!("deserialize failed for `{input}`: {e}"));

    // 4. Execute the deserialized program
    let functions: Vec<Rc<Function>> = deserialized.functions.into_iter().map(Rc::new).collect();
    let closure = Rc::new(Closure {
        func: Rc::new(Function {
            name: None,
            chunk: deserialized.chunk,
            upvalue_descs: Vec::new(),
            arity: 0,
            has_rest: false,
            local_names: Vec::new(),
            source_file: None,
        }),
        upvalues: Vec::new(),
    });
    let mut vm = VM::new(interp.global_env.clone(), functions);
    vm.execute(closure, &interp.ctx).unwrap_or_else(|e| {
        panic!("VM execution of deserialized program failed for `{input}`: {e}")
    })
}

/// Assert that the round-trip result matches the tree-walker result.
fn assert_roundtrip(input: &str) {
    let tw_result = common::eval_tw(input);
    let rt_result = eval_roundtrip(input);
    assert_eq!(
        rt_result, tw_result,
        "roundtrip mismatch for `{input}`:\n  roundtrip: {rt_result:?}\n  tree-walk: {tw_result:?}"
    );
}

fn assert_roundtrip_eq(input: &str, expected: Value) {
    let rt_result = eval_roundtrip(input);
    assert_eq!(
        rt_result, expected,
        "roundtrip mismatch for `{input}`:\n  roundtrip: {rt_result:?}\n  expected:  {expected:?}"
    );
}

// ============================================================
// 1. Closure with upvalue mutation
// ============================================================

#[test]
fn roundtrip_closure_upvalue_mutation() {
    assert_roundtrip_eq(
        "(begin (define (make-counter) (let ((n 0)) (fn () (set! n (+ n 1)) n))) (define c (make-counter)) (c) (c) (c))",
        Value::int(3),
    );
}

// ============================================================
// 2. Nested closures
// ============================================================

#[test]
fn roundtrip_nested_closures() {
    assert_roundtrip("(begin (define (f x) (fn (y) (fn (z) (list x y z)))) (((f 1) 2) 3))");
}

// ============================================================
// 3. Try/catch across function boundary
// ============================================================

#[test]
fn roundtrip_try_catch_across_function() {
    assert_roundtrip_eq(
        r#"(begin (define (g) (throw {:msg "x"})) (try (g) (catch e (get (get e :value) :msg))))"#,
        Value::string("x"),
    );
}

// ============================================================
// 4. Match with quoted literal
// ============================================================

#[test]
fn roundtrip_match_quoted_literal() {
    assert_roundtrip_eq("(match 'a ('a 1) (_ 0))", Value::int(1));
}

// ============================================================
// 5. Upvalue + do loop + closure calls
// ============================================================

#[test]
fn roundtrip_upvalue_do_loop() {
    assert_roundtrip_eq(
        "(begin (define (mk) (let ((x 0)) (fn () (set! x (+ x 1)) x))) (define c (mk)) (do ((i 0 (+ i 1)) (acc 0 (+ acc (c)))) ((= i 3) acc)))",
        Value::int(6),
    );
}

// ============================================================
// 6. String constant reuse (string table)
// ============================================================

#[test]
fn roundtrip_string_constant_reuse() {
    assert_roundtrip(
        r#"(list (string-length "hello") (string-length "hello") (string-append "hello" " " "world"))"#,
    );
}

// ============================================================
// 7. Map constants
// ============================================================

#[test]
fn roundtrip_map_constants() {
    assert_roundtrip_eq("(get {:a 1 :b 2 :c 3} :b)", Value::int(2));
}

// ============================================================
// 8. Deeply nested quoted structure
// ============================================================

#[test]
fn roundtrip_deeply_nested_quoted() {
    assert_roundtrip("(car (cdr '(1 (2 (3 (4))))))");
}

// ============================================================
// 9. Multiple functions with shared names (constant pool)
// ============================================================

#[test]
fn roundtrip_multiple_functions_shared_names() {
    assert_roundtrip_eq(
        "(begin (define (add a b) (+ a b)) (define (mul a b) (* a b)) (+ (add 3 4) (mul 5 6)))",
        Value::int(37),
    );
}

// ============================================================
// 10. Recursive function (self-reference serialization)
// ============================================================

#[test]
fn roundtrip_recursive_function() {
    assert_roundtrip_eq(
        "(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 10))",
        Value::int(3628800),
    );
}

// ============================================================
// 11. Letrec mutual recursion
// ============================================================

#[test]
fn roundtrip_letrec_mutual_recursion() {
    assert_roundtrip_eq(
        "(letrec ((even? (fn (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (fn (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))",
        Value::bool(true),
    );
}

// ============================================================
// 12. Vector and list constants
// ============================================================

#[test]
fn roundtrip_vector_and_list_constants() {
    assert_roundtrip_eq(
        "(begin (define v [1 2 3]) (define l '(4 5 6)) (+ (nth v 1) (nth l 1)))",
        Value::int(7),
    );
}
