mod common;

use sema_core::Value;

// ============================================================
// Arithmetic & Math
// ============================================================

dual_eval_tests! {
    arith_add: "(+ 1 2)" => Value::int(3),
    arith_sub: "(- 10 3)" => Value::int(7),
    arith_mul: "(* 4 5)" => Value::int(20),
    arith_div: "(/ 10 2)" => Value::int(5),
    arith_mod: "(mod 10 3)" => Value::int(1),
    arith_mixed_float: "(+ 1 2.0)" => Value::float(3.0),
    arith_identity_add: "(+)" => Value::int(0),
    arith_identity_mul: "(*)" => Value::int(1),
    unary_minus: "(- 5)" => Value::int(-5),
    negative: "(+ -3 -7)" => Value::int(-10),
    mixed_int_float: "(* 2 3.5)" => Value::float(7.0),
    pow_basic: "(pow 2 10)" => Value::int(1024),
    sqrt_basic: "(sqrt 16)" => Value::float(4.0),
    abs_basic: "(abs -5)" => Value::int(5),
    min_basic: "(min 3 1 2)" => Value::int(1),
    max_basic: "(max 3 1 2)" => Value::int(3),
    floor_basic: "(floor 3.7)" => Value::int(3),
    ceil_basic: "(ceil 3.2)" => Value::int(4),
    round_basic: "(round 3.5)" => Value::int(4),
    pi_const: "(> pi 3.14)" => Value::bool(true),
    e_const: "(> e 2.71)" => Value::bool(true),
    math_clamp: "(math/clamp 15 0 10)" => Value::int(10),
    math_sign_pos: "(math/sign 42)" => Value::int(1),
    math_sign_neg: "(math/sign -3)" => Value::int(-1),
    math_sign_zero: "(math/sign 0)" => Value::int(0),
    trig_sin: "(> (sin 0.0) -0.001)" => Value::bool(true),
}

// ============================================================
// Comparison & Logic
// ============================================================

dual_eval_tests! {
    cmp_lt: "(< 1 2)" => Value::bool(true),
    cmp_gt: "(> 3 2)" => Value::bool(true),
    cmp_lte: "(<= 2 2)" => Value::bool(true),
    cmp_eq: "(= 42 42)" => Value::bool(true),
    cmp_not: "(not #f)" => Value::bool(true),
    chained_cmp: "(< 1 2 3 4)" => Value::bool(true),
    chained_cmp_fail: "(< 1 2 2 3)" => Value::bool(false),
    truthiness_0: "(if 0 :truthy :falsy)" => Value::keyword("truthy"),
    truthiness_empty: r#"(if "" :truthy :falsy)"# => Value::keyword("truthy"),
    truthiness_nil: "(if nil :truthy :falsy)" => Value::keyword("falsy"),
    truthiness_false: "(if #f :truthy :falsy)" => Value::keyword("falsy"),
}

// ============================================================
// Core Forms (define, let, begin, set!, lambda, closures)
// ============================================================

dual_eval_tests! {
    define_var: "(begin (define x 42) x)" => Value::int(42),
    define_fn: "(begin (define (square x) (* x x)) (square 5))" => Value::int(25),
    defun_alias: "(begin (defun square (x) (* x x)) (square 5))" => Value::int(25),
    fn_alias: "((fn (x) (* x x)) 4)" => Value::int(16),
    lambda_basic: "((lambda (x y) (+ x y)) 3 4)" => Value::int(7),
    lambda_multi_body: "((lambda (x) (define y 2) (* x y)) 5)" => Value::int(10),
    let_basic: "(let ((x 10) (y 20)) (+ x y))" => Value::int(30),
    let_empty: "(let () 42)" => Value::int(42),
    let_star: "(let* ((x 10) (y (* x 2))) (+ x y))" => Value::int(30),
    letrec_basic: "(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))) (even? 10))" => Value::bool(true),
    begin_basic: "(begin 1 2 3)" => Value::int(3),
    begin_empty: "(begin)" => Value::nil(),
    set_bang: "(begin (define x 1) (set! x 2) x)" => Value::int(2),
    closure_adder: "(begin (define (make-adder n) (lambda (x) (+ n x))) ((make-adder 5) 3))" => Value::int(8),
    closure_counter: "(begin (define (make-counter) (let ((n 0)) (lambda () (set! n (+ n 1)) n))) (define c (make-counter)) (c) (c) (c))" => Value::int(3),
    nested_closure: "(begin (define (f x) (lambda (y) (lambda (z) (+ x y z)))) (((f 1) 2) 3))" => Value::int(6),
    internal_define: "(begin (define (foo x) (define y 10) (+ x y)) (foo 5))" => Value::int(15),
    rest_params: "(begin (define (sum . args) (foldl + 0 args)) (sum 1 2 3 4 5))" => Value::int(15),
    higher_order: "(begin (define (compose f g) (lambda (x) (f (g x)))) ((compose (lambda (x) (* x 2)) (lambda (x) (+ x 1))) 5))" => Value::int(12),
}

// ============================================================
// Control Flow (if, cond, case, when, unless, and, or, do)
// ============================================================

dual_eval_tests! {
    if_true: "(if #t 1 2)" => Value::int(1),
    if_false: "(if #f 1 2)" => Value::int(2),
    if_two_branch: "(if (> 3 2) :yes :no)" => Value::keyword("yes"),
    cond_basic: "(cond ((= 1 2) 10) ((= 1 1) 20) (else 30))" => Value::int(20),
    cond_no_match: "(cond ((= 1 2) 10) ((= 1 3) 20))" => Value::nil(),
    case_basic: "(case 2 ((1) :one) ((2) :two) (else :other))" => Value::keyword("two"),
    case_else: "(case 99 ((1) :one) (else :other))" => Value::keyword("other"),
    and_all_true: "(and 1 2 3)" => Value::int(3),
    and_short_circuit: "(and 1 #f 3)" => Value::bool(false),
    and_empty: "(and)" => Value::bool(true),
    or_found: "(or #f #f 3)" => Value::int(3),
    or_first: "(or 1 2 3)" => Value::int(1),
    or_empty: "(or)" => Value::bool(false),
    when_true: "(when #t 42)" => Value::int(42),
    when_false: "(when #f 42)" => Value::nil(),
    unless_false: "(unless #f 42)" => Value::int(42),
    unless_true: "(unless #t 42)" => Value::nil(),
    do_loop_sum: "(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum))" => Value::int(10),
    do_loop_factorial: "(do ((n 5 (- n 1)) (acc 1 (* acc n))) ((= n 0) acc))" => Value::int(120),
    named_let_sum: "(let loop ((n 10) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n))))" => Value::int(55),
    named_let_fib: "(let fib ((n 10)) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2))))))" => Value::int(55),
}

// ============================================================
// Quote, Quasiquote, Eval
// ============================================================

dual_eval_tests! {
    quote_list: "(car '(a b c))" => common::eval_tw("'a"),
    quasiquote_basic: "(begin (define x 42) (car (cdr `(a ,x b))))" => Value::int(42),
    unquote_splicing: "(begin (define xs '(2 3)) `(1 ,@xs 4))" => common::eval_tw("'(1 2 3 4)"),
    eval_basic: "(eval '(+ 1 2))" => Value::int(3),
    read_parse: r#"(eval (read "(+ 10 20)"))"# => Value::int(30),
    macroexpand_basic: "(begin (defmacro my-if (c t e) (list 'if c t e)) (macroexpand '(my-if #t 1 2)))" => common::eval_tw("'(if #t 1 2)"),
    defmacro_basic: "(begin (defmacro my-if (c t e) (list 'if c t e)) (my-if #t 1 2))" => Value::int(1),
    gensym_symbol: "(symbol? (gensym))" => Value::bool(true),
}

// ============================================================
// Error Handling (try/catch/throw)
// ============================================================

dual_eval_tests! {
    try_no_error: "(try 42 (catch e 0))" => Value::int(42),
    try_catch_error: r#"(try (error "boom") (catch e 99))"# => Value::int(99),
    try_catch_division: "(try (/ 1 0) (catch e :caught))" => Value::keyword("caught"),
    throw_catch_value: r#"(try (throw "oops") (catch e (:value e)))"# => Value::string("oops"),
    nested_try: r#"(try (try (error "inner") (catch e (error "outer"))) (catch e2 :recovered))"# => Value::keyword("recovered"),
}

// ============================================================
// TCO
// ============================================================

dual_eval_tests! {
    tco_deep: "(begin (define (count-down n) (if (= n 0) :done (count-down (- n 1)))) (count-down 100000))" => Value::keyword("done"),
    tco_mutual: "(begin (define (even? n) (if (= n 0) #t (odd? (- n 1)))) (define (odd? n) (if (= n 0) #f (even? (- n 1)))) (even? 1000))" => Value::bool(true),
    factorial_10: "(begin (define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1))))) (factorial 10))" => Value::int(3628800),
}

// ============================================================
// Scheme Aliases & Misc
// ============================================================

dual_eval_tests! {
    cons_basic: "(cons 1 '(2 3))" => common::eval_tw("'(1 2 3)"),
    car_cdr: "(car (cdr '(1 2 3)))" => Value::int(2),
    cadr: "(cadr '(1 2 3))" => Value::int(2),
    eq_identity: "(eq? 42 42)" => Value::bool(true),
    equal: "(equal? '(1 2) '(1 2))" => Value::bool(true),
    scheme_null: "(null? '())" => Value::bool(true),
    scheme_pair: "(pair? '(1 2))" => Value::bool(true),
}

// ============================================================
// Error tests
// ============================================================

dual_eval_error_tests! {
    err_division_by_zero: "(/ 1 0)",
    err_unbound_var: "undefined-variable",
}
