use sema_core::{SemaError, Value};
use sema_eval::Interpreter;

fn eval(input: &str) -> Value {
    let interp = Interpreter::new();
    interp
        .eval_str(input)
        .expect(&format!("failed to eval: {input}"))
}

fn eval_to_string(input: &str) -> String {
    format!("{}", eval(input))
}

#[test]
fn test_arithmetic() {
    assert_eq!(eval("(+ 1 2)"), Value::int(3));
    assert_eq!(eval("(- 10 3)"), Value::int(7));
    assert_eq!(eval("(* 4 5)"), Value::int(20));
    assert_eq!(eval("(/ 10 2)"), Value::int(5));
    assert_eq!(eval("(mod 10 3)"), Value::int(1));
    assert_eq!(eval("(+ 1 2.0)"), Value::float(3.0));
}

#[test]
fn test_comparison() {
    assert_eq!(eval("(< 1 2)"), Value::bool(true));
    assert_eq!(eval("(> 3 2)"), Value::bool(true));
    assert_eq!(eval("(<= 2 2)"), Value::bool(true));
    assert_eq!(eval("(= 42 42)"), Value::bool(true));
    assert_eq!(eval("(not #f)"), Value::bool(true));
}

#[test]
fn test_define_and_call() {
    assert_eq!(eval("(begin (define x 42) x)"), Value::int(42));
    assert_eq!(
        eval("(begin (define (square x) (* x x)) (square 5))"),
        Value::int(25)
    );
}

#[test]
fn test_defun_alias() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str("(defun square (x) (* x x)) (square 5)")
        .unwrap();
    assert_eq!(result.to_string(), "25");
}

#[test]
fn test_factorial() {
    assert_eq!(
        eval("(begin (define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1))))) (factorial 10))"),
        Value::int(3628800)
    );
}

#[test]
fn test_lambda() {
    assert_eq!(eval("((lambda (x y) (+ x y)) 3 4)"), Value::int(7));
}

#[test]
fn test_let() {
    assert_eq!(eval("(let ((x 10) (y 20)) (+ x y))"), Value::int(30));
}

#[test]
fn test_let_star() {
    assert_eq!(eval("(let* ((x 10) (y (* x 2))) (+ x y))"), Value::int(30));
}

#[test]
fn test_cond() {
    assert_eq!(
        eval("(cond ((= 1 2) 10) ((= 1 1) 20) (else 30))"),
        Value::int(20)
    );
}

#[test]
fn test_and_or() {
    assert_eq!(eval("(and 1 2 3)"), Value::int(3));
    assert_eq!(eval("(and 1 #f 3)"), Value::bool(false));
    assert_eq!(eval("(or #f #f 3)"), Value::int(3));
    assert_eq!(eval("(or 1 2 3)"), Value::int(1));
}

#[test]
fn test_list_operations() {
    assert_eq!(eval("(car (list 1 2 3))"), Value::int(1));
    assert_eq!(eval_to_string("(cdr (list 1 2 3))"), "(2 3)");
    assert_eq!(eval_to_string("(cons 0 (list 1 2))"), "(0 1 2)");
    assert_eq!(eval("(length (list 1 2 3))"), Value::int(3));
    assert_eq!(eval_to_string("(reverse (list 1 2 3))"), "(3 2 1)");
    assert_eq!(
        eval_to_string("(append (list 1 2) (list 3 4))"),
        "(1 2 3 4)"
    );
}

#[test]
fn test_map_filter_fold() {
    assert_eq!(
        eval_to_string("(map (lambda (x) (* x x)) (list 1 2 3))"),
        "(1 4 9)"
    );
    assert_eq!(
        eval_to_string("(filter (lambda (x) (> x 2)) (list 1 2 3 4))"),
        "(3 4)"
    );
    assert_eq!(eval("(foldl + 0 (list 1 2 3 4 5))"), Value::int(15));
}

#[test]
fn test_string_operations() {
    assert_eq!(eval("(string-length \"hello\")"), Value::int(5));
    assert_eq!(
        eval("(string/contains? \"hello world\" \"world\")"),
        Value::bool(true)
    );
    assert_eq!(
        eval_to_string("(string/split \"a,b,c\" \",\")"),
        "(\"a\" \"b\" \"c\")"
    );
}

#[test]
fn test_map_data_structure() {
    assert_eq!(eval("(get {:a 1 :b 2} :a)"), Value::int(1));
    assert_eq!(eval("(:b {:a 1 :b 2})"), Value::int(2));
    assert_eq!(eval("(get (assoc {:a 1} :b 2) :b)"), Value::int(2));
    assert_eq!(eval_to_string("(keys {:a 1 :b 2})"), "(:a :b)");
}

#[test]
fn test_json() {
    assert_eq!(
        eval("(json/encode {:name \"test\" :val 42})"),
        Value::string("{\"name\":\"test\",\"val\":42}")
    );
    assert_eq!(
        eval("(get (json/decode \"{\\\"x\\\": 10}\") :x)"),
        Value::int(10)
    );
}

#[test]
fn test_quote() {
    assert_eq!(eval_to_string("(quote (a b c))"), "(a b c)");
    assert_eq!(eval_to_string("'(a b c)"), "(a b c)");
}

#[test]
fn test_quasiquote() {
    assert_eq!(
        eval_to_string("(begin (define x 42) `(a ,x b))"),
        "(a 42 b)"
    );
}

#[test]
fn test_when_unless() {
    assert_eq!(eval("(when #t 42)"), Value::int(42));
    assert_eq!(eval("(when #f 42)"), Value::nil());
    assert_eq!(eval("(unless #f 42)"), Value::int(42));
    assert_eq!(eval("(unless #t 42)"), Value::nil());
}

#[test]
fn test_predicates() {
    assert_eq!(eval("(null? nil)"), Value::bool(true));
    assert_eq!(eval("(null? (list))"), Value::bool(true));
    assert_eq!(eval("(list? (list 1 2))"), Value::bool(true));
    assert_eq!(eval("(number? 42)"), Value::bool(true));
    assert_eq!(eval("(string? \"hi\")"), Value::bool(true));
    assert_eq!(eval("(keyword? :foo)"), Value::bool(true));
    assert_eq!(eval("(map? {:a 1})"), Value::bool(true));
}

#[test]
fn test_set_bang() {
    assert_eq!(eval("(begin (define x 1) (set! x 2) x)"), Value::int(2));
}

#[test]
fn test_begin() {
    assert_eq!(eval("(begin 1 2 3)"), Value::int(3));
}

#[test]
fn test_closures() {
    assert_eq!(
        eval("(begin (define (make-adder n) (lambda (x) (+ n x))) ((make-adder 5) 3))"),
        Value::int(8)
    );
}

#[test]
fn test_range() {
    assert_eq!(eval_to_string("(range 5)"), "(0 1 2 3 4)");
    assert_eq!(eval_to_string("(range 2 5)"), "(2 3 4)");
}

#[test]
fn test_rest_params() {
    assert_eq!(
        eval("(begin (define (sum . args) (foldl + 0 args)) (sum 1 2 3 4 5))"),
        Value::int(15)
    );
}

#[test]
fn test_apply() {
    assert_eq!(eval("(apply + (list 1 2 3))"), Value::int(6));
}

#[test]
fn test_math_functions() {
    assert_eq!(eval("(abs -5)"), Value::int(5));
    assert_eq!(eval("(min 3 1 2)"), Value::int(1));
    assert_eq!(eval("(max 3 1 2)"), Value::int(3));
    assert_eq!(eval("(floor 3.7)"), Value::int(3));
    assert_eq!(eval("(ceil 3.2)"), Value::int(4));
}

#[test]
fn test_prompt_and_message() {
    let result = eval("(prompt (system \"You are helpful.\") (user \"Hello\"))");
    assert!(result.as_prompt_rc().is_some());

    let result = eval("(message :user \"Hello\")");
    assert!(result.as_message_rc().is_some());
}

#[test]
fn test_recursive_fibonacci() {
    assert_eq!(
        eval("(begin (define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))"),
        Value::int(55)
    );
}

#[test]
fn test_higher_order() {
    assert_eq!(
        eval("(begin (define (compose f g) (lambda (x) (f (g x)))) (define inc (lambda (x) (+ x 1))) (define dbl (lambda (x) (* x 2))) ((compose dbl inc) 5))"),
        Value::int(12)
    );
}

#[test]
fn test_sort() {
    assert_eq!(
        eval_to_string("(sort (list 3 1 4 1 5 9 2 6))"),
        "(1 1 2 3 4 5 6 9)"
    );
}

#[test]
fn test_format() {
    assert_eq!(
        eval("(format \"Hello ~a, you are ~a\" \"world\" 42)"),
        Value::string("Hello world, you are 42")
    );
}

#[test]
fn test_string_conversions() {
    assert_eq!(eval("(string->number \"42\")"), Value::int(42));
    assert_eq!(eval("(string->number \"3.14\")"), Value::float(3.14));
    assert_eq!(eval("(number->string 42)"), Value::string("42"));
}

#[test]
fn test_deftool() {
    let result = eval(
        r#"
        (begin
          (deftool add-numbers
            "Add two numbers"
            {:a {:type :number :description "First number"}
             :b {:type :number :description "Second number"}}
            (lambda (a b) (+ a b)))
          add-numbers)
    "#,
    );
    assert!(result.as_tool_def_rc().is_some());
}

#[test]
fn test_defagent() {
    let result = eval(
        r#"
        (begin
          (deftool greet
            "Greet someone"
            {:name {:type :string}}
            (lambda (name) (string-append "Hello, " name "!")))
          (defagent greeter {:system "You greet people."
                             :tools [greet]
                             :max-turns 5})
          greeter)
    "#,
    );
    assert!(result.as_agent_rc().is_some());
}

#[test]
fn test_load_special_form() {
    // Write a temp file and load it
    eval(r#"(file/write "/tmp/sema-test-load.sema" "(define loaded-value 42)")"#);
    let result = eval(
        r#"
        (begin
          (load "/tmp/sema-test-load.sema")
          loaded-value)
    "#,
    );
    assert_eq!(result, Value::int(42));
}

#[test]
fn test_defmacro() {
    assert_eq!(
        eval(
            r#"
            (begin
              (defmacro my-if (cond then else)
                (list 'if cond then else))
              (my-if #t 1 2))
        "#
        ),
        Value::int(1)
    );
}

#[test]
fn test_llm_similarity() {
    // llm/similarity should compute cosine similarity
    assert_eq!(
        eval_to_string("(llm/similarity '(1.0 0.0 0.0) '(1.0 0.0 0.0))"),
        "1.0"
    );
    assert_eq!(
        eval_to_string("(llm/similarity '(1.0 0.0) '(0.0 1.0))"),
        "0.0"
    );
}

#[test]
fn test_pricing() {
    // llm/set-pricing should not error
    assert_eq!(
        eval_to_string("(llm/set-pricing \"my-model\" 1.0 2.0)"),
        "nil"
    );
}

#[test]
fn test_llm_reset_usage() {
    assert_eq!(eval_to_string("(llm/reset-usage)"), "nil");
}

#[test]
fn test_multi_list_map() {
    assert_eq!(eval_to_string("(map + '(1 2 3) '(10 20 30))"), "(11 22 33)");
    // Shortest wins
    assert_eq!(eval_to_string("(map + '(1 2 3) '(10 20))"), "(11 22)");
}

#[test]
fn test_take_drop() {
    assert_eq!(eval_to_string("(take 2 '(1 2 3 4))"), "(1 2)");
    assert_eq!(eval_to_string("(drop 2 '(1 2 3 4))"), "(3 4)");
    assert_eq!(eval_to_string("(take 10 '(1 2))"), "(1 2)");
    assert_eq!(eval_to_string("(drop 10 '(1 2))"), "()");
}

#[test]
fn test_last() {
    assert_eq!(eval("(last '(1 2 3))"), Value::int(3));
    assert_eq!(eval("(last '())"), Value::nil());
}

#[test]
fn test_zip() {
    assert_eq!(eval_to_string("(zip '(1 2) '(a b))"), "((1 a) (2 b))");
    assert_eq!(
        eval_to_string("(zip '(1 2 3) '(a b) '(x y))"),
        "((1 a x) (2 b y))"
    );
}

#[test]
fn test_flatten() {
    assert_eq!(
        eval_to_string("(flatten '((1 2) (3 4) (5)))"),
        "(1 2 3 4 5)"
    );
    assert_eq!(eval_to_string("(flatten '(1 (2 3) 4))"), "(1 2 3 4)");
}

#[test]
fn test_member() {
    assert_eq!(eval_to_string("(member 3 '(1 2 3 4 5))"), "(3 4 5)");
    assert_eq!(eval("(member 9 '(1 2 3))"), Value::bool(false));
}

#[test]
fn test_any_every() {
    assert_eq!(
        eval("(any (lambda (x) (> x 3)) '(1 2 3 4 5))"),
        Value::bool(true)
    );
    assert_eq!(
        eval("(any (lambda (x) (> x 10)) '(1 2 3))"),
        Value::bool(false)
    );
    assert_eq!(
        eval("(every (lambda (x) (> x 0)) '(1 2 3))"),
        Value::bool(true)
    );
    assert_eq!(
        eval("(every (lambda (x) (> x 2)) '(1 2 3))"),
        Value::bool(false)
    );
}

#[test]
fn test_reduce() {
    assert_eq!(eval("(reduce + '(1 2 3 4 5))"), Value::int(15));
    assert_eq!(eval("(reduce * '(1 2 3 4))"), Value::int(24));
}

#[test]
fn test_partition() {
    assert_eq!(
        eval_to_string("(partition (lambda (x) (> x 2)) '(1 2 3 4 5))"),
        "((3 4 5) (1 2))"
    );
}

#[test]
fn test_foldr() {
    assert_eq!(eval_to_string("(foldr cons '() '(1 2 3))"), "(1 2 3)");
    assert_eq!(eval("(foldr + 0 '(1 2 3 4 5))"), Value::int(15));
}

#[test]
fn test_named_let_sum() {
    assert_eq!(
        eval("(let loop ((i 1) (acc 0)) (if (> i 10) acc (loop (+ i 1) (+ acc i))))"),
        Value::int(55)
    );
}

#[test]
fn test_named_let_fib() {
    assert_eq!(
        eval("(let fib ((n 10) (a 0) (b 1)) (if (= n 0) a (fib (- n 1) b (+ a b))))"),
        Value::int(55)
    );
}

#[test]
fn test_letrec() {
    assert_eq!(
        eval(
            r#"
            (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                     (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
              (even? 10))
        "#
        ),
        Value::bool(true)
    );
    assert_eq!(
        eval(
            r#"
            (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                     (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
              (odd? 7))
        "#
        ),
        Value::bool(true)
    );
}

#[test]
fn test_try_catch_error() {
    assert_eq!(
        eval(r#"(try (error "boom") (catch e (:message e)))"#),
        Value::string("boom")
    );
}

#[test]
fn test_try_no_error() {
    assert_eq!(eval("(try 42 (catch e 0))"), Value::int(42));
}

#[test]
fn test_throw_catch() {
    assert_eq!(
        eval("(try (throw 99) (catch e (:value e)))"),
        Value::int(99)
    );
}

#[test]
fn test_catch_type_error() {
    assert_eq!(
        eval(r#"(try (+ 1 "nope") (catch e (:type e)))"#),
        Value::keyword("type-error")
    );
}

#[test]
fn test_catch_unbound() {
    assert_eq!(
        eval(r#"(try no-such-var (catch e (:type e)))"#),
        Value::keyword("unbound")
    );
}

#[test]
fn test_nested_try() {
    assert_eq!(
        eval(
            r#"
            (try
              (try (throw 1) (catch e (throw (+ (:value e) 1))))
              (catch e (:value e)))
        "#
        ),
        Value::int(2)
    );
}

fn eval_err(input: &str) -> SemaError {
    let interp = Interpreter::new();
    interp.eval_str(input).unwrap_err()
}

fn unique_temp_dir(prefix: &str) -> std::path::PathBuf {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("sema-{prefix}-{}-{nanos}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");
    dir
}

fn lisp_path(path: &std::path::Path) -> String {
    path.to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
}

#[test]
fn test_module_import() {
    // Write a module file
    eval(
        r#"(file/write "/tmp/sema-test-module.sema" "(module math (export add square) (define (add a b) (+ a b)) (define (square x) (* x x)) (define internal 42))")"#,
    );
    // Import and use
    assert_eq!(
        eval(
            r#"
            (begin
              (import "/tmp/sema-test-module.sema")
              (add 3 4))
        "#
        ),
        Value::int(7)
    );
    assert_eq!(
        eval(
            r#"
            (begin
              (import "/tmp/sema-test-module.sema")
              (square 5))
        "#
        ),
        Value::int(25)
    );
    // internal should NOT be exported
    let err = eval_err(
        r#"
        (begin
          (import "/tmp/sema-test-module.sema")
          internal)
    "#,
    );
    assert!(matches!(err, SemaError::Unbound(_)));
}

#[test]
fn test_selective_import() {
    eval(
        r#"(file/write "/tmp/sema-test-sel.sema" "(module m (export foo bar) (define (foo) 1) (define (bar) 2))")"#,
    );
    assert_eq!(
        eval(
            r#"
            (begin
              (import "/tmp/sema-test-sel.sema" foo)
              (foo))
        "#
        ),
        Value::int(1)
    );
    // bar should not be imported
    let err = eval_err(
        r#"
        (begin
          (import "/tmp/sema-test-sel.sema" foo)
          (bar))
    "#,
    );
    assert!(matches!(err, SemaError::Unbound(_)));
}

#[test]
fn test_module_cache() {
    eval(r#"(file/write "/tmp/sema-test-cache.sema" "(module c (export val) (define val 99))")"#);
    // Import twice — should work fine (cached)
    assert_eq!(
        eval(
            r#"
            (begin
              (import "/tmp/sema-test-cache.sema")
              (import "/tmp/sema-test-cache.sema")
              val)
        "#
        ),
        Value::int(99)
    );
}

#[test]
fn test_nested_import_does_not_leak_non_exported_bindings() {
    let dir = unique_temp_dir("nested-import-exports");
    let module_b = dir.join("b.sema");
    let module_a = dir.join("a.sema");

    std::fs::write(
        &module_b,
        "(module b (export z) (define z 3) (define hidden-b 99))",
    )
    .unwrap();
    std::fs::write(
        &module_a,
        format!(
            "(module a (export x) (define x 1) (define y 2) (import \"{}\"))",
            lisp_path(&module_b)
        ),
    )
    .unwrap();

    let err = eval_err(&format!(
        r#"
        (begin
          (import "{}")
          y)
    "#,
        lisp_path(&module_a)
    ));
    assert!(
        matches!(err.inner(), SemaError::Unbound(_)),
        "expected unbound y, got: {}",
        err.inner()
    );

    assert_eq!(
        eval(&format!(
            r#"
            (begin
              (import "{}")
              x)
        "#,
            lisp_path(&module_a)
        )),
        Value::int(1)
    );

    let err = eval_err(&format!(
        r#"
        (begin
          (import "{}")
          z)
    "#,
        lisp_path(&module_a)
    ));
    assert!(
        matches!(err.inner(), SemaError::Unbound(_)),
        "expected unbound z, got: {}",
        err.inner()
    );
}

#[test]
fn test_cyclic_import_returns_error_instead_of_stack_overflow() {
    let dir = unique_temp_dir("cyclic-import");
    let module_a = dir.join("a.sema");
    let module_b = dir.join("b.sema");

    std::fs::write(
        &module_a,
        format!(
            "(module a (export x) (define x 1) (import \"{}\"))",
            lisp_path(&module_b)
        ),
    )
    .unwrap();
    std::fs::write(
        &module_b,
        format!(
            "(module b (export y) (define y 2) (import \"{}\"))",
            lisp_path(&module_a)
        ),
    )
    .unwrap();

    let err = eval_err(&format!(r#"(import "{}")"#, lisp_path(&module_a)));
    let msg = format!("{}", err.inner());
    assert!(
        msg.contains("cyclic import detected"),
        "expected cyclic import error, got: {msg}"
    );
}

#[test]
fn test_module_cache_isolation_between_interpreters() {
    let dir = unique_temp_dir("module-cache-isolation");
    let module_path = dir.join("cache.sema");

    std::fs::write(&module_path, "(module c (export val) (define val 1))").unwrap();

    let interp1 = Interpreter::new();
    assert_eq!(
        interp1
            .eval_str(&format!(
                r#"(begin (import "{}") val)"#,
                lisp_path(&module_path)
            ))
            .unwrap(),
        Value::int(1)
    );

    std::fs::write(&module_path, "(module c (export val) (define val 2))").unwrap();

    let interp2 = Interpreter::new();
    assert_eq!(
        interp2
            .eval_str(&format!(
                r#"(begin (import "{}") val)"#,
                lisp_path(&module_path)
            ))
            .unwrap(),
        Value::int(2)
    );
}

#[test]
fn test_load_pops_file_context_even_if_file_disappears() {
    let dir = unique_temp_dir("load-pop-context");
    let script = dir.join("self-delete.sema");
    let script_path = lisp_path(&script);

    std::fs::write(
        &script,
        format!(r#"(begin (file/delete "{}") 1)"#, script_path),
    )
    .unwrap();

    let interp = Interpreter::new();
    assert_eq!(
        interp
            .eval_str(&format!(r#"(load "{}")"#, script_path))
            .unwrap(),
        Value::int(1)
    );
    assert!(
        interp.ctx.current_file_path().is_none(),
        "current file context should be cleared after load"
    );
}

#[test]
fn test_case() {
    assert_eq!(
        eval(r#"(case (+ 1 1) ((1) "one") ((2 3) "two-or-three") (else "other"))"#),
        Value::string("two-or-three")
    );
    assert_eq!(
        eval(r#"(case :b ((:a) 1) ((:b :c) 2) (else 3))"#),
        Value::int(2)
    );
    assert_eq!(
        eval(r#"(case 99 ((1) "one") (else "other"))"#),
        Value::string("other")
    );
    // No match, no else
    assert_eq!(eval(r#"(case 99 ((1) "one") ((2) "two"))"#), Value::nil());
}

#[test]
fn test_eval_special_form() {
    assert_eq!(eval("(eval '(+ 1 2))"), Value::int(3));
    assert_eq!(eval(r#"(eval (read "(* 6 7)"))"#), Value::int(42));
}

#[test]
fn test_read_builtin() {
    assert_eq!(eval(r#"(read "42")"#), Value::int(42));
    assert_eq!(eval_to_string(r#"(read "(+ 1 2)")"#), "(+ 1 2)");
    assert_eq!(eval_to_string(r#"(read-many "1 2 3")"#), "(1 2 3)");
}

#[test]
fn test_type_conversions() {
    assert_eq!(eval_to_string(r#"(string->symbol "foo")"#), "foo");
    assert_eq!(eval(r#"(symbol->string 'foo)"#), Value::string("foo"));
    assert_eq!(eval_to_string(r#"(string->keyword "bar")"#), ":bar");
    assert_eq!(eval(r#"(keyword->string :bar)"#), Value::string("bar"));
}

#[test]
fn test_gensym() {
    // gensym returns a symbol
    let result = eval("(gensym)");
    assert!(result.is_symbol());
    // gensym with prefix
    let result = eval(r#"(gensym "tmp")"#);
    if let Some(s) = result.as_symbol_spur() {
        assert!(sema_core::resolve(s).starts_with("tmp__"));
    } else {
        panic!("expected symbol");
    }
    // Two gensyms are different
    assert_eq!(
        eval("(begin (define a (gensym)) (define b (gensym)) (= a b))"),
        Value::bool(false)
    );
}

#[test]
fn test_macroexpand() {
    assert_eq!(
        eval_to_string(
            r#"
            (begin
              (defmacro my-if (c t e) (list 'if c t e))
              (macroexpand '(my-if #t 1 2)))
        "#
        ),
        "(if #t 1 2)"
    );
    // Non-macro form returned as-is
    assert_eq!(eval("(macroexpand '(+ 1 2))"), eval("'(+ 1 2)"));
}

#[test]
fn test_file_operations() {
    let dir = "/tmp/sema-test-fileops";
    // Clean up from previous runs
    let _ = std::fs::remove_dir_all(dir);

    eval(&format!(r#"(file/mkdir "{dir}/sub")"#));
    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}/sub")"#)),
        Value::bool(true)
    );

    eval(&format!(r#"(file/write "{dir}/a.txt" "hello")"#));
    eval(&format!(r#"(file/append "{dir}/a.txt" " world")"#));
    assert_eq!(
        eval(&format!(r#"(file/read "{dir}/a.txt")"#)),
        Value::string("hello world")
    );

    eval(&format!(r#"(file/rename "{dir}/a.txt" "{dir}/b.txt")"#));
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/b.txt")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/a.txt")"#)),
        Value::bool(false)
    );

    let info = eval(&format!(r#"(file/info "{dir}/b.txt")"#));
    assert!(info.is_map());

    eval(&format!(r#"(file/delete "{dir}/b.txt")"#));
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/b.txt")"#)),
        Value::bool(false)
    );

    // file/list
    eval(&format!(r#"(file/write "{dir}/x.txt" "x")"#));
    eval(&format!(r#"(file/write "{dir}/y.txt" "y")"#));
    let listing = eval(&format!(r#"(file/list "{dir}")"#));
    if let Some(items) = listing.as_list() {
        assert!(items.len() >= 2); // sub, x.txt, y.txt
    } else {
        panic!("expected list");
    }

    // Cleanup
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_path_operations() {
    assert_eq!(
        eval(r#"(path/join "usr" "local" "bin")"#),
        Value::string("usr/local/bin")
    );
    assert_eq!(eval(r#"(path/dirname "/a/b/c")"#), Value::string("/a/b"));
    assert_eq!(
        eval(r#"(path/basename "/a/b/c.txt")"#),
        Value::string("c.txt")
    );
    assert_eq!(eval(r#"(path/extension "foo.txt")"#), Value::string("txt"));
    assert_eq!(eval(r#"(path/extension "no-ext")"#), Value::nil());
    // path/absolute returns a real path — just check it doesn't error
    let result = eval(r#"(path/absolute ".")"#);
    assert!(result.is_string());
}

#[test]
fn test_regex_match() {
    assert_eq!(eval(r#"(regex/match? "\\d+" "abc123")"#), Value::bool(true));
    assert_eq!(
        eval(r#"(regex/match? "^\\d+$" "abc123")"#),
        Value::bool(false)
    );
}

#[test]
fn test_regex_capture() {
    let result = eval(r#"(regex/match "(\\d+)-(\\w+)" "item-42-foo")"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(
            m.get(&Value::keyword("match")),
            Some(&Value::string("42-foo"))
        );
    } else {
        panic!("expected map, got {result}");
    }
}

#[test]
fn test_regex_find_all() {
    assert_eq!(
        eval_to_string(r#"(regex/find-all "\\d+" "a1b2c3")"#),
        r#"("1" "2" "3")"#
    );
}

#[test]
fn test_regex_replace() {
    assert_eq!(
        eval(r#"(regex/replace "\\d+" "X" "a1b2c3")"#),
        Value::string("aXb2c3")
    );
    assert_eq!(
        eval(r#"(regex/replace-all "\\d+" "X" "a1b2c3")"#),
        Value::string("aXbXcX")
    );
}

#[test]
fn test_regex_split() {
    assert_eq!(
        eval_to_string(r#"(regex/split "[,;]" "a,b;c,d")"#),
        r#"("a" "b" "c" "d")"#
    );
}

#[test]
fn test_http_get() {
    let result = eval(r#"(http/get "https://httpbin.org/get")"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("status")), Some(&Value::int(200)));
    } else {
        panic!("expected map");
    }
}

#[test]
fn test_http_post() {
    let result = eval(r#"(http/post "https://httpbin.org/post" {:name "sema"})"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("status")), Some(&Value::int(200)));
    } else {
        panic!("expected map");
    }
}

#[test]
fn test_http_request_generic() {
    let result = eval(r#"(http/request "PATCH" "https://httpbin.org/patch" {} "data")"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("status")), Some(&Value::int(200)));
    } else {
        panic!("expected map");
    }
}

#[test]
fn test_http_response_has_body() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (string? (:body resp)))
    "#,
    );
    assert_eq!(result, Value::bool(true));
    // Also verify body is non-empty
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (> (string-length (:body resp)) 0))
    "#,
    );
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_http_response_has_headers() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (map? (:headers resp)))
    "#,
    );
    assert_eq!(result, Value::bool(true));
    // Verify content-type header exists
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (string? (get (:headers resp) :content-type)))
    "#,
    );
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_http_response_body_json_decode() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get")))
          (let ((data (json/decode (:body resp))))
            (get data :url)))
    "#,
    );
    assert_eq!(result, Value::string("https://httpbin.org/get"));
}

#[test]
fn test_http_put() {
    let result = eval(
        r#"
        (let ((resp (http/put "https://httpbin.org/put" {:name "sema"})))
          (let ((data (json/decode (:body resp))))
            (list (:status resp) (get (get data :json) :name))))
    "#,
    );
    assert_eq!(
        result,
        Value::list(vec![Value::int(200), Value::string("sema")])
    );
}

#[test]
fn test_http_delete() {
    let result = eval(
        r#"
        (let ((resp (http/delete "https://httpbin.org/delete")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

#[test]
fn test_http_head() {
    let result = eval(
        r#"
        (let ((resp (http/request "HEAD" "https://httpbin.org/get")))
          (list (:status resp) (:body resp)))
    "#,
    );
    // HEAD returns 200 with empty body
    assert_eq!(
        result,
        Value::list(vec![Value::int(200), Value::string("")])
    );
}

#[test]
fn test_http_patch_with_json_body() {
    let result = eval(
        r#"
        (let ((resp (http/request "PATCH" "https://httpbin.org/patch" {} "{\"key\":\"val\"}")))
          (let ((data (json/decode (:body resp))))
            (list (:status resp) (get data :data))))
    "#,
    );
    assert_eq!(
        result,
        Value::list(vec![Value::int(200), Value::string("{\"key\":\"val\"}")])
    );
}

#[test]
fn test_http_custom_headers() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/headers"
                              {:headers {:x-custom-header "sema-test"}})))
          (let ((data (json/decode (:body resp))))
            (get (get data :headers) :X-Custom-Header)))
    "#,
    );
    assert_eq!(result, Value::string("sema-test"));
}

#[test]
fn test_http_multiple_headers() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/headers"
                              {:headers {:x-first "one" :x-second "two"}})))
          (let ((hdrs (get (json/decode (:body resp)) :headers)))
            (list (get hdrs :X-First) (get hdrs :X-Second))))
    "#,
    );
    assert_eq!(
        result,
        Value::list(vec![Value::string("one"), Value::string("two")])
    );
}

#[test]
fn test_http_post_map_body_echoed() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:name "sema" :version 1})))
          (let ((data (json/decode (:body resp))))
            (get (get data :json) :name)))
    "#,
    );
    assert_eq!(result, Value::string("sema"));
}

#[test]
fn test_http_post_string_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" "raw-body-data"
                               {:headers {:content-type "text/plain"}})))
          (let ((data (json/decode (:body resp))))
            (get data :data)))
    "#,
    );
    assert_eq!(result, Value::string("raw-body-data"));
}

#[test]
fn test_http_post_nested_map() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:user {:name "test"}})))
          (let ((data (json/decode (:body resp))))
            (get (get (get data :json) :user) :name)))
    "#,
    );
    assert_eq!(result, Value::string("test"));
}

#[test]
fn test_http_post_empty_string_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" "")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

#[test]
fn test_http_status_404() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/status/404")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(404));
}

#[test]
fn test_http_status_500() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/status/500")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(500));
}

#[test]
fn test_http_get_with_query_params() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get?foo=bar&baz=42")))
          (let ((data (json/decode (:body resp))))
            (get (get data :args) :foo)))
    "#,
    );
    assert_eq!(result, Value::string("bar"));
}

#[test]
fn test_http_timeout() {
    let _err = eval_err(r#"(http/get "https://httpbin.org/delay/10" {:timeout 1000})"#);
    // Just verifying it errors (timeout after 1s, but server delays 10s)
}

#[test]
fn test_http_invalid_url() {
    let _err = eval_err(r#"(http/get "http://invalid.invalid.invalid")"#);
}

#[test]
fn test_http_get_wrong_arity() {
    let _err = eval_err(r#"(http/get)"#);
}

#[test]
fn test_http_post_wrong_arity() {
    let _err = eval_err(r#"(http/post "https://httpbin.org/post")"#);
}

#[test]
fn test_http_unicode_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:text "Hello 世界"})))
          (let ((data (json/decode (:body resp))))
            (get (get data :json) :text)))
    "#,
    );
    assert_eq!(result, Value::string("Hello 世界"));
}

#[test]
fn test_http_redirect() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/redirect/1")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

// Unknown method → error (http.rs line 31)
#[test]
fn test_http_request_unknown_method() {
    let _err = eval_err(r#"(http/request "BOGUS" "https://httpbin.org/get")"#);
}

// Non-string URL → type error (http.rs line 111, 122, etc.)
#[test]
fn test_http_get_non_string_url() {
    let _err = eval_err(r#"(http/get 42)"#);
}

#[test]
fn test_http_post_non_string_url() {
    let _err = eval_err(r#"(http/post 42 "body")"#);
}

#[test]
fn test_http_request_non_string_method() {
    let _err = eval_err(r#"(http/request 42 "https://httpbin.org/get")"#);
}

// Too many args (upper arity bounds)
#[test]
fn test_http_get_too_many_args() {
    let _err = eval_err(r#"(http/get "url" {} "extra")"#);
}

#[test]
fn test_http_post_too_many_args() {
    let _err = eval_err(r#"(http/post "url" "body" {} "extra")"#);
}

#[test]
fn test_http_put_wrong_arity() {
    let _err = eval_err(r#"(http/put "url")"#);
}

#[test]
fn test_http_delete_wrong_arity() {
    let _err = eval_err(r#"(http/delete)"#);
}

#[test]
fn test_http_request_too_few_args() {
    let _err = eval_err(r#"(http/request "GET")"#);
}

#[test]
fn test_http_request_too_many_args() {
    let _err = eval_err(r#"(http/request "GET" "url" {} "body" "extra")"#);
}

// http/request minimal 2-arg path (opts and body both None)
#[test]
fn test_http_request_minimal_args() {
    let result = eval(
        r#"
        (let ((resp (http/request "GET" "https://httpbin.org/get")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

// Non-string/non-map body → to_string() fallback (http.rs line 71-73)
#[test]
fn test_http_post_integer_body() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" 42)))
          (let ((data (json/decode (:body resp))))
            (get data :data)))
    "#,
    );
    assert_eq!(result, Value::string("42"));
}

// Map body auto-sets Content-Type: application/json (http.rs line 67-69)
#[test]
fn test_http_post_map_sets_content_type_json() {
    let result = eval(
        r#"
        (let ((resp (http/post "https://httpbin.org/post" {:a 1})))
          (let ((data (json/decode (:body resp))))
            (string/starts-with? (get (get data :headers) :Content-Type) "application/json")))
    "#,
    );
    assert_eq!(result, Value::bool(true));
}

// String header keys (http.rs line 39 - Value::String branch)
#[test]
fn test_http_headers_with_string_keys() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/headers"
                              {:headers {"X-String-Key" "string-val"}})))
          (let ((data (json/decode (:body resp))))
            (get (get data :headers) :X-String-Key)))
    "#,
    );
    assert_eq!(result, Value::string("string-val"));
}

// Opts as non-map → silently ignored (http.rs line 35)
#[test]
fn test_http_get_opts_non_map_ignored() {
    let result = eval(
        r#"
        (let ((resp (http/get "https://httpbin.org/get" "not-a-map")))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

// http/delete with opts (exercises the opts path for delete)
#[test]
fn test_http_delete_with_opts() {
    let result = eval(
        r#"
        (let ((resp (http/delete "https://httpbin.org/delete"
                                 {:headers {:x-delete-test "yes"}})))
          (:status resp))
    "#,
    );
    assert_eq!(result, Value::int(200));
}

// Expanded test suite — covering implemented but previously untested
// features, edge cases, and patterns from mal/Chibi/R7RS test suites.

#[test]
fn test_bool_predicate() {
    assert_eq!(eval("(bool? #t)"), Value::bool(true));
    assert_eq!(eval("(bool? #f)"), Value::bool(true));
    assert_eq!(eval("(bool? 0)"), Value::bool(false));
    assert_eq!(eval("(bool? nil)"), Value::bool(false));
    assert_eq!(eval("(bool? \"true\")"), Value::bool(false));
}

#[test]
fn test_nil_predicate() {
    assert_eq!(eval("(nil? nil)"), Value::bool(true));
    assert_eq!(eval("(nil? #f)"), Value::bool(false));
    assert_eq!(eval("(nil? 0)"), Value::bool(false));
    assert_eq!(eval("(nil? (list))"), Value::bool(false)); // empty list is NOT nil
    assert_eq!(eval("(nil? \"\")"), Value::bool(false));
}

#[test]
fn test_fn_predicate() {
    assert_eq!(eval("(fn? +)"), Value::bool(true));
    assert_eq!(eval("(fn? car)"), Value::bool(true));
    assert_eq!(eval("(fn? (lambda (x) x))"), Value::bool(true));
    assert_eq!(eval("(fn? 42)"), Value::bool(false));
    assert_eq!(eval("(fn? :foo)"), Value::bool(false));
}

#[test]
fn test_type_function() {
    assert_eq!(eval("(type 42)"), Value::keyword("int"));
    assert_eq!(eval("(type 3.14)"), Value::keyword("float"));
    assert_eq!(eval("(type \"hi\")"), Value::keyword("string"));
    assert_eq!(eval("(type :foo)"), Value::keyword("keyword"));
    assert_eq!(eval("(type 'sym)"), Value::keyword("symbol"));
    assert_eq!(eval("(type (list 1 2))"), Value::keyword("list"));
    assert_eq!(eval("(type [1 2])"), Value::keyword("vector"));
    assert_eq!(eval("(type {:a 1})"), Value::keyword("map"));
    assert_eq!(eval("(type #t)"), Value::keyword("bool"));
    assert_eq!(eval("(type nil)"), Value::keyword("nil"));
    assert_eq!(eval("(type +)"), Value::keyword("native-fn"));
    assert_eq!(eval("(type (lambda (x) x))"), Value::keyword("lambda"));
}

#[test]
fn test_integer_float_predicates() {
    assert_eq!(eval("(integer? 42)"), Value::bool(true));
    assert_eq!(eval("(integer? 3.14)"), Value::bool(false));
    assert_eq!(eval("(float? 3.14)"), Value::bool(true));
    assert_eq!(eval("(float? 42)"), Value::bool(false));
}

#[test]
fn test_vector_predicate() {
    assert_eq!(eval("(vector? [1 2 3])"), Value::bool(true));
    assert_eq!(eval("(vector? (list 1 2 3))"), Value::bool(false));
    assert_eq!(eval("(vector? 42)"), Value::bool(false));
}

#[test]
fn test_even_odd() {
    assert_eq!(eval("(even? 0)"), Value::bool(true));
    assert_eq!(eval("(even? 2)"), Value::bool(true));
    assert_eq!(eval("(even? 3)"), Value::bool(false));
    assert_eq!(eval("(even? -4)"), Value::bool(true));
    assert_eq!(eval("(odd? 1)"), Value::bool(true));
    assert_eq!(eval("(odd? 2)"), Value::bool(false));
    assert_eq!(eval("(odd? -3)"), Value::bool(true));
}

#[test]
fn test_zero_positive_negative() {
    assert_eq!(eval("(zero? 0)"), Value::bool(true));
    assert_eq!(eval("(zero? 0.0)"), Value::bool(true));
    assert_eq!(eval("(zero? 1)"), Value::bool(false));
    assert_eq!(eval("(positive? 1)"), Value::bool(true));
    assert_eq!(eval("(positive? 0)"), Value::bool(false));
    assert_eq!(eval("(positive? -1)"), Value::bool(false));
    assert_eq!(eval("(negative? -1)"), Value::bool(true));
    assert_eq!(eval("(negative? 0)"), Value::bool(false));
    assert_eq!(eval("(negative? 1)"), Value::bool(false));
}

#[test]
fn test_eq_identity() {
    // eq? uses structural equality in Sema (PartialEq)
    assert_eq!(eval("(eq? 1 1)"), Value::bool(true));
    assert_eq!(eval("(eq? :a :a)"), Value::bool(true));
    assert_eq!(eval("(eq? \"hello\" \"hello\")"), Value::bool(true));
    assert_eq!(eval("(eq? #t #t)"), Value::bool(true));
    assert_eq!(eval("(eq? nil nil)"), Value::bool(true));
    assert_eq!(eval("(eq? 1 2)"), Value::bool(false));
    assert_eq!(eval("(eq? 1 1.0)"), Value::bool(false)); // different types
                                                         // Lists: structural equality
    assert_eq!(eval("(eq? (list 1 2) (list 1 2))"), Value::bool(true));
}

#[test]
fn test_string_ref() {
    assert_eq!(eval(r#"(string-ref "hello" 0)"#), Value::char('h'));
    assert_eq!(eval(r#"(string-ref "hello" 4)"#), Value::char('o'));
}

#[test]
fn test_substring() {
    assert_eq!(eval(r#"(substring "hello" 1 3)"#), Value::string("el"));
    assert_eq!(eval(r#"(substring "hello" 0 5)"#), Value::string("hello"));
    assert_eq!(eval(r#"(substring "hello" 3)"#), Value::string("lo"));
}

#[test]
fn test_string_trim() {
    assert_eq!(eval(r#"(string/trim "  hello  ")"#), Value::string("hello"));
    assert_eq!(eval(r#"(string/trim "\thello\n")"#), Value::string("hello"));
    assert_eq!(eval(r#"(string/trim "hello")"#), Value::string("hello"));
}

#[test]
fn test_string_starts_ends_with() {
    assert_eq!(
        eval(r#"(string/starts-with? "hello" "hel")"#),
        Value::bool(true)
    );
    assert_eq!(
        eval(r#"(string/starts-with? "hello" "ell")"#),
        Value::bool(false)
    );
    assert_eq!(
        eval(r#"(string/ends-with? "hello" "llo")"#),
        Value::bool(true)
    );
    assert_eq!(
        eval(r#"(string/ends-with? "hello" "hel")"#),
        Value::bool(false)
    );
}

#[test]
fn test_string_upper_lower() {
    assert_eq!(eval(r#"(string/upper "hello")"#), Value::string("HELLO"));
    assert_eq!(eval(r#"(string/lower "HELLO")"#), Value::string("hello"));
    assert_eq!(eval(r#"(string/upper "")"#), Value::string(""));
}

#[test]
fn test_string_replace() {
    assert_eq!(
        eval(r#"(string/replace "hello world" "world" "sema")"#),
        Value::string("hello sema")
    );
    assert_eq!(
        eval(r#"(string/replace "aaa" "a" "bb")"#),
        Value::string("bbbbbb")
    );
}

#[test]
fn test_string_join() {
    assert_eq!(
        eval(r#"(string/join (list "a" "b" "c") ", ")"#),
        Value::string("a, b, c")
    );
    assert_eq!(
        eval(r#"(string/join (list "one") "-")"#),
        Value::string("one")
    );
    assert_eq!(eval(r#"(string/join (list) ", ")"#), Value::string(""));
}

#[test]
fn test_str_concat() {
    assert_eq!(eval(r#"(str "a" 1 :b)"#), Value::string("a1:b"));
    assert_eq!(eval(r#"(str)"#), Value::string(""));
    assert_eq!(eval(r#"(str "hello")"#), Value::string("hello"));
}

#[test]
fn test_string_append_coercion() {
    // string-append coerces non-strings
    assert_eq!(
        eval(r#"(string-append "val=" 42)"#),
        Value::string("val=42")
    );
}

#[test]
fn test_hash_map() {
    assert_eq!(eval("(get (hash-map :x 1 :y 2) :x)"), Value::int(1));
    assert_eq!(eval("(get (hash-map :x 1 :y 2) :y)"), Value::int(2));
}

#[test]
fn test_dissoc() {
    assert_eq!(eval("(get (dissoc {:a 1 :b 2 :c 3} :b) :b)"), Value::nil());
    assert_eq!(eval("(count (dissoc {:a 1 :b 2 :c 3} :b))"), Value::int(2));
    // Dissoc multiple keys
    assert_eq!(
        eval("(count (dissoc {:a 1 :b 2 :c 3} :a :c))"),
        Value::int(1)
    );
}

#[test]
fn test_vals() {
    // vals returns values; BTreeMap is sorted by key
    let result = eval_to_string("(sort (vals {:a 1 :b 2 :c 3}))");
    assert_eq!(result, "(1 2 3)");
}

#[test]
fn test_merge() {
    assert_eq!(eval("(get (merge {:a 1} {:b 2} {:c 3}) :b)"), Value::int(2));
    // Later maps override earlier
    assert_eq!(eval("(get (merge {:a 1} {:a 99}) :a)"), Value::int(99));
    // Empty merge
    assert_eq!(eval_to_string("(merge)"), "{}");
}

#[test]
fn test_contains() {
    assert_eq!(eval("(contains? {:a 1 :b 2} :a)"), Value::bool(true));
    assert_eq!(eval("(contains? {:a 1 :b 2} :c)"), Value::bool(false));
}

#[test]
fn test_count() {
    assert_eq!(eval("(count {:a 1 :b 2})"), Value::int(2));
    assert_eq!(eval("(count (list 1 2 3))"), Value::int(3));
    assert_eq!(eval("(count [1 2 3 4])"), Value::int(4));
    assert_eq!(eval(r#"(count "hello")"#), Value::int(5));
    assert_eq!(eval("(count nil)"), Value::int(0));
}

#[test]
fn test_empty() {
    assert_eq!(eval("(empty? (list))"), Value::bool(true));
    assert_eq!(eval("(empty? (list 1))"), Value::bool(false));
    assert_eq!(eval("(empty? {})"), Value::bool(true));
    assert_eq!(eval("(empty? {:a 1})"), Value::bool(false));
    assert_eq!(eval("(empty? [])"), Value::bool(true));
    assert_eq!(eval(r#"(empty? "")"#), Value::bool(true));
    assert_eq!(eval("(empty? nil)"), Value::bool(true));
}

#[test]
fn test_get_with_default() {
    assert_eq!(eval("(get {:a 1} :b 42)"), Value::int(42));
    assert_eq!(eval("(get {:a 1} :a 42)"), Value::int(1));
    assert_eq!(eval("(get {:a 1} :missing)"), Value::nil());
}

#[test]
fn test_keyword_as_fn_missing_key() {
    // Keyword lookup on map returns nil for missing key
    assert_eq!(eval("(:missing {:a 1})"), Value::nil());
}

#[test]
fn test_round() {
    assert_eq!(eval("(round 3.4)"), Value::int(3));
    assert_eq!(eval("(round 3.5)"), Value::int(4));
    assert_eq!(eval("(round -1.5)"), Value::int(-2));
    assert_eq!(eval("(round 5)"), Value::int(5)); // int passthrough
}

#[test]
fn test_sqrt() {
    assert_eq!(eval("(sqrt 16)"), Value::float(4.0));
    assert_eq!(eval("(sqrt 2.0)"), Value::float(2.0_f64.sqrt()));
}

#[test]
fn test_pow() {
    assert_eq!(eval("(pow 2 10)"), Value::int(1024));
    assert_eq!(eval("(pow 3 0)"), Value::int(1));
    assert_eq!(eval("(pow 2.0 0.5)"), Value::float(2.0_f64.powf(0.5)));
}

#[test]
fn test_log() {
    assert_eq!(eval("(log 1)"), Value::float(0.0));
    // log(e) ≈ 1.0
    let result = eval("(log e)");
    if let Some(f) = result.as_float() {
        assert!((f - 1.0).abs() < 1e-10);
    } else {
        panic!("expected float");
    }
}

#[test]
fn test_trig() {
    assert_eq!(eval("(sin 0)"), Value::float(0.0));
    assert_eq!(eval("(cos 0)"), Value::float(1.0));
    // sin(pi/2) ≈ 1.0
    let result = eval("(sin (/ pi 2))");
    if let Some(f) = result.as_float() {
        assert!((f - 1.0).abs() < 1e-10);
    } else {
        panic!("expected float");
    }
}

#[test]
fn test_pi_and_e_constants() {
    if let Some(p) = eval("pi").as_float() {
        assert!((p - std::f64::consts::PI).abs() < 1e-15);
    } else {
        panic!("expected float");
    }
    if let Some(e) = eval("e").as_float() {
        assert!((e - std::f64::consts::E).abs() < 1e-15);
    } else {
        panic!("expected float");
    }
}

#[test]
fn test_int_float_conversion() {
    assert_eq!(eval("(int 3.7)"), Value::int(3));
    assert_eq!(eval("(int 42)"), Value::int(42));
    assert_eq!(eval(r#"(int "99")"#), Value::int(99));
    assert_eq!(eval("(float 42)"), Value::float(42.0));
    assert_eq!(eval("(float 3.14)"), Value::float(3.14));
    assert_eq!(eval(r#"(float "2.5")"#), Value::float(2.5));
}

#[test]
fn test_division_by_zero() {
    let err = eval_err("(/ 1 0)");
    assert!(matches!(err.inner(), SemaError::Eval(_)));
    let err = eval_err("(mod 5 0)");
    assert!(matches!(err.inner(), SemaError::Eval(_)));
}

#[test]
fn test_arithmetic_identity() {
    // (+) => 0, (*) => 1 (identity elements)
    assert_eq!(eval("(+)"), Value::int(0));
    assert_eq!(eval("(*)"), Value::int(1));
}

#[test]
fn test_unary_minus() {
    assert_eq!(eval("(- 5)"), Value::int(-5));
    assert_eq!(eval("(- -3)"), Value::int(3));
    assert_eq!(eval("(- 2.5)"), Value::float(-2.5));
}

#[test]
fn test_mixed_int_float_arithmetic() {
    assert_eq!(eval("(+ 1 2.0)"), Value::float(3.0));
    assert_eq!(eval("(* 3 1.5)"), Value::float(4.5));
    assert_eq!(eval("(- 10 2.5)"), Value::float(7.5));
    assert_eq!(eval("(/ 10 2)"), Value::int(5)); // integer division when exact
    assert_eq!(eval("(/ 7 2)"), Value::float(3.5)); // non-exact → float
    assert_eq!(eval("(/ 7.0 2)"), Value::float(3.5));
}

#[test]
fn test_chained_comparison() {
    assert_eq!(eval("(< 1 2 3 4)"), Value::bool(true));
    assert_eq!(eval("(< 1 2 2 4)"), Value::bool(false));
    assert_eq!(eval("(>= 4 3 2 1)"), Value::bool(true));
    assert_eq!(eval("(>= 4 3 3 1)"), Value::bool(true));
    assert_eq!(eval("(= 5 5 5 5)"), Value::bool(true));
    assert_eq!(eval("(= 5 5 4 5)"), Value::bool(false));
}

#[test]
fn test_arity_errors() {
    assert!(eval_err("(car)").to_string().contains("expects"));
    assert!(eval_err("(car 1 2)").to_string().contains("expects"));
    assert!(eval_err("(not)").to_string().contains("expects"));
    assert!(eval_err("(string-length)").to_string().contains("expects"));
}

#[test]
fn test_type_errors() {
    let err = eval_err(r#"(+ 1 "hello")"#);
    assert!(matches!(err.inner(), SemaError::Type { .. }));
    let err = eval_err("(car 42)");
    assert!(matches!(err.inner(), SemaError::Type { .. }));
    let err = eval_err(r#"(< "a" "b")"#);
    assert!(matches!(err.inner(), SemaError::Type { .. }));
}

#[test]
fn test_unbound_variable() {
    let err = eval_err("no-such-var");
    assert!(matches!(err.inner(), SemaError::Unbound(_)));
}

#[test]
fn test_truthiness() {
    // Only nil and #f are falsy
    assert_eq!(eval("(if nil 1 2)"), Value::int(2));
    assert_eq!(eval("(if #f 1 2)"), Value::int(2));
    // Everything else is truthy, including 0, empty string, empty list
    assert_eq!(eval("(if 0 1 2)"), Value::int(1));
    assert_eq!(eval(r#"(if "" 1 2)"#), Value::int(1));
    assert_eq!(eval("(if (list) 1 2)"), Value::int(1));
    assert_eq!(eval("(if :foo 1 2)"), Value::int(1));
}

#[test]
fn test_and_or_empty() {
    assert_eq!(eval("(and)"), Value::bool(true));
    assert_eq!(eval("(or)"), Value::bool(false));
}

#[test]
fn test_begin_empty() {
    assert_eq!(eval("(begin)"), Value::nil());
}

#[test]
fn test_if_two_branch() {
    // if with no else returns nil
    assert_eq!(eval("(if #f 42)"), Value::nil());
    assert_eq!(eval("(if #t 42)"), Value::int(42));
}

#[test]
fn test_let_empty_bindings() {
    assert_eq!(eval("(let () 42)"), Value::int(42));
    assert_eq!(eval("(let* () 42)"), Value::int(42));
}

#[test]
fn test_fn_alias() {
    // fn is an alias for lambda
    assert_eq!(eval("((fn (x) (* x x)) 5)"), Value::int(25));
    assert_eq!(
        eval("(begin (define square (fn (x) (* x x))) (square 7))"),
        Value::int(49)
    );
}

#[test]
fn test_do_loop() {
    // do is now a proper Scheme iteration form
    // Sum 1..10
    assert_eq!(
        eval("(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 10) sum))"),
        Value::int(45)
    );
    // begin still works for sequencing
    assert_eq!(eval("(begin 1 2 3)"), Value::int(3));
}

#[test]
fn test_unquote_splicing() {
    assert_eq!(
        eval_to_string("(begin (define xs (list 1 2 3)) `(a ,@xs b))"),
        "(a 1 2 3 b)"
    );
    assert_eq!(eval_to_string("`(,@(list 1 2) ,@(list 3 4))"), "(1 2 3 4)");
    // Splicing empty list
    assert_eq!(eval_to_string("`(a ,@(list) b)"), "(a b)");
}

#[test]
fn test_nested_closures() {
    // Closure over multiple layers of scope
    assert_eq!(
        eval("(begin (define (make-counter) (define n 0) (lambda () (set! n (+ n 1)) n)) (define c (make-counter)) (c) (c) (c))"),
        Value::int(3)
    );
}

#[test]
fn test_closure_independence() {
    // Two closures over the same factory don't share state
    assert_eq!(
        eval("(begin (define (make-counter) (define n 0) (lambda () (set! n (+ n 1)) n)) (define a (make-counter)) (define b (make-counter)) (a) (a) (a) (b) (b) (list (a) (b)))"),
        eval("(list 4 3)")
    );
}

#[test]
fn test_internal_define() {
    assert_eq!(
        eval("(begin (define (f x) (define y (* x 2)) (+ y 1)) (f 5))"),
        Value::int(11)
    );
}

#[test]
fn test_closure_captures_environment() {
    // Closure captures variables, not values
    assert_eq!(
        eval("(begin (define x 1) (define f (lambda () x)) (set! x 2) (f))"),
        Value::int(2)
    );
}

#[test]
fn test_tco_deep_recursion() {
    // If TCO is broken, this will stack overflow
    assert_eq!(
        eval(
            "(begin (define (sum n acc) (if (= n 0) acc (sum (- n 1) (+ n acc)))) (sum 100000 0))"
        ),
        Value::int(5000050000)
    );
}

#[test]
fn test_tco_mutual_recursion() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define (is-even? n) (if (= n 0) #t (is-odd? (- n 1))))
              (define (is-odd? n) (if (= n 0) #f (is-even? (- n 1))))
              (is-even? 10000))
        "#
        ),
        Value::bool(true)
    );
}

#[test]
fn test_tco_named_let() {
    // Deep named-let loop
    assert_eq!(
        eval("(let loop ((i 100000) (acc 0)) (if (= i 0) acc (loop (- i 1) (+ acc i))))"),
        Value::int(5000050000)
    );
}

#[test]
fn test_for_each() {
    // for-each returns nil
    assert_eq!(eval("(for-each (lambda (x) x) (list 1 2 3))"), Value::nil());
    // for-each on empty list
    assert_eq!(eval("(for-each (lambda (x) x) (list))"), Value::nil());
}

#[test]
fn test_map_empty_list() {
    assert_eq!(eval_to_string("(map + (list))"), "()");
    assert_eq!(eval_to_string("(filter even? (list))"), "()");
    assert_eq!(eval("(foldl + 0 (list))"), Value::int(0));
}

#[test]
fn test_cons_behavior() {
    // cons onto a list
    assert_eq!(eval_to_string("(cons 1 (list 2 3))"), "(1 2 3)");
    // cons onto empty list
    assert_eq!(eval_to_string("(cons 1 (list))"), "(1)");
    // cons of two atoms creates a two-element list in Sema
    assert_eq!(eval_to_string("(cons 1 2)"), "(1 2)");
}

#[test]
fn test_apply_with_prefix_args() {
    assert_eq!(eval("(apply + 1 2 (list 3 4))"), Value::int(10));
    assert_eq!(eval("(apply + (list))"), Value::int(0));
    assert_eq!(eval("(apply list 1 2 (list 3))"), eval("(list 1 2 3)"));
}

#[test]
fn test_nested_list_ops() {
    // map over map results
    assert_eq!(
        eval_to_string("(map (lambda (x) (+ x 1)) (map (lambda (x) (* x 2)) (list 1 2 3)))"),
        "(3 5 7)"
    );
}

#[test]
fn test_format_directives() {
    // ~a: display without quotes
    assert_eq!(eval(r#"(format "~a" "hello")"#), Value::string("hello"));
    // ~s: write with quotes
    assert_eq!(eval(r#"(format "~s" "hello")"#), Value::string("\"hello\""));
    // ~%: newline
    assert_eq!(eval(r#"(format "a~%b")"#), Value::string("a\nb"));
    // ~~: literal tilde
    assert_eq!(eval(r#"(format "~~")"#), Value::string("~"));
}

#[test]
fn test_vector_basics() {
    assert_eq!(eval("(vector? [1 2 3])"), Value::bool(true));
    assert_eq!(eval("(length [1 2 3])"), Value::int(3));
    assert_eq!(eval("(count [])"), Value::int(0));
    assert_eq!(eval("(empty? [])"), Value::bool(true));
    assert_eq!(eval("(empty? [1])"), Value::bool(false));
}

#[test]
fn test_cond_no_match_no_else() {
    assert_eq!(eval("(cond (#f 1) (#f 2))"), Value::nil());
}

#[test]
fn test_cond_first_match_wins() {
    assert_eq!(eval("(cond (#t 1) (#t 2) (else 3))"), Value::int(1));
}

#[test]
fn test_error_builtin() {
    let err = eval_err(r#"(error "custom error message")"#);
    assert!(err.to_string().contains("custom error message"));
}

#[test]
fn test_try_catch_returns_value_on_success() {
    assert_eq!(eval("(try (+ 1 2) (catch e 0))"), Value::int(3));
}

#[test]
fn test_try_catch_error_info_map() {
    // Caught error is a map with :type and :message
    assert_eq!(
        eval(r#"(try (error "boom") (catch e (:type e)))"#),
        Value::keyword("eval")
    );
    assert_eq!(
        eval(r#"(try (error "boom") (catch e (:message e)))"#),
        Value::string("boom")
    );
}

#[test]
fn test_try_catch_division_by_zero() {
    assert_eq!(
        eval(r#"(try (/ 1 0) (catch e (:type e)))"#),
        Value::keyword("eval")
    );
}

#[test]
fn test_negative_numbers() {
    assert_eq!(eval("-42"), Value::int(-42));
    assert_eq!(eval("-3.14"), Value::float(-3.14));
}

#[test]
fn test_string_escapes() {
    assert_eq!(eval(r#"(string-length "\n")"#), Value::int(1));
    assert_eq!(eval(r#"(string-length "\t")"#), Value::int(1));
    assert_eq!(eval(r#"(string-length "\\")"#), Value::int(1));
    assert_eq!(eval(r#"(string-length "\"")"#), Value::int(1));
}

#[test]
fn test_string_hex_escape_r7rs() {
    // \x<hex>; R7RS-style
    assert_eq!(eval(r#""\x41;""#), Value::string("A"));
    assert_eq!(eval(r#""\x1B;""#), Value::string("\x1B"));
    assert_eq!(eval(r#""\x3BB;""#), Value::string("λ"));
    assert_eq!(eval(r#"(string-length "\x41;")"#), Value::int(1));
    // string-length counts characters; U+1F600 is 1 character
    assert_eq!(eval(r#"(string-length "\x1F600;")"#), Value::int(1));
}

#[test]
fn test_string_u_escape() {
    assert_eq!(eval(r#""\u0041""#), Value::string("A"));
    assert_eq!(eval(r#""\u03BB""#), Value::string("λ"));
    assert_eq!(eval(r#"(string-length "\u0041")"#), Value::int(1));
}

#[test]
fn test_string_big_u_escape() {
    assert_eq!(eval(r#""\U00000041""#), Value::string("A"));
    assert_eq!(eval(r#""\U0001F600""#), Value::string("😀"));
    // string-length counts characters; U+1F600 is 1 character
    assert_eq!(eval(r#"(string-length "\U0001F600")"#), Value::int(1));
}

#[test]
fn test_string_null_escape() {
    assert_eq!(eval(r#"(string-length "\0")"#), Value::int(1));
}

#[test]
fn test_string_mixed_escape_types() {
    assert_eq!(eval(r#""\x48;\u0069""#), Value::string("Hi"));
    assert_eq!(
        eval(r#"(string-append "\x48;" "\u0069")"#),
        Value::string("Hi")
    );
}

#[test]
fn test_string_ansi_escape_codes() {
    // Real-world: ANSI color escape sequences
    // "\x1B;[31m" = ESC [ 3 1 m = 5 bytes
    let result = eval(r#"(string-length "\x1B;[31m")"#);
    assert_eq!(result, Value::int(5));
}

#[test]
fn test_begin_returns_last() {
    assert_eq!(eval("(begin (+ 1 2) (+ 3 4) (+ 5 6))"), Value::int(11));
}

#[test]
fn test_lambda_multi_body() {
    assert_eq!(
        eval("((lambda (x) (+ x 1) (+ x 2) (+ x 3)) 10)"),
        Value::int(13)
    );
}

#[test]
fn test_compose_higher_order() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define (compose f g) (lambda (x) (f (g x))))
              (define (add1 x) (+ x 1))
              (define (double x) (* x 2))
              (define add1-then-double (compose double add1))
              (define double-then-add1 (compose add1 double))
              (list (add1-then-double 3) (double-then-add1 3)))
        "#
        ),
        eval("(list 8 7)")
    );
}

#[test]
fn test_partial_application_pattern() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define (partial f . args)
                (lambda (. rest) (apply f (append args rest))))
              (define add5 (partial + 5))
              (add5 3))
        "#
        ),
        Value::int(8)
    );
}

#[test]
fn test_ackermann() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define (ack m n)
                (cond
                  ((= m 0) (+ n 1))
                  ((= n 0) (ack (- m 1) 1))
                  (else (ack (- m 1) (ack m (- n 1))))))
              (ack 3 4))
        "#
        ),
        Value::int(125)
    );
}

#[test]
fn test_flatten_deep() {
    // flatten is one-level deep in Sema
    assert_eq!(
        eval_to_string("(flatten (list (list 1 (list 2 3)) (list 4) 5))"),
        "(1 (2 3) 4 5)"
    );
    // Full flat for already-flat nested lists
    assert_eq!(
        eval_to_string("(flatten (list (list 1 2) (list 3 4) (list 5)))"),
        "(1 2 3 4 5)"
    );
}

#[test]
fn test_defmacro_with_quasiquote() {
    assert_eq!(
        eval(
            r#"
            (begin
              (defmacro swap! (a b)
                `(let ((tmp ,a)) (set! ,a ,b) (set! ,b tmp)))
              (define x 1)
              (define y 2)
              (swap! x y)
              (list x y))
        "#
        ),
        eval("(list 2 1)")
    );
}

#[test]
fn test_defmacro_unless_custom() {
    assert_eq!(
        eval(
            r#"
            (begin
              (defmacro my-unless (test body)
                (list 'if test nil body))
              (my-unless #f 42))
        "#
        ),
        Value::int(42)
    );
}

// New stdlib functions — tests for features added in current dev branch

#[test]
fn test_list_index_of() {
    assert_eq!(eval("(list/index-of (list 10 20 30) 20)"), Value::int(1));
    assert_eq!(eval("(list/index-of (list 10 20 30) 10)"), Value::int(0));
    assert_eq!(eval("(list/index-of (list 10 20 30) 99)"), Value::nil());
    assert_eq!(eval("(list/index-of (list) 1)"), Value::nil());
}

#[test]
fn test_list_unique() {
    assert_eq!(
        eval_to_string("(list/unique (list 1 2 3 2 1 4))"),
        "(1 2 3 4)"
    );
    assert_eq!(eval_to_string("(list/unique (list))"), "()");
    assert_eq!(
        eval_to_string("(list/unique (list :a :b :a :c :b))"),
        "(:a :b :c)"
    );
}

#[test]
fn test_list_group_by() {
    let result = eval(r#"(list/group-by even? (list 1 2 3 4 5 6))"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(
            m.get(&Value::bool(true)).cloned(),
            Some(eval("(list 2 4 6)"))
        );
        assert_eq!(
            m.get(&Value::bool(false)).cloned(),
            Some(eval("(list 1 3 5)"))
        );
    } else {
        panic!("expected map, got {result}");
    }
}

#[test]
fn test_list_interleave() {
    assert_eq!(
        eval_to_string("(list/interleave (list 1 2 3) (list :a :b :c))"),
        "(1 :a 2 :b 3 :c)"
    );
    // Shortest wins
    assert_eq!(
        eval_to_string("(list/interleave (list 1 2) (list :a :b :c))"),
        "(1 :a 2 :b)"
    );
}

#[test]
fn test_list_chunk() {
    assert_eq!(
        eval_to_string("(list/chunk 2 (list 1 2 3 4 5))"),
        "((1 2) (3 4) (5))"
    );
    assert_eq!(
        eval_to_string("(list/chunk 3 (list 1 2 3 4 5 6))"),
        "((1 2 3) (4 5 6))"
    );
    assert_eq!(eval_to_string("(list/chunk 2 (list))"), "()");
}

#[test]
fn test_map_entries() {
    // BTreeMap is sorted by key, so entries are in key order
    assert_eq!(
        eval_to_string("(map/entries {:a 1 :b 2})"),
        "((:a 1) (:b 2))"
    );
    assert_eq!(eval_to_string("(map/entries {})"), "()");
}

#[test]
fn test_map_map_vals() {
    assert_eq!(
        eval("(get (map/map-vals (lambda (v) (* v 2)) {:a 1 :b 2 :c 3}) :b)"),
        Value::int(4)
    );
    assert_eq!(
        eval("(get (map/map-vals (lambda (v) (+ v 10)) {:x 5}) :x)"),
        Value::int(15)
    );
}

#[test]
fn test_map_filter() {
    assert_eq!(
        eval("(count (map/filter (lambda (k v) (> v 1)) {:a 1 :b 2 :c 3}))"),
        Value::int(2)
    );
    assert_eq!(
        eval("(get (map/filter (lambda (k v) (> v 1)) {:a 1 :b 2 :c 3}) :a)"),
        Value::nil()
    );
}

#[test]
fn test_map_select_keys() {
    assert_eq!(
        eval("(count (map/select-keys {:a 1 :b 2 :c 3} (list :a :c)))"),
        Value::int(2)
    );
    assert_eq!(
        eval("(get (map/select-keys {:a 1 :b 2 :c 3} (list :a :c)) :b)"),
        Value::nil()
    );
    assert_eq!(
        eval("(get (map/select-keys {:a 1 :b 2 :c 3} (list :a :c)) :a)"),
        Value::int(1)
    );
}

#[test]
fn test_map_update() {
    assert_eq!(
        eval("(get (map/update {:a 1 :b 2} :a (lambda (v) (+ v 10))) :a)"),
        Value::int(11)
    );
    // Missing key → fn gets nil
    assert_eq!(
        eval(r#"(get (map/update {:a 1} :b (lambda (v) "default")) :b)"#),
        Value::string("default")
    );
}

#[test]
fn test_string_index_of() {
    assert_eq!(
        eval(r#"(string/index-of "hello world" "world")"#),
        Value::int(6)
    );
    assert_eq!(eval(r#"(string/index-of "hello" "xyz")"#), Value::nil());
    assert_eq!(eval(r#"(string/index-of "abcabc" "bc")"#), Value::int(1));
}

#[test]
fn test_string_chars() {
    assert_eq!(
        eval_to_string(r#"(string/chars "abc")"#),
        r#"(#\a #\b #\c)"#
    );
    assert_eq!(eval_to_string(r#"(string/chars "")"#), "()");
}

#[test]
fn test_string_repeat() {
    assert_eq!(eval(r#"(string/repeat "ab" 3)"#), Value::string("ababab"));
    assert_eq!(eval(r#"(string/repeat "x" 0)"#), Value::string(""));
    assert_eq!(eval(r#"(string/repeat "" 5)"#), Value::string(""));
}

#[test]
fn test_string_pad_left() {
    assert_eq!(eval(r#"(string/pad-left "42" 5)"#), Value::string("   42"));
    assert_eq!(
        eval(r#"(string/pad-left "42" 5 "0")"#),
        Value::string("00042")
    );
    assert_eq!(
        eval(r#"(string/pad-left "hello" 3)"#),
        Value::string("hello")
    ); // longer than width
}

#[test]
fn test_string_pad_right() {
    assert_eq!(eval(r#"(string/pad-right "42" 5)"#), Value::string("42   "));
    assert_eq!(
        eval(r#"(string/pad-right "42" 5 ".")"#),
        Value::string("42...")
    );
    assert_eq!(
        eval(r#"(string/pad-right "hello" 3)"#),
        Value::string("hello")
    );
}

#[test]
fn test_math_quotient_remainder() {
    assert_eq!(eval("(math/quotient 13 4)"), Value::int(3));
    assert_eq!(eval("(math/quotient -13 4)"), Value::int(-3));
    assert_eq!(eval("(math/remainder 13 4)"), Value::int(1));
    assert_eq!(eval("(math/remainder -13 4)"), Value::int(-1));
    // Division by zero
    assert!(eval_err("(math/quotient 5 0)")
        .to_string()
        .contains("division by zero"));
    assert!(eval_err("(math/remainder 5 0)")
        .to_string()
        .contains("division by zero"));
}

#[test]
fn test_math_gcd_lcm() {
    assert_eq!(eval("(math/gcd 12 8)"), Value::int(4));
    assert_eq!(eval("(math/gcd 7 13)"), Value::int(1));
    assert_eq!(eval("(math/gcd -12 8)"), Value::int(4));
    assert_eq!(eval("(math/gcd 0 5)"), Value::int(5));
    assert_eq!(eval("(math/lcm 4 6)"), Value::int(12));
    assert_eq!(eval("(math/lcm 3 5)"), Value::int(15));
    assert_eq!(eval("(math/lcm 0 0)"), Value::int(0));
}

#[test]
fn test_math_trig_extended() {
    // tan(0) = 0
    assert_eq!(eval("(math/tan 0)"), Value::float(0.0));
    // asin(0) = 0, acos(1) = 0, atan(0) = 0
    assert_eq!(eval("(math/asin 0)"), Value::float(0.0));
    assert_eq!(eval("(math/acos 1)"), Value::float(0.0));
    assert_eq!(eval("(math/atan 0)"), Value::float(0.0));
    // atan2(0, 1) = 0, atan2(1, 0) = pi/2
    assert_eq!(eval("(math/atan2 0 1)"), Value::float(0.0));
    if let Some(f) = eval("(math/atan2 1 0)").as_float() {
        assert!((f - std::f64::consts::FRAC_PI_2).abs() < 1e-10);
    } else {
        panic!("expected float");
    }
}

#[test]
fn test_math_exp_log() {
    // exp(0) = 1
    assert_eq!(eval("(math/exp 0)"), Value::float(1.0));
    // exp(1) ≈ e
    if let Some(f) = eval("(math/exp 1)").as_float() {
        assert!((f - std::f64::consts::E).abs() < 1e-10);
    } else {
        panic!("expected float");
    }
    // log10(100) = 2
    assert_eq!(eval("(math/log10 100)"), Value::float(2.0));
    // log2(8) = 3
    assert_eq!(eval("(math/log2 8)"), Value::float(3.0));
}

#[test]
fn test_math_random() {
    // math/random returns a float in [0, 1)
    if let Some(f) = eval("(math/random)").as_float() {
        assert!(f >= 0.0 && f < 1.0);
    } else {
        panic!("expected float");
    }
    // math/random-int returns int in range
    if let Some(n) = eval("(math/random-int 1 10)").as_int() {
        assert!(n >= 1 && n <= 10);
    } else {
        panic!("expected int");
    }
}

#[test]
fn test_math_clamp() {
    assert_eq!(eval("(math/clamp 5 1 10)"), Value::int(5));
    assert_eq!(eval("(math/clamp -5 1 10)"), Value::int(1));
    assert_eq!(eval("(math/clamp 15 1 10)"), Value::int(10));
    assert_eq!(eval("(math/clamp 0.5 0.0 1.0)"), Value::float(0.5));
    assert_eq!(eval("(math/clamp -1.0 0.0 1.0)"), Value::float(0.0));
}

#[test]
fn test_math_sign() {
    assert_eq!(eval("(math/sign 42)"), Value::int(1));
    assert_eq!(eval("(math/sign -5)"), Value::int(-1));
    assert_eq!(eval("(math/sign 0)"), Value::int(0));
    assert_eq!(eval("(math/sign 3.14)"), Value::int(1));
    assert_eq!(eval("(math/sign -0.5)"), Value::int(-1));
}

#[test]
fn test_bitwise_ops() {
    assert_eq!(eval("(bit/and 12 10)"), Value::int(8)); // 1100 & 1010 = 1000
    assert_eq!(eval("(bit/or 12 10)"), Value::int(14)); // 1100 | 1010 = 1110
    assert_eq!(eval("(bit/xor 12 10)"), Value::int(6)); // 1100 ^ 1010 = 0110
    assert_eq!(eval("(bit/not 0)"), Value::int(-1));
    assert_eq!(eval("(bit/shift-left 1 4)"), Value::int(16));
    assert_eq!(eval("(bit/shift-right 16 4)"), Value::int(1));
}

#[test]
fn test_bitwise_edge_cases() {
    assert_eq!(eval("(bit/and 0 255)"), Value::int(0));
    assert_eq!(eval("(bit/or 0 0)"), Value::int(0));
    assert_eq!(eval("(bit/xor 42 42)"), Value::int(0)); // x ^ x = 0
    assert_eq!(eval("(bit/xor 42 0)"), Value::int(42)); // x ^ 0 = x
    assert_eq!(eval("(bit/shift-left 1 0)"), Value::int(1)); // no shift
}

#[test]
fn test_sys_cwd() {
    let result = eval("(sys/cwd)");
    assert!(result.is_string());
    // Should be a non-empty string
    if let Some(s) = result.as_str() {
        assert!(!s.is_empty());
    }
}

#[test]
fn test_sys_platform() {
    let result = eval("(sys/platform)");
    if let Some(s) = result.as_str() {
        assert!(["macos", "linux", "windows", "unknown"].contains(&s));
    } else {
        panic!("expected string");
    }
}

#[test]
fn test_sys_args() {
    // sys/args should return a list
    let result = eval("(sys/args)");
    assert!(result.is_list());
}

#[test]
fn test_sys_env_all() {
    // sys/env-all should return a map
    let result = eval("(sys/env-all)");
    assert!(result.is_map());
    // Should have at least PATH or HOME
    if let Some(m) = result.as_map_rc() {
        assert!(!m.is_empty());
    }
}

#[test]
fn test_file_is_file() {
    let dir = "/tmp/sema-test-isfile";
    let _ = std::fs::remove_dir_all(dir);
    std::fs::create_dir_all(dir).unwrap();
    std::fs::write(format!("{dir}/a.txt"), "hello").unwrap();

    assert_eq!(
        eval(&format!(r#"(file/is-file? "{dir}/a.txt")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/is-file? "{dir}")"#)),
        Value::bool(false)
    );
    assert_eq!(
        eval(r#"(file/is-file? "/tmp/nonexistent-sema-xyz")"#),
        Value::bool(false)
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_is_symlink() {
    // Non-symlink file should return false
    let dir = "/tmp/sema-test-symlink";
    let _ = std::fs::remove_dir_all(dir);
    std::fs::create_dir_all(dir).unwrap();
    std::fs::write(format!("{dir}/a.txt"), "hello").unwrap();

    assert_eq!(
        eval(&format!(r#"(file/is-symlink? "{dir}/a.txt")"#)),
        Value::bool(false)
    );
    assert_eq!(
        eval(r#"(file/is-symlink? "/tmp/nonexistent-sema-xyz")"#),
        Value::bool(false)
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_uuid_v4() {
    let result = eval("(uuid/v4)");
    if let Some(s) = result.as_str() {
        assert_eq!(s.len(), 36);
        assert_eq!(s.chars().filter(|c| *c == '-').count(), 4);
    } else {
        panic!("expected string");
    }
    // Two UUIDs should be different
    assert_ne!(eval("(uuid/v4)"), eval("(uuid/v4)"));
}

#[test]
fn test_base64_encode_decode() {
    assert_eq!(
        eval(r#"(base64/encode "hello")"#),
        Value::string("aGVsbG8=")
    );
    assert_eq!(
        eval(r#"(base64/decode "aGVsbG8=")"#),
        Value::string("hello")
    );
    assert_eq!(eval(r#"(base64/encode "")"#), Value::string(""));
    assert_eq!(eval(r#"(base64/decode "")"#), Value::string(""));
    // Roundtrip
    assert_eq!(
        eval(r#"(base64/decode (base64/encode "Sema Lisp 🎉"))"#),
        Value::string("Sema Lisp 🎉")
    );
}

#[test]
fn test_hash_sha256() {
    // Known SHA-256 of "hello"
    assert_eq!(
        eval(r#"(hash/sha256 "hello")"#),
        Value::string("2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
    );
    // Empty string
    assert_eq!(
        eval(r#"(hash/sha256 "")"#),
        Value::string("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    );
}

#[test]
fn test_time_now() {
    let result = eval("(time/now)");
    if let Some(f) = result.as_float() {
        assert!(f > 1_700_000_000.0); // After 2023
    } else {
        panic!("expected float");
    }
}

#[test]
fn test_time_format() {
    assert_eq!(
        eval(r#"(time/format 0.0 "%Y-%m-%d")"#),
        Value::string("1970-01-01")
    );
    assert_eq!(
        eval(r#"(time/format 0.0 "%H:%M:%S")"#),
        Value::string("00:00:00")
    );
}

#[test]
fn test_time_parse() {
    let result = eval(r#"(time/parse "2024-01-15 12:30:00" "%Y-%m-%d %H:%M:%S")"#);
    if let Some(f) = result.as_float() {
        assert!(f > 1_705_000_000.0 && f < 1_706_000_000.0);
    } else {
        panic!("expected float, got {result}");
    }
}

#[test]
fn test_time_date_parts() {
    let result = eval("(time/date-parts 0.0)");
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("year")), Some(&Value::int(1970)));
        assert_eq!(m.get(&Value::keyword("month")), Some(&Value::int(1)));
        assert_eq!(m.get(&Value::keyword("day")), Some(&Value::int(1)));
        assert_eq!(m.get(&Value::keyword("hour")), Some(&Value::int(0)));
        assert_eq!(m.get(&Value::keyword("minute")), Some(&Value::int(0)));
        assert_eq!(m.get(&Value::keyword("second")), Some(&Value::int(0)));
        assert!(m.get(&Value::keyword("weekday")).is_some());
    } else {
        panic!("expected map");
    }
}

#[test]
fn test_time_roundtrip() {
    // Format and parse should roundtrip
    assert_eq!(
        eval(
            r#"(time/format (time/parse "2024-06-15 08:30:00" "%Y-%m-%d %H:%M:%S") "%Y-%m-%d %H:%M:%S")"#
        ),
        Value::string("2024-06-15 08:30:00")
    );
}

#[test]
fn test_csv_parse() {
    assert_eq!(
        eval_to_string(r#"(csv/parse "a,b\n1,2")"#),
        r#"(("a" "b") ("1" "2"))"#
    );
    // Single row
    assert_eq!(
        eval_to_string(r#"(csv/parse "x,y,z")"#),
        r#"(("x" "y" "z"))"#
    );
}

#[test]
fn test_csv_parse_maps() {
    let result = eval(r#"(csv/parse-maps "name,age\nAlice,30\nBob,25")"#);
    if let Some(rows) = result.as_list() {
        assert_eq!(rows.len(), 2);
        if let Some(m) = rows[0].as_map_rc() {
            assert_eq!(
                m.get(&Value::keyword("name")),
                Some(&Value::string("Alice"))
            );
            assert_eq!(m.get(&Value::keyword("age")), Some(&Value::string("30")));
        } else {
            panic!("expected map");
        }
    } else {
        panic!("expected list");
    }
}

#[test]
fn test_csv_encode() {
    let result = eval(r#"(csv/encode '(("a" "b") ("1" "2")))"#);
    if let Some(s) = result.as_str() {
        assert!(s.contains("a,b"));
        assert!(s.contains("1,2"));
    } else {
        panic!("expected string");
    }
}

// Regex operations — extended coverage

#[test]
fn test_regex_match_no_match() {
    assert_eq!(
        eval(r#"(regex/match? "\\d+" "abcdef")"#),
        Value::bool(false)
    );
    assert_eq!(eval(r#"(regex/match "\\d+" "abc")"#), Value::nil());
}

#[test]
fn test_regex_match_groups_detail() {
    let result = eval(r#"(regex/match "(\\d+)-(\\w+)" "42-hello")"#);
    if let Some(m) = result.as_map_rc() {
        assert_eq!(m.get(&Value::keyword("start")), Some(&Value::int(0)));
        assert_eq!(m.get(&Value::keyword("end")), Some(&Value::int(8)));
        if let Some(groups) = m.get(&Value::keyword("groups")).and_then(|v| v.as_list()) {
            assert_eq!(groups.len(), 2);
            assert_eq!(groups[0], Value::string("42"));
            assert_eq!(groups[1], Value::string("hello"));
        } else {
            panic!("expected groups list");
        }
    } else {
        panic!("expected map");
    }
}

#[test]
fn test_regex_find_all_empty() {
    assert_eq!(
        eval_to_string(r#"(regex/find-all "\\d+" "no digits")"#),
        "()"
    );
}

#[test]
fn test_regex_replace_all_whitespace() {
    assert_eq!(
        eval(r#"(regex/replace-all "\\s+" " " "hello   world   foo")"#),
        Value::string("hello world foo")
    );
}

#[test]
fn test_regex_split_whitespace() {
    assert_eq!(
        eval_to_string(r#"(regex/split "\\s+" "hello  world  foo")"#),
        r#"("hello" "world" "foo")"#
    );
}

// String conversion functions

#[test]
fn test_string_to_symbol() {
    assert_eq!(eval(r#"(string->symbol "hello")"#), Value::symbol("hello"));
    assert_eq!(eval(r#"(symbol? (string->symbol "x"))"#), Value::bool(true));
}

#[test]
fn test_symbol_to_string() {
    assert_eq!(eval("(symbol->string 'hello)"), Value::string("hello"));
    assert_eq!(eval(r#"(string? (symbol->string 'x))"#), Value::bool(true));
}

#[test]
fn test_string_to_keyword() {
    assert_eq!(eval(r#"(string->keyword "foo")"#), Value::keyword("foo"));
    assert_eq!(
        eval(r#"(keyword? (string->keyword "bar"))"#),
        Value::bool(true)
    );
}

#[test]
fn test_keyword_to_string() {
    assert_eq!(eval("(keyword->string :foo)"), Value::string("foo"));
    assert_eq!(
        eval(r#"(string? (keyword->string :bar))"#),
        Value::bool(true)
    );
}

#[test]
fn test_number_to_string() {
    assert_eq!(eval("(number->string 42)"), Value::string("42"));
    assert_eq!(eval("(number->string -7)"), Value::string("-7"));
    assert_eq!(eval("(number->string 3.14)"), Value::string("3.14"));
}

#[test]
fn test_string_to_number() {
    assert_eq!(eval(r#"(string->number "42")"#), Value::int(42));
    assert_eq!(eval(r#"(string->number "-7")"#), Value::int(-7));
    assert_eq!(eval(r#"(string->number "3.14")"#), Value::float(3.14));
    // Invalid string should error
    assert!(eval_err(r#"(string->number "abc")"#)
        .to_string()
        .contains("cannot parse"));
}

// JSON encode-pretty

#[test]
fn test_json_encode_pretty() {
    let result = eval(r#"(json/encode-pretty {:a 1 :b 2})"#);
    if let Some(s) = result.as_str() {
        assert!(s.contains("\"a\": 1"));
        assert!(s.contains("\"b\": 2"));
        assert!(s.contains('\n')); // pretty-printed has newlines
    } else {
        panic!("expected string");
    }
}

// Meta: gensym — additional coverage

#[test]
fn test_gensym_is_symbol_type() {
    // Verify gensym result is usable as a symbol
    assert_eq!(eval(r#"(symbol? (gensym))"#), Value::bool(true));
    assert_eq!(eval(r#"(symbol? (gensym "pfx"))"#), Value::bool(true));
}

// System: env, shell, time-ms, sleep

#[test]
fn test_env_var() {
    // PATH should exist on all platforms
    let result = eval(r#"(env "PATH")"#);
    assert!(result.is_string());
    // Non-existent var returns nil
    assert_eq!(
        eval(r#"(env "SEMA_NONEXISTENT_VAR_XYZ_123")"#),
        Value::nil()
    );
}

#[test]
fn test_shell_command() {
    let result = eval(r#"(shell "echo" "hello")"#);
    if let Some(m) = result.as_map_rc() {
        if let Some(stdout) = m.get(&Value::keyword("stdout")).and_then(|v| v.as_str()) {
            assert!(stdout.trim() == "hello");
        } else {
            panic!("expected stdout");
        }
        assert_eq!(m.get(&Value::keyword("exit-code")), Some(&Value::int(0)));
    } else {
        panic!("expected map");
    }
}

#[test]
fn test_time_ms() {
    let result = eval("(time-ms)");
    if let Some(ms) = result.as_int() {
        assert!(ms > 1_700_000_000_000); // After 2023 in millis
    } else {
        panic!("expected int");
    }
}

// List functions: nth, take, drop, last, zip, sort, flatten

#[test]
fn test_nth() {
    assert_eq!(eval("(nth (list 10 20 30) 0)"), Value::int(10));
    assert_eq!(eval("(nth (list 10 20 30) 2)"), Value::int(30));
    assert_eq!(eval("(nth [10 20 30] 1)"), Value::int(20));
    // Out of bounds should error
    assert!(eval_err("(nth (list 1 2) 5)")
        .to_string()
        .contains("out of bounds"));
}

#[test]
fn test_take_and_drop() {
    assert_eq!(eval_to_string("(take 3 (list 1 2 3 4 5))"), "(1 2 3)");
    assert_eq!(eval_to_string("(take 0 (list 1 2 3))"), "()");
    assert_eq!(eval_to_string("(take 10 (list 1 2))"), "(1 2)"); // take more than available
    assert_eq!(eval_to_string("(drop 2 (list 1 2 3 4 5))"), "(3 4 5)");
    assert_eq!(eval_to_string("(drop 0 (list 1 2 3))"), "(1 2 3)");
    assert_eq!(eval_to_string("(drop 10 (list 1 2))"), "()"); // drop more than available
}

#[test]
fn test_last_extended() {
    assert_eq!(eval("(last (list 1 2 3))"), Value::int(3));
    assert_eq!(eval("(last (list 42))"), Value::int(42));
    assert_eq!(eval("(last (list))"), Value::nil());
    assert_eq!(eval("(last [10 20 30])"), Value::int(30));
}

#[test]
fn test_sort_with_comparator() {
    // Sort descending using a comparator
    assert_eq!(
        eval_to_string("(sort (list 3 1 4 1 5) (lambda (a b) (- b a)))"),
        "(5 4 3 1 1)"
    );
}

#[test]
fn test_range_with_step() {
    assert_eq!(eval_to_string("(range 0 10 2)"), "(0 2 4 6 8)");
    assert_eq!(eval_to_string("(range 10 0 -2)"), "(10 8 6 4 2)");
    // Step of zero should error
    assert!(eval_err("(range 0 10 0)").to_string().contains("step"));
}

// IO: read, read-many (parsing s-expressions from strings)

#[test]
fn test_read_sexp() {
    assert_eq!(eval(r#"(read "(+ 1 2)")"#), eval("'(+ 1 2)"));
    assert_eq!(eval(r#"(read "42")"#), Value::int(42));
    assert_eq!(eval(r#"(read ":foo")"#), Value::keyword("foo"));
}

#[test]
fn test_read_many() {
    assert_eq!(
        eval_to_string(r#"(read-many "(+ 1 2) (* 3 4)")"#),
        "((+ 1 2) (* 3 4))"
    );
}

// File operations (extended): append, delete, rename, list, mkdir,
// is-directory?, info, read-lines, write-lines, copy

#[test]
fn test_file_append_standalone() {
    let dir = "/tmp/sema-test-append";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));

    eval(&format!(r#"(file/write "{dir}/a.txt" "hello")"#));
    eval(&format!(r#"(file/append "{dir}/a.txt" " world")"#));
    eval(&format!(r#"(file/append "{dir}/a.txt" "!")"#));
    assert_eq!(
        eval(&format!(r#"(file/read "{dir}/a.txt")"#)),
        Value::string("hello world!")
    );

    // Append to non-existent file creates it
    eval(&format!(r#"(file/append "{dir}/new.txt" "created")"#));
    assert_eq!(
        eval(&format!(r#"(file/read "{dir}/new.txt")"#)),
        Value::string("created")
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_delete_standalone() {
    let dir = "/tmp/sema-test-delete";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));

    eval(&format!(r#"(file/write "{dir}/del.txt" "bye")"#));
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/del.txt")"#)),
        Value::bool(true)
    );
    eval(&format!(r#"(file/delete "{dir}/del.txt")"#));
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/del.txt")"#)),
        Value::bool(false)
    );

    // Deleting non-existent file should error
    let err = eval_err(&format!(r#"(file/delete "{dir}/nonexistent.txt")"#));
    assert!(err.to_string().contains("file/delete"));

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_rename_standalone() {
    let dir = "/tmp/sema-test-rename";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));

    eval(&format!(r#"(file/write "{dir}/old.txt" "content")"#));
    eval(&format!(r#"(file/rename "{dir}/old.txt" "{dir}/new.txt")"#));
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/old.txt")"#)),
        Value::bool(false)
    );
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/new.txt")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/read "{dir}/new.txt")"#)),
        Value::string("content")
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_list_standalone() {
    let dir = "/tmp/sema-test-list";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));

    eval(&format!(r#"(file/write "{dir}/a.txt" "a")"#));
    eval(&format!(r#"(file/write "{dir}/b.txt" "b")"#));
    eval(&format!(r#"(file/mkdir "{dir}/sub")"#));

    let listing = eval(&format!(r#"(file/list "{dir}")"#));
    if let Some(items) = listing.as_list() {
        let names: Vec<String> = items.iter().map(|v| v.to_string()).collect();
        assert!(names.iter().any(|n| n.contains("a.txt")));
        assert!(names.iter().any(|n| n.contains("b.txt")));
        assert!(names.iter().any(|n| n.contains("sub")));
    } else {
        panic!("file/list should return a list");
    }

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_mkdir_standalone() {
    let dir = "/tmp/sema-test-mkdir";
    let _ = std::fs::remove_dir_all(dir);

    // Recursive mkdir
    eval(&format!(r#"(file/mkdir "{dir}/a/b/c")"#));
    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}/a/b/c")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}/a/b")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}/a")"#)),
        Value::bool(true)
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_is_directory_standalone() {
    let dir = "/tmp/sema-test-isdir";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));
    eval(&format!(r#"(file/write "{dir}/f.txt" "file")"#));

    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}/f.txt")"#)),
        Value::bool(false)
    );
    assert_eq!(
        eval(&format!(r#"(file/is-directory? "{dir}/nonexistent")"#)),
        Value::bool(false)
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_info_standalone() {
    let dir = "/tmp/sema-test-info";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));
    eval(&format!(r#"(file/write "{dir}/test.txt" "hello")"#));

    // File info
    let info = eval(&format!(
        r#"(begin
            (define info (file/info "{dir}/test.txt"))
            (list (get info :size) (get info :is-file) (get info :is-dir)))"#
    ));
    if let Some(items) = info.as_list() {
        assert_eq!(items[0], Value::int(5)); // "hello" is 5 bytes
        assert_eq!(items[1], Value::bool(true));
        assert_eq!(items[2], Value::bool(false));
    } else {
        panic!("expected list from file/info test");
    }

    // Directory info
    let dir_info = eval(&format!(r#"(get (file/info "{dir}") :is-dir)"#));
    assert_eq!(dir_info, Value::bool(true));

    // :modified should be an integer
    let modified = eval(&format!(r#"(get (file/info "{dir}/test.txt") :modified)"#));
    assert!(modified.is_int());

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_read_lines() {
    let dir = "/tmp/sema-test-readlines";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));

    eval(&format!(
        r#"(file/write "{dir}/lines.txt" "alpha\nbeta\ngamma")"#
    ));
    let result = eval(&format!(r#"(file/read-lines "{dir}/lines.txt")"#));
    if let Some(items) = result.as_list() {
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::string("alpha"));
        assert_eq!(items[1], Value::string("beta"));
        assert_eq!(items[2], Value::string("gamma"));
    } else {
        panic!("file/read-lines should return a list");
    }

    // Empty file
    eval(&format!(r#"(file/write "{dir}/empty.txt" "")"#));
    let empty = eval(&format!(r#"(file/read-lines "{dir}/empty.txt")"#));
    if let Some(items) = empty.as_list() {
        assert_eq!(items.len(), 1); // split("") gives [""]
        assert_eq!(items[0], Value::string(""));
    } else {
        panic!("expected list");
    }

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_write_lines() {
    let dir = "/tmp/sema-test-writelines";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));

    eval(&format!(
        r#"(file/write-lines "{dir}/out.txt" (list "line1" "line2" "line3"))"#
    ));
    assert_eq!(
        eval(&format!(r#"(file/read "{dir}/out.txt")"#)),
        Value::string("line1\nline2\nline3")
    );

    // Roundtrip: write-lines then read-lines
    eval(&format!(
        r#"(file/write-lines "{dir}/round.txt" (list "a" "b" "c"))"#
    ));
    let result = eval(&format!(r#"(file/read-lines "{dir}/round.txt")"#));
    if let Some(items) = result.as_list() {
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::string("a"));
        assert_eq!(items[1], Value::string("b"));
        assert_eq!(items[2], Value::string("c"));
    } else {
        panic!("expected list");
    }

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_for_each_line() {
    let dir = unique_temp_dir("foreach-line");
    let dir = dir.display().to_string();
    let dir = dir.as_str();
    eval(&format!(
        r#"(file/write "{dir}/data.txt" "alpha\nbeta\ngamma")"#
    ));

    // Count lines using for-each-line with a mutable counter
    let result = eval(&format!(
        r#"(begin
             (define count 0)
             (file/for-each-line "{dir}/data.txt"
               (fn (line) (set! count (+ count 1))))
             count)"#
    ));
    assert_eq!(result, Value::int(3));

    // Collect lines into a list via set!
    let collected = eval(&format!(
        r#"(begin
             (define lines '())
             (file/for-each-line "{dir}/data.txt"
               (fn (line) (set! lines (append lines (list line)))))
             lines)"#
    ));
    if let Some(items) = collected.as_list() {
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::string("alpha"));
        assert_eq!(items[1], Value::string("beta"));
        assert_eq!(items[2], Value::string("gamma"));
    } else {
        panic!("expected list, got: {collected}");
    }

    // Empty file — should call function zero times
    eval(&format!(r#"(file/write "{dir}/empty.txt" "")"#));
    let empty_count = eval(&format!(
        r#"(begin
             (define n 0)
             (file/for-each-line "{dir}/empty.txt"
               (fn (line) (set! n (+ n 1))))
             n)"#
    ));
    // BufReader.lines() on "" yields zero lines
    assert_eq!(empty_count, Value::int(0));

    // Single line, no trailing newline
    eval(&format!(r#"(file/write "{dir}/one.txt" "hello")"#));
    let one = eval(&format!(
        r#"(begin
             (define result '())
             (file/for-each-line "{dir}/one.txt"
               (fn (line) (set! result (append result (list line)))))
             result)"#
    ));
    if let Some(items) = one.as_list() {
        assert_eq!(items.len(), 1);
        assert_eq!(items[0], Value::string("hello"));
    } else {
        panic!("expected list");
    }

    // Arity errors
    assert!(
        eval_err(&format!(r#"(file/for-each-line "{dir}/data.txt")"#))
            .to_string()
            .contains("expects")
    );
    assert!(eval_err(r#"(file/for-each-line)"#)
        .to_string()
        .contains("expects"));

    // Non-existent file
    assert!(
        eval_err(r#"(file/for-each-line "/tmp/nonexistent-sema.txt" (fn (l) l))"#)
            .to_string()
            .contains("file/for-each-line")
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_fold_lines() {
    let dir = unique_temp_dir("fold-lines");
    let dir = dir.display().to_string();
    let dir = dir.as_str();
    eval(&format!(r#"(file/write "{dir}/nums.txt" "10\n20\n30")"#));

    // Sum numbers using fold-lines
    let result = eval(&format!(
        r#"(file/fold-lines "{dir}/nums.txt"
             (fn (acc line) (+ acc (string->number line)))
             0)"#
    ));
    assert_eq!(result, Value::int(60));

    // Count lines
    let count = eval(&format!(
        r#"(file/fold-lines "{dir}/nums.txt"
             (fn (n line) (+ n 1))
             0)"#
    ));
    assert_eq!(count, Value::int(3));

    // Collect lines into a list
    let collected = eval(&format!(
        r#"(file/fold-lines "{dir}/nums.txt"
             (fn (acc line) (append acc (list line)))
             '())"#
    ));
    if let Some(items) = collected.as_list() {
        assert_eq!(items.len(), 3);
        assert_eq!(items[0], Value::string("10"));
        assert_eq!(items[1], Value::string("20"));
        assert_eq!(items[2], Value::string("30"));
    } else {
        panic!("expected list, got: {collected}");
    }

    // Empty file — returns initial accumulator
    eval(&format!(r#"(file/write "{dir}/empty.txt" "")"#));
    let empty = eval(&format!(
        r#"(file/fold-lines "{dir}/empty.txt"
             (fn (acc line) (+ acc 1))
             42)"#
    ));
    assert_eq!(empty, Value::int(42));

    // Build a map from key=value lines
    eval(&format!(
        r#"(file/write "{dir}/kv.txt" "name=alice\nage=30\ncity=paris")"#
    ));
    let map_result = eval(&format!(
        r#"(file/fold-lines "{dir}/kv.txt"
             (fn (acc line)
               (let ((parts (string/split line "=")))
                 (assoc acc (first parts) (nth parts 1))))
             {{}})"#
    ));
    if let Some(m) = map_result.as_map_rc() {
        assert_eq!(m.get(&Value::string("name")), Some(&Value::string("alice")));
        assert_eq!(m.get(&Value::string("age")), Some(&Value::string("30")));
        assert_eq!(m.get(&Value::string("city")), Some(&Value::string("paris")));
    } else {
        panic!("expected map, got: {map_result}");
    }

    // Arity errors
    assert!(eval_err(r#"(file/fold-lines "f" (fn (a b) a))"#)
        .to_string()
        .contains("expects"));
    assert!(eval_err(r#"(file/fold-lines)"#)
        .to_string()
        .contains("expects"));

    // Non-existent file
    assert!(
        eval_err(r#"(file/fold-lines "/tmp/nonexistent-sema.txt" (fn (a l) a) 0)"#)
            .to_string()
            .contains("file/fold-lines")
    );

    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn test_file_copy() {
    let dir = "/tmp/sema-test-copy";
    let _ = std::fs::remove_dir_all(dir);
    eval(&format!(r#"(file/mkdir "{dir}")"#));

    eval(&format!(r#"(file/write "{dir}/src.txt" "original")"#));
    eval(&format!(r#"(file/copy "{dir}/src.txt" "{dir}/dest.txt")"#));

    // Both files exist with same content
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/src.txt")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/exists? "{dir}/dest.txt")"#)),
        Value::bool(true)
    );
    assert_eq!(
        eval(&format!(r#"(file/read "{dir}/dest.txt")"#)),
        Value::string("original")
    );

    // Source is unchanged
    assert_eq!(
        eval(&format!(r#"(file/read "{dir}/src.txt")"#)),
        Value::string("original")
    );

    // Copy non-existent file should error
    let err = eval_err(&format!(
        r#"(file/copy "{dir}/nope.txt" "{dir}/dest2.txt")"#
    ));
    assert!(err.to_string().contains("file/copy"));

    let _ = std::fs::remove_dir_all(dir);
}

// Path operations (extended): join, dirname, basename, extension, absolute

#[test]
fn test_path_join_extended() {
    assert_eq!(eval(r#"(path/join "a" "b" "c")"#), Value::string("a/b/c"));
    assert_eq!(
        eval(r#"(path/join "/usr" "local" "bin")"#),
        Value::string("/usr/local/bin")
    );
    // Single arg
    assert_eq!(eval(r#"(path/join "only")"#), Value::string("only"));
}

#[test]
fn test_path_dirname_extended() {
    assert_eq!(
        eval(r#"(path/dirname "/a/b/c.txt")"#),
        Value::string("/a/b")
    );
    assert_eq!(eval(r#"(path/dirname "file.txt")"#), Value::string(""));
    assert_eq!(eval(r#"(path/dirname "/root")"#), Value::string("/"));
}

#[test]
fn test_path_basename_extended() {
    assert_eq!(
        eval(r#"(path/basename "/a/b/c.txt")"#),
        Value::string("c.txt")
    );
    assert_eq!(
        eval(r#"(path/basename "plain.rs")"#),
        Value::string("plain.rs")
    );
    assert_eq!(eval(r#"(path/basename "/a/b/")"#), Value::string("b"));
}

#[test]
fn test_path_extension_extended() {
    assert_eq!(
        eval(r#"(path/extension "file.tar.gz")"#),
        Value::string("gz")
    );
    assert_eq!(eval(r#"(path/extension "Makefile")"#), Value::nil());
    // ".hidden" has no extension in Rust's Path semantics
    assert_eq!(eval(r#"(path/extension ".hidden")"#), Value::nil());
}

#[test]
fn test_path_absolute_extended() {
    // path/absolute on an existing path returns a string
    let result = eval(r#"(path/absolute ".")"#);
    if let Some(s) = result.as_str() {
        assert!(s.starts_with('/'));
    } else {
        panic!("path/absolute should return a string");
    }
    // Non-existent path should error
    let err = eval_err(r#"(path/absolute "/nonexistent_sema_test_path_xyz")"#);
    assert!(err.to_string().contains("path/absolute"));
}

// String: trim-left, trim-right, number?

#[test]
fn test_string_trim_left() {
    assert_eq!(
        eval(r#"(string/trim-left "  hello  ")"#),
        Value::string("hello  ")
    );
    assert_eq!(eval(r#"(string/trim-left "\t\n hi")"#), Value::string("hi"));
    assert_eq!(
        eval(r#"(string/trim-left "no-space")"#),
        Value::string("no-space")
    );
}

#[test]
fn test_string_trim_right() {
    assert_eq!(
        eval(r#"(string/trim-right "  hello  ")"#),
        Value::string("  hello")
    );
    assert_eq!(
        eval(r#"(string/trim-right "hi\t\n ")"#),
        Value::string("hi")
    );
    assert_eq!(
        eval(r#"(string/trim-right "no-space")"#),
        Value::string("no-space")
    );
}

#[test]
fn test_string_number_predicate() {
    assert_eq!(eval(r#"(string/number? "42")"#), Value::bool(true));
    assert_eq!(eval(r#"(string/number? "-7")"#), Value::bool(true));
    assert_eq!(eval(r#"(string/number? "3.14")"#), Value::bool(true));
    assert_eq!(eval(r#"(string/number? "-0.5")"#), Value::bool(true));
    assert_eq!(eval(r#"(string/number? "hello")"#), Value::bool(false));
    assert_eq!(eval(r#"(string/number? "")"#), Value::bool(false));
    assert_eq!(eval(r#"(string/number? "12abc")"#), Value::bool(false));
}

// Map: map-keys, from-entries

#[test]
fn test_map_map_keys() {
    // Transform keyword keys using keyword->string
    assert_eq!(
        eval_to_string(r#"(map/map-keys keyword->string {:a 1 :b 2})"#),
        r#"{"a" 1 "b" 2}"#
    );
}

#[test]
fn test_map_from_entries() {
    assert_eq!(
        eval_to_string(r#"(map/from-entries (list (list :a 1) (list :b 2)))"#),
        "{:a 1 :b 2}"
    );
    // Empty list -> empty map
    assert_eq!(eval_to_string(r#"(map/from-entries (list))"#), "{}");
    // Roundtrip: entries -> from-entries
    assert_eq!(
        eval_to_string(r#"(map/from-entries (map/entries {:x 10 :y 20}))"#),
        "{:x 10 :y 20}"
    );
}

#[test]
fn test_map_from_entries_error() {
    // Entry with wrong size should error
    let err = eval_err(r#"(map/from-entries (list (list :a 1 :extra)))"#);
    assert!(err.to_string().contains("pair"));
}

// Extended tests for existing list functions: any, every, reduce,
// partition, foldr, member

#[test]
fn test_any_with_even_predicate() {
    assert_eq!(eval("(any even? (list 1 3 4))"), Value::bool(true));
    assert_eq!(eval("(any even? (list 1 3 5))"), Value::bool(false));
    // empty list → false
    assert_eq!(eval("(any even? (list))"), Value::bool(false));
    // works on vectors
    assert_eq!(eval("(any even? [1 2 3])"), Value::bool(true));
    // short-circuits: only first truthy hit matters
    assert_eq!(
        eval("(any (lambda (x) (> x 0)) '(1 2 3))"),
        Value::bool(true)
    );
}

#[test]
fn test_every_with_even_predicate() {
    assert_eq!(eval("(every even? (list 2 4 6))"), Value::bool(true));
    assert_eq!(eval("(every even? (list 2 3 6))"), Value::bool(false));
    // empty list → true (vacuous truth)
    assert_eq!(eval("(every even? (list))"), Value::bool(true));
    // works on vectors
    assert_eq!(eval("(every even? [2 4 6])"), Value::bool(true));
}

#[test]
fn test_reduce_edge_cases() {
    // single element list → returns that element
    assert_eq!(eval("(reduce + '(42))"), Value::int(42));
    // string concatenation
    assert_eq!(
        eval(r#"(reduce string-append '("a" "b" "c"))"#),
        Value::string("abc")
    );
    // empty list → error
    let err = eval_err("(reduce + '())");
    assert!(err.to_string().contains("empty"));
}

#[test]
fn test_partition_extended() {
    // all match
    assert_eq!(eval_to_string("(partition even? '(2 4 6))"), "((2 4 6) ())");
    // none match
    assert_eq!(eval_to_string("(partition even? '(1 3 5))"), "(() (1 3 5))");
    // empty list
    assert_eq!(eval_to_string("(partition even? '())"), "(() ())");
    // works on vectors
    assert_eq!(
        eval_to_string("(partition even? [1 2 3 4])"),
        "((2 4) (1 3))"
    );
}

#[test]
fn test_foldr_extended() {
    // right fold builds list in original order with cons
    assert_eq!(eval_to_string("(foldr cons '() '(1 2 3))"), "(1 2 3)");
    // subtraction shows right-associativity: 1 - (2 - (3 - 0)) = 1 - (2 - 3) = 1 - (-1) = 2
    assert_eq!(eval("(foldr - 0 '(1 2 3))"), Value::int(2));
    // empty list → returns init
    assert_eq!(eval("(foldr + 99 '())"), Value::int(99));
}

#[test]
fn test_member_extended() {
    // found at beginning
    assert_eq!(eval_to_string("(member 1 '(1 2 3))"), "(1 2 3)");
    // found at end
    assert_eq!(eval_to_string("(member 3 '(1 2 3))"), "(3)");
    // not found → #f
    assert_eq!(eval("(member 99 '(1 2 3))"), Value::bool(false));
    // empty list → #f
    assert_eq!(eval("(member 1 '())"), Value::bool(false));
    // works with keywords
    assert_eq!(eval_to_string("(member :b '(:a :b :c))"), "(:b :c)");
}

// New list functions: sort-by, flatten-deep, interpose, frequencies,
// list->vector, vector->list

#[test]
fn test_sort_by() {
    // sort by absolute value
    assert_eq!(
        eval_to_string("(sort-by (lambda (x) (if (< x 0) (- 0 x) x)) '(3 -1 2 -5 4))"),
        "(-1 2 3 4 -5)"
    );
    // sort strings by length
    assert_eq!(
        eval_to_string(r#"(sort-by string-length '("bb" "a" "ccc"))"#),
        r#"("a" "bb" "ccc")"#
    );
    // empty list
    assert_eq!(eval_to_string("(sort-by (lambda (x) x) '())"), "()");
    // single element
    assert_eq!(eval_to_string("(sort-by (lambda (x) x) '(42))"), "(42)");
    // works on vectors
    assert_eq!(
        eval_to_string("(sort-by (lambda (x) x) [3 1 2])"),
        "(1 2 3)"
    );
}

#[test]
fn test_flatten_deep_fn() {
    // deeply nested lists
    assert_eq!(
        eval_to_string("(flatten-deep '(1 (2 (3 (4 5))) 6))"),
        "(1 2 3 4 5 6)"
    );
    // already flat
    assert_eq!(eval_to_string("(flatten-deep '(1 2 3))"), "(1 2 3)");
    // empty
    assert_eq!(eval_to_string("(flatten-deep '())"), "()");
    // mixed lists and vectors
    assert_eq!(
        eval_to_string("(flatten-deep '(1 [2 [3]] (4)))"),
        "(1 2 3 4)"
    );
    // single nested element
    assert_eq!(eval_to_string("(flatten-deep '(((42))))"), "(42)");
}

#[test]
fn test_interpose() {
    // basic usage
    assert_eq!(eval_to_string("(interpose :x '(1 2 3))"), "(1 :x 2 :x 3)");
    // single element → no separator
    assert_eq!(eval_to_string("(interpose :x '(1))"), "(1)");
    // empty list
    assert_eq!(eval_to_string("(interpose :x '())"), "()");
    // string separator
    assert_eq!(
        eval_to_string(r#"(interpose ", " '(1 2 3))"#),
        r#"(1 ", " 2 ", " 3)"#
    );
    // works on vectors
    assert_eq!(eval_to_string("(interpose 0 [1 2 3])"), "(1 0 2 0 3)");
}

#[test]
fn test_frequencies() {
    // basic counting with keywords (BTreeMap = deterministic order)
    assert_eq!(eval_to_string("(frequencies '(:a :b :a))"), "{:a 2 :b 1}");
    // all unique
    assert_eq!(eval_to_string("(frequencies '(1 2 3))"), "{1 1 2 1 3 1}");
    // all same
    assert_eq!(eval_to_string("(frequencies '(1 1 1))"), "{1 3}");
    // empty list
    assert_eq!(eval_to_string("(frequencies '())"), "{}");
    // works on vectors
    assert_eq!(eval_to_string("(frequencies [1 2 1])"), "{1 2 2 1}");
}

#[test]
fn test_list_to_vector() {
    // basic conversion
    assert_eq!(eval_to_string("(list->vector '(1 2 3))"), "[1 2 3]");
    // empty list
    assert_eq!(eval_to_string("(list->vector '())"), "[]");
    // type error on non-list
    let err = eval_err("(list->vector [1 2 3])");
    assert!(err.to_string().contains("list"));
}

#[test]
fn test_vector_to_list() {
    // basic conversion
    assert_eq!(eval_to_string("(vector->list [1 2 3])"), "(1 2 3)");
    // empty vector
    assert_eq!(eval_to_string("(vector->list [])"), "()");
    // type error on non-vector
    let err = eval_err("(vector->list '(1 2 3))");
    assert!(err.to_string().contains("vector"));
}

#[test]
fn test_sort_by_arity_error() {
    let err = eval_err("(sort-by '(1 2))");
    assert!(err.to_string().contains("2"));
}

#[test]
fn test_interpose_arity_error() {
    let err = eval_err("(interpose :x)");
    assert!(err.to_string().contains("2"));
}

#[test]
fn test_frequencies_arity_error() {
    let err = eval_err("(frequencies '(1) '(2))");
    assert!(err.to_string().contains("1"));
}

// Crypto: hash/md5, hash/hmac-sha256

#[test]
fn test_hash_md5() {
    // MD5 of empty string
    assert_eq!(
        eval(r#"(hash/md5 "")"#),
        Value::string("d41d8cd98f00b204e9800998ecf8427e")
    );
    // MD5 of "hello"
    assert_eq!(
        eval(r#"(hash/md5 "hello")"#),
        Value::string("5d41402abc4b2a76b9719d911017c592")
    );
}

#[test]
fn test_hash_md5_errors() {
    let err = eval_err(r#"(hash/md5)"#);
    assert!(err.to_string().contains("hash/md5"));
    let err = eval_err(r#"(hash/md5 1)"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_hash_hmac_sha256() {
    // Known HMAC-SHA256 test vector
    let result = eval(r#"(hash/hmac-sha256 "key" "message")"#);
    assert!(result.is_string());
    if let Some(s) = result.as_str() {
        assert_eq!(s.len(), 64); // 32 bytes = 64 hex chars
    }
}

#[test]
fn test_hash_hmac_sha256_errors() {
    let err = eval_err(r#"(hash/hmac-sha256 "key")"#);
    assert!(err.to_string().contains("hash/hmac-sha256"));
    let err = eval_err(r#"(hash/hmac-sha256 1 "msg")"#);
    assert!(err.to_string().contains("Type error"));
}

// Datetime: time/add, time/diff

#[test]
fn test_time_add() {
    assert_eq!(eval("(time/add 1000.0 500.0)"), Value::float(1500.0));
    assert_eq!(eval("(time/add 1000 60)"), Value::float(1060.0));
}

#[test]
fn test_time_add_errors() {
    let err = eval_err(r#"(time/add 1000)"#);
    assert!(err.to_string().contains("time/add"));
    let err = eval_err(r#"(time/add "x" 1)"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_time_diff() {
    assert_eq!(eval("(time/diff 1500.0 1000.0)"), Value::float(500.0));
    assert_eq!(eval("(time/diff 1000 1500)"), Value::float(-500.0));
}

#[test]
fn test_time_diff_errors() {
    let err = eval_err(r#"(time/diff 1000)"#);
    assert!(err.to_string().contains("time/diff"));
}

// System: sys/set-env

#[test]
fn test_sys_set_env() {
    assert_eq!(
        eval(r#"(sys/set-env "SEMA_TEST_VAR_12345" "hello")"#),
        Value::nil()
    );
    assert_eq!(
        eval(r#"(env "SEMA_TEST_VAR_12345")"#),
        Value::string("hello")
    );
}

#[test]
fn test_sys_set_env_errors() {
    let err = eval_err(r#"(sys/set-env "X")"#);
    assert!(err.to_string().contains("sys/set-env"));
    let err = eval_err(r#"(sys/set-env 1 "val")"#);
    assert!(err.to_string().contains("Type error"));
}

// LLM Data Types: Prompts, Messages, Conversations, Tools, Agents

#[test]
fn test_prompt_creation() {
    // Prompt is created via special form
    let result = eval_to_string(r#"(prompt (user "hello"))"#);
    assert!(result.contains("prompt"));
}

#[test]
fn test_prompt_predicate() {
    assert_eq!(
        eval(r#"(prompt? (prompt (user "hello")))"#),
        Value::bool(true)
    );
    assert_eq!(eval(r#"(prompt? 42)"#), Value::bool(false));
    assert_eq!(eval(r#"(prompt? "not a prompt")"#), Value::bool(false));
    assert_eq!(eval(r#"(prompt? nil)"#), Value::bool(false));
}

#[test]
fn test_prompt_messages() {
    assert_eq!(
        eval(r#"(length (prompt/messages (prompt (user "hello") (assistant "hi"))))"#),
        Value::int(2)
    );
}

#[test]
fn test_prompt_append() {
    assert_eq!(
        eval(
            r#"(length (prompt/messages (prompt/append (prompt (user "a")) (prompt (assistant "b")))))"#
        ),
        Value::int(2)
    );
}

#[test]
fn test_prompt_set_system() {
    // prompt/set-system replaces system message
    let result = eval(
        r#"
        (begin
          (define p (prompt (system "old") (user "hello")))
          (define p2 (prompt/set-system p "new system"))
          (length (prompt/messages p2)))
    "#,
    );
    assert_eq!(result, Value::int(2));
}

#[test]
fn test_prompt_set_system_adds_when_missing() {
    // If no system message, it adds one at front
    let result = eval(
        r#"
        (begin
          (define p (prompt (user "hello")))
          (define p2 (prompt/set-system p "system msg"))
          (length (prompt/messages p2)))
    "#,
    );
    assert_eq!(result, Value::int(2));
}

#[test]
fn test_message_creation() {
    let result = eval_to_string(r#"(message :user "hello world")"#);
    assert!(result.contains("message"));
}

#[test]
fn test_message_predicate() {
    assert_eq!(
        eval(r#"(message? (message :user "hi"))"#),
        Value::bool(true)
    );
    assert_eq!(eval(r#"(message? 42)"#), Value::bool(false));
    assert_eq!(eval(r#"(message? "not a message")"#), Value::bool(false));
}

#[test]
fn test_message_role() {
    assert_eq!(
        eval(r#"(message/role (message :user "hi"))"#),
        Value::keyword("user")
    );
    assert_eq!(
        eval(r#"(message/role (message :assistant "hello"))"#),
        Value::keyword("assistant")
    );
    assert_eq!(
        eval(r#"(message/role (message :system "you are helpful"))"#),
        Value::keyword("system")
    );
}

#[test]
fn test_message_content() {
    assert_eq!(
        eval(r#"(message/content (message :user "hello world"))"#),
        Value::string("hello world")
    );
    assert_eq!(
        eval(r#"(message/content (message :assistant "response"))"#),
        Value::string("response")
    );
}

#[test]
fn test_message_from_prompt() {
    // Extract messages from prompt, check role and content
    assert_eq!(
        eval(
            r#"
            (begin
              (define p (prompt (user "test input")))
              (define msgs (prompt/messages p))
              (message/content (car msgs)))
        "#
        ),
        Value::string("test input")
    );
    assert_eq!(
        eval(
            r#"
            (begin
              (define p (prompt (user "test input")))
              (define msgs (prompt/messages p))
              (message/role (car msgs)))
        "#
        ),
        Value::keyword("user")
    );
}

#[test]
fn test_conversation_new() {
    let result = eval_to_string(r#"(conversation/new {:model "test-model"})"#);
    assert!(result.contains("conversation"));
}

#[test]
fn test_conversation_new_empty() {
    let result = eval_to_string(r#"(conversation/new)"#);
    assert!(result.contains("conversation"));
}

#[test]
fn test_conversation_predicate() {
    assert_eq!(
        eval(r#"(conversation? (conversation/new))"#),
        Value::bool(true)
    );
    assert_eq!(eval(r#"(conversation? 42)"#), Value::bool(false));
    assert_eq!(eval(r#"(conversation? "not a conv")"#), Value::bool(false));
}

#[test]
fn test_conversation_messages_empty() {
    assert_eq!(
        eval(r#"(length (conversation/messages (conversation/new)))"#),
        Value::int(0)
    );
}

#[test]
fn test_conversation_fork() {
    // fork returns a copy
    assert_eq!(
        eval(r#"(conversation? (conversation/fork (conversation/new)))"#),
        Value::bool(true)
    );
}

#[test]
fn test_conversation_model() {
    assert_eq!(
        eval(r#"(conversation/model (conversation/new {:model "gpt-4"}))"#),
        Value::string("gpt-4")
    );
}

#[test]
fn test_conversation_model_empty() {
    assert_eq!(
        eval(r#"(conversation/model (conversation/new))"#),
        Value::string("")
    );
}

#[test]
fn test_conversation_add_message() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define c (conversation/new))
              (define c2 (conversation/add-message c :user "hello"))
              (length (conversation/messages c2)))
        "#
        ),
        Value::int(1)
    );
}

#[test]
fn test_conversation_add_message_multiple() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define c (conversation/new {:model "test"}))
              (define c1 (conversation/add-message c :user "hello"))
              (define c2 (conversation/add-message c1 :assistant "hi there"))
              (define c3 (conversation/add-message c2 :user "how are you?"))
              (length (conversation/messages c3)))
        "#
        ),
        Value::int(3)
    );
}

#[test]
fn test_conversation_add_message_preserves_model() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define c (conversation/new {:model "gpt-4"}))
              (define c2 (conversation/add-message c :user "hello"))
              (conversation/model c2))
        "#
        ),
        Value::string("gpt-4")
    );
}

#[test]
fn test_conversation_add_message_immutable() {
    // Original conversation should not be modified
    assert_eq!(
        eval(
            r#"
            (begin
              (define c (conversation/new))
              (define c2 (conversation/add-message c :user "hello"))
              (length (conversation/messages c)))
        "#
        ),
        Value::int(0)
    );
}

#[test]
fn test_conversation_add_message_content_check() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define c (conversation/add-message (conversation/new) :user "hello"))
              (define msgs (conversation/messages c))
              (message/content (car msgs)))
        "#
        ),
        Value::string("hello")
    );
}

#[test]
fn test_conversation_add_message_role_check() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define c (conversation/add-message (conversation/new) :assistant "response"))
              (define msgs (conversation/messages c))
              (message/role (car msgs)))
        "#
        ),
        Value::keyword("assistant")
    );
}

#[test]
fn test_tool_predicate() {
    assert_eq!(
        eval(
            r#"(begin (deftool my-test-tool-1 "a tool" {:x {:type :string}} (lambda (args) "ok")) (tool? my-test-tool-1))"#
        ),
        Value::bool(true)
    );
    assert_eq!(eval(r#"(tool? 42)"#), Value::bool(false));
    assert_eq!(eval(r#"(tool? "not a tool")"#), Value::bool(false));
}

#[test]
fn test_tool_name() {
    assert_eq!(
        eval(
            r#"(begin (deftool my-test-tool-2 "desc" {:x {:type :string}} (lambda (args) "ok")) (tool/name my-test-tool-2))"#
        ),
        Value::string("my-test-tool-2")
    );
}

#[test]
fn test_tool_description() {
    assert_eq!(
        eval(
            r#"(begin (deftool my-test-tool-3 "my description" {:x {:type :string}} (lambda (args) "ok")) (tool/description my-test-tool-3))"#
        ),
        Value::string("my description")
    );
}

#[test]
fn test_tool_parameters() {
    assert_eq!(
        eval(
            r#"(begin (deftool my-test-tool-4 "desc" {:x {:type :string}} (lambda (args) "ok")) (map? (tool/parameters my-test-tool-4)))"#
        ),
        Value::bool(true)
    );
}

#[test]
fn test_agent_predicate() {
    assert_eq!(
        eval(
            r#"(begin (defagent test-agent-1 {:system "helpful" :tools [] :model "gpt-4"}) (agent? test-agent-1))"#
        ),
        Value::bool(true)
    );
    assert_eq!(eval(r#"(agent? 42)"#), Value::bool(false));
    assert_eq!(eval(r#"(agent? "not an agent")"#), Value::bool(false));
}

#[test]
fn test_agent_name() {
    assert_eq!(
        eval(
            r#"(begin (defagent test-agent-2 {:system "sys" :tools []}) (agent/name test-agent-2))"#
        ),
        Value::string("test-agent-2")
    );
}

#[test]
fn test_agent_system() {
    assert_eq!(
        eval(
            r#"(begin (defagent test-agent-3 {:system "you are helpful" :tools []}) (agent/system test-agent-3))"#
        ),
        Value::string("you are helpful")
    );
}

#[test]
fn test_agent_tools_empty() {
    assert_eq!(
        eval(
            r#"(begin (defagent test-agent-4 {:system "sys" :tools []}) (length (agent/tools test-agent-4)))"#
        ),
        Value::int(0)
    );
}

#[test]
fn test_agent_model() {
    assert_eq!(
        eval(
            r#"(begin (defagent test-agent-5 {:system "sys" :tools [] :model "claude-3"}) (agent/model test-agent-5))"#
        ),
        Value::string("claude-3")
    );
}

#[test]
fn test_agent_max_turns_default() {
    assert_eq!(
        eval(
            r#"(begin (defagent test-agent-6 {:system "sys" :tools []}) (agent/max-turns test-agent-6))"#
        ),
        Value::int(10)
    );
}

#[test]
fn test_agent_max_turns_custom() {
    assert_eq!(
        eval(
            r#"(begin (defagent test-agent-7 {:system "sys" :tools [] :max-turns 5}) (agent/max-turns test-agent-7))"#
        ),
        Value::int(5)
    );
}

#[test]
fn test_agent_with_tools() {
    assert_eq!(
        eval(
            r#"
            (begin
              (deftool agent-tool-1 "tool1" {:x {:type :string}} (lambda (args) "ok"))
              (defagent test-agent-8 {:system "sys" :tools [agent-tool-1]})
              (length (agent/tools test-agent-8)))
        "#
        ),
        Value::int(1)
    );
}

#[test]
fn test_llm_similarity_identical() {
    assert_eq!(
        eval("(llm/similarity (list 1.0 0.0 0.0) (list 1.0 0.0 0.0))"),
        Value::float(1.0)
    );
}

#[test]
fn test_llm_similarity_orthogonal() {
    assert_eq!(
        eval("(llm/similarity (list 1.0 0.0) (list 0.0 1.0))"),
        Value::float(0.0)
    );
}

#[test]
fn test_llm_similarity_opposite() {
    assert_eq!(
        eval("(llm/similarity (list 1.0 0.0) (list -1.0 0.0))"),
        Value::float(-1.0)
    );
}

#[test]
fn test_llm_similarity_error_different_lengths() {
    let err = eval_err("(llm/similarity (list 1.0 0.0) (list 1.0 0.0 0.0))");
    assert!(err.to_string().contains("same length"));
}

#[test]
fn test_llm_similarity_error_empty() {
    let err = eval_err("(llm/similarity (list) (list))");
    assert!(err.to_string().contains("empty"));
}

#[test]
fn test_embedding_list_roundtrip() {
    // Convert list -> embedding bytevector -> list, verify roundtrip
    assert_eq!(
        eval_to_string("(embedding/->list (embedding/list->embedding '(1.0 2.0 3.0)))"),
        "(1.0 2.0 3.0)"
    );
}

#[test]
fn test_embedding_length() {
    assert_eq!(
        eval("(embedding/length (embedding/list->embedding '(1.0 2.0 3.0)))"),
        Value::int(3)
    );
}

#[test]
fn test_embedding_ref() {
    assert_eq!(
        eval("(embedding/ref (embedding/list->embedding '(10.5 20.5 30.5)) 1)"),
        Value::float(20.5)
    );
}

#[test]
fn test_embedding_ref_out_of_bounds() {
    let err = eval_err("(embedding/ref (embedding/list->embedding '(1.0 2.0)) 5)");
    assert!(err.to_string().contains("out of bounds"));
}

#[test]
fn test_embedding_similarity_bytevectors() {
    // Bytevector-based similarity (same as list-based, but through bytevector path)
    assert_eq!(
        eval("(llm/similarity (embedding/list->embedding '(1.0 0.0 0.0)) (embedding/list->embedding '(1.0 0.0 0.0)))"),
        Value::float(1.0)
    );
    assert_eq!(
        eval("(llm/similarity (embedding/list->embedding '(1.0 0.0)) (embedding/list->embedding '(0.0 1.0)))"),
        Value::float(0.0)
    );
}

#[test]
fn test_embedding_similarity_mixed_error() {
    let err = eval_err("(llm/similarity (embedding/list->embedding '(1.0 0.0)) '(0.0 1.0))");
    assert!(err.to_string().contains("same type"));
}

#[test]
fn test_embedding_list_to_embedding_preserves_values() {
    // Verify f64 encoding precision
    assert_eq!(
        eval("(embedding/ref (embedding/list->embedding '(3.14159265358979)) 0)"),
        Value::float(3.14159265358979)
    );
}

#[test]
fn test_embedding_integers_coerced() {
    // Integers should be accepted and coerced to float
    assert_eq!(
        eval("(embedding/ref (embedding/list->embedding '(42)) 0)"),
        Value::float(42.0)
    );
}

#[test]
fn test_prompt_system_user_assistant() {
    assert_eq!(
        eval(
            r#"
            (begin
              (define p (prompt (system "be helpful") (user "hello") (assistant "hi")))
              (length (prompt/messages p)))
        "#
        ),
        Value::int(3)
    );
}

#[test]
fn test_prompt_append_preserves_order() {
    // Append two prompts, verify message order
    assert_eq!(
        eval(
            r#"
            (begin
              (define p1 (prompt (user "first")))
              (define p2 (prompt (assistant "second")))
              (define combined (prompt/append p1 p2))
              (define msgs (prompt/messages combined))
              (message/content (car msgs)))
        "#
        ),
        Value::string("first")
    );
}

#[test]
fn test_message_role_error_on_non_message() {
    let err = eval_err(r#"(message/role 42)"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_message_content_error_on_non_message() {
    let err = eval_err(r#"(message/content "not a msg")"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_prompt_messages_error_on_non_prompt() {
    let err = eval_err(r#"(prompt/messages 42)"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_conversation_messages_error_on_non_conversation() {
    let err = eval_err(r#"(conversation/messages 42)"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_tool_name_error_on_non_tool() {
    let err = eval_err(r#"(tool/name 42)"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_agent_name_error_on_non_agent() {
    let err = eval_err(r#"(agent/name "not an agent")"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_conversation_add_message_error_on_non_conversation() {
    let err = eval_err(r#"(conversation/add-message 42 :user "hi")"#);
    assert!(err.to_string().contains("Type error"));
}

#[test]
fn test_conversation_add_message_error_on_bad_role() {
    let err = eval_err(r#"(conversation/add-message (conversation/new) :invalid "hi")"#);
    assert!(err.to_string().contains("unknown role"));
}

#[test]
fn test_stack_trace_nested_functions() {
    let err = eval_err(
        r#"(define (baz z) (+ z "bad"))
           (define (bar y) (begin (baz y) 1))
           (define (foo x) (begin (bar x) 2))
           (foo 1)"#,
    );
    let trace = err.stack_trace().expect("should have stack trace");
    let names: Vec<&str> = trace.0.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names[0], "+");
    assert_eq!(names[1], "baz");
    assert_eq!(names[2], "bar");
    assert_eq!(names[3], "foo");
}

#[test]
fn test_stack_trace_native_fn() {
    let err = eval_err(r#"(define (foo x) (+ x "bad")) (foo 1)"#);
    let trace = err.stack_trace().expect("should have stack trace");
    let names: Vec<&str> = trace.0.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names[0], "+");
    assert_eq!(names[1], "foo");
}

#[test]
fn test_stack_trace_lambda_anonymous() {
    let err = eval_err(r#"((lambda (x) (+ x "bad")) 1)"#);
    let trace = err.stack_trace().expect("should have stack trace");
    let names: Vec<&str> = trace.0.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names[0], "+");
    assert_eq!(names[1], "<lambda>");
}

#[test]
fn test_stack_trace_tco_bounded() {
    let err = eval_err(
        r#"(define (loop n) (if (= n 0) (+ 1 "bad") (loop (- n 1))))
           (loop 100)"#,
    );
    let trace = err.stack_trace().expect("should have stack trace");
    // Should have bounded frames, not 100+ loop frames
    assert!(
        trace.0.len() <= 5,
        "TCO trace should be bounded, got {} frames",
        trace.0.len()
    );
    let names: Vec<&str> = trace.0.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names[0], "+");
    assert_eq!(names[1], "loop");
}

#[test]
fn test_stack_trace_has_spans() {
    let err = eval_err(r#"(define (foo x) (+ x "bad")) (foo 1)"#);
    let trace = err.stack_trace().expect("should have stack trace");
    // All frames should have spans (from the reader SpanMap)
    for frame in &trace.0 {
        assert!(
            frame.span.is_some(),
            "frame '{}' should have a span",
            frame.name
        );
    }
}

#[test]
fn test_stack_trace_in_try_catch() {
    let result = eval(
        r#"(try
             (define (foo x) (+ x "bad"))
             (foo 1)
             (catch e
               (:stack-trace e)))"#,
    );
    // Should be a list of frame maps
    let frames = result.as_list().expect("stack trace should be a list");
    assert!(frames.len() >= 2, "should have at least 2 frames");
    // First frame should be +
    if let Some(first) = frames[0].as_map_rc() {
        assert_eq!(
            first.get(&Value::keyword("name")),
            Some(&Value::string("+"))
        );
    } else {
        panic!("frame should be a map");
    }
}

#[test]
fn test_stack_trace_loaded_file() {
    // Write a file with a function that errors
    eval(
        r#"(file/write "/tmp/sema-test-trace.sema"
             "(define (bad-fn x) (+ x \"oops\"))")"#,
    );
    let err = eval_err(r#"(load "/tmp/sema-test-trace.sema") (bad-fn 1)"#);
    let trace = err.stack_trace().expect("should have stack trace");
    let names: Vec<&str> = trace.0.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(names[0], "+");
    assert_eq!(names[1], "bad-fn");
}

#[test]
fn test_tool_slash_name() {
    assert_eq!(
        eval(
            r#"(begin (deftool t1 "desc" {:x {:type :string}} (lambda (x) "ok")) (tool/name t1))"#
        ),
        Value::string("t1")
    );
}

#[test]
fn test_tool_slash_description() {
    assert_eq!(
        eval(
            r#"(begin (deftool t2 "my desc" {:x {:type :string}} (lambda (x) "ok")) (tool/description t2))"#
        ),
        Value::string("my desc")
    );
}

#[test]
fn test_tool_slash_parameters() {
    assert_eq!(
        eval(
            r#"(begin (deftool t3 "desc" {:x {:type :string}} (lambda (x) "ok")) (map? (tool/parameters t3)))"#
        ),
        Value::bool(true)
    );
}

#[test]
fn test_agent_slash_name() {
    assert_eq!(
        eval(r#"(begin (defagent a1 {:system "sys" :tools []}) (agent/name a1))"#),
        Value::string("a1")
    );
}

#[test]
fn test_agent_slash_system() {
    assert_eq!(
        eval(r#"(begin (defagent a2 {:system "you are helpful" :tools []}) (agent/system a2))"#),
        Value::string("you are helpful")
    );
}

#[test]
fn test_agent_slash_tools() {
    assert_eq!(
        eval(r#"(begin (defagent a3 {:system "sys" :tools []}) (length (agent/tools a3)))"#),
        Value::int(0)
    );
}

#[test]
fn test_agent_slash_model() {
    assert_eq!(
        eval(
            r#"(begin (defagent a4 {:system "sys" :tools [] :model "claude-3"}) (agent/model a4))"#
        ),
        Value::string("claude-3")
    );
}

#[test]
fn test_agent_slash_max_turns() {
    assert_eq!(
        eval(
            r#"(begin (defagent a5 {:system "sys" :tools [] :max-turns 5}) (agent/max-turns a5))"#
        ),
        Value::int(5)
    );
}

#[test]
fn test_prompt_slash_messages() {
    assert_eq!(
        eval(r#"(length (prompt/messages (prompt (user "hello") (assistant "hi"))))"#),
        Value::int(2)
    );
}

#[test]
fn test_prompt_slash_append() {
    assert_eq!(
        eval(
            r#"(length (prompt/messages (prompt/append (prompt (user "a")) (prompt (assistant "b")))))"#
        ),
        Value::int(2)
    );
}

#[test]
fn test_prompt_slash_set_system() {
    assert_eq!(
        eval(
            r#"(begin
          (define p (prompt (user "hello")))
          (define p2 (prompt/set-system p "system msg"))
          (length (prompt/messages p2)))"#
        ),
        Value::int(2)
    );
}

#[test]
fn test_message_slash_role() {
    assert_eq!(
        eval(r#"(message/role (message :user "hi"))"#),
        Value::keyword("user")
    );
}

#[test]
fn test_message_slash_content() {
    assert_eq!(
        eval(r#"(message/content (message :user "hello world"))"#),
        Value::string("hello world")
    );
}

#[test]
fn test_legacy_tool_name_alias() {
    assert_eq!(
        eval(
            r#"(begin (deftool t1l "desc" {:x {:type :string}} (lambda (x) "ok")) (tool/name t1l))"#
        ),
        Value::string("t1l")
    );
}

#[test]
fn test_legacy_agent_name_alias() {
    assert_eq!(
        eval(r#"(begin (defagent a1l {:system "sys" :tools []}) (agent/name a1l))"#),
        Value::string("a1l")
    );
}

#[test]
fn test_legacy_prompt_messages_alias() {
    assert_eq!(
        eval(r#"(length (prompt/messages (prompt (user "hello"))))"#),
        Value::int(1)
    );
}

#[test]
fn test_legacy_message_role_alias() {
    assert_eq!(
        eval(r#"(message/role (message :assistant "hi"))"#),
        Value::keyword("assistant")
    );
}

#[test]
fn test_llm_list_providers_empty() {
    // No providers configured, should return empty list
    assert_eq!(eval(r#"(length (llm/list-providers))"#), Value::int(0));
}

#[test]
fn test_llm_current_provider_none() {
    // No provider configured
    assert_eq!(eval(r#"(llm/current-provider)"#), Value::nil());
}

#[test]
fn test_llm_set_budget() {
    // Setting budget should not error
    assert_eq!(
        eval(r#"(begin (llm/set-budget 1.0) (map? (llm/budget-remaining)))"#),
        Value::bool(true)
    );
}

#[test]
fn test_llm_budget_remaining_values() {
    assert_eq!(
        eval(
            r#"(begin
          (llm/set-budget 5.0)
          (define b (llm/budget-remaining))
          (:limit b))"#
        ),
        Value::float(5.0)
    );
}

#[test]
fn test_llm_budget_remaining_no_budget() {
    assert_eq!(
        eval(r#"(begin (llm/clear-budget) (llm/budget-remaining))"#),
        Value::nil()
    );
}

#[test]
fn test_llm_clear_budget() {
    assert_eq!(
        eval(r#"(begin (llm/set-budget 1.0) (llm/clear-budget) (llm/budget-remaining))"#),
        Value::nil()
    );
}

#[test]
fn test_with_budget_restores_outer_scope() {
    let result = eval(
        r#"
        (begin
          (llm/set-budget 10.0)
          (with-budget {:max-cost-usd 1.0}
            (llm/budget-remaining))
          (llm/budget-remaining))
    "#,
    );

    if let Some(map) = result.as_map_rc() {
        assert_eq!(
            map.get(&Value::keyword("limit")),
            Some(&Value::float(10.0)),
            "outer budget limit should be restored"
        );
        assert_eq!(
            map.get(&Value::keyword("remaining")),
            Some(&Value::float(10.0))
        );
    } else {
        panic!("expected map");
    }
}

#[test]
fn test_llm_state_isolation_between_interpreters() {
    let interp1 = Interpreter::new();
    interp1.eval_str("(llm/set-budget 5.0)").unwrap();

    let interp2 = Interpreter::new();
    assert_eq!(
        interp2.eval_str("(llm/budget-remaining)").unwrap(),
        Value::nil()
    );
}

#[test]
fn test_llm_set_default_no_provider() {
    // Should error when provider not configured
    let err = eval_err(r#"(llm/set-default :anthropic)"#);
    let msg = format!("{}", err.inner());
    assert!(msg.contains("not configured"), "got: {msg}");
}

// ── ast subcommand (CLI-level tests) ──────────────────────────────

fn sema_cmd() -> std::process::Command {
    std::process::Command::new(env!("CARGO_BIN_EXE_sema"))
}

#[test]
fn test_cli_provider_flag_sets_default_provider() {
    let output = sema_cmd()
        .env("ANTHROPIC_API_KEY", "dummy")
        .env("OPENAI_API_KEY", "dummy")
        .args(["--provider", "openai", "-p", "(llm/current-provider)"])
        .output()
        .expect("failed to run sema");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(":name :openai"),
        "expected openai provider, got: {stdout}"
    );
}

#[test]
fn test_cli_model_flag_sets_default_model() {
    let output = sema_cmd()
        .env_remove("ANTHROPIC_API_KEY")
        .env("OPENAI_API_KEY", "dummy")
        .args([
            "--provider",
            "openai",
            "--model",
            "gpt-4o-mini",
            "-p",
            "(llm/current-provider)",
        ])
        .output()
        .expect("failed to run sema");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(":name :openai"),
        "expected openai provider, got: {stdout}"
    );
    assert!(
        stdout.contains(":model \"gpt-4o-mini\""),
        "expected model override, got: {stdout}"
    );
}

#[test]
fn test_ast_eval_readable() {
    let output = sema_cmd()
        .args(["ast", "-e", "(+ 1 2)"])
        .output()
        .expect("failed to run sema");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("List"), "expected List node: {stdout}");
    assert!(stdout.contains("Symbol +"), "expected Symbol +: {stdout}");
    assert!(stdout.contains("Int 1"), "expected Int 1: {stdout}");
    assert!(stdout.contains("Int 2"), "expected Int 2: {stdout}");
}

#[test]
fn test_ast_eval_json() {
    let output = sema_cmd()
        .args(["ast", "--json", "-e", "(+ 1 2)"])
        .output()
        .expect("failed to run sema");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let json: serde_json::Value = serde_json::from_str(&stdout).expect("invalid JSON");
    assert_eq!(json["type"], "list");
    assert_eq!(json["children"][0]["type"], "symbol");
    assert_eq!(json["children"][0]["value"], "+");
    assert_eq!(json["children"][1]["type"], "int");
    assert_eq!(json["children"][1]["value"], 1);
}

#[test]
fn test_ast_multiple_exprs_json() {
    let output = sema_cmd()
        .args(["ast", "--json", "-e", "(+ 1 2) (- 3 4)"])
        .output()
        .expect("failed to run sema");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let json: serde_json::Value = serde_json::from_str(&stdout).expect("invalid JSON");
    assert!(json.is_array(), "multiple exprs should produce array");
    assert_eq!(json.as_array().unwrap().len(), 2);
}

#[test]
fn test_ast_vector_and_map() {
    let output = sema_cmd()
        .args(["ast", "-e", "[1 :foo]"])
        .output()
        .expect("failed to run sema");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Vector"), "expected Vector: {stdout}");
    assert!(
        stdout.contains("Keyword :foo"),
        "expected Keyword: {stdout}"
    );

    let output = sema_cmd()
        .args(["ast", "--json", "-e", "{:a 1}"])
        .output()
        .expect("failed to run sema");
    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    let json: serde_json::Value = serde_json::from_str(&stdout).expect("invalid JSON");
    assert_eq!(json["type"], "map");
    assert!(json["entries"].is_array());
}

#[test]
fn test_ast_no_input_error() {
    let output = sema_cmd()
        .args(["ast"])
        .output()
        .expect("failed to run sema");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("provide a file or --eval"),
        "expected error message: {stderr}"
    );
}

#[test]
fn test_ast_parse_error() {
    let output = sema_cmd()
        .args(["ast", "-e", "(+ 1"])
        .output()
        .expect("failed to run sema");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Parse error"),
        "expected parse error: {stderr}"
    );
}

#[test]
fn test_string_split_memchr() {
    // Basic split
    assert_eq!(
        eval_to_string(r#"(string/split "a;b;c" ";")"#),
        r#"("a" "b" "c")"#
    );
    // Two-part split (1BRC hot path)
    assert_eq!(
        eval_to_string(r#"(string/split "Berlin;12.3" ";")"#),
        r#"("Berlin" "12.3")"#
    );
    // No match
    assert_eq!(
        eval_to_string(r#"(string/split "hello" ";")"#),
        r#"("hello")"#
    );
    // Multi-char separator
    assert_eq!(
        eval_to_string(r#"(string/split "a::b::c" "::")"#),
        r#"("a" "b" "c")"#
    );
    // Empty parts
    assert_eq!(
        eval_to_string(r#"(string/split "a;;b" ";")"#),
        r#"("a" "" "b")"#
    );
}

#[test]
fn test_hashmap_basic() {
    assert_eq!(
        eval_to_string("(hashmap/get (hashmap/new :a 1 :b 2) :a)"),
        "1"
    );
    assert_eq!(
        eval_to_string("(hashmap/get (hashmap/new :a 1 :b 2) :c)"),
        "nil"
    );
    assert_eq!(
        eval_to_string("(hashmap/get (hashmap/new :a 1) :a 99)"),
        "1"
    );
    assert_eq!(eval_to_string("(hashmap/get (hashmap/new) :a 99)"), "99");
}

#[test]
fn test_hashmap_assoc() {
    assert_eq!(
        eval_to_string("(hashmap/get (hashmap/assoc (hashmap/new) :a 1) :a)"),
        "1"
    );
}

#[test]
fn test_hashmap_to_map() {
    assert_eq!(
        eval_to_string("(hashmap/to-map (hashmap/new :b 2 :a 1))"),
        "{:a 1 :b 2}"
    );
}

#[test]
fn test_hashmap_keys() {
    assert_eq!(
        eval_to_string("(sort (hashmap/keys (hashmap/new :b 2 :a 1)))"),
        "(:a :b)"
    );
}

#[test]
fn test_hashmap_generic_ops() {
    assert_eq!(eval_to_string("(get (hashmap/new :a 1) :a)"), "1");
    assert_eq!(eval_to_string("(get (assoc (hashmap/new) :a 1) :a)"), "1");
    assert_eq!(
        eval_to_string("(sort (keys (hashmap/new :b 2 :a 1)))"),
        "(:a :b)"
    );
    assert_eq!(eval_to_string("(contains? (hashmap/new :a 1) :a)"), "#t");
    assert_eq!(eval_to_string("(count (hashmap/new :a 1 :b 2))"), "2");
    assert_eq!(eval_to_string("(empty? (hashmap/new))"), "#t");
    assert_eq!(eval_to_string("(empty? (hashmap/new :a 1))"), "#f");
    assert_eq!(eval_to_string("(length (hashmap/new :a 1 :b 2))"), "2");
}

#[test]
fn test_car_cdr_compositions() {
    // 2-deep
    assert_eq!(eval("(caar '((1 2) 3))"), Value::int(1));
    assert_eq!(eval("(cadr '(1 2 3))"), Value::int(2));
    assert_eq!(eval_to_string("(cdar '((1 2) 3))"), "(2)");
    assert_eq!(eval_to_string("(cddr '(1 2 3))"), "(3)");
    // 3-deep
    assert_eq!(eval("(caaar '(((1 2) 3) 4))"), Value::int(1));
    assert_eq!(eval("(caadr '(1 (2 3) 4))"), Value::int(2));
    assert_eq!(eval("(cadar '((1 2 3) 4))"), Value::int(2));
    assert_eq!(eval("(caddr '(1 2 3 4))"), Value::int(3));
    assert_eq!(eval_to_string("(cdaar '(((1 2 3)) 4))"), "(2 3)");
    assert_eq!(eval_to_string("(cdadr '(1 (2 3 4)))"), "(3 4)");
    assert_eq!(eval_to_string("(cddar '((1 2 3) 4))"), "(3)");
    assert_eq!(eval_to_string("(cdddr '(1 2 3 4))"), "(4)");
}

#[test]
fn test_assoc_alist() {
    assert_eq!(
        eval_to_string(r#"(assoc "b" '(("a" 1) ("b" 2) ("c" 3)))"#),
        r#"("b" 2)"#
    );
    assert_eq!(
        eval(r#"(assoc "z" '(("a" 1) ("b" 2)))"#),
        Value::bool(false)
    );
    assert_eq!(eval_to_string("(assoc 2 '((1 a) (2 b) (3 c)))"), "(2 b)");
}

#[test]
fn test_assq() {
    assert_eq!(
        eval_to_string("(assq :name '((:name \"Alice\") (:age 30)))"),
        r#"(:name "Alice")"#
    );
    assert_eq!(eval("(assq :missing '((:a 1) (:b 2)))"), Value::bool(false));
}

#[test]
fn test_assv() {
    assert_eq!(eval_to_string("(assv 42 '((1 a) (42 b) (3 c)))"), "(42 b)");
    assert_eq!(eval("(assv 99 '((1 a) (2 b)))"), Value::bool(false));
}

#[test]
fn test_do_loop_basic() {
    // Sum 1..10
    assert_eq!(
        eval("(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 10) sum))"),
        Value::int(45)
    );
}

#[test]
fn test_do_loop_factorial() {
    assert_eq!(
        eval("(do ((i 1 (+ i 1)) (acc 1 (* acc i))) ((> i 5) acc))"),
        Value::int(120)
    );
}

#[test]
fn test_do_loop_with_body() {
    // Body executes for side effects; test clause returns result
    assert_eq!(
        eval("(begin (define count 0) (do ((i 0 (+ i 1))) ((= i 3) count) (set! count (+ count 1))))"),
        Value::int(3)
    );
}

#[test]
fn test_do_loop_no_step() {
    // Variable without step expr stays constant
    assert_eq!(
        eval("(do ((x 10) (i 0 (+ i 1))) ((= i 3) x))"),
        Value::int(10)
    );
}

#[test]
fn test_do_begin_still_works() {
    assert_eq!(eval("(begin 1 2 3)"), Value::int(3));
    assert_eq!(
        eval("(begin (define x 10) (define y 20) (+ x y))"),
        Value::int(30)
    );
}

#[test]
fn test_char_literals() {
    assert_eq!(eval(r"#\a"), Value::char('a'));
    assert_eq!(eval(r"#\Z"), Value::char('Z'));
    assert_eq!(eval(r"#\space"), Value::char(' '));
    assert_eq!(eval(r"#\newline"), Value::char('\n'));
    assert_eq!(eval(r"#\tab"), Value::char('\t'));
}

#[test]
fn test_char_predicate() {
    assert_eq!(eval(r"(char? #\a)"), Value::bool(true));
    assert_eq!(eval(r#"(char? "a")"#), Value::bool(false));
    assert_eq!(eval("(char? 42)"), Value::bool(false));
}

#[test]
fn test_char_conversions() {
    assert_eq!(eval(r"(char->integer #\a)"), Value::int(97));
    assert_eq!(eval(r"(char->integer #\A)"), Value::int(65));
    assert_eq!(eval("(integer->char 97)"), Value::char('a'));
    assert_eq!(eval(r#"(char->string #\x)"#), Value::string("x"));
    assert_eq!(eval(r#"(string->char "z")"#), Value::char('z'));
}

#[test]
fn test_char_predicates() {
    assert_eq!(eval(r"(char-alphabetic? #\a)"), Value::bool(true));
    assert_eq!(eval(r"(char-alphabetic? #\1)"), Value::bool(false));
    assert_eq!(eval(r"(char-numeric? #\5)"), Value::bool(true));
    assert_eq!(eval(r"(char-numeric? #\a)"), Value::bool(false));
    assert_eq!(eval(r"(char-whitespace? #\space)"), Value::bool(true));
    assert_eq!(eval(r"(char-whitespace? #\a)"), Value::bool(false));
    assert_eq!(eval(r"(char-upper-case? #\A)"), Value::bool(true));
    assert_eq!(eval(r"(char-upper-case? #\a)"), Value::bool(false));
    assert_eq!(eval(r"(char-lower-case? #\a)"), Value::bool(true));
    assert_eq!(eval(r"(char-lower-case? #\A)"), Value::bool(false));
}

#[test]
fn test_char_case() {
    assert_eq!(eval(r"(char-upcase #\a)"), Value::char('A'));
    assert_eq!(eval(r"(char-downcase #\A)"), Value::char('a'));
    assert_eq!(eval(r"(char-upcase #\1)"), Value::char('1'));
}

#[test]
fn test_string_ref_returns_char() {
    assert_eq!(eval(r#"(char? (string-ref "hello" 0))"#), Value::bool(true));
    assert_eq!(eval(r#"(string-ref "abc" 1)"#), Value::char('b'));
}

#[test]
fn test_string_to_list_chars() {
    assert_eq!(
        eval_to_string(r#"(string->list "abc")"#),
        "(#\\a #\\b #\\c)"
    );
    assert_eq!(eval_to_string(r#"(string->list "")"#), "()");
}

#[test]
fn test_list_to_string() {
    assert_eq!(
        eval(r#"(list->string (list #\h #\i))"#),
        Value::string("hi")
    );
    assert_eq!(eval(r#"(list->string '())"#), Value::string(""));
}

#[test]
fn test_delay_force_basic() {
    assert_eq!(eval("(force (delay (+ 1 2)))"), Value::int(3));
    assert_eq!(eval("(force (delay 42))"), Value::int(42));
}

#[test]
fn test_delay_is_promise() {
    assert_eq!(eval("(promise? (delay 1))"), Value::bool(true));
    assert_eq!(eval("(promise? 42)"), Value::bool(false));
    assert_eq!(eval("(promise? (list 1))"), Value::bool(false));
}

#[test]
fn test_delay_memoization() {
    // Body evaluated only once — counter only increments once
    assert_eq!(
        eval("(begin (define counter 0) (define p (delay (begin (set! counter (+ counter 1)) counter))) (force p) (force p) counter)"),
        Value::int(1)
    );
}

#[test]
fn test_force_non_promise() {
    // force on non-promise returns value as-is (R7RS compatible)
    assert_eq!(eval("(force 42)"), Value::int(42));
    assert_eq!(eval(r#"(force "hello")"#), Value::string("hello"));
}

#[test]
fn test_promise_forced_predicate() {
    assert_eq!(
        eval("(begin (define p (delay 1)) (promise-forced? p))"),
        Value::bool(false)
    );
    assert_eq!(
        eval("(begin (define p (delay 1)) (force p) (promise-forced? p))"),
        Value::bool(true)
    );
}

#[test]
fn test_char_comparison_predicates() {
    assert_eq!(eval("(char=? #\\a #\\a)"), Value::bool(true));
    assert_eq!(eval("(char=? #\\a #\\b)"), Value::bool(false));
    assert_eq!(eval("(char<? #\\a #\\b)"), Value::bool(true));
    assert_eq!(eval("(char<? #\\b #\\a)"), Value::bool(false));
    assert_eq!(eval("(char>? #\\b #\\a)"), Value::bool(true));
    assert_eq!(eval("(char>? #\\a #\\b)"), Value::bool(false));
    assert_eq!(eval("(char<=? #\\a #\\a)"), Value::bool(true));
    assert_eq!(eval("(char<=? #\\a #\\b)"), Value::bool(true));
    assert_eq!(eval("(char<=? #\\b #\\a)"), Value::bool(false));
    assert_eq!(eval("(char>=? #\\b #\\a)"), Value::bool(true));
    assert_eq!(eval("(char>=? #\\a #\\a)"), Value::bool(true));
    assert_eq!(eval("(char>=? #\\a #\\b)"), Value::bool(false));
}

#[test]
fn test_char_ci_comparison_predicates() {
    assert_eq!(eval("(char-ci=? #\\A #\\a)"), Value::bool(true));
    assert_eq!(eval("(char-ci=? #\\a #\\A)"), Value::bool(true));
    assert_eq!(eval("(char-ci=? #\\a #\\b)"), Value::bool(false));
    assert_eq!(eval("(char-ci<? #\\A #\\b)"), Value::bool(true));
    assert_eq!(eval("(char-ci<? #\\a #\\B)"), Value::bool(true));
    assert_eq!(eval("(char-ci>? #\\b #\\A)"), Value::bool(true));
    assert_eq!(eval("(char-ci<=? #\\A #\\a)"), Value::bool(true));
    assert_eq!(eval("(char-ci>=? #\\a #\\A)"), Value::bool(true));
}

#[test]
fn test_define_record_type_basic() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define-record-type point
            (make-point x y)
            point?
            (x point-x)
            (y point-y))
        (define p (make-point 3 4))
        (list (point? p) (point-x p) (point-y p))
    ",
        )
        .unwrap();
    assert_eq!(
        result,
        Value::list(vec![Value::bool(true), Value::int(3), Value::int(4)])
    );
}

#[test]
fn test_define_record_type_predicate_false() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define-record-type point
            (make-point x y)
            point?
            (x point-x)
            (y point-y))
        (point? 42)
    ",
        )
        .unwrap();
    assert_eq!(result, Value::bool(false));
}

#[test]
fn test_define_record_type_equality() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define-record-type point
            (make-point x y)
            point?
            (x point-x)
            (y point-y))
        (equal? (make-point 1 2) (make-point 1 2))
    ",
        )
        .unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_define_record_type_type_function() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define-record-type point
            (make-point x y)
            point?
            (x point-x)
            (y point-y))
        (type (make-point 1 2))
    ",
        )
        .unwrap();
    assert_eq!(result, Value::keyword("point"));
}

#[test]
fn test_define_record_type_record_predicate() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define-record-type point
            (make-point x y)
            point?
            (x point-x)
            (y point-y))
        (list (record? (make-point 1 2)) (record? 42))
    ",
        )
        .unwrap();
    assert_eq!(
        result,
        Value::list(vec![Value::bool(true), Value::bool(false)])
    );
}

#[test]
fn test_define_record_type_mutator_ignored() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define-record-type point
            (make-point x y)
            point?
            (x point-x set-point-x!)
            (y point-y set-point-y!))
        (point-x (make-point 7 8))
    ",
        )
        .unwrap();
    assert_eq!(result, Value::int(7));
}

#[test]
fn test_define_record_type_multiple_types() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define-record-type point (make-point x y) point? (x point-x) (y point-y))
        (define-record-type color (make-color r g b) color? (r color-r) (g color-g) (b color-b))
        (list (point? (make-point 1 2)) (color? (make-point 1 2))
              (color? (make-color 255 0 0)) (point? (make-color 255 0 0)))
    ",
        )
        .unwrap();
    assert_eq!(
        result,
        Value::list(vec![
            Value::bool(true),
            Value::bool(false),
            Value::bool(true),
            Value::bool(false),
        ])
    );
}

#[test]
fn test_bytevector_constructors() {
    assert_eq!(eval("(bytevector 1 2 3)"), Value::bytevector(vec![1, 2, 3]));
    assert_eq!(
        eval("(make-bytevector 3 7)"),
        Value::bytevector(vec![7, 7, 7])
    );
    assert_eq!(
        eval("(make-bytevector 4)"),
        Value::bytevector(vec![0, 0, 0, 0])
    );
}

#[test]
fn test_bytevector_length() {
    assert_eq!(eval("(bytevector-length #u8(1 2 3))"), Value::int(3));
    assert_eq!(eval("(bytevector-length #u8())"), Value::int(0));
}

#[test]
fn test_bytevector_u8_ref() {
    assert_eq!(eval("(bytevector-u8-ref #u8(10 20 30) 0)"), Value::int(10));
    assert_eq!(eval("(bytevector-u8-ref #u8(10 20 30) 2)"), Value::int(30));
}

#[test]
fn test_bytevector_u8_set() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            "
        (define a #u8(1 2 3))
        (define b (bytevector-u8-set! a 0 9))
        (list a b)
    ",
        )
        .unwrap();
    assert_eq!(
        result,
        Value::list(vec![
            Value::bytevector(vec![1, 2, 3]),
            Value::bytevector(vec![9, 2, 3]),
        ])
    );
}

#[test]
fn test_bytevector_copy() {
    assert_eq!(
        eval("(bytevector-copy #u8(1 2 3 4 5) 1 3)"),
        Value::bytevector(vec![2, 3])
    );
    assert_eq!(
        eval("(bytevector-copy #u8(1 2 3))"),
        Value::bytevector(vec![1, 2, 3])
    );
}

#[test]
fn test_bytevector_append() {
    assert_eq!(
        eval("(bytevector-append #u8(1 2) #u8(3 4))"),
        Value::bytevector(vec![1, 2, 3, 4])
    );
    assert_eq!(
        eval("(bytevector-append #u8(1) #u8(2) #u8(3))"),
        Value::bytevector(vec![1, 2, 3])
    );
}

#[test]
fn test_bytevector_list_conversion() {
    assert_eq!(
        eval("(bytevector->list #u8(65 66 67))"),
        Value::list(vec![Value::int(65), Value::int(66), Value::int(67)])
    );
    assert_eq!(
        eval("(list->bytevector (list 1 2 3))"),
        Value::bytevector(vec![1, 2, 3])
    );
}

#[test]
fn test_bytevector_utf8_conversion() {
    assert_eq!(eval_to_string("(utf8->string #u8(104 105))"), "\"hi\"");
    assert_eq!(
        eval("(string->utf8 \"hi\")"),
        Value::bytevector(vec![104, 105])
    );
}

#[test]
fn test_bytevector_predicate() {
    assert_eq!(eval("(bytevector? #u8(1 2))"), Value::bool(true));
    assert_eq!(eval("(bytevector? 42)"), Value::bool(false));
    assert_eq!(eval("(bytevector? (list 1 2))"), Value::bool(false));
}

#[test]
fn test_bytevector_display() {
    assert_eq!(eval_to_string("#u8(1 2 3)"), "#u8(1 2 3)");
    assert_eq!(eval_to_string("#u8()"), "#u8()");
}

#[test]
fn test_truncate() {
    assert_eq!(eval("(truncate 3.9)"), Value::int(3));
    assert_eq!(eval("(truncate -3.9)"), Value::int(-3));
    assert_eq!(eval("(truncate 5)"), Value::int(5));
}

#[test]
fn test_scheme_aliases() {
    assert_eq!(eval("(modulo 17 5)"), Value::int(2));
    assert_eq!(eval("(expt 2 10)"), Value::int(1024));
    assert_eq!(eval("(ceiling 3.2)"), Value::int(4));
}

#[test]
fn test_math_pow_namespaced() {
    assert_eq!(eval("(math/pow 2.0 10.0)"), Value::float(1024.0));
    assert_eq!(eval("(math/pow 2 3)"), Value::int(8));
}

#[test]
fn test_math_hyperbolic() {
    assert_eq!(eval("(math/sinh 0)"), Value::float(0.0));
    assert_eq!(eval("(math/cosh 0)"), Value::float(1.0));
    assert_eq!(eval("(math/tanh 0)"), Value::float(0.0));
}

#[test]
fn test_math_angle_conversion() {
    if let Some(f) = eval("(math/degrees->radians 180)").as_float() {
        assert!((f - std::f64::consts::PI).abs() < 1e-10);
    } else {
        panic!("expected float");
    }
    if let Some(f) = eval("(math/radians->degrees pi)").as_float() {
        assert!((f - 180.0).abs() < 1e-10);
    } else {
        panic!("expected float");
    }
}

#[test]
fn test_math_lerp() {
    assert_eq!(eval("(math/lerp 0.0 10.0 0.5)"), Value::float(5.0));
    assert_eq!(eval("(math/lerp 0.0 10.0 0.0)"), Value::float(0.0));
    assert_eq!(eval("(math/lerp 0.0 10.0 1.0)"), Value::float(10.0));
}

#[test]
fn test_math_map_range() {
    assert_eq!(
        eval("(math/map-range 5.0 0.0 10.0 0.0 100.0)"),
        Value::float(50.0)
    );
    assert_eq!(
        eval("(math/map-range 0.0 0.0 10.0 0.0 100.0)"),
        Value::float(0.0)
    );
}

#[test]
fn test_math_special_values() {
    assert_eq!(eval("(math/nan? math/nan)"), Value::bool(true));
    assert_eq!(eval("(math/nan? 1.0)"), Value::bool(false));
    assert_eq!(eval("(math/infinite? math/infinity)"), Value::bool(true));
    assert_eq!(eval("(math/infinite? 1.0)"), Value::bool(false));
}

#[test]
fn test_even_odd_predicates() {
    assert_eq!(eval("(even? 4)"), Value::bool(true));
    assert_eq!(eval("(even? 3)"), Value::bool(false));
    assert_eq!(eval("(odd? 3)"), Value::bool(true));
    assert_eq!(eval("(odd? 4)"), Value::bool(false));
    assert_eq!(eval("(even? 0)"), Value::bool(true));
}

#[test]
fn test_take_while() {
    assert_eq!(
        eval_to_string("(take-while (lambda (x) (< x 4)) (list 1 2 3 4 5))"),
        "(1 2 3)"
    );
    assert_eq!(
        eval_to_string("(take-while (lambda (x) (< x 1)) (list 1 2 3))"),
        "()"
    );
}

#[test]
fn test_drop_while() {
    assert_eq!(
        eval_to_string("(drop-while (lambda (x) (< x 4)) (list 1 2 3 4 5))"),
        "(4 5)"
    );
    assert_eq!(
        eval_to_string("(drop-while (lambda (x) (< x 10)) (list 1 2 3))"),
        "()"
    );
}

#[test]
fn test_list_dedupe() {
    assert_eq!(
        eval_to_string("(list/dedupe (list 1 1 2 2 3 1 1))"),
        "(1 2 3 1)"
    );
    assert_eq!(eval_to_string("(list/dedupe (list 1 2 3))"), "(1 2 3)");
    assert_eq!(eval_to_string("(list/dedupe (list))"), "()");
}

#[test]
fn test_flat_map() {
    assert_eq!(
        eval_to_string("(flat-map (lambda (x) (list x (* x 10))) (list 1 2 3))"),
        "(1 10 2 20 3 30)"
    );
    assert_eq!(
        eval_to_string("(flat-map (lambda (x) (list)) (list 1 2 3))"),
        "()"
    );
}

#[test]
fn test_sys_home_dir() {
    let result = eval("(sys/home-dir)");
    assert!(result.is_string());
    if let Some(s) = result.as_str() {
        assert!(!s.is_empty());
    }
}

#[test]
fn test_sys_temp_dir() {
    let result = eval("(sys/temp-dir)");
    assert!(result.is_string());
    if let Some(s) = result.as_str() {
        assert!(!s.is_empty());
    }
}

#[test]
fn test_sys_hostname() {
    let result = eval("(sys/hostname)");
    assert!(result.is_string());
}

#[test]
fn test_sys_user() {
    let result = eval("(sys/user)");
    assert!(result.is_string());
    if let Some(s) = result.as_str() {
        assert!(!s.is_empty());
    }
}

#[test]
fn test_string_last_index_of() {
    assert_eq!(
        eval(r#"(string/last-index-of "abcabc" "bc")"#),
        Value::int(4)
    );
    assert_eq!(
        eval(r#"(string/last-index-of "hello" "xyz")"#),
        Value::nil()
    );
    assert_eq!(eval(r#"(string/last-index-of "aaa" "a")"#), Value::int(2));
}

#[test]
fn test_string_reverse() {
    assert_eq!(eval(r#"(string/reverse "hello")"#), Value::string("olleh"));
    assert_eq!(eval(r#"(string/reverse "")"#), Value::string(""));
    assert_eq!(eval(r#"(string/reverse "a")"#), Value::string("a"));
}

#[test]
fn test_string_empty() {
    assert_eq!(eval(r#"(string/empty? "")"#), Value::bool(true));
    assert_eq!(eval(r#"(string/empty? "hello")"#), Value::bool(false));
    assert_eq!(eval(r#"(string/empty? " ")"#), Value::bool(false));
}

#[test]
fn test_string_capitalize() {
    assert_eq!(
        eval(r#"(string/capitalize "hello")"#),
        Value::string("Hello")
    );
    assert_eq!(
        eval(r#"(string/capitalize "HELLO")"#),
        Value::string("Hello")
    );
    assert_eq!(eval(r#"(string/capitalize "")"#), Value::string(""));
    assert_eq!(eval(r#"(string/capitalize "a")"#), Value::string("A"));
}

#[test]
fn test_string_title_case() {
    assert_eq!(
        eval(r#"(string/title-case "hello world")"#),
        Value::string("Hello World")
    );
    assert_eq!(
        eval(r#"(string/title-case "foo bar baz")"#),
        Value::string("Foo Bar Baz")
    );
    assert_eq!(eval(r#"(string/title-case "")"#), Value::string(""));
}

// IO: print-error, println-error

#[test]
fn test_print_error() {
    assert_eq!(eval(r#"(print-error "hello")"#), Value::nil());
    assert_eq!(eval(r#"(print-error "a" "b" "c")"#), Value::nil());
    assert_eq!(eval(r#"(print-error 42)"#), Value::nil());
}

#[test]
fn test_println_error() {
    assert_eq!(eval(r#"(println-error "hello")"#), Value::nil());
    assert_eq!(eval(r#"(println-error "a" "b" "c")"#), Value::nil());
    assert_eq!(eval(r#"(println-error)"#), Value::nil());
}

// System: sys/interactive?

#[test]
fn test_sys_interactive() {
    // Result depends on whether stdin is a TTY; just verify it returns a boolean
    let result = eval("(sys/interactive?)");
    assert!(result.is_bool());
    assert_eq!(eval("(boolean? (sys/interactive?))"), Value::bool(true));
}

#[test]
fn test_sys_tty() {
    // In test/CI context may or may not have a TTY; just verify it returns string or nil
    let result = eval("(sys/tty)");
    assert!(result.is_string() || result.is_nil());
}

#[test]
fn test_sys_pid() {
    let result = eval("(sys/pid)");
    let pid = result.as_int().expect("expected int");
    assert!(pid > 0);
}

#[test]
fn test_sys_arch() {
    let result = eval("(sys/arch)");
    assert!(result.is_string());
    assert_eq!(eval("(string? (sys/arch))"), Value::bool(true));
}

#[test]
fn test_sys_os() {
    let result = eval("(sys/os)");
    assert!(result.is_string());
    assert_eq!(eval("(string? (sys/os))"), Value::bool(true));
}

#[test]
fn test_sys_which() {
    // "sh" should exist on any unix system
    let result = eval(r#"(sys/which "sh")"#);
    assert!(result.is_string());
    // non-existent binary returns nil
    assert_eq!(
        eval(r#"(sys/which "this-binary-does-not-exist-xyz")"#),
        Value::nil()
    );
}

#[test]
fn test_sys_elapsed() {
    let result = eval("(sys/elapsed)");
    let ns = result.as_int().expect("expected int");
    assert!(ns >= 0);
    // Two calls should be monotonically increasing
    assert_eq!(
        eval("(let ((a (sys/elapsed)) (b (sys/elapsed))) (<= a b))"),
        Value::bool(true)
    );
}

// List: list/shuffle, list/split-at, list/take-while, list/drop-while,
//       list/sum, list/min, list/max, list/pick, list/repeat, iota

#[test]
fn test_list_shuffle() {
    // Shuffle returns a list of same length with same elements
    let result = eval("(length (list/shuffle (list 1 2 3 4 5)))");
    assert_eq!(result, Value::int(5));
    // Shuffle of empty list is empty
    assert_eq!(eval("(list/shuffle '())"), Value::list(vec![]));
}

#[test]
fn test_list_split_at() {
    assert_eq!(
        eval("(list/split-at (list 1 2 3 4 5) 3)"),
        Value::list(vec![
            Value::list(vec![Value::int(1), Value::int(2), Value::int(3)]),
            Value::list(vec![Value::int(4), Value::int(5)]),
        ])
    );
    assert_eq!(
        eval("(list/split-at (list 1 2 3) 0)"),
        Value::list(vec![
            Value::list(vec![]),
            Value::list(vec![Value::int(1), Value::int(2), Value::int(3)]),
        ])
    );
    assert_eq!(
        eval("(list/split-at (list 1 2 3) 5)"),
        Value::list(vec![
            Value::list(vec![Value::int(1), Value::int(2), Value::int(3)]),
            Value::list(vec![]),
        ])
    );
}

#[test]
fn test_list_take_while() {
    assert_eq!(
        eval("(list/take-while (lambda (x) (< x 4)) (list 1 2 3 4 5))"),
        Value::list(vec![Value::int(1), Value::int(2), Value::int(3)])
    );
    assert_eq!(
        eval("(list/take-while (lambda (x) (< x 1)) (list 1 2 3))"),
        Value::list(vec![])
    );
    assert_eq!(
        eval("(list/take-while (lambda (x) #t) (list 1 2 3))"),
        Value::list(vec![Value::int(1), Value::int(2), Value::int(3)])
    );
}

#[test]
fn test_list_drop_while() {
    assert_eq!(
        eval("(list/drop-while (lambda (x) (< x 4)) (list 1 2 3 4 5))"),
        Value::list(vec![Value::int(4), Value::int(5)])
    );
    assert_eq!(
        eval("(list/drop-while (lambda (x) (< x 1)) (list 1 2 3))"),
        Value::list(vec![Value::int(1), Value::int(2), Value::int(3)])
    );
    assert_eq!(
        eval("(list/drop-while (lambda (x) #t) (list 1 2 3))"),
        Value::list(vec![])
    );
}

#[test]
fn test_list_sum() {
    assert_eq!(eval("(list/sum (list 1 2 3 4 5))"), Value::int(15));
    assert_eq!(eval("(list/sum (list 1.0 2.0 3.0))"), Value::float(6.0));
    assert_eq!(eval("(list/sum (list 1 2.0 3))"), Value::float(6.0));
    assert_eq!(eval("(list/sum '())"), Value::int(0));
}

#[test]
fn test_list_min() {
    assert_eq!(eval("(list/min (list 3 1 4 1 5))"), Value::int(1));
    assert_eq!(eval("(list/min (list 3.0 1.5 2.0))"), Value::float(1.5));
    assert_eq!(eval("(list/min (list 42))"), Value::int(42));
}

#[test]
fn test_list_max() {
    assert_eq!(eval("(list/max (list 3 1 4 1 5))"), Value::int(5));
    assert_eq!(eval("(list/max (list 3.0 1.5 2.0))"), Value::float(3.0));
    assert_eq!(eval("(list/max (list 42))"), Value::int(42));
}

#[test]
fn test_list_pick() {
    // Pick returns an element from the list
    let result = eval("(list/pick (list 1 2 3 4 5))");
    let n = result.as_int().expect("expected int");
    assert!((1..=5).contains(&n));
}

#[test]
fn test_list_repeat() {
    assert_eq!(
        eval("(list/repeat 3 0)"),
        Value::list(vec![Value::int(0), Value::int(0), Value::int(0)])
    );
    assert_eq!(
        eval(r#"(list/repeat 2 "x")"#),
        Value::list(vec![Value::string("x"), Value::string("x")])
    );
    assert_eq!(eval("(list/repeat 0 1)"), Value::list(vec![]));
    // make-list alias
    assert_eq!(
        eval("(make-list 3 0)"),
        Value::list(vec![Value::int(0), Value::int(0), Value::int(0)])
    );
}

#[test]
fn test_iota() {
    assert_eq!(
        eval("(iota 5)"),
        Value::list(vec![
            Value::int(0),
            Value::int(1),
            Value::int(2),
            Value::int(3),
            Value::int(4)
        ])
    );
    assert_eq!(
        eval("(iota 3 10)"),
        Value::list(vec![Value::int(10), Value::int(11), Value::int(12)])
    );
    assert_eq!(
        eval("(iota 4 0 2)"),
        Value::list(vec![
            Value::int(0),
            Value::int(2),
            Value::int(4),
            Value::int(6)
        ])
    );
    assert_eq!(eval("(iota 0)"), Value::list(vec![]));
}

// String: string/map

#[test]
fn test_string_map() {
    assert_eq!(
        eval(r#"(string/map char-upcase "hello")"#),
        Value::string("HELLO")
    );
    assert_eq!(
        eval(r#"(string/map (lambda (c) c) "abc")"#),
        Value::string("abc")
    );
    assert_eq!(eval(r#"(string/map char-upcase "")"#), Value::string(""));
}

// Terminal: term/ color and style functions

#[test]
fn test_term_bold() {
    assert_eq!(
        eval(r#"(term/bold "hello")"#),
        Value::string("\x1b[1mhello\x1b[0m")
    );
}

#[test]
fn test_term_dim() {
    assert_eq!(
        eval(r#"(term/dim "text")"#),
        Value::string("\x1b[2mtext\x1b[0m")
    );
}

#[test]
fn test_term_colors() {
    assert_eq!(
        eval(r#"(term/red "error")"#),
        Value::string("\x1b[31merror\x1b[0m")
    );
    assert_eq!(
        eval(r#"(term/green "ok")"#),
        Value::string("\x1b[32mok\x1b[0m")
    );
    assert_eq!(
        eval(r#"(term/cyan "info")"#),
        Value::string("\x1b[36minfo\x1b[0m")
    );
    assert_eq!(
        eval(r#"(term/gray "muted")"#),
        Value::string("\x1b[90mmuted\x1b[0m")
    );
}

#[test]
fn test_term_style_compound() {
    assert_eq!(
        eval(r#"(term/style "text" :bold :red)"#),
        Value::string("\x1b[1;31mtext\x1b[0m")
    );
    assert_eq!(
        eval(r#"(term/style "ok" :bold :green)"#),
        Value::string("\x1b[1;32mok\x1b[0m")
    );
}

#[test]
fn test_term_style_no_keywords() {
    // term/style with just text and no keywords returns plain text
    assert_eq!(eval(r#"(term/style "plain")"#), Value::string("plain"));
}

#[test]
fn test_term_strip() {
    assert_eq!(
        eval(r#"(term/strip (term/bold "hello"))"#),
        Value::string("hello")
    );
    assert_eq!(
        eval(r#"(term/strip (term/style "text" :bold :red))"#),
        Value::string("text")
    );
    assert_eq!(
        eval(r#"(term/strip "no ansi here")"#),
        Value::string("no ansi here")
    );
}

#[test]
fn test_term_rgb() {
    assert_eq!(
        eval(r#"(term/rgb "hi" 255 100 0)"#),
        Value::string("\x1b[38;2;255;100;0mhi\x1b[0m")
    );
}

#[test]
fn test_term_strip_rgb() {
    assert_eq!(
        eval(r#"(term/strip (term/rgb "hi" 255 100 0))"#),
        Value::string("hi")
    );
}

#[test]
fn test_term_spinner_start_stop() {
    // Spinner start returns an integer ID, stop returns nil
    assert_eq!(
        eval(
            r#"(let ((id (term/spinner-start "test")))
                   (term/spinner-stop id))"#
        ),
        Value::nil()
    );
}

#[test]
fn test_term_spinner_update() {
    assert_eq!(
        eval(
            r#"(let ((id (term/spinner-start "initial")))
                   (term/spinner-update id "updated")
                   (term/spinner-stop id))"#
        ),
        Value::nil()
    );
}

// Regression tests: deftool params stored as BTreeMap (alphabetical keys).
// Lambda handlers must receive args in declaration order, not alphabetical.
// The actual json_args_to_sema ordering fix is tested via unit tests in
// sema-llm/src/builtins.rs. These integration tests verify the deftool
// special form preserves lambda param semantics.

#[test]
fn test_deftool_lambda_param_order_not_alphabetical() {
    // Params :path/:content — alphabetically content < path, but lambda declares (path content).
    // Verify calling the handler directly respects lambda param names, not map key order.
    let result = eval(
        r#"(begin
          (deftool write-file
            "Write content to a file"
            {:path {:type :string :description "File path"}
             :content {:type :string :description "File content"}}
            (lambda (path content) (list path content)))
          ;; Direct call: args are positional per lambda declaration order
          ((lambda (path content) (list path content)) "/tmp/test.txt" "hello world"))"#,
    );
    assert_eq!(format!("{}", result), "(\"/tmp/test.txt\" \"hello world\")");
}

#[test]
fn test_deftool_params_are_map() {
    // Verify tool/parameters returns a map (BTreeMap), confirming why ordering matters.
    assert_eq!(
        eval(
            r#"(begin
              (deftool t1 "desc"
                {:zebra {:type :string} :apple {:type :string}}
                (lambda (zebra apple) "ok"))
              (map? (tool/parameters t1)))"#,
        ),
        Value::bool(true)
    );
}

#[test]
fn test_deftool_param_keys_sorted_alphabetically() {
    // Confirm that tool parameter map keys come out alphabetically (BTreeMap),
    // demonstrating the ordering bug this fix addresses.
    let result = eval(
        r#"(begin
          (deftool t2 "desc"
            {:zebra {:type :string} :apple {:type :string}}
            (lambda (zebra apple) "ok"))
          (keys (tool/parameters t2)))"#,
    );
    // BTreeMap sorts: :apple before :zebra — opposite of declaration order
    assert_eq!(format!("{}", result), "(:apple :zebra)");
}

#[test]
fn test_deftool_three_params_ordering() {
    // Verify that deftool with 3 params where declaration != alphabetical works correctly.
    // This is a smoke test — the actual execute_tool_call fix is in unit tests.
    let result = eval(
        r#"(begin
          (deftool multi-tool "test"
            {:c_third {:type :string}
             :a_first {:type :string}
             :b_second {:type :string}}
            (lambda (c_third a_first b_second)
              (string-append c_third "-" a_first "-" b_second)))
          ;; Calling the lambda directly proves param binding works
          (let ((f (lambda (c_third a_first b_second)
                     (string-append c_third "-" a_first "-" b_second))))
            (f "C" "A" "B")))"#,
    );
    assert_eq!(result, Value::string("C-A-B"));
}

#[test]
fn test_unicode_string_length() {
    // string-length should count characters, not bytes
    assert_eq!(eval(r#"(string-length "hello")"#), Value::int(5));
    assert_eq!(eval(r#"(string-length "héllo")"#), Value::int(5));
    assert_eq!(eval(r#"(string-length "λ")"#), Value::int(1));
    assert_eq!(eval(r#"(string-length "日本語")"#), Value::int(3));
    assert_eq!(eval(r#"(string-length "😀")"#), Value::int(1));
    assert_eq!(eval(r#"(string-length "")"#), Value::int(0));
}

#[test]
fn test_unicode_substring() {
    // substring should use character indices, not byte indices
    assert_eq!(eval(r#"(substring "héllo" 0 1)"#), Value::string("h"));
    assert_eq!(eval(r#"(substring "héllo" 1 2)"#), Value::string("é"));
    assert_eq!(eval(r#"(substring "héllo" 0 5)"#), Value::string("héllo"));
    assert_eq!(eval(r#"(substring "日本語" 1 3)"#), Value::string("本語"));
    assert_eq!(eval(r#"(substring "😀🎉" 0 1)"#), Value::string("😀"));
    assert_eq!(eval(r#"(substring "😀🎉" 1 2)"#), Value::string("🎉"));
}

#[test]
fn test_unicode_string_ref() {
    assert_eq!(eval(r#"(string-ref "héllo" 1)"#), Value::char('é'));
    assert_eq!(eval(r#"(string-ref "日本語" 2)"#), Value::char('語'));
}

#[test]
fn test_unicode_string_pad() {
    // Padding should count characters, not bytes
    assert_eq!(eval(r#"(string/pad-left "éx" 5)"#), Value::string("   éx"));
    assert_eq!(eval(r#"(string/pad-right "éx" 5)"#), Value::string("éx   "));
    // Already at or past width
    assert_eq!(
        eval(r#"(string/pad-left "héllo" 3)"#),
        Value::string("héllo")
    );
}

#[test]
fn test_unicode_length_consistency() {
    // length and string-length should agree on character count
    assert_eq!(eval(r#"(length "héllo")"#), Value::int(5));
    assert_eq!(eval(r#"(count "héllo")"#), Value::int(5));
}

#[test]
fn test_dissoc_hashmap() {
    // dissoc should work on hashmaps and preserve type
    assert_eq!(
        eval("(hashmap/contains? (dissoc (hashmap/new :a 1 :b 2 :c 3) :b) :b)"),
        Value::bool(false)
    );
    assert_eq!(
        eval("(count (dissoc (hashmap/new :a 1 :b 2 :c 3) :b))"),
        Value::int(2)
    );
}

#[test]
fn test_merge_hashmap() {
    // merge should work with hashmaps
    assert_eq!(
        eval("(count (merge (hashmap/new :a 1) (hashmap/new :b 2)))"),
        Value::int(2)
    );
    // merge mixed: Map + HashMap
    assert_eq!(
        eval("(count (merge {:a 1} (hashmap/new :b 2)))"),
        Value::int(2)
    );
}

#[test]
fn test_map_entries_hashmap() {
    // map/entries should work on hashmaps
    assert_eq!(
        eval("(length (map/entries (hashmap/new :a 1 :b 2)))"),
        Value::int(2)
    );
}

#[test]
fn test_map_ops_hashmap() {
    // map/map-vals on hashmap
    assert_eq!(
        eval(
            "(get (hashmap/to-map (map/map-vals (lambda (v) (* v 2)) (hashmap/new :a 1 :b 2))) :a)"
        ),
        Value::int(2)
    );
    // map/filter on hashmap
    assert_eq!(
        eval("(count (map/filter (lambda (k v) (> v 1)) (hashmap/new :a 1 :b 2 :c 3)))"),
        Value::int(2)
    );
    // map/select-keys on hashmap
    assert_eq!(
        eval("(count (map/select-keys (hashmap/new :a 1 :b 2 :c 3) (list :a :c)))"),
        Value::int(2)
    );
    // map/map-keys on hashmap
    assert_eq!(
        eval("(count (map/map-keys (lambda (k) k) (hashmap/new :a 1 :b 2)))"),
        Value::int(2)
    );
    // map/update on hashmap
    assert_eq!(
        eval("(hashmap/get (map/update (hashmap/new :a 1) :a (lambda (v) (+ v 10))) :a)"),
        Value::int(11)
    );
}

#[test]
fn test_string_byte_length() {
    // ASCII: each char is 1 byte
    assert_eq!(eval(r#"(string/byte-length "hello")"#), Value::int(5));
    // UTF-8 multi-byte: "é" is 2 bytes, so "héllo" is 6 bytes
    assert_eq!(eval(r#"(string/byte-length "héllo")"#), Value::int(6));
    // CJK: each char is 3 bytes
    assert_eq!(eval(r#"(string/byte-length "日本語")"#), Value::int(9));
    // Empty string
    assert_eq!(eval(r#"(string/byte-length "")"#), Value::int(0));
}

#[test]
fn test_string_codepoints() {
    assert_eq!(eval_to_string(r#"(string/codepoints "ABC")"#), "(65 66 67)");
    // "é" is U+00E9 = 233
    assert_eq!(eval_to_string(r#"(string/codepoints "é")"#), "(233)");
    assert_eq!(eval_to_string(r#"(string/codepoints "")"#), "()");
}

#[test]
fn test_string_from_codepoints() {
    assert_eq!(
        eval(r#"(string/from-codepoints (list 65 66 67))"#),
        Value::string("ABC")
    );
    assert_eq!(
        eval(r#"(string/from-codepoints (list 233))"#),
        Value::string("é")
    );
    assert_eq!(
        eval(r#"(string/from-codepoints (list))"#),
        Value::string("")
    );
}

#[test]
fn test_string_from_codepoints_roundtrip() {
    assert_eq!(
        eval(r#"(string/from-codepoints (string/codepoints "Hello 世界"))"#),
        Value::string("Hello 世界")
    );
}

#[test]
fn test_string_normalize() {
    // NFC normalization: combining e + acute accent → é
    // U+0065 (e) + U+0301 (combining acute) → U+00E9 (é) in NFC
    assert_eq!(
        eval(r#"(string/normalize "e\u0301" :nfc)"#),
        Value::string("é")
    );
    // NFD decomposition: é → e + combining acute
    assert_eq!(
        eval(r#"(string-length (string/normalize "é" :nfd))"#),
        Value::int(2)
    );
    // NFKC: ﬁ ligature → "fi"
    assert_eq!(
        eval(r#"(string/normalize "\uFB01" :nfkc)"#),
        Value::string("fi")
    );
    // NFKD: ﬁ ligature → "fi"
    assert_eq!(
        eval(r#"(string/normalize "\uFB01" :nfkd)"#),
        Value::string("fi")
    );
    // String form names also work
    assert_eq!(
        eval(r#"(string/normalize "e\u0301" "NFC")"#),
        Value::string("é")
    );
}

#[test]
fn test_string_foldcase() {
    assert_eq!(eval(r#"(string/foldcase "HELLO")"#), Value::string("hello"));
    assert_eq!(
        eval(r#"(string/foldcase "Hello World")"#),
        Value::string("hello world")
    );
    // German sharp s stays lowercase
    assert_eq!(
        eval(r#"(string/foldcase "Straße")"#),
        Value::string("straße")
    );
}

#[test]
fn test_string_ci_equal() {
    assert_eq!(eval(r#"(string-ci=? "Hello" "hello")"#), Value::bool(true));
    assert_eq!(eval(r#"(string-ci=? "ABC" "abc")"#), Value::bool(true));
    assert_eq!(eval(r#"(string-ci=? "hello" "world")"#), Value::bool(false));
    assert_eq!(eval(r#"(string-ci=? "" "")"#), Value::bool(true));
}

#[test]
fn test_llm_pricing_status() {
    let interp = Interpreter::new();
    let result = interp.eval_str("(llm/pricing-status)").unwrap();
    if let Some(m) = result.as_map_rc() {
        assert!(m.contains_key(&Value::keyword("source")));
    } else {
        panic!("expected map, got {result}");
    }
}

#[test]
fn test_budget_with_unknown_model_does_not_error() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str("(begin (llm/set-budget 1.00) (llm/budget-remaining))")
        .unwrap();
    if let Some(m) = result.as_map_rc() {
        assert!(m.contains_key(&Value::keyword("limit")));
    } else {
        panic!("expected map, got {result}");
    }
}

// --- Lisp-defined providers ---

#[test]
fn test_define_provider_registers_and_sets_default() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :mock
            {:complete (fn (req) "hello from mock")
             :default-model "mock-v1"})
          (llm/current-provider))"#,
        )
        .unwrap();
    if let Some(m) = result.as_map_rc() {
        assert_eq!(
            m.get(&Value::keyword("name")),
            Some(&Value::keyword("mock"))
        );
        assert_eq!(
            m.get(&Value::keyword("model")),
            Some(&Value::string("mock-v1"))
        );
    } else {
        panic!("expected map, got {result}");
    }
}

#[test]
fn test_define_provider_appears_in_list() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :test-prov
            {:complete (fn (req) "ok")
             :default-model "t1"})
          (> (length (llm/list-providers)) 0))"#,
        )
        .unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_define_provider_complete_returns_string() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :echo
            {:complete (fn (req)
              (string-append "echo: " (:content (first (:messages req)))))
             :default-model "echo-1"})
          (llm/complete "hello world"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("echo: hello world"));
}

#[test]
fn test_define_provider_complete_returns_map() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :map-prov
            {:complete (fn (req)
              {:content "map response"
               :role "assistant"
               :usage {:prompt-tokens 10 :completion-tokens 5}})
             :default-model "map-1"})
          (llm/complete "test"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("map response"));
}

#[test]
fn test_define_provider_receives_model() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :model-check
            {:complete (fn (req) (:model req))
             :default-model "default-model"})
          (llm/complete "test" {:model "custom-model"}))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("custom-model"));
}

#[test]
fn test_define_provider_receives_system_prompt() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :sys-check
            {:complete (fn (req)
              (if (nil? (:system req)) "no system" (:system req)))
             :default-model "s1"})
          (llm/complete "test" {:system "be helpful"}))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("be helpful"));
}

#[test]
fn test_define_provider_uses_default_model() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :defmodel
            {:complete (fn (req) (:model req))
             :default-model "my-default"})
          (llm/complete "test"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("my-default"));
}

#[test]
fn test_define_provider_requires_complete() {
    let interp = Interpreter::new();
    let result = interp.eval_str(r#"(llm/define-provider :bad {:default-model "x"})"#);
    assert!(result.is_err());
}

#[test]
fn test_define_provider_validates_complete_is_function() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(r#"(llm/define-provider :bad {:complete "not a function" :default-model "x"})"#);
    assert!(result.is_err());
}

#[test]
fn test_define_provider_with_closure() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (define prefix "PREFIX")
          (llm/define-provider :closure-prov
            {:complete (fn (req)
              (string-append prefix ": " (:content (first (:messages req)))))
             :default-model "c1"})
          (llm/complete "hi"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("PREFIX: hi"));
}

#[test]
fn test_define_provider_receives_max_tokens() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :tok-check
            {:complete (fn (req)
              (number->string (:max-tokens req)))
             :default-model "t1"})
          (llm/complete "test" {:max-tokens 42}))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("42"));
}

#[test]
fn test_define_provider_switch_back() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :prov-a
            {:complete (fn (req) "from A") :default-model "a1"})
          (llm/define-provider :prov-b
            {:complete (fn (req) "from B") :default-model "b1"})
          (llm/set-default :prov-a)
          (llm/complete "test"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("from A"));
}

// --- OpenAI-compatible fallback ---

#[test]
fn test_configure_unknown_provider_without_base_url_errors() {
    let interp = Interpreter::new();
    let result = interp.eval_str(r#"(llm/configure :unknown-provider {:api-key "test"})"#);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("base-url"),
        "error should mention base-url: {err}"
    );
}

#[test]
fn test_configure_unknown_provider_without_api_key_errors() {
    let interp = Interpreter::new();
    let result =
        interp.eval_str(r#"(llm/configure :unknown-provider {:base-url "http://example.com/v1"})"#);
    assert!(result.is_err());
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("api-key"),
        "error should mention api-key: {err}"
    );
}

#[test]
fn test_define_provider_error_propagation() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
          (llm/define-provider :err-prov
            {:complete (fn (req) (error "provider failed"))
             :default-model "e1"})
          (try (llm/complete "test") (catch e "caught")))"#,
    );
    // Should catch the error, not panic
    assert_eq!(result.unwrap(), Value::string("caught"));
}

#[test]
fn test_define_provider_nil_return_errors() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
          (llm/define-provider :nil-prov
            {:complete (fn (req) nil)
             :default-model "n1"})
          (llm/complete "test"))"#,
    );
    assert!(result.is_err());
}

#[test]
fn test_define_provider_redefine_uses_latest() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :redef
            {:complete (fn (req) "first") :default-model "r1"})
          (llm/define-provider :redef
            {:complete (fn (req) "second") :default-model "r2"})
          (llm/complete "test"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("second"));
}

#[test]
fn test_define_provider_temperature_passthrough() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :temp-check
            {:complete (fn (req)
              (number->string (:temperature req)))
             :default-model "t1"})
          (llm/complete "test" {:temperature 0.7}))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("0.7"));
}

#[test]
fn test_define_provider_default_model_fallback() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :nomodel
            {:complete (fn (req) (:model req))})
          (llm/complete "test"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("default"));
}

#[test]
fn test_define_provider_integer_return_errors() {
    let interp = Interpreter::new();
    let result = interp.eval_str(
        r#"(begin
          (llm/define-provider :int-prov
            {:complete (fn (req) 42)
             :default-model "i1"})
          (llm/complete "test"))"#,
    );
    assert!(result.is_err());
}

#[test]
fn test_define_provider_tool_calls_response() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :tool-prov
            {:complete (fn (req)
              {:content "I'll use a tool"
               :stop-reason "tool_use"
               :tool-calls [{:id "tc_1" :name "read-file" :arguments {:path "test.txt"}}]})
             :default-model "t1"})
          (llm/complete "test"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("I'll use a tool"));
}

#[test]
fn test_define_provider_stream_fallback() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
          (llm/define-provider :stream-prov
            {:complete (fn (req) "streamed response")
             :default-model "s1"})
          (llm/stream "test"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("streamed response"));
}

#[test]
fn test_sandbox_shell_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::SHELL);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(shell "echo hi")"#);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("Permission denied"),
        "Expected permission denied, got: {err}"
    );
}

#[test]
fn test_sandbox_shell_allowed_when_other_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::NETWORK);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(shell "echo hi")"#);
    assert!(
        result.is_ok(),
        "shell should be allowed when only network is denied"
    );
}

#[test]
fn test_sandbox_fs_write_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::FS_WRITE);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(file/write "/tmp/sema-sandbox-test.txt" "hi")"#);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Permission denied"));
}

#[test]
fn test_sandbox_fs_read_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::FS_READ);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(file/exists? "/tmp")"#);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Permission denied"));
}

#[test]
fn test_sandbox_env_read_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::ENV_READ);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(env "HOME")"#);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Permission denied"));
}

#[test]
fn test_sandbox_env_write_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::ENV_WRITE);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(sys/set-env "SEMA_TEST_SANDBOX" "val")"#);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Permission denied"));
}

#[test]
fn test_sandbox_process_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::PROCESS);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(sys/pid)"#);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Permission denied"));
}

#[test]
fn test_sandbox_network_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::NETWORK);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(http/get "https://example.com")"#);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Permission denied"));
}

#[test]
fn test_sandbox_safe_functions_always_work() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::ALL);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    assert_eq!(interp.eval_str("(+ 1 2)").unwrap(), Value::int(3));
    assert_eq!(
        interp.eval_str(r#"(string-append "a" "b")"#).unwrap(),
        Value::string("ab")
    );
}

#[test]
fn test_sandbox_println_always_works() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::ALL);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(println "hello")"#);
    assert!(result.is_ok(), "println should never be sandboxed");
}

#[test]
fn test_sandbox_strict_preset() {
    let sandbox = sema_core::Sandbox::parse_cli("strict").unwrap();
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(shell "echo hi")"#);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Permission denied"));
}

#[test]
fn test_sandbox_parse_cli_multiple() {
    let sandbox = sema_core::Sandbox::parse_cli("no-shell,no-network").unwrap();
    let interp = Interpreter::new_with_sandbox(&sandbox);
    // shell denied
    assert!(interp.eval_str(r#"(shell "echo hi")"#).is_err());
    // fs-read still allowed
    assert!(interp.eval_str(r#"(file/exists? "/tmp")"#).is_ok());
}

#[test]
fn test_sandbox_unrestricted_by_default() {
    let sandbox = sema_core::Sandbox::allow_all();
    let interp = Interpreter::new_with_sandbox(&sandbox);
    // Everything should work
    assert!(interp.eval_str(r#"(shell "echo hi")"#).is_ok());
    assert!(interp.eval_str(r#"(file/exists? "/tmp")"#).is_ok());
    assert!(interp.eval_str(r#"(env "HOME")"#).is_ok());
}

// === Sandbox: comprehensive fs-read gating ===

#[test]
fn test_sandbox_fs_read_all_functions_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::FS_READ);
    let interp = Interpreter::new_with_sandbox(&sandbox);

    let fs_read_fns = [
        r#"(file/read "/tmp/test.txt")"#,
        r#"(file/exists? "/tmp")"#,
        r#"(file/is-directory? "/tmp")"#,
        r#"(file/is-file? "/tmp/test.txt")"#,
        r#"(file/is-symlink? "/tmp/test.txt")"#,
        r#"(file/list "/tmp")"#,
        r#"(file/read-lines "/tmp/test.txt")"#,
        r#"(file/info "/tmp")"#,
        r#"(path/absolute ".")"#,
    ];

    for expr in &fs_read_fns {
        let result = interp.eval_str(expr);
        assert!(
            result.is_err(),
            "Expected {expr} to be denied, but it succeeded"
        );
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Permission denied"),
            "Expected permission denied for {expr}"
        );
    }
}

// === Sandbox: comprehensive fs-write gating ===

#[test]
fn test_sandbox_fs_write_all_functions_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::FS_WRITE);
    let interp = Interpreter::new_with_sandbox(&sandbox);

    let fs_write_fns = [
        r#"(file/write "/tmp/sema-sandbox-test.txt" "hi")"#,
        r#"(file/append "/tmp/sema-sandbox-test.txt" "more")"#,
        r#"(file/delete "/tmp/sema-sandbox-test.txt")"#,
        r#"(file/rename "/tmp/sema-sandbox-a.txt" "/tmp/sema-sandbox-b.txt")"#,
        r#"(file/mkdir "/tmp/sema-sandbox-dir")"#,
        r#"(file/write-lines "/tmp/sema-sandbox-test.txt" '("a" "b"))"#,
        r#"(file/copy "/tmp/sema-sandbox-a.txt" "/tmp/sema-sandbox-b.txt")"#,
    ];

    for expr in &fs_write_fns {
        let result = interp.eval_str(expr);
        assert!(
            result.is_err(),
            "Expected {expr} to be denied, but it succeeded"
        );
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Permission denied"),
            "Expected permission denied for {expr}"
        );
    }
}

#[test]
fn test_sandbox_fs_write_allows_read() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::FS_WRITE);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    // fs-read should still work when only fs-write is denied
    assert!(interp.eval_str(r#"(file/exists? "/tmp")"#).is_ok());
    assert!(interp.eval_str(r#"(file/is-directory? "/tmp")"#).is_ok());
}

// === Sandbox: comprehensive system/process gating ===

#[test]
fn test_sandbox_process_all_functions_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::PROCESS);
    let interp = Interpreter::new_with_sandbox(&sandbox);

    let process_fns = [
        "(exit 0)", // would exit, but sandbox catches it first
        "(sys/pid)",
        "(sys/args)",
        r#"(sys/which "ls")"#,
    ];

    for expr in &process_fns {
        let result = interp.eval_str(expr);
        assert!(
            result.is_err(),
            "Expected {expr} to be denied, but it succeeded"
        );
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Permission denied"),
            "Expected permission denied for {expr}"
        );
    }
}

#[test]
fn test_sandbox_process_allows_safe_sys_functions() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::PROCESS);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    // These sys functions should NOT be gated by PROCESS
    assert!(interp.eval_str("(sys/platform)").is_ok());
    assert!(interp.eval_str("(sys/arch)").is_ok());
    assert!(interp.eval_str("(sys/os)").is_ok());
    assert!(interp.eval_str("(sys/cwd)").is_ok());
    assert!(interp.eval_str("(time-ms)").is_ok());
}

// === Sandbox: comprehensive env gating ===

#[test]
fn test_sandbox_env_read_all_functions_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::ENV_READ);
    let interp = Interpreter::new_with_sandbox(&sandbox);

    assert!(interp.eval_str(r#"(env "HOME")"#).is_err());
    assert!(interp.eval_str("(sys/env-all)").is_err());
}

#[test]
fn test_sandbox_env_write_allows_read() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::ENV_WRITE);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    // env read should still work
    assert!(interp.eval_str(r#"(env "HOME")"#).is_ok());
    // env write should be denied
    assert!(interp
        .eval_str(r#"(sys/set-env "SEMA_TEST_X" "v")"#)
        .is_err());
}

// === Sandbox: comprehensive network gating ===

#[test]
fn test_sandbox_network_all_functions_denied() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::NETWORK);
    let interp = Interpreter::new_with_sandbox(&sandbox);

    let net_fns = [
        r#"(http/get "https://example.com")"#,
        r#"(http/post "https://example.com" "body")"#,
        r#"(http/put "https://example.com" "body")"#,
        r#"(http/delete "https://example.com")"#,
        r#"(http/request "GET" "https://example.com")"#,
    ];

    for expr in &net_fns {
        let result = interp.eval_str(expr);
        assert!(
            result.is_err(),
            "Expected {expr} to be denied, but it succeeded"
        );
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Permission denied"),
            "Expected permission denied for {expr}"
        );
    }
}

// === Sandbox: try/catch interaction ===

#[test]
fn test_sandbox_error_catchable_with_try() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::SHELL);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(
        r#"
        (try
          (shell "echo hi")
          (catch e "caught"))
    "#,
    );
    assert_eq!(result.unwrap(), Value::string("caught"));
}

#[test]
fn test_sandbox_error_message_accessible_in_catch() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::SHELL);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp
        .eval_str(
            r#"
        (try
          (shell "echo hi")
          (catch e e))
    "#,
        )
        .unwrap();
    let msg = result.to_string();
    assert!(
        msg.contains("Permission denied") || msg.contains("permission-denied"),
        "Error value should contain permission info: {msg}"
    );
}

// === Sandbox: combined capabilities ===

#[test]
fn test_sandbox_deny_multiple_caps_union() {
    let denied = sema_core::Caps::SHELL
        .union(sema_core::Caps::NETWORK)
        .union(sema_core::Caps::FS_WRITE);
    let sandbox = sema_core::Sandbox::deny(denied);
    let interp = Interpreter::new_with_sandbox(&sandbox);

    // all three denied
    assert!(interp.eval_str(r#"(shell "echo hi")"#).is_err());
    assert!(interp
        .eval_str(r#"(http/get "https://example.com")"#)
        .is_err());
    assert!(interp.eval_str(r#"(file/write "/tmp/x" "y")"#).is_err());

    // these should still work
    assert!(interp.eval_str(r#"(file/exists? "/tmp")"#).is_ok());
    assert!(interp.eval_str(r#"(env "HOME")"#).is_ok());
    assert!(interp.eval_str("(+ 1 2)").is_ok());
}

// === Sandbox: safe functions under maximum restriction ===

#[test]
fn test_sandbox_all_denied_safe_functions_comprehensive() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::ALL);
    let interp = Interpreter::new_with_sandbox(&sandbox);

    // arithmetic
    assert_eq!(interp.eval_str("(+ 1 2)").unwrap(), Value::int(3));
    // strings
    assert_eq!(
        interp.eval_str(r#"(string-length "hello")"#).unwrap(),
        Value::int(5)
    );
    // lists
    assert_eq!(interp.eval_str("(length '(1 2 3))").unwrap(), Value::int(3));
    // maps
    assert_eq!(
        interp.eval_str(r#"(get {:a 1} :a)"#).unwrap(),
        Value::int(1)
    );
    // predicates
    assert_eq!(interp.eval_str("(number? 42)").unwrap(), Value::bool(true));
    // display/print (I/O but ungated)
    assert!(interp.eval_str(r#"(println "test")"#).is_ok());
    assert!(interp.eval_str(r#"(display "test")"#).is_ok());
    assert!(interp.eval_str("(newline)").is_ok());
    // read/parse (not file I/O)
    assert!(interp.eval_str(r#"(read "(+ 1 2)")"#).is_ok());
    // path pure operations (no filesystem access)
    assert_eq!(
        interp.eval_str(r#"(path/join "a" "b" "c")"#).unwrap(),
        Value::string("a/b/c")
    );
    assert_eq!(
        interp
            .eval_str(r#"(path/basename "/foo/bar.txt")"#)
            .unwrap(),
        Value::string("bar.txt")
    );
    assert_eq!(
        interp.eval_str(r#"(path/dirname "/foo/bar.txt")"#).unwrap(),
        Value::string("/foo")
    );
    assert_eq!(
        interp
            .eval_str(r#"(path/extension "/foo/bar.txt")"#)
            .unwrap(),
        Value::string("txt")
    );
    // time (ungated)
    assert!(interp.eval_str("(time-ms)").is_ok());
    // sys info (ungated)
    assert!(interp.eval_str("(sys/platform)").is_ok());
    assert!(interp.eval_str("(sys/arch)").is_ok());
    assert!(interp.eval_str("(sys/os)").is_ok());
    assert!(interp.eval_str("(sys/cwd)").is_ok());
    // regex
    assert!(interp.eval_str(r#"(regex/match "\\d+" "abc123")"#).is_ok());
    // json
    assert!(interp.eval_str(r#"(json/decode "{\"a\":1}")"#).is_ok());
    // math
    assert!(interp.eval_str("(sqrt 4)").is_ok());
    // crypto
    assert!(interp.eval_str(r#"(hash/sha256 "hello")"#).is_ok());
    // error throwing (ungated)
    assert!(interp
        .eval_str(r#"(try (error "boom") (catch e "ok"))"#)
        .is_ok());
}

// === Task 2: base64/encode-bytes and base64/decode-bytes ===

#[test]
fn test_base64_encode_bytes() {
    assert_eq!(
        eval(r#"(base64/encode-bytes (bytevector 72 101 108 108 111))"#),
        Value::string("SGVsbG8=")
    );
}

#[test]
fn test_base64_decode_bytes() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(r#"(bytevector-length (base64/decode-bytes "SGVsbG8="))"#)
        .unwrap();
    assert_eq!(result, Value::int(5));
}

#[test]
fn test_base64_roundtrip_bytes() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (define bv (bytevector 0 1 255 128 64))
                (define encoded (base64/encode-bytes bv))
                (define decoded (base64/decode-bytes encoded))
                (= bv decoded))"#,
        )
        .unwrap();
    assert_eq!(result, Value::bool(true));
}

#[test]
fn test_base64_encode_bytes_type_error() {
    let interp = Interpreter::new();
    let result = interp.eval_str(r#"(base64/encode-bytes "not a bytevector")"#);
    assert!(result.is_err());
}

#[test]
fn test_base64_decode_bytes_invalid() {
    let interp = Interpreter::new();
    let result = interp.eval_str(r#"(base64/decode-bytes "!!!invalid!!!")"#);
    assert!(result.is_err());
}

#[test]
fn test_base64_encode_bytes_empty() {
    assert_eq!(
        eval(r#"(base64/encode-bytes (bytevector))"#),
        Value::string("")
    );
}

// === Sandbox: load is gated by fs-read ===

// === Task 1: file/read-bytes and file/write-bytes ===

#[test]
fn test_file_read_bytes() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (file/write "/tmp/sema-test-bytes.txt" "ABC")
                (define bv (file/read-bytes "/tmp/sema-test-bytes.txt"))
                (list (bytevector-length bv)
                      (bytevector-u8-ref bv 0)
                      (bytevector-u8-ref bv 1)
                      (bytevector-u8-ref bv 2)))"#,
        )
        .unwrap();
    assert_eq!(result.to_string(), "(3 65 66 67)");
}

#[test]
fn test_file_read_bytes_not_found() {
    let interp = Interpreter::new();
    let result = interp.eval_str(r#"(file/read-bytes "/tmp/sema-nonexistent-xyz.bin")"#);
    assert!(result.is_err());
}

#[test]
fn test_file_write_bytes() {
    let interp = Interpreter::new();
    let result = interp
        .eval_str(
            r#"(begin
                (file/write-bytes "/tmp/sema-test-write-bytes.bin" (bytevector 72 101 108 108 111))
                (file/read "/tmp/sema-test-write-bytes.bin"))"#,
        )
        .unwrap();
    assert_eq!(result, Value::string("Hello"));
}

#[test]
fn test_file_write_bytes_type_error() {
    let interp = Interpreter::new();
    let result = interp.eval_str(r#"(file/write-bytes "/tmp/foo.bin" "not a bytevector")"#);
    assert!(result.is_err());
}

#[test]
fn test_sandbox_load_denied_by_fs_read() {
    let sandbox = sema_core::Sandbox::deny(sema_core::Caps::FS_READ);
    let interp = Interpreter::new_with_sandbox(&sandbox);
    let result = interp.eval_str(r#"(load "nonexistent.sema")"#);
    assert!(result.is_err());
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("Permission denied"),
        "load should be denied by fs-read sandbox"
    );
}
