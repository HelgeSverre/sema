use sema_eval::Interpreter;
use sema_core::Value;

fn eval(input: &str) -> Value {
    let interp = Interpreter::new();
    interp.eval_str(input).expect(&format!("failed to eval: {input}"))
}

fn eval_to_string(input: &str) -> String {
    format!("{}", eval(input))
}

#[test]
fn test_arithmetic() {
    assert_eq!(eval("(+ 1 2)"), Value::Int(3));
    assert_eq!(eval("(- 10 3)"), Value::Int(7));
    assert_eq!(eval("(* 4 5)"), Value::Int(20));
    assert_eq!(eval("(/ 10 2)"), Value::Int(5));
    assert_eq!(eval("(mod 10 3)"), Value::Int(1));
    assert_eq!(eval("(+ 1 2.0)"), Value::Float(3.0));
}

#[test]
fn test_comparison() {
    assert_eq!(eval("(< 1 2)"), Value::Bool(true));
    assert_eq!(eval("(> 3 2)"), Value::Bool(true));
    assert_eq!(eval("(<= 2 2)"), Value::Bool(true));
    assert_eq!(eval("(= 42 42)"), Value::Bool(true));
    assert_eq!(eval("(not #f)"), Value::Bool(true));
}

#[test]
fn test_define_and_call() {
    assert_eq!(
        eval("(begin (define x 42) x)"),
        Value::Int(42)
    );
    assert_eq!(
        eval("(begin (define (square x) (* x x)) (square 5))"),
        Value::Int(25)
    );
}

#[test]
fn test_factorial() {
    assert_eq!(
        eval("(begin (define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1))))) (factorial 10))"),
        Value::Int(3628800)
    );
}

#[test]
fn test_lambda() {
    assert_eq!(
        eval("((lambda (x y) (+ x y)) 3 4)"),
        Value::Int(7)
    );
}

#[test]
fn test_let() {
    assert_eq!(
        eval("(let ((x 10) (y 20)) (+ x y))"),
        Value::Int(30)
    );
}

#[test]
fn test_let_star() {
    assert_eq!(
        eval("(let* ((x 10) (y (* x 2))) (+ x y))"),
        Value::Int(30)
    );
}

#[test]
fn test_cond() {
    assert_eq!(
        eval("(cond ((= 1 2) 10) ((= 1 1) 20) (else 30))"),
        Value::Int(20)
    );
}

#[test]
fn test_and_or() {
    assert_eq!(eval("(and 1 2 3)"), Value::Int(3));
    assert_eq!(eval("(and 1 #f 3)"), Value::Bool(false));
    assert_eq!(eval("(or #f #f 3)"), Value::Int(3));
    assert_eq!(eval("(or 1 2 3)"), Value::Int(1));
}

#[test]
fn test_list_operations() {
    assert_eq!(eval("(car (list 1 2 3))"), Value::Int(1));
    assert_eq!(eval_to_string("(cdr (list 1 2 3))"), "(2 3)");
    assert_eq!(eval_to_string("(cons 0 (list 1 2))"), "(0 1 2)");
    assert_eq!(eval("(length (list 1 2 3))"), Value::Int(3));
    assert_eq!(eval_to_string("(reverse (list 1 2 3))"), "(3 2 1)");
    assert_eq!(eval_to_string("(append (list 1 2) (list 3 4))"), "(1 2 3 4)");
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
    assert_eq!(
        eval("(foldl + 0 (list 1 2 3 4 5))"),
        Value::Int(15)
    );
}

#[test]
fn test_string_operations() {
    assert_eq!(
        eval("(string-length \"hello\")"),
        Value::Int(5)
    );
    assert_eq!(
        eval("(string/contains? \"hello world\" \"world\")"),
        Value::Bool(true)
    );
    assert_eq!(
        eval_to_string("(string/split \"a,b,c\" \",\")"),
        "(\"a\" \"b\" \"c\")"
    );
}

#[test]
fn test_map_data_structure() {
    assert_eq!(
        eval("(get {:a 1 :b 2} :a)"),
        Value::Int(1)
    );
    assert_eq!(
        eval("(:b {:a 1 :b 2})"),
        Value::Int(2)
    );
    assert_eq!(
        eval("(get (assoc {:a 1} :b 2) :b)"),
        Value::Int(2)
    );
    assert_eq!(
        eval_to_string("(keys {:a 1 :b 2})"),
        "(:a :b)"
    );
}

#[test]
fn test_json() {
    assert_eq!(
        eval("(json/encode {:name \"test\" :val 42})"),
        Value::string("{\"name\":\"test\",\"val\":42}")
    );
    assert_eq!(
        eval("(get (json/decode \"{\\\"x\\\": 10}\") :x)"),
        Value::Int(10)
    );
}

#[test]
fn test_quote() {
    assert_eq!(
        eval_to_string("(quote (a b c))"),
        "(a b c)"
    );
    assert_eq!(
        eval_to_string("'(a b c)"),
        "(a b c)"
    );
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
    assert_eq!(eval("(when #t 42)"), Value::Int(42));
    assert_eq!(eval("(when #f 42)"), Value::Nil);
    assert_eq!(eval("(unless #f 42)"), Value::Int(42));
    assert_eq!(eval("(unless #t 42)"), Value::Nil);
}

#[test]
fn test_predicates() {
    assert_eq!(eval("(null? nil)"), Value::Bool(true));
    assert_eq!(eval("(null? (list))"), Value::Bool(true));
    assert_eq!(eval("(list? (list 1 2))"), Value::Bool(true));
    assert_eq!(eval("(number? 42)"), Value::Bool(true));
    assert_eq!(eval("(string? \"hi\")"), Value::Bool(true));
    assert_eq!(eval("(keyword? :foo)"), Value::Bool(true));
    assert_eq!(eval("(map? {:a 1})"), Value::Bool(true));
}

#[test]
fn test_set_bang() {
    assert_eq!(
        eval("(begin (define x 1) (set! x 2) x)"),
        Value::Int(2)
    );
}

#[test]
fn test_begin() {
    assert_eq!(
        eval("(begin 1 2 3)"),
        Value::Int(3)
    );
}

#[test]
fn test_closures() {
    assert_eq!(
        eval("(begin (define (make-adder n) (lambda (x) (+ n x))) ((make-adder 5) 3))"),
        Value::Int(8)
    );
}

#[test]
fn test_range() {
    assert_eq!(
        eval_to_string("(range 5)"),
        "(0 1 2 3 4)"
    );
    assert_eq!(
        eval_to_string("(range 2 5)"),
        "(2 3 4)"
    );
}

#[test]
fn test_rest_params() {
    assert_eq!(
        eval("(begin (define (sum . args) (foldl + 0 args)) (sum 1 2 3 4 5))"),
        Value::Int(15)
    );
}

#[test]
fn test_apply() {
    assert_eq!(
        eval("(apply + (list 1 2 3))"),
        Value::Int(6)
    );
}

#[test]
fn test_math_functions() {
    assert_eq!(eval("(abs -5)"), Value::Int(5));
    assert_eq!(eval("(min 3 1 2)"), Value::Int(1));
    assert_eq!(eval("(max 3 1 2)"), Value::Int(3));
    assert_eq!(eval("(floor 3.7)"), Value::Int(3));
    assert_eq!(eval("(ceil 3.2)"), Value::Int(4));
}

#[test]
fn test_prompt_and_message() {
    let result = eval("(prompt (system \"You are helpful.\") (user \"Hello\"))");
    assert!(matches!(result, Value::Prompt(_)));

    let result = eval("(message :user \"Hello\")");
    assert!(matches!(result, Value::Message(_)));
}

#[test]
fn test_recursive_fibonacci() {
    assert_eq!(
        eval("(begin (define (fib n) (cond ((= n 0) 0) ((= n 1) 1) (else (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))"),
        Value::Int(55)
    );
}

#[test]
fn test_higher_order() {
    assert_eq!(
        eval("(begin (define (compose f g) (lambda (x) (f (g x)))) (define inc (lambda (x) (+ x 1))) (define dbl (lambda (x) (* x 2))) ((compose dbl inc) 5))"),
        Value::Int(12)
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
    assert_eq!(eval("(string->number \"42\")"), Value::Int(42));
    assert_eq!(eval("(string->number \"3.14\")"), Value::Float(3.14));
    assert_eq!(eval("(number->string 42)"), Value::string("42"));
}

#[test]
fn test_deftool() {
    let result = eval(r#"
        (begin
          (deftool add-numbers
            "Add two numbers"
            {:a {:type :number :description "First number"}
             :b {:type :number :description "Second number"}}
            (lambda (a b) (+ a b)))
          add-numbers)
    "#);
    assert!(matches!(result, Value::ToolDef(_)));
}

#[test]
fn test_defagent() {
    let result = eval(r#"
        (begin
          (deftool greet
            "Greet someone"
            {:name {:type :string}}
            (lambda (name) (string-append "Hello, " name "!")))
          (defagent greeter {:system "You greet people."
                             :tools [greet]
                             :max-turns 5})
          greeter)
    "#);
    assert!(matches!(result, Value::Agent(_)));
}

#[test]
fn test_load_special_form() {
    // Write a temp file and load it
    eval(r#"(write-file "/tmp/sema-test-load.sema" "(define loaded-value 42)")"#);
    let result = eval(r#"
        (begin
          (load "/tmp/sema-test-load.sema")
          loaded-value)
    "#);
    assert_eq!(result, Value::Int(42));
}

#[test]
fn test_defmacro() {
    assert_eq!(
        eval(r#"
            (begin
              (defmacro my-if (cond then else)
                (list 'if cond then else))
              (my-if #t 1 2))
        "#),
        Value::Int(1)
    );
}

#[test]
fn test_llm_pmap_sequential() {
    // llm/pmap should work like map for non-LLM functions
    assert_eq!(
        eval_to_string("(llm/pmap (lambda (x) (* x x)) (list 1 2 3 4))"),
        "(1 4 9 16)"
    );
}
