mod common;

use sema_core::Value;

// ============================================================
// Destructuring — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    destructure_let_vector: "(let (([a b] '(1 2))) (+ a b))" => Value::int(3),
    destructure_let_vector_from_vec: "(let (([a b] [10 20])) (+ a b))" => Value::int(30),
    destructure_let_rest: "(let (([a & rest] '(1 2 3))) rest)" => common::eval_tw("'(2 3)"),
    destructure_let_rest_empty: "(let (([a b & rest] '(1 2))) rest)" => common::eval_tw("'()"),
    destructure_let_wildcard: "(let (([_ b] '(1 2))) b)" => Value::int(2),
    destructure_let_nested: "(let (([[a b] c] '((1 2) 3))) (+ a b c))" => Value::int(6),
    destructure_let_map_keys: "(let (({:keys [x y]} {:x 10 :y 20})) (+ x y))" => Value::int(30),
    destructure_let_map_missing: "(let (({:keys [x y]} {:x 10})) y)" => Value::nil(),
    destructure_let_star_seq: "(let* (([a b] '(1 2)) (c (+ a b))) c)" => Value::int(3),
    destructure_define_vector: "(begin (define [a b c] '(1 2 3)) (+ a b c))" => Value::int(6),
    destructure_define_map: "(begin (define {:keys [name age]} {:name \"Alice\" :age 30}) age)" => Value::int(30),
    destructure_lambda_vector: "((lambda ([a b]) (+ a b)) '(1 2))" => Value::int(3),
    destructure_lambda_map: "((lambda ({:keys [x y]}) (+ x y)) {:x 3 :y 4})" => Value::int(7),
    destructure_lambda_mixed: "((lambda (a [b c]) (+ a b c)) 10 '(20 30))" => Value::int(60),
    destructure_nested_map_in_vec: "(let (([a {:keys [b]}] (list 1 {:b 2}))) (+ a b))" => Value::int(3),
}

dual_eval_error_tests! {
    destructure_err_too_few: "(let (([a b c] '(1 2))) a)",
    destructure_err_too_many: "(let (([a b] '(1 2 3))) a)",
    destructure_err_non_list: "(let (([a b] 42)) a)",
    destructure_err_non_map: "(let (({:keys [x]} '(1 2))) x)",
}

// ============================================================
// Pattern Matching — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    match_literal_int: r#"(match 42 (42 "found") (_ "nope"))"# => Value::string("found"),
    match_literal_string: r#"(match "hello" ("hello" 1) ("world" 2) (_ 0))"# => Value::int(1),
    match_literal_keyword: r#"(match :ok (:ok "success") (:err "failure"))"# => Value::string("success"),
    match_literal_bool: r#"(match #t (#t "yes") (#f "no"))"# => Value::string("yes"),
    match_wildcard: r#"(match 99 (1 "one") (2 "two") (_ "other"))"# => Value::string("other"),
    match_symbol_binding: "(match 42 (x (+ x 8)))" => Value::int(50),
    match_vector_pattern: "(match '(1 2 3) ([a b c] (+ a b c)))" => Value::int(6),
    match_vector_rest: "(match '(1 2 3 4) ([a & rest] rest))" => common::eval_tw("'(2 3 4)"),
    match_map_keys: "(match {:x 10 :y 20} ({:keys [x y]} (+ x y)))" => Value::int(30),
    match_guard: r#"(match 5 (x when (> x 10) "big") (x when (> x 0) "small") (_ "zero"))"# => Value::string("small"),
    match_no_match_nil: r#"(match 42 (1 "one") (2 "two"))"# => Value::nil(),
    match_nested: "(match '(1 (2 3)) ([a [b c]] (+ a b c)))" => Value::int(6),
    match_nil: r#"(match nil (nil "null") (_ "other"))"# => Value::string("null"),
    match_vector_mismatch: r#"(match '(1 2 3) ([a b] "two") (_ "other"))"# => Value::string("other"),
    match_map_structural: "(match {:type :ok :val 42} ({:type :ok :val v} v) (_ nil))" => Value::int(42),
    match_map_structural_fail: r#"(match {:type :err} ({:type :ok :val v} v) (_ "fallback"))"# => Value::string("fallback"),

    // Guard + pattern failure fallthrough (regression: VM returned nil instead of trying next clause)
    match_guard_pattern_fail_fallthrough: r#"
        (match {:a 1}
          ({:a x :b y} when (> x 0) "has-both")
          ({:a x} "has-a")
          (_ "nothing"))
    "# => Value::string("has-a"),

    match_guard_pattern_fail_to_wildcard: r#"
        (match {:x 1}
          ({:x v :y w} when #t "both")
          (_ "fallback"))
    "# => Value::string("fallback"),

    match_guard_false_then_pattern_fail: r#"
        (define (ok? v) (> v 10))
        (match {:id 5}
          ({:id n} when (ok? n) "big")
          ({:id n :name s} "has-name")
          ({:id n} (+ n 100))
          (_ 0))
    "# => Value::int(105),

    match_map_guard_multi_clause: r#"
        (define (find-user id) (if (= id 1) "Alice" #f))
        (match {:method :GET :path "/users" :id 99}
          ({:method :GET :path "/users" :id id} when (find-user id)
            (find-user id))
          ({:method :GET :path "/users" :id id}
            "not-found")
          ({:method :GET :path "/users"}
            "all")
          (_ "404"))
    "# => Value::string("not-found"),

    match_map_guard_no_key_falls_to_later: r#"
        (define (find-user id) (if (= id 1) "Alice" #f))
        (match {:method :GET :path "/users"}
          ({:method :GET :path "/users" :id id} when (find-user id)
            (find-user id))
          ({:method :GET :path "/users" :id id}
            "not-found")
          ({:method :GET :path "/users"}
            "all")
          (_ "404"))
    "# => Value::string("all"),
}

// ============================================================
// Regex Literals — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    regex_literal_basic: r#"(regex/match? #"\d+" "abc123")"# => Value::bool(true),
    regex_literal_class: r#"(regex/match? #"[a-z]+" "hello")"# => Value::bool(true),
    regex_literal_anchored: r#"(regex/match? #"^hello$" "hello")"# => Value::bool(true),
}

// ============================================================
// Debug helpers — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    spy_returns_value: r#"(spy "test" 42)"# => Value::int(42),
    spy_returns_string: r#"(spy "tag" "hello")"# => Value::string("hello"),
    assert_true: "(assert #t)" => Value::bool(true),
    assert_truthy: "(assert 42)" => Value::bool(true),
    assert_with_msg: r#"(assert #t "ok")"# => Value::bool(true),
    assert_eq_ints: "(assert= 42 42)" => Value::bool(true),
    assert_eq_strings: r#"(assert= "hello" "hello")"# => Value::bool(true),
    time_returns_result: "(time (fn () (+ 1 2)))" => Value::int(3),
}

dual_eval_error_tests! {
    assert_false: "(assert #f)",
    assert_nil: "(assert nil)",
    assert_with_message: r#"(assert #f "custom error")"#,
    assert_eq_mismatch: "(assert= 1 2)",
}

// ============================================================
// Multimethods — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    // Basic dispatch on keyword
    multi_basic_dispatch: r#"
        (begin
          (defmulti area (fn (shape) (get shape :type)))
          (defmethod area :circle (fn (s) (* 3 (get s :radius) (get s :radius))))
          (defmethod area :rect (fn (s) (* (get s :width) (get s :height))))
          (area {:type :circle :radius 5}))
    "# => Value::int(75),

    multi_rect_dispatch: r#"
        (begin
          (defmulti area (fn (shape) (get shape :type)))
          (defmethod area :circle (fn (s) (* 3 (get s :radius) (get s :radius))))
          (defmethod area :rect (fn (s) (* (get s :width) (get s :height))))
          (area {:type :rect :width 4 :height 6}))
    "# => Value::int(24),

    // Default method
    multi_default_method: r#"
        (begin
          (defmulti greet (fn (x) (get x :lang)))
          (defmethod greet :en (fn (x) "hello"))
          (defmethod greet :default (fn (x) "hi"))
          (greet {:lang :fr}))
    "# => Value::string("hi"),

    // Dispatch on runtime type
    multi_type_dispatch: r#"
        (begin
          (defmulti describe (fn (x) (type x)))
          (defmethod describe :int (fn (x) "integer"))
          (defmethod describe :string (fn (x) "text"))
          (list (describe 42) (describe "hi")))
    "# => Value::list(vec![Value::string("integer"), Value::string("text")]),

    // Multi-argument dispatch
    multi_two_arg_dispatch: r#"
        (begin
          (defmulti combine (fn (a b) (list (type a) (type b))))
          (defmethod combine '(:int :int) (fn (a b) (+ a b)))
          (defmethod combine '(:string :string) (fn (a b) (string-append a b)))
          (list (combine 1 2) (combine "a" "b")))
    "# => Value::list(vec![Value::int(3), Value::string("ab")]),

    // defmethod returns nil
    multi_defmethod_returns_nil: r#"
        (begin
          (defmulti f (fn (x) x))
          (defmethod f :a (fn (x) 1)))
    "# => Value::nil(),

    // Adding methods after initial definition
    multi_open_extension: r#"
        (begin
          (defmulti op (fn (x) (get x :kind)))
          (defmethod op :add (fn (x) (+ (get x :a) (get x :b))))
          (let ((r1 (op {:kind :add :a 1 :b 2})))
            (defmethod op :mul (fn (x) (* (get x :a) (get x :b))))
            (+ r1 (op {:kind :mul :a 3 :b 4}))))
    "# => Value::int(15),

    // Dispatch on integer values
    multi_int_dispatch: r#"
        (begin
          (defmulti fizzbuzz (fn (n) (cond ((= (modulo n 15) 0) :fizzbuzz)
                                           ((= (modulo n 3) 0) :fizz)
                                           ((= (modulo n 5) 0) :buzz)
                                           (#t :num))))
          (defmethod fizzbuzz :fizzbuzz (fn (n) "FizzBuzz"))
          (defmethod fizzbuzz :fizz (fn (n) "Fizz"))
          (defmethod fizzbuzz :buzz (fn (n) "Buzz"))
          (defmethod fizzbuzz :num (fn (n) n))
          (list (fizzbuzz 15) (fizzbuzz 9) (fizzbuzz 10) (fizzbuzz 7)))
    "# => Value::list(vec![
        Value::string("FizzBuzz"),
        Value::string("Fizz"),
        Value::string("Buzz"),
        Value::int(7),
    ]),
}

// ============================================================
// String interning — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    string_intern_returns_string: r#"(string/intern "hello")"# => Value::string("hello"),
    string_intern_eq: r#"(equal? (string/intern "hello") (string/intern "hello"))"# => Value::bool(true),
    string_intern_same_pointer: r#"(eq? (string/intern "abc") (string/intern "abc"))"# => Value::bool(true),
    string_intern_different_strings: r#"(eq? (string/intern "a") (string/intern "b"))"# => Value::bool(false),
    string_intern_as_map_key: r#"
        (let ((k (string/intern "key")))
          (get {k 42} k))
    "# => Value::int(42),
}

dual_eval_error_tests! {
    string_intern_wrong_type: "(string/intern 42)",
    string_intern_no_args: "(string/intern)",
}

// ============================================================
// TOML — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    toml_decode_basic: r#"
        (let ((t (toml/decode "[package]\nname = \"test\"\nversion = \"1.0\"")))
          (:name (:package t)))
    "# => Value::string("test"),

    toml_decode_deps: r#"
        (let ((t (toml/decode "[deps]\nhttp = \"github.com/user/http\"")))
          (:http (:deps t)))
    "# => Value::string("github.com/user/http"),

    toml_decode_array: r#"
        (let ((t (toml/decode "tags = [\"a\", \"b\"]")))
          (length (:tags t)))
    "# => Value::int(2),

    toml_decode_nested: r#"
        (let ((t (toml/decode "[package]\nname = \"x\"\n\n[deps]\nfoo = \"bar\"")))
          (list (:name (:package t)) (:foo (:deps t))))
    "# => Value::list(vec![Value::string("x"), Value::string("bar")]),

    toml_decode_integer: r#"
        (let ((t (toml/decode "port = 8080")))
          (:port t))
    "# => Value::int(8080),

    toml_decode_float: r#"
        (let ((t (toml/decode "pi = 3.14")))
          (:pi t))
    "# => Value::float(3.14),

    toml_decode_bool: r#"
        (let ((t (toml/decode "debug = true")))
          (:debug t))
    "# => Value::bool(true),

    toml_decode_empty_table: r#"
        (let ((t (toml/decode "")))
          (map? t))
    "# => Value::bool(true),

    toml_encode_basic: r#"
        (string/contains? (toml/encode {:name "test"}) "name = \"test\"")
    "# => Value::bool(true),

    toml_roundtrip_simple: r#"
        (let* ((original {:name "sema" :version "1.0"})
               (encoded (toml/encode original))
               (decoded (toml/decode encoded)))
          (list (:name decoded) (:version decoded)))
    "# => Value::list(vec![Value::string("sema"), Value::string("1.0")]),
}

dual_eval_error_tests! {
    toml_decode_invalid_input: r#"(toml/decode "[invalid")"#,

    toml_encode_non_map: r#"(toml/encode "not a map")"#,

    toml_decode_wrong_type: r#"(toml/decode 42)"#,

    toml_encode_nil_value: r#"(toml/encode {:key nil})"#,
}

dual_eval_error_tests! {
    // No matching method and no default
    multi_no_method: r#"
        (begin
          (defmulti f (fn (x) x))
          (defmethod f :a (fn (x) 1))
          (f :b))
    "#,
    // defmethod on non-multimethod
    multi_defmethod_not_multi: r#"
        (begin
          (define x 42)
          (defmethod x :a (fn (x) 1)))
    "#,
    // defmulti wrong arity
    multi_defmulti_no_args: "(defmulti)",
    // defmethod wrong arity
    multi_defmethod_no_args: "(defmethod)",
}

// ============================================================
// Dialect Aliases — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    alias_mapcar: "(mapcar (fn (x) (* x 2)) '(1 2 3))" => common::eval_tw("'(2 4 6)"),
    alias_fold: "(fold + 0 '(1 2 3))" => Value::int(6),
    alias_every_q: "(every? odd? '(1 3 5))" => Value::bool(true),
    alias_any_q: "(any? even? '(1 2 3))" => Value::bool(true),
    alias_some_q: "(some? even? '(1 2 3))" => Value::bool(true),
    alias_string_join: r#"(string-join '("a" "b" "c") ",")"# => Value::string("a,b,c"),
    alias_string_split: r#"(string-split "a,b,c" ",")"# => common::eval_tw(r#"'("a" "b" "c")"#),
    alias_string_trim: r#"(string-trim "  hello  ")"# => Value::string("hello"),
    alias_hash_map_q: "(hash-map? (hash-map :a 1))" => Value::bool(true),
    alias_hash_ref: "(hash-ref {:a 1 :b 2} :b)" => Value::int(2),
    alias_type_of: "(type-of 42)" => common::eval_tw("(type 42)"),
    alias_def_simple: "(def x 42) x" => Value::int(42),
    alias_def_function: "(def (add a b) (+ a b)) (add 3 4)" => Value::int(7),
    alias_defn: "(defn add (a b) (+ a b)) (add 3 4)" => Value::int(7),
    alias_progn: "(progn (define x 10) (define y 20) (+ x y))" => Value::int(30),
}

// ============================================================
// Auto-gensym — macro hygiene tests (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    // Core hygiene: macro's x# does NOT capture user's x
    auto_gensym_basic: r#"
        (begin
          (defmacro my-let1 (val body)
            `(let ((x# ,val)) ,body))
          (let ((x 10))
            (my-let1 42 x)))
    "# => Value::int(10),

    // Same foo# within one quasiquote maps to the same gensym
    auto_gensym_consistent: r#"
        (begin
          (defmacro my-bind (val body)
            `(let ((tmp# ,val)) (+ tmp# tmp#)))
          (my-bind 21 nil))
    "# => Value::int(42),

    // Different auto-gensym names get different symbols
    auto_gensym_different_names: r#"
        (begin
          (defmacro my-bind2 (a b)
            `(let ((x# ,a) (y# ,b)) (+ x# y#)))
          (my-bind2 10 20))
    "# => Value::int(30),

    // Auto-gensym does NOT interfere with unquote
    auto_gensym_with_unquote: r#"
        (begin
          (defmacro add-one (expr)
            `(let ((tmp# ,expr)) (+ tmp# 1)))
          (add-one 41))
    "# => Value::int(42),

    // Nested macro calls get independent gensyms (no collision)
    auto_gensym_nested_calls: r#"
        (begin
          (defmacro my-inc (expr)
            `(let ((v# ,expr)) (+ v# 1)))
          (my-inc (my-inc 10)))
    "# => Value::int(12),

    // Auto-gensym symbol outside quasiquote is just a regular symbol
    auto_gensym_outside_quasiquote: r#"
        (begin
          (define x# 42)
          x#)
    "# => Value::int(42),

    // Auto-gensym works inside vectors in quasiquote
    auto_gensym_in_vector: r#"
        (begin
          (defmacro vec-bind (val)
            `(let ((v# ,val)) [v# v#]))
          (vec-bind 5))
    "# => common::eval_tw("[5 5]"),

    // x## (double hash) is NOT auto-gensym — only single trailing # triggers it
    auto_gensym_double_hash_is_regular: r#"
        (begin
          (define x## 99)
          x##)
    "# => Value::int(99),
}

// ============================================================
// Prelude hygiene — dual eval
// ============================================================

dual_eval_tests! {
    // some-> should not capture user's __v variable
    some_arrow_no_capture: r#"
        (begin
          (define __v {:name "Alice" :age 30})
          (some-> __v (:name)))
    "# => Value::string("Alice"),
}

// ============================================================
// Auto-gensym edge cases — dual eval
// ============================================================

dual_eval_tests! {
    // Auto-gensym with splicing
    auto_gensym_with_splicing: r#"
        (begin
          (defmacro my-do (. body)
            `(let ((r# nil)) ,@body r#))
          (my-do (define x 1) (define y 2)))
    "# => Value::nil(),

    // Multiple quasiquotes in same macro body get independent gensyms
    auto_gensym_multi_quasiquote: r#"
        (begin
          (defmacro double-bind (a b)
            (let ((first `(let ((x# ,a)) x#))
                  (second `(let ((x# ,b)) x#)))
              `(+ ,first ,second)))
          (double-bind 10 20))
    "# => Value::int(30),

    // Manual gensym and auto-gensym share a counter — no collision
    auto_gensym_no_collision_with_manual: r#"
        (begin
          (define s1 (gensym "x"))
          (defmacro my-m (v) `(let ((x# ,v)) x#))
          (my-m 42))
    "# => Value::int(42),

    // Deeply nested macro calls — each level gets its own gensyms
    auto_gensym_deep_nesting: r#"
        (begin
          (defmacro wrap (expr)
            `(let ((v# ,expr)) (+ v# 0)))
          (wrap (wrap (wrap (wrap (wrap 100))))))
    "# => Value::int(100),

    // Macro that introduces a binding with same name as user variable
    auto_gensym_shadowing_proof: r#"
        (begin
          (defmacro capture-test (body)
            `(let ((result# 999)) ,body))
          (let ((result# 1))
            (capture-test result#)))
    "# => Value::int(1),

    // Shared counter: gensym then auto-gensym produce different names
    auto_gensym_shared_counter_proof: r#"
        (begin
          (define a (gensym "x"))
          (define b `x#)
          (not (= a b)))
    "# => Value::bool(true),

    // 20-level deep nesting — no stack issues, all gensyms independent
    auto_gensym_deep_nesting_20: r#"
        (begin
          (defmacro wrap (expr)
            `(let ((v# ,expr)) (+ v# 0)))
          (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap (wrap 7)))))))))))))))))))))
    "# => Value::int(7),

    // Stress: 1000 sequential gensyms are all unique
    auto_gensym_1000_unique: r#"
        (begin
          (define syms (map (fn (_) (symbol->string (gensym "s"))) (range 1000)))
          (define sorted (sort syms))
          (define (has-dup? lst)
            (if (or (null? lst) (null? (cdr lst)))
              #f
              (if (= (car lst) (cadr lst))
                #t
                (has-dup? (cdr lst)))))
          (not (has-dup? sorted)))
    "# => Value::bool(true),

    // Stress: 100 auto-gensym macro invocations — all get unique bindings
    auto_gensym_100_macro_invocations: r#"
        (begin
          (defmacro inc-wrap (expr)
            `(let ((v# ,expr)) (+ v# 1)))
          (define (apply-n f n x)
            (if (= n 0) x (apply-n f (- n 1) (f x))))
          (apply-n (fn (x) (inc-wrap x)) 100 0))
    "# => Value::int(100),

    // Recursive macro that generates auto-gensyms at each recursion level
    auto_gensym_recursive_macro: r#"
        (begin
          (defmacro count-down (n)
            (if (= n 0)
              0
              `(let ((v# ,n)) (+ v# (count-down ,(- n 1))))))
          (count-down 10))
    "# => Value::int(55),

    // Multiple different auto-gensym names in one quasiquote all independent
    auto_gensym_many_names: r#"
        (begin
          (defmacro multi (a b c d)
            `(let ((w# ,a) (x# ,b) (y# ,c) (z# ,d))
               (+ w# x# y# z#)))
          (multi 1 2 3 4))
    "# => Value::int(10),

    // Auto-gensym in nested let bindings within one quasiquote
    auto_gensym_nested_lets: r#"
        (begin
          (defmacro nested-bind (a b)
            `(let ((outer# ,a))
               (let ((inner# ,b))
                 (+ outer# inner#))))
          (nested-bind 10 20))
    "# => Value::int(30),
}
