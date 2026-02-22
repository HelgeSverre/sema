mod common;

use sema_core::Value;

// ============================================================
// String operations — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    str_upper: r#"(string/upper "hello")"# => Value::string("HELLO"),
    str_lower: r#"(string/lower "HELLO")"# => Value::string("hello"),
    str_trim: r#"(string/trim "  hi  ")"# => Value::string("hi"),
    str_trim_left: r#"(string/trim-left "  hi  ")"# => Value::string("hi  "),
    str_trim_right: r#"(string/trim-right "  hi  ")"# => Value::string("  hi"),
    str_split: r#"(length (string/split "a,b,c" ","))"# => Value::int(3),
    str_join: r#"(string/join '("a" "b" "c") "-")"# => Value::string("a-b-c"),
    str_contains: r#"(string/contains? "hello world" "world")"# => Value::bool(true),
    str_contains_false: r#"(string/contains? "hello" "xyz")"# => Value::bool(false),
    str_starts_with: r#"(string/starts-with? "hello" "hel")"# => Value::bool(true),
    str_ends_with: r#"(string/ends-with? "hello" "llo")"# => Value::bool(true),
    str_replace: r#"(string/replace "hello world" "world" "rust")"# => Value::string("hello rust"),
    str_repeat: r#"(string/repeat "ab" 3)"# => Value::string("ababab"),
    str_reverse: r#"(string/reverse "hello")"# => Value::string("olleh"),
    str_length: r#"(string-length "hello")"# => Value::int(5),
    str_append: r#"(string-append "hello" " " "world")"# => Value::string("hello world"),
    str_substring: r#"(substring "hello" 1 3)"# => Value::string("el"),
    str_index_of: r#"(string/index-of "hello" "ll")"# => Value::int(2),
    str_capitalize: r#"(string/capitalize "hello world")"# => Value::string("Hello world"),
    str_empty: r#"(string/empty? "")"# => Value::bool(true),
    str_not_empty: r#"(string/empty? "hi")"# => Value::bool(false),
    str_pad_left: r#"(string/pad-left "42" 5 "0")"# => Value::string("00042"),
    str_pad_right: r#"(string/pad-right "42" 5 "0")"# => Value::string("42000"),
    str_chars: r#"(length (string/chars "hello"))"# => Value::int(5),
}

// ============================================================
// String conversion functions — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    str_to_number: r#"(string->number "42")"# => Value::int(42),
    str_to_symbol: r#"(symbol? (string->symbol "foo"))"# => Value::bool(true),
    number_to_str: "(number->string 42)" => Value::string("42"),
    symbol_to_str: r#"(symbol->string 'foo)"# => Value::string("foo"),
    keyword_to_str: r#"(keyword->string :foo)"# => Value::string("foo"),
    str_to_keyword: r#"(keyword? (string->keyword "foo"))"# => Value::bool(true),
}

// ============================================================
// List operations — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    list_car: "(car '(1 2 3))" => Value::int(1),
    list_cdr: "(cdr '(1 2 3))" => common::eval_tw("'(2 3)"),
    list_cons: "(cons 1 '(2 3))" => common::eval_tw("'(1 2 3)"),
    list_length: "(length '(1 2 3))" => Value::int(3),
    list_append: "(append '(1 2) '(3 4))" => common::eval_tw("'(1 2 3 4)"),
    list_reverse: "(reverse '(1 2 3))" => common::eval_tw("'(3 2 1)"),
    list_map: "(map (fn (x) (* x 2)) '(1 2 3))" => common::eval_tw("'(2 4 6)"),
    list_filter: "(filter odd? '(1 2 3 4 5))" => common::eval_tw("'(1 3 5)"),
    list_foldl: "(foldl + 0 '(1 2 3))" => Value::int(6),
    list_foldr: "(foldr cons '() '(1 2 3))" => common::eval_tw("'(1 2 3)"),
    list_sort: "(sort '(3 1 2))" => common::eval_tw("'(1 2 3)"),
    list_sort_by: "(sort-by (fn (x) (- 0 x)) '(3 1 2))" => common::eval_tw("'(3 2 1)"),
    list_flatten: "(flatten '(1 (2 3) (4 5)))" => common::eval_tw("'(1 2 3 4 5)"),
    list_flatten_deep: "(flatten-deep '(1 (2 3) (4 (5))))" => common::eval_tw("'(1 2 3 4 5)"),
    list_zip: "(zip '(1 2 3) '(4 5 6))" => common::eval_tw("'((1 4) (2 5) (3 6))"),
    list_take: "(take 2 '(1 2 3 4))" => common::eval_tw("'(1 2)"),
    list_drop: "(drop 2 '(1 2 3 4))" => common::eval_tw("'(3 4)"),
    list_nth: "(nth '(10 20 30) 1)" => Value::int(20),
    list_last: "(last '(1 2 3))" => Value::int(3),
    list_range: "(range 1 5)" => common::eval_tw("'(1 2 3 4)"),
    list_range_step: "(range 0 10 3)" => common::eval_tw("'(0 3 6 9)"),
    list_unique: "(sort (list/unique '(1 2 2 3 3 3)))" => common::eval_tw("'(1 2 3)"),
    list_find: "(list/find odd? '(2 4 5 6))" => Value::int(5),
    list_count: "(count '(1 2 3))" => Value::int(3),
    list_empty: "(empty? '())" => Value::bool(true),
    list_partition: "(length (car (partition even? '(1 2 3 4 5))))" => Value::int(2),
    list_flat_map: "(flat-map (fn (x) (list x x)) '(1 2 3))" => common::eval_tw("'(1 1 2 2 3 3)"),
    list_for_each: "(begin (define acc '()) (for-each (fn (x) (set! acc (cons x acc))) '(1 2 3)) (reverse acc))" => common::eval_tw("'(1 2 3)"),
    list_member: "(member 2 '(1 2 3))" => common::eval_tw("'(2 3)"),
    list_member_missing: "(member 5 '(1 2 3))" => Value::bool(false),
    list_reduce: "(reduce + '(1 2 3 4))" => Value::int(10),
    list_iota: "(iota 5)" => common::eval_tw("'(0 1 2 3 4)"),
    list_interpose: r#"(interpose ", " '("a" "b" "c"))"# => common::eval_tw(r#"'("a" ", " "b" ", " "c")"#),
}

// ============================================================
// Vector operations — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    vec_nth: "(nth [10 20 30] 1)" => Value::int(20),
    vec_length: "(length [1 2 3])" => Value::int(3),
    vec_to_list: "(vector->list [1 2 3])" => common::eval_tw("'(1 2 3)"),
    list_to_vec: "(vector? (list->vector '(1 2 3)))" => Value::bool(true),
}

// ============================================================
// Apply — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    apply_basic: "(apply + '(1 2 3))" => Value::int(6),
    apply_prefix: "(apply + 1 2 '(3 4))" => Value::int(10),
}
