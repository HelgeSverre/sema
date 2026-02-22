mod common;

use sema_core::Value;

// ============================================================
// get-in — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    get_in_basic: r#"(get-in {:a {:b {:c 42}}} [:a :b :c])"# => Value::int(42),
    get_in_missing_nil: r#"(get-in {:a {:b 1}} [:a :c])"# => Value::nil(),
    get_in_missing_default: r#"(get-in {:a {:b 1}} [:a :c] "default")"# => Value::string("default"),
    get_in_nil_intermediate: r#"(get-in {:a nil} [:a :b :c])"# => Value::nil(),
    get_in_empty_path: r#"(get-in {:a 1} [])"# => common::eval_tw(r#"{:a 1}"#),
}

// ============================================================
// assoc-in — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    assoc_in_basic: r#"(get-in (assoc-in {:a {:b 1}} [:a :b] 42) [:a :b])"# => Value::int(42),
    assoc_in_creates_nested: r#"(get-in (assoc-in {} [:a :b :c] 99) [:a :b :c])"# => Value::int(99),
}

// ============================================================
// update-in — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    update_in_basic: r#"(get-in (update-in {:a {:b 10}} [:a :b] (fn (x) (+ x 1))) [:a :b])"# => Value::int(11),
    update_in_missing: r#"(get-in (update-in {} [:a :b] (fn (x) (if (nil? x) 1 (+ x 1)))) [:a :b])"# => Value::int(1),
}

// ============================================================
// deep-merge — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    deep_merge_preserves: r#"(get-in (deep-merge {:a {:b 1 :c 2}} {:a {:b 99}}) [:a :c])"# => Value::int(2),
    deep_merge_overwrites: r#"(get-in (deep-merge {:a {:b 1 :c 2}} {:a {:b 99}}) [:a :b])"# => Value::int(99),
    deep_merge_non_map: r#"(:a (deep-merge {:a {:b 1}} {:a 42}))"# => Value::int(42),
    deep_merge_multiple: r#"(get-in (deep-merge {:a 1} {:b 2} {:c 3}) [:c])"# => Value::int(3),
}

// ============================================================
// Core map operations — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    map_get: "(:a {:a 1 :b 2})" => Value::int(1),
    map_assoc: "(get (assoc {:a 1} :b 2) :b)" => Value::int(2),
    map_dissoc: "(contains? (dissoc {:a 1 :b 2} :a) :a)" => Value::bool(false),
    map_merge: "(get (merge {:a 1} {:b 2}) :b)" => Value::int(2),
    map_keys: "(sort (keys {:b 2 :a 1}))" => common::eval_tw("'(:a :b)"),
    map_vals: "(sort (vals {:a 1 :b 2}))" => common::eval_tw("'(1 2)"),
    map_entries: "(length (map/entries {:a 1 :b 2}))" => Value::int(2),
    map_contains: "(contains? {:a 1} :a)" => Value::bool(true),
    map_contains_missing: "(contains? {:a 1} :b)" => Value::bool(false),
    map_count: "(count {:a 1 :b 2 :c 3})" => Value::int(3),
    map_empty: "(empty? {})" => Value::bool(true),
    map_select_keys: "(count (map/select-keys {:a 1 :b 2 :c 3} '(:a :c)))" => Value::int(2),
    map_update: "(:a (map/update {:a 1} :a (fn (x) (+ x 10))))" => Value::int(11),
}

// ============================================================
// Hashmap operations — dual eval (tree-walker + VM)
// ============================================================

dual_eval_tests! {
    hashmap_basic: "(hash-map :a 1 :b 2)" => common::eval_tw("(hash-map :a 1 :b 2)"),
    hashmap_assoc: "(get (assoc (hash-map :a 1) :c 3) :c)" => Value::int(3),
    hashmap_dissoc: "(contains? (dissoc (hash-map :a 1 :b 2) :a) :a)" => Value::bool(false),
    hashmap_keys_count: "(length (keys (hash-map :a 1 :b 2)))" => Value::int(2),
}
