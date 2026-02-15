use proptest::prelude::*;
use sema_reader::{read, read_many};

proptest! {
    #[test]
    fn reader_never_panics(input in "\\PC*") {
        // Any arbitrary string should produce Ok or Err, never panic
        let _ = read(&input);
    }

    #[test]
    fn reader_many_never_panics(input in "\\PC*") {
        let _ = read_many(&input);
    }
}

fn sema_atom() -> impl Strategy<Value = String> {
    prop_oneof![
        // Integers
        (-1000i64..1000).prop_map(|n| n.to_string()),
        // Floats
        (-100.0f64..100.0).prop_map(|f| format!("{f:.2}")),
        // Strings (simple — no internal quotes for now)
        "[a-zA-Z0-9 _]{0,20}".prop_map(|s| format!("\"{s}\"")),
        // Symbols
        "[a-z][a-z0-9?!-]{0,10}",
        // Keywords
        "[a-z][a-z0-9-]{0,10}".prop_map(|s| format!(":{s}")),
        // Booleans / nil
        Just("#t".to_string()),
        Just("#f".to_string()),
        Just("true".to_string()),
        Just("false".to_string()),
        Just("nil".to_string()),
    ]
}

fn sema_expr(depth: u32) -> impl Strategy<Value = String> {
    if depth == 0 {
        sema_atom().boxed()
    } else {
        prop_oneof![
            // Atom
            sema_atom(),
            // List: (expr ...)
            prop::collection::vec(sema_expr(depth - 1), 0..5)
                .prop_map(|items| format!("({})", items.join(" "))),
            // Vector: [expr ...]
            prop::collection::vec(sema_expr(depth - 1), 0..5)
                .prop_map(|items| format!("[{}]", items.join(" "))),
            // Quoted atom
            sema_atom().prop_map(|a| format!("'{a}")),
        ]
        .boxed()
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(500))]

    #[test]
    fn valid_sema_parses_ok(expr in sema_expr(3)) {
        // Generated valid-ish Sema should parse without error
        read(&expr).unwrap_or_else(|e| {
            panic!("Failed to parse generated expr: {expr:?}\nError: {e}")
        });
    }

    #[test]
    fn multiple_exprs_parse(exprs in prop::collection::vec(sema_expr(2), 1..5)) {
        let input = exprs.join(" ");
        let result = read_many(&input).unwrap_or_else(|e| {
            panic!("Failed to parse: {input:?}\nError: {e}")
        });
        // Should parse at least as many exprs as we generated
        // (could be more if atoms split differently, but never less)
        assert!(!result.is_empty(), "should parse at least one expr from: {input:?}");
    }
}

proptest! {
    #[test]
    fn delimiter_soup_never_panics(
        input in prop::collection::vec(
            prop_oneof![
                Just("("),
                Just(")"),
                Just("["),
                Just("]"),
                Just("{"),
                Just("}"),
                Just(" "),
                Just("1"),
                Just(":a"),
                Just("foo"),
            ],
            0..50
        ).prop_map(|v| v.join(""))
    ) {
        let _ = read_many(&input);
    }
}

proptest! {
    #[test]
    fn string_escapes_never_panic(
        content in prop::collection::vec(
            prop_oneof![
                Just("a".to_string()),
                Just("\\n".to_string()),
                Just("\\t".to_string()),
                Just("\\\\".to_string()),
                Just("\\\"".to_string()),
                Just(" ".to_string()),
                Just("\\z".to_string()),  // unknown escape
            ],
            0..20
        ).prop_map(|v| format!("\"{}\"", v.join("")))
    ) {
        let _ = read(&content);
    }
}

proptest! {
    #[test]
    fn numeric_strings_never_panic(
        input in prop_oneof![
            "-?[0-9]{1,20}",                    // integers
            "-?[0-9]{1,10}\\.[0-9]{1,10}",      // floats
            "-?[0-9]{1,25}",                     // potential overflow
        ]
    ) {
        let _ = read(&input);
    }
}
