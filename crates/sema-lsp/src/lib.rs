use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use serde::Serialize;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use sema_core::{Caps, Sandbox, Span, SpanMap};

mod builtin_docs;
pub(crate) mod helpers;
pub mod scope;

// Re-export public helpers for external consumers
pub use helpers::{
    analyze_document, compile_diagnostics, error_span, extract_params, extract_params_from_ast,
    extract_prefix, extract_symbol_at, find_enclosing_call, import_path_at_cursor,
    import_path_from_ast, import_paths_from_ast, parse_diagnostics, resolve_import_path,
    span_to_range, top_level_ranges, user_definitions, user_definitions_from_ast,
    user_definitions_with_spans, utf16_to_byte_offset, document_symbols_from_ast,
};

use helpers::*;

// ── Custom notification: sema/evalResult ─────────────────────────

#[derive(Debug, serde::Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct EvalResultParams {
    pub uri: Url,
    pub range: Range,
    pub kind: String,
    pub value: Option<String>,
    pub stdout: String,
    pub stderr: String,
    pub ok: bool,
    pub error: Option<String>,
    pub elapsed_ms: u64,
}

pub enum EvalResultNotification {}

impl Notification for EvalResultNotification {
    type Params = EvalResultParams;
    const METHOD: &'static str = "sema/evalResult";
}

/// Collect user-defined names from top-level `define`/`defun`/`defn`/`defmacro`/`defagent`/`deftool` forms.
#[cfg(test)]
mod tests {
    use super::*;

    // ── span_to_range ────────────────────────────────────────────

    #[test]
    fn span_to_range_converts_1_indexed_to_0() {
        let span = Span::new(1, 1, 1, 5);
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 4);
    }

    #[test]
    fn span_to_range_multiline() {
        let span = Span::new(3, 10, 5, 2);
        let range = span_to_range(&span);
        assert_eq!(range.start.line, 2);
        assert_eq!(range.start.character, 9);
        assert_eq!(range.end.line, 4);
        assert_eq!(range.end.character, 1);
    }

    // ── parse_diagnostics ────────────────────────────────────────

    #[test]
    fn valid_code_no_diagnostics() {
        let diags = parse_diagnostics("(define x 42)");
        assert!(diags.is_empty());
    }

    #[test]
    fn empty_input_no_diagnostics() {
        let diags = parse_diagnostics("");
        assert!(diags.is_empty());
    }

    #[test]
    fn unclosed_paren_produces_diagnostic() {
        let diags = parse_diagnostics("(define x");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diags[0].source.as_deref(), Some("sema"));
    }

    #[test]
    fn unterminated_string_produces_diagnostic() {
        let diags = parse_diagnostics("(define x \"hello)");
        assert_eq!(diags.len(), 1);
    }

    #[test]
    fn diagnostic_includes_hint() {
        // quote requires an expression after it
        let diags = parse_diagnostics("'");
        assert_eq!(diags.len(), 1);
        // The reader error for bare quote includes a hint
        assert!(
            diags[0].message.contains("hint:") || diags[0].message.contains("quote"),
            "message was: {}",
            diags[0].message
        );
    }

    // ── extract_prefix ───────────────────────────────────────────

    #[test]
    fn prefix_basic_symbol() {
        // cursor after 'x': (define x| 42)
        assert_eq!(extract_prefix("(define x 42)", 9), "x");
    }

    #[test]
    fn prefix_namespaced() {
        assert_eq!(extract_prefix("(string/trim s)", 12), "string/trim");
    }

    #[test]
    fn prefix_at_start_of_line() {
        assert_eq!(extract_prefix("define", 3), "def");
    }

    #[test]
    fn prefix_after_open_paren() {
        assert_eq!(extract_prefix("(def", 4), "def");
    }

    #[test]
    fn prefix_empty_after_space() {
        assert_eq!(extract_prefix("(define ", 8), "");
    }

    #[test]
    fn prefix_predicate() {
        assert_eq!(extract_prefix("(string? x)", 8), "string?");
    }

    #[test]
    fn prefix_bang() {
        assert_eq!(extract_prefix("(set! x 1)", 5), "set!");
    }

    // ── user_definitions ─────────────────────────────────────────

    #[test]
    fn user_defs_defun() {
        let defs = user_definitions("(defun foo (x) x)");
        assert_eq!(defs, vec!["foo"]);
    }

    #[test]
    fn user_defs_defn() {
        let defs = user_definitions("(defn bar (x y) (+ x y))");
        assert_eq!(defs, vec!["bar"]);
    }

    #[test]
    fn user_defs_define() {
        let defs = user_definitions("(define pi 3.14)");
        assert_eq!(defs, vec!["pi"]);
    }

    #[test]
    fn user_defs_define_function_shorthand() {
        let defs = user_definitions("(define (square x) (* x x))");
        assert_eq!(defs, vec!["square"]);
    }

    #[test]
    fn user_defs_multiple() {
        let src = "(define x 1)\n(defun f (a) a)\n(defmacro m (x) x)";
        let defs = user_definitions(src);
        assert_eq!(defs, vec!["x", "f", "m"]);
    }

    #[test]
    fn user_defs_bad_syntax_returns_empty() {
        let defs = user_definitions("(define x");
        assert!(defs.is_empty());
    }

    // ── compile_diagnostics / analyze_document ─────────────────

    #[test]
    fn compile_invalid_define_produces_warning() {
        // (define) has wrong arity — caught by the VM lowering pass
        let diags = analyze_document("(define)");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::WARNING));
        assert_eq!(diags[0].source.as_deref(), Some("sema"));
        assert!(
            diags[0].message.contains("define"),
            "expected mention of 'define', got: {}",
            diags[0].message
        );
    }

    #[test]
    fn compile_valid_code_no_diagnostics() {
        let diags = analyze_document("(define x 1) (+ x 1)");
        assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");
    }

    #[test]
    fn compile_parse_error_returns_error_not_warning() {
        let diags = analyze_document("(define x");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn compile_empty_input_no_diagnostics() {
        let diags = analyze_document("");
        assert!(diags.is_empty());
    }

    #[test]
    fn compile_empty_lambda_body_produces_warning() {
        let diags = analyze_document("(lambda (x))");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::WARNING));
    }

    // ── error recovery (multiple diagnostics) ────────────────────

    #[test]
    fn multiple_stray_closers_reports_multiple_errors() {
        let diags = analyze_document(") (define x 1) )");
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Some(DiagnosticSeverity::ERROR))
            .collect();
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
    }

    #[test]
    fn error_recovery_still_reports_single_error() {
        // Single unclosed paren still produces exactly one error
        let diags = analyze_document("(define x");
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].severity, Some(DiagnosticSeverity::ERROR));
    }

    // ── extract_symbol_at ────────────────────────────────────────

    #[test]
    fn symbol_at_cursor_middle() {
        assert_eq!(extract_symbol_at("(define foo 42)", 9), "foo");
    }

    #[test]
    fn symbol_at_cursor_start() {
        assert_eq!(extract_symbol_at("define", 0), "define");
    }

    #[test]
    fn symbol_at_cursor_namespaced() {
        assert_eq!(extract_symbol_at("(string/trim s)", 5), "string/trim");
    }

    #[test]
    fn symbol_at_cursor_end_of_symbol() {
        assert_eq!(extract_symbol_at("(define foo 42)", 10), "foo");
    }

    #[test]
    fn symbol_at_cursor_on_paren() {
        assert_eq!(extract_symbol_at("(define foo)", 0), "");
    }

    #[test]
    fn symbol_at_cursor_predicate() {
        assert_eq!(extract_symbol_at("(null? x)", 3), "null?");
    }

    #[test]
    fn symbol_at_cursor_operator_plus() {
        assert_eq!(extract_symbol_at("(+ 1 2)", 1), "+");
    }

    #[test]
    fn symbol_at_cursor_operator_lte() {
        assert_eq!(extract_symbol_at("(<= x 5)", 1), "<=");
    }

    #[test]
    fn symbol_at_cursor_operator_arrow() {
        assert_eq!(extract_symbol_at("(-> x f g)", 1), "->");
    }

    #[test]
    fn prefix_operator_plus() {
        assert_eq!(extract_prefix("(+ 1 2)", 2), "+");
    }

    #[test]
    fn prefix_operator_lte() {
        assert_eq!(extract_prefix("(<= x 5)", 3), "<=");
    }

    // ── utf16_to_byte_offset ─────────────────────────────────────

    #[test]
    fn utf16_ascii_identity() {
        assert_eq!(utf16_to_byte_offset("hello", 3), 3);
    }

    #[test]
    fn utf16_past_end() {
        assert_eq!(utf16_to_byte_offset("hi", 10), 2);
    }

    #[test]
    fn utf16_with_multibyte() {
        // "aé" — 'é' is 2 bytes in UTF-8, 1 code unit in UTF-16
        let s = "aéb";
        assert_eq!(utf16_to_byte_offset(s, 0), 0); // 'a'
        assert_eq!(utf16_to_byte_offset(s, 1), 1); // 'é' starts at byte 1
        assert_eq!(utf16_to_byte_offset(s, 2), 3); // 'b' starts at byte 3
    }

    #[test]
    fn utf16_with_emoji() {
        // "a🌍b" — '🌍' is 4 bytes in UTF-8, 2 code units in UTF-16
        let s = "a🌍b";
        assert_eq!(utf16_to_byte_offset(s, 0), 0); // 'a'
        assert_eq!(utf16_to_byte_offset(s, 1), 1); // '🌍' starts at byte 1
        assert_eq!(utf16_to_byte_offset(s, 3), 5); // 'b' starts at byte 5
    }

    #[test]
    fn symbol_at_after_multibyte_comment() {
        // "; é\n(define x 1)" — symbol 'define' after multibyte char
        let line = "(define x 1)";
        assert_eq!(extract_symbol_at(line, 1), "define");
    }

    // ── span helpers ─────────────────────────────────────────────

    #[test]
    fn span_contains_basic() {
        let outer = sema_core::Span::new(1, 1, 3, 10);
        let inner = sema_core::Span::new(2, 5, 2, 8);
        assert!(span_contains(&outer, &inner));
    }

    #[test]
    fn span_contains_same() {
        let span = sema_core::Span::new(1, 1, 1, 10);
        assert!(span_contains(&span, &span));
    }

    #[test]
    fn span_contains_outside() {
        let outer = sema_core::Span::new(1, 1, 1, 10);
        let inner = sema_core::Span::new(2, 1, 2, 5);
        assert!(!span_contains(&outer, &inner));
    }

    #[test]
    fn find_name_span_basic() {
        let sym_spans = vec![
            ("define".to_string(), sema_core::Span::new(1, 2, 1, 8)),
            ("foo".to_string(), sema_core::Span::new(1, 9, 1, 12)),
        ];
        let form_span = sema_core::Span::new(1, 1, 1, 20);
        let result = find_name_span("foo", &form_span, &sym_spans);
        assert!(result.is_some());
        let range = result.unwrap();
        assert_eq!(range.start.line, 0); // LSP is 0-indexed
        assert_eq!(range.start.character, 8); // col 9 → character 8
    }

    #[test]
    fn find_name_span_not_in_form() {
        let sym_spans = vec![("foo".to_string(), sema_core::Span::new(5, 1, 5, 4))];
        let form_span = sema_core::Span::new(1, 1, 1, 20);
        assert!(find_name_span("foo", &form_span, &sym_spans).is_none());
    }

    // ── precise name spans ─────────────────────────────────────

    #[test]
    fn precise_name_span_defun() {
        // "(defun foo (x) x)" — "foo" is at col 8-11 (1-indexed)
        let defs = user_definitions_with_spans("(defun foo (x) x)");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].0, "foo");
        let range = defs[0].1.unwrap();
        // LSP is 0-indexed, so col 8 → character 7
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 7);
        // Span should cover just "foo" (3 chars), not the whole form
        assert_eq!(range.end.character - range.start.character, 3);
    }

    #[test]
    fn precise_name_span_define() {
        // "(define x 42)" — "x" is at col 9 (1-indexed)
        let defs = user_definitions_with_spans("(define x 42)");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].0, "x");
        let range = defs[0].1.unwrap();
        assert_eq!(range.start.character, 8); // col 9 → character 8
        assert_eq!(range.end.character - range.start.character, 1); // "x" = 1 char
    }

    #[test]
    fn precise_name_span_define_function_shorthand() {
        // "(define (square x) (* x x))" — "square" is inside the signature list
        let defs = user_definitions_with_spans("(define (square x) (* x x))");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].0, "square");
        let range = defs[0].1.unwrap();
        // "square" starts at col 10 (1-indexed) → character 9
        assert_eq!(range.start.character, 9);
        assert_eq!(range.end.character - range.start.character, 6); // "square" = 6 chars
    }

    // ── user_definitions_with_spans ──────────────────────────────

    #[test]
    fn user_defs_with_spans_basic() {
        let defs = user_definitions_with_spans("(defun foo (x) x)");
        assert_eq!(defs.len(), 1);
        assert_eq!(defs[0].0, "foo");
        assert!(defs[0].1.is_some(), "should have a span");
    }

    #[test]
    fn user_defs_with_spans_multiple() {
        let src = "(define x 1)\n(defun f (a) a)";
        let defs = user_definitions_with_spans(src);
        assert_eq!(defs.len(), 2);
        assert_eq!(defs[0].0, "x");
        assert_eq!(defs[1].0, "f");
        // Both should have spans
        assert!(defs[0].1.is_some());
        assert!(defs[1].1.is_some());
        // Second def should be on a later line
        let r0 = defs[0].1.unwrap();
        let r1 = defs[1].1.unwrap();
        assert!(r1.start.line > r0.start.line);
    }

    #[test]
    fn user_defs_with_spans_bad_syntax() {
        let defs = user_definitions_with_spans("(define x");
        assert!(defs.is_empty());
    }

    // ── extract_params ───────────────────────────────────────────

    #[test]
    fn extract_params_defun() {
        let params = extract_params("(defun add (a b) (+ a b))", "add");
        assert!(params.is_some());
        assert!(params.unwrap().contains("a"));
    }

    #[test]
    fn extract_params_define_shorthand() {
        let params = extract_params("(define (square x) (* x x))", "square");
        assert!(params.is_some());
        assert!(params.unwrap().contains("x"));
    }

    #[test]
    fn extract_params_not_found() {
        let params = extract_params("(defun foo (x) x)", "bar");
        assert!(params.is_none());
    }

    #[test]
    fn extract_params_define_variable() {
        // (define x 42) is not a function — no params
        let params = extract_params("(define x 42)", "x");
        assert!(params.is_none());
    }

    // ── import_path_at_cursor ────────────────────────────────────

    #[test]
    fn import_path_on_import_line() {
        let src = "(import \"utils.sema\")";
        let path = import_path_at_cursor(src, 0, 10);
        assert_eq!(path, Some("utils.sema".to_string()));
    }

    #[test]
    fn import_path_on_load_line() {
        let src = "(load \"config.sema\")";
        let path = import_path_at_cursor(src, 0, 10);
        assert_eq!(path, Some("config.sema".to_string()));
    }

    #[test]
    fn import_path_wrong_line() {
        let src = "(define x 1)\n(import \"utils.sema\")";
        // cursor on first line, not the import
        let path = import_path_at_cursor(src, 0, 5);
        assert!(path.is_none());
    }

    #[test]
    fn import_path_on_correct_line_multiline() {
        let src = "(define x 1)\n(import \"utils.sema\")";
        let path = import_path_at_cursor(src, 1, 10);
        assert_eq!(path, Some("utils.sema".to_string()));
    }

    // ── resolve_import_path ──────────────────────────────────────

    #[test]
    fn resolve_relative_path() {
        let uri = Url::parse("file:///project/src/main.sema").unwrap();
        let resolved = resolve_import_path(&uri, "utils.sema");
        assert_eq!(resolved, Some(PathBuf::from("/project/src/utils.sema")));
    }

    #[test]
    fn resolve_absolute_path() {
        let uri = Url::parse("file:///project/src/main.sema").unwrap();
        let resolved = resolve_import_path(&uri, "/lib/utils.sema");
        assert_eq!(resolved, Some(PathBuf::from("/lib/utils.sema")));
    }

    // ── import_paths_from_ast ────────────────────────────────────

    #[test]
    fn import_paths_extracts_imports() {
        let src = "(import \"utils.sema\")\n(import \"lib.sema\")\n(define x 1)";
        let (ast, _) = sema_reader::read_many_with_spans(src).unwrap();
        let paths = import_paths_from_ast(&ast);
        assert_eq!(paths, vec!["utils.sema", "lib.sema"]);
    }

    #[test]
    fn import_paths_extracts_loads() {
        let src = "(load \"config.sema\")\n(define x 1)";
        let (ast, _) = sema_reader::read_many_with_spans(src).unwrap();
        let paths = import_paths_from_ast(&ast);
        assert_eq!(paths, vec!["config.sema"]);
    }

    #[test]
    fn import_paths_empty_when_no_imports() {
        let src = "(define x 1)\n(defun f (a) a)";
        let (ast, _) = sema_reader::read_many_with_spans(src).unwrap();
        let paths = import_paths_from_ast(&ast);
        assert!(paths.is_empty());
    }

    #[test]
    fn import_paths_mixed() {
        let src = "(import \"a.sema\")\n(load \"b.sema\")\n(import \"c.sema\" (foo bar))";
        let (ast, _) = sema_reader::read_many_with_spans(src).unwrap();
        let paths = import_paths_from_ast(&ast);
        assert_eq!(paths, vec!["a.sema", "b.sema", "c.sema"]);
    }

    // ── find_enclosing_call ──────────────────────────────────────

    #[test]
    fn enclosing_call_simple() {
        // (foo |) — cursor after space, 0 args
        let result = find_enclosing_call("(foo )", 0, 5);
        assert_eq!(result, Some(("foo".to_string(), 0)));
    }

    #[test]
    fn enclosing_call_one_arg() {
        // (foo bar |) — cursor after bar, 1 complete arg
        let result = find_enclosing_call("(foo bar )", 0, 9);
        assert_eq!(result, Some(("foo".to_string(), 1)));
    }

    #[test]
    fn enclosing_call_two_args() {
        // (foo bar baz |)
        let result = find_enclosing_call("(foo bar baz )", 0, 13);
        assert_eq!(result, Some(("foo".to_string(), 2)));
    }

    #[test]
    fn enclosing_call_nested() {
        // (foo (bar |)) — cursor inside nested call
        let result = find_enclosing_call("(foo (bar ))", 0, 10);
        assert_eq!(result, Some(("bar".to_string(), 0)));
    }

    #[test]
    fn enclosing_call_after_nested() {
        // (foo (bar 1) |) — cursor after completed nested expr
        let result = find_enclosing_call("(foo (bar 1) )", 0, 13);
        assert_eq!(result, Some(("foo".to_string(), 1)));
    }

    #[test]
    fn enclosing_call_multiline() {
        let src = "(defun add\n  (a b)\n  (+ a b))";
        // cursor on line 2, col 7: inside (+ a |b)
        let result = find_enclosing_call(src, 2, 7);
        assert_eq!(result, Some(("+".to_string(), 1)));
    }

    #[test]
    fn enclosing_call_string_arg() {
        // (foo "hello" |) — string counts as one arg
        let result = find_enclosing_call("(foo \"hello\" )", 0, 13);
        assert_eq!(result, Some(("foo".to_string(), 1)));
    }

    #[test]
    fn enclosing_call_in_vector() {
        // [1 2 |] — inside vector, not a call
        let result = find_enclosing_call("[1 2 ]", 0, 5);
        assert!(result.is_none());
    }

    #[test]
    fn enclosing_call_empty_parens() {
        // (|) — cursor in empty parens, no function name
        let result = find_enclosing_call("()", 0, 1);
        assert!(result.is_none());
    }

    #[test]
    fn enclosing_call_with_comment() {
        // Paren in comment should be ignored
        let src = "; (not a call\n(foo bar )";
        let result = find_enclosing_call(src, 1, 9);
        assert_eq!(result, Some(("foo".to_string(), 1)));
    }

    #[test]
    fn enclosing_call_string_with_paren() {
        // Paren inside string should be ignored
        let src = "(foo \"(not\" bar )";
        let result = find_enclosing_call(src, 0, 16);
        assert_eq!(result, Some(("foo".to_string(), 2)));
    }

    // ── parse_param_names ────────────────────────────────────────

    #[test]
    fn param_names_simple() {
        assert_eq!(parse_param_names("(a b c)"), vec!["a", "b", "c"]);
    }

    #[test]
    fn param_names_single() {
        assert_eq!(parse_param_names("(x)"), vec!["x"]);
    }

    #[test]
    fn param_names_variadic() {
        assert_eq!(parse_param_names("(a b . rest)"), vec!["a", "b", "rest"]);
    }

    #[test]
    fn param_names_empty() {
        let result: Vec<String> = parse_param_names("()");
        assert!(result.is_empty());
    }

    // ── document_symbols_from_ast ────────────────────────────────

    #[test]
    fn doc_symbols_defun() {
        let src = "(defun foo (x) x)";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert!(symbols[0].detail.is_some());
    }

    #[test]
    fn doc_symbols_define_variable() {
        let src = "(define x 42)";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "x");
        assert_eq!(symbols[0].kind, SymbolKind::VARIABLE);
        assert!(symbols[0].detail.is_none());
    }

    #[test]
    fn doc_symbols_define_function_shorthand() {
        let src = "(define (square x) (* x x))";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "square");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn doc_symbols_defmacro() {
        let src = "(defmacro unless (test body) `(if (not ,test) ,body))";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "unless");
        assert_eq!(symbols[0].kind, SymbolKind::OPERATOR);
    }

    #[test]
    fn doc_symbols_multiple() {
        let src = "(define x 1)\n(defun f (a) a)\n(defmacro m (x) x)";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 3);
        assert_eq!(symbols[0].name, "x");
        assert_eq!(symbols[1].name, "f");
        assert_eq!(symbols[2].name, "m");
    }

    #[test]
    fn doc_symbols_no_defs() {
        let src = "(+ 1 2)\n(println \"hello\")";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert!(symbols.is_empty());
    }

    #[test]
    fn doc_symbols_selection_range_is_name() {
        let src = "(defun foo (x) x)";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 1);
        // selection_range should be just "foo", not the whole form
        let sel = symbols[0].selection_range;
        assert_eq!(sel.end.character - sel.start.character, 3); // "foo" = 3 chars
    }

    // ── extract_params_from_doc ──────────────────────────────────

    #[test]
    fn extract_params_from_doc_simple() {
        let doc = "Do something.\n\n```sema\n(foo a b c)\n```";
        let params = extract_params_from_doc(doc, "foo");
        assert_eq!(params, Some(vec!["a".into(), "b".into(), "c".into()]));
    }

    #[test]
    fn extract_params_from_doc_nested_parens() {
        let doc = "Doc.\n\n```sema\n(foo (+ 1 2) y)\n```";
        let params = extract_params_from_doc(doc, "foo");
        assert_eq!(params, Some(vec!["(+ 1 2)".into(), "y".into()]));
    }

    #[test]
    fn extract_params_from_doc_no_match() {
        let doc = "Doc.\n\n```sema\n(bar a b)\n```";
        assert!(extract_params_from_doc(doc, "foo").is_none());
    }

    #[test]
    fn extract_params_from_doc_no_code_block() {
        let doc = "Just a description, no code.";
        assert!(extract_params_from_doc(doc, "foo").is_none());
    }

    #[test]
    fn extract_params_from_doc_zero_arg_call() {
        // (foo) has no args after the name
        let doc = "Doc.\n\n```sema\n(foo)\n```";
        assert!(extract_params_from_doc(doc, "foo").is_none());
    }

    #[test]
    fn extract_params_from_doc_string_arg() {
        let doc = "Doc.\n\n```sema\n(foo \"hello\" x)\n```";
        let params = extract_params_from_doc(doc, "foo");
        assert_eq!(params, Some(vec!["\"hello\"".into(), "x".into()]));
    }

    // ── find_arg_positions_in_form ───────────────────────────────

    #[test]
    fn arg_positions_simple() {
        let text = "(foo a b c)";
        let lines: Vec<&str> = text.lines().collect();
        let span = sema_core::Span::new(1, 1, 1, 11);
        let positions = find_arg_positions_in_form(&span, &lines, 3);
        assert_eq!(positions.len(), 3);
        // 'a' at col 5 (0-indexed)
        assert_eq!(positions[0], (0, 5));
        // 'b' at col 7
        assert_eq!(positions[1], (0, 7));
        // 'c' at col 9
        assert_eq!(positions[2], (0, 9));
    }

    #[test]
    fn arg_positions_with_nested() {
        let text = "(foo (+ 1 2) x)";
        let lines: Vec<&str> = text.lines().collect();
        let span = sema_core::Span::new(1, 1, 1, 15);
        let positions = find_arg_positions_in_form(&span, &lines, 2);
        assert_eq!(positions.len(), 2);
        // '(' of nested form at col 5
        assert_eq!(positions[0], (0, 5));
        // 'x' at col 13
        assert_eq!(positions[1], (0, 13));
    }

    #[test]
    fn arg_positions_with_string() {
        let text = "(foo \"hello\" x)";
        let lines: Vec<&str> = text.lines().collect();
        let span = sema_core::Span::new(1, 1, 1, 15);
        let positions = find_arg_positions_in_form(&span, &lines, 2);
        assert_eq!(positions.len(), 2);
        // '"' at col 5
        assert_eq!(positions[0], (0, 5));
        // 'x' at col 13
        assert_eq!(positions[1], (0, 13));
    }

    #[test]
    fn arg_positions_multiline() {
        let text = "(foo\n  a\n  b)";
        let lines: Vec<&str> = text.lines().collect();
        let span = sema_core::Span::new(1, 1, 3, 4);
        let positions = find_arg_positions_in_form(&span, &lines, 2);
        assert_eq!(positions.len(), 2);
        // 'a' on line 1 (0-indexed) col 2
        assert_eq!(positions[0], (1, 2));
        // 'b' on line 2 col 2
        assert_eq!(positions[1], (2, 2));
    }

    #[test]
    fn arg_positions_max_args_limit() {
        let text = "(foo a b c d)";
        let lines: Vec<&str> = text.lines().collect();
        let span = sema_core::Span::new(1, 1, 1, 13);
        let positions = find_arg_positions_in_form(&span, &lines, 2);
        // Only 2 positions even though there are 4 args
        assert_eq!(positions.len(), 2);
    }

    #[test]
    fn arg_positions_no_args() {
        let text = "(foo)";
        let lines: Vec<&str> = text.lines().collect();
        let span = sema_core::Span::new(1, 1, 1, 5);
        let positions = find_arg_positions_in_form(&span, &lines, 5);
        assert!(positions.is_empty());
    }

    #[test]
    fn arg_positions_with_comment() {
        let text = "(foo a ; comment\n  b)";
        let lines: Vec<&str> = text.lines().collect();
        let span = sema_core::Span::new(1, 1, 2, 4);
        let positions = find_arg_positions_in_form(&span, &lines, 2);
        assert_eq!(positions.len(), 2);
        assert_eq!(positions[0], (0, 5)); // 'a'
        assert_eq!(positions[1], (1, 2)); // 'b'
    }

    // ── top_level_ranges ─────────────────────────────────────────

    #[test]
    fn top_level_ranges_basic() {
        let src = "(define x 1)\n(defun f (a) a)";
        let (ast, span_map) = sema_reader::read_many_with_spans(src).unwrap();
        let ranges = top_level_ranges(&ast, &span_map);
        assert_eq!(ranges.len(), 2);
        assert_eq!(ranges[0].0, 0); // first form
        assert_eq!(ranges[1].0, 1); // second form
        assert_eq!(ranges[0].1.start.line, 0);
        assert_eq!(ranges[1].1.start.line, 1);
    }

    #[test]
    fn top_level_ranges_empty() {
        let src = "";
        let (ast, span_map) = sema_reader::read_many_with_spans(src).unwrap();
        let ranges = top_level_ranges(&ast, &span_map);
        assert!(ranges.is_empty());
    }

    // ── format_error_message ─────────────────────────────────────

    #[test]
    fn format_error_with_hint_and_note() {
        // Create an error with hint
        let err = SemaError::eval("test error").with_hint("try this instead");
        let msg = format_error_message(&err);
        assert!(msg.contains("test error"), "msg: {msg}");
        assert!(msg.contains("hint: try this instead"), "msg: {msg}");
    }

    // ── extract_symbol_at edge cases ──────────────────────────────

    #[test]
    fn symbol_at_empty_string() {
        assert_eq!(extract_symbol_at("", 0), "");
    }

    #[test]
    fn symbol_at_whitespace_only() {
        assert_eq!(extract_symbol_at("   ", 1), "");
    }

    #[test]
    fn symbol_at_end_of_string() {
        assert_eq!(extract_symbol_at("foo", 3), "foo");
    }

    #[test]
    fn symbol_at_between_parens() {
        assert_eq!(extract_symbol_at("()foo()", 2), "foo");
    }

    #[test]
    fn symbol_at_hash_symbol() {
        // '#' is a valid symbol char in Sema
        assert_eq!(extract_symbol_at("(#t)", 1), "#t");
    }

    #[test]
    fn prefix_empty_string() {
        assert_eq!(extract_prefix("", 0), "");
    }

    #[test]
    fn prefix_only_paren() {
        assert_eq!(extract_prefix("(", 1), "");
    }

    #[test]
    fn prefix_hash_lambda() {
        // '#' and '(' are not symbol chars, so prefix from col 2 is empty
        assert_eq!(extract_prefix("#(+ 1 %)", 2), "");
    }

    // ── document_symbols edge cases ──────────────────────────────

    #[test]
    fn doc_symbols_defagent() {
        let src = "(defagent my-agent :model \"claude\")";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "my-agent");
        assert_eq!(symbols[0].kind, SymbolKind::CLASS);
    }

    #[test]
    fn doc_symbols_deftool() {
        let src = "(deftool get-weather (location) \"Get weather\" location)";
        let (ast, span_map, sym_spans) = sema_reader::read_many_with_symbol_spans(src).unwrap();
        let symbols = document_symbols_from_ast(&ast, &span_map, &sym_spans);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "get-weather");
        assert_eq!(symbols[0].kind, SymbolKind::METHOD);
    }

    // ── find_enclosing_call edge cases ───────────────────────────

    #[test]
    fn enclosing_call_deeply_nested() {
        // (a (b (c |))) — cursor at innermost
        let result = find_enclosing_call("(a (b (c )))", 0, 9);
        assert_eq!(result, Some(("c".to_string(), 0)));
    }

    #[test]
    fn enclosing_call_cursor_on_func_name() {
        // Cursor on the function name itself (mid-name), no args yet
        let result = find_enclosing_call("(foo)", 0, 3);
        assert_eq!(result, Some(("fo".to_string(), 0)));
    }

    #[test]
    fn enclosing_call_empty_input() {
        assert!(find_enclosing_call("", 0, 0).is_none());
    }

    #[test]
    fn enclosing_call_escaped_string() {
        // String with escaped quote shouldn't break paren tracking
        let src = r#"(foo "he\"llo" bar )"#;
        let result = find_enclosing_call(src, 0, 19);
        assert_eq!(result, Some(("foo".to_string(), 2)));
    }

    #[test]
    fn enclosing_call_keyword_function() {
        // (:name person) — keyword in call position
        let result = find_enclosing_call("(:name person)", 0, 13);
        assert_eq!(result, Some((":name".to_string(), 0)));
    }

    // ── utf16_to_byte_offset edge cases ──────────────────────────

    #[test]
    fn utf16_empty_string() {
        assert_eq!(utf16_to_byte_offset("", 0), 0);
    }

    #[test]
    fn utf16_zero_offset() {
        assert_eq!(utf16_to_byte_offset("hello", 0), 0);
    }

    #[test]
    fn utf16_exact_end() {
        assert_eq!(utf16_to_byte_offset("abc", 3), 3);
    }

    #[test]
    fn utf16_surrogate_pair_middle() {
        // 🌍 is a surrogate pair in UTF-16 (2 code units), 4 bytes in UTF-8
        // "a" = 1 byte, "🌍" = 4 bytes, "b" = 1 byte
        // UTF-16 offset 2 is mid-emoji (second code unit of surrogate pair)
        // The function loops: offset 0→byte 0 ('a', 1 unit), offset 1→byte 1 ('🌍', 2 units),
        // then utf16_count=3 ≥ 2, so it never returns early → falls through to s.len()=6.
        // But actually: at byte_idx=1 ('🌍'), utf16_count is 1 < 2, so it adds 2 → utf16_count=3.
        // Next iteration: byte_idx=5 ('b'), utf16_count=3 ≥ 2 → returns 5.
        let s = "a🌍b";
        assert_eq!(utf16_to_byte_offset(s, 2), 5);
    }

    // ── parse_param_names edge cases ─────────────────────────────

    #[test]
    fn param_names_no_parens() {
        // Bare names without parens
        assert_eq!(parse_param_names("a b c"), vec!["a", "b", "c"]);
    }

    #[test]
    fn param_names_with_comments() {
        // Comments in multiline param string
        assert_eq!(
            parse_param_names("(a ; first\n b)"),
            vec!["a", "b"]
        );
    }

    #[test]
    fn param_names_extra_whitespace() {
        assert_eq!(parse_param_names("(  a   b  )"), vec!["a", "b"]);
    }

    // ── analyze_document edge cases ──────────────────────────────

    #[test]
    fn analyze_nested_valid_forms() {
        let diags = analyze_document("(defun f (x) (if (> x 0) x (- x)))");
        assert!(diags.is_empty(), "got: {diags:?}");
    }

    #[test]
    fn analyze_multiple_top_level_forms() {
        let src = "(define x 1)\n(define y 2)\n(+ x y)";
        let diags = analyze_document(src);
        assert!(diags.is_empty(), "got: {diags:?}");
    }

    #[test]
    fn analyze_comments_only() {
        let diags = analyze_document("; just a comment\n; another one");
        assert!(diags.is_empty(), "got: {diags:?}");
    }
}

/// Incremental workspace scanner state.
/// Walks directories one at a time, collecting `.sema` files and parsing them,
/// so the backend can yield to interactive requests between directories.
struct WorkspaceScanner {
    /// Directories remaining to visit.
    dir_stack: Vec<PathBuf>,
    /// Canonical paths already visited (symlink cycle protection).
    visited: std::collections::HashSet<PathBuf>,
    /// Files from the current directory not yet parsed (for batching large dirs).
    pending_files: Vec<PathBuf>,
}

impl WorkspaceScanner {
    fn new(root: &Path) -> Self {
        let mut visited = std::collections::HashSet::new();
        let canonical_root = std::fs::canonicalize(root).unwrap_or_else(|_| root.to_path_buf());
        visited.insert(canonical_root.clone());
        WorkspaceScanner {
            dir_stack: vec![canonical_root],
            visited,
            pending_files: Vec::new(),
        }
    }

    /// Process the next directory on the stack.
    /// Returns the `.sema` files found in that single directory.
    /// Returns `None` when the scan is complete (no more directories).
    fn next_dir(&mut self) -> Option<Vec<PathBuf>> {
        let dir = self.dir_stack.pop()?;
        let entries = match std::fs::read_dir(&dir) {
            Ok(e) => e,
            Err(_) => return Some(Vec::new()),
        };
        let mut files = Vec::new();
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy();
            // Skip hidden dirs, target, node_modules, .git
            if name_str.starts_with('.') || name_str == "target" || name_str == "node_modules" {
                continue;
            }
            // Follow symlinks for file discovery; cycles are detected via canonicalize + visited set
            let meta = match entry.metadata() {
                Ok(m) => m,
                Err(_) => continue,
            };
            let path = entry.path();
            if meta.is_dir() {
                if let Ok(canonical) = std::fs::canonicalize(&path) {
                    if self.visited.insert(canonical) {
                        self.dir_stack.push(path);
                    }
                }
            } else if meta.is_file()
                && path.extension().and_then(|e| e.to_str()) == Some("sema")
            {
                files.push(path);
            }
        }
        Some(files)
    }
}

// ── Backend thread messages ──────────────────────────────────────

enum LspRequest {
    /// Document opened or changed — reparse and publish diagnostics.
    DocumentChanged { uri: Url, text: String },
    /// Document closed — remove from cache and clear diagnostics.
    DocumentClosed { uri: Url },
    /// Completion request.
    Complete {
        uri: Url,
        position: Position,
        reply: tokio::sync::oneshot::Sender<Vec<CompletionItem>>,
    },
    /// Go-to-definition request.
    GotoDefinition {
        uri: Url,
        position: Position,
        reply: tokio::sync::oneshot::Sender<Option<GotoDefinitionResponse>>,
    },
    /// Hover request.
    Hover {
        uri: Url,
        position: Position,
        reply: tokio::sync::oneshot::Sender<Option<Hover>>,
    },
    /// CodeLens request.
    CodeLens {
        uri: Url,
        reply: tokio::sync::oneshot::Sender<Vec<CodeLens>>,
    },
    /// Find all references request.
    References {
        uri: Url,
        position: Position,
        reply: tokio::sync::oneshot::Sender<Vec<Location>>,
    },
    /// Document symbols request.
    DocumentSymbols {
        uri: Url,
        reply: tokio::sync::oneshot::Sender<DocumentSymbolResponse>,
    },
    /// Workspace symbols request.
    WorkspaceSymbols {
        query: String,
        #[allow(deprecated)]
        reply: tokio::sync::oneshot::Sender<Vec<SymbolInformation>>,
    },
    /// Signature help request.
    SignatureHelp {
        uri: Url,
        position: Position,
        reply: tokio::sync::oneshot::Sender<Option<SignatureHelp>>,
    },
    /// Rename request.
    Rename {
        uri: Url,
        position: Position,
        new_name: String,
        reply: tokio::sync::oneshot::Sender<Option<WorkspaceEdit>>,
    },
    /// Prepare rename request.
    PrepareRename {
        uri: Url,
        position: Position,
        reply: tokio::sync::oneshot::Sender<Option<PrepareRenameResponse>>,
    },
    /// Semantic tokens request.
    SemanticTokensFull {
        uri: Url,
        reply: tokio::sync::oneshot::Sender<Option<SemanticTokensResult>>,
    },
    /// Folding ranges request.
    FoldingRange {
        uri: Url,
        reply: tokio::sync::oneshot::Sender<Vec<FoldingRange>>,
    },
    /// Inlay hints request.
    InlayHints {
        uri: Url,
        range: Range,
        reply: tokio::sync::oneshot::Sender<Option<Vec<InlayHint>>>,
    },
    /// Document highlight request.
    DocumentHighlight {
        uri: Url,
        position: Position,
        reply: tokio::sync::oneshot::Sender<Option<Vec<DocumentHighlight>>>,
    },
    /// Execute command (sema.runTopLevel).
    ExecuteCommand {
        command: String,
        arguments: Vec<serde_json::Value>,
    },
    /// Set the sema binary path (from initializationOptions).
    SetSemaBinary { path: String },
    /// Scan workspace for .sema files (triggered on initialized).
    ScanWorkspace { root: PathBuf },
    /// Continue incremental workspace scanning (directory-by-directory with yielding).
    ScanWorkspaceContinue { scanner: WorkspaceScanner },
    /// Shutdown the backend thread.
    Shutdown,
}

// ── Backend (runs on a dedicated std::thread, owns all Rc state) ─

/// Cached parse result for an imported file.
struct ImportCache {
    ast: Vec<sema_core::Value>,
    span_map: SpanMap,
    symbol_spans: Vec<(String, Span)>,
    scope_tree: scope::ScopeTree,
    /// Modification time when we last read the file.
    mtime: std::time::SystemTime,
}

/// Cached parse result for an open document (updated on every didChange).
struct CachedParse {
    ast: Vec<sema_core::Value>,
    span_map: SpanMap,
    symbol_spans: Vec<(String, Span)>,
    scope_tree: scope::ScopeTree,
}

// ── Semantic token legend ─────────────────────────────────────────

/// Indices into the token types legend for semantic tokens.
mod token_types {
    pub const KEYWORD: u32 = 0;
    pub const FUNCTION: u32 = 1;
    pub const VARIABLE: u32 = 2;
    pub const PARAMETER: u32 = 3;
    pub const MACRO: u32 = 4;
}

/// Indices into the token modifiers legend for semantic tokens.
mod token_modifiers {
    pub const DEFAULT_LIBRARY: u32 = 0b0000_0001;
}

fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::KEYWORD,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::MACRO,
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DEFAULT_LIBRARY,
            SemanticTokenModifier::DEFINITION,
        ],
    }
}

struct BackendState {
    /// Cached builtin names (from stdlib env) — HashSet for O(1) lookups.
    builtin_names: HashSet<String>,
    /// Per-document source text.
    documents: HashMap<String, String>,
    /// Cached user definitions per document (from last successful parse).
    /// Avoids losing completions while the user is typing (syntax errors).
    cached_user_defs: HashMap<String, Vec<String>>,
    /// Builtin documentation (name → markdown doc string).
    builtin_docs: HashMap<String, String>,
    /// Cached parse results for imported files (by absolute path).
    import_cache: HashMap<PathBuf, ImportCache>,
    /// Cached parse results for open documents (updated on didChange).
    cached_parses: HashMap<String, CachedParse>,
    /// Path to the sema binary (from initializationOptions or default).
    sema_binary: String,
}

impl BackendState {
    fn new() -> Self {
        // Create a sandboxed interpreter just to harvest builtin names.
        let sandbox = Sandbox::deny(Caps::ALL);
        let interp = sema_eval::Interpreter::new_with_sandbox(&sandbox);
        let mut builtin_names = HashSet::new();
        {
            let bindings = interp.global_env.bindings.borrow();
            for (spur, _) in bindings.iter() {
                let name = sema_core::resolve(*spur);
                builtin_names.insert(name);
            }
        }

        BackendState {
            builtin_names,
            documents: HashMap::new(),
            cached_user_defs: HashMap::new(),
            builtin_docs: builtin_docs::build_builtin_docs(),
            import_cache: HashMap::new(),
            cached_parses: HashMap::new(),
            sema_binary: "sema".to_string(),
        }
    }

    /// Lightweight constructor with only documents — for subprocess dispatch threads.
    fn new_without_builtins(documents: HashMap<String, String>, sema_binary: String) -> Self {
        BackendState {
            builtin_names: HashSet::new(),
            documents,
            cached_user_defs: HashMap::new(),
            builtin_docs: HashMap::new(),
            import_cache: HashMap::new(),
            cached_parses: HashMap::new(),
            sema_binary,
        }
    }

    /// Maximum number of entries in the import cache. Prevents unbounded
    /// memory growth when scanning large workspaces.
    const MAX_IMPORT_CACHE_SIZE: usize = 500;

    /// Get or refresh the cached parse result for an imported file.
    fn get_import_cache(&mut self, path: &Path) -> Option<&ImportCache> {
        let mtime = std::fs::metadata(path).and_then(|m| m.modified()).ok()?;

        // Check if cache is still valid
        if let Some(cached) = self.import_cache.get(path) {
            if cached.mtime == mtime {
                return self.import_cache.get(path);
            }
        }

        // Evict oldest entries when at capacity (by arbitrary key order —
        // not true LRU, but prevents unbounded growth cheaply).
        if self.import_cache.len() >= Self::MAX_IMPORT_CACHE_SIZE {
            let keys_to_remove: Vec<PathBuf> = self
                .import_cache
                .keys()
                .take(Self::MAX_IMPORT_CACHE_SIZE / 10)
                .cloned()
                .collect();
            for key in keys_to_remove {
                self.import_cache.remove(&key);
            }
        }

        // Read and parse the file
        let text = std::fs::read_to_string(path).ok()?;
        let (ast, span_map, symbol_spans) = sema_reader::read_many_with_symbol_spans(&text).ok()?;
        let scope_tree = scope::ScopeTree::build(&ast, &span_map, &symbol_spans);

        self.import_cache.insert(
            path.to_path_buf(),
            ImportCache {
                ast,
                span_map,
                symbol_spans,
                scope_tree,
                mtime,
            },
        );
        self.import_cache.get(path)
    }

    fn handle_complete(&self, uri: &Url, position: &Position) -> Vec<CompletionItem> {
        let uri_str = uri.as_str();
        let text = match self.documents.get(uri_str) {
            Some(t) => t,
            None => return vec![],
        };

        // Get the line at cursor
        let line_idx = position.line as usize;
        let line = match text.lines().nth(line_idx) {
            Some(l) => l,
            None => return vec![],
        };

        let byte_offset = utf16_to_byte_offset(line, position.character);
        let prefix = extract_prefix(line, byte_offset);

        let mut items = Vec::new();

        // Special forms
        for &name in sema_eval::SPECIAL_FORM_NAMES {
            if prefix.is_empty() || name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.to_string(),
                    kind: Some(CompletionItemKind::KEYWORD),
                    documentation: self.builtin_docs.get(name).map(|doc| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: doc.clone(),
                        })
                    }),
                    ..Default::default()
                });
            }
        }

        // Builtins (sorted for deterministic completion order)
        let mut sorted_builtins: Vec<&String> = self.builtin_names.iter().collect();
        sorted_builtins.sort();
        for name in sorted_builtins {
            if prefix.is_empty() || name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    documentation: self.builtin_docs.get(name.as_str()).map(|doc| {
                        Documentation::MarkupContent(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: doc.clone(),
                        })
                    }),
                    ..Default::default()
                });
            }
        }

        // User definitions: use cached defs (survives syntax errors while typing)
        let user_defs = self.cached_user_defs.get(uri_str);
        for name in user_defs.into_iter().flatten() {
            if prefix.is_empty() || name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::VARIABLE),
                    ..Default::default()
                });
            }
        }

        // Local bindings from scope tree
        if let Some(cached) = self.cached_parses.get(uri_str) {
            let sema_line = position.line as usize + 1;
            let sema_col = position.character as usize + 1;
            for (name, _span) in cached.scope_tree.visible_bindings_at(sema_line, sema_col) {
                if prefix.is_empty() || name.starts_with(prefix) {
                    items.push(CompletionItem {
                        label: name,
                        kind: Some(CompletionItemKind::VARIABLE),
                        sort_text: Some("0".to_string()),
                        ..Default::default()
                    });
                }
            }
        }

        items
    }

    fn handle_code_lens(&self, uri: &Url) -> Vec<CodeLens> {
        let uri_str = uri.as_str();
        let text = match self.documents.get(uri_str) {
            Some(t) => t,
            None => return vec![],
        };

        let (exprs, span_map) = match sema_reader::read_many_with_spans(text) {
            Ok(r) => r,
            Err(_) => return vec![],
        };

        let indexed_ranges = top_level_ranges(&exprs, &span_map);

        indexed_ranges
            .into_iter()
            .map(|(form_index, range)| {
                let command = Command {
                    title: "▶ Run".to_string(),
                    command: "sema.runTopLevel".to_string(),
                    arguments: Some(vec![serde_json::json!({
                        "uri": uri.as_str(),
                        "formIndex": form_index,
                    })]),
                };
                CodeLens {
                    range,
                    command: Some(command),
                    data: None,
                }
            })
            .collect()
    }

    fn handle_goto_definition(
        &mut self,
        uri: &Url,
        position: &Position,
    ) -> Option<GotoDefinitionResponse> {
        let uri_str = uri.as_str();
        let cached = self.cached_parses.get(uri_str)?;

        // Phase 3a: Check if cursor is on an import/load path string
        if let Some(path_str) = import_path_from_ast(&cached.ast, &cached.span_map, position.line) {
            if let Some(resolved) = resolve_import_path(uri, &path_str) {
                if resolved.exists() {
                    let target_uri = Url::from_file_path(&resolved).ok()?;
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: target_uri,
                        range: Range::default(),
                    }));
                }
            }
            return None;
        }

        // Phase 3b: Check if cursor is on a user-defined symbol
        let line_idx = position.line as usize;
        let text = self.documents.get(uri_str)?;
        let line = text.lines().nth(line_idx)?;
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset).to_string();
        if symbol.is_empty() {
            return None;
        }

        // Check scope tree for binding definition (local + top-level)
        let cached = self.cached_parses.get(uri_str)?;
        let sema_line = position.line as usize + 1;
        let sema_col = position.character as usize + 1;
        if let Some(resolved) = cached.scope_tree.resolve_at(&symbol, sema_line, sema_col) {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: span_to_range(&resolved.def_span),
            }));
        }

        // Phase 3c: Search imported modules for the definition
        let import_paths = import_paths_from_ast(&cached.ast);
        for path_str in &import_paths {
            let resolved = match resolve_import_path(uri, path_str) {
                Some(p) if p.exists() => p,
                _ => continue,
            };
            let cached = match self.get_import_cache(&resolved) {
                Some(c) => c,
                None => continue,
            };
            let target_defs =
                user_definitions_from_ast(&cached.ast, &cached.span_map, &cached.symbol_spans);
            for (name, range) in &target_defs {
                if name == &symbol {
                    if let Some(range) = range {
                        let target_uri = Url::from_file_path(&resolved).ok()?;
                        return Some(GotoDefinitionResponse::Scalar(Location {
                            uri: target_uri,
                            range: *range,
                        }));
                    }
                }
            }
        }

        None
    }

    fn handle_references(&self, uri: &Url, position: &Position) -> Vec<Location> {
        let uri_str = uri.as_str();
        let text = match self.documents.get(uri_str) {
            Some(t) => t,
            None => return vec![],
        };

        let line_idx = position.line as usize;
        let line = match text.lines().nth(line_idx) {
            Some(l) => l,
            None => return vec![],
        };
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset);
        if symbol.is_empty() {
            return vec![];
        }

        // 1-indexed position for scope tree queries
        let sema_line = position.line as usize + 1;
        let sema_col = position.character as usize + 1;

        // Check scope tree in the current document
        if let Some(cached) = self.cached_parses.get(uri_str) {
            if cached
                .scope_tree
                .is_locally_scoped(symbol, sema_line, sema_col)
            {
                // Locally scoped — only return references within this document's scope
                let refs = cached.scope_tree.find_scope_aware_references(
                    symbol,
                    sema_line,
                    sema_col,
                    &cached.symbol_spans,
                );
                return refs
                    .into_iter()
                    .map(|span| Location {
                        uri: uri.clone(),
                        range: span_to_range(&span),
                    })
                    .collect();
            }
        }

        // Top-level/global symbol — search all open documents, but skip
        // occurrences that are shadowed by local bindings in each document.
        let mut locations = Vec::new();
        let mut searched_uris = std::collections::HashSet::new();

        for (doc_uri_str, cached) in &self.cached_parses {
            let doc_uri = match Url::parse(doc_uri_str) {
                Ok(u) => u,
                Err(_) => continue,
            };
            searched_uris.insert(doc_uri_str.clone());
            for (name, span) in &cached.symbol_spans {
                if name != symbol {
                    continue;
                }
                // Only include this occurrence if it resolves to the top-level
                // definition (not shadowed by a local binding).
                match cached.scope_tree.resolve_at(name, span.line, span.col) {
                    Some(resolved) if !resolved.is_top_level => continue,
                    _ => {}
                }
                locations.push(Location {
                    uri: doc_uri.clone(),
                    range: span_to_range(span),
                });
            }
        }

        // Also search workspace files not currently open (import_cache)
        for (path, import_cached) in &self.import_cache {
            let import_uri = match Url::from_file_path(path) {
                Ok(u) => u,
                Err(_) => continue,
            };
            // Skip files already searched via cached_parses
            if searched_uris.contains(import_uri.as_str()) {
                continue;
            }
            for (name, span) in &import_cached.symbol_spans {
                if name != symbol {
                    continue;
                }
                match import_cached
                    .scope_tree
                    .resolve_at(name, span.line, span.col)
                {
                    Some(resolved) if !resolved.is_top_level => continue,
                    _ => {}
                }
                locations.push(Location {
                    uri: import_uri.clone(),
                    range: span_to_range(span),
                });
            }
        }

        locations
    }

    fn handle_document_highlight(
        &self,
        uri: &Url,
        position: &Position,
    ) -> Option<Vec<DocumentHighlight>> {
        let uri_str = uri.as_str();
        let text = self.documents.get(uri_str)?;
        let line_idx = position.line as usize;
        let line = text.lines().nth(line_idx)?;
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset);
        if symbol.is_empty() {
            return None;
        }

        let cached = self.cached_parses.get(uri_str)?;
        let sema_line = position.line as usize + 1;
        let sema_col = position.character as usize + 1;

        // Use scope-aware references for locally scoped symbols
        if cached
            .scope_tree
            .is_locally_scoped(symbol, sema_line, sema_col)
        {
            let refs = cached.scope_tree.find_scope_aware_references(
                symbol,
                sema_line,
                sema_col,
                &cached.symbol_spans,
            );
            let highlights: Vec<DocumentHighlight> = refs
                .into_iter()
                .map(|span| DocumentHighlight {
                    range: span_to_range(&span),
                    kind: None,
                })
                .collect();
            return if highlights.is_empty() {
                None
            } else {
                Some(highlights)
            };
        }

        // Top-level/global: all occurrences in this document that resolve to top-level
        let mut highlights = Vec::new();
        for (name, span) in &cached.symbol_spans {
            if name != symbol {
                continue;
            }
            match cached.scope_tree.resolve_at(name, span.line, span.col) {
                Some(resolved) if !resolved.is_top_level => continue,
                _ => {}
            }
            highlights.push(DocumentHighlight {
                range: span_to_range(span),
                kind: None,
            });
        }

        if highlights.is_empty() {
            None
        } else {
            Some(highlights)
        }
    }

    fn handle_hover(&mut self, uri: &Url, position: &Position) -> Option<Hover> {
        let uri_str = uri.as_str();
        let text = self.documents.get(uri_str)?;
        let line_idx = position.line as usize;
        let line = text.lines().nth(line_idx)?;
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset).to_string();
        if symbol.is_empty() {
            return None;
        }

        // Check builtin docs first
        if let Some(doc) = self.builtin_docs.get(symbol.as_str()) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc.clone(),
                }),
                range: None,
            });
        }

        // Use cached parse for user definition lookup + params extraction
        let cached = self.cached_parses.get(uri_str)?;

        // Check user definitions — show signature if available
        {
            let defs =
                user_definitions_from_ast(&cached.ast, &cached.span_map, &cached.symbol_spans);
            for (name, _) in &defs {
                if name == &symbol {
                    let mut hover_text = format!("```sema\n({symbol}");
                    if let Some(params) = extract_params_from_ast(&cached.ast, &symbol) {
                        hover_text.push(' ');
                        hover_text.push_str(&params);
                    }
                    hover_text.push_str(")\n```\n\n*User-defined*");
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_text,
                        }),
                        range: None,
                    });
                }
            }
        }

        // Check if it's a known special form (without explicit doc)
        if sema_eval::SPECIAL_FORM_NAMES.contains(&symbol.as_str()) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```sema\n{symbol}\n```\n\n*Special form*"),
                }),
                range: None,
            });
        }

        // Check if it's a known builtin (without explicit doc)
        if self.builtin_names.contains(symbol.as_str()) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```sema\n{symbol}\n```\n\n*Built-in function*"),
                }),
                range: None,
            });
        }

        // Phase 3c: Check imported modules for hover info
        {
            let import_paths = import_paths_from_ast(&cached.ast);
            for path_str in &import_paths {
                let resolved = match resolve_import_path(uri, path_str) {
                    Some(p) if p.exists() => p,
                    _ => continue,
                };
                let import_cached = match self.get_import_cache(&resolved) {
                    Some(c) => c,
                    None => continue,
                };
                let target_defs = user_definitions_from_ast(
                    &import_cached.ast,
                    &import_cached.span_map,
                    &import_cached.symbol_spans,
                );
                if target_defs.iter().any(|(n, _)| n == &symbol) {
                    let module_name = Path::new(path_str)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or(path_str);
                    let mut hover_text = format!("```sema\n({symbol}");
                    if let Some(params) = extract_params_from_ast(&import_cached.ast, &symbol) {
                        hover_text.push(' ');
                        hover_text.push_str(&params);
                    }
                    hover_text.push_str(&format!(")\n```\n\n*Imported from `{module_name}`*"));
                    return Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: hover_text,
                        }),
                        range: None,
                    });
                }
            }
        }

        None
    }

    fn handle_execute_command(
        &self,
        command: &str,
        arguments: &[serde_json::Value],
        client: &Client,
        handle: &tokio::runtime::Handle,
    ) {
        if command != "sema.runTopLevel" {
            return;
        }

        let arg = match arguments.first() {
            Some(a) => a,
            None => return,
        };

        let uri_str = arg.get("uri").and_then(|v| v.as_str()).unwrap_or("");
        let form_index = arg.get("formIndex").and_then(|v| v.as_u64()).unwrap_or(0) as usize;

        let uri = match Url::parse(uri_str) {
            Ok(u) => u,
            Err(_) => return,
        };

        let text = match self.documents.get(uri_str) {
            Some(t) => t.clone(),
            None => return,
        };

        let (exprs, span_map) = match sema_reader::read_many_with_spans(&text) {
            Ok(r) => r,
            Err(_) => return,
        };

        if form_index >= exprs.len() {
            return;
        }

        let indexed_ranges = top_level_ranges(&exprs, &span_map);
        let form_range = indexed_ranges
            .iter()
            .find(|(i, _)| *i == form_index)
            .map(|(_, r)| *r)
            .unwrap_or_default();

        // Build program text: pretty-print forms [0..=form_index]
        let program: String = exprs[..=form_index]
            .iter()
            .map(|v| sema_core::pretty_print(v, 80))
            .collect::<Vec<_>>()
            .join("\n");

        // Use configured sema binary path
        let sema_bin = PathBuf::from(&self.sema_binary);

        // Build args
        let mut args = vec![
            "eval".to_string(),
            "--stdin".to_string(),
            "--json".to_string(),
            "--sandbox".to_string(),
            "strict".to_string(),
            "--no-llm".to_string(),
        ];

        // Add --path if the URI is a file
        if let Ok(path) = uri.to_file_path() {
            args.push("--path".to_string());
            args.push(path.display().to_string());
        }

        let start = std::time::Instant::now();

        let result = std::process::Command::new(&sema_bin)
            .args(&args)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .and_then(|mut child| {
                use std::io::Write;
                if let Some(mut stdin) = child.stdin.take() {
                    let _ = stdin.write_all(program.as_bytes());
                    // stdin is dropped here, closing the pipe
                }
                child.wait_with_output()
            });

        let elapsed_ms = start.elapsed().as_millis() as u64;

        let params = match result {
            Ok(output) => {
                let stdout_str = String::from_utf8_lossy(&output.stdout).to_string();
                let stderr_str = String::from_utf8_lossy(&output.stderr).to_string();

                // Parse the JSON envelope from stdout
                if let Ok(json) = serde_json::from_str::<serde_json::Value>(&stdout_str) {
                    let ok = json.get("ok").and_then(|v| v.as_bool()).unwrap_or(false);
                    let value = json
                        .get("value")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());
                    let captured_stdout = json
                        .get("stdout")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string();
                    let captured_stderr = json
                        .get("stderr")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string();
                    let error = json.get("error").and_then(|v| {
                        v.get("message")
                            .and_then(|m| m.as_str())
                            .map(|s| s.to_string())
                    });
                    let eval_elapsed = json
                        .get("elapsedMs")
                        .and_then(|v| v.as_u64())
                        .unwrap_or(elapsed_ms);

                    EvalResultParams {
                        uri: uri.clone(),
                        range: form_range,
                        kind: "run".to_string(),
                        value,
                        stdout: captured_stdout,
                        stderr: captured_stderr,
                        ok,
                        error,
                        elapsed_ms: eval_elapsed,
                    }
                } else {
                    EvalResultParams {
                        uri: uri.clone(),
                        range: form_range,
                        kind: "run".to_string(),
                        value: None,
                        stdout: stdout_str,
                        stderr: stderr_str,
                        ok: false,
                        error: Some("Failed to parse eval output".to_string()),
                        elapsed_ms,
                    }
                }
            }
            Err(e) => EvalResultParams {
                uri: uri.clone(),
                range: form_range,
                kind: "run".to_string(),
                value: None,
                stdout: String::new(),
                stderr: String::new(),
                ok: false,
                error: Some(format!("Failed to spawn sema: {e}")),
                elapsed_ms,
            },
        };

        let client = client.clone();
        handle.block_on(async {
            client
                .send_notification::<EvalResultNotification>(params)
                .await;
        });
    }

    fn handle_document_symbols(&self, uri: &Url) -> DocumentSymbolResponse {
        let cached = match self.cached_parses.get(uri.as_str()) {
            Some(c) => c,
            None => return DocumentSymbolResponse::Nested(vec![]),
        };
        let symbols =
            document_symbols_from_ast(&cached.ast, &cached.span_map, &cached.symbol_spans);
        DocumentSymbolResponse::Nested(symbols)
    }

    #[allow(deprecated)]
    fn handle_workspace_symbols(&self, query: &str) -> Vec<SymbolInformation> {
        let mut results = Vec::new();
        let query_lower = query.to_lowercase();

        for (doc_uri_str, cached) in &self.cached_parses {
            let doc_uri = match Url::parse(doc_uri_str) {
                Ok(u) => u,
                Err(_) => continue,
            };

            let symbols =
                document_symbols_from_ast(&cached.ast, &cached.span_map, &cached.symbol_spans);

            for sym in symbols {
                if query.is_empty() || sym.name.to_lowercase().contains(&query_lower) {
                    results.push(SymbolInformation {
                        name: sym.name,
                        kind: sym.kind,
                        tags: None,
                        deprecated: None,
                        location: Location {
                            uri: doc_uri.clone(),
                            range: sym.selection_range,
                        },
                        container_name: None,
                    });
                }
            }
        }

        results
    }

    fn handle_signature_help(&mut self, uri: &Url, position: &Position) -> Option<SignatureHelp> {
        let uri_str = uri.as_str();
        let text = self.documents.get(uri_str)?;

        let (func_name, active_param) =
            find_enclosing_call(text, position.line, position.character)?;

        // Try user definitions in current document (use cached parse)
        let cached = self.cached_parses.get(uri_str)?;

        if let Some(params_str) = extract_params_from_ast(&cached.ast, &func_name) {
            let param_names = parse_param_names(&params_str);
            let label = format!("({func_name} {})", param_names.join(" "));
            let parameters: Vec<ParameterInformation> = param_names
                .iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple(p.clone()),
                    documentation: None,
                })
                .collect();

            return Some(SignatureHelp {
                signatures: vec![SignatureInformation {
                    label,
                    documentation: None,
                    parameters: Some(parameters),
                    active_parameter: Some(active_param as u32),
                }],
                active_signature: Some(0),
                active_parameter: Some(active_param as u32),
            });
        }

        // Try imported files
        let import_paths = import_paths_from_ast(&cached.ast);
        for path_str in &import_paths {
            let resolved = match resolve_import_path(uri, path_str) {
                Some(p) if p.exists() => p,
                _ => continue,
            };
            let cached = match self.get_import_cache(&resolved) {
                Some(c) => c,
                None => continue,
            };
            if let Some(params_str) = extract_params_from_ast(&cached.ast, &func_name) {
                let param_names = parse_param_names(&params_str);
                let label = format!("({func_name} {})", param_names.join(" "));
                let parameters: Vec<ParameterInformation> = param_names
                    .iter()
                    .map(|p| ParameterInformation {
                        label: ParameterLabel::Simple(p.clone()),
                        documentation: None,
                    })
                    .collect();

                return Some(SignatureHelp {
                    signatures: vec![SignatureInformation {
                        label,
                        documentation: None,
                        parameters: Some(parameters),
                        active_parameter: Some(active_param as u32),
                    }],
                    active_signature: Some(0),
                    active_parameter: Some(active_param as u32),
                });
            }
        }

        // Try builtin docs (no parameter highlighting, just doc)
        if let Some(doc) = self.builtin_docs.get(&func_name) {
            return Some(SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: func_name,
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: doc.clone(),
                    })),
                    parameters: None,
                    active_parameter: None,
                }],
                active_signature: Some(0),
                active_parameter: None,
            });
        }

        None
    }

    fn handle_semantic_tokens_full(&self, uri: &Url) -> Option<SemanticTokensResult> {
        let uri_str = uri.as_str();
        let cached = self.cached_parses.get(uri_str)?;

        // Single pass: collect user-defined function and macro names
        let mut user_fn_names = HashSet::new();
        let mut user_macro_names = HashSet::new();
        for expr in &cached.ast {
            if let Some(items) = expr.as_list() {
                if items.len() >= 2 {
                    if let Some(head) = items[0].as_symbol() {
                        if let Some(name) = items[1].as_symbol() {
                            match head.as_str() {
                                "defun" | "defn" => {
                                    user_fn_names.insert(name);
                                }
                                "defmacro" => {
                                    user_macro_names.insert(name);
                                }
                                "define" => {
                                    // (define (f x) ...) shorthand
                                    // Already handled below
                                }
                                _ => {}
                            }
                        } else if head == "define" {
                            // (define (f args...) body) — function shorthand
                            if let Some(sig) = items[1].as_list() {
                                if !sig.is_empty() {
                                    if let Some(name) = sig[0].as_symbol() {
                                        user_fn_names.insert(name);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        let mut raw_tokens: Vec<(usize, usize, usize, u32, u32)> = Vec::new();

        for (name, span) in &cached.symbol_spans {
            let (token_type, modifiers) =
                if sema_eval::SPECIAL_FORM_NAMES.contains(&name.as_str()) {
                    (token_types::KEYWORD, 0u32)
                } else if user_macro_names.contains(name.as_str()) {
                    (token_types::MACRO, 0u32)
                } else if self.builtin_names.contains(name.as_str()) {
                    (token_types::FUNCTION, token_modifiers::DEFAULT_LIBRARY)
                } else {
                    // Check scope tree for classification
                    match cached.scope_tree.resolve_at(name, span.line, span.col) {
                        Some(resolved) if !resolved.is_top_level => {
                            (token_types::PARAMETER, 0u32)
                        }
                        Some(_) => {
                            if user_fn_names.contains(name.as_str()) {
                                (token_types::FUNCTION, 0u32)
                            } else {
                                (token_types::VARIABLE, 0u32)
                            }
                        }
                        None => continue,
                    }
                };

            // Skip multi-line tokens (shouldn't happen for symbols, but
            // the length calculation assumes a single line).
            if span.line != span.end_line || span.line == 0 {
                continue;
            }
            let length = span.end_col.saturating_sub(span.col);
            if length == 0 {
                continue;
            }
            raw_tokens.push((span.line, span.col, length, token_type, modifiers));
        }

        // Sort by position
        raw_tokens.sort_by_key(|&(line, col, _, _, _)| (line, col));

        // Encode as deltas
        let mut data = Vec::new();
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;

        for &(line, col, length, token_type, modifiers) in &raw_tokens {
            let lsp_line = (line - 1) as u32;
            let lsp_col = (col - 1) as u32;

            // Use saturating_sub to guard against underflow from unexpected
            // out-of-order spans (shouldn't happen after sort, but defensive).
            let delta_line = lsp_line.saturating_sub(prev_line);
            let delta_start = if delta_line == 0 {
                lsp_col.saturating_sub(prev_start)
            } else {
                lsp_col
            };

            data.push(SemanticToken {
                delta_line,
                delta_start,
                length: length as u32,
                token_type,
                token_modifiers_bitset: modifiers,
            });

            prev_line = lsp_line;
            prev_start = lsp_col;
        }

        Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data,
        }))
    }

    fn handle_folding_ranges(&self, uri: &Url) -> Vec<FoldingRange> {
        let cached = match self.cached_parses.get(uri.as_str()) {
            Some(c) => c,
            None => return vec![],
        };

        let mut ranges = Vec::new();
        Self::collect_folding_ranges(&cached.ast, &cached.span_map, &mut ranges);
        ranges
    }

    fn collect_folding_ranges(
        exprs: &[sema_core::Value],
        span_map: &SpanMap,
        ranges: &mut Vec<FoldingRange>,
    ) {
        for expr in exprs {
            if let Some(items) = expr.as_list() {
                if let Some(span) = expr_span(expr, span_map) {
                    if span.end_line > span.line {
                        ranges.push(FoldingRange {
                            start_line: (span.line - 1) as u32,
                            start_character: Some((span.col - 1) as u32),
                            end_line: (span.end_line - 1) as u32,
                            end_character: Some((span.end_col - 1) as u32),
                            kind: Some(FoldingRangeKind::Region),
                            collapsed_text: None,
                        });
                    }
                }
                // Recurse into sub-expressions
                Self::collect_folding_ranges(items, span_map, ranges);
            }
        }
    }

    fn handle_inlay_hints(&mut self, uri: &Url, range: &Range) -> Option<Vec<InlayHint>> {
        let uri_str = uri.as_str();

        // Pre-populate import caches before the immutable borrow phase,
        // so resolve_param_names can be called without &mut self.
        if let Some(cached) = self.cached_parses.get(uri_str) {
            let import_paths = import_paths_from_ast(&cached.ast);
            let paths_to_cache: Vec<PathBuf> = import_paths
                .iter()
                .filter_map(|p| resolve_import_path(uri, p))
                .filter(|p| p.exists())
                .collect();
            for path in &paths_to_cache {
                let _ = self.get_import_cache(path);
            }
        }

        let text = self.documents.get(uri_str)?;
        let cached = self.cached_parses.get(uri_str)?;

        let mut hints = Vec::new();
        Self::collect_inlay_hints_inner(
            &cached.ast,
            &cached.span_map,
            text,
            uri,
            range,
            &self.cached_parses,
            &self.import_cache,
            &self.builtin_docs,
            &mut hints,
        );
        if hints.is_empty() {
            None
        } else {
            Some(hints)
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn collect_inlay_hints_inner(
        exprs: &[sema_core::Value],
        span_map: &SpanMap,
        text: &str,
        uri: &Url,
        range: &Range,
        cached_parses: &HashMap<String, CachedParse>,
        import_cache: &HashMap<PathBuf, ImportCache>,
        builtin_docs: &HashMap<String, String>,
        hints: &mut Vec<InlayHint>,
    ) {
        let lines: Vec<&str> = text.lines().collect();

        for expr in exprs {
            let items = match expr.as_list() {
                Some(items) if items.len() >= 2 => items,
                _ => continue,
            };

            // Check if this form's span intersects the requested range
            let form_span = match expr_span(expr, span_map) {
                Some(s) => s,
                None => {
                    Self::collect_inlay_hints_inner(
                        items,
                        span_map,
                        text,
                        uri,
                        range,
                        cached_parses,
                        import_cache,
                        builtin_docs,
                        hints,
                    );
                    continue;
                }
            };
            let form_start_line = form_span.line.saturating_sub(1) as u32;
            let form_end_line = form_span.end_line.saturating_sub(1) as u32;
            if form_end_line < range.start.line || form_start_line > range.end.line {
                continue;
            }

            // Get the function name
            let func_name = match items[0].as_symbol() {
                Some(name) => name,
                None => {
                    Self::collect_inlay_hints_inner(
                        items,
                        span_map,
                        text,
                        uri,
                        range,
                        cached_parses,
                        import_cache,
                        builtin_docs,
                        hints,
                    );
                    continue;
                }
            };

            // Skip special forms — they don't have positional params
            if sema_eval::SPECIAL_FORM_NAMES.contains(&func_name.as_str()) {
                for item in &items[1..] {
                    if let Some(sub) = item.as_list() {
                        Self::collect_inlay_hints_inner(
                            sub,
                            span_map,
                            text,
                            uri,
                            range,
                            cached_parses,
                            import_cache,
                            builtin_docs,
                            hints,
                        );
                    }
                }
                continue;
            }

            // Try to resolve parameter names
            let param_names = Self::resolve_param_names_immut(
                uri,
                &func_name,
                cached_parses,
                import_cache,
                builtin_docs,
            );

            if let Some(params) = &param_names {
                // Find argument positions by scanning the source text within the form.
                let arg_positions =
                    find_arg_positions_in_form(form_span, &lines, items.len() - 1);

                let args = &items[1..];
                for (i, _arg) in args.iter().enumerate() {
                    if i >= params.len() {
                        break;
                    }
                    let param = &params[i];
                    if param == "." || param == "..." {
                        break;
                    }
                    if let Some(&(line, col)) = arg_positions.get(i) {
                        hints.push(InlayHint {
                            position: Position {
                                line: line as u32,
                                character: col as u32,
                            },
                            label: InlayHintLabel::String(format!("{}:", param)),
                            kind: Some(InlayHintKind::PARAMETER),
                            text_edits: None,
                            tooltip: None,
                            padding_left: None,
                            padding_right: Some(true),
                            data: None,
                        });
                    }
                }
            }

            // Recurse into arguments (they may contain nested calls)
            for item in &items[1..] {
                if let Some(sub) = item.as_list() {
                    Self::collect_inlay_hints_inner(
                        sub,
                        span_map,
                        text,
                        uri,
                        range,
                        cached_parses,
                        import_cache,
                        builtin_docs,
                        hints,
                    );
                }
            }
        }
    }

    /// Resolve parameter names for a function, checking current document,
    /// imported modules, and builtin docs. Immutable version — import caches
    /// must be pre-populated before calling.
    fn resolve_param_names_immut(
        uri: &Url,
        func_name: &str,
        cached_parses: &HashMap<String, CachedParse>,
        import_cache: &HashMap<PathBuf, ImportCache>,
        builtin_docs: &HashMap<String, String>,
    ) -> Option<Vec<String>> {
        let uri_str = uri.as_str();

        // 1. Check current document
        if let Some(cached) = cached_parses.get(uri_str) {
            if let Some(params_str) = extract_params_from_ast(&cached.ast, func_name) {
                let names = parse_param_names(&params_str);
                if !names.is_empty() {
                    return Some(names);
                }
            }
        }

        // 2. Check imported modules (from pre-populated cache)
        if let Some(cached) = cached_parses.get(uri_str) {
            let paths = import_paths_from_ast(&cached.ast);
            for path_str in &paths {
                let resolved = match resolve_import_path(uri, path_str) {
                    Some(p) if p.exists() => p,
                    _ => continue,
                };
                if let Some(import_cached) = import_cache.get(&resolved) {
                    if let Some(params_str) = extract_params_from_ast(&import_cached.ast, func_name)
                    {
                        let names = parse_param_names(&params_str);
                        if !names.is_empty() {
                            return Some(names);
                        }
                    }
                }
            }
        }

        // 3. Try builtin docs — parse param list from the first code block
        if let Some(doc) = builtin_docs.get(func_name) {
            if let Some(params) = extract_params_from_doc(doc, func_name) {
                if !params.is_empty() {
                    return Some(params);
                }
            }
        }

        None
    }

    fn handle_prepare_rename(
        &self,
        uri: &Url,
        position: &Position,
    ) -> Option<PrepareRenameResponse> {
        let text = self.documents.get(uri.as_str())?;
        let line_idx = position.line as usize;
        let line = text.lines().nth(line_idx)?;
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset);
        if symbol.is_empty() {
            return None;
        }

        // Don't allow renaming builtins or special forms
        if self.builtin_names.contains(symbol)
            || sema_eval::SPECIAL_FORM_NAMES.contains(&symbol)
        {
            return None;
        }

        // Find the symbol occurrence at this cursor position using cached parse
        let cached = self.cached_parses.get(uri.as_str())?;
        for (name, span) in &cached.symbol_spans {
            if name == symbol {
                let range = span_to_range(span);
                if position.line >= range.start.line
                    && position.line <= range.end.line
                    && position.character >= range.start.character
                    && position.character < range.end.character
                {
                    return Some(PrepareRenameResponse::RangeWithPlaceholder {
                        range,
                        placeholder: symbol.to_string(),
                    });
                }
            }
        }

        None
    }

    fn handle_rename(
        &self,
        uri: &Url,
        position: &Position,
        new_name: &str,
    ) -> Option<WorkspaceEdit> {
        let text = self.documents.get(uri.as_str())?;
        let line_idx = position.line as usize;
        let line = text.lines().nth(line_idx)?;
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset);
        if symbol.is_empty() {
            return None;
        }

        // Don't allow renaming builtins or special forms
        if self.builtin_names.contains(symbol)
            || sema_eval::SPECIAL_FORM_NAMES.contains(&symbol)
        {
            return None;
        }

        // 1-indexed position for scope tree queries
        let sema_line = position.line as usize + 1;
        let sema_col = position.character as usize + 1;

        let mut changes: HashMap<Url, Vec<TextEdit>> = HashMap::new();

        // Check if the symbol is locally scoped
        if let Some(cached) = self.cached_parses.get(uri.as_str()) {
            if cached
                .scope_tree
                .is_locally_scoped(symbol, sema_line, sema_col)
            {
                // Locally scoped — only rename within this document's scope
                let refs = cached.scope_tree.find_scope_aware_references(
                    symbol,
                    sema_line,
                    sema_col,
                    &cached.symbol_spans,
                );
                let edits: Vec<TextEdit> = refs
                    .into_iter()
                    .map(|span| TextEdit {
                        range: span_to_range(&span),
                        new_text: new_name.to_string(),
                    })
                    .collect();
                if edits.is_empty() {
                    return None;
                }
                changes.insert(uri.clone(), edits);
                return Some(WorkspaceEdit {
                    changes: Some(changes),
                    document_changes: None,
                    change_annotations: None,
                });
            }
        }

        // Top-level/global symbol — rename across all documents,
        // but skip occurrences shadowed by local bindings.
        let mut searched_uris = std::collections::HashSet::new();

        for (doc_uri_str, cached) in &self.cached_parses {
            let doc_uri = match Url::parse(doc_uri_str) {
                Ok(u) => u,
                Err(_) => continue,
            };
            searched_uris.insert(doc_uri_str.clone());
            let mut edits = Vec::new();
            for (name, span) in &cached.symbol_spans {
                if name != symbol {
                    continue;
                }
                match cached.scope_tree.resolve_at(name, span.line, span.col) {
                    Some(resolved) if !resolved.is_top_level => continue,
                    _ => {}
                }
                edits.push(TextEdit {
                    range: span_to_range(span),
                    new_text: new_name.to_string(),
                });
            }
            if !edits.is_empty() {
                changes.insert(doc_uri, edits);
            }
        }

        // Also rename in workspace files not currently open (import_cache)
        for (path, import_cached) in &self.import_cache {
            let import_uri = match Url::from_file_path(path) {
                Ok(u) => u,
                Err(_) => continue,
            };
            if searched_uris.contains(import_uri.as_str()) {
                continue;
            }
            let mut edits = Vec::new();
            for (name, span) in &import_cached.symbol_spans {
                if name != symbol {
                    continue;
                }
                match import_cached
                    .scope_tree
                    .resolve_at(name, span.line, span.col)
                {
                    Some(resolved) if !resolved.is_top_level => continue,
                    _ => {}
                }
                edits.push(TextEdit {
                    range: span_to_range(span),
                    new_text: new_name.to_string(),
                });
            }
            if !edits.is_empty() {
                changes.insert(import_uri, edits);
            }
        }

        if changes.is_empty() {
            return None;
        }

        Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        })
    }
}

// ── tower-lsp Backend ────────────────────────────────────────────

struct Backend {
    #[allow(dead_code)]
    client: Client,
    tx: tokio::sync::mpsc::UnboundedSender<LspRequest>,
    /// Workspace root extracted from InitializeParams, used for workspace scanning.
    workspace_root: tokio::sync::Mutex<Option<PathBuf>>,
}

impl Backend {
    fn new(client: Client, tx: tokio::sync::mpsc::UnboundedSender<LspRequest>) -> Self {
        Backend {
            client,
            tx,
            workspace_root: tokio::sync::Mutex::new(None),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Extract sema binary path from initializationOptions
        if let Some(opts) = &params.initialization_options {
            if let Some(path) = opts.get("semaPath").and_then(|v| v.as_str()) {
                let _ = self.tx.send(LspRequest::SetSemaBinary {
                    path: path.to_string(),
                });
            }
        }

        // Store workspace root for scanning in `initialized`
        let root = params
            .root_uri
            .as_ref()
            .and_then(|uri| uri.to_file_path().ok())
            .or_else(|| {
                #[allow(deprecated)]
                params.root_path.as_ref().map(PathBuf::from)
            });
        *self.workspace_root.lock().await = root;

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec!["(".to_string(), " ".to_string()]),
                    ..Default::default()
                }),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), " ".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: Default::default(),
                }),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: Default::default(),
                })),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["sema.runTopLevel".to_string()],
                    ..Default::default()
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_token_legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            work_done_progress_options: Default::default(),
                        },
                    ),
                ),
                document_highlight_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        // Scan workspace for .sema files to populate the definition cache
        if let Some(root) = self.workspace_root.lock().await.take() {
            let _ = self.tx.send(LspRequest::ScanWorkspace { root });
        }
    }

    async fn shutdown(&self) -> Result<()> {
        let _ = self.tx.send(LspRequest::Shutdown);
        // Workaround for tower-lsp#399: `Server::serve()` may not return
        // after the `exit` notification. Schedule a forced exit so the
        // process doesn't hang indefinitely. The 2-second delay gives the
        // client time to read any final responses before the process dies.
        // Tradeoff: pending writes or cache flushes may be cut short.
        tokio::spawn(async {
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
            std::process::exit(0);
        });
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let _ = self.tx.send(LspRequest::DocumentChanged { uri, text });
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        // We use FULL sync, so there's exactly one content change with the full text.
        if let Some(change) = params.content_changes.into_iter().last() {
            let _ = self.tx.send(LspRequest::DocumentChanged {
                uri,
                text: change.text,
            });
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let _ = self.tx.send(LspRequest::DocumentClosed {
            uri: params.text_document.uri,
        });
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::Complete {
            uri,
            position,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(items) => Ok(Some(CompletionResponse::Array(items))),
            Err(_) => Ok(None),
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::GotoDefinition {
            uri,
            position,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(response) => Ok(response),
            Err(_) => Ok(None),
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::Hover {
            uri,
            position,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(response) => Ok(response),
            Err(_) => Ok(None),
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::References {
            uri,
            position,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(locations) if locations.is_empty() => Ok(None),
            Ok(locations) => Ok(Some(locations)),
            Err(_) => Ok(None),
        }
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::DocumentHighlight {
            uri: params.text_document_position_params.text_document.uri,
            position: params.text_document_position_params.position,
            reply: tx,
        });
        Ok(rx.await.unwrap_or(None))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let (tx, rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::InlayHints {
            uri: params.text_document.uri,
            range: params.range,
            reply: tx,
        });
        Ok(rx.await.unwrap_or(None))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::CodeLens {
            uri,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(lenses) => Ok(Some(lenses)),
            Err(_) => Ok(None),
        }
    }

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        let _ = self.tx.send(LspRequest::ExecuteCommand {
            command: params.command,
            arguments: params.arguments,
        });
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::DocumentSymbols {
            uri,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(response) => Ok(Some(response)),
            Err(_) => Ok(None),
        }
    }

    async fn symbol(
        &self,
        params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::WorkspaceSymbols {
            query: params.query,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(symbols) if symbols.is_empty() => Ok(None),
            Ok(symbols) => Ok(Some(symbols)),
            Err(_) => Ok(None),
        }
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::SignatureHelp {
            uri,
            position,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(response) => Ok(response),
            Err(_) => Ok(None),
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::Rename {
            uri,
            position,
            new_name,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(response) => Ok(response),
            Err(_) => Ok(None),
        }
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = params.text_document.uri;
        let position = params.position;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::PrepareRename {
            uri,
            position,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(response) => Ok(response),
            Err(_) => Ok(None),
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::SemanticTokensFull {
            uri,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(response) => Ok(response),
            Err(_) => Ok(None),
        }
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri;

        let (reply_tx, reply_rx) = tokio::sync::oneshot::channel();
        let _ = self.tx.send(LspRequest::FoldingRange {
            uri,
            reply: reply_tx,
        });

        match reply_rx.await {
            Ok(ranges) => Ok(Some(ranges)),
            Err(_) => Ok(None),
        }
    }
}

// ── Server entry point ───────────────────────────────────────────

pub async fn run_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<LspRequest>();

    let (service, socket) = LspService::new(|client| Backend::new(client, tx.clone()));

    // Extract the client for publishing diagnostics from the backend thread.
    let client = service.inner().client.clone();

    // Capture a handle to the tokio runtime so the backend thread can call async methods.
    let handle = tokio::runtime::Handle::current();

    // Spawn the backend thread (owns Rc / non-Send state).
    let backend_handle = std::thread::spawn(move || {
        let mut state = BackendState::new();
        // Deferred messages from document change batching, processed
        // before reading from the channel to preserve ordering.
        let mut deferred: std::collections::VecDeque<LspRequest> = std::collections::VecDeque::new();

        while let Some(req) = if deferred.is_empty() {
            rx.blocking_recv()
        } else {
            // Process deferred DocumentChanged events first to ensure
            // interactive requests always see the latest AST. Only
            // yield to new interactive requests when the deferred queue
            // contains non-document-change items (e.g. scan continuations).
            let front_is_doc_change = matches!(
                deferred.front(),
                Some(LspRequest::DocumentChanged { .. })
            );
            if front_is_doc_change {
                // Must process document updates before any interactive
                // request to prevent stale-AST responses.
                deferred.pop_front()
            } else {
                // Deferred item is a scan continuation or similar low-priority
                // work — yield to interactive requests if any arrived.
                match rx.try_recv() {
                    Ok(msg) => Some(msg),
                    Err(_) => deferred.pop_front(),
                }
            }
        } {
            match req {
                LspRequest::DocumentChanged { uri, text } => {
                    // Batch document changes: drain any consecutive pending
                    // changes for the same URI so we only parse the latest
                    // version. Stops as soon as a non-matching message appears,
                    // preserving message ordering.
                    let (uri, text) = {
                        let latest_uri = uri;
                        let mut latest_text = text;
                        loop {
                            match rx.try_recv() {
                                Ok(LspRequest::DocumentChanged { uri: u, text: t })
                                    if u == latest_uri =>
                                {
                                    latest_text = t;
                                }
                                Ok(other) => {
                                    // Non-matching message: push it to the front
                                    // of the deferred queue and process it next,
                                    // preserving strict ordering.
                                    deferred.push_front(other);
                                    break;
                                }
                                Err(_) => break,
                            }
                        }
                        (latest_uri, latest_text)
                    };

                    // Parse once, cache the result, and derive diagnostics
                    let (ast, span_map, symbol_spans, errors) =
                        sema_reader::read_many_with_spans_recover(&text);

                    let mut diags: Vec<Diagnostic> = errors
                        .iter()
                        .map(|err| error_diagnostic(err, DiagnosticSeverity::ERROR))
                        .collect();
                    if diags.is_empty() {
                        diags.extend(compile_diagnostics(&ast));
                    }

                    let uri_str = uri.as_str().to_string();
                    let defs: Vec<String> =
                        user_definitions_from_ast(&ast, &span_map, &symbol_spans)
                            .into_iter()
                            .map(|(name, _)| name)
                            .collect();
                    if !defs.is_empty() || diags.is_empty() {
                        state.cached_user_defs.insert(uri_str.clone(), defs);
                    }

                    let scope_tree = scope::ScopeTree::build(&ast, &span_map, &symbol_spans);
                    state.cached_parses.insert(
                        uri_str.clone(),
                        CachedParse {
                            ast,
                            span_map,
                            symbol_spans,
                            scope_tree,
                        },
                    );
                    state.documents.insert(uri_str, text);

                    let client = client.clone();
                    handle.block_on(async {
                        client.publish_diagnostics(uri, diags, None).await;
                    });
                }
                LspRequest::DocumentClosed { uri } => {
                    state.documents.remove(uri.as_str());
                    state.cached_user_defs.remove(uri.as_str());
                    state.cached_parses.remove(uri.as_str());

                    let client = client.clone();
                    handle.block_on(async {
                        client.publish_diagnostics(uri, vec![], None).await;
                    });
                }
                LspRequest::Complete {
                    uri,
                    position,
                    reply,
                } => {
                    let items = state.handle_complete(&uri, &position);
                    let _ = reply.send(items);
                }
                LspRequest::GotoDefinition {
                    uri,
                    position,
                    reply,
                } => {
                    let result = state.handle_goto_definition(&uri, &position);
                    let _ = reply.send(result);
                }
                LspRequest::Hover {
                    uri,
                    position,
                    reply,
                } => {
                    let result = state.handle_hover(&uri, &position);
                    let _ = reply.send(result);
                }
                LspRequest::CodeLens { uri, reply } => {
                    let lenses = state.handle_code_lens(&uri);
                    let _ = reply.send(lenses);
                }
                LspRequest::References {
                    uri,
                    position,
                    reply,
                } => {
                    let result = state.handle_references(&uri, &position);
                    let _ = reply.send(result);
                }
                LspRequest::DocumentSymbols { uri, reply } => {
                    let result = state.handle_document_symbols(&uri);
                    let _ = reply.send(result);
                }
                LspRequest::WorkspaceSymbols { query, reply } => {
                    let result = state.handle_workspace_symbols(&query);
                    let _ = reply.send(result);
                }
                LspRequest::SignatureHelp {
                    uri,
                    position,
                    reply,
                } => {
                    let result = state.handle_signature_help(&uri, &position);
                    let _ = reply.send(result);
                }
                LspRequest::Rename {
                    uri,
                    position,
                    new_name,
                    reply,
                } => {
                    let result = state.handle_rename(&uri, &position, &new_name);
                    let _ = reply.send(result);
                }
                LspRequest::PrepareRename {
                    uri,
                    position,
                    reply,
                } => {
                    let result = state.handle_prepare_rename(&uri, &position);
                    let _ = reply.send(result);
                }
                LspRequest::SemanticTokensFull { uri, reply } => {
                    let result = state.handle_semantic_tokens_full(&uri);
                    let _ = reply.send(result);
                }
                LspRequest::FoldingRange { uri, reply } => {
                    let result = state.handle_folding_ranges(&uri);
                    let _ = reply.send(result);
                }
                LspRequest::DocumentHighlight {
                    uri,
                    position,
                    reply,
                } => {
                    let result = state.handle_document_highlight(&uri, &position);
                    let _ = reply.send(result);
                }
                LspRequest::InlayHints { uri, range, reply } => {
                    let result = state.handle_inlay_hints(&uri, &range);
                    let _ = reply.send(result);
                }
                LspRequest::ExecuteCommand { command, arguments } => {
                    // Run subprocess on a separate thread to avoid blocking
                    // the backend (which would freeze diagnostics/completions).
                    // Only clone the document text needed for this command.
                    let target_uri = arguments
                        .first()
                        .and_then(|a| a.get("uri"))
                        .and_then(|v| v.as_str())
                        .unwrap_or("");
                    let mut docs = HashMap::new();
                    if let Some(text) = state.documents.get(target_uri) {
                        docs.insert(target_uri.to_string(), text.clone());
                    }
                    let sema_binary = state.sema_binary.clone();
                    let client = client.clone();
                    let handle = handle.clone();
                    std::thread::spawn(move || {
                        let tmp = BackendState::new_without_builtins(docs, sema_binary);
                        tmp.handle_execute_command(&command, &arguments, &client, &handle);
                    });
                }
                LspRequest::SetSemaBinary { path } => {
                    state.sema_binary = path;
                }
                LspRequest::ScanWorkspace { root } => {
                    // Start incremental workspace scanning. The scanner
                    // processes one directory at a time, yielding to
                    // interactive requests between directories.
                    let scanner = WorkspaceScanner::new(&root);
                    deferred.push_back(LspRequest::ScanWorkspaceContinue { scanner });
                }
                LspRequest::ScanWorkspaceContinue { mut scanner } => {
                    const BATCH_SIZE: usize = 10;

                    // Process pending files first (from a previous large directory)
                    // before discovering new directories, so files are parsed in
                    // the order they're discovered.
                    if !scanner.pending_files.is_empty() {
                        let to_parse = scanner.pending_files.len().min(BATCH_SIZE);
                        let batch: Vec<PathBuf> =
                            scanner.pending_files.drain(..to_parse).collect();
                        for path in &batch {
                            let _ = state.get_import_cache(path);
                        }
                    } else if let Some(files) = scanner.next_dir() {
                        let to_parse = files.len().min(BATCH_SIZE);
                        for path in &files[..to_parse] {
                            let _ = state.get_import_cache(path);
                        }
                        if to_parse < files.len() {
                            scanner.pending_files = files[to_parse..].to_vec();
                        }
                    }

                    // Re-enqueue if more directories or files remain.
                    if !scanner.dir_stack.is_empty() || !scanner.pending_files.is_empty() {
                        deferred.push_back(LspRequest::ScanWorkspaceContinue { scanner });
                    }
                }
                LspRequest::Shutdown => break,
            }
        }
    });

    Server::new(stdin, stdout, socket).serve(service).await;

    // Wait for backend thread to finish.
    let _ = backend_handle.join();
}
