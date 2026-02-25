use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use serde::Serialize;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use sema_core::{Caps, SemaError, Sandbox, Span, SpanMap};

mod builtin_docs;

// ── Public helpers (also used by tests) ──────────────────────────

/// Check if a character is valid inside a Sema symbol.
/// Matches the reader's `is_symbol_char` in `lexer.rs`.
fn is_sema_symbol_char(ch: char) -> bool {
    ch.is_alphanumeric()
        || matches!(
            ch,
            '+' | '-' | '*' | '/' | '!' | '?' | '<' | '>' | '=' | '_' | '&' | '%' | '^' | '~'
                | '.' | '#'
        )
}

/// Convert a 0-indexed LSP UTF-16 character offset to a byte offset in a UTF-8 string.
/// Returns the byte offset, clamped to the string length.
pub fn utf16_to_byte_offset(line: &str, utf16_offset: u32) -> usize {
    let mut utf16_count = 0u32;
    for (byte_idx, ch) in line.char_indices() {
        if utf16_count >= utf16_offset {
            return byte_idx;
        }
        utf16_count += ch.len_utf16() as u32;
    }
    line.len()
}

/// Look up the LSP Range for an expression via its Rc pointer in the SpanMap.
fn expr_range(expr: &sema_core::Value, span_map: &SpanMap) -> Option<Range> {
    let rc = expr.as_list_rc()?;
    let ptr = Rc::as_ptr(&rc) as usize;
    span_map.get(&ptr).map(span_to_range)
}

/// Look up the raw Span for an expression via its Rc pointer in the SpanMap.
fn expr_span<'a>(expr: &sema_core::Value, span_map: &'a SpanMap) -> Option<&'a Span> {
    let rc = expr.as_list_rc()?;
    let ptr = Rc::as_ptr(&rc) as usize;
    span_map.get(&ptr)
}

/// Check if `inner` span is fully contained within `outer` span.
fn span_contains(outer: &Span, inner: &Span) -> bool {
    let inner_start = (inner.line, inner.col);
    let inner_end = (inner.end_line, inner.end_col);
    let outer_start = (outer.line, outer.col);
    let outer_end = (outer.end_line, outer.end_col);
    inner_start >= outer_start && inner_end <= outer_end
}

/// Find the precise span of a symbol `name` within a form's span.
/// Searches `symbol_spans` for the first occurrence of `name` contained in `form_span`.
fn find_name_span(
    name: &str,
    form_span: &Span,
    symbol_spans: &[(String, Span)],
) -> Option<Range> {
    symbol_spans
        .iter()
        .find(|(sym_name, sym_span)| sym_name == name && span_contains(form_span, sym_span))
        .map(|(_, span)| span_to_range(span))
}

/// Walk backwards from `pos` to find the byte offset where a symbol starts.
fn symbol_start(line: &str, pos: usize) -> usize {
    line[..pos]
        .char_indices()
        .rev()
        .take_while(|&(_, ch)| is_sema_symbol_char(ch))
        .last()
        .map(|(i, _)| i)
        .unwrap_or(pos)
}

/// Extract span from a SemaError, unwrapping wrapper layers.
pub fn error_span(err: &SemaError) -> Option<&Span> {
    match err.inner() {
        SemaError::Reader { span, .. } => Some(span),
        _ => None,
    }
}

/// Convert a 1-indexed Sema `Span` to a 0-indexed LSP `Range`.
pub fn span_to_range(span: &Span) -> Range {
    Range {
        start: Position {
            line: span.line.saturating_sub(1) as u32,
            character: span.col.saturating_sub(1) as u32,
        },
        end: Position {
            line: span.end_line.saturating_sub(1) as u32,
            character: span.end_col.saturating_sub(1) as u32,
        },
    }
}

/// Build a diagnostic message from a `SemaError`, appending hint/note if present.
fn format_error_message(err: &SemaError) -> String {
    let mut message = match err.inner() {
        SemaError::Reader { message, .. } => message.clone(),
        other => other.to_string(),
    };
    if let Some(hint) = err.hint() {
        message.push_str(&format!("\nhint: {hint}"));
    }
    if let Some(note) = err.note() {
        message.push_str(&format!("\nnote: {note}"));
    }
    message
}

/// Convert a SemaError into a diagnostic with the given severity.
fn error_diagnostic(err: &SemaError, severity: DiagnosticSeverity) -> Diagnostic {
    let range = error_span(err).map(span_to_range).unwrap_or_default();
    Diagnostic {
        range,
        severity: Some(severity),
        source: Some("sema".to_string()),
        message: format_error_message(err),
        ..Default::default()
    }
}

/// Parse source text and return diagnostics.
pub fn parse_diagnostics(text: &str) -> Vec<Diagnostic> {
    match sema_reader::read_many_with_spans(text) {
        Ok(_) => vec![],
        Err(err) => vec![error_diagnostic(&err, DiagnosticSeverity::ERROR)],
    }
}

/// Run the VM compilation pipeline on parsed expressions to catch
/// deeper errors (unbound variables, arity mismatches, invalid forms).
pub fn compile_diagnostics(exprs: &[sema_core::Value]) -> Vec<Diagnostic> {
    match sema_vm::compile_program(exprs) {
        Ok(_) => vec![],
        Err(err) => vec![error_diagnostic(&err, DiagnosticSeverity::WARNING)],
    }
}

/// Parse and compile-check source text, returning all diagnostics.
/// Uses error recovery to report multiple parse errors at once.
pub fn analyze_document(text: &str) -> Vec<Diagnostic> {
    let (exprs, _spans, errors) = sema_reader::read_many_with_spans_recover(text);
    let mut diags: Vec<Diagnostic> = errors
        .iter()
        .map(|err| error_diagnostic(err, DiagnosticSeverity::ERROR))
        .collect();
    // Only run compile diagnostics when there are no parse errors,
    // since missing forms would cause false unbound-variable errors.
    if diags.is_empty() {
        diags.extend(compile_diagnostics(&exprs));
    }
    diags
}

/// Extract the symbol prefix at the given cursor position for completion.
/// `byte_offset` is a byte index into the UTF-8 line string.
/// Returns the prefix string (may be empty).
pub fn extract_prefix(line: &str, byte_offset: usize) -> &str {
    let end = byte_offset.min(line.len());
    &line[symbol_start(line, end)..end]
}

/// Return (index, LSP range) for each top-level list form that has a span.
/// Non-list forms (bare atoms) and forms without spans are skipped.
pub fn top_level_ranges(
    exprs: &[sema_core::Value],
    span_map: &SpanMap,
) -> Vec<(usize, Range)> {
    exprs
        .iter()
        .enumerate()
        .filter_map(|(i, expr)| Some((i, expr_range(expr, span_map)?)))
        .collect()
}

/// Extract the full symbol at the given cursor position.
/// `byte_offset` is a byte index into the UTF-8 line string.
/// Returns the symbol string (may be empty if cursor is not on a symbol).
pub fn extract_symbol_at(line: &str, byte_offset: usize) -> &str {
    let pos = byte_offset.min(line.len());
    let start = symbol_start(line, pos);

    // Walk forwards to find end
    let end = line[pos..]
        .char_indices()
        .take_while(|&(_, ch)| is_sema_symbol_char(ch))
        .last()
        .map(|(i, ch)| pos + i + ch.len_utf8())
        .unwrap_or(pos);

    &line[start..end]
}

/// Collect user-defined names with their spans from a pre-parsed AST.
/// Returns (name, range) for each `define`/`defun`/`defn`/`defmacro`/`defagent`/`deftool`.
/// When `symbol_spans` is provided, returns the precise span of just the name symbol;
/// otherwise falls back to the span of the entire definition form.
pub fn user_definitions_from_ast(
    ast: &[sema_core::Value],
    span_map: &SpanMap,
    symbol_spans: &[(String, Span)],
) -> Vec<(String, Option<Range>)> {
    let mut defs = Vec::new();
    for expr in ast {
        if let Some(items) = expr.as_list() {
            if items.len() >= 2 {
                if let Some(head) = items[0].as_symbol() {
                    match head.as_str() {
                        "define" | "defun" | "defn" | "defmacro" | "defagent" | "deftool" => {
                            let form_span = expr_span(expr, span_map);
                            // (define name ...) or (defun name (...) ...)
                            if let Some(name) = items[1].as_symbol() {
                                let name_range = form_span
                                    .and_then(|fs| find_name_span(&name, fs, symbol_spans));
                                let range =
                                    name_range.or_else(|| form_span.map(span_to_range));
                                defs.push((name, range));
                            }
                            // (define (name args...) body) - function shorthand
                            else if let Some(sig) = items[1].as_list() {
                                if !sig.is_empty() {
                                    if let Some(name) = sig[0].as_symbol() {
                                        // Search within the signature list's span
                                        let sig_span = expr_span(&items[1], span_map);
                                        let name_range = sig_span
                                            .and_then(|ss| find_name_span(&name, ss, symbol_spans));
                                        let range = name_range
                                            .or_else(|| form_span.map(span_to_range));
                                        defs.push((name, range));
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    defs
}

/// Convenience wrapper: parse text and collect user definitions with spans.
pub fn user_definitions_with_spans(text: &str) -> Vec<(String, Option<Range>)> {
    let (ast, span_map, symbol_spans) = match sema_reader::read_many_with_symbol_spans(text) {
        Ok(result) => result,
        Err(_) => return vec![],
    };
    user_definitions_from_ast(&ast, &span_map, &symbol_spans)
}

/// Extract parameter list string from a pre-parsed AST for hover display.
pub fn extract_params_from_ast(ast: &[sema_core::Value], name: &str) -> Option<String> {
    for expr in ast {
        if let Some(items) = expr.as_list() {
            if items.len() >= 3 {
                if let Some(head) = items[0].as_symbol() {
                    match head.as_str() {
                        "defun" | "defn" => {
                            if let Some(sym) = items[1].as_symbol() {
                                if sym == name {
                                    return Some(sema_core::pretty_print(&items[2], 80));
                                }
                            }
                        }
                        "define" => {
                            if let Some(sig) = items[1].as_list() {
                                if !sig.is_empty() {
                                    if let Some(sym) = sig[0].as_symbol() {
                                        if sym == name {
                                            let params: Vec<_> = sig[1..]
                                                .iter()
                                                .map(|v| sema_core::pretty_print(v, 80))
                                                .collect();
                                            return Some(format!("({})", params.join(" ")));
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    None
}

/// Convenience wrapper: parse text and extract parameter list.
pub fn extract_params(text: &str, name: &str) -> Option<String> {
    let ast = match sema_reader::read_many_with_spans(text) {
        Ok((values, _)) => values,
        Err(_) => return None,
    };
    extract_params_from_ast(&ast, name)
}

/// Resolve an import/load path relative to a document URI.
/// Returns the absolute path if resolvable.
pub fn resolve_import_path(uri: &Url, path_str: &str) -> Option<PathBuf> {
    let file_path = uri.to_file_path().ok()?;
    let dir = file_path.parent()?;
    if Path::new(path_str).is_absolute() {
        Some(PathBuf::from(path_str))
    } else if sema_core::resolve::is_package_import(path_str) {
        sema_core::resolve::resolve_package_import(path_str).ok()
    } else {
        Some(dir.join(path_str))
    }
}

/// Check if the cursor is on a string argument of an import/load form.
/// Uses the SpanMap to verify the form's span covers the cursor line.
pub fn import_path_from_ast(
    ast: &[sema_core::Value],
    span_map: &SpanMap,
    line: u32,
) -> Option<String> {
    for expr in ast {
        if let Some(items) = expr.as_list() {
            if items.len() >= 2 {
                if let Some(head) = items[0].as_symbol() {
                    if head == "import" || head == "load" {
                        if let Some(path) = items[1].as_str() {
                            // Use SpanMap to check if this form covers the cursor line
                            let covers_line = expr_range(expr, span_map)
                                .map(|r| line >= r.start.line && line <= r.end.line)
                                .unwrap_or(false);
                            if covers_line {
                                return Some(path.to_string());
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Convenience wrapper: parse text and check for import path at cursor.
pub fn import_path_at_cursor(text: &str, line: u32, _character: u32) -> Option<String> {
    let (ast, span_map) = sema_reader::read_many_with_spans(text).ok()?;
    import_path_from_ast(&ast, &span_map, line)
}

/// Extract all import/load path strings from a pre-parsed AST.
pub fn import_paths_from_ast(ast: &[sema_core::Value]) -> Vec<String> {
    let mut paths = Vec::new();
    for expr in ast {
        if let Some(items) = expr.as_list() {
            if items.len() >= 2 {
                if let Some(head) = items[0].as_symbol() {
                    if head == "import" || head == "load" {
                        if let Some(path) = items[1].as_str() {
                            paths.push(path.to_string());
                        }
                    }
                }
            }
        }
    }
    paths
}

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
pub fn user_definitions(text: &str) -> Vec<String> {
    user_definitions_with_spans(text)
        .into_iter()
        .map(|(name, _)| name)
        .collect()
}

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
        let sym_spans = vec![
            ("foo".to_string(), sema_core::Span::new(5, 1, 5, 4)),
        ];
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
        assert_eq!(
            resolved,
            Some(PathBuf::from("/project/src/utils.sema"))
        );
    }

    #[test]
    fn resolve_absolute_path() {
        let uri = Url::parse("file:///project/src/main.sema").unwrap();
        let resolved = resolve_import_path(&uri, "/lib/utils.sema");
        assert_eq!(
            resolved,
            Some(PathBuf::from("/lib/utils.sema"))
        );
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
}

// ── Backend thread messages ──────────────────────────────────────

enum LspRequest {
    /// Document opened or changed — reparse and publish diagnostics.
    DocumentChanged {
        uri: Url,
        text: String,
    },
    /// Document closed — remove from cache and clear diagnostics.
    DocumentClosed {
        uri: Url,
    },
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
    /// Execute command (sema.runTopLevel).
    ExecuteCommand {
        command: String,
        arguments: Vec<serde_json::Value>,
    },
    /// Shutdown the backend thread.
    Shutdown,
}

// ── Backend (runs on a dedicated std::thread, owns all Rc state) ─

struct BackendState {
    /// Cached builtin names (from stdlib env).
    builtin_names: Vec<String>,
    /// Per-document source text.
    documents: HashMap<String, String>,
    /// Cached user definitions per document (from last successful parse).
    /// Avoids losing completions while the user is typing (syntax errors).
    cached_user_defs: HashMap<String, Vec<String>>,
    /// Builtin documentation (name → markdown doc string).
    builtin_docs: HashMap<String, String>,
}

impl BackendState {
    fn new() -> Self {
        // Create a sandboxed interpreter just to harvest builtin names.
        let sandbox = Sandbox::deny(Caps::ALL);
        let interp = sema_eval::Interpreter::new_with_sandbox(&sandbox);
        let mut builtin_names = Vec::new();
        {
            let bindings = interp.global_env.bindings.borrow();
            for (spur, _) in bindings.iter() {
                let name = sema_core::resolve(*spur);
                builtin_names.push(name);
            }
        }
        builtin_names.sort();
        builtin_names.dedup();

        BackendState {
            builtin_names,
            documents: HashMap::new(),
            cached_user_defs: HashMap::new(),
            builtin_docs: builtin_docs::build_builtin_docs(),
        }
    }

    /// Lightweight constructor with only documents — for subprocess dispatch threads.
    fn new_without_builtins(documents: HashMap<String, String>) -> Self {
        BackendState {
            builtin_names: Vec::new(),
            documents,
            cached_user_defs: HashMap::new(),
            builtin_docs: HashMap::new(),
        }
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
                    ..Default::default()
                });
            }
        }

        // Builtins
        for name in &self.builtin_names {
            if prefix.is_empty() || name.starts_with(prefix) {
                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
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
        &self,
        uri: &Url,
        position: &Position,
    ) -> Option<GotoDefinitionResponse> {
        let uri_str = uri.as_str();
        let text = self.documents.get(uri_str)?;

        // Parse once for import, symbol, and definition lookups
        let (ast, span_map, symbol_spans) =
            sema_reader::read_many_with_symbol_spans(text).ok()?;

        // Phase 3a: Check if cursor is on an import/load path string
        if let Some(path_str) = import_path_from_ast(&ast, &span_map, position.line) {
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
        let line = text.lines().nth(line_idx)?;
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset);
        if symbol.is_empty() {
            return None;
        }

        // Search current document for the definition
        let defs = user_definitions_from_ast(&ast, &span_map, &symbol_spans);
        for (name, range) in &defs {
            if name == symbol {
                if let Some(range) = range {
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range: *range,
                    }));
                }
            }
        }

        // Phase 3c: Search imported modules for the definition
        let import_paths = import_paths_from_ast(&ast);
        for path_str in &import_paths {
            let resolved = match resolve_import_path(uri, path_str) {
                Some(p) if p.exists() => p,
                _ => continue,
            };
            let target_text = match std::fs::read_to_string(&resolved) {
                Ok(t) => t,
                Err(_) => continue,
            };
            let (target_ast, target_span_map, target_sym_spans) =
                match sema_reader::read_many_with_symbol_spans(&target_text) {
                    Ok(r) => r,
                    Err(_) => continue,
                };
            let target_defs =
                user_definitions_from_ast(&target_ast, &target_span_map, &target_sym_spans);
            for (name, range) in &target_defs {
                if name == symbol {
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

    fn handle_references(
        &self,
        uri: &Url,
        position: &Position,
    ) -> Vec<Location> {
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

        let (_, _, symbol_spans) =
            match sema_reader::read_many_with_symbol_spans(text) {
                Ok(r) => r,
                Err(_) => return vec![],
            };

        // Find all occurrences of this symbol in the current document
        symbol_spans
            .iter()
            .filter(|(name, _)| name == symbol)
            .map(|(_, span)| Location {
                uri: uri.clone(),
                range: span_to_range(span),
            })
            .collect()
    }

    fn handle_hover(
        &self,
        uri: &Url,
        position: &Position,
    ) -> Option<Hover> {
        let uri_str = uri.as_str();
        let text = self.documents.get(uri_str)?;

        let line_idx = position.line as usize;
        let line = text.lines().nth(line_idx)?;
        let byte_offset = utf16_to_byte_offset(line, position.character);
        let symbol = extract_symbol_at(line, byte_offset);
        if symbol.is_empty() {
            return None;
        }

        // Check builtin docs first
        if let Some(doc) = self.builtin_docs.get(symbol) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc.clone(),
                }),
                range: None,
            });
        }

        // Parse once for user definition lookup + params extraction
        let parsed = sema_reader::read_many_with_symbol_spans(text).ok();

        // Check user definitions — show signature if available
        if let Some((ref ast, ref span_map, ref sym_spans)) = parsed {
            let defs = user_definitions_from_ast(ast, span_map, sym_spans);
            for (name, _) in &defs {
                if name == symbol {
                    let mut hover_text = format!("```sema\n({symbol}");
                    if let Some(params) = extract_params_from_ast(ast, symbol) {
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
        if sema_eval::SPECIAL_FORM_NAMES.contains(&symbol) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```sema\n{symbol}\n```\n\n*Special form*"),
                }),
                range: None,
            });
        }

        // Check if it's a known builtin (without explicit doc)
        if self.builtin_names.iter().any(|n| n == symbol) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```sema\n{symbol}\n```\n\n*Built-in function*"),
                }),
                range: None,
            });
        }

        // Phase 3c: Check imported modules for hover info
        if let Some((ref ast, _, _)) = parsed {
            let import_paths = import_paths_from_ast(ast);
            for path_str in &import_paths {
                let resolved = match resolve_import_path(uri, path_str) {
                    Some(p) if p.exists() => p,
                    _ => continue,
                };
                let target_text = match std::fs::read_to_string(&resolved) {
                    Ok(t) => t,
                    Err(_) => continue,
                };
                let (target_ast, target_span_map, target_sym_spans) =
                    match sema_reader::read_many_with_symbol_spans(&target_text) {
                        Ok(r) => r,
                        Err(_) => continue,
                    };
                let target_defs =
                    user_definitions_from_ast(&target_ast, &target_span_map, &target_sym_spans);
                if target_defs.iter().any(|(n, _)| n == symbol) {
                    let module_name = Path::new(path_str)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .unwrap_or(path_str);
                    let mut hover_text = format!("```sema\n({symbol}");
                    if let Some(params) = extract_params_from_ast(&target_ast, symbol) {
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

        // Find the sema binary
        let sema_bin = std::env::current_exe()
            .unwrap_or_else(|_| std::path::PathBuf::from("sema"));

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
                        v.get("message").and_then(|m| m.as_str()).map(|s| s.to_string())
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
}

// ── tower-lsp Backend ────────────────────────────────────────────

struct Backend {
    #[allow(dead_code)]
    client: Client,
    tx: tokio::sync::mpsc::UnboundedSender<LspRequest>,
}

impl Backend {
    fn new(
        client: Client,
        tx: tokio::sync::mpsc::UnboundedSender<LspRequest>,
    ) -> Self {
        Backend { client, tx }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["sema.runTopLevel".to_string()],
                    ..Default::default()
                }),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn shutdown(&self) -> Result<()> {
        let _ = self.tx.send(LspRequest::Shutdown);
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

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
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

    async fn hover(
        &self,
        params: HoverParams,
    ) -> Result<Option<Hover>> {
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

    async fn references(
        &self,
        params: ReferenceParams,
    ) -> Result<Option<Vec<Location>>> {
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

    async fn code_lens(
        &self,
        params: CodeLensParams,
    ) -> Result<Option<Vec<CodeLens>>> {
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

        while let Some(req) = rx.blocking_recv() {
            match req {
                LspRequest::DocumentChanged { uri, text } => {
                    let diags = analyze_document(&text);
                    // Update cached user definitions on successful parse
                    let uri_str = uri.as_str().to_string();
                    let defs = user_definitions(&text);
                    if !defs.is_empty() || diags.is_empty() {
                        state.cached_user_defs.insert(uri_str.clone(), defs);
                    }
                    state.documents.insert(uri_str, text);

                    let client = client.clone();
                    handle.block_on(async {
                        client.publish_diagnostics(uri, diags, None).await;
                    });
                }
                LspRequest::DocumentClosed { uri } => {
                    state.documents.remove(uri.as_str());
                    state.cached_user_defs.remove(uri.as_str());

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
                LspRequest::ExecuteCommand { command, arguments } => {
                    // Run subprocess on a separate thread to avoid blocking
                    // the backend (which would freeze diagnostics/completions).
                    let docs = state.documents.clone();
                    let client = client.clone();
                    let handle = handle.clone();
                    std::thread::spawn(move || {
                        let tmp = BackendState::new_without_builtins(docs);
                        tmp.handle_execute_command(
                            &command,
                            &arguments,
                            &client,
                            &handle,
                        );
                    });
                }
                LspRequest::Shutdown => break,
            }
        }
    });

    Server::new(stdin, stdout, socket).serve(service).await;

    // Wait for backend thread to finish.
    let _ = backend_handle.join();
}
