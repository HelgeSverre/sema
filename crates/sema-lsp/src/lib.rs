use std::collections::HashMap;
use std::rc::Rc;

use serde::Serialize;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use sema_core::{Caps, SemaError, Sandbox, Span, SpanMap};

// ── Public helpers (also used by tests) ──────────────────────────

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

/// Parse source text and return diagnostics.
pub fn parse_diagnostics(text: &str) -> Vec<Diagnostic> {
    match sema_reader::read_many_with_spans(text) {
        Ok(_) => vec![],
        Err(err) => {
            let range = error_span(&err)
                .map(span_to_range)
                .unwrap_or_default();
            vec![Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("sema".to_string()),
                message: format_error_message(&err),
                ..Default::default()
            }]
        }
    }
}

/// Run the VM compilation pipeline on parsed expressions to catch
/// deeper errors (unbound variables, arity mismatches, invalid forms).
pub fn compile_diagnostics(exprs: &[sema_core::Value]) -> Vec<Diagnostic> {
    match sema_vm::compile_program(exprs) {
        Ok(_) => vec![],
        Err(err) => {
            vec![Diagnostic {
                range: Range::default(),
                severity: Some(DiagnosticSeverity::WARNING),
                source: Some("sema".to_string()),
                message: format_error_message(&err),
                ..Default::default()
            }]
        }
    }
}

/// Parse and compile-check source text, returning all diagnostics.
pub fn analyze_document(text: &str) -> Vec<Diagnostic> {
    let (exprs, _spans) = match sema_reader::read_many_with_spans(text) {
        Ok(result) => result,
        Err(err) => {
            let range = error_span(&err)
                .map(span_to_range)
                .unwrap_or_default();
            return vec![Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("sema".to_string()),
                message: format_error_message(&err),
                ..Default::default()
            }];
        }
    };

    compile_diagnostics(&exprs)
}

/// Extract the symbol prefix at the given cursor position for completion.
/// Returns the prefix string (may be empty).
pub fn extract_prefix(line: &str, character: usize) -> &str {
    let bytes = line.as_bytes();
    let end = character.min(bytes.len());
    let mut start = end;
    while start > 0 {
        let ch = bytes[start - 1] as char;
        // These are valid in Sema symbol names
        if ch.is_alphanumeric() || matches!(ch, '/' | '-' | '?' | '!' | '>' | '*' | '.' | '_') {
            start -= 1;
        } else {
            break;
        }
    }
    &line[start..end]
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
        .filter_map(|(i, expr)| {
            let items_rc = expr.as_list_rc()?;
            let ptr = Rc::as_ptr(&items_rc) as usize;
            let span = span_map.get(&ptr)?;
            Some((i, span_to_range(span)))
        })
        .collect()
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
    let ast = match sema_reader::read_many_with_spans(text) {
        Ok((values, _)) => values,
        Err(_) => return vec![],
    };
    let mut defs = Vec::new();
    for expr in &ast {
        if let Some(items) = expr.as_list() {
            if items.len() >= 2 {
                if let Some(head) = items[0].as_symbol() {
                    match head.as_str() {
                        "define" | "defun" | "defn" | "defmacro" | "defagent" | "deftool" => {
                            // (define name ...) or (defun name (...) ...)
                            if let Some(name) = items[1].as_symbol() {
                                defs.push(name);
                            }
                            // (define (name args...) body) - function shorthand
                            else if let Some(sig) = items[1].as_list() {
                                if !sig.is_empty() {
                                    if let Some(name) = sig[0].as_symbol() {
                                        defs.push(name);
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
    /// CodeLens request.
    CodeLens {
        uri: Url,
        reply: tokio::sync::oneshot::Sender<Vec<CodeLens>>,
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
        }
    }

    /// Lightweight constructor with only documents — for subprocess dispatch threads.
    fn new_without_builtins(documents: HashMap<String, String>) -> Self {
        BackendState {
            builtin_names: Vec::new(),
            documents,
            cached_user_defs: HashMap::new(),
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

        let prefix = extract_prefix(line, position.character as usize);

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
                LspRequest::CodeLens { uri, reply } => {
                    let lenses = state.handle_code_lens(&uri);
                    let _ = reply.send(lenses);
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
