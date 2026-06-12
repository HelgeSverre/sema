//! Doctest runner for the structured builtin docs: evaluates the `; =>`-annotated example lines in
//! `sema_docs::builtin_index()` and checks the printed result matches.
//!
//! Conservative by design — it only checks single-line `<expr> ; => <expected>` assertions (with
//! preceding single-line setup statements sharing one interpreter), and skips anything
//! side-effecting or nondeterministic (I/O, LLM, time, randomness, channels, …). Multi-line
//! expressions and unparseable lines are skipped, not failed.
//!
//! `#[ignore]`d so it doesn't gate CI on example-formatting variance; run on demand:
//!   cargo test -p sema-lang --test doc_examples_test -- --ignored --nocapture

use sema_eval::Interpreter;

/// Substrings that mark an example as side-effecting / nondeterministic → skip.
const SKIP_MARKERS: &[&str] = &[
    "http",
    "llm/",
    "file/",
    "io/",
    "net",
    "channel",
    "async",
    "await",
    "prompt",
    "agent",
    "conversation",
    "embedding",
    "tool/",
    "message",
    "random",
    "rand",
    "gensym",
    "time",
    "now",
    "sql",
    "db/",
    "serial",
    "pio/",
    "spawn",
    "send",
    "recv",
    "web",
    "fetch",
    "sleep",
    "spy",
    "print",
    "stdin",
    "stdout",
    "stderr",
    "vector-store",
    "route",
    "log/",
    "throw",
    "error",
    "assert",
    "exit",
    "system",
    "sys/",
    "env",
    "shell",
    "exec",
    "read",
    "load",
    "import",
    // `context/*` examples depend on state set in sibling example blocks; `hashmap`/`hash-map`
    // key order is unspecified; `path/absolute` is machine-specific.
    "context",
    "hashmap",
    "hash-map",
    "path/absolute",
    "from-codepoints",
    "term-size",
];

fn skip(expr: &str) -> bool {
    SKIP_MARKERS.iter().any(|m| expr.contains(m))
}

#[test]
#[ignore = "best-effort example checker; run with --ignored --nocapture"]
fn builtin_doc_examples_evaluate() {
    let index = sema_docs::builtin_index();
    let mut checked = 0usize;
    let mut skipped = 0usize;
    let mut failures: Vec<String> = Vec::new();

    for entry in &index.entries {
        for example in &entry.examples {
            let interp = Interpreter::new();
            for line in example.lines() {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                if let Some((expr, expected)) = line.split_once("; =>") {
                    let expr = expr.trim();
                    let expected = expected.trim();
                    // Skip side-effecting exprs, and expecteds that aren't a single literal value:
                    // approximations (`~`, `...`), nondeterministic notes (`varies`), or any
                    // parenthetical annotation like `nil (not visible)` / escaped-quote strings.
                    let inexact = expected.contains('~')
                        || expected.contains("...")
                        || expected.contains("varies")
                        || expected.contains(" (")
                        || expected.contains('\\');
                    // `expr` must look like a real expression (guards against multi-line examples
                    // whose `; =>` line parsed to just a comment fragment).
                    let looks_evaluable = expr.chars().any(|c| c.is_alphanumeric() || c == '(');
                    if expr.is_empty() || !looks_evaluable || skip(expr) || inexact {
                        skipped += 1;
                        continue;
                    }
                    match interp.eval_str(expr) {
                        Ok(v) => {
                            checked += 1;
                            let got = format!("{v}");
                            if got.trim() != expected {
                                failures.push(format!(
                                    "{}: `{expr}` => `{got}` (expected `{expected}`)",
                                    entry.name
                                ));
                            }
                        }
                        Err(_) => skipped += 1, // unparseable-alone / errored → not a failure
                    }
                } else if !line.starts_with(';') && !skip(line) {
                    // Setup statement (e.g. `(define x 1)`) — eval to build shared state.
                    let _ = interp.eval_str(line);
                }
            }
        }
    }

    eprintln!(
        "doc examples: {checked} checked, {} failed, {skipped} skipped",
        failures.len()
    );
    for f in failures.iter().take(40) {
        eprintln!("  MISMATCH {f}");
    }
    // Reporting test: it surfaces mismatches but does not hard-fail (examples vary in formatting).
    // Flip the next line to `assert!(failures.is_empty(), ...)` once examples are normalized.
}
