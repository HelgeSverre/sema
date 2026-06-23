//! Gate for concurrent single-shot `llm/embed` with full per-task OTel tracing.
//!
//! Two concurrent `llm/embed`s (spawned tasks) must (1) OVERLAP on the
//! cooperative scheduler — wall ≈ max(delay), not the sum — and (2) each emit a
//! DISTINCT, correctly-isolated `embeddings` span: distinct trace_id + span_id,
//! neither parenting the other, each carrying its OWN input-token total (the
//! per-task otel-isolation proof). The sync path is unchanged: one embed outside
//! an async context emits exactly one correct span.
//!
//! Deterministic + keyless (a delayed FakeProvider embed). Own binary — the
//! in-memory exporter and `sema_otel::testing::install()` are process-global, so
//! the timing/overlap test must not share a process with unrelated span capture.

#![cfg(not(target_arch = "wasm32"))]

use std::time::Instant;

use sema_eval::Interpreter;
use sema_llm::builtins::{register_test_provider, reset_runtime_state};
use sema_llm::fake::FakeProvider;
use serial_test::serial;

/// Two concurrent `llm/embed`s overlap AND produce two distinct, isolated
/// `embeddings` spans. The FakeProvider injects a 300 ms delay into each
/// `embed()`, so two serial embeds would take ~600 ms; overlapping ~300 ms.
/// Each embed is scripted with a DISTINCT prompt-token count (7 / 11) so the two
/// spans must carry their own input-token totals — proving the per-task otel TLS
/// swap kept them isolated rather than cross-contaminating one shared stack.
#[test]
#[serial]
fn two_concurrent_embeds_overlap_with_isolated_spans() {
    let cap = sema_otel::testing::install();

    // Two scripted embeds, in spawn order: distinct vectors + distinct token
    // counts. `async/all` preserves spawn (input) order, so embed #0 (7 tokens)
    // resolves first in the result, embed #1 (11 tokens) second.
    let fake = FakeProvider::builder("fake")
        .model("fake-embed")
        .embed_delay(300)
        .embed_with_tokens(vec![vec![0.1, 0.2, 0.3]], 7)
        .embed_with_tokens(vec![vec![0.4, 0.5, 0.6]], 11)
        .build();

    let interp = Interpreter::new();
    reset_runtime_state();
    register_test_provider(Box::new(fake));

    let program = r#"
        (let ((t0 (sys/elapsed)))
          (let ((res (async/all
                       (map (fn (t) (async/spawn (fn () (embedding/length (llm/embed t)))))
                            (list "alpha" "beta")))))
            (list res (floor (/ (- (sys/elapsed) t0) 1000000)))))
    "#;

    let t0 = Instant::now();
    let result = interp
        .eval_str_compiled(program)
        .expect("concurrent embed program evaluated");
    let wall_ms = t0.elapsed().as_millis();

    // (1a) Correctness: two embeddings, each 3-dim (24 bytes / 8).
    let outer = result.as_list().expect("result is (results wall-ms)");
    let res = outer[0].as_list().expect("results list");
    assert_eq!(res.len(), 2, "expected two embedding results");
    assert_eq!(res[0].as_int(), Some(3), "embed #0 has 3 dims");
    assert_eq!(res[1].as_int(), Some(3), "embed #1 has 3 dims");

    // (1b) Overlap: serial floor ~600 ms; overlapping ~300 ms. Generous ceiling.
    assert!(
        wall_ms < 500,
        "expected overlapped wall-clock < 500 ms (serial floor ~600 ms), got {wall_ms} ms"
    );

    // (2) Two distinct, isolated `embeddings` spans.
    let spans = cap.spans_json();
    let embed_spans: Vec<&serde_json::Value> = spans
        .iter()
        .filter(|s| s["attributes"]["gen_ai.operation.name"] == "embeddings")
        .collect();
    assert_eq!(
        embed_spans.len(),
        2,
        "expected exactly two embeddings spans, got {}",
        embed_spans.len()
    );

    let trace_ids: Vec<&serde_json::Value> = embed_spans.iter().map(|s| &s["trace_id"]).collect();
    let span_ids: Vec<&serde_json::Value> = embed_spans.iter().map(|s| &s["span_id"]).collect();

    // Distinct span ids (always) and distinct trace ids (each detached embed is
    // its own root → its own trace).
    assert_ne!(
        span_ids[0], span_ids[1],
        "the two spans must have distinct span_ids"
    );
    assert_ne!(
        trace_ids[0], trace_ids[1],
        "the two embeds run in distinct tasks → distinct traces"
    );

    // Neither span parents the other (no cross-task nesting from a shared stack).
    for s in &embed_spans {
        let parent = &s["parent_span_id"];
        assert_ne!(parent, span_ids[0], "span parented under the other embed");
        assert_ne!(parent, span_ids[1], "span parented under the other embed");
    }

    // Each span carries its OWN input-token total (7 and 11), proving the
    // per-task otel context swap kept the two spans isolated.
    let mut tokens: Vec<i64> = embed_spans
        .iter()
        .map(|s| {
            s["attributes"]["gen_ai.usage.input_tokens"]
                .as_i64()
                .expect("input_tokens present")
        })
        .collect();
    tokens.sort_unstable();
    assert_eq!(
        tokens,
        vec![7, 11],
        "each embed span must carry its own input-token count (7 and 11), got {tokens:?}"
    );
    // And output tokens are zero on both (embeddings report input only).
    for s in &embed_spans {
        assert_eq!(s["attributes"]["gen_ai.usage.output_tokens"], 0);
        assert_eq!(s["kind"], "client");
        assert_eq!(s["attributes"]["gen_ai.provider.name"], "fake");
    }
}

/// Sync path unchanged: a single `(llm/embed "x")` OUTSIDE any async context emits
/// exactly one correct `embeddings` span via the synchronous path.
#[test]
#[serial]
fn sync_embed_outside_async_emits_one_span() {
    let cap = sema_otel::testing::install();

    let fake = FakeProvider::builder("fake")
        .model("fake-embed")
        .embed_with_tokens(vec![vec![0.1, 0.2, 0.3]], 5)
        .build();
    let interp = Interpreter::new();
    reset_runtime_state();
    register_test_provider(Box::new(fake));

    interp
        .eval_str_compiled(r#"(llm/embed "hello world")"#)
        .expect("sync embed should run against the fake");

    let embed_spans: Vec<serde_json::Value> = cap
        .spans_json()
        .into_iter()
        .filter(|s| s["attributes"]["gen_ai.operation.name"] == "embeddings")
        .collect();
    assert_eq!(
        embed_spans.len(),
        1,
        "exactly one embeddings span on the sync path"
    );
    let embed = &embed_spans[0];
    assert_eq!(embed["kind"], "client");
    assert_eq!(embed["attributes"]["gen_ai.provider.name"], "fake");
    assert_eq!(embed["attributes"]["gen_ai.response.model"], "fake-embed");
    assert_eq!(embed["attributes"]["gen_ai.usage.input_tokens"], 5);
    assert_eq!(embed["attributes"]["gen_ai.usage.output_tokens"], 0);
}

/// Live overlap (run with `--ignored` and OPENAI_API_KEY set): two concurrent
/// real `text-embedding-3-small` embeds overlap and emit two distinct spans.
#[test]
#[ignore]
#[serial]
fn live_two_concurrent_real_embeds_overlap() {
    if std::env::var("OPENAI_API_KEY").is_err() {
        eprintln!("skipping: OPENAI_API_KEY not set");
        return;
    }
    let cap = sema_otel::testing::install();
    let interp = Interpreter::new();

    let program = r#"
        (llm/configure-embeddings :openai {:api-key (env "OPENAI_API_KEY")
                                           :model "text-embedding-3-small"})
        (let ((t0 (sys/elapsed)))
          (let ((res (async/all
                       (map (fn (t) (async/spawn (fn () (embedding/length (llm/embed t)))))
                            (list "the quick brown fox" "lorem ipsum dolor")))))
            (list res (floor (/ (- (sys/elapsed) t0) 1000000)))))
    "#;
    let result = interp
        .eval_str_compiled(program)
        .expect("live concurrent embeds evaluated");
    let outer = result.as_list().expect("(results wall-ms)");
    let res = outer[0].as_list().expect("results");
    assert_eq!(res.len(), 2, "two embeddings");

    let embed_spans: Vec<serde_json::Value> = cap
        .spans_json()
        .into_iter()
        .filter(|s| s["attributes"]["gen_ai.operation.name"] == "embeddings")
        .collect();
    assert_eq!(embed_spans.len(), 2, "two embeddings spans");
    assert_ne!(
        embed_spans[0]["span_id"], embed_spans[1]["span_id"],
        "distinct span_ids"
    );
    assert_ne!(
        embed_spans[0]["trace_id"], embed_spans[1]["trace_id"],
        "distinct trace_ids"
    );
}
