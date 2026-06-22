//! Compat-layer auto-tags, user `:tags`/`:metadata`, and streaming time-to-first-token.
//! Isolated in its own test binary because `sema_otel::testing::install()` + `set_compat`
//! are process-global (sibling tests emitting spans would pollute the capture). All three
//! scenarios run under a single install and are told apart by their distinct request model.

use sema_eval::Interpreter;
use sema_llm::builtins::{register_test_provider, reset_runtime_state};
use sema_llm::fake::FakeProvider;

fn run(src: &str, fake: FakeProvider) -> Result<sema_core::Value, sema_core::SemaError> {
    let interp = Interpreter::new();
    reset_runtime_state();
    register_test_provider(Box::new(fake));
    interp.eval_str_compiled(src)
}

#[test]
fn auto_tags_user_tags_metadata_and_ttft_arrive_on_spans() {
    // Content capture on so embedding input texts are emitted.
    std::env::set_var("OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT", "true");
    // Turn on every backend whose tag/metadata/ttft/cost/embedding keys we assert.
    sema_otel::testing::set_compat("openinference,langfuse,langsmith,traceloop,braintrust");
    let cap = sema_otel::testing::install();

    // (1) auto-tags + user :tags on a standalone completion.
    run(
        r#"(llm/complete "p" {:model "m-tags" :tags ["prod" "v2"]})"#,
        FakeProvider::builder("fake")
            .model("m-tags")
            .reply("a")
            .build(),
    )
    .expect("tags completion");

    // (2) :metadata fans out per backend prefix.
    run(
        r#"(llm/complete "p" {:model "m-meta" :metadata {:env "prod" :team "ml"}})"#,
        FakeProvider::builder("fake")
            .model("m-meta")
            .reply("a")
            .build(),
    )
    .expect("metadata completion");

    // (3) streaming time-to-first-token.
    run(
        r#"(llm/stream "p" (lambda (c) nil) {:model "m-stream"})"#,
        FakeProvider::builder("fake")
            .model("m-stream")
            .stream(&["hello ", "world"])
            .build(),
    )
    .expect("streamed completion");

    // (4) llm/chat must also honor user :tags / :metadata (regression: it was unwired).
    run(
        r#"(llm/chat (list (message :user "p"))
             {:model "m-chat" :tags ["chat-tag"] :metadata {:env "prod"}})"#,
        FakeProvider::builder("fake")
            .model("m-chat")
            .reply("a")
            .build(),
    )
    .expect("chat completion");

    // (5) per-direction cost split (OpenInference llm.cost.prompt / .completion). Pin
    // deterministic pricing: 1000 prompt tok @ $10/M = $0.01, 1000 completion @ $30/M = $0.03.
    run(
        r#"(begin (llm/set-pricing "costmodel" 10.0 30.0)
                  (llm/complete "p" {:model "costmodel"}))"#,
        FakeProvider::builder("fake")
            .model("costmodel")
            .reply_with_usage("a", 1000, 1000)
            .build(),
    )
    .expect("cost-split completion");

    // (6) embedding detail: named model + (gated) input texts.
    run(
        r#"(llm/embed "embed this text" {:model "m-embed"})"#,
        FakeProvider::builder("fake")
            .model("m-embed")
            .embed(vec![vec![0.1, 0.2, 0.3]])
            .build(),
    )
    .expect("embedding");

    // (7) reranker span (OpenInference RERANKER).
    run(
        r#"(llm/rerank "q" (list "a" "b") {:top-k 2 :model "rr-model"})"#,
        FakeProvider::builder("fake")
            .model("fake")
            .rerank(&[(1, 0.9), (0, 0.2)])
            .build(),
    )
    .expect("rerank");

    // (8) retriever span (OpenInference RETRIEVER) from a vector-store search.
    run(
        r#"(begin
             (vector-store/create "t")
             (define qv (llm/embed "seed"))
             (vector-store/add "t" "doc1" qv {:text "hello world"})
             (vector-store/search "t" qv 3))"#,
        FakeProvider::builder("fake")
            .model("fake")
            .embed(vec![vec![0.1, 0.2, 0.3]])
            .build(),
    )
    .expect("retrieve");

    let spans = cap.spans_json();
    let by_model = |m: &str| {
        spans
            .iter()
            .find(|s| s["attributes"]["gen_ai.request.model"] == m)
            .cloned()
            .unwrap_or_else(|| panic!("no chat span for model {m}"))
    };

    // (1) Tags: auto-derived operation/model + the two user tags, on langfuse.trace.tags.
    let tags_span = by_model("m-tags");
    let tags = tags_span["attributes"]["langfuse.trace.tags"]
        .as_array()
        .expect("langfuse.trace.tags is a string array");
    let tags: Vec<&str> = tags.iter().filter_map(|t| t.as_str()).collect();
    for expected in ["operation:chat", "model:m-tags", "prod", "v2"] {
        assert!(
            tags.contains(&expected),
            "expected tag {expected:?} in {tags:?}"
        );
    }
    // LangSmith uses a CSV form of the same tags.
    let ls_tags = tags_span["attributes"]["langsmith.span.tags"]
        .as_str()
        .expect("langsmith.span.tags is a CSV string");
    assert!(ls_tags.contains("prod") && ls_tags.contains("model:m-tags"));

    // (2) Metadata: same keys under each backend's prefix.
    let meta_span = by_model("m-meta");
    let a = &meta_span["attributes"];
    assert_eq!(a["langfuse.trace.metadata.env"], "prod");
    assert_eq!(a["langfuse.trace.metadata.team"], "ml");
    assert_eq!(a["langsmith.metadata.env"], "prod");
    assert_eq!(a["traceloop.association.properties.team"], "ml");
    let bt: serde_json::Value = serde_json::from_str(
        a["braintrust.metadata"]
            .as_str()
            .expect("braintrust.metadata is a JSON string"),
    )
    .expect("braintrust.metadata parses");
    assert_eq!(bt["env"], "prod");

    // (3) TTFT: always-on sema markers + the Langfuse/OpenLLMetry compat keys.
    let stream_span = by_model("m-stream");
    let a = &stream_span["attributes"];
    assert_eq!(a["sema.gen_ai.is_streaming"], true);
    assert!(
        a["sema.gen_ai.server.time_to_first_token"].is_number(),
        "ttft must be a number"
    );
    let cst = a["langfuse.observation.completion_start_time"]
        .as_str()
        .expect("langfuse completion_start_time is an RFC3339 string");
    assert!(
        cst.ends_with('Z') && cst.contains('T'),
        "completion_start_time {cst:?} must be RFC3339 UTC"
    );
    // OpenLLMetry's real streaming span attribute is the boolean gen_ai.is_streaming.
    assert_eq!(a["gen_ai.is_streaming"], true);

    // (4) llm/chat honors user :tags / :metadata too.
    let chat_span = by_model("m-chat");
    let a = &chat_span["attributes"];
    let chat_tags: Vec<&str> = a["langfuse.trace.tags"]
        .as_array()
        .expect("chat span has langfuse.trace.tags")
        .iter()
        .filter_map(|t| t.as_str())
        .collect();
    assert!(
        chat_tags.contains(&"chat-tag"),
        "user tag must survive on an llm/chat span: {chat_tags:?}"
    );
    assert_eq!(a["langfuse.trace.metadata.env"], "prod");

    // (5) Cost split: prompt/completion costs render separately (OpenInference).
    let cost_span = by_model("costmodel");
    let a = &cost_span["attributes"];
    assert_eq!(a["llm.cost.prompt"], 0.01);
    assert_eq!(a["llm.cost.completion"], 0.03);
    assert_eq!(a["llm.cost.total"], 0.04);

    // (6) Embedding span: named model + gated input text.
    let embed_span = by_model("m-embed");
    let a = &embed_span["attributes"];
    assert_eq!(a["embedding.model_name"], "m-embed");
    assert_eq!(
        a["embedding.embeddings.0.embedding.text"],
        "embed this text"
    );

    let by_name = |n: &str| {
        spans
            .iter()
            .find(|s| s["name"] == n)
            .cloned()
            .unwrap_or_else(|| panic!("no span named {n}"))
    };

    // (7) Reranker span: OpenInference RERANKER kind + meta + scored output documents.
    let rr = by_name("rerank");
    let a = &rr["attributes"];
    assert_eq!(a["openinference.span.kind"], "RERANKER");
    assert_eq!(a["reranker.model_name"], "rr-model");
    assert_eq!(a["reranker.top_k"], 2);
    assert_eq!(a["reranker.query"], "q"); // content-gated, capture is on
                                          // Highest-relevance output document first, with its score.
    assert_eq!(a["reranker.output_documents.0.document.score"], 0.9);

    // (8) Retriever span: OpenInference RETRIEVER kind + per-document id/score/content.
    let rt = by_name("retrieve");
    let a = &rt["attributes"];
    assert_eq!(a["openinference.span.kind"], "RETRIEVER");
    assert_eq!(a["retrieval.documents.0.document.id"], "doc1");
    assert_eq!(a["retrieval.documents.0.document.content"], "hello world");
    assert!(a["retrieval.documents.0.document.score"].is_number());
}
