//! Gap-2: a failed completion records error.type + Error status on the chat span.
//! Own binary (global provider is process-global).

#![cfg(not(target_arch = "wasm32"))]

use sema_eval::Interpreter;
use sema_llm::builtins::{register_test_provider, reset_runtime_state};
use sema_llm::fake::FakeProvider;
use sema_llm::types::LlmError;

#[test]
fn provider_error_tags_span_error() {
    let cap = sema_otel::testing::install();

    // A non-retryable 400 fails fast (no retries).
    let fake = FakeProvider::builder("fake")
        .model("fake-model")
        .error(LlmError::Api {
            status: 400,
            message: "bad request".into(),
        })
        .build();
    let interp = Interpreter::new();
    reset_runtime_state();
    register_test_provider(Box::new(fake));

    let res = interp.eval_str_compiled(r#"(llm/complete "boom")"#);
    assert!(res.is_err(), "the completion should fail");

    let chat = cap
        .spans_json()
        .into_iter()
        .find(|s| s["attributes"]["gen_ai.operation.name"] == "chat")
        .expect("a chat span should still be emitted on error");
    assert_eq!(chat["attributes"]["error.type"], "provider_error");
    assert!(
        chat["status"]
            .as_str()
            .is_some_and(|s| s.starts_with("error")),
        "span status should be Error, got {:?}",
        chat["status"]
    );
}
