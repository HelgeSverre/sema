//! Deterministic, key-free tests of the LLM/agent paths using a scripted
//! `FakeProvider`. This is the regression oracle for the agent tool loop —
//! including the round-2 tool-result message shape that the Phase 2 fix targets.
//!
//! No network, no API keys: `register_test_provider` installs the fake as the
//! default provider into the thread-local registry the runtime reads from.

use std::sync::Arc;

use sema_core::Value;
use sema_eval::Interpreter;
use sema_llm::builtins::{register_test_provider, reset_runtime_state};
use sema_llm::fake::{FakeProvider, FakeRecorder};

/// Build an interpreter, install `fake` as the default provider, run `src`.
/// Returns the eval result plus the recorder handle for asserting on the exact
/// requests the runtime built.
fn eval_with_fake(
    src: &str,
    fake: FakeProvider,
) -> (Result<Value, sema_core::SemaError>, Arc<FakeRecorder>) {
    let interp = Interpreter::new();
    // Fresh provider state, then install the fake as default.
    reset_runtime_state();
    let recorder = fake.recorder();
    register_test_provider(Box::new(fake));
    let result = interp.eval_str_compiled(src);
    (result, recorder)
}

#[test]
fn llm_complete_returns_scripted_text() {
    let fake = FakeProvider::builder("fake").reply("hello there").build();
    let (result, recorder) = eval_with_fake(r#"(llm/complete "say hi")"#, fake);
    let val = result.expect("llm/complete should succeed against the fake");
    assert_eq!(val.as_str(), Some("hello there"));
    assert_eq!(recorder.call_count(), 1);
}

#[test]
fn agent_loop_completes_with_tool_call() {
    // Round 1: the model emits a tool call. Round 2 (after the tool result is fed
    // back): the model returns the final answer.
    let fake = FakeProvider::builder("fake")
        .model("fake-model")
        .tool_call("call_1", "get-weather", serde_json::json!({"city": "Oslo"}))
        .reply("It is sunny in Oslo.")
        .build();

    let src = r#"
        (deftool get-weather
          "Get current weather for a city"
          {:city {:type :string :description "City name"}}
          (lambda (city)
            (format "{\"city\": \"~a\", \"temp\": 22, \"condition\": \"sunny\"}" city)))

        (defagent weather-bot
          {:model "fake-model"
           :system "You are a weather assistant. Use tools. Be concise."
           :tools [get-weather]
           :max-turns 5})

        (agent/run weather-bot "What's the weather in Oslo?")
    "#;

    let (result, recorder) = eval_with_fake(src, fake);
    let val = result.expect("agent/run should complete against the fake");

    // The agent loop ran two provider rounds and returned the final answer.
    assert_eq!(val.as_str(), Some("It is sunny in Oslo."));
    assert_eq!(
        recorder.call_count(),
        2,
        "expected exactly 2 provider rounds (tool call, then final answer)"
    );

    // Round 2 must carry MORE messages than round 1 — i.e. the assistant turn and
    // the tool result were fed back into history. (The strict correlation check —
    // that the tool result is a correlated `role:tool` / tool_result message with
    // a tool_call_id — is asserted in `agent_loop_round2_is_correlated` once the
    // Phase 2 message model lands.)
    let reqs = recorder.requests();
    assert_eq!(reqs.len(), 2);
    assert!(
        reqs[1].messages.len() > reqs[0].messages.len(),
        "round 2 should include the fed-back tool result"
    );

    // NOTE: the strict correlation oracle (`agent_loop_round2_is_correlated`) —
    // asserting round 2 echoes the assistant tool_calls and sends a `role:tool`
    // result keyed by tool_call_id — is added in Phase 2, since it references
    // ChatMessage fields that don't exist yet.
}
