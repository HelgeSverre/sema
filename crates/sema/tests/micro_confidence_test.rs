//! Deterministic regression tests for the micro/confidence recipe.
//!
//! Each test loads `packages/micro/package.sema` (embedded inline as a
//! string constant), defines a `defworkflow` that uses it, and drives it
//! against a scripted `FakeProvider`.

mod workflow_common;
use workflow_common as wc;

use sema_llm::fake::FakeProvider;

const MICRO_CONFIDENCE_SRC: &str = include_str!("../../../packages/micro/package.sema");

// ---------------------------------------------------------------------------
// 1. confidence — First model succeeds and meets threshold
//    Script: 1 candidate -> confidence 0.9 (>= 0.8)
// ---------------------------------------------------------------------------
#[test]
fn confidence_accepts_first_model_if_threshold_met() {
    let fake = FakeProvider::builder("fake")
        .model("fake-model")
        .reply("{\"answer\": \"Use the docs\", \"confidence\": 0.9}")
        .build();

    let src = format!(
        r#"
        {MICRO_CONFIDENCE_SRC}

        (defworkflow confidence-test-1
          "confidence macro: accepts first model"
          {{:phases ["Run"]}}
          (phase "Run")
          (def r (micro/confidence
                   {{:name "support-answer"
                    :models ["fake-model-1" "fake-model-2"]
                    :threshold 0.8
                    :schema {{:answer :string :confidence :number}}}}
                   "Prompt"))
          {{:status :success :r r}})
        "#
    );

    let out = wc::run_once(&src, fake, "wf_confidence_1");

    let ended = wc::events_of(&out.events, "run.ended");
    assert_eq!(ended[0]["status"], "success", "run must succeed");

    // The result carries the first attempt.
    assert_eq!(
        out.result["r"]["answer"].as_str(),
        Some("Use the docs"),
        "must return the candidate"
    );

    // Verify journal events
    let tool_calls = wc::events_of(&out.events, "agent.tool_call");
    let started = tool_calls.iter().find(|e| e["tool_name"] == "micro.recipe.started").expect("should journal start");
    assert_eq!(started["args_json"]["recipe"], "confidence");

    let completed = tool_calls.iter().find(|e| e["tool_name"] == "micro.candidate.completed").expect("should journal completion");
    assert_eq!(completed["args_json"]["model"], "fake-model-1");

    let ended = tool_calls.iter().find(|e| e["tool_name"] == "micro.recipe.ended").expect("should journal end");
    assert_eq!(ended["args_json"]["decision"], "accepted");
    assert_eq!(ended["args_json"]["model"], "fake-model-1");
}

// ---------------------------------------------------------------------------
// 2. confidence — First model below threshold, second model accepted
//    Script: 1 candidate -> confidence 0.5 (fails threshold) -> escalate
//            2 candidate -> confidence 0.85 (passes threshold) -> accepted
// ---------------------------------------------------------------------------
#[test]
fn confidence_escalates_if_below_threshold() {
    let fake = FakeProvider::builder("fake")
        .model("fake-model")
        .reply("{\"answer\": \"I am not sure\", \"confidence\": 0.5}")
        .reply("{\"answer\": \"I am sure\", \"confidence\": 0.85}")
        .build();

    let src = format!(
        r#"
        {MICRO_CONFIDENCE_SRC}

        (defworkflow confidence-test-2
          "confidence macro: escalates"
          {{:phases ["Run"]}}
          (phase "Run")
          (def r (micro/confidence
                   {{:name "support-answer"
                    :models ["fake-model-1" "fake-model-2"]
                    :threshold 0.8
                    :schema {{:answer :string :confidence :number}}}}
                   "Prompt"))
          {{:status :success :r r}})
        "#
    );

    let out = wc::run_once(&src, fake, "wf_confidence_2");

    let ended = wc::events_of(&out.events, "run.ended");
    assert_eq!(ended[0]["status"], "success", "run must succeed");

    assert_eq!(
        out.result["r"]["answer"].as_str(),
        Some("I am sure"),
        "must return the second candidate"
    );

    // Verify journal events
    let tool_calls = wc::events_of(&out.events, "agent.tool_call");
    
    let escalated = tool_calls.iter().find(|e| e["tool_name"] == "micro.escalated").expect("should escalate");
    assert_eq!(escalated["args_json"]["reason"], "confidence_below_threshold");
    assert_eq!(escalated["args_json"]["from"], "fake-model-1");
    assert_eq!(escalated["args_json"]["to"], "fake-model-2");

    let ended = tool_calls.iter().find(|e| e["tool_name"] == "micro.recipe.ended").expect("should journal end");
    assert_eq!(ended["args_json"]["decision"], "accepted");
    assert_eq!(ended["args_json"]["model"], "fake-model-2");
}

// ---------------------------------------------------------------------------
// 3. confidence — First model invalid schema, second model accepted
//    Script: 1 candidate -> invalid json -> step fails/throws -> try catches it
//            2 candidate -> confidence 0.9 -> accepted
//    Wait: FakeProvider needs to reply with invalid json for both initial and re-ask
// ---------------------------------------------------------------------------
#[test]
fn confidence_escalates_on_schema_invalid() {
    let fake = FakeProvider::builder("fake")
        .model("fake-model")
        .reply("not json") // attempt 1 for model 1
        .reply("still not json") // re-ask for model 1
        .reply("{\"answer\": \"I am sure\", \"confidence\": 0.9}") // model 2
        .build();

    let src = format!(
        r#"
        {MICRO_CONFIDENCE_SRC}

        (defworkflow confidence-test-3
          "confidence macro: escalates on schema invalid"
          {{:phases ["Run"]}}
          (phase "Run")
          (def r (micro/confidence
                   {{:name "support-answer"
                    :models ["fake-model-1" "fake-model-2"]
                    :threshold 0.8
                    :schema {{:answer :string :confidence :number}}}}
                   "Prompt"))
          {{:status :success :r r}})
        "#
    );

    let out = wc::run_once(&src, fake, "wf_confidence_3");

    let ended = wc::events_of(&out.events, "run.ended");
    assert_eq!(ended[0]["status"], "success", "run must succeed");

    assert_eq!(
        out.result["r"]["answer"].as_str(),
        Some("I am sure"),
        "must return the second candidate"
    );

    // Verify journal events
    let tool_calls = wc::events_of(&out.events, "agent.tool_call");
    
    let escalated = tool_calls.iter().find(|e| e["tool_name"] == "micro.escalated").expect("should escalate");
    assert_eq!(escalated["args_json"]["reason"], "schema_invalid");

    let ended = tool_calls.iter().find(|e| e["tool_name"] == "micro.recipe.ended").expect("should journal end");
    assert_eq!(ended["args_json"]["decision"], "accepted");
}

// ---------------------------------------------------------------------------
// 4. confidence — All models fail, falls back to best candidate (which is last_valid)
// ---------------------------------------------------------------------------
#[test]
fn confidence_falls_back_when_all_models_fail() {
    let fake = FakeProvider::builder("fake")
        .model("fake-model")
        .reply("{\"answer\": \"First try\", \"confidence\": 0.5}")
        .reply("{\"answer\": \"Second try\", \"confidence\": 0.6}")
        .build();

    let src = format!(
        r#"
        {MICRO_CONFIDENCE_SRC}

        (defworkflow confidence-test-4
          "confidence macro: fallback"
          {{:phases ["Run"]}}
          (phase "Run")
          (def r (micro/confidence
                   {{:name "support-answer"
                    :models ["fake-model-1" "fake-model-2"]
                    :threshold 0.8
                    :schema {{:answer :string :confidence :number}}}}
                   "Prompt"))
          {{:status :success :r r}})
        "#
    );

    let out = wc::run_once(&src, fake, "wf_confidence_4");

    let ended = wc::events_of(&out.events, "run.ended");
    assert_eq!(ended[0]["status"], "success", "run must succeed");

    // Must return the last valid candidate
    assert_eq!(
        out.result["r"]["answer"].as_str(),
        Some("Second try"),
        "must return the last valid candidate"
    );

    let tool_calls = wc::events_of(&out.events, "agent.tool_call");
    let ended = tool_calls.iter().find(|e| e["tool_name"] == "micro.recipe.ended").expect("should journal end");
    assert_eq!(ended["args_json"]["decision"], "fallback");
    assert_eq!(ended["args_json"]["model"], "fake-model-2");
}
