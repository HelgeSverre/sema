//! Deterministic real-tool-agent test (track #2, slice S2).
//!
//! `(agent prompt {:tools [...]})` routes the leaf through the REAL `run_tool_loop`
//! (via `llm/chat`, which owns the tool dispatch) and journals each genuine tool call
//! as an `agent.tool_call` event through the `:on-tool-call` callback. Driven against
//! a scripted `FakeProvider` (no network/keys) so the tool call is deterministic.
//!
//! Same env-isolation discipline as `workflow_budget_test`: own binary + a `SERIAL`
//! mutex (the `SEMA_WORKFLOW_*` env vars and the provider registry are process-global).

use std::sync::{Mutex, MutexGuard};

use sema_eval::Interpreter;
use sema_llm::builtins::{register_test_provider, reset_runtime_state};
use sema_llm::fake::FakeProvider;

static SERIAL: Mutex<()> = Mutex::new(());

fn run_workflow(src: &str, fake: FakeProvider, run_id: &str) -> Vec<serde_json::Value> {
    let _guard: MutexGuard<()> = SERIAL.lock().unwrap_or_else(|e| e.into_inner());

    let mut dir = std::env::temp_dir();
    dir.push(format!("sema-wf-tools-{}-{}", std::process::id(), run_id));
    let _ = std::fs::remove_dir_all(&dir);

    std::env::set_var("SEMA_WORKFLOW_FIXED_TS", "0");
    std::env::set_var("SEMA_WORKFLOW_RUN_ID", run_id);
    std::env::set_var("SEMA_WORKFLOW_RUN_DIR", &dir);

    let interp = Interpreter::new();
    reset_runtime_state();
    register_test_provider(Box::new(fake));
    let res = interp.eval_str_compiled(src);

    std::env::remove_var("SEMA_WORKFLOW_FIXED_TS");
    std::env::remove_var("SEMA_WORKFLOW_RUN_ID");
    std::env::remove_var("SEMA_WORKFLOW_RUN_DIR");

    res.expect("workflow body evaluates");
    let run = dir.join(run_id);
    let events = std::fs::read_to_string(run.join("events.jsonl")).expect("events.jsonl");
    let parsed = events
        .lines()
        .map(|l| serde_json::from_str(l).expect("valid event json"))
        .collect();
    let _ = std::fs::remove_dir_all(&dir);
    parsed
}

#[test]
fn tools_agent_journals_one_real_tool_call() {
    // Round 1: the model emits a tool call. Round 2: a final reply. The real loop runs
    // → the on-tool-call callback fires once (gated on "start") → one agent.tool_call.
    let fake = FakeProvider::builder("fake")
        .model("fake-model")
        .tool_call(
            "call_1",
            "get-weather",
            serde_json::json!({ "city": "Oslo" }),
        )
        .reply("It is sunny in Oslo.")
        .build();

    let src = r#"
        (deftool get-weather
          "Get current weather for a city"
          {:city {:type :string :description "City name"}}
          (lambda (city) (str "{\"city\":\"" city "\",\"temp\":22}")))

        (defworkflow tool-demo
          "one tool-using agent"
          {:phases ["Go"]}
          (phase "Go")
          (def r (agent "What is the weather in Oslo?" {:tools [get-weather] :name "scout"}))
          {:status :success :r r})
    "#;

    let events = run_workflow(src, fake, "wf_tools_demo");

    // Exactly ONE agent.tool_call — the gate-on-"start" prevents the start+end double.
    let tool_calls: Vec<&serde_json::Value> = events
        .iter()
        .filter(|e| e["event"] == "agent.tool_call")
        .collect();
    assert_eq!(
        tool_calls.len(),
        1,
        "expected exactly one journaled tool call, got {}",
        tool_calls.len()
    );
    let tc = tool_calls[0];
    assert_eq!(tc["tool_name"], "get-weather");
    assert_eq!(
        tc["agent_id"], "scout_1",
        "tool call attributes to the scout agent"
    );
    let args_json = tc["args_json"].as_str().unwrap_or("");
    assert!(
        args_json.contains("Oslo") || args_json.contains("city"),
        "args_json should carry the real call args, got {args_json:?}"
    );

    // The run completed successfully (the loop returned the final text).
    let ended: Vec<&serde_json::Value> = events
        .iter()
        .filter(|e| e["event"] == "run.ended")
        .collect();
    assert_eq!(ended[0]["status"], "success");

    // The agent row is present and ok.
    let results: Vec<&serde_json::Value> = events
        .iter()
        .filter(|e| e["event"] == "agent.result")
        .collect();
    assert_eq!(results.len(), 1);
    assert_eq!(results[0]["status"], "ok");
}
