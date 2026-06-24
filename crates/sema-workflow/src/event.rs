//! The FROZEN workflow event vocabulary.
//!
//! Determinism is NOT free: `serde_json` is built without `preserve_order` in this
//! workspace (root `Cargo.toml`), so any *map*-encoded event would emit keys in
//! alphabetical order — a fragile basis for a byte-identical golden. We therefore
//! encode events as a `#[derive(Serialize)]` enum with an internal `event` tag and
//! explicit per-variant field declarations: serde emits struct fields in *source
//! declaration order*, which is stable and reviewable. Do NOT replace this with a
//! `serde_json::Map`.
//!
//! Tag values carry dots (`run.started`), which `rename_all = "snake_case"` cannot
//! produce, so every variant pins its tag with an explicit `#[serde(rename = "…")]`.
//!
//! Field ordering convention: `seq` then `ts` lead every variant (so a human or
//! `jq` scan sees ordering+time first), followed by the variant-specific payload.
//!
//! This vocabulary is FROZEN. Add fields to existing variants (append-only, all
//! `Option`/skippable to keep old goldens valid) rather than inventing new variants.

use serde::Serialize;

/// One journaled workflow event. Serialized as a single JSON object per line via
/// [`crate::Journal::write`]. The `event` tag discriminates the variant.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "event")]
pub enum WorkflowEvent {
    /// First line of every run. Records the workflow identity and the inputs/budget
    /// snapshot so the journal is self-describing without args.json/metadata.json.
    #[serde(rename = "run.started")]
    RunStarted {
        seq: u64,
        ts: String,
        /// `defworkflow` name.
        workflow: String,
        /// Run identity (the `<run-id>` directory name).
        run_id: String,
        /// Per-workflow-form code-version hash (Spike 4 contract — recorded now so
        /// the frozen metadata need not be re-opened later). Empty if the caller has
        /// no hash yet.
        #[serde(default, skip_serializing_if = "String::is_empty")]
        code_version: String,
        /// The run's `--args` input as a JSON string (the dashboard shows it on the
        /// run.started stream row). Empty when no args were supplied.
        #[serde(default, skip_serializing_if = "String::is_empty")]
        args_json: String,
    },

    /// A `phase` opened. Paired with exactly one [`Self::PhaseEnded`] (emitted even on
    /// the error path).
    #[serde(rename = "phase.started")]
    PhaseStarted { seq: u64, ts: String, phase: String },

    /// A `phase` closed. `status` is `"success"` or `"failed"`. `dur_ms` is `0` under
    /// the fixed-timestamp test seam.
    #[serde(rename = "phase.ended")]
    PhaseEnded {
        seq: u64,
        ts: String,
        phase: String,
        status: String,
        dur_ms: u64,
    },

    /// An agent leaf began executing. `agent_id` is the per-invocation correlation
    /// key (e.g. `write-article_2`); `agent_name` is the role label; `model` is the
    /// LLM model (empty until known — it is filled on [`Self::AgentResult`]).
    #[serde(rename = "agent.started")]
    AgentStarted {
        seq: u64,
        ts: String,
        agent_id: String,
        agent_name: String,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        model: String,
    },

    /// An agent leaf produced a result. `status` is `"ok"`/`"failed"`. `output` is an
    /// OPAQUE string/digest only. `model` is the model the call used (from usage),
    /// filled here because it is unknown when the agent started.
    #[serde(rename = "agent.result")]
    AgentResult {
        seq: u64,
        ts: String,
        agent_id: String,
        status: String,
        /// Opaque agent output (string or digest). Never a structured/typed field.
        output: String,
        dur_ms: u64,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        model: String,
    },

    /// An agent invoked a tool. Names + opaque argument digest only.
    #[serde(rename = "agent.tool_call")]
    AgentToolCall {
        seq: u64,
        ts: String,
        agent_id: String,
        tool_name: String,
        /// Opaque digest of the call arguments (a `"gated"` sentinel = not captured).
        #[serde(default, skip_serializing_if = "String::is_empty")]
        args_json: String,
    },

    /// A `checkpoint` recorded a keyed step value. The value itself is NOT stored in
    /// the event stream — only a (lossy) digest — and a `content_key` resume hash.
    #[serde(rename = "checkpoint")]
    Checkpoint {
        seq: u64,
        ts: String,
        /// `start_seq` of the enclosing phase (the dashboard nests it there).
        #[serde(default, skip_serializing_if = "Option::is_none")]
        phase_seq: Option<u64>,
        /// Checkpoint key (the `:k` of `(checkpoint :k v)`), as a bare name.
        key: String,
        /// Stable resume key (hash of inputs + code version); short hex.
        #[serde(default, skip_serializing_if = "String::is_empty")]
        content_key: String,
        /// Opaque digest of the recorded value.
        value_digest: String,
    },

    /// A budget / usage observation. Per-event (never a mutated counter) so summing
    /// over events never double-charges. Carries an `agent_id` when the usage is
    /// attributable to one agent leaf (the only way per-agent tokens are shown).
    #[serde(rename = "budget")]
    Budget {
        seq: u64,
        ts: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        agent_id: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        phase_seq: Option<u64>,
        input_tokens: u64,
        output_tokens: u64,
        /// `None` when pricing is unknown for the model (genuinely absent, not 0).
        #[serde(default, skip_serializing_if = "Option::is_none")]
        cost_usd: Option<f64>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        budget_limit: Option<u64>,
    },

    /// Last line of every run. `status` mirrors the `{:status …}` envelope's status
    /// (`"success"` / `"failed"`); `reason` carries the failure reason when failed.
    #[serde(rename = "run.ended")]
    RunEnded {
        seq: u64,
        ts: String,
        status: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        reason: Option<String>,
        dur_ms: u64,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The tag must come FIRST and carry the dotted name; field order is declaration
    /// order (NOT alphabetical). This pins the wire shape the golden depends on.
    #[test]
    fn run_started_wire_shape() {
        let ev = WorkflowEvent::RunStarted {
            seq: 0,
            ts: "0".into(),
            workflow: "hello-wf".into(),
            run_id: "wf_test_0001".into(),
            code_version: String::new(), // skipped when empty
            args_json: String::new(),    // skipped when empty
        };
        let line = serde_json::to_string(&ev).unwrap();
        assert_eq!(
            line,
            r#"{"event":"run.started","seq":0,"ts":"0","workflow":"hello-wf","run_id":"wf_test_0001"}"#
        );
    }

    #[test]
    fn agent_events_use_agent_id() {
        let started = WorkflowEvent::AgentStarted {
            seq: 5,
            ts: "0".into(),
            agent_id: "auditor_1".into(),
            agent_name: "auditor".into(),
            model: String::new(),
        };
        assert_eq!(
            serde_json::to_string(&started).unwrap(),
            r#"{"event":"agent.started","seq":5,"ts":"0","agent_id":"auditor_1","agent_name":"auditor"}"#
        );
        let budget = WorkflowEvent::Budget {
            seq: 9,
            ts: "0".into(),
            agent_id: Some("auditor_1".into()),
            phase_seq: Some(4),
            input_tokens: 3120,
            output_tokens: 880,
            cost_usd: Some(0.0041),
            budget_limit: Some(250000),
        };
        let line = serde_json::to_string(&budget).unwrap();
        assert!(
            line.contains(r#""agent_id":"auditor_1""#) && line.contains(r#""input_tokens":3120"#)
        );
    }

    #[test]
    fn phase_ended_carries_status_and_dur() {
        let ev = WorkflowEvent::PhaseEnded {
            seq: 3,
            ts: "0".into(),
            phase: "Inventory".into(),
            status: "success".into(),
            dur_ms: 0,
        };
        let line = serde_json::to_string(&ev).unwrap();
        assert_eq!(
            line,
            r#"{"event":"phase.ended","seq":3,"ts":"0","phase":"Inventory","status":"success","dur_ms":0}"#
        );
    }

    #[test]
    fn checkpoint_digest_only() {
        let ev = WorkflowEvent::Checkpoint {
            seq: 2,
            ts: "0".into(),
            phase_seq: Some(1),
            key: "files".into(),
            content_key: "ck_4d2f8a1c".into(),
            value_digest: "abc123".into(),
        };
        let line = serde_json::to_string(&ev).unwrap();
        assert_eq!(
            line,
            r#"{"event":"checkpoint","seq":2,"ts":"0","phase_seq":1,"key":"files","content_key":"ck_4d2f8a1c","value_digest":"abc123"}"#
        );
    }
}
