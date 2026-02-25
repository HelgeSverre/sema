use serde::{Deserialize, Serialize};

/// Generic DAP message envelope (incoming).
#[derive(Debug, Deserialize)]
pub struct DapMessage {
    pub seq: u64,
    #[serde(rename = "type")]
    pub msg_type: String,
    pub command: Option<String>,
    pub arguments: Option<serde_json::Value>,
    pub event: Option<String>,
    pub body: Option<serde_json::Value>,
}

/// Generic DAP response (outgoing).
#[derive(Debug, Serialize)]
pub struct DapResponse {
    pub seq: u64,
    #[serde(rename = "type")]
    pub msg_type: String,
    pub request_seq: u64,
    pub success: bool,
    pub command: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<serde_json::Value>,
}

/// Generic DAP event (outgoing).
#[derive(Debug, Serialize)]
pub struct DapEvent {
    pub seq: u64,
    #[serde(rename = "type")]
    pub msg_type: String,
    pub event: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<serde_json::Value>,
}

impl DapResponse {
    pub fn success(
        seq: u64,
        request_seq: u64,
        command: &str,
        body: Option<serde_json::Value>,
    ) -> Self {
        DapResponse {
            seq,
            msg_type: "response".to_string(),
            request_seq,
            success: true,
            command: command.to_string(),
            message: None,
            body,
        }
    }

    pub fn error(seq: u64, request_seq: u64, command: &str, message: &str) -> Self {
        DapResponse {
            seq,
            msg_type: "response".to_string(),
            request_seq,
            success: false,
            command: command.to_string(),
            message: Some(message.to_string()),
            body: None,
        }
    }
}

impl DapEvent {
    pub fn new(seq: u64, event: &str, body: Option<serde_json::Value>) -> Self {
        DapEvent {
            seq,
            msg_type: "event".to_string(),
            event: event.to_string(),
            body,
        }
    }
}
