use serde::{Deserialize, Serialize};

/// A content block in a chat message — either text or an image.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ContentBlock {
    #[serde(rename = "text")]
    Text { text: String },
    #[serde(rename = "image")]
    Image {
        #[serde(skip_serializing_if = "Option::is_none")]
        media_type: Option<String>,
        /// Base64-encoded image data
        data: String,
    },
}

/// Message content: either a simple string or multi-modal content blocks.
#[derive(Debug, Clone)]
pub enum MessageContent {
    Text(String),
    Blocks(Vec<ContentBlock>),
}

impl MessageContent {
    pub fn text(s: impl Into<String>) -> Self {
        MessageContent::Text(s.into())
    }

    pub fn as_text(&self) -> Option<&str> {
        match self {
            MessageContent::Text(s) => Some(s),
            MessageContent::Blocks(blocks) => {
                if blocks.len() == 1 {
                    if let ContentBlock::Text { text } = &blocks[0] {
                        return Some(text);
                    }
                }
                None
            }
        }
    }

    /// Get the text content, concatenating if needed.
    pub fn to_text(&self) -> String {
        match self {
            MessageContent::Text(s) => s.clone(),
            MessageContent::Blocks(blocks) => blocks
                .iter()
                .filter_map(|b| match b {
                    ContentBlock::Text { text } => Some(text.as_str()),
                    _ => None,
                })
                .collect::<Vec<_>>()
                .join(""),
        }
    }

    pub fn has_images(&self) -> bool {
        match self {
            MessageContent::Text(_) => false,
            MessageContent::Blocks(blocks) => blocks
                .iter()
                .any(|b| matches!(b, ContentBlock::Image { .. })),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChatMessage {
    pub role: String,
    pub content: MessageContent,
}

impl ChatMessage {
    pub fn new(role: impl Into<String>, content: impl Into<String>) -> Self {
        ChatMessage {
            role: role.into(),
            content: MessageContent::Text(content.into()),
        }
    }

    pub fn with_blocks(role: impl Into<String>, blocks: Vec<ContentBlock>) -> Self {
        ChatMessage {
            role: role.into(),
            content: MessageContent::Blocks(blocks),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolCall {
    pub id: String,
    pub name: String,
    pub arguments: serde_json::Value,
}

#[derive(Debug, Clone)]
pub struct ChatRequest {
    pub model: String,
    pub messages: Vec<ChatMessage>,
    pub max_tokens: Option<u32>,
    pub temperature: Option<f64>,
    pub system: Option<String>,
    pub tools: Vec<ToolSchema>,
    pub stop_sequences: Vec<String>,
    /// When true, providers that support it will request JSON output mode.
    pub json_mode: bool,
}

impl ChatRequest {
    pub fn new(model: String, messages: Vec<ChatMessage>) -> Self {
        ChatRequest {
            model,
            messages,
            max_tokens: Some(4096),
            temperature: None,
            system: None,
            tools: Vec::new(),
            stop_sequences: Vec::new(),
            json_mode: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChatResponse {
    pub content: String,
    pub role: String,
    pub model: String,
    pub tool_calls: Vec<ToolCall>,
    pub usage: Usage,
    pub stop_reason: Option<String>,
}

#[derive(Debug, Clone, Default)]
pub struct Usage {
    pub prompt_tokens: u32,
    pub completion_tokens: u32,
    pub model: String,
}

impl Usage {
    pub fn total_tokens(&self) -> u32 {
        self.prompt_tokens + self.completion_tokens
    }
}

#[derive(Debug, Clone)]
pub struct EmbedRequest {
    pub texts: Vec<String>,
    pub model: Option<String>,
}

#[derive(Debug, Clone)]
pub struct EmbedResponse {
    pub embeddings: Vec<Vec<f64>>,
    pub model: String,
    pub usage: Usage,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolSchema {
    pub name: String,
    pub description: String,
    pub parameters: serde_json::Value,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum LlmError {
    #[error("HTTP error: {0}")]
    Http(String),
    #[error("API error: {status} - {message}")]
    Api { status: u16, message: String },
    #[error("Parse error: {0}")]
    Parse(String),
    #[error("Config error: {0}")]
    Config(String),
    #[error("Rate limited: retry after {retry_after_ms}ms")]
    RateLimited { retry_after_ms: u64 },
}
