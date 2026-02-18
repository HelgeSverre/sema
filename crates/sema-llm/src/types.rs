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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn message_content_text_as_text() {
        let mc = MessageContent::text("hello");
        assert_eq!(mc.as_text(), Some("hello"));
    }

    #[test]
    fn message_content_single_block_as_text() {
        let mc = MessageContent::Blocks(vec![ContentBlock::Text {
            text: "hi".into(),
        }]);
        assert_eq!(mc.as_text(), Some("hi"));
    }

    #[test]
    fn message_content_multi_block_as_text_is_none() {
        let mc = MessageContent::Blocks(vec![
            ContentBlock::Text { text: "a".into() },
            ContentBlock::Text { text: "b".into() },
        ]);
        assert_eq!(mc.as_text(), None);
    }

    #[test]
    fn message_content_to_text_concatenates() {
        let mc = MessageContent::Blocks(vec![
            ContentBlock::Text {
                text: "hello ".into(),
            },
            ContentBlock::Image {
                media_type: None,
                data: "base64".into(),
            },
            ContentBlock::Text {
                text: "world".into(),
            },
        ]);
        assert_eq!(mc.to_text(), "hello world");
    }

    #[test]
    fn message_content_has_images() {
        assert!(!MessageContent::text("hi").has_images());
        let with_img = MessageContent::Blocks(vec![ContentBlock::Image {
            media_type: Some("image/png".into()),
            data: "abc".into(),
        }]);
        assert!(with_img.has_images());
    }

    #[test]
    fn chat_message_new() {
        let msg = ChatMessage::new("user", "hi");
        assert_eq!(msg.role, "user");
        assert_eq!(msg.content.as_text(), Some("hi"));
    }

    #[test]
    fn chat_request_defaults() {
        let req = ChatRequest::new("gpt-4".into(), vec![]);
        assert_eq!(req.max_tokens, Some(4096));
        assert!(!req.json_mode);
        assert!(req.tools.is_empty());
        assert!(req.system.is_none());
    }

    #[test]
    fn usage_total_tokens() {
        let u = Usage {
            prompt_tokens: 10,
            completion_tokens: 20,
            model: "test".into(),
        };
        assert_eq!(u.total_tokens(), 30);
    }

    #[test]
    fn llm_error_display() {
        let e = LlmError::Http("timeout".into());
        assert!(e.to_string().contains("timeout"));

        let e = LlmError::Api {
            status: 429,
            message: "rate limited".into(),
        };
        let s = e.to_string();
        assert!(s.contains("429") && s.contains("rate limited"));

        let e = LlmError::RateLimited {
            retry_after_ms: 5000,
        };
        assert!(e.to_string().contains("5000"));
    }
}
