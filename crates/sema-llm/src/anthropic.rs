use serde::{Deserialize, Serialize};

use crate::provider::LlmProvider;
use crate::types::*;

pub struct AnthropicProvider {
    api_key: String,
    default_model: String,
    client: reqwest::Client,
    runtime: tokio::runtime::Runtime,
}

impl AnthropicProvider {
    pub fn new(api_key: String, default_model: Option<String>) -> Result<Self, LlmError> {
        let runtime = tokio::runtime::Runtime::new()
            .map_err(|e| LlmError::Config(format!("failed to create tokio runtime: {e}")))?;
        Ok(AnthropicProvider {
            api_key,
            default_model: default_model
                .unwrap_or_else(|| "claude-sonnet-4-5-20250929".to_string()),
            client: reqwest::Client::new(),
            runtime,
        })
    }
}

#[derive(Serialize)]
struct AnthropicRequest {
    model: String,
    messages: Vec<AnthropicMessage>,
    max_tokens: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    system: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tools: Vec<AnthropicTool>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    stop_sequences: Vec<String>,
}

#[derive(Serialize, Deserialize)]
struct AnthropicMessage {
    role: String,
    content: serde_json::Value,
}

#[derive(Serialize)]
struct AnthropicTool {
    name: String,
    description: String,
    input_schema: serde_json::Value,
}

#[derive(Deserialize)]
struct AnthropicResponse {
    content: Vec<ContentBlock>,
    model: String,
    role: String,
    usage: AnthropicUsage,
    stop_reason: Option<String>,
}

#[derive(Deserialize)]
#[serde(tag = "type")]
enum ContentBlock {
    #[serde(rename = "text")]
    Text { text: String },
    #[serde(rename = "tool_use")]
    ToolUse {
        id: String,
        name: String,
        input: serde_json::Value,
    },
}

#[derive(Deserialize)]
struct AnthropicUsage {
    input_tokens: u32,
    output_tokens: u32,
}

#[derive(Deserialize)]
struct AnthropicError {
    error: AnthropicErrorDetail,
}

#[derive(Deserialize)]
struct AnthropicErrorDetail {
    message: String,
    #[serde(rename = "type")]
    _error_type: Option<String>,
}

impl LlmProvider for AnthropicProvider {
    fn name(&self) -> &str {
        "anthropic"
    }

    fn default_model(&self) -> &str {
        &self.default_model
    }

    fn complete(&self, request: ChatRequest) -> Result<ChatResponse, LlmError> {
        self.runtime.block_on(async {
            let model = if request.model.is_empty() {
                self.default_model.clone()
            } else {
                request.model.clone()
            };

            let messages: Vec<AnthropicMessage> = request
                .messages
                .iter()
                .filter(|m| m.role != "system")
                .map(|m| AnthropicMessage {
                    role: m.role.clone(),
                    content: serde_json::Value::String(m.content.clone()),
                })
                .collect();

            // Extract system message
            let system = request.system.or_else(|| {
                request
                    .messages
                    .iter()
                    .find(|m| m.role == "system")
                    .map(|m| m.content.clone())
            });

            let tools: Vec<AnthropicTool> = request
                .tools
                .iter()
                .map(|t| AnthropicTool {
                    name: t.name.clone(),
                    description: t.description.clone(),
                    input_schema: t.parameters.clone(),
                })
                .collect();

            let body = AnthropicRequest {
                model,
                messages,
                max_tokens: request.max_tokens.unwrap_or(4096),
                temperature: request.temperature,
                system,
                tools,
                stop_sequences: request.stop_sequences.clone(),
            };

            let resp = self
                .client
                .post("https://api.anthropic.com/v1/messages")
                .header("x-api-key", &self.api_key)
                .header("anthropic-version", "2023-06-01")
                .header("content-type", "application/json")
                .json(&body)
                .send()
                .await
                .map_err(|e| LlmError::Http(e.to_string()))?;

            let status = resp.status().as_u16();
            if status == 429 {
                return Err(LlmError::RateLimited {
                    retry_after_ms: 5000,
                });
            }
            if status != 200 {
                let text = resp.text().await.unwrap_or_default();
                if let Ok(err) = serde_json::from_str::<AnthropicError>(&text) {
                    return Err(LlmError::Api {
                        status,
                        message: err.error.message,
                    });
                }
                return Err(LlmError::Api {
                    status,
                    message: text,
                });
            }

            let api_resp: AnthropicResponse = resp
                .json()
                .await
                .map_err(|e| LlmError::Parse(e.to_string()))?;

            let mut content = String::new();
            let mut tool_calls = Vec::new();
            for block in &api_resp.content {
                match block {
                    ContentBlock::Text { text } => {
                        if !content.is_empty() {
                            content.push('\n');
                        }
                        content.push_str(text);
                    }
                    ContentBlock::ToolUse { id, name, input } => {
                        tool_calls.push(ToolCall {
                            id: id.clone(),
                            name: name.clone(),
                            arguments: input.clone(),
                        });
                    }
                }
            }

            Ok(ChatResponse {
                content,
                role: api_resp.role,
                model: api_resp.model,
                tool_calls,
                usage: Usage {
                    prompt_tokens: api_resp.usage.input_tokens,
                    completion_tokens: api_resp.usage.output_tokens,
                },
                stop_reason: api_resp.stop_reason,
            })
        })
    }
}
