use serde::{Deserialize, Serialize};

use crate::provider::LlmProvider;
use crate::types::*;

pub struct OpenAiProvider {
    api_key: String,
    base_url: String,
    default_model: String,
    client: reqwest::Client,
    runtime: tokio::runtime::Runtime,
}

impl OpenAiProvider {
    pub fn new(
        api_key: String,
        base_url: Option<String>,
        default_model: Option<String>,
    ) -> Result<Self, LlmError> {
        let runtime = tokio::runtime::Runtime::new()
            .map_err(|e| LlmError::Config(format!("failed to create tokio runtime: {e}")))?;
        Ok(OpenAiProvider {
            api_key,
            base_url: base_url.unwrap_or_else(|| "https://api.openai.com/v1".to_string()),
            default_model: default_model.unwrap_or_else(|| "gpt-4o".to_string()),
            client: reqwest::Client::new(),
            runtime,
        })
    }
}

#[derive(Serialize)]
struct OpenAiRequest {
    model: String,
    messages: Vec<OpenAiMessage>,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_tokens: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f64>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    tools: Vec<OpenAiTool>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    stop: Vec<String>,
}

#[derive(Serialize, Deserialize)]
struct OpenAiMessage {
    role: String,
    content: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    tool_calls: Option<Vec<OpenAiToolCall>>,
}

#[derive(Serialize, Deserialize)]
struct OpenAiTool {
    #[serde(rename = "type")]
    tool_type: String,
    function: OpenAiFunction,
}

#[derive(Serialize, Deserialize)]
struct OpenAiFunction {
    name: String,
    description: String,
    parameters: serde_json::Value,
}

#[derive(Serialize, Deserialize)]
struct OpenAiToolCall {
    id: String,
    #[serde(rename = "type")]
    call_type: String,
    function: OpenAiFunctionCall,
}

#[derive(Serialize, Deserialize)]
struct OpenAiFunctionCall {
    name: String,
    arguments: String,
}

#[derive(Deserialize)]
struct OpenAiResponse {
    choices: Vec<OpenAiChoice>,
    model: String,
    usage: Option<OpenAiUsage>,
}

#[derive(Deserialize)]
struct OpenAiChoice {
    message: OpenAiMessage,
    finish_reason: Option<String>,
}

#[derive(Deserialize)]
struct OpenAiUsage {
    prompt_tokens: u32,
    completion_tokens: u32,
}

impl LlmProvider for OpenAiProvider {
    fn name(&self) -> &str {
        "openai"
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

            let messages: Vec<OpenAiMessage> = request
                .messages
                .iter()
                .map(|m| OpenAiMessage {
                    role: m.role.clone(),
                    content: Some(m.content.clone()),
                    tool_calls: None,
                })
                .collect();

            let tools: Vec<OpenAiTool> = request
                .tools
                .iter()
                .map(|t| OpenAiTool {
                    tool_type: "function".to_string(),
                    function: OpenAiFunction {
                        name: t.name.clone(),
                        description: t.description.clone(),
                        parameters: t.parameters.clone(),
                    },
                })
                .collect();

            let body = OpenAiRequest {
                model,
                messages,
                max_tokens: request.max_tokens,
                temperature: request.temperature,
                tools,
                stop: request.stop_sequences.clone(),
            };

            let resp = self
                .client
                .post(format!("{}/chat/completions", self.base_url))
                .header("Authorization", format!("Bearer {}", self.api_key))
                .header("Content-Type", "application/json")
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
                return Err(LlmError::Api {
                    status,
                    message: text,
                });
            }

            let api_resp: OpenAiResponse = resp
                .json()
                .await
                .map_err(|e| LlmError::Parse(e.to_string()))?;

            let choice = api_resp
                .choices
                .first()
                .ok_or_else(|| LlmError::Parse("no choices in response".to_string()))?;

            let content = choice.message.content.clone().unwrap_or_default();
            let tool_calls = choice
                .message
                .tool_calls
                .as_ref()
                .map(|tcs| {
                    tcs.iter()
                        .map(|tc| ToolCall {
                            id: tc.id.clone(),
                            name: tc.function.name.clone(),
                            arguments: serde_json::from_str(&tc.function.arguments)
                                .unwrap_or(serde_json::Value::Object(serde_json::Map::new())),
                        })
                        .collect()
                })
                .unwrap_or_default();

            let usage = api_resp.usage.map(|u| Usage {
                prompt_tokens: u.prompt_tokens,
                completion_tokens: u.completion_tokens,
            }).unwrap_or_default();

            Ok(ChatResponse {
                content,
                role: "assistant".to_string(),
                model: api_resp.model,
                tool_calls,
                usage,
                stop_reason: choice.finish_reason.clone(),
            })
        })
    }
}
