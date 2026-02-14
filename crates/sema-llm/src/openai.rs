use serde::{Deserialize, Serialize};

use crate::provider::LlmProvider;
use crate::types::{ChatRequest, ChatResponse, EmbedRequest, EmbedResponse, LlmError, ToolCall, Usage};

pub struct OpenAiProvider {
    name: String,
    api_key: String,
    base_url: String,
    default_model: String,
    send_stream_options: bool,
    client: reqwest::Client,
    runtime: tokio::runtime::Runtime,
}

impl OpenAiProvider {
    pub fn new(
        api_key: String,
        base_url: Option<String>,
        default_model: Option<String>,
    ) -> Result<Self, LlmError> {
        Self::named(
            "openai".to_string(),
            api_key,
            base_url.unwrap_or_else(|| "https://api.openai.com/v1".to_string()),
            default_model.unwrap_or_else(|| "gpt-4o".to_string()),
            true,
        )
    }

    pub fn named(
        name: String,
        api_key: String,
        base_url: String,
        default_model: String,
        send_stream_options: bool,
    ) -> Result<Self, LlmError> {
        let runtime = tokio::runtime::Runtime::new()
            .map_err(|e| LlmError::Config(format!("failed to create tokio runtime: {e}")))?;
        Ok(OpenAiProvider {
            name,
            api_key,
            base_url,
            default_model,
            send_stream_options,
            client: reqwest::Client::builder()
                .timeout(std::time::Duration::from_secs(120))
                .build()
                .map_err(|e| LlmError::Config(format!("failed to create http client: {e}")))?,
            runtime,
        })
    }

    fn resolve_model(&self, model: &str) -> String {
        if model.is_empty() {
            self.default_model.clone()
        } else {
            model.to_string()
        }
    }

    fn build_request_body(&self, request: &ChatRequest) -> OpenAiRequest {
        let model = self.resolve_model(&request.model);
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

        OpenAiRequest {
            model,
            messages,
            max_tokens: request.max_tokens,
            temperature: request.temperature,
            tools,
            stop: request.stop_sequences.clone(),
            stream: None,
            stream_options: None,
        }
    }

    async fn complete_async(&self, request: ChatRequest) -> Result<ChatResponse, LlmError> {
        let body = self.build_request_body(&request);

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

        let usage = api_resp
            .usage
            .map(|u| Usage {
                prompt_tokens: u.prompt_tokens,
                completion_tokens: u.completion_tokens,
                model: api_resp.model.clone(),
            })
            .unwrap_or_default();

        Ok(ChatResponse {
            content,
            role: "assistant".to_string(),
            model: api_resp.model,
            tool_calls,
            usage,
            stop_reason: choice.finish_reason.clone(),
        })
    }

    async fn stream_complete_async(
        &self,
        request: ChatRequest,
        on_chunk: &mut dyn FnMut(&str) -> Result<(), LlmError>,
    ) -> Result<ChatResponse, LlmError> {
        let mut body = self.build_request_body(&request);
        body.stream = Some(true);
        if self.send_stream_options {
            body.stream_options = Some(StreamOptions {
                include_usage: true,
            });
        }
        let model_name = body.model.clone();

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

        let mut full_content = String::new();
        let mut prompt_tokens = 0u32;
        let mut completion_tokens = 0u32;
        let mut finish_reason = None;

        crate::sse::parse_sse_stream(resp, |data| {
            if let Ok(chunk) = serde_json::from_str::<serde_json::Value>(data) {
                // Extract usage from final chunk
                if let Some(usage) = chunk.get("usage") {
                    if !usage.is_null() {
                        prompt_tokens = usage
                            .get("prompt_tokens")
                            .and_then(|v| v.as_u64())
                            .unwrap_or(0) as u32;
                        completion_tokens = usage
                            .get("completion_tokens")
                            .and_then(|v| v.as_u64())
                            .unwrap_or(0) as u32;
                    }
                }
                // Extract content delta
                if let Some(choices) = chunk.get("choices").and_then(|c| c.as_array()) {
                    for choice in choices {
                        if let Some(delta) = choice.get("delta") {
                            if let Some(content) = delta.get("content").and_then(|c| c.as_str()) {
                                full_content.push_str(content);
                                on_chunk(content)?;
                            }
                        }
                        if let Some(fr) = choice.get("finish_reason") {
                            if !fr.is_null() {
                                finish_reason = fr.as_str().map(|s| s.to_string());
                            }
                        }
                    }
                }
            }
            Ok(())
        })
        .await?;

        Ok(ChatResponse {
            content: full_content,
            role: "assistant".to_string(),
            model: model_name.clone(),
            tool_calls: Vec::new(),
            usage: Usage {
                prompt_tokens,
                completion_tokens,
                model: model_name,
            },
            stop_reason: finish_reason,
        })
    }

    async fn embed_async(&self, request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        let model = request
            .model
            .unwrap_or_else(|| "text-embedding-3-small".to_string());

        let body = serde_json::json!({
            "input": request.texts,
            "model": model,
        });

        let resp = self
            .client
            .post(format!("{}/embeddings", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/json")
            .json(&body)
            .send()
            .await
            .map_err(|e| LlmError::Http(e.to_string()))?;

        let status = resp.status().as_u16();
        if status != 200 {
            let text = resp.text().await.unwrap_or_default();
            return Err(LlmError::Api {
                status,
                message: text,
            });
        }

        let api_resp: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| LlmError::Parse(e.to_string()))?;

        let resp_model = api_resp
            .get("model")
            .and_then(|m| m.as_str())
            .unwrap_or(&model)
            .to_string();

        let embeddings = api_resp
            .get("data")
            .and_then(|d| d.as_array())
            .ok_or_else(|| LlmError::Parse("missing data in embedding response".to_string()))?
            .iter()
            .filter_map(|item| {
                item.get("embedding")
                    .and_then(|e| e.as_array())
                    .map(|arr| arr.iter().filter_map(|v| v.as_f64()).collect::<Vec<f64>>())
            })
            .collect::<Vec<Vec<f64>>>();

        let usage = api_resp
            .get("usage")
            .map(|u| Usage {
                prompt_tokens: u.get("prompt_tokens").and_then(|v| v.as_u64()).unwrap_or(0) as u32,
                completion_tokens: 0,
                model: resp_model.clone(),
            })
            .unwrap_or_default();

        Ok(EmbedResponse {
            embeddings,
            model: resp_model,
            usage,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    stream: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    stream_options: Option<StreamOptions>,
}

#[derive(Serialize)]
struct StreamOptions {
    include_usage: bool,
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
        &self.name
    }

    fn default_model(&self) -> &str {
        &self.default_model
    }

    fn complete(&self, request: ChatRequest) -> Result<ChatResponse, LlmError> {
        self.runtime.block_on(self.complete_async(request))
    }

    fn stream_complete(
        &self,
        request: ChatRequest,
        on_chunk: &mut dyn FnMut(&str) -> Result<(), LlmError>,
    ) -> Result<ChatResponse, LlmError> {
        self.runtime
            .block_on(self.stream_complete_async(request, on_chunk))
    }

    fn batch_complete(&self, requests: Vec<ChatRequest>) -> Vec<Result<ChatResponse, LlmError>> {
        self.runtime.block_on(async {
            let futures: Vec<_> = requests
                .into_iter()
                .map(|req| self.complete_async(req))
                .collect();
            futures::future::join_all(futures).await
        })
    }

    fn embed(&self, request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        self.runtime.block_on(self.embed_async(request))
    }
}
