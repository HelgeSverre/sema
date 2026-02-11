use crate::provider::LlmProvider;
use crate::types::*;

pub struct GeminiProvider {
    api_key: String,
    base_url: String,
    default_model: String,
    client: reqwest::Client,
    runtime: tokio::runtime::Runtime,
}

impl GeminiProvider {
    pub fn new(api_key: String, default_model: Option<String>) -> Result<Self, LlmError> {
        let runtime = tokio::runtime::Runtime::new()
            .map_err(|e| LlmError::Config(format!("failed to create tokio runtime: {e}")))?;
        Ok(GeminiProvider {
            api_key,
            base_url: "https://generativelanguage.googleapis.com/v1beta".to_string(),
            default_model: default_model.unwrap_or_else(|| "gemini-2.0-flash".to_string()),
            client: reqwest::Client::new(),
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

    async fn complete_async(&self, request: ChatRequest) -> Result<ChatResponse, LlmError> {
        let model = self.resolve_model(&request.model);
        let url = format!(
            "{}/models/{}:generateContent?key={}",
            self.base_url, model, self.api_key
        );

        let body = self.build_request_body(&request);

        let resp = self
            .client
            .post(&url)
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

        let api_resp: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| LlmError::Parse(e.to_string()))?;

        self.parse_response(&api_resp, &model)
    }

    async fn stream_complete_async(
        &self,
        request: ChatRequest,
        on_chunk: &mut dyn FnMut(&str) -> Result<(), LlmError>,
    ) -> Result<ChatResponse, LlmError> {
        let model = self.resolve_model(&request.model);
        let url = format!(
            "{}/models/{}:streamGenerateContent?key={}&alt=sse",
            self.base_url, model, self.api_key
        );

        let body = self.build_request_body(&request);

        let resp = self
            .client
            .post(&url)
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

        crate::sse::parse_sse_stream(resp, |data| {
            if let Ok(chunk) = serde_json::from_str::<serde_json::Value>(data) {
                // Extract text from candidates
                if let Some(candidates) = chunk.get("candidates").and_then(|c| c.as_array()) {
                    for candidate in candidates {
                        if let Some(parts) = candidate
                            .pointer("/content/parts")
                            .and_then(|p| p.as_array())
                        {
                            for part in parts {
                                if let Some(text) = part.get("text").and_then(|t| t.as_str()) {
                                    full_content.push_str(text);
                                    on_chunk(text)?;
                                }
                            }
                        }
                    }
                }
                // Extract usage metadata
                if let Some(usage) = chunk.get("usageMetadata") {
                    if let Some(pt) = usage.get("promptTokenCount").and_then(|v| v.as_u64()) {
                        prompt_tokens = pt as u32;
                    }
                    if let Some(ct) = usage.get("candidatesTokenCount").and_then(|v| v.as_u64()) {
                        completion_tokens = ct as u32;
                    }
                }
            }
            Ok(())
        })
        .await?;

        Ok(ChatResponse {
            content: full_content,
            role: "assistant".to_string(),
            model: model.clone(),
            tool_calls: Vec::new(),
            usage: Usage {
                prompt_tokens,
                completion_tokens,
                model,
            },
            stop_reason: Some("stop".to_string()),
        })
    }

    fn build_request_body(&self, request: &ChatRequest) -> serde_json::Value {
        // Convert messages to Gemini format
        let mut contents = Vec::new();
        for msg in &request.messages {
            if msg.role == "system" {
                continue; // handled separately
            }
            let role = match msg.role.as_str() {
                "assistant" => "model",
                other => other,
            };
            contents.push(serde_json::json!({
                "role": role,
                "parts": [{"text": msg.content}]
            }));
        }

        let mut body = serde_json::json!({
            "contents": contents,
        });

        // System instruction
        let system = request.system.clone().or_else(|| {
            request
                .messages
                .iter()
                .find(|m| m.role == "system")
                .map(|m| m.content.clone())
        });
        if let Some(sys) = system {
            body["systemInstruction"] = serde_json::json!({
                "parts": [{"text": sys}]
            });
        }

        // Generation config
        let mut gen_config = serde_json::Map::new();
        if let Some(max_tokens) = request.max_tokens {
            gen_config.insert("maxOutputTokens".to_string(), serde_json::json!(max_tokens));
        }
        if let Some(temp) = request.temperature {
            gen_config.insert("temperature".to_string(), serde_json::json!(temp));
        }
        if !request.stop_sequences.is_empty() {
            gen_config.insert(
                "stopSequences".to_string(),
                serde_json::json!(request.stop_sequences),
            );
        }
        if !gen_config.is_empty() {
            body["generationConfig"] = serde_json::Value::Object(gen_config);
        }

        body
    }

    fn parse_response(
        &self,
        resp: &serde_json::Value,
        model: &str,
    ) -> Result<ChatResponse, LlmError> {
        let mut content = String::new();

        if let Some(candidates) = resp.get("candidates").and_then(|c| c.as_array()) {
            if let Some(candidate) = candidates.first() {
                if let Some(parts) = candidate
                    .pointer("/content/parts")
                    .and_then(|p| p.as_array())
                {
                    for part in parts {
                        if let Some(text) = part.get("text").and_then(|t| t.as_str()) {
                            if !content.is_empty() {
                                content.push('\n');
                            }
                            content.push_str(text);
                        }
                    }
                }
            }
        }

        let mut prompt_tokens = 0u32;
        let mut completion_tokens = 0u32;
        if let Some(usage) = resp.get("usageMetadata") {
            prompt_tokens = usage
                .get("promptTokenCount")
                .and_then(|v| v.as_u64())
                .unwrap_or(0) as u32;
            completion_tokens = usage
                .get("candidatesTokenCount")
                .and_then(|v| v.as_u64())
                .unwrap_or(0) as u32;
        }

        Ok(ChatResponse {
            content,
            role: "assistant".to_string(),
            model: model.to_string(),
            tool_calls: Vec::new(),
            usage: Usage {
                prompt_tokens,
                completion_tokens,
                model: model.to_string(),
            },
            stop_reason: Some("stop".to_string()),
        })
    }
}

impl LlmProvider for GeminiProvider {
    fn name(&self) -> &str {
        "gemini"
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
}
