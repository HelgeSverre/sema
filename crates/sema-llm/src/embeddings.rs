use crate::provider::LlmProvider;
use crate::types::{ChatRequest, ChatResponse, EmbedRequest, EmbedResponse, LlmError, Usage};

/// An embedding-only provider that uses OpenAI-compatible embed API.
/// Works for Jina, Voyage, and other providers with the same format.
pub struct OpenAiCompatEmbeddingProvider {
    name: String,
    api_key: String,
    base_url: String,
    default_model: String,
    client: reqwest::Client,
    runtime: tokio::runtime::Runtime,
}

impl OpenAiCompatEmbeddingProvider {
    pub fn new(
        name: String,
        api_key: String,
        base_url: String,
        default_model: String,
    ) -> Result<Self, LlmError> {
        let runtime = crate::http::create_runtime()?;
        Ok(Self {
            name,
            api_key,
            base_url,
            default_model,
            client: crate::http::create_client(None)?,
            runtime,
        })
    }

    async fn embed_async(&self, request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        let model = request.model.unwrap_or_else(|| self.default_model.clone());
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
                prompt_tokens: u
                    .get("prompt_tokens")
                    .or(u.get("total_tokens"))
                    .and_then(|v| v.as_u64())
                    .unwrap_or(0) as u32,
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

impl LlmProvider for OpenAiCompatEmbeddingProvider {
    fn name(&self) -> &str {
        &self.name
    }

    fn default_model(&self) -> &str {
        &self.default_model
    }

    fn complete(&self, _request: ChatRequest) -> Result<ChatResponse, LlmError> {
        Err(LlmError::Config(format!(
            "{} does not support chat completions (embedding-only provider)",
            self.name
        )))
    }

    fn embed(&self, request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        self.runtime.block_on(self.embed_async(request))
    }
}

/// Cohere embedding provider — unique API format.
pub struct CohereEmbeddingProvider {
    api_key: String,
    default_model: String,
    client: reqwest::Client,
    runtime: tokio::runtime::Runtime,
}

impl CohereEmbeddingProvider {
    pub fn new(api_key: String, default_model: Option<String>) -> Result<Self, LlmError> {
        let runtime = crate::http::create_runtime()?;
        Ok(Self {
            api_key,
            default_model: default_model.unwrap_or_else(|| "embed-english-v3.0".to_string()),
            client: crate::http::create_client(None)?,
            runtime,
        })
    }

    async fn embed_async(&self, request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        let model = request.model.unwrap_or_else(|| self.default_model.clone());
        let body = serde_json::json!({
            "model": model,
            "texts": request.texts,
            "input_type": "search_document",
            "embedding_types": ["float"],
        });

        let resp = self
            .client
            .post("https://api.cohere.com/v2/embed")
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

        // Cohere nests under embeddings.float
        let embeddings = api_resp
            .pointer("/embeddings/float")
            .and_then(|e| e.as_array())
            .ok_or_else(|| {
                LlmError::Parse("missing embeddings.float in Cohere response".to_string())
            })?
            .iter()
            .filter_map(|item| {
                item.as_array()
                    .map(|arr| arr.iter().filter_map(|v| v.as_f64()).collect::<Vec<f64>>())
            })
            .collect::<Vec<Vec<f64>>>();

        Ok(EmbedResponse {
            embeddings,
            model: model.clone(),
            usage: Usage {
                prompt_tokens: 0,
                completion_tokens: 0,
                model,
            },
        })
    }
}

impl LlmProvider for CohereEmbeddingProvider {
    fn name(&self) -> &str {
        "cohere"
    }

    fn default_model(&self) -> &str {
        &self.default_model
    }

    fn complete(&self, _request: ChatRequest) -> Result<ChatResponse, LlmError> {
        Err(LlmError::Config(
            "cohere does not support chat completions (embedding-only provider)".to_string(),
        ))
    }

    fn embed(&self, request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        self.runtime.block_on(self.embed_async(request))
    }
}
