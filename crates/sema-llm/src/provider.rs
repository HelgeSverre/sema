use crate::types::{ChatRequest, ChatResponse, EmbedRequest, EmbedResponse, LlmError};

/// The core LLM provider trait. Sync interface — async internals are hidden.
pub trait LlmProvider: Send + Sync {
    fn name(&self) -> &str;
    fn complete(&self, request: ChatRequest) -> Result<ChatResponse, LlmError>;
    fn default_model(&self) -> &str;

    /// Streaming — calls on_chunk for each text delta, returns full response at end.
    fn stream_complete(
        &self,
        request: ChatRequest,
        on_chunk: &mut dyn FnMut(&str) -> Result<(), LlmError>,
    ) -> Result<ChatResponse, LlmError> {
        let resp = self.complete(request)?;
        on_chunk(&resp.content)?;
        Ok(resp)
    }

    /// Batch — run multiple requests concurrently.
    fn batch_complete(&self, requests: Vec<ChatRequest>) -> Vec<Result<ChatResponse, LlmError>> {
        requests.into_iter().map(|r| self.complete(r)).collect()
    }

    /// Embeddings.
    fn embed(&self, _request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        Err(LlmError::Config(format!(
            "{} does not support embeddings",
            self.name()
        )))
    }
}

/// Registry of providers by name, plus a separate embedding provider slot.
pub struct ProviderRegistry {
    providers: std::collections::HashMap<String, Box<dyn LlmProvider>>,
    default: Option<String>,
    embedding_provider: Option<String>,
}

impl ProviderRegistry {
    pub fn new() -> Self {
        ProviderRegistry {
            providers: std::collections::HashMap::new(),
            default: None,
            embedding_provider: None,
        }
    }

    pub fn register(&mut self, provider: Box<dyn LlmProvider>) {
        let name = provider.name().to_string();
        if self.default.is_none() {
            self.default = Some(name.clone());
        }
        self.providers.insert(name, provider);
    }

    pub fn get(&self, name: &str) -> Option<&dyn LlmProvider> {
        self.providers.get(name).map(|p| p.as_ref())
    }

    pub fn default_provider(&self) -> Option<&dyn LlmProvider> {
        self.default
            .as_ref()
            .and_then(|name| self.providers.get(name))
            .map(|p| p.as_ref())
    }

    pub fn set_default(&mut self, name: &str) {
        if self.providers.contains_key(name) {
            self.default = Some(name.to_string());
        }
    }

    pub fn set_embedding_provider(&mut self, name: &str) {
        self.embedding_provider = Some(name.to_string());
    }

    pub fn embedding_provider(&self) -> Option<&dyn LlmProvider> {
        self.embedding_provider
            .as_ref()
            .and_then(|name| self.providers.get(name))
            .map(|p| p.as_ref())
    }
}

impl Default for ProviderRegistry {
    fn default() -> Self {
        Self::new()
    }
}
