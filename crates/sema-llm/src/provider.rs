use crate::types::{ChatRequest, ChatResponse, LlmError};

/// The core LLM provider trait. Sync interface — async internals are hidden.
pub trait LlmProvider: Send + Sync {
    fn name(&self) -> &str;
    fn complete(&self, request: ChatRequest) -> Result<ChatResponse, LlmError>;
    fn default_model(&self) -> &str;
}

/// Registry of providers by name.
pub struct ProviderRegistry {
    providers: std::collections::HashMap<String, Box<dyn LlmProvider>>,
    default: Option<String>,
}

impl ProviderRegistry {
    pub fn new() -> Self {
        ProviderRegistry {
            providers: std::collections::HashMap::new(),
            default: None,
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
}

impl Default for ProviderRegistry {
    fn default() -> Self {
        Self::new()
    }
}
