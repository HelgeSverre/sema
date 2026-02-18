use std::time::Duration;

use crate::types::LlmError;

/// Default HTTP request timeout for LLM providers (2 minutes).
pub const DEFAULT_TIMEOUT: Duration = Duration::from_secs(120);

/// Create a new Tokio runtime for synchronous LLM calls.
pub fn create_runtime() -> Result<tokio::runtime::Runtime, LlmError> {
    tokio::runtime::Runtime::new()
        .map_err(|e| LlmError::Config(format!("failed to create tokio runtime: {e}")))
}

/// Create a new HTTP client with the given optional timeout.
/// Falls back to [`DEFAULT_TIMEOUT`] if `None`.
pub fn create_client(timeout: Option<Duration>) -> Result<reqwest::Client, LlmError> {
    let mut builder = reqwest::Client::builder();
    if let Some(t) = timeout.or(Some(DEFAULT_TIMEOUT)) {
        builder = builder.timeout(t);
    }
    builder
        .build()
        .map_err(|e| LlmError::Config(format!("failed to create http client: {e}")))
}
