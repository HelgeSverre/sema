use std::time::Duration;

use crate::types::LlmError;

/// Default HTTP request timeout for LLM providers (2 minutes).
pub const DEFAULT_TIMEOUT: Duration = Duration::from_secs(120);

/// A Tokio runtime whose `Drop` never blocks.
///
/// A plain [`tokio::runtime::Runtime`] panics when dropped from within another
/// async context ("Cannot drop a runtime in a context where blocking is not
/// allowed"). LLM providers each own a runtime and are stored in the
/// interpreter; when a short-lived interpreter is dropped inside an async
/// context — e.g. the MCP server's stdio loop — that panic aborts the whole
/// process. Shutting the runtime down in the background sidesteps the blocking
/// teardown, so dropping a provider is always safe regardless of context.
pub struct BlockingRuntime(Option<tokio::runtime::Runtime>);

impl BlockingRuntime {
    fn new(runtime: tokio::runtime::Runtime) -> Self {
        Self(Some(runtime))
    }
}

impl std::ops::Deref for BlockingRuntime {
    type Target = tokio::runtime::Runtime;

    fn deref(&self) -> &Self::Target {
        // The inner runtime is `Some` for the entire lifetime of the value; it
        // is only taken in `Drop`, after which `deref` is never called.
        self.0.as_ref().expect("runtime present until drop")
    }
}

impl Drop for BlockingRuntime {
    fn drop(&mut self) {
        if let Some(runtime) = self.0.take() {
            runtime.shutdown_background();
        }
    }
}

/// Create a new Tokio runtime for synchronous LLM calls.
pub fn create_runtime() -> Result<BlockingRuntime, LlmError> {
    let runtime = tokio::runtime::Runtime::new()
        .map_err(|e| LlmError::Config(format!("failed to create tokio runtime: {e}")))?;
    Ok(BlockingRuntime::new(runtime))
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
