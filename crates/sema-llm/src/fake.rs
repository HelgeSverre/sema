//! A scripted, in-process [`LlmProvider`] for deterministic, key-free testing of
//! the LLM and agentic paths (completion, chat, streaming, batch, embeddings, and
//! the agent tool loop).
//!
//! This is test infrastructure — it is `pub` so integration tests in other crates
//! (`crates/sema/tests`) can use it, but it performs no network I/O and returns
//! only what it was scripted to return. Register one as the default provider with
//! [`crate::builtins::register_test_provider`].
//!
//! It records every request it receives into a shared [`FakeRecorder`], so tests
//! can assert on the exact messages the runtime built — including the round-2
//! tool-result messages the agent loop sends back, which is how the OpenAI
//! tool-protocol breakage is caught.

use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

use crate::provider::LlmProvider;
use crate::types::{
    ChatRequest, ChatResponse, EmbedRequest, EmbedResponse, LlmError, ToolCall, Usage,
};

/// One scripted interaction. The fake pops these in order as calls arrive.
pub enum FakeReply {
    /// A plain chat/completion response.
    Chat(ChatResponse),
    /// A streamed response: emit `chunks` to `on_chunk`, then return `response`.
    Stream {
        chunks: Vec<String>,
        response: ChatResponse,
    },
    /// An embedding response.
    Embed(EmbedResponse),
    /// Inject an error (for resilience tests).
    Error(LlmError),
}

/// Shared record of everything the fake provider was asked to do. The test holds
/// an `Arc` to this (via [`FakeProvider::recorder`]) so it can inspect requests
/// after the run, even though the provider itself is moved into the registry.
#[derive(Default)]
pub struct FakeRecorder {
    requests: Mutex<Vec<ChatRequest>>,
    embeds: Mutex<Vec<EmbedRequest>>,
}

impl FakeRecorder {
    /// All chat/completion requests received, in order.
    pub fn requests(&self) -> Vec<ChatRequest> {
        self.requests.lock().unwrap().clone()
    }

    /// All embedding requests received, in order.
    pub fn embeds(&self) -> Vec<EmbedRequest> {
        self.embeds.lock().unwrap().clone()
    }

    /// Number of chat/completion calls made.
    pub fn call_count(&self) -> usize {
        self.requests.lock().unwrap().len()
    }
}

/// A scripted [`LlmProvider`]. Build with [`FakeProvider::builder`].
pub struct FakeProvider {
    name: String,
    default_model: String,
    script: Mutex<VecDeque<FakeReply>>,
    recorder: Arc<FakeRecorder>,
}

impl FakeProvider {
    /// Start building a fake provider named `name` (the name the registry keys it
    /// under and that cost tracking attributes usage to).
    pub fn builder(name: &str) -> FakeProviderBuilder {
        FakeProviderBuilder {
            name: name.to_string(),
            default_model: "fake-model".to_string(),
            script: VecDeque::new(),
        }
    }

    /// Grab a handle to the shared recorder *before* moving the provider into the
    /// registry, so the test can inspect recorded requests afterwards.
    pub fn recorder(&self) -> Arc<FakeRecorder> {
        self.recorder.clone()
    }
}

/// Builder for [`FakeProvider`]. Each method appends one scripted reply.
pub struct FakeProviderBuilder {
    name: String,
    default_model: String,
    script: VecDeque<FakeReply>,
}

impl FakeProviderBuilder {
    /// Set the model name reported by the provider.
    pub fn model(mut self, model: &str) -> Self {
        self.default_model = model.to_string();
        self
    }

    /// Script a plain text reply (default small usage).
    pub fn reply(mut self, text: &str) -> Self {
        self.script
            .push_back(FakeReply::Chat(self.chat_text(text, None)));
        self
    }

    /// Script a text reply with explicit token usage (for cost-tracking tests).
    pub fn reply_with_usage(
        mut self,
        text: &str,
        prompt_tokens: u32,
        completion_tokens: u32,
    ) -> Self {
        self.script.push_back(FakeReply::Chat(
            self.chat_text(text, Some((prompt_tokens, completion_tokens))),
        ));
        self
    }

    /// Script an assistant turn that emits a single tool call (empty text content,
    /// `tool_use` stop reason) — mirrors how OpenAI/Anthropic return tool calls.
    pub fn tool_call(mut self, id: &str, name: &str, arguments: serde_json::Value) -> Self {
        let resp = ChatResponse {
            content: String::new(),
            role: "assistant".to_string(),
            model: self.default_model.clone(),
            tool_calls: vec![ToolCall {
                id: id.to_string(),
                name: name.to_string(),
                arguments,
            }],
            usage: Usage {
                prompt_tokens: 10,
                completion_tokens: 5,
                model: self.default_model.clone(),
            },
            stop_reason: Some("tool_use".to_string()),
        };
        self.script.push_back(FakeReply::Chat(resp));
        self
    }

    /// Script a streamed reply: `chunks` are delivered to `on_chunk`, then the
    /// concatenation is returned as the final response.
    pub fn stream(mut self, chunks: &[&str]) -> Self {
        let full: String = chunks.concat();
        let response = self.chat_text(&full, None);
        self.script.push_back(FakeReply::Stream {
            chunks: chunks.iter().map(|c| c.to_string()).collect(),
            response,
        });
        self
    }

    /// Script an embedding reply.
    pub fn embed(mut self, vectors: Vec<Vec<f64>>) -> Self {
        let model = self.default_model.clone();
        self.script.push_back(FakeReply::Embed(EmbedResponse {
            embeddings: vectors,
            model: model.clone(),
            usage: Usage {
                prompt_tokens: 1,
                completion_tokens: 0,
                model,
            },
        }));
        self
    }

    /// Script an injected error (e.g. `LlmError::RateLimited`, `LlmError::Api`).
    pub fn error(mut self, err: LlmError) -> Self {
        self.script.push_back(FakeReply::Error(err));
        self
    }

    /// Finish building.
    pub fn build(self) -> FakeProvider {
        FakeProvider {
            name: self.name,
            default_model: self.default_model,
            script: Mutex::new(self.script),
            recorder: Arc::new(FakeRecorder::default()),
        }
    }

    fn chat_text(&self, text: &str, usage: Option<(u32, u32)>) -> ChatResponse {
        let (p, c) = usage.unwrap_or((10, 5));
        ChatResponse {
            content: text.to_string(),
            role: "assistant".to_string(),
            model: self.default_model.clone(),
            tool_calls: Vec::new(),
            usage: Usage {
                prompt_tokens: p,
                completion_tokens: c,
                model: self.default_model.clone(),
            },
            stop_reason: Some("end_turn".to_string()),
        }
    }
}

impl FakeProvider {
    fn next(&self) -> Option<FakeReply> {
        self.script.lock().unwrap().pop_front()
    }
}

impl LlmProvider for FakeProvider {
    fn name(&self) -> &str {
        &self.name
    }

    fn default_model(&self) -> &str {
        &self.default_model
    }

    fn complete(&self, request: ChatRequest) -> Result<ChatResponse, LlmError> {
        self.recorder.requests.lock().unwrap().push(request);
        match self.next() {
            Some(FakeReply::Chat(r)) => Ok(r),
            Some(FakeReply::Stream { response, .. }) => Ok(response),
            Some(FakeReply::Error(e)) => Err(e),
            Some(FakeReply::Embed(_)) => Err(LlmError::Config(
                "FakeProvider: complete() reached an embed-scripted reply".to_string(),
            )),
            None => Err(LlmError::Config(
                "FakeProvider: no scripted reply left for complete()".to_string(),
            )),
        }
    }

    fn stream_complete(
        &self,
        request: ChatRequest,
        on_chunk: &mut dyn FnMut(&str) -> Result<(), LlmError>,
    ) -> Result<ChatResponse, LlmError> {
        self.recorder.requests.lock().unwrap().push(request);
        match self.next() {
            Some(FakeReply::Stream { chunks, response }) => {
                for c in &chunks {
                    on_chunk(c)?;
                }
                Ok(response)
            }
            Some(FakeReply::Chat(r)) => {
                on_chunk(&r.content)?;
                Ok(r)
            }
            Some(FakeReply::Error(e)) => Err(e),
            Some(FakeReply::Embed(_)) => Err(LlmError::Config(
                "FakeProvider: stream_complete() reached an embed-scripted reply".to_string(),
            )),
            None => Err(LlmError::Config(
                "FakeProvider: no scripted reply left for stream_complete()".to_string(),
            )),
        }
    }

    fn batch_complete(&self, requests: Vec<ChatRequest>) -> Vec<Result<ChatResponse, LlmError>> {
        requests.into_iter().map(|r| self.complete(r)).collect()
    }

    fn embed(&self, request: EmbedRequest) -> Result<EmbedResponse, LlmError> {
        self.recorder.embeds.lock().unwrap().push(request);
        match self.next() {
            Some(FakeReply::Embed(r)) => Ok(r),
            Some(FakeReply::Error(e)) => Err(e),
            _ => Err(LlmError::Config(
                "FakeProvider: no scripted embed reply left".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scripts_replies_in_order_and_records_requests() {
        let fake = FakeProvider::builder("fake")
            .tool_call("call_1", "get_weather", serde_json::json!({"city": "Oslo"}))
            .reply("It is sunny")
            .build();
        let recorder = fake.recorder();

        let r1 = fake
            .complete(ChatRequest::new("fake-model".into(), vec![]))
            .unwrap();
        assert_eq!(r1.tool_calls.len(), 1);
        assert_eq!(r1.tool_calls[0].name, "get_weather");

        let r2 = fake
            .complete(ChatRequest::new("fake-model".into(), vec![]))
            .unwrap();
        assert_eq!(r2.content, "It is sunny");
        assert!(r2.tool_calls.is_empty());

        assert_eq!(recorder.call_count(), 2);
    }

    #[test]
    fn injects_errors() {
        let fake = FakeProvider::builder("fake")
            .error(LlmError::RateLimited { retry_after_ms: 1 })
            .build();
        let err = fake
            .complete(ChatRequest::new("fake-model".into(), vec![]))
            .unwrap_err();
        assert!(matches!(err, LlmError::RateLimited { .. }));
    }
}
