use std::cell::Cell;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{
    resolve, Conversation, Env, EvalContext, Message, NativeFn, Prompt, Role, SemaError, Value,
};

use crate::anthropic::AnthropicProvider;
use crate::embeddings::{CohereEmbeddingProvider, OpenAiCompatEmbeddingProvider};
use crate::gemini::GeminiProvider;
use crate::ollama::OllamaProvider;
use crate::openai::OpenAiProvider;
use crate::pricing;
use crate::provider::{LlmProvider, ProviderRegistry};
use crate::types::{
    ChatMessage, ChatRequest, ChatResponse, EmbedRequest, LlmError, ToolCall, ToolSchema, Usage,
};

/// Type for a full evaluator callback: (ctx, expr, env) -> Result<Value, SemaError>
pub type EvalCallback = Box<dyn Fn(&EvalContext, &Value, &Env) -> Result<Value, SemaError>>;

thread_local! {
    static PROVIDER_REGISTRY: RefCell<ProviderRegistry> = RefCell::new(ProviderRegistry::new());
    static SESSION_USAGE: RefCell<Usage> = RefCell::new(Usage::default());
    static LAST_USAGE: RefCell<Option<Usage>> = const { RefCell::new(None) };
    static EVAL_FN: RefCell<Option<EvalCallback>> = RefCell::new(None);
    static SESSION_COST: RefCell<f64> = const { RefCell::new(0.0) };
    static BUDGET_LIMIT: RefCell<Option<f64>> = const { RefCell::new(None) };
    static BUDGET_SPENT: RefCell<f64> = const { RefCell::new(0.0) };
    static BUDGET_STACK: RefCell<Vec<(Option<f64>, f64)>> = const { RefCell::new(Vec::new()) };
}

thread_local! {
    static PRICING_WARNING_SHOWN: Cell<bool> = const { Cell::new(false) };
    static LISP_PROVIDERS: RefCell<std::collections::HashMap<String, LispProviderCallbacks>> = RefCell::new(std::collections::HashMap::new());
}

struct LispProviderCallbacks {
    complete_fn: Value,
}

/// Register a full evaluator for use by tool handlers and other LLM builtins.
pub fn set_eval_callback(
    f: impl Fn(&EvalContext, &Value, &Env) -> Result<Value, SemaError> + 'static,
) {
    EVAL_FN.with(|eval| {
        *eval.borrow_mut() = Some(Box::new(f));
    });
}

/// Reset LLM runtime state used by builtins.
/// Called by interpreter construction to avoid cross-instance leakage.
pub fn reset_runtime_state() {
    PROVIDER_REGISTRY.with(|r| *r.borrow_mut() = ProviderRegistry::new());
    SESSION_USAGE.with(|u| *u.borrow_mut() = Usage::default());
    LAST_USAGE.with(|u| *u.borrow_mut() = None);
    EVAL_FN.with(|e| *e.borrow_mut() = None);
    SESSION_COST.with(|c| *c.borrow_mut() = 0.0);
    BUDGET_LIMIT.with(|l| *l.borrow_mut() = None);
    BUDGET_SPENT.with(|s| *s.borrow_mut() = 0.0);
    BUDGET_STACK.with(|s| s.borrow_mut().clear());
    PRICING_WARNING_SHOWN.with(|shown| shown.set(false));
    LISP_PROVIDERS.with(|p| p.borrow_mut().clear());
    pricing::clear_fetched_pricing();
}

/// Evaluate an expression using the registered full evaluator.
fn full_eval(ctx: &EvalContext, expr: &Value, env: &Env) -> Result<Value, SemaError> {
    EVAL_FN.with(|eval_fn| {
        let eval_fn = eval_fn.borrow();
        match &*eval_fn {
            Some(f) => f(ctx, expr, env),
            None => simple_eval(ctx, expr, env),
        }
    })
}

fn register_fn(env: &Env, name: &str, f: impl Fn(&[Value]) -> Result<Value, SemaError> + 'static) {
    env.set(
        sema_core::intern(name),
        Value::NativeFn(Rc::new(NativeFn::simple(name, f))),
    );
}

fn register_fn_ctx(
    env: &Env,
    name: &str,
    f: impl Fn(&EvalContext, &[Value]) -> Result<Value, SemaError> + 'static,
) {
    env.set(
        sema_core::intern(name),
        Value::NativeFn(Rc::new(NativeFn::with_ctx(name, f))),
    );
}

fn with_provider<F, R>(f: F) -> Result<R, SemaError>
where
    F: FnOnce(&dyn LlmProvider) -> Result<R, SemaError>,
{
    PROVIDER_REGISTRY.with(|reg| {
        let reg = reg.borrow();
        let provider = reg.default_provider().ok_or_else(|| {
            SemaError::Llm(
                "no LLM provider configured. Use (llm/configure :anthropic {:api-key ...}) first"
                    .to_string(),
            )
        })?;
        f(provider)
    })
}

fn with_embedding_provider<F, R>(f: F) -> Result<R, SemaError>
where
    F: FnOnce(&dyn LlmProvider) -> Result<R, SemaError>,
{
    PROVIDER_REGISTRY.with(|reg| {
        let reg = reg.borrow();
        let provider = reg
            .embedding_provider()
            .or_else(|| reg.default_provider())
            .ok_or_else(|| {
                SemaError::Llm(
                    "no embedding provider configured. Use (llm/configure-embeddings ...) first"
                        .to_string(),
                )
            })?;
        f(provider)
    })
}

fn track_usage(usage: &Usage) -> Result<(), SemaError> {
    let cost = pricing::calculate_cost(usage);

    LAST_USAGE.with(|u| *u.borrow_mut() = Some(usage.clone()));
    SESSION_USAGE.with(|u| {
        let mut session = u.borrow_mut();
        session.prompt_tokens += usage.prompt_tokens;
        session.completion_tokens += usage.completion_tokens;
    });

    if let Some(c) = cost {
        SESSION_COST.with(|sc| *sc.borrow_mut() += c);

        // Check budget
        BUDGET_LIMIT.with(|limit| {
            let limit = limit.borrow();
            if let Some(max_cost) = *limit {
                BUDGET_SPENT.with(|spent| {
                    let mut spent = spent.borrow_mut();
                    *spent += c;
                    if *spent > max_cost {
                        return Err(SemaError::Llm(format!(
                            "budget exceeded: spent ${:.4} of ${:.4} limit",
                            *spent, max_cost
                        )));
                    }
                    Ok(())
                })
            } else {
                Ok(())
            }
        })?;
    } else {
        // Cost unknown — warn once if budget is active
        BUDGET_LIMIT.with(|limit| {
            if limit.borrow().is_some() {
                PRICING_WARNING_SHOWN.with(|shown| {
                    if !shown.get() {
                        shown.set(true);
                        eprintln!(
                            "Warning: pricing unknown for model '{}'; budget enforcement is best-effort",
                            usage.model
                        );
                    }
                });
            }
        });
    }

    Ok(())
}

/// Set a budget limit for LLM calls.
pub fn set_budget(max_cost_usd: f64) {
    BUDGET_LIMIT.with(|l| *l.borrow_mut() = Some(max_cost_usd));
    BUDGET_SPENT.with(|s| *s.borrow_mut() = 0.0);
}

/// Clear the budget limit.
pub fn clear_budget() {
    BUDGET_LIMIT.with(|l| *l.borrow_mut() = None);
}

/// Push a scoped budget and reset spent for the new scope.
pub fn push_budget_scope(max_cost_usd: f64) {
    let prev_limit = BUDGET_LIMIT.with(|l| *l.borrow());
    let prev_spent = BUDGET_SPENT.with(|s| *s.borrow());
    BUDGET_STACK.with(|stack| stack.borrow_mut().push((prev_limit, prev_spent)));
    set_budget(max_cost_usd);
}

/// Pop a scoped budget and restore the previous budget state.
pub fn pop_budget_scope() {
    let prev = BUDGET_STACK.with(|stack| stack.borrow_mut().pop());
    if let Some((limit, spent)) = prev {
        BUDGET_LIMIT.with(|l| *l.borrow_mut() = limit);
        BUDGET_SPENT.with(|s| *s.borrow_mut() = spent);
    } else {
        clear_budget();
        BUDGET_SPENT.with(|s| *s.borrow_mut() = 0.0);
    }
}

fn get_opt_string(opts: &BTreeMap<Value, Value>, key: &str) -> Option<String> {
    opts.get(&Value::keyword(key)).and_then(|v| match v {
        Value::String(s) => Some(s.to_string()),
        _ => None,
    })
}

fn get_opt_f64(opts: &BTreeMap<Value, Value>, key: &str) -> Option<f64> {
    opts.get(&Value::keyword(key)).and_then(|v| v.as_float())
}

fn get_opt_u32(opts: &BTreeMap<Value, Value>, key: &str) -> Option<u32> {
    opts.get(&Value::keyword(key))
        .and_then(|v| v.as_int())
        .map(|n| n as u32)
}

/// A provider defined in Sema code via lambdas.
/// Only stores String fields (Send+Sync); callbacks live in the
/// LISP_PROVIDERS thread-local, accessed only from the same thread.
struct LispProvider {
    name: String,
    default_model: String,
}

impl LlmProvider for LispProvider {
    fn name(&self) -> &str {
        &self.name
    }

    fn default_model(&self) -> &str {
        &self.default_model
    }

    fn complete(&self, request: ChatRequest) -> Result<ChatResponse, LlmError> {
        let name = self.name.clone();
        LISP_PROVIDERS.with(|providers| {
            let providers = providers.borrow();
            let callbacks = providers.get(&name).ok_or_else(|| {
                LlmError::Config(format!("lisp provider '{}' callbacks not found", name))
            })?;
            let complete_fn = callbacks.complete_fn.clone();

            let request_map = chat_request_to_value(&request);

            let ctx = EvalContext::new();
            ctx.eval_step_limit.set(1_000_000);
            let result = call_value_fn(&ctx, &complete_fn, &[request_map]);

            match result {
                Ok(response_val) => parse_lisp_provider_response(&response_val, &request.model),
                Err(e) => Err(LlmError::Api {
                    status: 0,
                    message: e.to_string(),
                }),
            }
        })
    }
}

/// Convert a ChatRequest into a Sema Value::Map for passing to Lisp provider callbacks.
fn chat_request_to_value(request: &ChatRequest) -> Value {
    let mut map = BTreeMap::new();
    map.insert(Value::keyword("model"), Value::string(&request.model));

    let msgs: Vec<Value> = request
        .messages
        .iter()
        .map(|m| {
            let mut msg_map = BTreeMap::new();
            msg_map.insert(Value::keyword("role"), Value::string(&m.role));
            msg_map.insert(Value::keyword("content"), Value::string(&m.content));
            Value::Map(Rc::new(msg_map))
        })
        .collect();
    map.insert(Value::keyword("messages"), Value::list(msgs));

    if let Some(max_tokens) = request.max_tokens {
        map.insert(Value::keyword("max-tokens"), Value::Int(max_tokens as i64));
    }
    if let Some(temp) = request.temperature {
        map.insert(Value::keyword("temperature"), Value::Float(temp));
    }
    if let Some(ref system) = request.system {
        map.insert(Value::keyword("system"), Value::string(system));
    }

    if !request.tools.is_empty() {
        let tools: Vec<Value> = request
            .tools
            .iter()
            .map(|t| {
                let mut tool_map = BTreeMap::new();
                tool_map.insert(Value::keyword("name"), Value::string(&t.name));
                tool_map.insert(Value::keyword("description"), Value::string(&t.description));
                tool_map.insert(
                    Value::keyword("parameters"),
                    json_to_sema_value(&t.parameters),
                );
                Value::Map(Rc::new(tool_map))
            })
            .collect();
        map.insert(Value::keyword("tools"), Value::list(tools));
    }

    if !request.stop_sequences.is_empty() {
        let seqs: Vec<Value> = request
            .stop_sequences
            .iter()
            .map(|s| Value::string(s))
            .collect();
        map.insert(Value::keyword("stop-sequences"), Value::list(seqs));
    }

    Value::Map(Rc::new(map))
}

/// Parse a Sema Value returned by a Lisp provider callback into a ChatResponse.
fn parse_lisp_provider_response(val: &Value, model: &str) -> Result<ChatResponse, LlmError> {
    match val {
        Value::String(s) => Ok(ChatResponse {
            content: s.to_string(),
            role: "assistant".to_string(),
            model: model.to_string(),
            tool_calls: vec![],
            usage: Usage::default(),
            stop_reason: Some("end_turn".to_string()),
        }),
        Value::Map(map) => {
            let content = map
                .get(&Value::keyword("content"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .unwrap_or_default();
            let role = map
                .get(&Value::keyword("role"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| "assistant".to_string());
            let resp_model = map
                .get(&Value::keyword("model"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| model.to_string());
            let stop_reason = map
                .get(&Value::keyword("stop-reason"))
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
                .or(Some("end_turn".to_string()));

            let usage = if let Some(Value::Map(usage_map)) = map.get(&Value::keyword("usage")) {
                let prompt_tokens = usage_map
                    .get(&Value::keyword("prompt-tokens"))
                    .and_then(|v| v.as_int())
                    .unwrap_or(0) as u32;
                let completion_tokens = usage_map
                    .get(&Value::keyword("completion-tokens"))
                    .and_then(|v| v.as_int())
                    .unwrap_or(0) as u32;
                Usage {
                    prompt_tokens,
                    completion_tokens,
                    model: resp_model.clone(),
                }
            } else {
                Usage {
                    model: resp_model.clone(),
                    ..Default::default()
                }
            };

            let tool_calls = if let Some(Value::List(tcs)) = map.get(&Value::keyword("tool-calls"))
            {
                tcs.iter()
                    .filter_map(|tc| {
                        if let Value::Map(tc_map) = tc {
                            let id = tc_map
                                .get(&Value::keyword("id"))
                                .and_then(|v| v.as_str())
                                .map(|s| s.to_string())
                                .unwrap_or_default();
                            let name = tc_map
                                .get(&Value::keyword("name"))
                                .and_then(|v| v.as_str())
                                .map(|s| s.to_string())?;
                            let arguments = tc_map
                                .get(&Value::keyword("arguments"))
                                .map(|v| match sema_value_to_json(v) {
                                    Ok(j) => j,
                                    Err(_) => serde_json::Value::Object(serde_json::Map::new()),
                                })
                                .unwrap_or(serde_json::Value::Object(serde_json::Map::new()));
                            Some(ToolCall {
                                id,
                                name,
                                arguments,
                            })
                        } else {
                            None
                        }
                    })
                    .collect()
            } else {
                vec![]
            };

            Ok(ChatResponse {
                content,
                role,
                model: resp_model,
                tool_calls,
                usage,
                stop_reason,
            })
        }
        _ => Err(LlmError::Parse(
            "lisp provider must return a string or map with :content".to_string(),
        )),
    }
}

fn register_fn_gated(
    env: &Env,
    sandbox: &sema_core::Sandbox,
    cap: sema_core::Caps,
    name: &str,
    f: impl Fn(&[Value]) -> Result<Value, SemaError> + 'static,
) {
    if sandbox.is_unrestricted() {
        register_fn(env, name, f);
    } else {
        let sandbox = sandbox.clone();
        let fn_name = name.to_string();
        register_fn(env, name, move |args| {
            sandbox.check(cap, &fn_name)?;
            f(args)
        });
    }
}

fn register_fn_ctx_gated(
    env: &Env,
    sandbox: &sema_core::Sandbox,
    cap: sema_core::Caps,
    name: &str,
    f: impl Fn(&sema_core::EvalContext, &[Value]) -> Result<Value, SemaError> + 'static,
) {
    if sandbox.is_unrestricted() {
        register_fn_ctx(env, name, f);
    } else {
        let sandbox = sandbox.clone();
        let fn_name = name.to_string();
        register_fn_ctx(env, name, move |ctx, args| {
            sandbox.check(cap, &fn_name)?;
            f(ctx, args)
        });
    }
}

pub fn register_llm_builtins(env: &Env, sandbox: &sema_core::Sandbox) {
    // (llm/configure :anthropic {:api-key "..." :default-model "..."})
    // (llm/configure :openai {:api-key "..." :base-url "..." :default-model "..."})
    register_fn(env, "llm/configure", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("llm/configure", "2", args.len()));
        }
        let provider_name = args[0]
            .as_keyword()
            .ok_or_else(|| SemaError::type_error("keyword", args[0].type_name()))?;
        let opts = match &args[1] {
            Value::Map(m) => m.as_ref().clone(),
            _ => return Err(SemaError::type_error("map", args[1].type_name())),
        };

        let api_key = get_opt_string(&opts, "api-key");

        PROVIDER_REGISTRY.with(|reg| {
            let mut reg = reg.borrow_mut();
            match provider_name.as_str() {
                "anthropic" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model");
                    let provider = AnthropicProvider::new(api_key, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("anthropic");
                }
                "openai" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let base_url = get_opt_string(&opts, "base-url");
                    let model = get_opt_string(&opts, "default-model");
                    let provider = OpenAiProvider::new(api_key, base_url, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("openai");
                }
                "gemini" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model");
                    let provider = GeminiProvider::new(api_key, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("gemini");
                }
                "groq" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "llama-3.3-70b-versatile".to_string());
                    let base_url = get_opt_string(&opts, "base-url")
                        .unwrap_or_else(|| "https://api.groq.com/openai/v1".to_string());
                    let provider =
                        OpenAiProvider::named("groq".to_string(), api_key, base_url, model, true)
                            .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("groq");
                }
                "xai" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "grok-3-mini-fast".to_string());
                    let base_url = get_opt_string(&opts, "base-url")
                        .unwrap_or_else(|| "https://api.x.ai/v1".to_string());
                    let provider =
                        OpenAiProvider::named("xai".to_string(), api_key, base_url, model, true)
                            .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("xai");
                }
                "mistral" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "mistral-small-latest".to_string());
                    let base_url = get_opt_string(&opts, "base-url")
                        .unwrap_or_else(|| "https://api.mistral.ai/v1".to_string());
                    let provider = OpenAiProvider::named(
                        "mistral".to_string(),
                        api_key,
                        base_url,
                        model,
                        false,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("mistral");
                }
                "moonshot" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "moonshot-v1-8k".to_string());
                    let base_url = get_opt_string(&opts, "base-url")
                        .unwrap_or_else(|| "https://api.moonshot.ai/v1".to_string());
                    let provider = OpenAiProvider::named(
                        "moonshot".to_string(),
                        api_key,
                        base_url,
                        model,
                        false,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("moonshot");
                }
                "ollama" => {
                    let host =
                        get_opt_string(&opts, "host").or_else(|| get_opt_string(&opts, "base-url"));
                    let model = get_opt_string(&opts, "default-model");
                    // Ollama doesn't use api-key
                    let provider = OllamaProvider::new(host, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("ollama");
                }
                "jina" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "jina-embeddings-v3".to_string());
                    let provider = OpenAiCompatEmbeddingProvider::new(
                        "jina".to_string(),
                        api_key,
                        "https://api.jina.ai/v1".to_string(),
                        model,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider("jina");
                }
                "voyage" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "voyage-3-lite".to_string());
                    let provider = OpenAiCompatEmbeddingProvider::new(
                        "voyage".to_string(),
                        api_key,
                        "https://api.voyageai.com/v1".to_string(),
                        model,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider("voyage");
                }
                "cohere" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model");
                    let provider = CohereEmbeddingProvider::new(api_key, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider("cohere");
                }
                other => {
                    // Treat unknown providers as OpenAI-compatible if base-url and api-key are provided
                    let api_key = api_key.clone().ok_or_else(|| {
                        SemaError::Llm(format!(
                            "unknown provider '{other}': provide :api-key and :base-url to register as OpenAI-compatible"
                        ))
                    })?;
                    let base_url = get_opt_string(&opts, "base-url").ok_or_else(|| {
                        SemaError::Llm(format!(
                            "unknown provider '{other}': provide :base-url to register as OpenAI-compatible"
                        ))
                    })?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "default".to_string());
                    let provider = OpenAiProvider::named(
                        other.to_string(),
                        api_key,
                        base_url,
                        model,
                        false,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default(other);
                }
            }
            Ok(Value::Nil)
        })
    });

    // (llm/define-provider :name {:complete fn :default-model "..." :stream fn})
    register_fn(env, "llm/define-provider", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("llm/define-provider", "2", args.len()));
        }
        let provider_name = args[0]
            .as_keyword()
            .ok_or_else(|| SemaError::type_error("keyword", args[0].type_name()))?;
        let opts = match &args[1] {
            Value::Map(m) => m.as_ref().clone(),
            _ => return Err(SemaError::type_error("map", args[1].type_name())),
        };

        let complete_fn = opts
            .get(&Value::keyword("complete"))
            .cloned()
            .ok_or_else(|| SemaError::eval("llm/define-provider requires :complete function"))?;

        match &complete_fn {
            Value::Lambda(_) | Value::NativeFn(_) => {}
            _ => return Err(SemaError::type_error("function", complete_fn.type_name())),
        }

        let default_model =
            get_opt_string(&opts, "default-model").unwrap_or_else(|| "default".to_string());

        let name_for_callbacks = provider_name.clone();
        LISP_PROVIDERS.with(|providers| {
            providers
                .borrow_mut()
                .insert(name_for_callbacks, LispProviderCallbacks { complete_fn });
        });

        let name_for_registry = provider_name.clone();
        let model_clone = default_model.clone();
        PROVIDER_REGISTRY.with(|reg| {
            let mut reg = reg.borrow_mut();
            reg.register(Box::new(LispProvider {
                name: name_for_registry,
                default_model: model_clone,
            }));
            reg.set_default(&provider_name);
        });

        Ok(Value::keyword(&provider_name))
    });

    // Auto-configure from environment variables
    register_fn(env, "llm/auto-configure", |_args| {
        let forced_model = std::env::var("SEMA_DEFAULT_MODEL")
            .ok()
            .filter(|m| !m.is_empty());
        let forced_provider = std::env::var("SEMA_LLM_PROVIDER")
            .ok()
            .map(|p| p.trim().to_ascii_lowercase())
            .filter(|p| !p.is_empty());

        let result = PROVIDER_REGISTRY.with(|reg| {
            let mut reg = reg.borrow_mut();
            let mut first_configured: Option<String> = None;

            // Try Anthropic first (preferred)
            if let Ok(key) = std::env::var("ANTHROPIC_API_KEY") {
                if !key.is_empty() {
                    let provider = AnthropicProvider::new(key, forced_model.clone())
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if first_configured.is_none() {
                        reg.set_default("anthropic");
                        first_configured = Some("anthropic".to_string());
                    }
                }
            }
            // Try OpenAI
            if let Ok(key) = std::env::var("OPENAI_API_KEY") {
                if !key.is_empty() {
                    let provider = OpenAiProvider::new(key, None, forced_model.clone())
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if first_configured.is_none() {
                        reg.set_default("openai");
                        first_configured = Some("openai".to_string());
                    }
                }
            }
            // Try Groq
            if let Ok(key) = std::env::var("GROQ_API_KEY") {
                if !key.is_empty() {
                    let model = forced_model
                        .clone()
                        .unwrap_or_else(|| "llama-3.3-70b-versatile".to_string());
                    let provider = OpenAiProvider::named(
                        "groq".to_string(),
                        key,
                        "https://api.groq.com/openai/v1".to_string(),
                        model,
                        true,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if first_configured.is_none() {
                        reg.set_default("groq");
                        first_configured = Some("groq".to_string());
                    }
                }
            }
            // Try xAI
            if let Ok(key) = std::env::var("XAI_API_KEY") {
                if !key.is_empty() {
                    let model = forced_model
                        .clone()
                        .unwrap_or_else(|| "grok-3-mini-fast".to_string());
                    let provider = OpenAiProvider::named(
                        "xai".to_string(),
                        key,
                        "https://api.x.ai/v1".to_string(),
                        model,
                        true,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if first_configured.is_none() {
                        reg.set_default("xai");
                        first_configured = Some("xai".to_string());
                    }
                }
            }
            // Try Mistral
            if let Ok(key) = std::env::var("MISTRAL_API_KEY") {
                if !key.is_empty() {
                    let model = forced_model
                        .clone()
                        .unwrap_or_else(|| "mistral-small-latest".to_string());
                    let provider = OpenAiProvider::named(
                        "mistral".to_string(),
                        key,
                        "https://api.mistral.ai/v1".to_string(),
                        model,
                        false,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if first_configured.is_none() {
                        reg.set_default("mistral");
                        first_configured = Some("mistral".to_string());
                    }
                }
            }
            // Try Moonshot
            if let Ok(key) = std::env::var("MOONSHOT_API_KEY") {
                if !key.is_empty() {
                    let model = forced_model
                        .clone()
                        .unwrap_or_else(|| "moonshot-v1-8k".to_string());
                    let provider = OpenAiProvider::named(
                        "moonshot".to_string(),
                        key,
                        "https://api.moonshot.ai/v1".to_string(),
                        model,
                        false,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if first_configured.is_none() {
                        reg.set_default("moonshot");
                        first_configured = Some("moonshot".to_string());
                    }
                }
            }
            // Try Google Gemini
            if let Ok(key) = std::env::var("GOOGLE_API_KEY") {
                if !key.is_empty() {
                    let provider = GeminiProvider::new(key, forced_model.clone())
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if first_configured.is_none() {
                        reg.set_default("gemini");
                        first_configured = Some("gemini".to_string());
                    }
                }
            }
            // Try Ollama (local, no auth)
            if std::env::var("OLLAMA_HOST").is_ok() {
                let provider = OllamaProvider::new(None, forced_model.clone())
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                reg.register(Box::new(provider));
                if first_configured.is_none() {
                    reg.set_default("ollama");
                    first_configured = Some("ollama".to_string());
                }
            }

            // Auto-configure embedding providers
            if let Ok(key) = std::env::var("JINA_API_KEY") {
                if !key.is_empty() {
                    let provider = OpenAiCompatEmbeddingProvider::new(
                        "jina".to_string(),
                        key,
                        "https://api.jina.ai/v1".to_string(),
                        "jina-embeddings-v3".to_string(),
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider("jina");
                }
            }
            if let Ok(key) = std::env::var("VOYAGE_API_KEY") {
                if !key.is_empty() {
                    let provider = OpenAiCompatEmbeddingProvider::new(
                        "voyage".to_string(),
                        key,
                        "https://api.voyageai.com/v1".to_string(),
                        "voyage-3".to_string(),
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    // Only set as embedding provider if jina wasn't already set
                    if reg.embedding_provider().is_none() {
                        reg.set_embedding_provider("voyage");
                    }
                }
            }
            if let Ok(key) = std::env::var("COHERE_API_KEY") {
                if !key.is_empty() {
                    let provider = CohereEmbeddingProvider::new(key, None)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    if reg.embedding_provider().is_none() {
                        reg.set_embedding_provider("cohere");
                    }
                }
            }

            if let Some(requested_provider) = forced_provider.as_deref() {
                if reg.get(requested_provider).is_some() {
                    reg.set_default(requested_provider);
                    first_configured = Some(requested_provider.to_string());
                } else {
                    return Err(SemaError::Llm(format!(
                        "requested provider is not configured: {requested_provider}"
                    )));
                }
            }

            match first_configured {
                Some(name) => Ok(Value::keyword(&name)),
                None => Ok(Value::Nil),
            }
        })?;

        // Best-effort pricing refresh (outside PROVIDER_REGISTRY borrow)
        pricing::refresh_pricing(pricing::default_cache_path().as_deref());

        Ok(result)
    });

    // (llm/complete "prompt text" {:model "..." :max-tokens 200 :temperature 0.5})
    register_fn_gated(env, sandbox, sema_core::Caps::LLM, "llm/complete", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("llm/complete", "1-2", args.len()));
        }
        let prompt_text = match &args[0] {
            Value::String(s) => s.to_string(),
            Value::Prompt(p) => {
                // Use the prompt's messages
                return complete_with_prompt(p, args.get(1));
            }
            _ => {
                return Err(SemaError::type_error(
                    "string or prompt",
                    args[0].type_name(),
                ))
            }
        };

        let mut model = String::new();
        let mut max_tokens = None;
        let mut temperature = None;
        let mut system = None;

        if let Some(opts_val) = args.get(1) {
            if let Value::Map(opts) = opts_val {
                model = get_opt_string(opts, "model").unwrap_or_default();
                max_tokens = get_opt_u32(opts, "max-tokens");
                temperature = get_opt_f64(opts, "temperature");
                system = get_opt_string(opts, "system");
            }
        }

        let messages = vec![ChatMessage {
            role: "user".to_string(),
            content: prompt_text,
        }];

        let mut request = ChatRequest::new(model, messages);
        request.max_tokens = max_tokens.or(Some(4096));
        request.temperature = temperature;
        request.system = system;

        let response = do_complete(request)?;
        track_usage(&response.usage)?;
        Ok(Value::String(Rc::new(response.content)))
    });

    // (llm/chat messages {:model "..." :tools [...] :tool-mode :auto ...})
    register_fn_ctx_gated(
        env,
        sandbox,
        sema_core::Caps::LLM,
        "llm/chat",
        |ctx, args| {
            if args.is_empty() || args.len() > 2 {
                return Err(SemaError::arity("llm/chat", "1-2", args.len()));
            }

            let messages = extract_messages(&args[0])?;

            let mut model = String::new();
            let mut max_tokens = None;
            let mut temperature = None;
            let mut system = None;
            let mut tools: Vec<Value> = Vec::new();
            let mut tool_mode = "auto".to_string();
            let mut max_tool_rounds = 10usize;

            if let Some(opts_val) = args.get(1) {
                if let Value::Map(opts) = opts_val {
                    model = get_opt_string(opts, "model").unwrap_or_default();
                    max_tokens = get_opt_u32(opts, "max-tokens");
                    temperature = get_opt_f64(opts, "temperature");
                    system = get_opt_string(opts, "system");
                    if let Some(Value::List(t)) = opts.get(&Value::keyword("tools")) {
                        tools = t.as_ref().clone();
                    } else if let Some(Value::Vector(t)) = opts.get(&Value::keyword("tools")) {
                        tools = t.as_ref().clone();
                    }
                    if let Some(mode) = opts.get(&Value::keyword("tool-mode")) {
                        if let Some(s) = mode.as_keyword() {
                            tool_mode = s;
                        }
                    }
                    if let Some(rounds) = opts.get(&Value::keyword("max-tool-rounds")) {
                        if let Some(n) = rounds.as_int() {
                            max_tool_rounds = n as usize;
                        }
                    }
                }
            }

            if tools.is_empty() || tool_mode == "none" {
                // Simple chat without tools
                let mut request = ChatRequest::new(model, messages);
                request.max_tokens = max_tokens.or(Some(4096));
                request.temperature = temperature;
                request.system = system;
                let response = do_complete(request)?;
                track_usage(&response.usage)?;
                Ok(Value::String(Rc::new(response.content)))
            } else {
                // Chat with tool execution loop
                let tool_schemas = build_tool_schemas(&tools)?;
                let (result, _msgs) = run_tool_loop(
                    ctx,
                    messages,
                    model,
                    max_tokens,
                    temperature,
                    system,
                    &tools,
                    &tool_schemas,
                    max_tool_rounds,
                    None,
                )?;
                Ok(Value::String(Rc::new(result)))
            }
        },
    );

    // (llm/send prompt {:model "..." ...})
    register_fn_gated(env, sandbox, sema_core::Caps::LLM, "llm/send", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("llm/send", "1-2", args.len()));
        }
        let prompt = match &args[0] {
            Value::Prompt(p) => p.clone(),
            _ => return Err(SemaError::type_error("prompt", args[0].type_name())),
        };
        complete_with_prompt(&prompt, args.get(1))
    });

    // (llm/stream "prompt" callback {:max-tokens 200})
    // (llm/stream "prompt" {:max-tokens 200})  — prints to stdout
    register_fn_ctx(env, "llm/stream", |ctx, args| {
        if args.is_empty() || args.len() > 3 {
            return Err(SemaError::arity("llm/stream", "1-3", args.len()));
        }

        // Parse the prompt/messages
        let messages = match &args[0] {
            Value::String(s) => vec![ChatMessage {
                role: "user".to_string(),
                content: s.to_string(),
            }],
            Value::Prompt(p) => p
                .messages
                .iter()
                .map(|m| ChatMessage {
                    role: m.role.to_string(),
                    content: m.content.clone(),
                })
                .collect(),
            Value::List(_) | Value::Vector(_) => extract_messages(&args[0])?,
            _ => {
                return Err(SemaError::type_error(
                    "string, prompt, or messages",
                    args[0].type_name(),
                ))
            }
        };

        // Parse optional callback and opts
        let mut callback: Option<Value> = None;
        let mut opts_map: Option<&BTreeMap<Value, Value>> = None;

        for arg in &args[1..] {
            match arg {
                Value::Lambda(_) | Value::NativeFn(_) => callback = Some(arg.clone()),
                Value::Map(m) => opts_map = Some(m.as_ref()),
                _ => {}
            }
        }

        let mut model = String::new();
        let mut max_tokens = None;
        let mut temperature = None;
        let mut system = None;

        if let Some(opts) = opts_map {
            model = get_opt_string(opts, "model").unwrap_or_default();
            max_tokens = get_opt_u32(opts, "max-tokens");
            temperature = get_opt_f64(opts, "temperature");
            system = get_opt_string(opts, "system");
        }

        let mut request = ChatRequest::new(model, messages);
        request.max_tokens = max_tokens.or(Some(4096));
        request.temperature = temperature;
        request.system = system;

        let response = with_provider(|p| {
            if request.model.is_empty() {
                let mut req = request.clone();
                req.model = p.default_model().to_string();
                let mut chunk_cb = |chunk: &str| -> Result<(), crate::types::LlmError> {
                    if let Some(ref cb) = callback {
                        call_value_fn(ctx, cb, &[Value::String(Rc::new(chunk.to_string()))])
                            .map_err(|e| crate::types::LlmError::Config(e.to_string()))?;
                    } else {
                        print!("{}", chunk);
                        use std::io::Write;
                        std::io::stdout().flush().ok();
                    }
                    Ok(())
                };
                p.stream_complete(req, &mut chunk_cb)
                    .map_err(|e| SemaError::Llm(e.to_string()))
            } else {
                let mut chunk_cb = |chunk: &str| -> Result<(), crate::types::LlmError> {
                    if let Some(ref cb) = callback {
                        call_value_fn(ctx, cb, &[Value::String(Rc::new(chunk.to_string()))])
                            .map_err(|e| crate::types::LlmError::Config(e.to_string()))?;
                    } else {
                        print!("{}", chunk);
                        use std::io::Write;
                        std::io::stdout().flush().ok();
                    }
                    Ok(())
                };
                p.stream_complete(request.clone(), &mut chunk_cb)
                    .map_err(|e| SemaError::Llm(e.to_string()))
            }
        })?;

        // Print newline after streaming if using default display
        if callback.is_none() {
            println!();
        }

        track_usage(&response.usage)?;
        Ok(Value::String(Rc::new(response.content)))
    });

    // (llm/extract schema text {:model "..." :validate true :retries 2})
    register_fn(env, "llm/extract", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("llm/extract", "2-3", args.len()));
        }
        let schema = &args[0];
        let text = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;

        let schema_desc = format_schema(schema);
        let system = format!(
            "Extract structured data from the text. Respond with ONLY a JSON object matching this schema:\n{}\nDo not include any other text.",
            schema_desc
        );
        let messages = vec![ChatMessage {
            role: "user".to_string(),
            content: text.to_string(),
        }];

        let mut model = String::new();
        let mut validate = false;
        let mut max_retries: u32 = 0;
        if let Some(opts_val) = args.get(2) {
            if let Value::Map(opts) = opts_val {
                model = get_opt_string(opts, "model").unwrap_or_default();
                validate = opts
                    .get(&Value::keyword("validate"))
                    .map(|v| v.is_truthy())
                    .unwrap_or(false);
                max_retries = get_opt_u32(opts, "retries").unwrap_or(0);
            }
        }

        let mut last_validation_error = String::new();

        for attempt in 0..=max_retries {
            let mut request = ChatRequest::new(model.clone(), messages.clone());
            if attempt == 0 {
                request.system = Some(system.clone());
            } else {
                request.system = Some(format!(
                    "{}\n\nYour previous response had validation errors: {}. Please fix these issues.",
                    system, last_validation_error
                ));
            }

            let response = do_complete(request)?;
            track_usage(&response.usage)?;

            // Parse JSON response back to Sema value
            let content = response.content.trim();
            // Strip markdown code fences if present
            let json_str = if content.starts_with("```") {
                let inner = content
                    .trim_start_matches("```json")
                    .trim_start_matches("```")
                    .trim_end_matches("```")
                    .trim();
                inner
            } else {
                content
            };
            let json: serde_json::Value = serde_json::from_str(json_str).map_err(|e| {
                SemaError::Llm(format!(
                    "failed to parse LLM JSON response: {e}\nResponse was: {content}"
                ))
            })?;
            let result = json_to_sema_value(&json);

            if validate {
                match validate_extraction(&result, schema) {
                    Ok(()) => return Ok(result),
                    Err(err) => {
                        last_validation_error = err;
                        if attempt == max_retries {
                            return Err(SemaError::Llm(format!(
                                "extraction validation failed after {} attempt(s): {}",
                                max_retries + 1,
                                last_validation_error
                            )));
                        }
                    }
                }
            } else {
                return Ok(result);
            }
        }

        unreachable!()
    });

    // (llm/classify categories text {:model "..."})
    register_fn(env, "llm/classify", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("llm/classify", "2-3", args.len()));
        }
        let categories = match &args[0] {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };
        let text = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;

        let cat_names: Vec<String> = categories
            .iter()
            .map(|c| match c {
                Value::Keyword(s) => resolve(*s),
                Value::String(s) => s.to_string(),
                other => other.to_string(),
            })
            .collect();

        let system = format!(
            "Classify the following text into exactly one of these categories: {}\nRespond with ONLY the category name, nothing else.",
            cat_names.join(", ")
        );
        let messages = vec![ChatMessage {
            role: "user".to_string(),
            content: text.to_string(),
        }];

        let mut model = String::new();
        if let Some(opts_val) = args.get(2) {
            if let Value::Map(opts) = opts_val {
                model = get_opt_string(opts, "model").unwrap_or_default();
            }
        }

        let mut request = ChatRequest::new(model, messages);
        request.system = Some(system);

        let response = do_complete(request)?;
        track_usage(&response.usage)?;

        let category = response.content.trim().to_string();
        // Return as keyword if it was in the original list as keyword
        if categories
            .iter()
            .any(|c| matches!(c, Value::Keyword(s) if resolve(*s) == category))
        {
            Ok(Value::keyword(&category))
        } else {
            Ok(Value::String(Rc::new(category)))
        }
    });

    // Conversation functions

    // (conversation/new {:model "..."})
    register_fn(env, "conversation/new", |args| {
        let mut model = String::new();
        let mut metadata = BTreeMap::new();
        if let Some(opts_val) = args.first() {
            if let Value::Map(opts) = opts_val {
                model = get_opt_string(opts, "model").unwrap_or_default();
                for (k, v) in opts.iter() {
                    if let Value::Keyword(key) = k {
                        let key_str = resolve(*key);
                        if key_str != "model" {
                            metadata.insert(
                                key_str,
                                match v {
                                    Value::String(s) => s.to_string(),
                                    other => other.to_string(),
                                },
                            );
                        }
                    }
                }
            }
        }
        Ok(Value::Conversation(Rc::new(Conversation {
            messages: Vec::new(),
            model,
            metadata,
        })))
    });

    // (conversation/say conv "message" {:temperature 0.5 :max-tokens 2048 :system "..."})
    register_fn(env, "conversation/say", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("conversation/say", "2-3", args.len()));
        }
        let conv = match &args[0] {
            Value::Conversation(c) => c.clone(),
            _ => return Err(SemaError::type_error("conversation", args[0].type_name())),
        };
        let user_msg = match &args[1] {
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };

        // Parse optional opts
        let mut temperature = None;
        let mut max_tokens = None;
        let mut system = None;
        if let Some(opts_val) = args.get(2) {
            if let Value::Map(opts) = opts_val {
                temperature = get_opt_f64(opts, "temperature");
                max_tokens = get_opt_u32(opts, "max-tokens");
                system = get_opt_string(opts, "system");
            }
        }

        // Build messages for API call
        let mut chat_messages: Vec<ChatMessage> = conv
            .messages
            .iter()
            .map(|m| ChatMessage {
                role: m.role.to_string(),
                content: m.content.clone(),
            })
            .collect();
        chat_messages.push(ChatMessage {
            role: "user".to_string(),
            content: user_msg.clone(),
        });

        let mut request = ChatRequest::new(conv.model.clone(), chat_messages);
        request.temperature = temperature;
        request.max_tokens = max_tokens.or(Some(4096));
        request.system = system;

        let response = do_complete(request)?;
        track_usage(&response.usage)?;

        // Build new conversation with user message + assistant reply
        let mut new_messages = conv.messages.clone();
        new_messages.push(Message {
            role: Role::User,
            content: user_msg,
        });
        new_messages.push(Message {
            role: Role::Assistant,
            content: response.content,
        });

        Ok(Value::Conversation(Rc::new(Conversation {
            messages: new_messages,
            model: conv.model.clone(),
            metadata: conv.metadata.clone(),
        })))
    });

    // (conversation/messages conv)
    register_fn(env, "conversation/messages", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("conversation/messages", "1", args.len()));
        }
        let conv = match &args[0] {
            Value::Conversation(c) => c.clone(),
            _ => return Err(SemaError::type_error("conversation", args[0].type_name())),
        };
        let msgs: Vec<Value> = conv
            .messages
            .iter()
            .map(|m| Value::Message(Rc::new(m.clone())))
            .collect();
        Ok(Value::list(msgs))
    });

    // (conversation/last-reply conv)
    register_fn(env, "conversation/last-reply", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("conversation/last-reply", "1", args.len()));
        }
        let conv = match &args[0] {
            Value::Conversation(c) => c.clone(),
            _ => return Err(SemaError::type_error("conversation", args[0].type_name())),
        };
        conv.messages
            .iter()
            .rfind(|m| m.role == Role::Assistant)
            .map(|m| Value::String(Rc::new(m.content.clone())))
            .ok_or_else(|| SemaError::eval("no assistant reply in conversation"))
    });

    // (conversation/fork conv)
    register_fn(env, "conversation/fork", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("conversation/fork", "1", args.len()));
        }
        // Fork returns a copy - since conversations are immutable, this is just clone
        Ok(args[0].clone())
    });

    // Prompt functions

    // (prompt/append p1 p2)
    register_fn(env, "prompt/append", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("prompt/append", "2", args.len()));
        }
        let p1 = match &args[0] {
            Value::Prompt(p) => p.clone(),
            _ => return Err(SemaError::type_error("prompt", args[0].type_name())),
        };
        let p2 = match &args[1] {
            Value::Prompt(p) => p.clone(),
            _ => return Err(SemaError::type_error("prompt", args[1].type_name())),
        };
        let mut messages = p1.messages.clone();
        messages.extend(p2.messages.iter().cloned());
        Ok(Value::Prompt(Rc::new(Prompt { messages })))
    });

    // (prompt/messages prompt)
    register_fn(env, "prompt/messages", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("prompt/messages", "1", args.len()));
        }
        let p = match &args[0] {
            Value::Prompt(p) => p.clone(),
            _ => return Err(SemaError::type_error("prompt", args[0].type_name())),
        };
        let msgs: Vec<Value> = p
            .messages
            .iter()
            .map(|m| Value::Message(Rc::new(m.clone())))
            .collect();
        Ok(Value::list(msgs))
    });

    // (prompt/set-system prompt "new system message")
    register_fn(env, "prompt/set-system", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("prompt/set-system", "2", args.len()));
        }
        let p = match &args[0] {
            Value::Prompt(p) => p.clone(),
            _ => return Err(SemaError::type_error("prompt", args[0].type_name())),
        };
        let new_system = match &args[1] {
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };
        let mut messages: Vec<Message> = p
            .messages
            .iter()
            .filter(|m| m.role != Role::System)
            .cloned()
            .collect();
        messages.insert(
            0,
            Message {
                role: Role::System,
                content: new_system,
            },
        );
        Ok(Value::Prompt(Rc::new(Prompt { messages })))
    });

    // (message/role msg)
    register_fn(env, "message/role", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("message/role", "1", args.len()));
        }
        let msg = match &args[0] {
            Value::Message(m) => m.clone(),
            _ => return Err(SemaError::type_error("message", args[0].type_name())),
        };
        Ok(Value::keyword(&msg.role.to_string()))
    });

    // (message/content msg)
    register_fn(env, "message/content", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("message/content", "1", args.len()));
        }
        let msg = match &args[0] {
            Value::Message(m) => m.clone(),
            _ => return Err(SemaError::type_error("message", args[0].type_name())),
        };
        Ok(Value::String(Rc::new(msg.content.clone())))
    });

    // Usage tracking

    // (llm/last-usage)
    register_fn(env, "llm/last-usage", |_args| {
        LAST_USAGE.with(|u| {
            let u = u.borrow();
            match &*u {
                Some(usage) => {
                    let mut map = BTreeMap::new();
                    map.insert(
                        Value::keyword("prompt-tokens"),
                        Value::Int(usage.prompt_tokens as i64),
                    );
                    map.insert(
                        Value::keyword("completion-tokens"),
                        Value::Int(usage.completion_tokens as i64),
                    );
                    map.insert(
                        Value::keyword("total-tokens"),
                        Value::Int(usage.total_tokens() as i64),
                    );
                    map.insert(
                        Value::keyword("model"),
                        Value::String(Rc::new(usage.model.clone())),
                    );
                    if let Some(cost) = pricing::calculate_cost(usage) {
                        map.insert(Value::keyword("cost-usd"), Value::Float(cost));
                    }
                    Ok(Value::Map(Rc::new(map)))
                }
                None => Ok(Value::Nil),
            }
        })
    });

    // (llm/session-usage)
    register_fn(env, "llm/session-usage", |_args| {
        SESSION_USAGE.with(|u| {
            let usage = u.borrow();
            let mut map = BTreeMap::new();
            map.insert(
                Value::keyword("prompt-tokens"),
                Value::Int(usage.prompt_tokens as i64),
            );
            map.insert(
                Value::keyword("completion-tokens"),
                Value::Int(usage.completion_tokens as i64),
            );
            map.insert(
                Value::keyword("total-tokens"),
                Value::Int(usage.total_tokens() as i64),
            );
            let session_cost = SESSION_COST.with(|sc| *sc.borrow());
            map.insert(Value::keyword("cost-usd"), Value::Float(session_cost));
            Ok(Value::Map(Rc::new(map)))
        })
    });

    // (agent/run agent "msg") returns string
    // (agent/run agent "msg" {:on-tool-call cb :messages history}) returns {:response "..." :messages [...]}
    register_fn_ctx(env, "agent/run", |ctx, args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("agent/run", "2-3", args.len()));
        }
        let agent = match &args[0] {
            Value::Agent(a) => a.clone(),
            _ => return Err(SemaError::type_error("agent", args[0].type_name())),
        };
        let user_msg = match &args[1] {
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };

        // Extract options from 3rd arg
        let opts = if let Some(Value::Map(m)) = args.get(2) {
            Some(m.clone())
        } else {
            None
        };

        let on_tool_call = opts
            .as_ref()
            .and_then(|o| o.get(&Value::keyword("on-tool-call")).cloned());

        // Build messages: prior history + new user message
        let mut messages = if let Some(ref o) = opts {
            if let Some(history) = o.get(&Value::keyword("messages")) {
                sema_list_to_chat_messages(history)?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        messages.push(ChatMessage {
            role: "user".to_string(),
            content: user_msg,
        });

        let tool_schemas = build_tool_schemas(&agent.tools)?;
        let system = if agent.system.is_empty() {
            None
        } else {
            Some(agent.system.clone())
        };

        let (result, final_messages) = run_tool_loop(
            ctx,
            messages,
            agent.model.clone(),
            Some(4096),
            None,
            system,
            &agent.tools,
            &tool_schemas,
            agent.max_turns,
            on_tool_call.as_ref(),
        )?;

        // 3-arg form with opts: return {:response "..." :messages [...]}
        if opts.is_some() {
            let mut map = BTreeMap::new();
            map.insert(Value::keyword("response"), Value::String(Rc::new(result)));
            map.insert(
                Value::keyword("messages"),
                chat_messages_to_sema_list(&final_messages),
            );
            Ok(Value::Map(Rc::new(map)))
        } else {
            // 2-arg form: return string (backward compat)
            Ok(Value::String(Rc::new(result)))
        }
    });

    // (llm/pmap fn collection {:max-tokens N ...})
    // Maps fn over collection to produce prompts, then sends all prompts in parallel via batch_complete
    register_fn_ctx(env, "llm/pmap", |ctx, args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("llm/pmap", "2-3", args.len()));
        }
        let func = &args[0];
        let items = match &args[1] {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
        };

        let mut model = String::new();
        let mut max_tokens = None;
        let mut temperature = None;
        let mut system = None;

        if let Some(opts_val) = args.get(2) {
            if let Value::Map(opts) = opts_val {
                model = get_opt_string(opts, "model").unwrap_or_default();
                max_tokens = get_opt_u32(opts, "max-tokens");
                temperature = get_opt_f64(opts, "temperature");
                system = get_opt_string(opts, "system");
            }
        }

        // Step 1: Map fn over items to produce prompt strings (sequentially, since Rc)
        let mut prompts = Vec::with_capacity(items.len());
        for item in &items {
            let result = call_value_fn(ctx, func, &[item.clone()])?;
            let prompt_str = match &result {
                Value::String(s) => s.to_string(),
                other => other.to_string(),
            };
            prompts.push(prompt_str);
        }

        // Step 2: Build ChatRequests
        let requests: Vec<ChatRequest> = prompts
            .into_iter()
            .map(|prompt_text| {
                let messages = vec![ChatMessage {
                    role: "user".to_string(),
                    content: prompt_text,
                }];
                let mut req = ChatRequest::new(model.clone(), messages);
                req.max_tokens = max_tokens.or(Some(4096));
                req.temperature = temperature;
                req.system = system.clone();
                req
            })
            .collect();

        // Step 3: batch_complete (runs concurrently at provider level)
        let responses = with_provider(|p| {
            let reqs: Vec<ChatRequest> = requests
                .into_iter()
                .map(|mut r| {
                    if r.model.is_empty() {
                        r.model = p.default_model().to_string();
                    }
                    r
                })
                .collect();
            Ok(p.batch_complete(reqs))
        })?;

        // Step 4: Collect results
        let mut results = Vec::with_capacity(responses.len());
        for resp_result in responses {
            let resp = resp_result.map_err(|e| SemaError::Llm(e.to_string()))?;
            track_usage(&resp.usage)?;
            results.push(Value::String(Rc::new(resp.content)));
        }
        Ok(Value::list(results))
    });

    // (llm/batch ["prompt1" "prompt2" "prompt3"] {:max-tokens 100})
    register_fn(env, "llm/batch", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("llm/batch", "1-2", args.len()));
        }
        let prompts = match &args[0] {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };

        let mut model = String::new();
        let mut max_tokens = None;
        let mut temperature = None;
        let mut system = None;

        if let Some(opts_val) = args.get(1) {
            if let Value::Map(opts) = opts_val {
                model = get_opt_string(opts, "model").unwrap_or_default();
                max_tokens = get_opt_u32(opts, "max-tokens");
                temperature = get_opt_f64(opts, "temperature");
                system = get_opt_string(opts, "system");
            }
        }

        let requests: Vec<ChatRequest> = prompts
            .iter()
            .map(|prompt_val| {
                let prompt_text = match prompt_val {
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                };
                let messages = vec![ChatMessage {
                    role: "user".to_string(),
                    content: prompt_text,
                }];
                let mut req = ChatRequest::new(model.clone(), messages);
                req.max_tokens = max_tokens.or(Some(4096));
                req.temperature = temperature;
                req.system = system.clone();
                req
            })
            .collect();

        let responses = with_provider(|p| {
            let reqs: Vec<ChatRequest> = requests
                .into_iter()
                .map(|mut r| {
                    if r.model.is_empty() {
                        r.model = p.default_model().to_string();
                    }
                    r
                })
                .collect();
            Ok(p.batch_complete(reqs))
        })?;

        let mut results = Vec::with_capacity(responses.len());
        for resp_result in responses {
            let resp = resp_result.map_err(|e| SemaError::Llm(e.to_string()))?;
            track_usage(&resp.usage)?;
            results.push(Value::String(Rc::new(resp.content)));
        }
        Ok(Value::list(results))
    });

    // (llm/set-pricing "model-pattern" input-per-million output-per-million)
    register_fn(env, "llm/set-pricing", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("llm/set-pricing", "3", args.len()));
        }
        let model_pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let input_cost = args[1]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
        let output_cost = args[2]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[2].type_name()))?;
        pricing::set_custom_pricing(model_pattern, input_cost, output_cost);
        Ok(Value::Nil)
    });

    // (llm/configure-embeddings :openai {:api-key "..." :base-url "..." :model "..."})
    // (llm/configure-embeddings :jina {:api-key "..."})
    // (llm/configure-embeddings :voyage {:api-key "..."})
    // (llm/configure-embeddings :cohere {:api-key "..."})
    register_fn(env, "llm/configure-embeddings", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity(
                "llm/configure-embeddings",
                "2",
                args.len(),
            ));
        }
        let provider_name = args[0]
            .as_keyword()
            .ok_or_else(|| SemaError::type_error("keyword", args[0].type_name()))?;
        let opts = match &args[1] {
            Value::Map(m) => m.as_ref().clone(),
            _ => return Err(SemaError::type_error("map", args[1].type_name())),
        };

        let api_key = get_opt_string(&opts, "api-key");

        PROVIDER_REGISTRY.with(|reg| {
            let mut reg = reg.borrow_mut();
            match provider_name.as_str() {
                "jina" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "jina-embeddings-v3".to_string());
                    let provider = OpenAiCompatEmbeddingProvider::new(
                        "jina".to_string(),
                        api_key,
                        "https://api.jina.ai/v1".to_string(),
                        model,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider("jina");
                }
                "voyage" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model")
                        .unwrap_or_else(|| "voyage-3".to_string());
                    let provider = OpenAiCompatEmbeddingProvider::new(
                        "voyage".to_string(),
                        api_key,
                        "https://api.voyageai.com/v1".to_string(),
                        model,
                    )
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider("voyage");
                }
                "cohere" => {
                    let api_key = api_key
                        .clone()
                        .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;
                    let model = get_opt_string(&opts, "default-model");
                    let provider = CohereEmbeddingProvider::new(api_key, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider("cohere");
                }
                _ => {
                    // Default: OpenAI-compatible
                    let api_key = api_key.unwrap_or_default();
                    let base_url = get_opt_string(&opts, "base-url");
                    let model = get_opt_string(&opts, "default-model")
                        .or_else(|| get_opt_string(&opts, "model"));
                    let provider = OpenAiProvider::new(api_key, base_url, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    let name = provider.name().to_string();
                    reg.register(Box::new(provider));
                    reg.set_embedding_provider(&name);
                }
            }
            Ok(Value::Nil)
        })
    });

    // (llm/embed "text" {:model "..."})
    // (llm/embed ["text1" "text2"] {:model "..."})
    register_fn(env, "llm/embed", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("llm/embed", "1-2", args.len()));
        }

        let (texts, single) = match &args[0] {
            Value::String(s) => (vec![s.to_string()], true),
            Value::List(l) | Value::Vector(l) => {
                let texts: Vec<String> = l
                    .iter()
                    .map(|v| match v {
                        Value::String(s) => Ok(s.to_string()),
                        other => Ok(other.to_string()),
                    })
                    .collect::<Result<_, SemaError>>()?;
                (texts, false)
            }
            _ => return Err(SemaError::type_error("string or list", args[0].type_name())),
        };

        let model = if let Some(opts_val) = args.get(1) {
            if let Value::Map(opts) = opts_val {
                get_opt_string(opts, "model")
            } else {
                None
            }
        } else {
            None
        };

        let request = EmbedRequest { texts, model };
        let response = with_embedding_provider(|p| {
            p.embed(request).map_err(|e| SemaError::Llm(e.to_string()))
        })?;

        track_usage(&response.usage)?;

        if single {
            let embedding = response.embeddings.into_iter().next().unwrap_or_default();
            let bytes: Vec<u8> = embedding.iter().flat_map(|f| f.to_le_bytes()).collect();
            Ok(Value::bytevector(bytes))
        } else {
            Ok(Value::list(
                response
                    .embeddings
                    .into_iter()
                    .map(|emb| {
                        let bytes: Vec<u8> = emb.iter().flat_map(|f| f.to_le_bytes()).collect();
                        Value::bytevector(bytes)
                    })
                    .collect(),
            ))
        }
    });

    // (llm/similarity vec1 vec2) — cosine similarity
    register_fn(env, "llm/similarity", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("llm/similarity", "2", args.len()));
        }

        let a_is_bv = matches!(&args[0], Value::Bytevector(_));
        let b_is_bv = matches!(&args[1], Value::Bytevector(_));
        let a_is_list = matches!(&args[0], Value::List(_) | Value::Vector(_));
        let b_is_list = matches!(&args[1], Value::List(_) | Value::Vector(_));

        if a_is_bv && b_is_bv {
            let ba = match &args[0] {
                Value::Bytevector(b) => b,
                _ => unreachable!(),
            };
            let bb = match &args[1] {
                Value::Bytevector(b) => b,
                _ => unreachable!(),
            };
            if ba.len() != bb.len() {
                return Err(SemaError::eval(format!(
                    "llm/similarity: bytevectors must have same length ({} vs {})",
                    ba.len(),
                    bb.len()
                )));
            }
            if ba.is_empty() {
                return Err(SemaError::eval("llm/similarity: empty vectors"));
            }
            if ba.len() % 8 != 0 {
                return Err(SemaError::eval(format!(
                    "llm/similarity: bytevector length must be a multiple of 8 (got {})",
                    ba.len()
                )));
            }
            let mut dot = 0.0_f64;
            let mut mag_a = 0.0_f64;
            let mut mag_b = 0.0_f64;
            for (ca, cb) in ba.chunks_exact(8).zip(bb.chunks_exact(8)) {
                let fa = f64::from_le_bytes(ca.try_into().unwrap());
                let fb = f64::from_le_bytes(cb.try_into().unwrap());
                dot += fa * fb;
                mag_a += fa * fa;
                mag_b += fb * fb;
            }
            if mag_a == 0.0 || mag_b == 0.0 {
                Ok(Value::Float(0.0))
            } else {
                Ok(Value::Float(dot / (mag_a.sqrt() * mag_b.sqrt())))
            }
        } else if a_is_list && b_is_list {
            let va = extract_float_vec(&args[0])?;
            let vb = extract_float_vec(&args[1])?;
            if va.len() != vb.len() {
                return Err(SemaError::eval(format!(
                    "llm/similarity: vectors must have same length ({} vs {})",
                    va.len(),
                    vb.len()
                )));
            }
            if va.is_empty() {
                return Err(SemaError::eval("llm/similarity: empty vectors"));
            }
            let mut dot = 0.0_f64;
            let mut mag_a = 0.0_f64;
            let mut mag_b = 0.0_f64;
            for i in 0..va.len() {
                dot += va[i] * vb[i];
                mag_a += va[i] * va[i];
                mag_b += vb[i] * vb[i];
            }
            if mag_a == 0.0 || mag_b == 0.0 {
                Ok(Value::Float(0.0))
            } else {
                Ok(Value::Float(dot / (mag_a.sqrt() * mag_b.sqrt())))
            }
        } else {
            Err(SemaError::eval(
                "llm/similarity: both arguments must be the same type (both bytevectors or both lists). \
                 Use embedding/->list or embedding/list->embedding to convert between formats.",
            ))
        }
    });

    register_fn(env, "embedding/length", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("embedding/length", "1", args.len()));
        }
        match &args[0] {
            Value::Bytevector(bv) => {
                if bv.len() % 8 != 0 {
                    return Err(SemaError::eval(format!(
                        "embedding/length: bytevector length {} is not divisible by 8",
                        bv.len()
                    )));
                }
                Ok(Value::Int((bv.len() / 8) as i64))
            }
            _ => Err(SemaError::type_error("bytevector", args[0].type_name())),
        }
    });

    register_fn(env, "embedding/ref", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("embedding/ref", "2", args.len()));
        }
        match (&args[0], &args[1]) {
            (Value::Bytevector(bv), Value::Int(idx)) => {
                if bv.len() % 8 != 0 {
                    return Err(SemaError::eval(format!(
                        "embedding/ref: bytevector length {} is not divisible by 8",
                        bv.len()
                    )));
                }
                let idx = *idx as usize;
                let num_elements = bv.len() / 8;
                if idx >= num_elements {
                    return Err(SemaError::eval(format!(
                        "embedding/ref: index {} out of bounds (length {})",
                        idx, num_elements
                    )));
                }
                let start = idx * 8;
                let bytes: [u8; 8] = bv[start..start + 8].try_into().unwrap();
                Ok(Value::Float(f64::from_le_bytes(bytes)))
            }
            (Value::Bytevector(_), _) => Err(SemaError::type_error("integer", args[1].type_name())),
            _ => Err(SemaError::type_error("bytevector", args[0].type_name())),
        }
    });

    register_fn(env, "embedding/->list", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("embedding/->list", "1", args.len()));
        }
        match &args[0] {
            Value::Bytevector(bv) => {
                if bv.len() % 8 != 0 {
                    return Err(SemaError::eval(format!(
                        "embedding/->list: bytevector length {} is not divisible by 8",
                        bv.len()
                    )));
                }
                let floats: Vec<Value> = bv
                    .chunks_exact(8)
                    .map(|chunk| {
                        let bytes: [u8; 8] = chunk.try_into().unwrap();
                        Value::Float(f64::from_le_bytes(bytes))
                    })
                    .collect();
                Ok(Value::list(floats))
            }
            _ => Err(SemaError::type_error("bytevector", args[0].type_name())),
        }
    });

    register_fn(env, "embedding/list->embedding", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity(
                "embedding/list->embedding",
                "1",
                args.len(),
            ));
        }
        match &args[0] {
            Value::List(items) => {
                let mut bytes = Vec::with_capacity(items.len() * 8);
                for (i, item) in items.iter().enumerate() {
                    let f = match item {
                        Value::Float(f) => *f,
                        Value::Int(n) => *n as f64,
                        _ => {
                            return Err(SemaError::eval(format!(
                                "embedding/list->embedding: element {} is {}, expected number",
                                i,
                                item.type_name()
                            )));
                        }
                    };
                    bytes.extend_from_slice(&f.to_le_bytes());
                }
                Ok(Value::bytevector(bytes))
            }
            _ => Err(SemaError::type_error("list", args[0].type_name())),
        }
    });

    register_fn(env, "llm/reset-usage", |_args| {
        SESSION_USAGE.with(|u| {
            let mut usage = u.borrow_mut();
            usage.prompt_tokens = 0;
            usage.completion_tokens = 0;
        });
        LAST_USAGE.with(|u| *u.borrow_mut() = None);
        SESSION_COST.with(|sc| *sc.borrow_mut() = 0.0);
        Ok(Value::Nil)
    });

    // Type predicates for LLM types
    register_fn(env, "prompt?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("prompt?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(args[0], Value::Prompt(_))))
    });

    register_fn(env, "message?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("message?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(args[0], Value::Message(_))))
    });

    register_fn(env, "conversation?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("conversation?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(args[0], Value::Conversation(_))))
    });

    register_fn(env, "tool?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("tool?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(args[0], Value::ToolDef(_))))
    });

    register_fn(env, "agent?", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("agent?", "1", args.len()));
        }
        Ok(Value::Bool(matches!(args[0], Value::Agent(_))))
    });

    // Tool accessor functions
    register_fn(env, "tool/name", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("tool/name", "1", args.len()));
        }
        match &args[0] {
            Value::ToolDef(t) => Ok(Value::string(&t.name)),
            _ => Err(SemaError::type_error("tool", args[0].type_name())),
        }
    });

    register_fn(env, "tool/description", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("tool/description", "1", args.len()));
        }
        match &args[0] {
            Value::ToolDef(t) => Ok(Value::string(&t.description)),
            _ => Err(SemaError::type_error("tool", args[0].type_name())),
        }
    });

    register_fn(env, "tool/parameters", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("tool/parameters", "1", args.len()));
        }
        match &args[0] {
            Value::ToolDef(t) => Ok(t.parameters.clone()),
            _ => Err(SemaError::type_error("tool", args[0].type_name())),
        }
    });

    // Agent accessor functions
    register_fn(env, "agent/name", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("agent/name", "1", args.len()));
        }
        match &args[0] {
            Value::Agent(a) => Ok(Value::string(&a.name)),
            _ => Err(SemaError::type_error("agent", args[0].type_name())),
        }
    });

    register_fn(env, "agent/system", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("agent/system", "1", args.len()));
        }
        match &args[0] {
            Value::Agent(a) => Ok(Value::string(&a.system)),
            _ => Err(SemaError::type_error("agent", args[0].type_name())),
        }
    });

    register_fn(env, "agent/tools", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("agent/tools", "1", args.len()));
        }
        match &args[0] {
            Value::Agent(a) => Ok(Value::list(a.tools.clone())),
            _ => Err(SemaError::type_error("agent", args[0].type_name())),
        }
    });

    register_fn(env, "agent/model", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("agent/model", "1", args.len()));
        }
        match &args[0] {
            Value::Agent(a) => Ok(Value::string(&a.model)),
            _ => Err(SemaError::type_error("agent", args[0].type_name())),
        }
    });

    register_fn(env, "agent/max-turns", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("agent/max-turns", "1", args.len()));
        }
        match &args[0] {
            Value::Agent(a) => Ok(Value::Int(a.max_turns as i64)),
            _ => Err(SemaError::type_error("agent", args[0].type_name())),
        }
    });

    // (conversation/add-message conv :role "content")
    register_fn(env, "conversation/add-message", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity(
                "conversation/add-message",
                "3",
                args.len(),
            ));
        }
        let conv = match &args[0] {
            Value::Conversation(c) => c.clone(),
            _ => return Err(SemaError::type_error("conversation", args[0].type_name())),
        };
        let role = match &args[1] {
            Value::Keyword(s) => match resolve(*s).as_str() {
                "system" => Role::System,
                "user" => Role::User,
                "assistant" => Role::Assistant,
                "tool" => Role::Tool,
                other => {
                    return Err(SemaError::eval(format!(
                        "conversation/add-message: unknown role '{other}'"
                    )))
                }
            },
            _ => return Err(SemaError::type_error("keyword", args[1].type_name())),
        };
        let content = match &args[2] {
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };
        let mut new_messages = conv.messages.clone();
        new_messages.push(Message { role, content });
        Ok(Value::Conversation(Rc::new(Conversation {
            messages: new_messages,
            model: conv.model.clone(),
            metadata: conv.metadata.clone(),
        })))
    });

    // (conversation/model conv) — get the model name
    register_fn(env, "conversation/model", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("conversation/model", "1", args.len()));
        }
        match &args[0] {
            Value::Conversation(c) => Ok(Value::string(&c.model)),
            _ => Err(SemaError::type_error("conversation", args[0].type_name())),
        }
    });

    // (llm/set-default :provider-name) — switch the active provider
    register_fn(env, "llm/set-default", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("llm/set-default", "1", args.len()));
        }
        let name = args[0]
            .as_keyword()
            .or_else(|| args[0].as_str().map(|s| s.to_string()))
            .ok_or_else(|| SemaError::type_error("keyword or string", args[0].type_name()))?;
        PROVIDER_REGISTRY.with(|reg| {
            let mut reg = reg.borrow_mut();
            if reg.get(&name).is_some() {
                reg.set_default(&name);
                Ok(Value::keyword(&name))
            } else {
                Err(SemaError::Llm(format!("provider not configured: {name}")))
            }
        })
    });

    // (llm/list-providers) — list configured providers
    register_fn(env, "llm/list-providers", |_args| {
        PROVIDER_REGISTRY.with(|reg| {
            let reg = reg.borrow();
            let names: Vec<Value> = reg
                .provider_names()
                .into_iter()
                .map(|n| Value::keyword(&n))
                .collect();
            Ok(Value::list(names))
        })
    });

    // (llm/current-provider) — query active provider/model
    register_fn(env, "llm/current-provider", |_args| {
        PROVIDER_REGISTRY.with(|reg| {
            let reg = reg.borrow();
            match reg.default_provider() {
                Some(p) => {
                    let mut map = BTreeMap::new();
                    map.insert(Value::keyword("name"), Value::keyword(p.name()));
                    map.insert(Value::keyword("model"), Value::string(p.default_model()));
                    Ok(Value::Map(Rc::new(map)))
                }
                None => Ok(Value::Nil),
            }
        })
    });

    // (llm/pricing-status)
    register_fn(env, "llm/pricing-status", |_args| {
        let (source, updated_at) = pricing::pricing_status();
        let mut map = std::collections::BTreeMap::new();
        map.insert(Value::keyword("source"), Value::symbol(source));
        if let Some(date) = updated_at {
            map.insert(Value::keyword("updated-at"), Value::string(&date));
        }
        Ok(Value::Map(Rc::new(map)))
    });

    // (llm/set-budget max-cost-usd) — set a budget limit
    register_fn(env, "llm/set-budget", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("llm/set-budget", "1", args.len()));
        }
        let max_cost = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        crate::builtins::set_budget(max_cost);
        Ok(Value::Nil)
    });

    // (llm/clear-budget) — clear the budget limit
    register_fn(env, "llm/clear-budget", |_args| {
        crate::builtins::clear_budget();
        Ok(Value::Nil)
    });

    // (llm/budget-remaining) — query budget status
    register_fn(env, "llm/budget-remaining", |_args| {
        BUDGET_LIMIT.with(|limit| {
            let limit = limit.borrow();
            match *limit {
                Some(max_cost) => {
                    let spent = BUDGET_SPENT.with(|s| *s.borrow());
                    let mut map = BTreeMap::new();
                    map.insert(Value::keyword("limit"), Value::Float(max_cost));
                    map.insert(Value::keyword("spent"), Value::Float(spent));
                    map.insert(Value::keyword("remaining"), Value::Float(max_cost - spent));
                    Ok(Value::Map(Rc::new(map)))
                }
                None => Ok(Value::Nil),
            }
        })
    });
}

fn extract_float_vec(val: &Value) -> Result<Vec<f64>, SemaError> {
    match val {
        Value::List(l) | Value::Vector(l) => l
            .iter()
            .map(|v| {
                v.as_float()
                    .ok_or_else(|| SemaError::type_error("number", v.type_name()))
            })
            .collect(),
        _ => Err(SemaError::type_error("list of numbers", val.type_name())),
    }
}

fn complete_with_prompt(prompt: &Prompt, opts: Option<&Value>) -> Result<Value, SemaError> {
    let messages: Vec<ChatMessage> = prompt
        .messages
        .iter()
        .map(|m| ChatMessage {
            role: m.role.to_string(),
            content: m.content.clone(),
        })
        .collect();

    let mut model = String::new();
    let mut max_tokens = None;
    let mut temperature = None;

    if let Some(Value::Map(opts)) = opts {
        model = get_opt_string(opts, "model").unwrap_or_default();
        max_tokens = get_opt_u32(opts, "max-tokens");
        temperature = get_opt_f64(opts, "temperature");
    }

    let mut request = ChatRequest::new(model, messages);
    request.max_tokens = max_tokens.or(Some(4096));
    request.temperature = temperature;

    let response = do_complete(request)?;
    track_usage(&response.usage)?;
    Ok(Value::String(Rc::new(response.content)))
}

fn extract_messages(val: &Value) -> Result<Vec<ChatMessage>, SemaError> {
    match val {
        Value::List(items) | Value::Vector(items) => {
            let mut messages = Vec::new();
            for item in items.iter() {
                match item {
                    Value::Message(m) => {
                        messages.push(ChatMessage {
                            role: m.role.to_string(),
                            content: m.content.clone(),
                        });
                    }
                    _ => return Err(SemaError::type_error("message", item.type_name())),
                }
            }
            Ok(messages)
        }
        Value::Prompt(p) => Ok(p
            .messages
            .iter()
            .map(|m| ChatMessage {
                role: m.role.to_string(),
                content: m.content.clone(),
            })
            .collect()),
        _ => Err(SemaError::type_error(
            "list of messages or prompt",
            val.type_name(),
        )),
    }
}

fn format_schema(val: &Value) -> String {
    match val {
        Value::Map(map) => {
            let mut fields = Vec::new();
            for (k, v) in map.iter() {
                let key = match k {
                    Value::Keyword(s) => resolve(*s),
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                };
                let type_str = match v {
                    Value::Map(inner) => {
                        if let Some(t) = inner.get(&Value::keyword("type")) {
                            match t {
                                Value::Keyword(s) => resolve(*s),
                                Value::String(s) => s.to_string(),
                                other => other.to_string(),
                            }
                        } else {
                            "any".to_string()
                        }
                    }
                    _ => "any".to_string(),
                };
                fields.push(format!("  \"{key}\": <{type_str}>"));
            }
            format!("{{\n{}\n}}", fields.join(",\n"))
        }
        _ => val.to_string(),
    }
}

/// Validate that an extracted Sema value matches the expected schema.
/// The schema is a map of keyword keys to field descriptors (maps with `:type`).
/// Returns Ok(()) if valid, or Err with a description of mismatches.
fn validate_extraction(result: &Value, schema: &Value) -> Result<(), String> {
    let schema_map = match schema {
        Value::Map(m) => m,
        _ => return Ok(()),
    };
    let result_map = match result {
        Value::Map(m) => m,
        _ => return Err(format!("expected map result, got {}", result.type_name())),
    };

    let mut errors = Vec::new();

    for (key, field_spec) in schema_map.iter() {
        let key_name = match key {
            Value::Keyword(s) => resolve(*s),
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };

        let result_val = result_map.get(key);
        match result_val {
            None => {
                errors.push(format!("missing key: {key_name}"));
            }
            Some(val) => {
                if let Value::Map(spec) = field_spec {
                    if let Some(type_val) = spec.get(&Value::keyword("type")) {
                        let type_name = match type_val {
                            Value::Keyword(s) => resolve(*s),
                            Value::String(s) => s.to_string(),
                            other => other.to_string(),
                        };
                        let ok = match type_name.as_str() {
                            "string" => matches!(val, Value::String(_)),
                            "number" => matches!(val, Value::Int(_) | Value::Float(_)),
                            "boolean" | "bool" => matches!(val, Value::Bool(_)),
                            "list" | "array" => {
                                matches!(val, Value::List(_) | Value::Vector(_))
                            }
                            _ => true,
                        };
                        if !ok {
                            errors.push(format!(
                                "key {key_name}: expected {type_name}, got {}",
                                val.type_name()
                            ));
                        }
                    }
                }
            }
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors.join("; "))
    }
}

/// Send a ChatRequest via the default provider with model fallback and rate-limit retry.
fn do_complete(mut request: ChatRequest) -> Result<ChatResponse, SemaError> {
    with_provider(|p| {
        if request.model.is_empty() {
            request.model = p.default_model().to_string();
        }
        let mut retries = 0;
        let max_retries = 3;
        loop {
            match p.complete(request.clone()) {
                Ok(resp) => return Ok(resp),
                Err(crate::types::LlmError::RateLimited { retry_after_ms }) => {
                    retries += 1;
                    if retries > max_retries {
                        return Err(SemaError::Llm("rate limited after 3 retries".to_string()));
                    }
                    let wait = std::cmp::min(retry_after_ms, 30000);
                    std::thread::sleep(std::time::Duration::from_millis(wait));
                }
                Err(e) => return Err(SemaError::Llm(e.to_string())),
            }
        }
    })
}

/// Build ToolSchema list from Sema ToolDef values.
fn build_tool_schemas(tools: &[Value]) -> Result<Vec<ToolSchema>, SemaError> {
    let mut schemas = Vec::new();
    for tool in tools {
        match tool {
            Value::ToolDef(td) => {
                let params_json = sema_value_to_json_schema(&td.parameters);
                schemas.push(ToolSchema {
                    name: td.name.clone(),
                    description: td.description.clone(),
                    parameters: params_json,
                });
            }
            _ => return Err(SemaError::type_error("tool", tool.type_name())),
        }
    }
    Ok(schemas)
}

/// Convert a Sema schema map into a JSON Schema object for the LLM API.
fn sema_value_to_json_schema(val: &Value) -> serde_json::Value {
    match val {
        Value::Map(map) => {
            let mut properties = serde_json::Map::new();
            let mut required = Vec::new();
            for (k, v) in map.iter() {
                let key = match k {
                    Value::Keyword(s) => resolve(*s),
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                };
                let prop = match v {
                    Value::Map(inner) => {
                        let mut prop_obj = serde_json::Map::new();
                        if let Some(t) = inner.get(&Value::keyword("type")) {
                            let type_str = match t {
                                Value::Keyword(s) => resolve(*s),
                                Value::String(s) => s.to_string(),
                                _ => "string".to_string(),
                            };
                            prop_obj
                                .insert("type".to_string(), serde_json::Value::String(type_str));
                        }
                        if let Some(d) = inner.get(&Value::keyword("description")) {
                            let desc = match d {
                                Value::String(s) => s.to_string(),
                                other => other.to_string(),
                            };
                            prop_obj
                                .insert("description".to_string(), serde_json::Value::String(desc));
                        }
                        if let Some(e) = inner.get(&Value::keyword("enum")) {
                            if let Value::List(items) | Value::Vector(items) = e {
                                let vals: Vec<serde_json::Value> = items
                                    .iter()
                                    .map(|v| match v {
                                        Value::String(s) => {
                                            serde_json::Value::String(s.to_string())
                                        }
                                        Value::Keyword(s) => serde_json::Value::String(resolve(*s)),
                                        _ => serde_json::Value::String(v.to_string()),
                                    })
                                    .collect();
                                prop_obj.insert("enum".to_string(), serde_json::Value::Array(vals));
                            }
                        }
                        // Mark as required unless :optional #t
                        let optional = inner
                            .get(&Value::keyword("optional"))
                            .map(|v| v.is_truthy())
                            .unwrap_or(false);
                        if !optional {
                            required.push(serde_json::Value::String(key.clone()));
                        }
                        serde_json::Value::Object(prop_obj)
                    }
                    _ => {
                        required.push(serde_json::Value::String(key.clone()));
                        serde_json::json!({"type": "string"})
                    }
                };
                properties.insert(key, prop);
            }
            serde_json::json!({
                "type": "object",
                "properties": properties,
                "required": required
            })
        }
        _ => serde_json::json!({"type": "object", "properties": {}}),
    }
}

fn sema_list_to_chat_messages(val: &Value) -> Result<Vec<ChatMessage>, SemaError> {
    let items = match val {
        Value::List(l) => l.as_ref().clone(),
        Value::Vector(v) => v.as_ref().clone(),
        Value::Nil => return Ok(Vec::new()),
        _ => {
            return Err(SemaError::type_error(
                "list of message maps",
                val.type_name(),
            ))
        }
    };
    let mut messages = Vec::with_capacity(items.len());
    for item in &items {
        if let Value::Map(m) = item {
            let role = m
                .get(&Value::keyword("role"))
                .map(|v| match v {
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                })
                .unwrap_or_default();
            let content = m
                .get(&Value::keyword("content"))
                .map(|v| match v {
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                })
                .unwrap_or_default();
            messages.push(ChatMessage { role, content });
        } else {
            return Err(SemaError::type_error("message map", item.type_name()));
        }
    }
    Ok(messages)
}

fn chat_messages_to_sema_list(messages: &[ChatMessage]) -> Value {
    let items: Vec<Value> = messages
        .iter()
        .map(|msg| {
            let mut map = BTreeMap::new();
            map.insert(
                Value::keyword("role"),
                Value::String(Rc::new(msg.role.clone())),
            );
            map.insert(
                Value::keyword("content"),
                Value::String(Rc::new(msg.content.clone())),
            );
            Value::Map(Rc::new(map))
        })
        .collect();
    Value::List(Rc::new(items))
}

/// The tool execution loop: send -> check for tool_calls -> execute -> send results -> repeat.
fn run_tool_loop(
    ctx: &EvalContext,
    initial_messages: Vec<ChatMessage>,
    model: String,
    max_tokens: Option<u32>,
    temperature: Option<f64>,
    system: Option<String>,
    tools: &[Value],
    tool_schemas: &[ToolSchema],
    max_rounds: usize,
    on_tool_call: Option<&Value>,
) -> Result<(String, Vec<ChatMessage>), SemaError> {
    let mut messages = initial_messages;
    let mut last_content = String::new();

    for _round in 0..max_rounds {
        let mut request = ChatRequest::new(model.clone(), messages.clone());
        request.max_tokens = max_tokens.or(Some(4096));
        request.temperature = temperature;
        request.system = system.clone();
        request.tools = tool_schemas.to_vec();

        let response = do_complete(request)?;
        track_usage(&response.usage)?;
        last_content = response.content.clone();

        if response.tool_calls.is_empty() {
            // Push final assistant message onto history
            if !last_content.is_empty() {
                messages.push(ChatMessage {
                    role: "assistant".to_string(),
                    content: last_content.clone(),
                });
            }
            return Ok((last_content, messages));
        }

        // Add assistant message (with content if any)
        if !response.content.is_empty() {
            messages.push(ChatMessage {
                role: "assistant".to_string(),
                content: response.content.clone(),
            });
        }

        // Execute each tool call and add results
        for tc in &response.tool_calls {
            // Build args map for callback
            let args_value = json_to_sema_value(&tc.arguments);

            // Fire "start" event
            if let Some(callback) = on_tool_call {
                let mut event_map = BTreeMap::new();
                event_map.insert(
                    Value::keyword("event"),
                    Value::String(Rc::new("start".to_string())),
                );
                event_map.insert(
                    Value::keyword("tool"),
                    Value::String(Rc::new(tc.name.clone())),
                );
                event_map.insert(Value::keyword("args"), args_value.clone());
                let _ = call_value_fn(ctx, callback, &[Value::Map(Rc::new(event_map))]);
            }

            let start_time = std::time::Instant::now();
            let result = execute_tool_call(ctx, tools, &tc.name, &tc.arguments)?;
            let duration_ms = start_time.elapsed().as_millis() as i64;

            // Fire "end" event
            if let Some(callback) = on_tool_call {
                let mut event_map = BTreeMap::new();
                event_map.insert(
                    Value::keyword("event"),
                    Value::String(Rc::new("end".to_string())),
                );
                event_map.insert(
                    Value::keyword("tool"),
                    Value::String(Rc::new(tc.name.clone())),
                );
                event_map.insert(Value::keyword("args"), args_value);
                // Truncate result for the callback to avoid huge payloads
                let result_preview = if result.len() > 200 {
                    format!("{}...", &result[..200])
                } else {
                    result.clone()
                };
                event_map.insert(
                    Value::keyword("result"),
                    Value::String(Rc::new(result_preview)),
                );
                event_map.insert(Value::keyword("duration-ms"), Value::Int(duration_ms));
                let _ = call_value_fn(ctx, callback, &[Value::Map(Rc::new(event_map))]);
            }

            messages.push(ChatMessage {
                role: "user".to_string(),
                content: format!("[Tool result for {}]: {}", tc.name, result),
            });
        }
    }

    // Push final assistant message if we exhausted rounds
    if !last_content.is_empty() {
        messages.push(ChatMessage {
            role: "assistant".to_string(),
            content: last_content.clone(),
        });
    }
    Ok((last_content, messages))
}

/// Execute a tool call by finding the handler and invoking it.
fn execute_tool_call(
    ctx: &EvalContext,
    tools: &[Value],
    name: &str,
    arguments: &serde_json::Value,
) -> Result<String, SemaError> {
    // Find the tool definition
    let tool_def = tools
        .iter()
        .find_map(|t| match t {
            Value::ToolDef(td) if td.name == name => Some(td.clone()),
            _ => None,
        })
        .ok_or_else(|| SemaError::Llm(format!("tool not found: {name}")))?;

    // Convert JSON arguments to Sema values and call the handler
    let sema_args = json_args_to_sema(&tool_def.parameters, arguments, &tool_def.handler);
    let result = call_value_fn(ctx, &tool_def.handler, &sema_args)?;

    // Convert result to string for sending back to LLM
    match &result {
        Value::String(s) => Ok(s.to_string()),
        Value::Map(_) | Value::List(_) | Value::Vector(_) => {
            // JSON-encode complex results
            let json =
                crate::builtins::sema_value_to_json(&result).unwrap_or(serde_json::Value::Null);
            Ok(serde_json::to_string(&json).unwrap_or_else(|_| result.to_string()))
        }
        other => Ok(other.to_string()),
    }
}

/// Convert JSON arguments into a list of Sema values based on the parameter schema order.
/// When the handler is a lambda, uses its param names (declaration order) instead of
/// BTreeMap key order (alphabetical), fixing argument ordering mismatches.
fn json_args_to_sema(params: &Value, arguments: &serde_json::Value, handler: &Value) -> Vec<Value> {
    if let serde_json::Value::Object(json_obj) = arguments {
        // Prefer lambda param names (preserves declaration order) over BTreeMap keys
        if let Value::Lambda(lambda) = handler {
            return lambda
                .params
                .iter()
                .map(|name| {
                    json_obj
                        .get(&resolve(*name))
                        .map(json_to_sema_value)
                        .unwrap_or(Value::Nil)
                })
                .collect();
        }
        // Fallback: use param map keys (BTreeMap order — alphabetical)
        if let Value::Map(param_map) = params {
            return param_map
                .keys()
                .map(|k| {
                    let key_str = match k {
                        Value::Keyword(s) => resolve(*s),
                        Value::String(s) => s.to_string(),
                        other => other.to_string(),
                    };
                    json_obj
                        .get(&key_str)
                        .map(json_to_sema_value)
                        .unwrap_or(Value::Nil)
                })
                .collect();
        }
    }
    vec![json_to_sema_value(arguments)]
}

/// Call a Sema value as a function (lambda or native).
fn call_value_fn(ctx: &EvalContext, func: &Value, args: &[Value]) -> Result<Value, SemaError> {
    match func {
        Value::NativeFn(native) => (native.func)(ctx, args),
        Value::Lambda(lambda) => {
            let env = Env::with_parent(Rc::new(lambda.env.clone()));
            // Bind params
            if let Some(ref rest) = lambda.rest_param {
                if args.len() < lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda
                            .name
                            .map(resolve)
                            .unwrap_or_else(|| "lambda".to_string()),
                        format!("{}+", lambda.params.len()),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    env.set(*param, arg.clone());
                }
                let rest_args = args[lambda.params.len()..].to_vec();
                env.set(*rest, Value::list(rest_args));
            } else {
                if args.len() != lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda
                            .name
                            .map(resolve)
                            .unwrap_or_else(|| "lambda".to_string()),
                        lambda.params.len().to_string(),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    env.set(*param, arg.clone());
                }
            }
            // Self-reference
            if let Some(name) = lambda.name {
                env.set(
                    name,
                    Value::Lambda(Rc::new(sema_core::Lambda {
                        params: lambda.params.clone(),
                        rest_param: lambda.rest_param,
                        body: lambda.body.clone(),
                        env: lambda.env.clone(),
                        name: lambda.name,
                    })),
                );
            }
            // Eval body using the full evaluator (supports let, if, cond, etc.)
            let mut result = Value::Nil;
            for expr in &lambda.body {
                result = full_eval(ctx, expr, &env)?;
            }
            Ok(result)
        }
        _ => Err(SemaError::eval(format!(
            "not callable: {} ({})",
            func,
            func.type_name()
        ))),
    }
}

/// Minimal evaluator for use within LLM builtins (avoids circular dep with sema-eval).
fn simple_eval(ctx: &EvalContext, expr: &Value, env: &Env) -> Result<Value, SemaError> {
    match expr {
        Value::Symbol(spur) => env
            .get(*spur)
            .ok_or_else(|| SemaError::Unbound(resolve(*spur))),
        Value::List(items) if !items.is_empty() => {
            let func_val = simple_eval(ctx, &items[0], env)?;
            let mut args = Vec::new();
            for arg in &items[1..] {
                args.push(simple_eval(ctx, arg, env)?);
            }
            call_value_fn(ctx, &func_val, &args)
        }
        _ => Ok(expr.clone()),
    }
}

pub fn sema_value_to_json(val: &Value) -> Result<serde_json::Value, SemaError> {
    match val {
        Value::Nil => Ok(serde_json::Value::Null),
        Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        Value::Int(n) => Ok(serde_json::Value::Number((*n).into())),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .ok_or_else(|| SemaError::eval("cannot encode NaN/Infinity as JSON")),
        Value::String(s) => Ok(serde_json::Value::String(s.to_string())),
        Value::Keyword(s) => Ok(serde_json::Value::String(resolve(*s))),
        Value::Symbol(s) => Ok(serde_json::Value::String(resolve(*s))),
        Value::List(items) | Value::Vector(items) => {
            let arr: Result<Vec<_>, _> = items.iter().map(sema_value_to_json).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        Value::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                let key = match k {
                    Value::String(s) => s.to_string(),
                    Value::Keyword(s) => resolve(*s),
                    other => other.to_string(),
                };
                obj.insert(key, sema_value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        Value::HashMap(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                let key = match k {
                    Value::String(s) => s.to_string(),
                    Value::Keyword(s) => resolve(*s),
                    other => other.to_string(),
                };
                obj.insert(key, sema_value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        _ => Err(SemaError::eval(format!(
            "cannot encode {} as JSON",
            val.type_name()
        ))),
    }
}

pub fn json_to_sema_value(json: &serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(b) => Value::Bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Nil
            }
        }
        serde_json::Value::String(s) => Value::String(Rc::new(s.clone())),
        serde_json::Value::Array(arr) => Value::list(arr.iter().map(json_to_sema_value).collect()),
        serde_json::Value::Object(obj) => {
            let mut map = BTreeMap::new();
            for (k, v) in obj {
                map.insert(Value::keyword(k), json_to_sema_value(v));
            }
            Value::Map(Rc::new(map))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sema_core::{intern, Lambda};
    use serde_json::json;

    fn make_lambda(params: &[&str]) -> Value {
        Value::Lambda(Rc::new(Lambda {
            params: params.iter().map(|s| intern(s)).collect(),
            rest_param: None,
            body: vec![Value::Nil],
            env: Env::new(),
            name: None,
        }))
    }

    fn make_param_map(keys: &[&str]) -> Value {
        let mut map = BTreeMap::new();
        for k in keys {
            map.insert(Value::keyword(*k), Value::Map(Rc::new(BTreeMap::new())));
        }
        Value::Map(Rc::new(map))
    }

    // -- json_args_to_sema tests --

    #[test]
    fn test_json_args_to_sema_lambda_declaration_order() {
        // Params declared as (path, content) — but alphabetically content < path.
        // The lambda path must use declaration order, not alphabetical.
        let handler = make_lambda(&["path", "content"]);
        let params = make_param_map(&["path", "content"]);
        let args = json!({"path": "/tmp/test.txt", "content": "hello world"});

        let result = json_args_to_sema(&params, &args, &handler);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], Value::string("/tmp/test.txt"));
        assert_eq!(result[1], Value::string("hello world"));
    }

    #[test]
    fn test_json_args_to_sema_many_params_declaration_order() {
        // 4 params where alphabetical (a, b, c, d) != declaration order (d, b, a, c)
        let handler = make_lambda(&["delta", "bravo", "alpha", "charlie"]);
        let params = make_param_map(&["delta", "bravo", "alpha", "charlie"]);
        let args = json!({
            "alpha": "A",
            "bravo": "B",
            "charlie": "C",
            "delta": "D"
        });

        let result = json_args_to_sema(&params, &args, &handler);

        assert_eq!(result.len(), 4);
        assert_eq!(result[0], Value::string("D")); // delta first (declaration order)
        assert_eq!(result[1], Value::string("B")); // bravo second
        assert_eq!(result[2], Value::string("A")); // alpha third
        assert_eq!(result[3], Value::string("C")); // charlie fourth
    }

    #[test]
    fn test_json_args_to_sema_missing_arg_yields_nil() {
        let handler = make_lambda(&["path", "content"]);
        let params = make_param_map(&["path", "content"]);
        let args = json!({"path": "/tmp/test.txt"});

        let result = json_args_to_sema(&params, &args, &handler);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0], Value::string("/tmp/test.txt"));
        assert_eq!(result[1], Value::Nil);
    }

    #[test]
    fn test_json_args_to_sema_non_lambda_falls_back_to_btreemap() {
        // With a NativeFn handler, should fall back to param_map key order (alphabetical).
        let handler = Value::NativeFn(Rc::new(NativeFn::simple("test", |_args| Ok(Value::Nil))));
        let params = make_param_map(&["zebra", "apple"]);
        let args = json!({"zebra": "Z", "apple": "A"});

        let result = json_args_to_sema(&params, &args, &handler);

        // BTreeMap sorts alphabetically: apple < zebra
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], Value::string("A")); // apple first (alphabetical)
        assert_eq!(result[1], Value::string("Z")); // zebra second
    }

    #[test]
    fn test_json_args_to_sema_non_object_json() {
        let handler = make_lambda(&["x"]);
        let params = make_param_map(&["x"]);
        let args = json!("just a string");

        let result = json_args_to_sema(&params, &args, &handler);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0], Value::string("just a string"));
    }

    #[test]
    fn test_json_args_to_sema_mixed_types() {
        let handler = make_lambda(&["name", "age", "active"]);
        let params = make_param_map(&["name", "age", "active"]);
        let args = json!({"name": "Alice", "age": 30, "active": true});

        let result = json_args_to_sema(&params, &args, &handler);

        // Declaration order: name, age, active
        assert_eq!(result[0], Value::string("Alice"));
        assert_eq!(result[1], Value::Int(30));
        assert_eq!(result[2], Value::Bool(true));
    }

    // -- execute_tool_call tests --

    #[test]
    fn test_execute_tool_call_arg_ordering() {
        // Tool with params where alphabetical != declaration order.
        // Handler returns "path={path}, content={content}" to verify ordering.
        let handler = Value::Lambda(Rc::new(Lambda {
            params: vec![intern("path"), intern("content")],
            rest_param: None,
            body: vec![
                // Body: (string-append path "|" content)
                // We can't call string-append without a full evaluator,
                // so just return the first param to verify it's the path.
                Value::Symbol(intern("path")),
            ],
            env: Env::new(),
            name: Some(intern("write-file-handler")),
        }));

        let tool = Value::ToolDef(Rc::new(sema_core::ToolDefinition {
            name: "write-file".to_string(),
            description: "Write content to a file".to_string(),
            parameters: make_param_map(&["path", "content"]),
            handler,
        }));

        let args = json!({"path": "/tmp/test.txt", "content": "file body here"});
        let ctx = EvalContext::new();
        let result = execute_tool_call(&ctx, &[tool], "write-file", &args).unwrap();

        // If ordering is wrong (alphabetical), content would be bound to `path`
        // and we'd get "file body here" instead of the actual path.
        assert_eq!(result, "/tmp/test.txt");
    }

    #[test]
    fn test_execute_tool_call_reverse_alpha_order() {
        // Declare params (z_last, a_first) — exact reverse of alphabetical.
        let handler = Value::Lambda(Rc::new(Lambda {
            params: vec![intern("z_last"), intern("a_first")],
            rest_param: None,
            body: vec![Value::Symbol(intern("z_last"))],
            env: Env::new(),
            name: Some(intern("test-handler")),
        }));

        let tool = Value::ToolDef(Rc::new(sema_core::ToolDefinition {
            name: "test-tool".to_string(),
            description: "test".to_string(),
            parameters: make_param_map(&["z_last", "a_first"]),
            handler,
        }));

        let args = json!({"z_last": "ZLAST", "a_first": "AFIRST"});
        let ctx = EvalContext::new();
        let result = execute_tool_call(&ctx, &[tool], "test-tool", &args).unwrap();

        // Lambda body returns z_last — must be "ZLAST", not "AFIRST"
        assert_eq!(result, "ZLAST");
    }
}
