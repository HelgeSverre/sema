use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{Conversation, Env, Message, NativeFn, Prompt, Role, SemaError, Value};

use crate::anthropic::AnthropicProvider;
use crate::openai::OpenAiProvider;
use crate::provider::{LlmProvider, ProviderRegistry};
use crate::types::{ChatMessage, ChatRequest, ChatResponse, ToolSchema, Usage};

/// Type for a full evaluator callback: (expr, env) -> Result<Value, SemaError>
pub type EvalCallback = Box<dyn Fn(&Value, &Env) -> Result<Value, SemaError>>;

thread_local! {
    static PROVIDER_REGISTRY: RefCell<ProviderRegistry> = RefCell::new(ProviderRegistry::new());
    static SESSION_USAGE: RefCell<Usage> = RefCell::new(Usage::default());
    static LAST_USAGE: RefCell<Option<Usage>> = RefCell::new(None);
    static EVAL_FN: RefCell<Option<EvalCallback>> = RefCell::new(None);
}

/// Register a full evaluator for use by tool handlers and other LLM builtins.
pub fn set_eval_callback(f: impl Fn(&Value, &Env) -> Result<Value, SemaError> + 'static) {
    EVAL_FN.with(|eval| {
        *eval.borrow_mut() = Some(Box::new(f));
    });
}

/// Evaluate an expression using the registered full evaluator.
fn full_eval(expr: &Value, env: &Env) -> Result<Value, SemaError> {
    EVAL_FN.with(|eval_fn| {
        let eval_fn = eval_fn.borrow();
        match &*eval_fn {
            Some(f) => f(expr, env),
            None => simple_eval(expr, env),
        }
    })
}

fn register_fn(
    env: &Env,
    name: &str,
    f: impl Fn(&[Value]) -> Result<Value, SemaError> + 'static,
) {
    env.set(
        name.to_string(),
        Value::NativeFn(Rc::new(NativeFn {
            name: name.to_string(),
            func: Box::new(f),
        })),
    );
}

fn with_provider<F, R>(f: F) -> Result<R, SemaError>
where
    F: FnOnce(&dyn LlmProvider) -> Result<R, SemaError>,
{
    PROVIDER_REGISTRY.with(|reg| {
        let reg = reg.borrow();
        let provider = reg
            .default_provider()
            .ok_or_else(|| SemaError::Llm("no LLM provider configured. Use (llm/configure :anthropic {:api-key ...}) first".to_string()))?;
        f(provider)
    })
}

fn track_usage(usage: &Usage) {
    LAST_USAGE.with(|u| *u.borrow_mut() = Some(usage.clone()));
    SESSION_USAGE.with(|u| {
        let mut session = u.borrow_mut();
        session.prompt_tokens += usage.prompt_tokens;
        session.completion_tokens += usage.completion_tokens;
    });
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

pub fn register_llm_builtins(env: &Env) {
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

        let api_key = get_opt_string(&opts, "api-key")
            .ok_or_else(|| SemaError::Llm("missing :api-key".to_string()))?;

        PROVIDER_REGISTRY.with(|reg| {
            let mut reg = reg.borrow_mut();
            match provider_name {
                "anthropic" => {
                    let model = get_opt_string(&opts, "default-model");
                    let provider = AnthropicProvider::new(api_key, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("anthropic");
                }
                "openai" => {
                    let base_url = get_opt_string(&opts, "base-url");
                    let model = get_opt_string(&opts, "default-model");
                    let provider = OpenAiProvider::new(api_key, base_url, model)
                        .map_err(|e| SemaError::Llm(e.to_string()))?;
                    reg.register(Box::new(provider));
                    reg.set_default("openai");
                }
                other => {
                    return Err(SemaError::Llm(format!("unknown provider: {other}")));
                }
            }
            Ok(Value::Nil)
        })
    });

    // Auto-configure from environment variables
    register_fn(env, "llm/auto-configure", |_args| {
        PROVIDER_REGISTRY.with(|reg| {
            let mut reg = reg.borrow_mut();
            // Try Anthropic first
            if let Ok(key) = std::env::var("ANTHROPIC_API_KEY") {
                let provider = AnthropicProvider::new(key, None)
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                reg.register(Box::new(provider));
                reg.set_default("anthropic");
                return Ok(Value::keyword("anthropic"));
            }
            // Try OpenAI
            if let Ok(key) = std::env::var("OPENAI_API_KEY") {
                let provider = OpenAiProvider::new(key, None, None)
                    .map_err(|e| SemaError::Llm(e.to_string()))?;
                reg.register(Box::new(provider));
                reg.set_default("openai");
                return Ok(Value::keyword("openai"));
            }
            Ok(Value::Nil)
        })
    });

    // (llm/complete "prompt text" {:model "..." :max-tokens 200 :temperature 0.5})
    register_fn(env, "llm/complete", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("llm/complete", "1-2", args.len()));
        }
        let prompt_text = match &args[0] {
            Value::String(s) => s.to_string(),
            Value::Prompt(p) => {
                // Use the prompt's messages
                return complete_with_prompt(p, args.get(1));
            }
            _ => return Err(SemaError::type_error("string or prompt", args[0].type_name())),
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

        let response = with_provider(|p| {
            if request.model.is_empty() {
                let mut req = request.clone();
                req.model = p.default_model().to_string();
                p.complete(req).map_err(|e| SemaError::Llm(e.to_string()))
            } else {
                p.complete(request.clone())
                    .map_err(|e| SemaError::Llm(e.to_string()))
            }
        })?;

        track_usage(&response.usage);
        Ok(Value::String(Rc::new(response.content)))
    });

    // (llm/chat messages {:model "..." :tools [...] :tool-mode :auto ...})
    register_fn(env, "llm/chat", |args| {
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
                        tool_mode = s.to_string();
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
            track_usage(&response.usage);
            Ok(Value::String(Rc::new(response.content)))
        } else {
            // Chat with tool execution loop
            let tool_schemas = build_tool_schemas(&tools)?;
            let result = run_tool_loop(
                messages, model, max_tokens, temperature, system,
                &tools, &tool_schemas, max_tool_rounds,
            )?;
            Ok(Value::String(Rc::new(result)))
        }
    });

    // (llm/send prompt {:model "..." ...})
    register_fn(env, "llm/send", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("llm/send", "1-2", args.len()));
        }
        let prompt = match &args[0] {
            Value::Prompt(p) => p.clone(),
            _ => return Err(SemaError::type_error("prompt", args[0].type_name())),
        };
        complete_with_prompt(&prompt, args.get(1))
    });

    // (llm/extract schema text {:model "..."})
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
        if let Some(opts_val) = args.get(2) {
            if let Value::Map(opts) = opts_val {
                model = get_opt_string(opts, "model").unwrap_or_default();
            }
        }

        let mut request = ChatRequest::new(model, messages);
        request.system = Some(system);

        let response = with_provider(|p| {
            if request.model.is_empty() {
                let mut req = request.clone();
                req.model = p.default_model().to_string();
                p.complete(req).map_err(|e| SemaError::Llm(e.to_string()))
            } else {
                p.complete(request.clone())
                    .map_err(|e| SemaError::Llm(e.to_string()))
            }
        })?;

        track_usage(&response.usage);

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
        let json: serde_json::Value = serde_json::from_str(json_str)
            .map_err(|e| SemaError::Llm(format!("failed to parse LLM JSON response: {e}\nResponse was: {content}")))?;
        Ok(json_to_sema_value(&json))
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
                Value::Keyword(s) => s.to_string(),
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

        let response = with_provider(|p| {
            if request.model.is_empty() {
                let mut req = request.clone();
                req.model = p.default_model().to_string();
                p.complete(req).map_err(|e| SemaError::Llm(e.to_string()))
            } else {
                p.complete(request.clone())
                    .map_err(|e| SemaError::Llm(e.to_string()))
            }
        })?;

        track_usage(&response.usage);

        let category = response.content.trim().to_string();
        // Return as keyword if it was in the original list as keyword
        if categories.iter().any(|c| matches!(c, Value::Keyword(s) if s.as_ref() == &category)) {
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
                        if key.as_ref() != "model" {
                            metadata.insert(
                                key.to_string(),
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

    // (conversation/say conv "message")
    register_fn(env, "conversation/say", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("conversation/say", "2", args.len()));
        }
        let conv = match &args[0] {
            Value::Conversation(c) => c.clone(),
            _ => return Err(SemaError::type_error("conversation", args[0].type_name())),
        };
        let user_msg = match &args[1] {
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };

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

        let request = ChatRequest::new(conv.model.clone(), chat_messages);

        let response = with_provider(|p| {
            if request.model.is_empty() {
                let mut req = request.clone();
                req.model = p.default_model().to_string();
                p.complete(req).map_err(|e| SemaError::Llm(e.to_string()))
            } else {
                p.complete(request.clone())
                    .map_err(|e| SemaError::Llm(e.to_string()))
            }
        })?;

        track_usage(&response.usage);

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

    // (prompt-append p1 p2)
    register_fn(env, "prompt-append", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("prompt-append", "2", args.len()));
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

    // (prompt-messages prompt)
    register_fn(env, "prompt-messages", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("prompt-messages", "1", args.len()));
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

    // (prompt-set-system prompt "new system message")
    register_fn(env, "prompt-set-system", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("prompt-set-system", "2", args.len()));
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

    // (message-role msg)
    register_fn(env, "message-role", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("message-role", "1", args.len()));
        }
        let msg = match &args[0] {
            Value::Message(m) => m.clone(),
            _ => return Err(SemaError::type_error("message", args[0].type_name())),
        };
        Ok(Value::keyword(&msg.role.to_string()))
    });

    // (message-content msg)
    register_fn(env, "message-content", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("message-content", "1", args.len()));
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
            Ok(Value::Map(Rc::new(map)))
        })
    });

    // (agent/run agent "user message")
    register_fn(env, "agent/run", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("agent/run", "2", args.len()));
        }
        let agent = match &args[0] {
            Value::Agent(a) => a.clone(),
            _ => return Err(SemaError::type_error("agent", args[0].type_name())),
        };
        let user_msg = match &args[1] {
            Value::String(s) => s.to_string(),
            other => other.to_string(),
        };

        let messages = vec![ChatMessage {
            role: "user".to_string(),
            content: user_msg,
        }];

        let tool_schemas = build_tool_schemas(&agent.tools)?;
        let system = if agent.system.is_empty() {
            None
        } else {
            Some(agent.system.clone())
        };

        let result = run_tool_loop(
            messages,
            agent.model.clone(),
            Some(4096),
            None,
            system,
            &agent.tools,
            &tool_schemas,
            agent.max_turns,
        )?;
        Ok(Value::String(Rc::new(result)))
    });

    // (llm/parallel [expr1 expr2 expr3])
    // Each expression should be a thunk (zero-arg lambda) or will be treated as a value
    register_fn(env, "llm/parallel", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("llm/parallel", "1", args.len()));
        }
        let items = match &args[0] {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[0].type_name())),
        };

        // Run each item. These are already-evaluated values (strings from llm/complete calls).
        // For true parallelism, we'd need to accept thunks and run them on threads.
        // Since our values are Rc (not Send), we run them sequentially here.
        // The parallel semantics are that callers use this as a batch wrapper.
        Ok(Value::list(items))
    });

    // (llm/pmap fn collection {:concurrency N})
    register_fn(env, "llm/pmap", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("llm/pmap", "2-3", args.len()));
        }
        let func = &args[0];
        let items = match &args[1] {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => return Err(SemaError::type_error("list or vector", args[1].type_name())),
        };

        // Execute sequentially (Rc values can't cross thread boundaries).
        // This still provides the right API shape for future async implementation.
        let mut results = Vec::with_capacity(items.len());
        for item in &items {
            results.push(call_value_fn(func, &[item.clone()])?);
        }
        Ok(Value::list(results))
    });

    // (with-budget {:max-cost-usd N} body...) — currently a passthrough, tracks and errors on overspend
    // Registered as a native fn that just returns the last arg (budget tracking is TODO)
    register_fn(env, "llm/reset-usage", |_args| {
        SESSION_USAGE.with(|u| {
            let mut usage = u.borrow_mut();
            usage.prompt_tokens = 0;
            usage.completion_tokens = 0;
        });
        LAST_USAGE.with(|u| *u.borrow_mut() = None);
        Ok(Value::Nil)
    });
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

    let response = with_provider(|p| {
        if request.model.is_empty() {
            let mut req = request.clone();
            req.model = p.default_model().to_string();
            p.complete(req).map_err(|e| SemaError::Llm(e.to_string()))
        } else {
            p.complete(request.clone())
                .map_err(|e| SemaError::Llm(e.to_string()))
        }
    })?;

    track_usage(&response.usage);
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
                    Value::Keyword(s) => s.to_string(),
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                };
                let type_str = match v {
                    Value::Map(inner) => {
                        if let Some(t) = inner.get(&Value::keyword("type")) {
                            match t {
                                Value::Keyword(s) => s.to_string(),
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

/// Send a ChatRequest via the default provider with model fallback.
fn do_complete(mut request: ChatRequest) -> Result<ChatResponse, SemaError> {
    with_provider(|p| {
        if request.model.is_empty() {
            request.model = p.default_model().to_string();
        }
        p.complete(request.clone())
            .map_err(|e| SemaError::Llm(e.to_string()))
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
                    Value::Keyword(s) => s.to_string(),
                    Value::String(s) => s.to_string(),
                    other => other.to_string(),
                };
                let prop = match v {
                    Value::Map(inner) => {
                        let mut prop_obj = serde_json::Map::new();
                        if let Some(t) = inner.get(&Value::keyword("type")) {
                            let type_str = match t {
                                Value::Keyword(s) => s.to_string(),
                                Value::String(s) => s.to_string(),
                                _ => "string".to_string(),
                            };
                            prop_obj.insert(
                                "type".to_string(),
                                serde_json::Value::String(type_str),
                            );
                        }
                        if let Some(d) = inner.get(&Value::keyword("description")) {
                            let desc = match d {
                                Value::String(s) => s.to_string(),
                                other => other.to_string(),
                            };
                            prop_obj.insert(
                                "description".to_string(),
                                serde_json::Value::String(desc),
                            );
                        }
                        if let Some(e) = inner.get(&Value::keyword("enum")) {
                            if let Value::List(items) | Value::Vector(items) = e {
                                let vals: Vec<serde_json::Value> = items
                                    .iter()
                                    .map(|v| match v {
                                        Value::String(s) => {
                                            serde_json::Value::String(s.to_string())
                                        }
                                        Value::Keyword(s) => {
                                            serde_json::Value::String(s.to_string())
                                        }
                                        _ => serde_json::Value::String(v.to_string()),
                                    })
                                    .collect();
                                prop_obj.insert(
                                    "enum".to_string(),
                                    serde_json::Value::Array(vals),
                                );
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

/// The tool execution loop: send → check for tool_calls → execute → send results → repeat.
fn run_tool_loop(
    initial_messages: Vec<ChatMessage>,
    model: String,
    max_tokens: Option<u32>,
    temperature: Option<f64>,
    system: Option<String>,
    tools: &[Value],
    tool_schemas: &[ToolSchema],
    max_rounds: usize,
) -> Result<String, SemaError> {
    let mut messages = initial_messages;
    let mut last_content = String::new();

    for _round in 0..max_rounds {
        let mut request = ChatRequest::new(model.clone(), messages.clone());
        request.max_tokens = max_tokens.or(Some(4096));
        request.temperature = temperature;
        request.system = system.clone();
        request.tools = tool_schemas.to_vec();

        let response = do_complete(request)?;
        track_usage(&response.usage);
        last_content = response.content.clone();

        if response.tool_calls.is_empty() {
            return Ok(last_content);
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
            let result = execute_tool_call(tools, &tc.name, &tc.arguments)?;

            // For Anthropic: tool results go as user messages with tool_use_id reference
            // For simplicity, we add as a user message with the result
            messages.push(ChatMessage {
                role: "user".to_string(),
                content: format!(
                    "[Tool result for {}]: {}",
                    tc.name, result
                ),
            });
        }
    }

    Ok(last_content)
}

/// Execute a tool call by finding the handler and invoking it.
fn execute_tool_call(
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
    let sema_args = json_args_to_sema(&tool_def.parameters, arguments);
    let result = call_value_fn(&tool_def.handler, &sema_args)?;

    // Convert result to string for sending back to LLM
    match &result {
        Value::String(s) => Ok(s.to_string()),
        Value::Map(_) | Value::List(_) | Value::Vector(_) => {
            // JSON-encode complex results
            let json = crate::builtins::sema_value_to_json(&result)
                .unwrap_or(serde_json::Value::Null);
            Ok(serde_json::to_string(&json).unwrap_or_else(|_| result.to_string()))
        }
        other => Ok(other.to_string()),
    }
}

/// Convert JSON arguments into a list of Sema values based on the parameter schema order.
fn json_args_to_sema(params: &Value, arguments: &serde_json::Value) -> Vec<Value> {
    match (params, arguments) {
        (Value::Map(param_map), serde_json::Value::Object(json_obj)) => {
            // Pass arguments in the order defined in the parameter schema
            param_map
                .keys()
                .map(|k| {
                    let key_str = match k {
                        Value::Keyword(s) => s.to_string(),
                        Value::String(s) => s.to_string(),
                        other => other.to_string(),
                    };
                    json_obj
                        .get(&key_str)
                        .map(json_to_sema_value)
                        .unwrap_or(Value::Nil)
                })
                .collect()
        }
        _ => {
            // Single argument or fallback
            vec![json_to_sema_value(arguments)]
        }
    }
}

/// Call a Sema value as a function (lambda or native).
fn call_value_fn(func: &Value, args: &[Value]) -> Result<Value, SemaError> {
    match func {
        Value::NativeFn(native) => (native.func)(args),
        Value::Lambda(lambda) => {
            let env = Env::with_parent(Rc::new(lambda.env.clone()));
            // Bind params
            if let Some(ref rest) = lambda.rest_param {
                if args.len() < lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda.name.as_deref().unwrap_or("lambda"),
                        &format!("{}+", lambda.params.len()),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    env.set(param.clone(), arg.clone());
                }
                let rest_args = args[lambda.params.len()..].to_vec();
                env.set(rest.clone(), Value::list(rest_args));
            } else {
                if args.len() != lambda.params.len() {
                    return Err(SemaError::arity(
                        lambda.name.as_deref().unwrap_or("lambda"),
                        &lambda.params.len().to_string(),
                        args.len(),
                    ));
                }
                for (param, arg) in lambda.params.iter().zip(args.iter()) {
                    env.set(param.clone(), arg.clone());
                }
            }
            // Self-reference
            if let Some(ref name) = lambda.name {
                env.set(
                    name.clone(),
                    Value::Lambda(Rc::new(sema_core::Lambda {
                        params: lambda.params.clone(),
                        rest_param: lambda.rest_param.clone(),
                        body: lambda.body.clone(),
                        env: lambda.env.clone(),
                        name: lambda.name.clone(),
                    })),
                );
            }
            // Eval body using the full evaluator (supports let, if, cond, etc.)
            let mut result = Value::Nil;
            for expr in &lambda.body {
                result = full_eval(expr, &env)?;
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
fn simple_eval(expr: &Value, env: &Env) -> Result<Value, SemaError> {
    match expr {
        Value::Symbol(name) => env
            .get(name)
            .ok_or_else(|| SemaError::Unbound(name.to_string())),
        Value::List(items) if !items.is_empty() => {
            let func_val = simple_eval(&items[0], env)?;
            let mut args = Vec::new();
            for arg in &items[1..] {
                args.push(simple_eval(arg, env)?);
            }
            call_value_fn(&func_val, &args)
        }
        _ => Ok(expr.clone()),
    }
}

/// Convert a Sema value to a serde_json Value.
pub fn sema_value_to_json(val: &Value) -> Result<serde_json::Value, SemaError> {
    match val {
        Value::Nil => Ok(serde_json::Value::Null),
        Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        Value::Int(n) => Ok(serde_json::Value::Number((*n).into())),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .ok_or_else(|| SemaError::eval("cannot encode NaN/Infinity as JSON")),
        Value::String(s) => Ok(serde_json::Value::String(s.to_string())),
        Value::Keyword(s) => Ok(serde_json::Value::String(s.to_string())),
        Value::Symbol(s) => Ok(serde_json::Value::String(s.to_string())),
        Value::List(items) | Value::Vector(items) => {
            let arr: Result<Vec<_>, _> = items.iter().map(sema_value_to_json).collect();
            Ok(serde_json::Value::Array(arr?))
        }
        Value::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map.iter() {
                let key = match k {
                    Value::String(s) => s.to_string(),
                    Value::Keyword(s) => s.to_string(),
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

/// Convert serde_json::Value to sema Value (used by extract)
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
        serde_json::Value::Array(arr) => {
            Value::list(arr.iter().map(json_to_sema_value).collect())
        }
        serde_json::Value::Object(obj) => {
            let mut map = BTreeMap::new();
            for (k, v) in obj {
                map.insert(Value::keyword(k), json_to_sema_value(v));
            }
            Value::Map(Rc::new(map))
        }
    }
}
