use std::rc::Rc;

use sema_core::{Agent, Env, Lambda, Macro, SemaError, ToolDefinition, Value};

use crate::eval::{self, Trampoline};

/// Evaluate a special form. Returns Some(result) if the head is a special form, None otherwise.
pub fn try_eval_special(
    head: &str,
    args: &[Value],
    env: &Env,
) -> Option<Result<Trampoline, SemaError>> {
    match head {
        "quote" => Some(eval_quote(args)),
        "if" => Some(eval_if(args, env)),
        "cond" => Some(eval_cond(args, env)),
        "define" => Some(eval_define(args, env)),
        "set!" => Some(eval_set(args, env)),
        "lambda" | "fn" => Some(eval_lambda(args, env, None)),
        "let" => Some(eval_let(args, env)),
        "let*" => Some(eval_let_star(args, env)),
        "begin" | "do" => Some(eval_begin(args, env)),
        "and" => Some(eval_and(args, env)),
        "or" => Some(eval_or(args, env)),
        "when" => Some(eval_when(args, env)),
        "unless" => Some(eval_unless(args, env)),
        "defmacro" => Some(eval_defmacro(args, env)),
        "quasiquote" => Some(eval_quasiquote(args, env)),
        "prompt" => Some(eval_prompt(args, env)),
        "message" => Some(eval_message(args, env)),
        "deftool" => Some(eval_deftool(args, env)),
        "defagent" => Some(eval_defagent(args, env)),
        "load" => Some(eval_load(args, env)),
        "with-budget" => Some(eval_with_budget(args, env)),
        _ => None,
    }
}

fn eval_quote(args: &[Value]) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("quote", "1", args.len()));
    }
    Ok(Trampoline::Value(args[0].clone()))
}

fn eval_if(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(SemaError::arity("if", "2 or 3", args.len()));
    }
    let cond = eval::eval_value(&args[0], env)?;
    if cond.is_truthy() {
        Ok(Trampoline::Eval(args[1].clone(), env.clone()))
    } else if args.len() == 3 {
        Ok(Trampoline::Eval(args[2].clone(), env.clone()))
    } else {
        Ok(Trampoline::Value(Value::Nil))
    }
}

fn eval_cond(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    for clause in args {
        let items = clause
            .as_list()
            .ok_or_else(|| SemaError::eval("cond clause must be a list"))?;
        if items.is_empty() {
            return Err(SemaError::eval("cond clause must not be empty"));
        }
        // (else body...) or (test body...)
        let is_else = matches!(&items[0], Value::Symbol(s) if s.as_ref() == "else");
        if is_else || eval::eval_value(&items[0], env)?.is_truthy() {
            if items.len() == 1 {
                return Ok(Trampoline::Value(Value::Bool(true)));
            }
            // Eval all but last, tail-call last
            for expr in &items[1..items.len() - 1] {
                eval::eval_value(expr, env)?;
            }
            return Ok(Trampoline::Eval(
                items.last().unwrap().clone(),
                env.clone(),
            ));
        }
    }
    Ok(Trampoline::Value(Value::Nil))
}

fn eval_define(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("define", "2+", 0));
    }
    match &args[0] {
        // (define x expr)
        Value::Symbol(name) => {
            if args.len() != 2 {
                return Err(SemaError::arity("define", "2", args.len()));
            }
            let val = eval::eval_value(&args[1], env)?;
            env.set(name.to_string(), val);
            Ok(Trampoline::Value(Value::Nil))
        }
        // (define (f x y) body...) => (define f (lambda (x y) body...))
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(SemaError::eval("define: empty function signature"));
            }
            let name = sig[0]
                .as_symbol()
                .ok_or_else(|| SemaError::eval("define: function name must be a symbol"))?
                .to_string();
            let params: Vec<String> = sig[1..]
                .iter()
                .map(|v| {
                    v.as_symbol()
                        .map(|s| s.to_string())
                        .ok_or_else(|| SemaError::eval("define: parameter must be a symbol"))
                })
                .collect::<Result<_, _>>()?;
            let (params, rest_param) = parse_params(&params);
            let body = args[1..].to_vec();
            if body.is_empty() {
                return Err(SemaError::eval("define: function body cannot be empty"));
            }
            let lambda = Value::Lambda(Rc::new(Lambda {
                params,
                rest_param,
                body,
                env: env.clone(),
                name: Some(name.clone()),
            }));
            env.set(name, lambda);
            Ok(Trampoline::Value(Value::Nil))
        }
        other => Err(SemaError::type_error(
            "symbol or list",
            other.type_name(),
        )),
    }
}

fn eval_set(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("set!", "2", args.len()));
    }
    let name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("set!: first argument must be a symbol"))?;
    let val = eval::eval_value(&args[1], env)?;
    if !env.set_existing(name, val) {
        return Err(SemaError::Unbound(name.to_string()));
    }
    Ok(Trampoline::Value(Value::Nil))
}

fn eval_lambda(
    args: &[Value],
    env: &Env,
    name: Option<String>,
) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("lambda", "2+", args.len()));
    }
    let param_list = match &args[0] {
        Value::List(params) => params.as_ref().clone(),
        Value::Vector(params) => params.as_ref().clone(),
        other => {
            return Err(SemaError::type_error(
                "list or vector",
                other.type_name(),
            ))
        }
    };
    let param_names: Vec<String> = param_list
        .iter()
        .map(|v| {
            v.as_symbol()
                .map(|s| s.to_string())
                .ok_or_else(|| SemaError::eval("lambda: parameter must be a symbol"))
        })
        .collect::<Result<_, _>>()?;
    let (params, rest_param) = parse_params(&param_names);
    let body = args[1..].to_vec();
    Ok(Trampoline::Value(Value::Lambda(Rc::new(Lambda {
        params,
        rest_param,
        body,
        env: env.clone(),
        name,
    }))))
}

fn eval_let(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("let", "2+", args.len()));
    }
    let bindings = args[0]
        .as_list()
        .ok_or_else(|| SemaError::eval("let: bindings must be a list"))?;

    let new_env = Env::with_parent(Rc::new(env.clone()));

    for binding in bindings {
        let pair = binding
            .as_list()
            .ok_or_else(|| SemaError::eval("let: each binding must be a list"))?;
        if pair.len() != 2 {
            return Err(SemaError::eval("let: each binding must have 2 elements"));
        }
        let name = pair[0]
            .as_symbol()
            .ok_or_else(|| SemaError::eval("let: binding name must be a symbol"))?;
        // Evaluate in the OUTER env for let (not let*)
        let val = eval::eval_value(&pair[1], env)?;
        new_env.set(name.to_string(), val);
    }

    // Eval body with tail call on last expr
    for expr in &args[1..args.len() - 1] {
        eval::eval_value(expr, &new_env)?;
    }
    Ok(Trampoline::Eval(
        args.last().unwrap().clone(),
        new_env,
    ))
}

fn eval_let_star(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("let*", "2+", args.len()));
    }
    let bindings = args[0]
        .as_list()
        .ok_or_else(|| SemaError::eval("let*: bindings must be a list"))?;

    let new_env = Env::with_parent(Rc::new(env.clone()));

    for binding in bindings {
        let pair = binding
            .as_list()
            .ok_or_else(|| SemaError::eval("let*: each binding must be a list"))?;
        if pair.len() != 2 {
            return Err(SemaError::eval("let*: each binding must have 2 elements"));
        }
        let name = pair[0]
            .as_symbol()
            .ok_or_else(|| SemaError::eval("let*: binding name must be a symbol"))?;
        // Evaluate in the NEW env (sequential binding)
        let val = eval::eval_value(&pair[1], &new_env)?;
        new_env.set(name.to_string(), val);
    }

    for expr in &args[1..args.len() - 1] {
        eval::eval_value(expr, &new_env)?;
    }
    Ok(Trampoline::Eval(
        args.last().unwrap().clone(),
        new_env,
    ))
}

fn eval_begin(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Ok(Trampoline::Value(Value::Nil));
    }
    for expr in &args[..args.len() - 1] {
        eval::eval_value(expr, env)?;
    }
    Ok(Trampoline::Eval(
        args.last().unwrap().clone(),
        env.clone(),
    ))
}

fn eval_and(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Ok(Trampoline::Value(Value::Bool(true)));
    }
    for expr in &args[..args.len() - 1] {
        let val = eval::eval_value(expr, env)?;
        if !val.is_truthy() {
            return Ok(Trampoline::Value(val));
        }
    }
    Ok(Trampoline::Eval(
        args.last().unwrap().clone(),
        env.clone(),
    ))
}

fn eval_or(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Ok(Trampoline::Value(Value::Bool(false)));
    }
    for expr in &args[..args.len() - 1] {
        let val = eval::eval_value(expr, env)?;
        if val.is_truthy() {
            return Ok(Trampoline::Value(val));
        }
    }
    Ok(Trampoline::Eval(
        args.last().unwrap().clone(),
        env.clone(),
    ))
}

fn eval_when(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("when", "2+", args.len()));
    }
    let cond = eval::eval_value(&args[0], env)?;
    if cond.is_truthy() {
        for expr in &args[1..args.len() - 1] {
            eval::eval_value(expr, env)?;
        }
        Ok(Trampoline::Eval(
            args.last().unwrap().clone(),
            env.clone(),
        ))
    } else {
        Ok(Trampoline::Value(Value::Nil))
    }
}

fn eval_unless(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("unless", "2+", args.len()));
    }
    let cond = eval::eval_value(&args[0], env)?;
    if !cond.is_truthy() {
        for expr in &args[1..args.len() - 1] {
            eval::eval_value(expr, env)?;
        }
        Ok(Trampoline::Eval(
            args.last().unwrap().clone(),
            env.clone(),
        ))
    } else {
        Ok(Trampoline::Value(Value::Nil))
    }
}

fn eval_defmacro(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defmacro", "3+", args.len()));
    }
    let name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("defmacro: name must be a symbol"))?
        .to_string();
    let param_list = args[1]
        .as_list()
        .ok_or_else(|| SemaError::eval("defmacro: params must be a list"))?;
    let param_names: Vec<String> = param_list
        .iter()
        .map(|v| {
            v.as_symbol()
                .map(|s| s.to_string())
                .ok_or_else(|| SemaError::eval("defmacro: parameter must be a symbol"))
        })
        .collect::<Result<_, _>>()?;
    let (params, rest_param) = parse_params(&param_names);
    let body = args[2..].to_vec();

    let mac = Value::Macro(Rc::new(Macro {
        params,
        rest_param,
        body,
        name: name.clone(),
    }));
    env.set(name, mac);
    Ok(Trampoline::Value(Value::Nil))
}

fn eval_quasiquote(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("quasiquote", "1", args.len()));
    }
    let result = expand_quasiquote(&args[0], env)?;
    Ok(Trampoline::Value(result))
}

fn expand_quasiquote(val: &Value, env: &Env) -> Result<Value, SemaError> {
    match val {
        Value::List(items) => {
            if items.is_empty() {
                return Ok(val.clone());
            }
            // Check for (unquote x)
            if let Some(sym) = items[0].as_symbol() {
                if sym == "unquote" {
                    if items.len() != 2 {
                        return Err(SemaError::arity("unquote", "1", items.len() - 1));
                    }
                    return eval::eval_value(&items[1], env);
                }
            }
            // Expand each element, handling splicing
            let mut result = Vec::new();
            for item in items.iter() {
                if let Value::List(inner) = item {
                    if !inner.is_empty() {
                        if let Some(sym) = inner[0].as_symbol() {
                            if sym == "unquote-splicing" {
                                if inner.len() != 2 {
                                    return Err(SemaError::arity(
                                        "unquote-splicing",
                                        "1",
                                        inner.len() - 1,
                                    ));
                                }
                                let splice_val = eval::eval_value(&inner[1], env)?;
                                if let Value::List(splice_items) = splice_val {
                                    result.extend(splice_items.iter().cloned());
                                } else {
                                    return Err(SemaError::type_error(
                                        "list",
                                        splice_val.type_name(),
                                    ));
                                }
                                continue;
                            }
                        }
                    }
                }
                result.push(expand_quasiquote(item, env)?);
            }
            Ok(Value::list(result))
        }
        Value::Vector(items) => {
            let mut result = Vec::new();
            for item in items.iter() {
                result.push(expand_quasiquote(item, env)?);
            }
            Ok(Value::vector(result))
        }
        _ => Ok(val.clone()),
    }
}

fn eval_prompt(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    use sema_core::{Message, Prompt, Role};

    let role_names = ["system", "user", "assistant", "tool"];

    let mut messages = Vec::new();
    for arg in args {
        // Check if this is a (role content...) form BEFORE evaluating
        if let Value::List(items) = arg {
            if !items.is_empty() {
                if let Some(role_str) = items[0].as_symbol() {
                    if role_names.contains(&role_str) {
                        let role = match role_str {
                            "system" => Role::System,
                            "user" => Role::User,
                            "assistant" => Role::Assistant,
                            "tool" => Role::Tool,
                            _ => unreachable!(),
                        };
                        // Evaluate and concatenate the content parts
                        let mut content = String::new();
                        for part in &items[1..] {
                            let part_val = eval::eval_value(part, env)?;
                            match &part_val {
                                Value::String(s) => content.push_str(s),
                                other => content.push_str(&other.to_string()),
                            }
                        }
                        messages.push(Message { role, content });
                        continue;
                    }
                }
            }
        }
        // Not a role form — evaluate it and check if it's a Message value
        let val = eval::eval_value(arg, env)?;
        match &val {
            Value::Message(msg) => messages.push((**msg).clone()),
            _ => return Err(SemaError::eval("prompt: expected (role content...) or message value")),
        }
    }
    Ok(Trampoline::Value(Value::Prompt(Rc::new(Prompt {
        messages,
    }))))
}

fn eval_message(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    use sema_core::{Message, Role};

    if args.len() < 2 {
        return Err(SemaError::arity("message", "2+", args.len()));
    }
    let role_val = eval::eval_value(&args[0], env)?;
    let role = match &role_val {
        Value::Keyword(s) => match s.as_ref() as &str {
            "system" => Role::System,
            "user" => Role::User,
            "assistant" => Role::Assistant,
            "tool" => Role::Tool,
            other => return Err(SemaError::eval(format!("message: unknown role '{other}'"))),
        },
        other => return Err(SemaError::type_error("keyword", other.type_name())),
    };
    let mut content = String::new();
    for part in &args[1..] {
        let val = eval::eval_value(part, env)?;
        match &val {
            Value::String(s) => content.push_str(s),
            other => content.push_str(&other.to_string()),
        }
    }
    Ok(Trampoline::Value(Value::Message(Rc::new(Message {
        role,
        content,
    }))))
}

/// (deftool name "description" {:param {:type :string :description "..."}} handler-expr)
fn eval_deftool(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 4 {
        return Err(SemaError::arity("deftool", "4", args.len()));
    }
    let name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("deftool: name must be a symbol"))?
        .to_string();
    let description = eval::eval_value(&args[1], env)?;
    let description = match &description {
        Value::String(s) => s.to_string(),
        other => return Err(SemaError::type_error("string", other.type_name())),
    };
    let parameters = eval::eval_value(&args[2], env)?;
    let handler = eval::eval_value(&args[3], env)?;

    let tool = Value::ToolDef(Rc::new(ToolDefinition {
        name: name.clone(),
        description,
        parameters,
        handler,
    }));
    env.set(name, tool.clone());
    Ok(Trampoline::Value(tool))
}

/// (defagent name {:system "..." :tools [...] :max-turns N :model "..."})
fn eval_defagent(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("defagent", "2", args.len()));
    }
    let name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("defagent: name must be a symbol"))?
        .to_string();
    let opts = eval::eval_value(&args[1], env)?;
    let opts_map = match &opts {
        Value::Map(m) => m.as_ref(),
        _ => return Err(SemaError::type_error("map", opts.type_name())),
    };

    let system = opts_map
        .get(&Value::keyword("system"))
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_default();

    let tools = opts_map
        .get(&Value::keyword("tools"))
        .map(|v| match v {
            Value::List(l) => l.as_ref().clone(),
            Value::Vector(v) => v.as_ref().clone(),
            _ => vec![],
        })
        .unwrap_or_default();

    let max_turns = opts_map
        .get(&Value::keyword("max-turns"))
        .and_then(|v| v.as_int())
        .unwrap_or(10) as usize;

    let model = opts_map
        .get(&Value::keyword("model"))
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_default();

    let agent = Value::Agent(Rc::new(Agent {
        name: name.clone(),
        system,
        tools,
        max_turns,
        model,
    }));
    env.set(name, agent.clone());
    Ok(Trampoline::Value(agent))
}

/// (load "file.sema") — read and evaluate a file in the current environment
fn eval_load(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("load", "1", args.len()));
    }
    let path_val = eval::eval_value(&args[0], env)?;
    let path = path_val
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", path_val.type_name()))?;
    let content = std::fs::read_to_string(path)
        .map_err(|e| SemaError::Io(format!("load {path}: {e}")))?;
    let exprs = sema_reader::read_many(&content)?;
    let mut result = Value::Nil;
    for expr in &exprs {
        result = eval::eval_value(expr, env)?;
    }
    Ok(Trampoline::Value(result))
}

/// (with-budget {:max-cost-usd 0.50} body...)
fn eval_with_budget(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("with-budget", "2+", args.len()));
    }
    let opts = eval::eval_value(&args[0], env)?;
    let max_cost = match &opts {
        Value::Map(m) => m
            .get(&Value::keyword("max-cost-usd"))
            .and_then(|v| v.as_float())
            .ok_or_else(|| SemaError::eval("with-budget: missing :max-cost-usd"))?,
        _ => return Err(SemaError::type_error("map", opts.type_name())),
    };

    sema_llm::builtins::set_budget(max_cost);

    let mut result = Value::Nil;
    let body_result = (|| {
        for expr in &args[1..] {
            result = eval::eval_value(expr, env)?;
        }
        Ok(result.clone())
    })();

    sema_llm::builtins::clear_budget();

    body_result?;
    Ok(Trampoline::Value(result))
}

/// Parse parameter list, handling rest params (e.g., `(a b . rest)`)
fn parse_params(names: &[String]) -> (Vec<String>, Option<String>) {
    if let Some(pos) = names.iter().position(|s| s == ".") {
        let params = names[..pos].to_vec();
        let rest = if pos + 1 < names.len() {
            Some(names[pos + 1].clone())
        } else {
            None
        };
        (params, rest)
    } else {
        (names.to_vec(), None)
    }
}
