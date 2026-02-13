use std::rc::Rc;

use sema_core::{intern, resolve, Agent, Env, Lambda, Macro, SemaError, ToolDefinition, Value};

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
        "defun" => Some(eval_defun(args, env)),
        "set!" => Some(eval_set(args, env)),
        "lambda" | "fn" => Some(eval_lambda(args, env, None)),
        "let" => Some(eval_let(args, env)),
        "let*" => Some(eval_let_star(args, env)),
        "letrec" => Some(eval_letrec(args, env)),
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
        "throw" => Some(eval_throw(args, env)),
        "try" => Some(eval_try(args, env)),
        "module" => Some(eval_module(args, env)),
        "import" => Some(eval_import(args, env)),
        "load" => Some(eval_load(args, env)),
        "case" => Some(eval_case(args, env)),
        "eval" => Some(eval_eval(args, env)),
        "macroexpand" => Some(eval_macroexpand(args, env)),
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
        let is_else = matches!(&items[0], Value::Symbol(s) if resolve(*s) == "else");
        if is_else || eval::eval_value(&items[0], env)?.is_truthy() {
            if items.len() == 1 {
                return Ok(Trampoline::Value(Value::Bool(true)));
            }
            // Eval all but last, tail-call last
            for expr in &items[1..items.len() - 1] {
                eval::eval_value(expr, env)?;
            }
            return Ok(Trampoline::Eval(items.last().unwrap().clone(), env.clone()));
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
        Value::Symbol(spur) => {
            if args.len() != 2 {
                return Err(SemaError::arity("define", "2", args.len()));
            }
            let val = eval::eval_value(&args[1], env)?;
            env.set(*spur, val);
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
            env.set(intern(&name), lambda);
            Ok(Trampoline::Value(Value::Nil))
        }
        other => Err(SemaError::type_error("symbol or list", other.type_name())),
    }
}

/// (defun name (params...) body...) => (define (name params...) body...)
fn eval_defun(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defun", "3+", args.len()));
    }
    let name = match &args[0] {
        Value::Symbol(_) => args[0].clone(),
        other => return Err(SemaError::type_error("symbol", other.type_name())),
    };
    let params = match &args[1] {
        Value::List(p) => p.clone(),
        other => return Err(SemaError::type_error("list", other.type_name())),
    };
    // Build (name params...) signature list
    let mut sig = vec![name];
    sig.extend(params.iter().cloned());
    // Build transformed args: [(name params...), body...]
    let mut define_args = vec![Value::List(sig.into())];
    define_args.extend_from_slice(&args[2..]);
    eval_define(&define_args, env)
}

fn eval_set(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("set!", "2", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::eval("set!: first argument must be a symbol"))?;
    let val = eval::eval_value(&args[1], env)?;
    if !env.set_existing(spur, val) {
        return Err(SemaError::Unbound(resolve(spur)));
    }
    Ok(Trampoline::Value(Value::Nil))
}

fn eval_lambda(args: &[Value], env: &Env, name: Option<String>) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("lambda", "2+", args.len()));
    }
    let param_list = match &args[0] {
        Value::List(params) => params.as_ref().clone(),
        Value::Vector(params) => params.as_ref().clone(),
        other => return Err(SemaError::type_error("list or vector", other.type_name())),
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

    // Named let: (let name ((var init) ...) body...)
    if let Some(loop_name) = args[0].as_symbol() {
        if args.len() < 3 {
            return Err(SemaError::arity("named let", "3+", args.len()));
        }
        let bindings_list = args[1]
            .as_list()
            .ok_or_else(|| SemaError::eval("named let: bindings must be a list"))?;

        let mut params = Vec::new();
        let mut init_vals = Vec::new();
        for binding in bindings_list {
            let pair = binding
                .as_list()
                .ok_or_else(|| SemaError::eval("named let: each binding must be a list"))?;
            if pair.len() != 2 {
                return Err(SemaError::eval(
                    "named let: each binding must have 2 elements",
                ));
            }
            let pname = pair[0]
                .as_symbol()
                .ok_or_else(|| SemaError::eval("named let: binding name must be a symbol"))?
                .to_string();
            let val = eval::eval_value(&pair[1], env)?;
            params.push(pname);
            init_vals.push(val);
        }

        let body = args[2..].to_vec();
        let lambda = Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: env.clone(),
            name: Some(loop_name.to_string()),
        };

        // Build env with params bound + self-reference
        let new_env = Env::with_parent(Rc::new(env.clone()));
        for (p, v) in params.iter().zip(init_vals.iter()) {
            new_env.set(intern(p), v.clone());
        }
        new_env.set(
            intern(&loop_name),
            Value::Lambda(Rc::new(Lambda {
                params: lambda.params.clone(),
                rest_param: None,
                body: lambda.body.clone(),
                env: env.clone(),
                name: lambda.name.clone(),
            })),
        );

        // Tail-call on last body expr
        let body_ref = &args[2..];
        for expr in &body_ref[..body_ref.len() - 1] {
            eval::eval_value(expr, &new_env)?;
        }
        return Ok(Trampoline::Eval(body_ref.last().unwrap().clone(), new_env));
    }

    // Regular let
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
        let name_spur = pair[0]
            .as_symbol_spur()
            .ok_or_else(|| SemaError::eval("let: binding name must be a symbol"))?;
        // Evaluate in the OUTER env for let (not let*)
        let val = eval::eval_value(&pair[1], env)?;
        new_env.set(name_spur, val);
    }

    // Eval body with tail call on last expr
    for expr in &args[1..args.len() - 1] {
        eval::eval_value(expr, &new_env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), new_env))
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
        let name_spur = pair[0]
            .as_symbol_spur()
            .ok_or_else(|| SemaError::eval("let*: binding name must be a symbol"))?;
        // Evaluate in the NEW env (sequential binding)
        let val = eval::eval_value(&pair[1], &new_env)?;
        new_env.set(name_spur, val);
    }

    for expr in &args[1..args.len() - 1] {
        eval::eval_value(expr, &new_env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), new_env))
}

fn eval_letrec(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("letrec", "2+", args.len()));
    }
    let bindings = args[0]
        .as_list()
        .ok_or_else(|| SemaError::eval("letrec: bindings must be a list"))?;

    let new_env = Env::with_parent(Rc::new(env.clone()));

    // Pass 1: bind all names to Nil (placeholders)
    let mut name_spurs = Vec::new();
    for binding in bindings {
        let pair = binding
            .as_list()
            .ok_or_else(|| SemaError::eval("letrec: each binding must be a list"))?;
        if pair.len() != 2 {
            return Err(SemaError::eval("letrec: each binding must have 2 elements"));
        }
        let spur = pair[0]
            .as_symbol_spur()
            .ok_or_else(|| SemaError::eval("letrec: binding name must be a symbol"))?;
        name_spurs.push(spur);
        new_env.set(spur, Value::Nil);
    }

    // Pass 2: evaluate init exprs in new env, update bindings
    for (i, binding) in bindings.iter().enumerate() {
        let pair = binding.as_list().unwrap();
        let val = eval::eval_value(&pair[1], &new_env)?;
        new_env.set(name_spurs[i], val);
    }

    // Eval body with tail call on last expr
    for expr in &args[1..args.len() - 1] {
        eval::eval_value(expr, &new_env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), new_env))
}

fn eval_begin(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Ok(Trampoline::Value(Value::Nil));
    }
    for expr in &args[..args.len() - 1] {
        eval::eval_value(expr, env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
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
    Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
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
    Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
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
        Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
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
        Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
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
    env.set(intern(&name), mac);
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
                    if role_names.contains(&role_str.as_str()) {
                        let role = match role_str.as_str() {
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
            _ => {
                return Err(SemaError::eval(
                    "prompt: expected (role content...) or message value",
                ))
            }
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
        Value::Keyword(spur) => {
            let s = resolve(*spur);
            match s.as_str() {
                "system" => Role::System,
                "user" => Role::User,
                "assistant" => Role::Assistant,
                "tool" => Role::Tool,
                other => return Err(SemaError::eval(format!("message: unknown role '{other}'"))),
            }
        }
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
    env.set(intern(&name), tool.clone());
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
    env.set(intern(&name), agent.clone());
    Ok(Trampoline::Value(agent))
}

/// (throw value) — raise a user exception
fn eval_throw(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("throw", "1", args.len()));
    }
    let val = eval::eval_value(&args[0], env)?;
    Err(SemaError::UserException(val))
}

/// (try body... (catch e handler-body...))
fn eval_try(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("try", "1+", 0));
    }

    // Last arg must be (catch var handler...)
    let last = &args[args.len() - 1];
    let catch_form = last
        .as_list()
        .ok_or_else(|| SemaError::eval("try: last argument must be (catch var handler...)"))?;
    if catch_form.is_empty() {
        return Err(SemaError::eval("try: catch form is empty"));
    }
    let is_catch = matches!(&catch_form[0], Value::Symbol(s) if resolve(*s) == "catch");
    if !is_catch {
        return Err(SemaError::eval(
            "try: last argument must be (catch var handler...)",
        ));
    }
    if catch_form.len() < 3 {
        return Err(SemaError::eval("try: catch needs (catch var handler...)"));
    }
    let catch_var = catch_form[1]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("try: catch variable must be a symbol"))?
        .to_string();
    let handler_body = &catch_form[2..];

    // Evaluate body exprs (not the catch form)
    let body = &args[..args.len() - 1];
    let body_result: Result<Value, SemaError> = (|| {
        let mut result = Value::Nil;
        for expr in body {
            result = eval::eval_value(expr, env)?;
        }
        Ok(result)
    })();

    match body_result {
        Ok(val) => Ok(Trampoline::Value(val)),
        Err(err) => {
            // Convert error to a Sema value (map)
            let err_val = error_to_value(&err);
            let catch_env = Env::with_parent(Rc::new(env.clone()));
            catch_env.set(intern(&catch_var), err_val);
            // Eval handler with TCO on last expr
            for expr in &handler_body[..handler_body.len() - 1] {
                eval::eval_value(expr, &catch_env)?;
            }
            Ok(Trampoline::Eval(
                handler_body.last().unwrap().clone(),
                catch_env,
            ))
        }
    }
}

/// Convert a SemaError into a Sema map value
fn error_to_value(err: &SemaError) -> Value {
    use std::collections::BTreeMap;

    // Extract stack trace if present, then work with the inner error
    let trace = err.stack_trace().cloned();
    let inner = err.inner();

    let mut map = BTreeMap::new();
    match inner {
        SemaError::Reader { message, span } => {
            map.insert(Value::keyword("type"), Value::keyword("reader"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("{message} at {span}")),
            );
        }
        SemaError::Eval(msg) => {
            map.insert(Value::keyword("type"), Value::keyword("eval"));
            map.insert(Value::keyword("message"), Value::string(msg));
        }
        SemaError::Type { expected, got } => {
            map.insert(Value::keyword("type"), Value::keyword("type-error"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("expected {expected}, got {got}")),
            );
            map.insert(Value::keyword("expected"), Value::string(expected));
            map.insert(Value::keyword("got"), Value::string(got));
        }
        SemaError::Arity {
            name,
            expected,
            got,
        } => {
            map.insert(Value::keyword("type"), Value::keyword("arity"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("{name} expects {expected} args, got {got}")),
            );
        }
        SemaError::Unbound(name) => {
            map.insert(Value::keyword("type"), Value::keyword("unbound"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!("Unbound variable: {name}")),
            );
            map.insert(Value::keyword("name"), Value::string(name));
        }
        SemaError::Llm(msg) => {
            map.insert(Value::keyword("type"), Value::keyword("llm"));
            map.insert(Value::keyword("message"), Value::string(msg));
        }
        SemaError::Io(msg) => {
            map.insert(Value::keyword("type"), Value::keyword("io"));
            map.insert(Value::keyword("message"), Value::string(msg));
        }
        SemaError::UserException(val) => {
            map.insert(Value::keyword("type"), Value::keyword("user"));
            map.insert(Value::keyword("message"), Value::string(&val.to_string()));
            map.insert(Value::keyword("value"), val.clone());
        }
        SemaError::WithTrace { .. } => unreachable!("inner() already unwraps WithTrace"),
    }

    // Add stack trace as list of maps
    if let Some(st) = trace {
        let frames: Vec<Value> =
            st.0.iter()
                .map(|frame| {
                    let mut fm = BTreeMap::new();
                    fm.insert(Value::keyword("name"), Value::string(&frame.name));
                    if let Some(ref file) = frame.file {
                        fm.insert(Value::keyword("file"), Value::string(file));
                    }
                    if let Some(ref span) = frame.span {
                        fm.insert(Value::keyword("line"), Value::Int(span.line as i64));
                        fm.insert(Value::keyword("col"), Value::Int(span.col as i64));
                    }
                    Value::Map(Rc::new(fm))
                })
                .collect();
        map.insert(Value::keyword("stack-trace"), Value::list(frames));
    }

    Value::Map(Rc::new(map))
}

/// (module name (export sym1 sym2 ...) body...)
fn eval_module(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("module", "2+", args.len()));
    }
    // Parse module name (just for documentation, not used in resolution)
    let _name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("module: name must be a symbol"))?;

    // Parse export list: (export sym1 sym2 ...)
    let export_list = args[1]
        .as_list()
        .ok_or_else(|| SemaError::eval("module: second argument must be (export sym1 sym2 ...)"))?;
    if export_list.is_empty()
        || !matches!(&export_list[0], Value::Symbol(s) if resolve(*s) == "export")
    {
        return Err(SemaError::eval(
            "module: second argument must start with 'export'",
        ));
    }
    let export_names: Vec<String> = export_list[1..]
        .iter()
        .map(|v| {
            v.as_symbol()
                .map(|s| s.to_string())
                .ok_or_else(|| SemaError::eval("module: export names must be symbols"))
        })
        .collect::<Result<_, _>>()?;

    eval::set_module_exports(export_names);

    // Evaluate body
    let body = &args[2..];
    if body.is_empty() {
        return Ok(Trampoline::Value(Value::Nil));
    }
    for expr in &body[..body.len() - 1] {
        eval::eval_value(expr, env)?;
    }
    Ok(Trampoline::Eval(body.last().unwrap().clone(), env.clone()))
}

/// (import "path.sema") or (import "path.sema" sym1 sym2)
fn eval_import(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("import", "1+", 0));
    }
    let path_val = eval::eval_value(&args[0], env)?;
    let path_str = path_val
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", path_val.type_name()))?;

    // Resolve path relative to current file
    let resolved = if std::path::Path::new(path_str).is_absolute() {
        std::path::PathBuf::from(path_str)
    } else if let Some(dir) = eval::current_file_dir() {
        dir.join(path_str)
    } else {
        std::path::PathBuf::from(path_str)
    };
    let canonical = resolved
        .canonicalize()
        .map_err(|e| SemaError::Io(format!("import {path_str}: {e}")))?;

    // Selective import names
    let selective: Vec<String> = args[1..]
        .iter()
        .map(|v| {
            v.as_symbol()
                .map(|s| s.to_string())
                .ok_or_else(|| SemaError::eval("import: selective names must be symbols"))
        })
        .collect::<Result<_, _>>()?;

    // Check cache
    if let Some(cached) = eval::get_cached_module(&canonical) {
        copy_exports_to_env(&cached, &selective, env)?;
        return Ok(Trampoline::Value(Value::Nil));
    }

    // Load and evaluate the module
    let content = std::fs::read_to_string(&canonical)
        .map_err(|e| SemaError::Io(format!("import {path_str}: {e}")))?;
    let (exprs, spans) = sema_reader::read_many_with_spans(&content)?;
    eval::merge_span_table(spans);

    let module_env = eval::create_module_env(env);
    eval::push_file_path(canonical.clone());
    eval::clear_module_exports();

    let eval_result = (|| {
        for expr in &exprs {
            eval::eval_value(expr, &module_env)?;
        }
        Ok(())
    })();

    eval::pop_file_path();

    eval_result?;

    // Collect exports
    let declared = eval::take_module_exports();
    let exports = collect_module_exports(&module_env, declared.as_deref());

    // Cache
    eval::cache_module(canonical, exports.clone());

    // Copy to caller env
    copy_exports_to_env(&exports, &selective, env)?;

    Ok(Trampoline::Value(Value::Nil))
}

/// Collect exported bindings from a module env
fn collect_module_exports(
    module_env: &Env,
    declared: Option<&[String]>,
) -> std::collections::BTreeMap<String, Value> {
    let bindings = module_env.bindings.borrow();
    match declared {
        Some(names) => {
            let mut exports = std::collections::BTreeMap::new();
            for name in names {
                let spur = intern(name);
                if let Some(val) = bindings.get(&spur) {
                    exports.insert(name.clone(), val.clone());
                }
            }
            exports
        }
        None => {
            let mut exports = std::collections::BTreeMap::new();
            for (spur, val) in bindings.iter() {
                exports.insert(resolve(*spur), val.clone());
            }
            exports
        }
    }
}

/// Copy exports into the caller environment
fn copy_exports_to_env(
    exports: &std::collections::BTreeMap<String, Value>,
    selective: &[String],
    env: &Env,
) -> Result<(), SemaError> {
    if selective.is_empty() {
        for (name, val) in exports {
            env.set(intern(name), val.clone());
        }
    } else {
        for name in selective {
            let val = exports.get(name).ok_or_else(|| {
                SemaError::eval(format!("import: module does not export '{name}'"))
            })?;
            env.set(intern(name), val.clone());
        }
    }
    Ok(())
}

/// (load "file.sema") — read and evaluate a file in the current environment
fn eval_load(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("load", "1", args.len()));
    }
    let path_val = eval::eval_value(&args[0], env)?;
    let path_str = path_val
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", path_val.type_name()))?;

    // Resolve path relative to current file
    let resolved = if std::path::Path::new(path_str).is_absolute() {
        std::path::PathBuf::from(path_str)
    } else if let Some(dir) = eval::current_file_dir() {
        dir.join(path_str)
    } else {
        std::path::PathBuf::from(path_str)
    };

    let content = std::fs::read_to_string(&resolved)
        .map_err(|e| SemaError::Io(format!("load {}: {e}", resolved.display())))?;
    let (exprs, spans) = sema_reader::read_many_with_spans(&content)?;
    eval::merge_span_table(spans);

    // Push the file path so nested load/import resolves correctly
    if let Ok(canonical) = resolved.canonicalize() {
        eval::push_file_path(canonical);
    }

    let mut result = Value::Nil;
    let eval_result = (|| {
        for expr in &exprs {
            result = eval::eval_value(expr, env)?;
        }
        Ok(result.clone())
    })();

    // Pop regardless of success/failure
    if resolved.canonicalize().is_ok() {
        eval::pop_file_path();
    }

    eval_result?;
    Ok(Trampoline::Value(result))
}

/// (case key-expr ((datum ...) body ...) ... (else body ...))
fn eval_case(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("case", "2+", args.len()));
    }
    let key = eval::eval_value(&args[0], env)?;

    for clause in &args[1..] {
        let items = clause
            .as_list()
            .ok_or_else(|| SemaError::eval("case: clause must be a list"))?;
        if items.is_empty() {
            return Err(SemaError::eval("case: clause must not be empty"));
        }
        // (else body...)
        let is_else = matches!(&items[0], Value::Symbol(s) if resolve(*s) == "else");
        if is_else {
            if items.len() == 1 {
                return Ok(Trampoline::Value(Value::Nil));
            }
            for expr in &items[1..items.len() - 1] {
                eval::eval_value(expr, env)?;
            }
            return Ok(Trampoline::Eval(items.last().unwrap().clone(), env.clone()));
        }
        // ((datum ...) body...)
        let datums = items[0]
            .as_list()
            .ok_or_else(|| SemaError::eval("case: datums must be a list"))?;
        if datums.contains(&key) {
            if items.len() == 1 {
                return Ok(Trampoline::Value(Value::Nil));
            }
            for expr in &items[1..items.len() - 1] {
                eval::eval_value(expr, env)?;
            }
            return Ok(Trampoline::Eval(items.last().unwrap().clone(), env.clone()));
        }
    }
    Ok(Trampoline::Value(Value::Nil))
}

/// (eval expr) — evaluate a quoted expression
fn eval_eval(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("eval", "1", args.len()));
    }
    let expr = eval::eval_value(&args[0], env)?;
    Ok(Trampoline::Eval(expr, env.clone()))
}

/// (macroexpand '(macro-call args...)) — expand a macro once without evaluating
fn eval_macroexpand(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("macroexpand", "1", args.len()));
    }
    let form = eval::eval_value(&args[0], env)?;
    // Check if it's a list starting with a macro name
    if let Value::List(items) = &form {
        if !items.is_empty() {
            if let Value::Symbol(spur) = &items[0] {
                if let Some(Value::Macro(mac)) = env.get(*spur) {
                    let expanded = eval::apply_macro(&mac, &items[1..], env)?;
                    return Ok(Trampoline::Value(expanded));
                }
            }
        }
    }
    // Not a macro form — return as-is
    Ok(Trampoline::Value(form))
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
