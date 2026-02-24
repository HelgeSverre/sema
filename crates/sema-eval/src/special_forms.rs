use std::cell::{Cell, RefCell};
use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{
    intern, resolve, Agent, Env, EvalContext, Lambda, Macro, MultiMethod, Record, SemaError, Spur,
    Thunk, ToolDefinition, Value,
};

use crate::destructure;
use crate::eval::{self, Trampoline};

/// Pre-interned `Spur` handles for all special form names.
///
/// Special form dispatch is the hottest path in the evaluator — every list expression
/// checks whether its head is a special form. By caching the interned `Spur` (u32) for
/// each name, we compare integers instead of resolving strings. This also avoids calling
/// `resolve()` which allocates a `String` from the thread-local interner on every lookup.
struct SpecialFormSpurs {
    // Core language
    and: Spur,
    begin: Spur,
    case: Spur,
    catch: Spur,
    cond: Spur,
    define: Spur,
    define_record_type: Spur,
    defmacro: Spur,
    defmethod: Spur,
    defmulti: Spur,
    defun: Spur,
    delay: Spur,
    do_: Spur,
    else_: Spur,
    eval: Spur,
    export: Spur,
    fn_: Spur,
    force: Spur,
    if_: Spur,
    lambda: Spur,
    let_: Spur,
    let_star: Spur,
    letrec: Spur,
    macroexpand: Spur,
    match_: Spur,
    or: Spur,
    quasiquote: Spur,
    quote: Spur,
    set_bang: Spur,
    throw: Spur,
    try_: Spur,
    unless: Spur,
    when: Spur,
    while_: Spur,

    // Modules
    import: Spur,
    load: Spur,
    module: Spur,

    // LLM primitives
    defagent: Spur,
    deftool: Spur,
    message: Spur,
    prompt: Spur,

    // Introspection
    ask: Spur,
    ask_code: Spur,
    doc: Spur,
    doc_search: Spur,
    doctest: Spur,
    heal: Spur,
    meta: Spur,
    source_of: Spur,

    // Runtime self-modification
    become_: Spur,
    freeze: Spur,
    history: Spur,
    observe: Spur,
    rollback: Spur,

    // Silent aliases for other Lisp dialects
    def: Spur,
    defn: Spur,
    progn: Spur,
}

impl SpecialFormSpurs {
    fn init() -> Self {
        Self {
            // Core language
            and: intern("and"),
            begin: intern("begin"),
            case: intern("case"),
            catch: intern("catch"),
            cond: intern("cond"),
            define: intern("define"),
            define_record_type: intern("define-record-type"),
            defmacro: intern("defmacro"),
            defmethod: intern("defmethod"),
            defmulti: intern("defmulti"),
            defun: intern("defun"),
            delay: intern("delay"),
            do_: intern("do"),
            else_: intern("else"),
            eval: intern("eval"),
            export: intern("export"),
            fn_: intern("fn"),
            force: intern("force"),
            if_: intern("if"),
            lambda: intern("lambda"),
            let_: intern("let"),
            let_star: intern("let*"),
            letrec: intern("letrec"),
            macroexpand: intern("macroexpand"),
            match_: intern("match"),
            or: intern("or"),
            quasiquote: intern("quasiquote"),
            quote: intern("quote"),
            set_bang: intern("set!"),
            throw: intern("throw"),
            try_: intern("try"),
            unless: intern("unless"),
            when: intern("when"),
            while_: intern("while"),

            // Modules
            import: intern("import"),
            load: intern("load"),
            module: intern("module"),

            // LLM primitives
            defagent: intern("defagent"),
            deftool: intern("deftool"),
            message: intern("message"),
            prompt: intern("prompt"),

            // Introspection
            ask: intern("ask"),
            ask_code: intern("ask/code"),
            doc: intern("doc"),
            doc_search: intern("doc/search"),
            doctest: intern("doctest"),
            heal: intern("heal!"),
            meta: intern("meta"),
            source_of: intern("source-of"),

            // Runtime self-modification
            become_: intern("become!"),
            freeze: intern("freeze!"),
            history: intern("history"),
            observe: intern("observe!"),
            rollback: intern("rollback!"),

            // Silent aliases
            def: intern("def"),
            defn: intern("defn"),
            progn: intern("progn"),
        }
    }
}

thread_local! {
    static SF: Cell<Option<&'static SpecialFormSpurs>> = const { Cell::new(None) };
}

fn special_forms() -> &'static SpecialFormSpurs {
    SF.with(|cell| match cell.get() {
        Some(sf) => sf,
        None => {
            let sf: &'static SpecialFormSpurs = Box::leak(Box::new(SpecialFormSpurs::init()));
            cell.set(Some(sf));
            sf
        }
    })
}

/// Canonical list of all special form names recognized by the evaluator.
///
/// This is the single source of truth — used by the REPL for completion,
/// the LSP for highlighting, and anywhere else that needs to enumerate special forms.
pub const SPECIAL_FORM_NAMES: &[&str] = &[
    // Core language
    "and",
    "begin",
    "case",
    "cond",
    "define",
    "define-record-type",
    "defmacro",
    "defmethod",
    "defmulti",
    "defun",
    "delay",
    "do",
    "eval",
    "fn",
    "force",
    "if",
    "lambda",
    "let",
    "let*",
    "letrec",
    "macroexpand",
    "match",
    "or",
    "quasiquote",
    "quote",
    "set!",
    "throw",
    "try",
    "unless",
    "when",
    "while",
    // Modules
    "export",
    "import",
    "load",
    "module",
    // LLM primitives
    "defagent",
    "deftool",
    "message",
    "prompt",
    // Introspection
    "ask",
    "ask/code",
    "doc",
    "doc/search",
    "doctest",
    "heal!",
    "meta",
    "source-of",
    // Runtime self-modification
    "become!",
    "freeze!",
    "history",
    "observe!",
    "rollback!",
    // Silent aliases for other Lisp dialects (undocumented)
    "def",
    "defn",
    "progn",
];

/// Evaluate a special form. Returns Some(result) if the head is a special form, None otherwise.
pub fn try_eval_special(
    head_spur: Spur,
    args: &[Value],
    env: &Env,
    ctx: &EvalContext,
) -> Option<Result<Trampoline, SemaError>> {
    let sf = special_forms();

    // Core language — hot path forms first (if, define, let, begin, lambda)
    if head_spur == sf.if_ {
        Some(eval_if(args, env, ctx))
    } else if head_spur == sf.define || head_spur == sf.def {
        Some(eval_define(args, env, ctx))
    } else if head_spur == sf.let_ {
        Some(eval_let(args, env, ctx))
    } else if head_spur == sf.begin || head_spur == sf.progn {
        Some(eval_begin(args, env, ctx))
    } else if head_spur == sf.lambda || head_spur == sf.fn_ {
        Some(eval_lambda(args, env, None))
    } else if head_spur == sf.and {
        Some(eval_and(args, env, ctx))
    } else if head_spur == sf.case {
        Some(eval_case(args, env, ctx))
    } else if head_spur == sf.cond {
        Some(eval_cond(args, env, ctx))
    } else if head_spur == sf.define_record_type {
        Some(eval_define_record_type(args, env))
    } else if head_spur == sf.defmacro {
        Some(eval_defmacro(args, env, ctx))
    } else if head_spur == sf.defmethod {
        Some(eval_defmethod(args, env, ctx))
    } else if head_spur == sf.defmulti {
        Some(eval_defmulti(args, env, ctx))
    } else if head_spur == sf.defun || head_spur == sf.defn {
        Some(eval_defun(args, env, ctx))
    } else if head_spur == sf.delay {
        Some(eval_delay(args, env))
    } else if head_spur == sf.do_ {
        Some(eval_do(args, env, ctx))
    } else if head_spur == sf.eval {
        Some(eval_eval(args, env, ctx))
    } else if head_spur == sf.force {
        Some(eval_force(args, env, ctx))
    } else if head_spur == sf.let_star {
        Some(eval_let_star(args, env, ctx))
    } else if head_spur == sf.letrec {
        Some(eval_letrec(args, env, ctx))
    } else if head_spur == sf.macroexpand {
        Some(eval_macroexpand(args, env, ctx))
    } else if head_spur == sf.match_ {
        Some(eval_match(args, env, ctx))
    } else if head_spur == sf.or {
        Some(eval_or(args, env, ctx))
    } else if head_spur == sf.quasiquote {
        Some(eval_quasiquote(args, env, ctx))
    } else if head_spur == sf.quote {
        Some(eval_quote(args))
    } else if head_spur == sf.set_bang {
        Some(eval_set(args, env, ctx))
    } else if head_spur == sf.throw {
        Some(eval_throw(args, env, ctx))
    } else if head_spur == sf.try_ {
        Some(eval_try(args, env, ctx))
    } else if head_spur == sf.unless {
        Some(eval_unless(args, env, ctx))
    } else if head_spur == sf.when {
        Some(eval_when(args, env, ctx))
    } else if head_spur == sf.while_ {
        Some(eval_while(args, env, ctx))

    // Modules
    } else if head_spur == sf.import {
        Some(eval_import(args, env, ctx))
    } else if head_spur == sf.load {
        Some(eval_load(args, env, ctx))
    } else if head_spur == sf.module {
        Some(eval_module(args, env, ctx))

    // LLM primitives
    } else if head_spur == sf.defagent {
        Some(eval_defagent(args, env, ctx))
    } else if head_spur == sf.deftool {
        Some(eval_deftool(args, env, ctx))
    } else if head_spur == sf.message {
        Some(eval_message(args, env, ctx))
    } else if head_spur == sf.prompt {
        Some(eval_prompt(args, env, ctx))

    // Introspection
    } else if head_spur == sf.ask {
        Some(eval_ask(args, env, ctx))
    } else if head_spur == sf.ask_code {
        Some(eval_ask_code(args, env, ctx))
    } else if head_spur == sf.doc {
        Some(eval_doc(args, env))
    } else if head_spur == sf.doc_search {
        Some(eval_doc_search(args, env, ctx))
    } else if head_spur == sf.doctest {
        Some(eval_doctest(args, env, ctx))
    } else if head_spur == sf.heal {
        Some(eval_heal(args, env, ctx))
    } else if head_spur == sf.meta {
        Some(eval_meta(args, env))
    } else if head_spur == sf.source_of {
        Some(eval_source_of(args, env))

    // Runtime self-modification
    } else if head_spur == sf.become_ {
        Some(eval_become(args, env, ctx))
    } else if head_spur == sf.freeze {
        Some(eval_freeze(args, env))
    } else if head_spur == sf.history {
        Some(eval_history(args, env))
    } else if head_spur == sf.observe {
        Some(eval_observe(args, env, ctx))
    } else if head_spur == sf.rollback {
        Some(eval_rollback(args, env, ctx))
    } else {
        None
    }
}

fn eval_quote(args: &[Value]) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("quote", "1", args.len()));
    }
    Ok(Trampoline::Value(args[0].clone()))
}

fn eval_if(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(SemaError::arity("if", "2 or 3", args.len()));
    }
    let cond = eval::eval_value(ctx, &args[0], env)?;
    if cond.is_truthy() {
        Ok(Trampoline::Eval(args[1].clone(), env.clone()))
    } else if args.len() == 3 {
        Ok(Trampoline::Eval(args[2].clone(), env.clone()))
    } else {
        Ok(Trampoline::Value(Value::nil()))
    }
}

fn eval_cond(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let sf = special_forms();
    for clause in args {
        let items = clause
            .as_list()
            .ok_or_else(|| SemaError::eval("cond clause must be a list"))?;
        if items.is_empty() {
            return Err(SemaError::eval("cond clause must not be empty"));
        }
        // (else body...) or (test body...)
        let is_else = items[0].as_symbol_spur().is_some_and(|s| s == sf.else_);
        if is_else || eval::eval_value(ctx, &items[0], env)?.is_truthy() {
            if items.len() == 1 {
                return Ok(Trampoline::Value(Value::bool(true)));
            }
            // Eval all but last, tail-call last
            for expr in &items[1..items.len() - 1] {
                eval::eval_value(ctx, expr, env)?;
            }
            return Ok(Trampoline::Eval(items.last().unwrap().clone(), env.clone()));
        }
    }
    Ok(Trampoline::Value(Value::nil()))
}

fn eval_define(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("define", "2+", 0));
    }
    if let Some(spur) = args[0].as_symbol_spur() {
        // (define x expr)
        if args.len() != 2 {
            return Err(SemaError::arity("define", "2", args.len()));
        }
        let val = eval::eval_value(ctx, &args[1], env)?;
        if ctx.interactive.get() {
            if let Some(existing) = env.get(spur) {
                if existing.as_native_fn_rc().is_some() {
                    eprintln!("  warning: redefining builtin '{}'", resolve(spur));
                }
            }
        }
        env.set(spur, val);
        Ok(Trampoline::Value(Value::nil()))
    } else if let Some(sig) = args[0].as_list() {
        // (define (f x y) body...) => (define f (lambda (x y) body...))
        if sig.is_empty() {
            return Err(SemaError::eval("define: empty function signature"));
        }
        let name_spur = sig[0]
            .as_symbol_spur()
            .ok_or_else(|| SemaError::eval("define: function name must be a symbol"))?;
        let param_names: Vec<Spur> = sig[1..]
            .iter()
            .map(|v| {
                v.as_symbol_spur()
                    .ok_or_else(|| SemaError::eval("define: parameter must be a symbol"))
            })
            .collect::<Result<_, _>>()?;
        let (params, rest_param) = parse_params(&param_names);

        // Detect optional docstring: (define (f x) "doc" body...)
        let (docstring, body_start) = if args.len() > 2 && args[1].as_str().is_some() {
            (args[1].as_str().map(|s| s.to_string()), 2)
        } else {
            (None, 1)
        };

        let body = args[body_start..].to_vec();
        if body.is_empty() {
            return Err(SemaError::eval("define: function body cannot be empty"));
        }
        let lambda = Value::lambda(Lambda {
            params,
            rest_param,
            body,
            env: env.clone(),
            name: Some(name_spur),
        });
        if ctx.interactive.get() {
            if let Some(existing) = env.get(name_spur) {
                if existing.as_native_fn_rc().is_some() {
                    eprintln!("  warning: redefining builtin '{}'", resolve(name_spur));
                }
            }
        }
        env.set(name_spur, lambda);
        if let Some(ref doc) = docstring {
            env.set_meta(name_spur, Value::keyword("doc"), Value::string(doc));
        }

        // Store source text
        let mut source_parts: Vec<Value> = vec![Value::symbol("define"), args[0].clone()];
        if let Some(ref doc) = docstring {
            source_parts.push(Value::string(doc));
        }
        source_parts.extend_from_slice(&args[body_start..]);
        let source_form = Value::list(source_parts);
        env.set_meta(
            name_spur,
            Value::keyword("source"),
            Value::string(&source_form.to_string()),
        );
        if let Some(file_path) = ctx.current_file_path() {
            env.set_meta(
                name_spur,
                Value::keyword("file"),
                Value::string(&file_path.to_string_lossy()),
            );
        }

        Ok(Trampoline::Value(Value::nil()))
    } else if destructure::is_destructuring_pattern(&args[0]) {
        // (define [a b] expr) or (define {:keys [x y]} expr)
        if args.len() != 2 {
            return Err(SemaError::arity("define", "2", args.len()));
        }
        let val = eval::eval_value(ctx, &args[1], env)?;
        let binds = destructure::destructure(&args[0], &val)?;
        for (spur, v) in binds {
            env.set(spur, v);
        }
        Ok(Trampoline::Value(Value::nil()))
    } else {
        Err(SemaError::type_error(
            "symbol, list, vector, or map",
            args[0].type_name(),
        ))
    }
}

/// (defun name (params...) body...) or (defun name "doc" (params...) body...)
fn eval_defun(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defun", "3+", args.len()));
    }
    let name_spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;

    // Detect optional docstring: (defn name "doc" (params) body...)
    let (docstring, params_idx) = if args[1].as_str().is_some() && args.len() >= 3 {
        (args[1].as_str().map(|s| s.to_string()), 2)
    } else {
        (None, 1)
    };

    if params_idx >= args.len() {
        return Err(SemaError::eval(
            "defun: missing parameter list after docstring",
        ));
    }

    let name = args[0].clone();
    let params = args[params_idx]
        .as_list_rc()
        .ok_or_else(|| SemaError::type_error("list", args[params_idx].type_name()))?;
    // Build (name params...) signature list
    let mut sig = vec![name];
    sig.extend(params.iter().cloned());
    // Build transformed args: [(name params...), body...]
    let mut define_args = vec![Value::list(sig)];
    define_args.extend_from_slice(&args[params_idx + 1..]);
    let result = eval_define(&define_args, env, ctx)?;

    if let Some(ref doc) = docstring {
        env.set_meta(name_spur, Value::keyword("doc"), Value::string(doc));
    }

    // Store source text as defn form (overrides define source from eval_define)
    let mut source_parts = vec![Value::symbol("defn"), args[0].clone()];
    if let Some(ref doc) = docstring {
        source_parts.push(Value::string(doc));
    }
    source_parts.push(args[params_idx].clone());
    source_parts.extend_from_slice(&args[params_idx + 1..]);
    let source_form = Value::list(source_parts);
    env.set_meta(
        name_spur,
        Value::keyword("source"),
        Value::string(&source_form.to_string()),
    );
    if let Some(file_path) = ctx.current_file_path() {
        env.set_meta(
            name_spur,
            Value::keyword("file"),
            Value::string(&file_path.to_string_lossy()),
        );
    }

    Ok(result)
}

fn eval_set(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("set!", "2", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::eval("set!: first argument must be a symbol"))?;
    let val = eval::eval_value(ctx, &args[1], env)?;
    if !env.set_existing(spur, val) {
        let name = resolve(spur);
        let mut err = SemaError::Unbound(name.clone());
        let all_names: Vec<String> = env.all_names().iter().map(|s| resolve(*s)).collect();
        let candidates: Vec<&str> = all_names.iter().map(|s| s.as_str()).collect();
        if let Some(suggestion) = sema_core::error::suggest_similar(&name, &candidates) {
            err = err.with_hint(format!("Did you mean '{suggestion}'?"));
        }
        return Err(err);
    }
    Ok(Trampoline::Value(Value::nil()))
}

fn eval_lambda(args: &[Value], env: &Env, name: Option<Spur>) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("lambda", "2+", args.len()));
    }
    let param_list = if let Some(params) = args[0].as_list() {
        params.to_vec()
    } else if let Some(params) = args[0].as_vector() {
        params.to_vec()
    } else {
        return Err(SemaError::type_error("list or vector", args[0].type_name()));
    };

    // Check if any param needs destructuring
    let needs_destructuring = param_list.iter().any(|p| {
        let dot = intern(".");
        p.as_symbol_spur() != Some(dot) && destructure::is_destructuring_pattern(p)
    });

    if needs_destructuring {
        // Desugar: (lambda ([a b] {:keys [x]}) body...)
        // into:    (lambda (__arg0 __arg1) (let* (([a b] __arg0) ({:keys [x]} __arg1)) body...))
        let dot = intern(".");
        let mut temp_spurs = Vec::new();
        let mut let_bindings = Vec::new();
        let mut hit_dot = false;
        let mut rest_spur = None;

        for (idx, p) in param_list.iter().enumerate() {
            if let Some(s) = p.as_symbol_spur() {
                if s == dot {
                    hit_dot = true;
                    continue;
                }
                if hit_dot {
                    // Rest param after dot — keep as-is
                    rest_spur = Some(s);
                    continue;
                }
                temp_spurs.push(s);
                // No destructuring needed — no let binding
            } else {
                let temp_name = format!("__sema_arg_{idx}__");
                let temp_spur = intern(&temp_name);
                temp_spurs.push(temp_spur);
                // Build (pattern __argN) binding for let*
                let_bindings.push(Value::list(vec![p.clone(), Value::symbol(&temp_name)]));
            }
        }

        let body = if let_bindings.is_empty() {
            args[1..].to_vec()
        } else {
            // Wrap body in (let* (bindings...) body...)
            let mut let_form = vec![Value::symbol("let*"), Value::list(let_bindings)];
            let_form.extend_from_slice(&args[1..]);
            vec![Value::list(let_form)]
        };

        Ok(Trampoline::Value(Value::lambda(Lambda {
            params: temp_spurs,
            rest_param: rest_spur,
            body,
            env: env.clone(),
            name,
        })))
    } else {
        // Fast path: all params are symbols
        let param_names: Vec<Spur> = param_list
            .iter()
            .map(|v| {
                v.as_symbol_spur()
                    .ok_or_else(|| SemaError::eval("lambda: parameter must be a symbol"))
            })
            .collect::<Result<_, _>>()?;
        let (params, rest_param) = parse_params(&param_names);
        let body = args[1..].to_vec();
        Ok(Trampoline::Value(Value::lambda(Lambda {
            params,
            rest_param,
            body,
            env: env.clone(),
            name,
        })))
    }
}

fn eval_let(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("let", "2+", args.len()));
    }

    // Named let: (let name ((var init) ...) body...)
    if let Some(loop_name_spur) = args[0].as_symbol_spur() {
        if args.len() < 3 {
            return Err(SemaError::arity("named let", "3+", args.len()));
        }
        let bindings_list = args[1]
            .as_list()
            .ok_or_else(|| SemaError::eval("named let: bindings must be a list"))?;

        let mut params: Vec<Spur> = Vec::new();
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
                .as_symbol_spur()
                .ok_or_else(|| SemaError::eval("named let: binding name must be a symbol"))?;
            let val = eval::eval_value(ctx, &pair[1], env)?;
            params.push(pname);
            init_vals.push(val);
        }

        let body = args[2..].to_vec();
        let lambda = Lambda {
            params: params.clone(),
            rest_param: None,
            body,
            env: env.clone(),
            name: Some(loop_name_spur),
        };

        // Build env with params bound + self-reference
        let new_env = Env::with_parent(Rc::new(env.clone()));
        for (p, v) in params.iter().zip(init_vals.iter()) {
            new_env.set(*p, v.clone());
        }
        new_env.set(
            loop_name_spur,
            Value::lambda(Lambda {
                params: lambda.params.clone(),
                rest_param: None,
                body: lambda.body.clone(),
                env: env.clone(),
                name: lambda.name,
            }),
        );

        // Tail-call on last body expr
        let body_ref = &args[2..];
        for expr in &body_ref[..body_ref.len() - 1] {
            eval::eval_value(ctx, expr, &new_env)?;
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
        // Evaluate in the OUTER env for let (not let*)
        let val = eval::eval_value(ctx, &pair[1], env)?;
        if let Some(name_spur) = pair[0].as_symbol_spur() {
            new_env.set(name_spur, val);
        } else if destructure::is_destructuring_pattern(&pair[0]) {
            let binds = destructure::destructure(&pair[0], &val)?;
            for (spur, v) in binds {
                new_env.set(spur, v);
            }
        } else {
            return Err(SemaError::eval(
                "let: binding name must be a symbol, vector, or map pattern",
            ));
        }
    }

    // Eval body with tail call on last expr
    for expr in &args[1..args.len() - 1] {
        eval::eval_value(ctx, expr, &new_env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), new_env))
}

fn eval_let_star(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
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
        // Evaluate in the NEW env (sequential binding)
        let val = eval::eval_value(ctx, &pair[1], &new_env)?;
        if let Some(name_spur) = pair[0].as_symbol_spur() {
            new_env.set(name_spur, val);
        } else if destructure::is_destructuring_pattern(&pair[0]) {
            let binds = destructure::destructure(&pair[0], &val)?;
            for (spur, v) in binds {
                new_env.set(spur, v);
            }
        } else {
            return Err(SemaError::eval(
                "let*: binding name must be a symbol, vector, or map pattern",
            ));
        }
    }

    for expr in &args[1..args.len() - 1] {
        eval::eval_value(ctx, expr, &new_env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), new_env))
}

fn eval_letrec(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
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
        new_env.set(spur, Value::nil());
    }

    // Pass 2: evaluate init exprs in new env, update bindings
    for (i, binding) in bindings.iter().enumerate() {
        let pair = binding.as_list().unwrap();
        let val = eval::eval_value(ctx, &pair[1], &new_env)?;
        new_env.set(name_spurs[i], val);
    }

    // Eval body with tail call on last expr
    for expr in &args[1..args.len() - 1] {
        eval::eval_value(ctx, expr, &new_env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), new_env))
}

fn eval_begin(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Ok(Trampoline::Value(Value::nil()));
    }
    for expr in &args[..args.len() - 1] {
        eval::eval_value(ctx, expr, env)?;
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
}

fn eval_and(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Ok(Trampoline::Value(Value::bool(true)));
    }
    for expr in &args[..args.len() - 1] {
        let val = eval::eval_value(ctx, expr, env)?;
        if !val.is_truthy() {
            return Ok(Trampoline::Value(val));
        }
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
}

fn eval_or(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Ok(Trampoline::Value(Value::bool(false)));
    }
    for expr in &args[..args.len() - 1] {
        let val = eval::eval_value(ctx, expr, env)?;
        if val.is_truthy() {
            return Ok(Trampoline::Value(val));
        }
    }
    Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
}

fn eval_when(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("when", "2+", args.len()));
    }
    let cond = eval::eval_value(ctx, &args[0], env)?;
    if cond.is_truthy() {
        for expr in &args[1..args.len() - 1] {
            eval::eval_value(ctx, expr, env)?;
        }
        Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
    } else {
        Ok(Trampoline::Value(Value::nil()))
    }
}

fn eval_unless(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("unless", "2+", args.len()));
    }
    let cond = eval::eval_value(ctx, &args[0], env)?;
    if !cond.is_truthy() {
        for expr in &args[1..args.len() - 1] {
            eval::eval_value(ctx, expr, env)?;
        }
        Ok(Trampoline::Eval(args.last().unwrap().clone(), env.clone()))
    } else {
        Ok(Trampoline::Value(Value::nil()))
    }
}

/// (while test body ...)
fn eval_while(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("while", "2+", args.len()));
    }
    loop {
        let cond = eval::eval_value(ctx, &args[0], env)?;
        if !cond.is_truthy() {
            return Ok(Trampoline::Value(Value::nil()));
        }
        for expr in &args[1..] {
            eval::eval_value(ctx, expr, env)?;
        }
    }
}

fn eval_defmacro(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defmacro", "3+", args.len()));
    }
    let name_spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::eval("defmacro: name must be a symbol"))?;

    // Detect optional docstring: (defmacro name "doc" (params) body...)
    let (docstring, params_idx) = if args[1].as_str().is_some() && args.len() >= 4 {
        (args[1].as_str().map(|s| s.to_string()), 2)
    } else {
        (None, 1)
    };

    let param_list = args[params_idx]
        .as_list()
        .ok_or_else(|| SemaError::eval("defmacro: params must be a list"))?;
    let param_names: Vec<Spur> = param_list
        .iter()
        .map(|v| {
            v.as_symbol_spur()
                .ok_or_else(|| SemaError::eval("defmacro: parameter must be a symbol"))
        })
        .collect::<Result<_, _>>()?;
    let (params, rest_param) = parse_params(&param_names);
    let body = args[params_idx + 1..].to_vec();

    let mac = Value::macro_val(Macro {
        params,
        rest_param,
        body,
        name: name_spur,
    });
    env.set(name_spur, mac);
    if let Some(ref doc) = docstring {
        env.set_meta(name_spur, Value::keyword("doc"), Value::string(doc));
    }

    // Store source text
    let mut source_parts = vec![Value::symbol("defmacro"), args[0].clone()];
    if let Some(ref doc) = docstring {
        source_parts.push(Value::string(doc));
    }
    source_parts.push(args[params_idx].clone());
    source_parts.extend_from_slice(&args[params_idx + 1..]);
    let source_form = Value::list(source_parts);
    env.set_meta(
        name_spur,
        Value::keyword("source"),
        Value::string(&source_form.to_string()),
    );
    if let Some(file_path) = ctx.current_file_path() {
        env.set_meta(
            name_spur,
            Value::keyword("file"),
            Value::string(&file_path.to_string_lossy()),
        );
    }

    Ok(Trampoline::Value(Value::nil()))
}

/// (defmulti name dispatch-fn)
fn eval_defmulti(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("defmulti", "2", args.len()));
    }
    let name_spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::eval("defmulti: name must be a symbol"))?;
    let dispatch_fn = eval::eval_value(ctx, &args[1], env)?;
    let mm = Value::multimethod(MultiMethod {
        name: name_spur,
        dispatch_fn,
        methods: std::cell::RefCell::new(std::collections::BTreeMap::new()),
        default: std::cell::RefCell::new(None),
    });
    env.set(name_spur, mm);
    Ok(Trampoline::Value(Value::nil()))
}

/// (defmethod multi-name dispatch-value handler-fn)
fn eval_defmethod(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 3 {
        return Err(SemaError::arity("defmethod", "3", args.len()));
    }
    let name_spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::eval("defmethod: name must be a symbol"))?;
    let mm_val = env.get(name_spur).ok_or_else(|| {
        SemaError::eval(format!(
            "defmethod: '{}' is not defined",
            resolve(name_spur)
        ))
    })?;
    let mm = mm_val.as_multimethod_rc().ok_or_else(|| {
        SemaError::eval(format!(
            "defmethod: '{}' is not a multimethod",
            resolve(name_spur)
        ))
    })?;
    let dispatch_val = eval::eval_value(ctx, &args[1], env)?;
    let handler = eval::eval_value(ctx, &args[2], env)?;
    // :default sets the default handler
    if let Some(kw) = dispatch_val.as_keyword_spur() {
        if resolve(kw) == "default" {
            *mm.default.borrow_mut() = Some(handler);
            return Ok(Trampoline::Value(Value::nil()));
        }
    }
    mm.methods.borrow_mut().insert(dispatch_val, handler);
    Ok(Trampoline::Value(Value::nil()))
}

fn is_auto_gensym(sym: &str) -> bool {
    sym.len() > 1 && sym.ends_with('#') && !sym.ends_with("##")
}

fn eval_quasiquote(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("quasiquote", "1", args.len()));
    }
    let mut gensym_map: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    let result = expand_quasiquote(&args[0], env, ctx, &mut gensym_map)?;
    Ok(Trampoline::Value(result))
}

fn expand_quasiquote(
    val: &Value,
    env: &Env,
    ctx: &EvalContext,
    gensym_map: &mut std::collections::HashMap<String, String>,
) -> Result<Value, SemaError> {
    // Auto-gensym: replace foo# with a consistent gensym within this quasiquote
    if let Some(sym) = val.as_symbol() {
        if is_auto_gensym(&sym) {
            let prefix = &sym[..sym.len() - 1];
            let resolved = gensym_map
                .entry(sym.to_string())
                .or_insert_with(|| sema_core::next_gensym(prefix))
                .clone();
            return Ok(Value::symbol(&resolved));
        }
    }

    if let Some(items) = val.as_list() {
        if items.is_empty() {
            return Ok(val.clone());
        }
        // Check for (unquote x)
        if let Some(sym) = items[0].as_symbol() {
            if sym == "unquote" {
                if items.len() != 2 {
                    return Err(SemaError::arity("unquote", "1", items.len() - 1));
                }
                return eval::eval_value(ctx, &items[1], env);
            }
        }
        // Expand each element, handling splicing
        let mut result = Vec::new();
        for item in items.iter() {
            if let Some(inner) = item.as_list() {
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
                            let splice_val = eval::eval_value(ctx, &inner[1], env)?;
                            if let Some(splice_items) = splice_val.as_list() {
                                result.extend(splice_items.iter().cloned());
                            } else {
                                return Err(SemaError::type_error("list", splice_val.type_name()));
                            }
                            continue;
                        }
                    }
                }
            }
            result.push(expand_quasiquote(item, env, ctx, gensym_map)?);
        }
        Ok(Value::list(result))
    } else if let Some(items) = val.as_vector() {
        let mut result = Vec::new();
        for item in items.iter() {
            result.push(expand_quasiquote(item, env, ctx, gensym_map)?);
        }
        Ok(Value::vector(result))
    } else {
        Ok(val.clone())
    }
}

fn eval_prompt(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    use sema_core::{Message, Prompt, Role};

    let role_names = ["system", "user", "assistant", "tool"];

    let mut messages = Vec::new();
    for arg in args {
        // Check if this is a (role content...) form BEFORE evaluating
        if let Some(items) = arg.as_list() {
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
                            let part_val = eval::eval_value(ctx, part, env)?;
                            if let Some(s) = part_val.as_str() {
                                content.push_str(s);
                            } else {
                                content.push_str(&part_val.to_string());
                            }
                        }
                        messages.push(Message {
                            role,
                            content,
                            images: Vec::new(),
                        });
                        continue;
                    }
                }
            }
        }
        // Not a role form — evaluate it and check if it's a Message value
        let val = eval::eval_value(ctx, arg, env)?;
        if let Some(msg) = val.as_message_rc() {
            messages.push((*msg).clone());
        } else {
            return Err(SemaError::eval(
                "prompt: expected (role content...) or message value",
            ));
        }
    }
    Ok(Trampoline::Value(Value::prompt(Prompt { messages })))
}

fn eval_message(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    use sema_core::{Message, Role};

    if args.len() < 2 {
        return Err(SemaError::arity("message", "2+", args.len()));
    }
    let role_val = eval::eval_value(ctx, &args[0], env)?;
    let role = if let Some(spur) = role_val.as_keyword_spur() {
        let s = resolve(spur);
        match s.as_str() {
            "system" => Role::System,
            "user" => Role::User,
            "assistant" => Role::Assistant,
            "tool" => Role::Tool,
            other => return Err(SemaError::eval(format!("message: unknown role '{other}'"))),
        }
    } else {
        return Err(SemaError::type_error("keyword", role_val.type_name()));
    };
    let mut content = String::new();
    for part in &args[1..] {
        let val = eval::eval_value(ctx, part, env)?;
        if let Some(s) = val.as_str() {
            content.push_str(s);
        } else {
            content.push_str(&val.to_string());
        }
    }
    Ok(Trampoline::Value(Value::message(Message {
        role,
        content,
        images: Vec::new(),
    })))
}

/// (deftool name "description" {:param {:type :string :description "..."}} handler-expr)
fn eval_deftool(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 4 {
        return Err(SemaError::arity("deftool", "4", args.len()));
    }
    let name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("deftool: name must be a symbol"))?
        .to_string();
    let description = eval::eval_value(ctx, &args[1], env)?;
    let description = description
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", description.type_name()))?
        .to_string();
    let parameters = eval::eval_value(ctx, &args[2], env)?;
    let handler = eval::eval_value(ctx, &args[3], env)?;

    let tool = Value::tool_def(ToolDefinition {
        name: name.clone(),
        description,
        parameters,
        handler,
    });
    env.set(intern(&name), tool.clone());
    Ok(Trampoline::Value(tool))
}

/// (defagent name {:system "..." :tools [...] :max-turns N :model "..."})
fn eval_defagent(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("defagent", "2", args.len()));
    }
    let name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("defagent: name must be a symbol"))?
        .to_string();
    let opts = eval::eval_value(ctx, &args[1], env)?;
    let opts_map = opts
        .as_map_rc()
        .ok_or_else(|| SemaError::type_error("map", opts.type_name()))?;

    let system = opts_map
        .get(&Value::keyword("system"))
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_default();

    let tools = opts_map
        .get(&Value::keyword("tools"))
        .map(|v| {
            if let Some(l) = v.as_list() {
                l.to_vec()
            } else if let Some(v) = v.as_vector() {
                v.to_vec()
            } else {
                vec![]
            }
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

    let agent = Value::agent(Agent {
        name: name.clone(),
        system,
        tools,
        max_turns,
        model,
    });
    env.set(intern(&name), agent.clone());
    Ok(Trampoline::Value(agent))
}

/// (throw value) — raise a user exception
fn eval_throw(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("throw", "1", args.len()));
    }
    let val = eval::eval_value(ctx, &args[0], env)?;
    Err(SemaError::UserException(val))
}

/// (try body... (catch e handler-body...))
fn eval_try(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let sf = special_forms();
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
    let is_catch = catch_form[0]
        .as_symbol_spur()
        .is_some_and(|s| s == sf.catch);
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
        let mut result = Value::nil();
        for expr in body {
            result = eval::eval_value(ctx, expr, env)?;
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
                eval::eval_value(ctx, expr, &catch_env)?;
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
        SemaError::Type { expected, got, .. } => {
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
        SemaError::PermissionDenied {
            function,
            capability,
        } => {
            map.insert(Value::keyword("type"), Value::keyword("permission-denied"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!(
                    "Permission denied: {function} requires '{capability}' capability"
                )),
            );
            map.insert(Value::keyword("function"), Value::string(function));
            map.insert(Value::keyword("capability"), Value::string(capability));
        }
        SemaError::UserException(val) => {
            map.insert(Value::keyword("type"), Value::keyword("user"));
            map.insert(Value::keyword("message"), Value::string(&val.to_string()));
            map.insert(Value::keyword("value"), val.clone());
        }
        SemaError::PathDenied { function, path } => {
            map.insert(Value::keyword("type"), Value::keyword("permission-denied"));
            map.insert(
                Value::keyword("message"),
                Value::string(&format!(
                    "Permission denied: {function} — path '{path}' is outside allowed directories"
                )),
            );
            map.insert(Value::keyword("function"), Value::string(function));
            map.insert(Value::keyword("path"), Value::string(path));
        }
        SemaError::WithTrace { .. } => unreachable!("inner() already unwraps WithTrace"),
        SemaError::WithContext { .. } => unreachable!("inner() already unwraps WithContext"),
    }

    // Add stack trace as list of maps
    if let Some(st) = trace {
        let frames: Vec<Value> =
            st.0.iter()
                .map(|frame| {
                    let mut fm = BTreeMap::new();
                    fm.insert(Value::keyword("name"), Value::string(&frame.name));
                    if let Some(ref file) = frame.file {
                        fm.insert(
                            Value::keyword("file"),
                            Value::string(&file.to_string_lossy()),
                        );
                    }
                    if let Some(ref span) = frame.span {
                        fm.insert(Value::keyword("line"), Value::int(span.line as i64));
                        fm.insert(Value::keyword("col"), Value::int(span.col as i64));
                    }
                    Value::map(fm)
                })
                .collect();
        map.insert(Value::keyword("stack-trace"), Value::list(frames));
    }

    Value::map(map)
}

/// (module name (export sym1 sym2 ...) body...)
fn eval_module(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let sf = special_forms();
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
    if export_list.is_empty() || export_list[0].as_symbol_spur() != Some(sf.export) {
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

    ctx.set_module_exports(export_names);

    // Evaluate body
    let body = &args[2..];
    if body.is_empty() {
        return Ok(Trampoline::Value(Value::nil()));
    }
    for expr in &body[..body.len() - 1] {
        eval::eval_value(ctx, expr, env)?;
    }
    Ok(Trampoline::Eval(body.last().unwrap().clone(), env.clone()))
}

/// (import "path.sema") or (import "path.sema" sym1 sym2)
fn eval_import(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("import", "1+", 0));
    }
    let path_val = eval::eval_value(ctx, &args[0], env)?;
    let path_str = path_val
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", path_val.type_name()))?;

    // Selective import names
    let selective: Vec<String> = args[1..]
        .iter()
        .map(|v| {
            v.as_symbol()
                .map(|s| s.to_string())
                .ok_or_else(|| SemaError::eval("import: selective names must be symbols"))
        })
        .collect::<Result<_, _>>()?;

    // Check VFS first — bundled executables have packages embedded in the VFS
    // and won't have them installed on the filesystem.
    if sema_core::vfs::is_vfs_active() {
        let base_dir = ctx
            .current_file_dir()
            .map(|d| d.to_string_lossy().to_string());

        // Compute the resolved VFS path — this is the actual VFS key that
        // matched, and becomes the canonical identity for caching and
        // current_file_dir() resolution.
        //
        // Two cases:
        //   1. Direct hit: vfs_read("github.com/u/repo") or vfs_read("lib.sema")
        //      → resolved = path_str itself
        //   2. Base-dir hit: vfs_read("github.com/u/repo/helpers.sema")
        //      after joining base_dir + "helpers.sema"
        //      → resolved = base_dir/path_str
        let resolved_vfs_path = if sema_core::vfs::vfs_exists(path_str) == Some(true) {
            std::path::PathBuf::from(path_str)
        } else if let Some(ref base) = base_dir {
            std::path::Path::new(base.as_str()).join(path_str)
        } else {
            std::path::PathBuf::from(path_str)
        };

        // For package entries, the VFS key has no filename component
        // (e.g., "github.com/u/repo" or "json-utils"). We append a synthetic
        // filename so current_file_dir() returns the package directory.
        // This is only needed for direct-hit package entries, not for
        // files resolved via base_dir (those already have a filename).
        let is_direct_hit = sema_core::vfs::vfs_exists(path_str) == Some(true);
        let is_package = is_direct_hit
            && (sema_core::resolve::is_package_import(path_str)
                || (!path_str.ends_with(".sema")
                    && !path_str.starts_with("./")
                    && !path_str.starts_with("../")
                    && !path_str.starts_with('/')));
        let file_path = if is_package {
            resolved_vfs_path.join("__entry__")
        } else {
            resolved_vfs_path.clone()
        };

        // Use the resolved path as cache key — this prevents collisions
        // when two packages have identically-named internal files
        // (e.g., both have "helpers.sema" → distinct resolved paths).
        if let Some(cached) = ctx.get_cached_module(&resolved_vfs_path) {
            copy_exports_to_env(&cached, &selective, env)?;
            return Ok(Trampoline::Value(Value::nil()));
        }

        if let Some(content_bytes) =
            sema_core::vfs::vfs_resolve_and_read(path_str, base_dir.as_deref())
        {
            let content = String::from_utf8(content_bytes).map_err(|e| {
                SemaError::Io(format!("import {path_str}: invalid UTF-8 in VFS: {e}"))
            })?;

            ctx.begin_module_load(&resolved_vfs_path)?;

            let load_result: Result<std::collections::BTreeMap<String, Value>, SemaError> =
                (|| {
                    let (exprs, spans) = sema_reader::read_many_with_spans(&content)?;
                    ctx.merge_span_table(spans);

                    let module_env = eval::create_module_env(env);
                    ctx.push_file_path(file_path);
                    ctx.clear_module_exports();

                    let eval_result = (|| {
                        for expr in &exprs {
                            eval::eval_value(ctx, expr, &module_env)?;
                        }
                        Ok(())
                    })();

                    ctx.pop_file_path();
                    let declared = ctx.take_module_exports();
                    eval_result?;

                    Ok(collect_module_exports(&module_env, declared.as_deref()))
                })();

            ctx.end_module_load(&resolved_vfs_path);
            let exports = load_result?;

            ctx.cache_module(resolved_vfs_path, exports.clone());
            copy_exports_to_env(&exports, &selective, env)?;

            return Ok(Trampoline::Value(Value::nil()));
        }
    }

    // Resolve path: package imports first, then relative/absolute
    let resolved = if sema_core::resolve::is_package_import(path_str) {
        sema_core::resolve::resolve_package_import(path_str)?
    } else if std::path::Path::new(path_str).is_absolute() {
        std::path::PathBuf::from(path_str)
    } else if let Some(dir) = ctx.current_file_dir() {
        dir.join(path_str)
    } else {
        std::path::PathBuf::from(path_str)
    };

    // Check cache for preloaded modules (before canonicalize, which requires a real file).
    if let Some(cached) = ctx.get_cached_module(&resolved) {
        copy_exports_to_env(&cached, &selective, env)?;
        return Ok(Trampoline::Value(Value::nil()));
    }

    let canonical = resolved
        .canonicalize()
        .map_err(|e| SemaError::Io(format!("import {path_str}: {e}")))?;

    // Check cache for on-disk modules
    if let Some(cached) = ctx.get_cached_module(&canonical) {
        copy_exports_to_env(&cached, &selective, env)?;
        return Ok(Trampoline::Value(Value::nil()));
    }

    ctx.begin_module_load(&canonical)?;

    let load_result: Result<std::collections::BTreeMap<String, Value>, SemaError> = (|| {
        // Load and evaluate the module
        let content = std::fs::read_to_string(&canonical)
            .map_err(|e| SemaError::Io(format!("import {path_str}: {e}")))?;
        let (exprs, spans) = sema_reader::read_many_with_spans(&content)?;
        ctx.merge_span_table(spans);

        let module_env = eval::create_module_env(env);
        ctx.push_file_path(canonical.clone());
        ctx.clear_module_exports();

        let eval_result = (|| {
            for expr in &exprs {
                eval::eval_value(ctx, expr, &module_env)?;
            }
            Ok(())
        })();

        ctx.pop_file_path();

        // Always pop export scope, even if module evaluation fails.
        let declared = ctx.take_module_exports();
        eval_result?;

        Ok(collect_module_exports(&module_env, declared.as_deref()))
    })();

    ctx.end_module_load(&canonical);
    let exports = load_result?;

    // Cache
    ctx.cache_module(canonical, exports.clone());

    // Copy to caller env
    copy_exports_to_env(&exports, &selective, env)?;

    Ok(Trampoline::Value(Value::nil()))
}

/// Collect exported bindings from a module env
fn collect_module_exports(
    module_env: &Env,
    declared: Option<&[String]>,
) -> std::collections::BTreeMap<String, Value> {
    match declared {
        Some(names) => {
            let mut exports = std::collections::BTreeMap::new();
            for name in names {
                let spur = intern(name);
                if let Some(val) = module_env.get_local(spur) {
                    exports.insert(name.clone(), val);
                }
            }
            exports
        }
        None => {
            let mut exports = std::collections::BTreeMap::new();
            module_env.iter_bindings(|spur, val| {
                exports.insert(resolve(spur), val.clone());
            });
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
fn eval_load(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("load", "1", args.len()));
    }
    ctx.sandbox.check(sema_core::Caps::FS_READ, "load")?;
    let path_val = eval::eval_value(ctx, &args[0], env)?;
    let path_str = path_val
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", path_val.type_name()))?;

    // Resolve path relative to current file
    let resolved = if std::path::Path::new(path_str).is_absolute() {
        std::path::PathBuf::from(path_str)
    } else if let Some(dir) = ctx.current_file_dir() {
        dir.join(path_str)
    } else {
        std::path::PathBuf::from(path_str)
    };

    // Check VFS before hitting the filesystem
    if sema_core::vfs::is_vfs_active() {
        let base_dir = ctx
            .current_file_dir()
            .map(|d| d.to_string_lossy().to_string());
        if let Some(content_bytes) =
            sema_core::vfs::vfs_resolve_and_read(path_str, base_dir.as_deref())
        {
            let content = String::from_utf8(content_bytes).map_err(|e| {
                SemaError::Io(format!("load {path_str}: invalid UTF-8 in VFS: {e}"))
            })?;
            let (exprs, spans) = sema_reader::read_many_with_spans(&content)?;
            ctx.merge_span_table(spans);

            // Push resolved VFS path so nested load/import resolves correctly.
            // Determine which VFS key matched: direct or base-dir-relative.
            let vfs_path = if sema_core::vfs::vfs_exists(path_str) == Some(true) {
                std::path::PathBuf::from(path_str)
            } else if let Some(base) = &base_dir {
                std::path::Path::new(base.as_str()).join(path_str)
            } else {
                std::path::PathBuf::from(path_str)
            };
            ctx.push_file_path(vfs_path);

            let mut result = Value::nil();
            let eval_result = (|| {
                for expr in &exprs {
                    result = eval::eval_value(ctx, expr, env)?;
                }
                Ok(result.clone())
            })();

            ctx.pop_file_path();
            eval_result?;
            return Ok(Trampoline::Value(result));
        }
    }

    let content = std::fs::read_to_string(&resolved)
        .map_err(|e| SemaError::Io(format!("load {}: {e}", resolved.display())))?;
    let (exprs, spans) = sema_reader::read_many_with_spans(&content)?;
    ctx.merge_span_table(spans);

    // Push the file path so nested load/import resolves correctly
    let canonical = resolved.canonicalize().ok();
    if let Some(path) = canonical.clone() {
        ctx.push_file_path(path);
    }

    let mut result = Value::nil();
    let eval_result = (|| {
        for expr in &exprs {
            result = eval::eval_value(ctx, expr, env)?;
        }
        Ok(result.clone())
    })();

    // Pop regardless of success/failure
    if canonical.is_some() {
        ctx.pop_file_path();
    }

    eval_result?;
    Ok(Trampoline::Value(result))
}

/// (case key-expr ((datum ...) body ...) ... (else body ...))
fn eval_case(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    let sf = special_forms();
    if args.len() < 2 {
        return Err(SemaError::arity("case", "2+", args.len()));
    }
    let key = eval::eval_value(ctx, &args[0], env)?;

    for clause in &args[1..] {
        let items = clause
            .as_list()
            .ok_or_else(|| SemaError::eval("case: clause must be a list"))?;
        if items.is_empty() {
            return Err(SemaError::eval("case: clause must not be empty"));
        }
        // (else body...)
        let is_else = items[0].as_symbol_spur() == Some(sf.else_);
        if is_else {
            if items.len() == 1 {
                return Ok(Trampoline::Value(Value::nil()));
            }
            for expr in &items[1..items.len() - 1] {
                eval::eval_value(ctx, expr, env)?;
            }
            return Ok(Trampoline::Eval(items.last().unwrap().clone(), env.clone()));
        }
        // ((datum ...) body...)
        let datums = items[0]
            .as_list()
            .ok_or_else(|| SemaError::eval("case: datums must be a list"))?;
        if datums.contains(&key) {
            if items.len() == 1 {
                return Ok(Trampoline::Value(Value::nil()));
            }
            for expr in &items[1..items.len() - 1] {
                eval::eval_value(ctx, expr, env)?;
            }
            return Ok(Trampoline::Eval(items.last().unwrap().clone(), env.clone()));
        }
    }
    Ok(Trampoline::Value(Value::nil()))
}

/// (match expr [pattern body...] [pattern when guard body...] ...)
fn eval_match(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("match", "2+", args.len()));
    }
    let val = eval::eval_value(ctx, &args[0], env)?;
    let when_spur = intern("when");

    for clause in &args[1..] {
        let items = if let Some(l) = clause.as_list() {
            l
        } else if let Some(v) = clause.as_vector() {
            v
        } else {
            return Err(
                SemaError::eval("match: each clause must be a list or vector")
                    .with_hint("e.g. (match x (1 \"one\") (_ \"other\"))"),
            );
        };

        if items.is_empty() {
            return Err(SemaError::eval("match: clause must not be empty"));
        }

        let pattern = &items[0];

        // Check for guard: [pattern when guard body...]
        let (has_guard, guard_idx) = if items.len() >= 3 {
            if let Some(s) = items[1].as_symbol_spur() {
                if s == when_spur {
                    (true, 2)
                } else {
                    (false, 0)
                }
            } else {
                (false, 0)
            }
        } else {
            (false, 0)
        };

        let body_start = if has_guard { guard_idx + 1 } else { 1 };

        if let Some(bindings) = destructure::try_match(pattern, &val)? {
            let match_env = Env::with_parent(Rc::new(env.clone()));
            for (spur, v) in &bindings {
                match_env.set(*spur, v.clone());
            }

            // Evaluate guard if present
            if has_guard {
                let guard_val = eval::eval_value(ctx, &items[guard_idx], &match_env)?;
                if guard_val.is_falsy() {
                    continue;
                }
            }

            if body_start >= items.len() {
                return Ok(Trampoline::Value(Value::nil()));
            }

            // Eval body with TCO on last expression
            for expr in &items[body_start..items.len() - 1] {
                eval::eval_value(ctx, expr, &match_env)?;
            }
            return Ok(Trampoline::Eval(items.last().unwrap().clone(), match_env));
        }
    }

    // No clause matched
    Ok(Trampoline::Value(Value::nil()))
}

/// (eval expr) — evaluate a quoted expression
fn eval_eval(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("eval", "1", args.len()));
    }
    let expr = eval::eval_value(ctx, &args[0], env)?;
    Ok(Trampoline::Eval(expr, env.clone()))
}

/// (macroexpand '(macro-call args...)) — expand a macro once without evaluating
fn eval_macroexpand(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("macroexpand", "1", args.len()));
    }
    let form = eval::eval_value(ctx, &args[0], env)?;
    // Check if it's a list starting with a macro name
    if let Some(items) = form.as_list() {
        if !items.is_empty() {
            if let Some(spur) = items[0].as_symbol_spur() {
                if let Some(mac_val) = env.get(spur) {
                    if let Some(mac) = mac_val.as_macro_rc() {
                        let expanded = eval::apply_macro(ctx, &mac, &items[1..], env)?;
                        return Ok(Trampoline::Value(expanded));
                    }
                }
            }
        }
    }
    // Not a macro form — return as-is
    Ok(Trampoline::Value(form))
}

/// (do ((var init step) ...) (test result ...) body ...)
fn eval_do(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("do", "2+", args.len()));
    }

    let bindings = args[0]
        .as_list()
        .ok_or_else(|| SemaError::eval("do: bindings must be a list"))?;
    let test_clause = args[1]
        .as_list()
        .ok_or_else(|| SemaError::eval("do: test clause must be a list"))?;
    if test_clause.is_empty() {
        return Err(SemaError::eval("do: test clause must not be empty"));
    }
    let body = &args[2..];

    let mut var_names = Vec::new();
    let mut step_exprs: Vec<Option<Value>> = Vec::new();

    let loop_env = Env::with_parent(Rc::new(env.clone()));

    for binding in bindings {
        let parts = binding
            .as_list()
            .ok_or_else(|| SemaError::eval("do: each binding must be a list"))?;
        if parts.len() < 2 || parts.len() > 3 {
            return Err(SemaError::eval(
                "do: binding must be (var init) or (var init step)",
            ));
        }
        let name_spur = parts[0]
            .as_symbol_spur()
            .ok_or_else(|| SemaError::eval("do: variable name must be a symbol"))?;
        let init_val = eval::eval_value(ctx, &parts[1], env)?;
        let step = if parts.len() == 3 {
            Some(parts[2].clone())
        } else {
            None
        };
        loop_env.set(name_spur, init_val);
        var_names.push(name_spur);
        step_exprs.push(step);
    }

    let mut new_vals: Vec<Option<Value>> = vec![None; var_names.len()];
    loop {
        let test_result = eval::eval_value(ctx, &test_clause[0], &loop_env)?;
        if test_result.is_truthy() {
            if test_clause.len() == 1 {
                return Ok(Trampoline::Value(Value::nil()));
            }
            for expr in &test_clause[1..test_clause.len() - 1] {
                eval::eval_value(ctx, expr, &loop_env)?;
            }
            return Ok(Trampoline::Eval(
                test_clause.last().unwrap().clone(),
                loop_env,
            ));
        }

        for expr in body {
            eval::eval_value(ctx, expr, &loop_env)?;
        }

        // Compute all step values before updating (parallel assignment)
        for (i, step) in step_exprs.iter().enumerate() {
            new_vals[i] = match step {
                Some(expr) => Some(eval::eval_value(ctx, expr, &loop_env)?),
                None => None,
            };
        }

        for (i, name) in var_names.iter().enumerate() {
            if let Some(val) = new_vals[i].take() {
                loop_env.set(*name, val);
            }
        }
    }
}

/// (delay expr) — create a lazy promise
fn eval_delay(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("delay", "1", args.len()));
    }
    let lambda = Value::lambda(Lambda {
        params: vec![],
        rest_param: None,
        body: vec![args[0].clone()],
        env: env.clone(),
        name: None,
    });
    let thunk = Thunk {
        body: lambda,
        forced: std::cell::RefCell::new(None),
    };
    Ok(Trampoline::Value(Value::thunk(thunk)))
}

/// (force promise) — evaluate a promise, memoizing the result
fn eval_force(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("force", "1", args.len()));
    }
    let val = eval::eval_value(ctx, &args[0], env)?;
    if let Some(thunk) = val.as_thunk_rc() {
        if let Some(cached) = &*thunk.forced.borrow() {
            return Ok(Trampoline::Value(cached.clone()));
        }
        let result = if let Some(lambda) = thunk.body.as_lambda_rc() {
            let thunk_env = Env::with_parent(Rc::new(lambda.env.clone()));
            let mut res = Value::nil();
            for expr in &lambda.body {
                res = eval::eval_value(ctx, expr, &thunk_env)?;
            }
            res
        } else {
            eval::eval_value(ctx, &thunk.body, env)?
        };
        *thunk.forced.borrow_mut() = Some(result.clone());
        Ok(Trampoline::Value(result))
    } else {
        Ok(Trampoline::Value(val))
    }
}

/// (define-record-type <name> (<ctor> <field> ...) <pred> (<field> <accessor> [<mutator>]) ...)
fn eval_define_record_type(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::eval(
            "define-record-type: requires at least type name, constructor, and predicate",
        ));
    }

    let type_name = args[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("define-record-type: type name must be a symbol"))?;
    let type_tag = intern(&type_name);

    let ctor_spec = args[1]
        .as_list()
        .ok_or_else(|| SemaError::eval("define-record-type: constructor spec must be a list"))?;
    if ctor_spec.is_empty() {
        return Err(SemaError::eval(
            "define-record-type: constructor spec must have a name",
        ));
    }
    let ctor_name = ctor_spec[0]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("define-record-type: constructor name must be a symbol"))?;
    let field_names: Vec<String> = ctor_spec[1..]
        .iter()
        .map(|v| {
            v.as_symbol()
                .ok_or_else(|| SemaError::eval("define-record-type: field name must be a symbol"))
        })
        .collect::<Result<_, _>>()?;
    let field_count = field_names.len();

    let pred_name = args[2]
        .as_symbol()
        .ok_or_else(|| SemaError::eval("define-record-type: predicate must be a symbol"))?;

    let ctor_name_clone = ctor_name.clone();
    env.set_str(
        &ctor_name,
        Value::native_fn(sema_core::NativeFn::simple(
            ctor_name.clone(),
            move |args: &[Value]| {
                if args.len() != field_count {
                    return Err(SemaError::arity(
                        &ctor_name_clone,
                        field_count.to_string(),
                        args.len(),
                    ));
                }
                Ok(Value::record(Record {
                    type_tag,
                    fields: args.to_vec(),
                }))
            },
        )),
    );

    let pred_name_for_closure = pred_name.clone();
    let pred_name_for_set = pred_name.clone();
    env.set_str(
        &pred_name_for_set,
        Value::native_fn(sema_core::NativeFn::simple(
            pred_name,
            move |args: &[Value]| {
                if args.len() != 1 {
                    return Err(SemaError::arity(&pred_name_for_closure, "1", args.len()));
                }
                Ok(Value::bool(
                    args[0].as_record().is_some_and(|r| r.type_tag == type_tag),
                ))
            },
        )),
    );

    for field_spec_val in &args[3..] {
        let field_spec = field_spec_val
            .as_list()
            .ok_or_else(|| SemaError::eval("define-record-type: field spec must be a list"))?;
        if field_spec.len() < 2 {
            return Err(SemaError::eval(
                "define-record-type: field spec must have at least (field-name accessor)",
            ));
        }

        let field_name = field_spec[0]
            .as_symbol()
            .ok_or_else(|| SemaError::eval("define-record-type: field name must be a symbol"))?;

        let field_idx = field_names
            .iter()
            .position(|n| n == &field_name)
            .ok_or_else(|| {
                SemaError::eval(format!(
                    "define-record-type: field '{field_name}' not in constructor"
                ))
            })?;

        let accessor_name = field_spec[1]
            .as_symbol()
            .ok_or_else(|| SemaError::eval("define-record-type: accessor must be a symbol"))?;

        let accessor_name_for_closure = accessor_name.clone();
        let accessor_name_for_set = accessor_name.clone();
        let type_name_for_err = type_name.clone();
        env.set_str(
            &accessor_name_for_set,
            Value::native_fn(sema_core::NativeFn::simple(
                accessor_name,
                move |args: &[Value]| {
                    if args.len() != 1 {
                        return Err(SemaError::arity(
                            &accessor_name_for_closure,
                            "1",
                            args.len(),
                        ));
                    }
                    match args[0].as_record() {
                        Some(r) if r.type_tag == type_tag => Ok(r.fields[field_idx].clone()),
                        _ => Err(SemaError::type_error(
                            &type_name_for_err,
                            args[0].type_name(),
                        )),
                    }
                },
            )),
        );
    }

    Ok(Trampoline::Value(Value::nil()))
}

// ── Introspection special forms ───────────────────────────────────

/// Gather context string for the `ask` family of special forms.
fn gather_ask_context(target: &Value, env: &Env) -> String {
    if let Some(lambda) = target.as_lambda_rc() {
        let mut ctx = String::new();
        let name = lambda
            .name
            .map(resolve)
            .unwrap_or_else(|| "<anonymous>".to_string());
        ctx.push_str(&format!("Function: {name}\n"));

        let params: Vec<String> = lambda.params.iter().map(|s| resolve(*s)).collect();
        ctx.push_str(&format!("Parameters: ({})\n", params.join(" ")));

        if let Some(name_spur) = lambda.name {
            if let Some(meta) = env.get_meta(name_spur) {
                if let Some(doc) = meta.get(&Value::keyword("doc")) {
                    if let Some(s) = doc.as_str() {
                        ctx.push_str(&format!("Documentation:\n{s}\n"));
                    }
                }
                if let Some(source) = meta.get(&Value::keyword("source")) {
                    if let Some(s) = source.as_str() {
                        ctx.push_str(&format!("Source code:\n{s}\n"));
                    }
                }
            }
        }
        ctx
    } else if let Some(s) = target.as_str() {
        if s.ends_with(".sema") {
            match std::fs::read_to_string(s) {
                Ok(content) => format!("File: {s}\nContents:\n{content}\n"),
                Err(e) => format!("File: {s}\nError reading: {e}\n"),
            }
        } else {
            format!("Value: {s}\n")
        }
    } else {
        let repr = target.to_string();
        let truncated = if repr.len() > 4000 {
            format!("{}... (truncated)", &repr[..4000])
        } else {
            repr
        };
        format!("Value ({}):\n{truncated}\n", target.type_name())
    }
}

/// Call llm/complete through the env binding.
fn call_llm_complete(
    env: &Env,
    ctx: &EvalContext,
    prompt: &str,
    system: &str,
) -> Result<Value, SemaError> {
    let llm_complete_spur = intern("llm/complete");
    let llm_fn = env.get(llm_complete_spur).ok_or_else(|| {
        SemaError::eval(
            "ask: llm/complete not available. Configure an LLM provider first with (llm/configure ...)",
        )
        .with_hint(
            "Example: (llm/configure :anthropic {:api-key (env/get \"ANTHROPIC_API_KEY\")})",
        )
    })?;

    let mut opts = BTreeMap::new();
    opts.insert(Value::keyword("system"), Value::string(system));

    let args = vec![Value::string(prompt), Value::map(opts)];
    sema_core::call_callback(ctx, &llm_fn, &args)
}

/// (ask target question) — ask a question about any value
fn eval_ask(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("ask", "2", args.len()));
    }
    let target = eval::eval_value(ctx, &args[0], env)?;
    let question = eval::eval_value(ctx, &args[1], env)?;
    let question_str = question
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", question.type_name()))?;

    let context = gather_ask_context(&target, env);

    let system = "You are a Sema language expert. You have been given source code and metadata \
                  for a Sema value. Answer the user's question about it. If suggesting code \
                  changes, write valid Sema. Be concise.";

    let prompt = format!("{context}\n\nQuestion: {question_str}");

    let result = call_llm_complete(env, ctx, &prompt, system)?;
    Ok(Trampoline::Value(result))
}

/// (ask/code target instruction) — get executable Sema code from LLM
fn eval_ask_code(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("ask/code", "2", args.len()));
    }
    let target = eval::eval_value(ctx, &args[0], env)?;
    let instruction = eval::eval_value(ctx, &args[1], env)?;
    let instruction_str = instruction
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", instruction.type_name()))?;

    let context = gather_ask_context(&target, env);

    let system = "You are a Sema language expert. Given source code and an instruction, \
                  respond with ONLY valid Sema code. No markdown, no explanation, no code \
                  fences. Just the Sema expression.";

    let prompt = format!("{context}\n\nInstruction: {instruction_str}");

    let response = call_llm_complete(env, ctx, &prompt, system)?;
    let response_str = response
        .as_str()
        .ok_or_else(|| SemaError::eval("ask/code: LLM did not return a string"))?;

    let code = response_str
        .trim()
        .strip_prefix("```lisp")
        .or_else(|| response_str.trim().strip_prefix("```"))
        .unwrap_or(response_str.trim());
    let code = code.strip_suffix("```").unwrap_or(code).trim();

    match sema_reader::read(code) {
        Ok(expr) => Ok(Trampoline::Value(expr)),
        Err(e) => Err(SemaError::eval(format!(
            "ask/code: failed to parse LLM response as Sema: {e}\nResponse was: {code}"
        ))),
    }
}

/// (heal! sym) or (heal! sym {:max-attempts N}) — use LLM to fix a function so its doctests pass
fn eval_heal(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.is_empty() || args.len() > 2 {
        return Err(SemaError::arity("heal!", "1-2", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);

    // Get options
    let opts = if args.len() > 1 {
        eval::eval_value(ctx, &args[1], env)?
    } else {
        Value::nil()
    };
    let max_attempts = opts
        .as_map_rc()
        .and_then(|m| m.get(&Value::keyword("max-attempts")).cloned())
        .and_then(|v| v.as_int())
        .unwrap_or(3) as usize;

    // Get docstring
    let docstring = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("doc")).cloned())
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .ok_or_else(|| {
            SemaError::eval(format!("heal!: '{name}' has no docstring"))
                .with_hint("Add a docstring with >>> examples to use heal!")
        })?;

    // Check for actual doctests
    let doctests = crate::doctest::parse_doctests(&docstring);
    if doctests.is_empty() {
        return Err(SemaError::eval(format!("heal!: '{name}' has no doctests"))
            .with_hint("Add >>> examples to the docstring"));
    }

    // Get source
    let source = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("source")).cloned())
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .unwrap_or_else(|| format!("<source not available for '{name}'>"));

    // Run doctests first to see what's failing
    let child_env = Env::with_parent(Rc::new(env.clone()));
    let initial_results = crate::doctest::run_doctests(&docstring, &mut |input| {
        eval::eval_string(ctx, input, &child_env)
    });

    let total = initial_results.len();
    let passing = initial_results.iter().filter(|r| r.passed).count();

    if passing == total {
        let mut map = BTreeMap::new();
        map.insert(Value::keyword("status"), Value::keyword("ok"));
        map.insert(
            Value::keyword("message"),
            Value::string(&format!("All {total} doctests already pass")),
        );
        return Ok(Trampoline::Value(Value::map(map)));
    }

    // Build failure report
    let mut failure_report = String::new();
    for r in &initial_results {
        if r.passed {
            failure_report.push_str(&format!("  ✓ {} => {}\n", r.input, r.expected));
        } else {
            failure_report.push_str(&format!(
                "  ✗ {} => expected {}, got {}\n",
                r.input, r.expected, r.actual
            ));
        }
    }

    eprintln!("{name}: {passing}/{total} doctests passing. Healing...");

    // Healing loop
    let system = "You are writing Sema (a Lisp dialect). Fix this function to pass all its \
                  doctests. Return ONLY the fixed (defn ...) form. No markdown, no explanation, \
                  no code fences. Just the Sema expression. Keep the docstring intact.";

    for attempt in 1..=max_attempts {
        let prompt = format!(
            "Function source:\n{source}\n\nDoctest results:\n{failure_report}\n\
             Fix the implementation to pass all doctests. Return only the complete (defn ...) form."
        );

        let response = call_llm_complete(env, ctx, &prompt, system)?;
        let response_str = response
            .as_str()
            .ok_or_else(|| SemaError::eval("heal!: LLM did not return a string"))?;

        // Strip code fences if present
        let code = response_str
            .trim()
            .strip_prefix("```lisp")
            .or_else(|| response_str.trim().strip_prefix("```sema"))
            .or_else(|| response_str.trim().strip_prefix("```"))
            .unwrap_or(response_str.trim());
        let code = code.strip_suffix("```").unwrap_or(code).trim();

        eprintln!("  Attempt {attempt}: parsing candidate...");

        // Parse the candidate
        let candidate_expr = match sema_reader::read(code) {
            Ok(expr) => expr,
            Err(e) => {
                eprintln!("  Attempt {attempt}: parse error: {e}");
                continue;
            }
        };

        // Eval in sandbox
        let sandbox_env = Env::with_parent(Rc::new(env.clone()));
        if let Err(e) = eval::eval_value(ctx, &candidate_expr, &sandbox_env) {
            eprintln!("  Attempt {attempt}: eval error: {e}");
            continue;
        }

        // Run doctests against the candidate
        let candidate_results = crate::doctest::run_doctests(&docstring, &mut |input| {
            eval::eval_string(ctx, input, &sandbox_env)
        });

        let candidate_passing = candidate_results.iter().filter(|r| r.passed).count();
        let candidate_total = candidate_results.len();

        if candidate_passing == candidate_total {
            eprintln!("  ✓ Healed! {candidate_total}/{candidate_total} doctests pass.");
            eprintln!("  New implementation:\n  {code}");

            // Apply: eval the candidate in the real env
            eval::eval_value(ctx, &candidate_expr, env)?;

            let mut map = BTreeMap::new();
            map.insert(Value::keyword("status"), Value::keyword("healed"));
            map.insert(Value::keyword("attempts"), Value::int(attempt as i64));
            map.insert(Value::keyword("source"), Value::string(code));
            return Ok(Trampoline::Value(Value::map(map)));
        } else {
            eprintln!(
                "  Attempt {attempt}: {candidate_passing}/{candidate_total} passing (not enough)"
            );
        }
    }

    Err(SemaError::eval(format!(
        "heal!: failed to heal '{name}' after {max_attempts} attempts"
    )))
}

/// (meta sym) — return metadata map for a symbol
fn eval_meta(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("meta", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;

    let mut result = BTreeMap::new();

    // Add stored metadata (doc, etc.)
    if let Some(meta) = env.get_meta(spur) {
        result.extend(meta);
    }

    // Add name
    result.insert(Value::keyword("name"), Value::string(&resolve(spur)));

    // Add type and arity info from the value itself
    if let Some(val) = env.get(spur) {
        result.insert(Value::keyword("type"), Value::keyword(val.type_name()));
        if let Some(lambda) = val.as_lambda_rc() {
            let param_names: Vec<Value> = lambda
                .params
                .iter()
                .map(|s| Value::symbol(&resolve(*s)))
                .collect();
            result.insert(Value::keyword("params"), Value::list(param_names));
            result.insert(
                Value::keyword("arity"),
                Value::int(lambda.params.len() as i64),
            );
            if lambda.rest_param.is_some() {
                result.insert(Value::keyword("variadic?"), Value::bool(true));
            }
        } else if let Some(mac) = val.as_macro_rc() {
            let param_names: Vec<Value> = mac
                .params
                .iter()
                .map(|s| Value::symbol(&resolve(*s)))
                .collect();
            result.insert(Value::keyword("params"), Value::list(param_names));
            result.insert(Value::keyword("arity"), Value::int(mac.params.len() as i64));
            if mac.rest_param.is_some() {
                result.insert(Value::keyword("variadic?"), Value::bool(true));
            }
        }
    }

    if result.is_empty() {
        Ok(Trampoline::Value(Value::nil()))
    } else {
        Ok(Trampoline::Value(Value::map(result)))
    }
}

/// (source-of sym) — return the source text of a definition
fn eval_source_of(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("source-of", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let source = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("source")).cloned())
        .unwrap_or_else(Value::nil);
    Ok(Trampoline::Value(source))
}

/// (doc sym) — pretty-print documentation for a symbol
fn eval_doc(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("doc", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);

    let mut output = String::new();

    // Function signature
    if let Some(val) = env.get(spur) {
        if let Some(lambda) = val.as_lambda_rc() {
            let params: Vec<String> = lambda.params.iter().map(|s| resolve(*s)).collect();
            let params_str = params.join(" ");
            if let Some(rest) = &lambda.rest_param {
                output.push_str(&format!(
                    "{name} : (fn {params_str} . {})\n",
                    resolve(*rest)
                ));
            } else {
                output.push_str(&format!("{name} : (fn {params_str})\n"));
            }
        } else if val.as_native_fn_rc().is_some() {
            output.push_str(&format!("{name} : <native function>\n"));
        } else if val.as_macro_rc().is_some() {
            output.push_str(&format!("{name} : <macro>\n"));
        }
    }

    // Docstring
    if let Some(meta) = env.get_meta(spur) {
        if let Some(doc) = meta.get(&Value::keyword("doc")) {
            if let Some(s) = doc.as_str() {
                output.push('\n');
                output.push_str(s);
                output.push('\n');
            }
        }
    }

    if output.is_empty() {
        output = format!("No documentation for '{name}'.\n");
    }

    eprint!("{output}");
    Ok(Trampoline::Value(Value::nil()))
}

/// (doc/search "query") — search all bindings by name or docstring content
fn eval_doc_search(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("doc/search", "1", args.len()));
    }
    let query_val = eval::eval_value(ctx, &args[0], env)?;
    let query = query_val
        .as_str()
        .ok_or_else(|| SemaError::type_error("string", query_val.type_name()))?
        .to_lowercase();

    let mut results = Vec::new();
    for spur in env.all_names() {
        let name = resolve(spur);
        let name_lower = name.to_lowercase();
        let mut matches = name_lower.contains(&query);

        if !matches {
            if let Some(meta) = env.get_meta(spur) {
                if let Some(doc) = meta.get(&Value::keyword("doc")) {
                    if let Some(s) = doc.as_str() {
                        matches = s.to_lowercase().contains(&query);
                    }
                }
            }
        }

        if matches {
            let mut entry = BTreeMap::new();
            entry.insert(Value::keyword("name"), Value::string(&name));
            if let Some(meta) = env.get_meta(spur) {
                if let Some(doc) = meta.get(&Value::keyword("doc")) {
                    entry.insert(Value::keyword("doc"), doc.clone());
                }
            }
            results.push(Value::map(entry));
        }
    }

    Ok(Trampoline::Value(Value::list(results)))
}

/// (doctest sym) — run doctests from a symbol's docstring
fn eval_doctest(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("doctest", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);

    // Get docstring from metadata
    let docstring = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("doc")).cloned())
        .and_then(|v| v.as_str().map(|s| s.to_string()))
        .ok_or_else(|| SemaError::eval(format!("doctest: no docstring for '{name}'")))?;

    // Create child env for isolated evaluation
    let child_env = Env::with_parent(Rc::new(env.clone()));

    // Run doctests
    let results = crate::doctest::run_doctests(&docstring, &mut |input| {
        eval::eval_string(ctx, input, &child_env)
    });

    let total = results.len() as i64;
    let passed = results.iter().filter(|r| r.passed).count() as i64;

    // Print results
    for r in &results {
        if r.passed {
            eprintln!("  ✓ {} => {}", r.input, r.expected);
        } else {
            eprintln!(
                "  ✗ {} => expected {}, got {}",
                r.input, r.expected, r.actual
            );
        }
    }

    let mut map = BTreeMap::new();
    map.insert(Value::keyword("passed"), Value::int(passed));
    map.insert(Value::keyword("total"), Value::int(total));
    map.insert(Value::keyword("name"), Value::string(&name));
    Ok(Trampoline::Value(Value::map(map)))
}

// ── Runtime self-modification special forms ───────────────────────

/// (observe! target sample-size callback)
/// Wraps target function with a logger. After sample-size calls, invokes callback with call log
/// and restores the original function.
fn eval_observe(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 3 {
        return Err(SemaError::arity("observe!", "3", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let sample_size = eval::eval_value(ctx, &args[1], env)?;
    let sample_n = sample_size
        .as_int()
        .ok_or_else(|| SemaError::type_error("integer", sample_size.type_name()))?
        as usize;
    let callback = eval::eval_value(ctx, &args[2], env)?;

    let original = env
        .get(spur)
        .ok_or_else(|| SemaError::eval(format!("observe!: '{}' is not defined", resolve(spur))))?;

    let call_log: Rc<RefCell<Vec<Value>>> = Rc::new(RefCell::new(Vec::new()));
    let call_count = Rc::new(Cell::new(0usize));
    let original_for_call = original.clone();
    let original_for_restore = original.clone();
    let callback_clone = callback;
    let log_clone = call_log.clone();
    let count_clone = call_count;
    let env_clone = env.clone();
    let spur_copy = spur;

    let wrapper_name = format!("{}/observed", resolve(spur));
    let wrapper = Value::native_fn(sema_core::NativeFn::with_ctx(
        wrapper_name,
        move |ctx, call_args| {
            let start = std::time::Instant::now();
            let result = sema_core::call_callback(ctx, &original_for_call, call_args)?;
            let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;

            let mut entry = BTreeMap::new();
            entry.insert(Value::keyword("args"), Value::list(call_args.to_vec()));
            entry.insert(Value::keyword("result"), result.clone());
            entry.insert(Value::keyword("time-ms"), Value::float(elapsed_ms));
            log_clone.borrow_mut().push(Value::map(entry));

            let n = count_clone.get() + 1;
            count_clone.set(n);

            if n >= sample_n {
                let log_entries = log_clone.borrow().clone();
                env_clone.set(spur_copy, original_for_restore.clone());
                sema_core::call_callback(ctx, &callback_clone, &[Value::list(log_entries)])?;
            }

            Ok(result)
        },
    ));

    env.set(spur, wrapper);
    Ok(Trampoline::Value(Value::nil()))
}

/// (become! target new-definition)
/// Replace a function's definition at runtime. Validates doctests if present.
/// Saves old version to history metadata.
fn eval_become(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("become!", "2", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);

    // Check if frozen
    if let Some(meta) = env.get_meta(spur) {
        if meta.get(&Value::keyword("frozen?")) == Some(&Value::bool(true)) {
            return Err(SemaError::eval(format!(
                "become!: '{name}' is frozen and cannot be modified"
            ))
            .with_hint("Use (history <name>) to inspect previous versions"));
        }
    }

    // Get the current value
    let current = env
        .get(spur)
        .ok_or_else(|| SemaError::eval(format!("become!: '{name}' is not defined")))?;

    // Evaluate the new definition
    let new_def = eval::eval_value(ctx, &args[1], env)?;

    // If the function has doctests, validate the candidate passes them.
    // Temporarily install new_def so recursive calls see the new version.
    if let Some(meta) = env.get_meta(spur) {
        if let Some(doc_val) = meta.get(&Value::keyword("doc")) {
            if let Some(docstring) = doc_val.as_str() {
                let doctests = crate::doctest::parse_doctests(docstring);
                if !doctests.is_empty() {
                    // Temporarily swap in the new definition so recursive calls
                    // resolve to the candidate, not the old version.
                    env.set(spur, new_def.clone());

                    let child_env = Env::with_parent(Rc::new(env.clone()));
                    let results = crate::doctest::run_doctests(docstring, &mut |input| {
                        eval::eval_string(ctx, input, &child_env)
                    });

                    let total = results.len();
                    let passed = results.iter().filter(|r| r.passed).count();

                    if passed < total {
                        // Restore the old definition on failure
                        env.set(spur, current.clone());

                        let mut failures = String::new();
                        for r in &results {
                            if !r.passed {
                                failures.push_str(&format!(
                                    "\n  {} => expected {}, got {}",
                                    r.input, r.expected, r.actual
                                ));
                            }
                        }
                        return Err(SemaError::eval(format!(
                            "become!: candidate for '{name}' fails {}/{total} doctests:{failures}",
                            total - passed
                        )));
                    }

                    // Doctests passed — restore current so history save below captures it
                    env.set(spur, current.clone());
                }
            }
        }
    }

    // Save current version to history
    let history_key = Value::keyword("history");
    let mut history_list: Vec<Value> = env
        .get_meta(spur)
        .and_then(|m| m.get(&history_key).cloned())
        .and_then(|v| v.as_list().map(|l| l.to_vec()))
        .unwrap_or_default();

    let mut version_entry = BTreeMap::new();
    version_entry.insert(
        Value::keyword("version"),
        Value::int(history_list.len() as i64 + 1),
    );
    version_entry.insert(Value::keyword("value"), current);
    if let Some(meta) = env.get_meta(spur) {
        if let Some(source) = meta.get(&Value::keyword("source")) {
            version_entry.insert(Value::keyword("source"), source.clone());
        }
    }
    history_list.push(Value::map(version_entry));
    env.set_meta(spur, history_key, Value::list(history_list));

    // Install the new definition and update source metadata
    env.set(spur, new_def);
    env.set_meta(
        spur,
        Value::keyword("source"),
        Value::string(&args[1].to_string()),
    );

    Ok(Trampoline::Value(Value::nil()))
}

/// (history name) — return the history list for a binding
fn eval_history(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("history", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let history_key = Value::keyword("history");
    let history = env
        .get_meta(spur)
        .and_then(|m| m.get(&history_key).cloned())
        .unwrap_or_else(|| Value::list(vec![]));
    Ok(Trampoline::Value(history))
}

/// (rollback! sym version-number) — restore a previous version from history
fn eval_rollback(args: &[Value], env: &Env, ctx: &EvalContext) -> Result<Trampoline, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("rollback!", "2", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;
    let name = resolve(spur);
    let version_val = eval::eval_value(ctx, &args[1], env)?;
    let version = version_val
        .as_int()
        .ok_or_else(|| SemaError::type_error("integer", version_val.type_name()))?;

    // Check if frozen
    if let Some(meta) = env.get_meta(spur) {
        if meta.get(&Value::keyword("frozen?")) == Some(&Value::bool(true)) {
            return Err(SemaError::eval(format!(
                "rollback!: '{name}' is frozen and cannot be modified"
            )));
        }
    }

    let history_list = env
        .get_meta(spur)
        .and_then(|m| m.get(&Value::keyword("history")).cloned())
        .and_then(|v| v.as_list().map(|l| l.to_vec()))
        .unwrap_or_default();

    // Find the entry with matching version number
    let entry = history_list
        .iter()
        .find(|e| {
            e.as_map_rc()
                .and_then(|m| m.get(&Value::keyword("version")).cloned())
                .and_then(|v| v.as_int())
                == Some(version)
        })
        .ok_or_else(|| {
            SemaError::eval(format!(
                "rollback!: version {version} not found in history for '{name}'"
            ))
            .with_hint(format!(
                "Available versions: {}",
                history_list
                    .iter()
                    .filter_map(|e| e
                        .as_map_rc()
                        .and_then(|m| m.get(&Value::keyword("version")).cloned())
                        .and_then(|v| v.as_int()))
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
        })?;

    let entry_map = entry
        .as_map_rc()
        .ok_or_else(|| SemaError::eval("rollback!: history entry is not a map"))?;

    let old_value = entry_map
        .get(&Value::keyword("value"))
        .cloned()
        .ok_or_else(|| SemaError::eval("rollback!: history entry missing :value"))?;

    let old_source = entry_map.get(&Value::keyword("source")).cloned();

    // Save current version to history before rolling back
    let current = env
        .get(spur)
        .ok_or_else(|| SemaError::eval(format!("rollback!: '{name}' is not defined")))?;

    let mut updated_history = history_list;
    let mut rollback_entry = BTreeMap::new();
    rollback_entry.insert(
        Value::keyword("version"),
        Value::int(updated_history.len() as i64 + 1),
    );
    rollback_entry.insert(Value::keyword("value"), current);
    if let Some(meta) = env.get_meta(spur) {
        if let Some(source) = meta.get(&Value::keyword("source")) {
            rollback_entry.insert(Value::keyword("source"), source.clone());
        }
    }
    updated_history.push(Value::map(rollback_entry));
    env.set_meta(
        spur,
        Value::keyword("history"),
        Value::list(updated_history),
    );

    // Restore the old value and source metadata
    env.set(spur, old_value);
    if let Some(source) = old_source {
        env.set_meta(spur, Value::keyword("source"), source);
    }

    Ok(Trampoline::Value(Value::nil()))
}

/// (freeze! sym) — permanently prevent further become!/rollback! on a function
fn eval_freeze(args: &[Value], env: &Env) -> Result<Trampoline, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("freeze!", "1", args.len()));
    }
    let spur = args[0]
        .as_symbol_spur()
        .ok_or_else(|| SemaError::type_error("symbol", args[0].type_name()))?;

    env.set_meta(spur, Value::keyword("frozen?"), Value::bool(true));

    Ok(Trampoline::Value(Value::nil()))
}

/// Parse parameter list, handling rest params (e.g., `(a b . rest)`)
fn parse_params(names: &[Spur]) -> (Vec<Spur>, Option<Spur>) {
    let dot = intern(".");
    if let Some(pos) = names.iter().position(|s| *s == dot) {
        let params = names[..pos].to_vec();
        let rest = if pos + 1 < names.len() {
            Some(names[pos + 1])
        } else {
            None
        };
        (params, rest)
    } else {
        (names.to_vec(), None)
    }
}
