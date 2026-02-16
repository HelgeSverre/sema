use std::rc::Rc;

use sema_core::{intern, SemaError, Spur, Value};

use crate::core_expr::{CoreExpr, DoLoop, DoVar, LambdaDef, PromptEntry};

/// Lower a Value AST into CoreExpr IR.
pub fn lower(expr: &Value) -> Result<CoreExpr, SemaError> {
    lower_expr(expr, false)
}

/// Lower a sequence of expressions, marking the last as tail position.
pub fn lower_body(exprs: &[Value], tail: bool) -> Result<Vec<CoreExpr>, SemaError> {
    let mut result = Vec::with_capacity(exprs.len());
    for (i, expr) in exprs.iter().enumerate() {
        let is_last = i == exprs.len() - 1;
        result.push(lower_expr(expr, tail && is_last)?);
    }
    Ok(result)
}

fn lower_expr(expr: &Value, tail: bool) -> Result<CoreExpr, SemaError> {
    match expr {
        Value::Nil
        | Value::Bool(_)
        | Value::Int(_)
        | Value::Float(_)
        | Value::String(_)
        | Value::Char(_)
        | Value::Keyword(_)
        | Value::Bytevector(_)
        | Value::NativeFn(_)
        | Value::Lambda(_)
        | Value::HashMap(_)
        | Value::Thunk(_) => Ok(CoreExpr::Const(expr.clone())),

        Value::Symbol(spur) => Ok(CoreExpr::Var(*spur)),

        Value::Vector(items) => {
            let exprs = items
                .iter()
                .map(|v| lower_expr(v, false))
                .collect::<Result<_, _>>()?;
            Ok(CoreExpr::MakeVector(exprs))
        }

        Value::Map(map) => {
            let pairs = map
                .iter()
                .map(|(k, v)| Ok((lower_expr(k, false)?, lower_expr(v, false)?)))
                .collect::<Result<Vec<_>, SemaError>>()?;
            Ok(CoreExpr::MakeMap(pairs))
        }

        Value::List(items) => {
            if items.is_empty() {
                return Ok(CoreExpr::Const(Value::Nil));
            }
            lower_list(items, tail)
        }

        // Remaining types are self-evaluating
        _ => Ok(CoreExpr::Const(expr.clone())),
    }
}

fn lower_list(items: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    let head = &items[0];
    let args = &items[1..];

    if let Value::Symbol(spur) = head {
        let s = *spur;
        if s == sf("quote") {
            return lower_quote(args);
        } else if s == sf("if") {
            return lower_if(args, tail);
        } else if s == sf("cond") {
            return lower_cond(args, tail);
        } else if s == sf("define") {
            return lower_define(args);
        } else if s == sf("defun") {
            return lower_defun(args);
        } else if s == sf("set!") {
            return lower_set(args);
        } else if s == sf("lambda") || s == sf("fn") {
            return lower_lambda(args, None);
        } else if s == sf("let") {
            return lower_let(args, tail);
        } else if s == sf("let*") {
            return lower_let_star(args, tail);
        } else if s == sf("letrec") {
            return lower_letrec(args, tail);
        } else if s == sf("begin") {
            return lower_begin(args, tail);
        } else if s == sf("do") {
            return lower_do(args, tail);
        } else if s == sf("and") {
            return lower_and(args, tail);
        } else if s == sf("or") {
            return lower_or(args, tail);
        } else if s == sf("when") {
            return lower_when(args, tail);
        } else if s == sf("unless") {
            return lower_unless(args, tail);
        } else if s == sf("defmacro") {
            return lower_defmacro(args);
        } else if s == sf("quasiquote") {
            return lower_quasiquote(args);
        } else if s == sf("throw") {
            return lower_throw(args);
        } else if s == sf("try") {
            return lower_try(args, tail);
        } else if s == sf("case") {
            return lower_case(args, tail);
        } else if s == sf("eval") {
            return lower_eval(args);
        } else if s == sf("macroexpand") {
            return lower_macroexpand(args);
        } else if s == sf("module") {
            return lower_module(args, tail);
        } else if s == sf("import") {
            return lower_import(args);
        } else if s == sf("load") {
            return lower_load(args);
        } else if s == sf("prompt") {
            return lower_prompt(args);
        } else if s == sf("message") {
            return lower_message(args);
        } else if s == sf("deftool") {
            return lower_deftool(args);
        } else if s == sf("defagent") {
            return lower_defagent(args);
        } else if s == sf("with-budget") {
            return lower_with_budget(args);
        } else if s == sf("delay") {
            return lower_delay(args);
        } else if s == sf("force") {
            return lower_force(args);
        } else if s == sf("define-record-type") {
            return lower_define_record_type(args);
        }
    }

    // Not a special form — function call
    let func = lower_expr(head, false)?;
    let call_args = args
        .iter()
        .map(|a| lower_expr(a, false))
        .collect::<Result<_, _>>()?;
    Ok(CoreExpr::Call {
        func: Box::new(func),
        args: call_args,
        tail,
    })
}

/// Intern a string and return its Spur. Used for special form name matching.
fn sf(name: &str) -> Spur {
    intern(name)
}

fn require_symbol(val: &Value, context: &str) -> Result<Spur, SemaError> {
    val.as_symbol_spur()
        .ok_or_else(|| SemaError::eval(format!("{context}: expected a symbol")))
}

fn require_list<'a>(val: &'a Value, context: &str) -> Result<&'a [Value], SemaError> {
    val.as_list()
        .ok_or_else(|| SemaError::eval(format!("{context}: expected a list")))
}

/// Parse parameter list, handling rest params `(a b . rest)`.
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

fn extract_param_spurs(param_list: &[Value], context: &str) -> Result<Vec<Spur>, SemaError> {
    param_list
        .iter()
        .map(|v| require_symbol(v, context))
        .collect()
}

/// Parse a binding list like `((x 1) (y 2))` into `Vec<(Spur, CoreExpr)>`.
fn parse_bindings(bindings_val: &Value, context: &str) -> Result<Vec<(Spur, CoreExpr)>, SemaError> {
    let bindings_list = require_list(bindings_val, context)?;
    let mut bindings = Vec::new();
    for binding in bindings_list {
        let pair = require_list(binding, context)?;
        if pair.len() != 2 {
            return Err(SemaError::eval(format!(
                "{context}: each binding must have 2 elements"
            )));
        }
        let name = require_symbol(&pair[0], context)?;
        let init = lower_expr(&pair[1], false)?;
        bindings.push((name, init));
    }
    Ok(bindings)
}

// --- Special form lowering ---

fn lower_quote(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("quote", "1", args.len()));
    }
    Ok(CoreExpr::Quote(args[0].clone()))
}

fn lower_if(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(SemaError::arity("if", "2 or 3", args.len()));
    }
    let test = lower_expr(&args[0], false)?;
    let then = lower_expr(&args[1], tail)?;
    let else_ = if args.len() == 3 {
        lower_expr(&args[2], tail)?
    } else {
        CoreExpr::Const(Value::Nil)
    };
    Ok(CoreExpr::If {
        test: Box::new(test),
        then: Box::new(then),
        else_: Box::new(else_),
    })
}

fn lower_cond(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    let else_spur = sf("else");
    lower_cond_clauses(args, 0, tail, else_spur)
}

fn lower_cond_clauses(
    clauses: &[Value],
    idx: usize,
    tail: bool,
    else_spur: Spur,
) -> Result<CoreExpr, SemaError> {
    if idx >= clauses.len() {
        return Ok(CoreExpr::Const(Value::Nil));
    }
    let clause = require_list(&clauses[idx], "cond")?;
    if clause.is_empty() {
        return Err(SemaError::eval("cond clause must not be empty"));
    }

    let is_else = matches!(&clause[0], Value::Symbol(s) if *s == else_spur);
    if is_else {
        let body = lower_body(&clause[1..], tail)?;
        return if body.is_empty() {
            Ok(CoreExpr::Const(Value::Bool(true)))
        } else if body.len() == 1 {
            Ok(body.into_iter().next().unwrap())
        } else {
            Ok(CoreExpr::Begin(body))
        };
    }

    let test = lower_expr(&clause[0], false)?;
    let then = if clause.len() == 1 {
        CoreExpr::Const(Value::Bool(true))
    } else {
        let body = lower_body(&clause[1..], tail)?;
        if body.len() == 1 {
            body.into_iter().next().unwrap()
        } else {
            CoreExpr::Begin(body)
        }
    };
    let else_ = lower_cond_clauses(clauses, idx + 1, tail, else_spur)?;

    Ok(CoreExpr::If {
        test: Box::new(test),
        then: Box::new(then),
        else_: Box::new(else_),
    })
}

fn lower_define(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("define", "2+", 0));
    }
    match &args[0] {
        Value::Symbol(spur) => {
            if args.len() != 2 {
                return Err(SemaError::arity("define", "2", args.len()));
            }
            let val = lower_expr(&args[1], false)?;
            Ok(CoreExpr::Define(*spur, Box::new(val)))
        }
        Value::List(sig) => {
            if sig.is_empty() {
                return Err(SemaError::eval("define: empty function signature"));
            }
            let name_spur = require_symbol(&sig[0], "define")?;
            let param_spurs = extract_param_spurs(&sig[1..], "define")?;
            let (params, rest) = parse_params(&param_spurs);
            let body = lower_body(&args[1..], true)?;
            if body.is_empty() {
                return Err(SemaError::eval("define: function body cannot be empty"));
            }
            Ok(CoreExpr::Define(
                name_spur,
                Box::new(CoreExpr::Lambda(LambdaDef {
                    name: Some(name_spur),
                    params,
                    rest,
                    body,
                })),
            ))
        }
        other => Err(SemaError::type_error("symbol or list", other.type_name())),
    }
}

fn lower_defun(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defun", "3+", args.len()));
    }
    let name_spur = require_symbol(&args[0], "defun")?;
    let param_list = require_list(&args[1], "defun")?;
    let param_spurs = extract_param_spurs(param_list, "defun")?;
    let (params, rest) = parse_params(&param_spurs);
    let body = lower_body(&args[2..], true)?;
    Ok(CoreExpr::Define(
        name_spur,
        Box::new(CoreExpr::Lambda(LambdaDef {
            name: Some(name_spur),
            params,
            rest,
            body,
        })),
    ))
}

fn lower_set(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("set!", "2", args.len()));
    }
    let spur = require_symbol(&args[0], "set!")?;
    let val = lower_expr(&args[1], false)?;
    Ok(CoreExpr::Set(spur, Box::new(val)))
}

fn lower_lambda(args: &[Value], name: Option<Spur>) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("lambda", "2+", args.len()));
    }
    let param_vals = match &args[0] {
        Value::List(params) => params.as_ref().clone(),
        Value::Vector(params) => params.as_ref().clone(),
        other => return Err(SemaError::type_error("list or vector", other.type_name())),
    };
    let param_spurs = extract_param_spurs(&param_vals, "lambda")?;
    let (params, rest) = parse_params(&param_spurs);
    let body = lower_body(&args[1..], true)?;
    Ok(CoreExpr::Lambda(LambdaDef {
        name,
        params,
        rest,
        body,
    }))
}

fn lower_let(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("let", "2+", args.len()));
    }

    // Named let: (let name ((var init) ...) body...)
    if let Value::Symbol(loop_name) = &args[0] {
        if args.len() < 3 {
            return Err(SemaError::arity("named let", "3+", args.len()));
        }
        let bindings_list = require_list(&args[1], "named let")?;
        let mut bindings = Vec::new();
        for binding in bindings_list {
            let pair = require_list(binding, "named let")?;
            if pair.len() != 2 {
                return Err(SemaError::eval(
                    "named let: each binding must have 2 elements",
                ));
            }
            let name = require_symbol(&pair[0], "named let")?;
            let init = lower_expr(&pair[1], false)?;
            bindings.push((name, init));
        }
        let body = lower_body(&args[2..], tail)?;
        let (params, inits): (Vec<Spur>, Vec<CoreExpr>) = bindings.into_iter().unzip();
        return Ok(CoreExpr::Letrec {
            bindings: vec![(
                *loop_name,
                CoreExpr::Lambda(LambdaDef {
                    name: Some(*loop_name),
                    params,
                    rest: None,
                    body,
                }),
            )],
            body: vec![CoreExpr::Call {
                func: Box::new(CoreExpr::Var(*loop_name)),
                args: inits,
                tail,
            }],
        });
    }

    // Regular let
    let bindings = parse_bindings(&args[0], "let")?;
    let body = lower_body(&args[1..], tail)?;
    Ok(CoreExpr::Let { bindings, body })
}

fn lower_let_star(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("let*", "2+", args.len()));
    }
    let bindings = parse_bindings(&args[0], "let*")?;
    let body = lower_body(&args[1..], tail)?;
    Ok(CoreExpr::LetStar { bindings, body })
}

fn lower_letrec(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("letrec", "2+", args.len()));
    }
    let bindings = parse_bindings(&args[0], "letrec")?;
    let body = lower_body(&args[1..], tail)?;
    Ok(CoreExpr::Letrec { bindings, body })
}

fn lower_begin(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.is_empty() {
        return Ok(CoreExpr::Const(Value::Nil));
    }
    let body = lower_body(args, tail)?;
    if body.len() == 1 {
        Ok(body.into_iter().next().unwrap())
    } else {
        Ok(CoreExpr::Begin(body))
    }
}

fn lower_and(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.is_empty() {
        return Ok(CoreExpr::Const(Value::Bool(true)));
    }
    let exprs = lower_body(args, tail)?;
    if exprs.len() == 1 {
        Ok(exprs.into_iter().next().unwrap())
    } else {
        Ok(CoreExpr::And(exprs))
    }
}

fn lower_or(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.is_empty() {
        return Ok(CoreExpr::Const(Value::Bool(false)));
    }
    let exprs = lower_body(args, tail)?;
    if exprs.len() == 1 {
        Ok(exprs.into_iter().next().unwrap())
    } else {
        Ok(CoreExpr::Or(exprs))
    }
}

fn lower_when(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("when", "2+", args.len()));
    }
    let test = lower_expr(&args[0], false)?;
    let body = lower_body(&args[1..], tail)?;
    let then = if body.len() == 1 {
        body.into_iter().next().unwrap()
    } else {
        CoreExpr::Begin(body)
    };
    Ok(CoreExpr::If {
        test: Box::new(test),
        then: Box::new(then),
        else_: Box::new(CoreExpr::Const(Value::Nil)),
    })
}

fn lower_unless(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("unless", "2+", args.len()));
    }
    let test = lower_expr(&args[0], false)?;
    let body = lower_body(&args[1..], tail)?;
    let else_ = if body.len() == 1 {
        body.into_iter().next().unwrap()
    } else {
        CoreExpr::Begin(body)
    };
    Ok(CoreExpr::If {
        test: Box::new(test),
        then: Box::new(CoreExpr::Const(Value::Nil)),
        else_: Box::new(else_),
    })
}

fn lower_defmacro(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::arity("defmacro", "3+", args.len()));
    }
    // Delegate defmacro entirely to the tree-walker: reconstruct the original
    // form and pass it quoted to __vm-defmacro-form so the body stays unevaluated.
    let mut form = vec![Value::Symbol(intern("defmacro"))];
    form.extend(args.iter().cloned());
    Ok(CoreExpr::Call {
        func: Box::new(CoreExpr::Var(intern("__vm-defmacro-form"))),
        args: vec![CoreExpr::Const(Value::List(Rc::new(form)))],
        tail: false,
    })
}

fn lower_quasiquote(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("quasiquote", "1", args.len()));
    }
    expand_quasiquote(&args[0])
}

fn expand_quasiquote(val: &Value) -> Result<CoreExpr, SemaError> {
    match val {
        Value::List(items) => {
            if items.is_empty() {
                return Ok(CoreExpr::Quote(val.clone()));
            }
            // Check for (unquote x)
            if let Some(sym) = items[0].as_symbol() {
                if sym == "unquote" {
                    if items.len() != 2 {
                        return Err(SemaError::arity("unquote", "1", items.len() - 1));
                    }
                    return lower_expr(&items[1], false);
                }
            }
            // Expand each element, handling unquote-splicing
            let mut has_splice = false;
            for item in items.iter() {
                if let Value::List(inner) = item {
                    if !inner.is_empty() {
                        if let Some(sym) = inner[0].as_symbol() {
                            if sym == "unquote-splicing" {
                                has_splice = true;
                                break;
                            }
                        }
                    }
                }
            }

            if has_splice {
                // Build using append: collect segments, splice where needed
                let mut segments: Vec<CoreExpr> = Vec::new();
                let mut current_list: Vec<CoreExpr> = Vec::new();

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
                                    // Flush current_list as a MakeList segment
                                    if !current_list.is_empty() {
                                        segments.push(CoreExpr::MakeList(std::mem::take(
                                            &mut current_list,
                                        )));
                                    }
                                    // The spliced expr evaluates to a list
                                    segments.push(lower_expr(&inner[1], false)?);
                                    continue;
                                }
                            }
                        }
                    }
                    current_list.push(expand_quasiquote(item)?);
                }
                if !current_list.is_empty() {
                    segments.push(CoreExpr::MakeList(current_list));
                }

                // Build (append seg1 seg2 ...) call
                if segments.len() == 1 {
                    Ok(segments.into_iter().next().unwrap())
                } else {
                    Ok(CoreExpr::Call {
                        func: Box::new(CoreExpr::Var(intern("append"))),
                        args: segments,
                        tail: false,
                    })
                }
            } else {
                // No splicing — just expand each element
                let exprs = items
                    .iter()
                    .map(expand_quasiquote)
                    .collect::<Result<_, _>>()?;
                Ok(CoreExpr::MakeList(exprs))
            }
        }
        Value::Vector(items) => {
            let exprs = items
                .iter()
                .map(expand_quasiquote)
                .collect::<Result<_, _>>()?;
            Ok(CoreExpr::MakeVector(exprs))
        }
        _ => Ok(CoreExpr::Quote(val.clone())),
    }
}

fn lower_throw(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("throw", "1", args.len()));
    }
    Ok(CoreExpr::Throw(Box::new(lower_expr(&args[0], false)?)))
}

fn lower_try(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("try", "1+", 0));
    }
    let catch_spur = sf("catch");
    let last = &args[args.len() - 1];
    let catch_form = require_list(last, "try")?;
    if catch_form.is_empty() {
        return Err(SemaError::eval("try: catch form is empty"));
    }
    let is_catch = matches!(&catch_form[0], Value::Symbol(s) if *s == catch_spur);
    if !is_catch {
        return Err(SemaError::eval(
            "try: last argument must be (catch var handler...)",
        ));
    }
    if catch_form.len() < 3 {
        return Err(SemaError::eval("try: catch needs (catch var handler...)"));
    }
    let catch_var = require_symbol(&catch_form[1], "try catch")?;
    let handler = lower_body(&catch_form[2..], tail)?;
    // Body is NOT tail position (handler must be reachable)
    let body = lower_body(&args[..args.len() - 1], false)?;
    Ok(CoreExpr::Try {
        body,
        catch_var,
        handler,
    })
}

fn lower_case(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("case", "2+", args.len()));
    }
    let key = lower_expr(&args[0], false)?;
    // Bind the key to a temp variable so we only evaluate it once
    let tmp = intern("__case_key__");
    let body = lower_case_clauses(&args[1..], tmp, tail)?;
    Ok(CoreExpr::Let {
        bindings: vec![(tmp, key)],
        body: vec![body],
    })
}

fn lower_case_clauses(clauses: &[Value], key_var: Spur, tail: bool) -> Result<CoreExpr, SemaError> {
    let else_spur = sf("else");
    if clauses.is_empty() {
        return Ok(CoreExpr::Const(Value::Nil));
    }
    let clause = require_list(&clauses[0], "case")?;
    if clause.is_empty() {
        return Err(SemaError::eval("case: clause must not be empty"));
    }

    let is_else = matches!(&clause[0], Value::Symbol(s) if *s == else_spur);
    if is_else {
        let body = lower_body(&clause[1..], tail)?;
        return if body.is_empty() {
            Ok(CoreExpr::Const(Value::Nil))
        } else if body.len() == 1 {
            Ok(body.into_iter().next().unwrap())
        } else {
            Ok(CoreExpr::Begin(body))
        };
    }

    // ((datum ...) body...)
    let datums = require_list(&clause[0], "case")?;
    // Build equality tests: (or (= key d1) (= key d2) ...)
    let eq_spur = intern("=");
    let tests: Vec<CoreExpr> = datums
        .iter()
        .map(|datum| {
            Ok(CoreExpr::Call {
                func: Box::new(CoreExpr::Var(eq_spur)),
                args: vec![CoreExpr::Var(key_var), CoreExpr::Quote(datum.clone())],
                tail: false,
            })
        })
        .collect::<Result<_, SemaError>>()?;

    let test = if tests.len() == 1 {
        tests.into_iter().next().unwrap()
    } else {
        CoreExpr::Or(tests)
    };

    let then_body = lower_body(&clause[1..], tail)?;
    let then = if then_body.is_empty() {
        CoreExpr::Const(Value::Nil)
    } else if then_body.len() == 1 {
        then_body.into_iter().next().unwrap()
    } else {
        CoreExpr::Begin(then_body)
    };

    let else_ = lower_case_clauses(&clauses[1..], key_var, tail)?;

    Ok(CoreExpr::If {
        test: Box::new(test),
        then: Box::new(then),
        else_: Box::new(else_),
    })
}

fn lower_do(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("do", "2+", args.len()));
    }
    let bindings = require_list(&args[0], "do")?;
    let test_clause = require_list(&args[1], "do")?;
    if test_clause.is_empty() {
        return Err(SemaError::eval("do: test clause must not be empty"));
    }
    let body_vals = &args[2..];

    let mut vars = Vec::new();
    for binding in bindings {
        let parts = require_list(binding, "do")?;
        if parts.len() < 2 || parts.len() > 3 {
            return Err(SemaError::eval(
                "do: binding must be (var init) or (var init step)",
            ));
        }
        let name = require_symbol(&parts[0], "do")?;
        let init = lower_expr(&parts[1], false)?;
        let step = if parts.len() == 3 {
            Some(lower_expr(&parts[2], false)?)
        } else {
            None
        };
        vars.push(DoVar { name, init, step });
    }

    let test = lower_expr(&test_clause[0], false)?;
    // Result exprs: last is tail position
    let result = lower_body(&test_clause[1..], tail)?;
    // Loop body: NOT tail position
    let body = lower_body(body_vals, false)?;

    Ok(CoreExpr::Do(DoLoop {
        vars,
        test: Box::new(test),
        result,
        body,
    }))
}

fn lower_eval(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("eval", "1", args.len()));
    }
    Ok(CoreExpr::Eval(Box::new(lower_expr(&args[0], false)?)))
}

fn lower_macroexpand(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("macroexpand", "1", args.len()));
    }
    Ok(CoreExpr::Macroexpand(Box::new(lower_expr(
        &args[0], false,
    )?)))
}

fn lower_module(args: &[Value], tail: bool) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("module", "2+", args.len()));
    }
    let name = require_symbol(&args[0], "module")?;
    let export_list = require_list(&args[1], "module")?;
    let export_spur = sf("export");
    if export_list.is_empty() || !matches!(&export_list[0], Value::Symbol(s) if *s == export_spur) {
        return Err(SemaError::eval(
            "module: second argument must start with 'export'",
        ));
    }
    let exports = export_list[1..]
        .iter()
        .map(|v| require_symbol(v, "module export"))
        .collect::<Result<_, _>>()?;
    let body = lower_body(&args[2..], tail)?;
    Ok(CoreExpr::Module {
        name,
        exports,
        body,
    })
}

fn lower_import(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.is_empty() {
        return Err(SemaError::arity("import", "1+", 0));
    }
    let path = lower_expr(&args[0], false)?;
    let selective = args[1..]
        .iter()
        .map(|v| require_symbol(v, "import"))
        .collect::<Result<_, _>>()?;
    Ok(CoreExpr::Import {
        path: Box::new(path),
        selective,
    })
}

fn lower_load(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("load", "1", args.len()));
    }
    Ok(CoreExpr::Load(Box::new(lower_expr(&args[0], false)?)))
}

fn lower_prompt(args: &[Value]) -> Result<CoreExpr, SemaError> {
    let role_names = ["system", "user", "assistant", "tool"];
    let mut entries = Vec::new();
    for arg in args {
        if let Value::List(items) = arg {
            if !items.is_empty() {
                if let Some(sym) = items[0].as_symbol() {
                    if role_names.contains(&sym.as_str()) {
                        let parts = items[1..]
                            .iter()
                            .map(|v| lower_expr(v, false))
                            .collect::<Result<_, _>>()?;
                        entries.push(PromptEntry::RoleContent {
                            role: sym.to_string(),
                            parts,
                        });
                        continue;
                    }
                }
            }
        }
        entries.push(PromptEntry::Expr(lower_expr(arg, false)?));
    }
    Ok(CoreExpr::Prompt(entries))
}

fn lower_message(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("message", "2+", args.len()));
    }
    let role = lower_expr(&args[0], false)?;
    let parts = args[1..]
        .iter()
        .map(|v| lower_expr(v, false))
        .collect::<Result<_, _>>()?;
    Ok(CoreExpr::Message {
        role: Box::new(role),
        parts,
    })
}

fn lower_deftool(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() < 4 {
        return Err(SemaError::arity("deftool", "4", args.len()));
    }
    let name = require_symbol(&args[0], "deftool")?;
    let description = lower_expr(&args[1], false)?;
    let parameters = lower_expr(&args[2], false)?;
    let handler = lower_expr(&args[3], false)?;
    Ok(CoreExpr::Deftool {
        name,
        description: Box::new(description),
        parameters: Box::new(parameters),
        handler: Box::new(handler),
    })
}

fn lower_defagent(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 2 {
        return Err(SemaError::arity("defagent", "2", args.len()));
    }
    let name = require_symbol(&args[0], "defagent")?;
    let options = lower_expr(&args[1], false)?;
    Ok(CoreExpr::Defagent {
        name,
        options: Box::new(options),
    })
}

fn lower_with_budget(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() < 2 {
        return Err(SemaError::arity("with-budget", "2+", args.len()));
    }
    let options = lower_expr(&args[0], false)?;
    let body = lower_body(&args[1..], false)?;
    Ok(CoreExpr::WithBudget {
        options: Box::new(options),
        body,
    })
}

fn lower_delay(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("delay", "1", args.len()));
    }
    // Store the original unevaluated body as a quoted constant for __vm-delay.
    // This preserves laziness: the body is NOT evaluated at definition time.
    let body_val = args[0].clone();
    Ok(CoreExpr::Call {
        func: Box::new(CoreExpr::Var(intern("__vm-delay"))),
        args: vec![CoreExpr::Const(body_val)],
        tail: false,
    })
}

fn lower_force(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() != 1 {
        return Err(SemaError::arity("force", "1", args.len()));
    }
    // Force evaluates its argument (to get the thunk), then calls __vm-force
    let expr = lower_expr(&args[0], false)?;
    Ok(CoreExpr::Call {
        func: Box::new(CoreExpr::Var(intern("__vm-force"))),
        args: vec![expr],
        tail: false,
    })
}

fn lower_define_record_type(args: &[Value]) -> Result<CoreExpr, SemaError> {
    if args.len() < 3 {
        return Err(SemaError::eval(
            "define-record-type: requires at least type name, constructor, and predicate",
        ));
    }
    let type_name = require_symbol(&args[0], "define-record-type")?;
    let ctor_spec = require_list(&args[1], "define-record-type")?;
    if ctor_spec.is_empty() {
        return Err(SemaError::eval(
            "define-record-type: constructor spec must have a name",
        ));
    }
    let ctor_name = require_symbol(&ctor_spec[0], "define-record-type")?;
    let field_names = extract_param_spurs(&ctor_spec[1..], "define-record-type")?;
    let pred_name = require_symbol(&args[2], "define-record-type")?;

    let mut field_specs = Vec::new();
    for spec_val in &args[3..] {
        let spec = require_list(spec_val, "define-record-type")?;
        if spec.len() < 2 {
            return Err(SemaError::eval(
                "define-record-type: field spec must have at least (field accessor)",
            ));
        }
        let field = require_symbol(&spec[0], "define-record-type")?;
        let accessor = require_symbol(&spec[1], "define-record-type")?;
        field_specs.push((field, accessor));
    }

    Ok(CoreExpr::DefineRecordType {
        type_name,
        ctor_name,
        pred_name,
        field_names,
        field_specs,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Value {
        sema_reader::read(input).unwrap()
    }

    fn lower_str(input: &str) -> CoreExpr {
        lower(&parse(input)).unwrap()
    }

    #[test]
    fn test_lower_int() {
        assert!(matches!(lower_str("42"), CoreExpr::Const(Value::Int(42))));
    }

    #[test]
    fn test_lower_string() {
        assert!(matches!(
            lower_str("\"hello\""),
            CoreExpr::Const(Value::String(_))
        ));
    }

    #[test]
    fn test_lower_bool() {
        assert!(matches!(
            lower_str("#t"),
            CoreExpr::Const(Value::Bool(true))
        ));
        assert!(matches!(
            lower_str("#f"),
            CoreExpr::Const(Value::Bool(false))
        ));
    }

    #[test]
    fn test_lower_nil() {
        assert!(matches!(lower_str("nil"), CoreExpr::Const(Value::Nil)));
    }

    #[test]
    fn test_lower_keyword() {
        assert!(matches!(
            lower_str(":key"),
            CoreExpr::Const(Value::Keyword(_))
        ));
    }

    #[test]
    fn test_lower_symbol() {
        assert!(matches!(lower_str("x"), CoreExpr::Var(_)));
    }

    #[test]
    fn test_lower_empty_list() {
        assert!(matches!(lower_str("()"), CoreExpr::Const(Value::Nil)));
    }

    #[test]
    fn test_lower_vector() {
        assert!(matches!(lower_str("[1 2 3]"), CoreExpr::MakeVector(_)));
    }

    #[test]
    fn test_lower_map() {
        assert!(matches!(lower_str("{:a 1 :b 2}"), CoreExpr::MakeMap(_)));
    }

    #[test]
    fn test_lower_quote() {
        assert!(matches!(lower_str("(quote x)"), CoreExpr::Quote(_)));
    }

    #[test]
    fn test_lower_if() {
        match lower_str("(if #t 1 2)") {
            CoreExpr::If { test, then, else_ } => {
                assert!(matches!(*test, CoreExpr::Const(Value::Bool(true))));
                assert!(matches!(*then, CoreExpr::Const(Value::Int(1))));
                assert!(matches!(*else_, CoreExpr::Const(Value::Int(2))));
            }
            other => panic!("expected If, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_if_no_else() {
        match lower_str("(if #t 1)") {
            CoreExpr::If { else_, .. } => {
                assert!(matches!(*else_, CoreExpr::Const(Value::Nil)));
            }
            other => panic!("expected If, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_cond() {
        assert!(matches!(lower_str("(cond (#t 1))"), CoreExpr::If { .. }));
    }

    #[test]
    fn test_lower_cond_else() {
        assert!(matches!(
            lower_str("(cond (#f 1) (else 2))"),
            CoreExpr::If { .. }
        ));
    }

    #[test]
    fn test_lower_define_simple() {
        match lower_str("(define x 42)") {
            CoreExpr::Define(_, val) => {
                assert!(matches!(*val, CoreExpr::Const(Value::Int(42))));
            }
            other => panic!("expected Define, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_define_function() {
        match lower_str("(define (f x) x)") {
            CoreExpr::Define(_, val) => {
                assert!(matches!(*val, CoreExpr::Lambda(_)));
            }
            other => panic!("expected Define with Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_defun() {
        match lower_str("(defun f (x) x)") {
            CoreExpr::Define(_, val) => {
                assert!(matches!(*val, CoreExpr::Lambda(_)));
            }
            other => panic!("expected Define with Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_set() {
        assert!(matches!(lower_str("(set! x 42)"), CoreExpr::Set(_, _)));
    }

    #[test]
    fn test_lower_lambda() {
        match lower_str("(lambda (x y) (+ x y))") {
            CoreExpr::Lambda(def) => {
                assert_eq!(def.params.len(), 2);
                assert!(def.rest.is_none());
                assert!(def.name.is_none());
            }
            other => panic!("expected Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_fn() {
        assert!(matches!(lower_str("(fn (x) x)"), CoreExpr::Lambda(_)));
    }

    #[test]
    fn test_lower_lambda_rest() {
        match lower_str("(lambda (x . rest) rest)") {
            CoreExpr::Lambda(def) => {
                assert_eq!(def.params.len(), 1);
                assert!(def.rest.is_some());
            }
            other => panic!("expected Lambda with rest, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_let() {
        match lower_str("(let ((x 1)) x)") {
            CoreExpr::Let { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                assert_eq!(body.len(), 1);
            }
            other => panic!("expected Let, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_named_let() {
        // Named let desugars to letrec + lambda
        match lower_str("(let loop ((n 10)) (if (= n 0) 0 (loop (- n 1))))") {
            CoreExpr::Letrec { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                assert!(matches!(&bindings[0].1, CoreExpr::Lambda(_)));
                assert_eq!(body.len(), 1);
                assert!(matches!(&body[0], CoreExpr::Call { .. }));
            }
            other => panic!("expected Letrec, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_let_star() {
        assert!(matches!(
            lower_str("(let* ((x 1) (y x)) y)"),
            CoreExpr::LetStar { .. }
        ));
    }

    #[test]
    fn test_lower_letrec() {
        assert!(matches!(
            lower_str("(letrec ((f (lambda () f))) f)"),
            CoreExpr::Letrec { .. }
        ));
    }

    #[test]
    fn test_lower_begin() {
        assert!(matches!(lower_str("(begin 1 2 3)"), CoreExpr::Begin(_)));
    }

    #[test]
    fn test_lower_begin_single() {
        // Single-expr begin unwraps
        assert!(matches!(
            lower_str("(begin 42)"),
            CoreExpr::Const(Value::Int(42))
        ));
    }

    #[test]
    fn test_lower_and() {
        assert!(matches!(lower_str("(and 1 2 3)"), CoreExpr::And(_)));
    }

    #[test]
    fn test_lower_and_empty() {
        assert!(matches!(
            lower_str("(and)"),
            CoreExpr::Const(Value::Bool(true))
        ));
    }

    #[test]
    fn test_lower_or() {
        assert!(matches!(lower_str("(or 1 2 3)"), CoreExpr::Or(_)));
    }

    #[test]
    fn test_lower_or_empty() {
        assert!(matches!(
            lower_str("(or)"),
            CoreExpr::Const(Value::Bool(false))
        ));
    }

    #[test]
    fn test_lower_when() {
        match lower_str("(when #t 42)") {
            CoreExpr::If { else_, .. } => {
                assert!(matches!(*else_, CoreExpr::Const(Value::Nil)));
            }
            other => panic!("expected If from when, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_unless() {
        match lower_str("(unless #f 42)") {
            CoreExpr::If { then, .. } => {
                assert!(matches!(*then, CoreExpr::Const(Value::Nil)));
            }
            other => panic!("expected If from unless, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_throw() {
        assert!(matches!(lower_str("(throw \"error\")"), CoreExpr::Throw(_)));
    }

    #[test]
    fn test_lower_try() {
        match lower_str("(try 1 (catch e e))") {
            CoreExpr::Try { body, handler, .. } => {
                assert_eq!(body.len(), 1);
                assert_eq!(handler.len(), 1);
            }
            other => panic!("expected Try, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_do() {
        match lower_str("(do ((i 0 (+ i 1))) ((= i 10) i))") {
            CoreExpr::Do(loop_) => {
                assert_eq!(loop_.vars.len(), 1);
                assert!(loop_.vars[0].step.is_some());
            }
            other => panic!("expected Do, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_eval() {
        assert!(matches!(lower_str("(eval '(+ 1 2))"), CoreExpr::Eval(_)));
    }

    #[test]
    fn test_lower_case() {
        // (case x ((1) "one") (else "other")) → Let + If
        assert!(matches!(
            lower_str("(case x ((1) \"one\") (else \"other\"))"),
            CoreExpr::Let { .. }
        ));
    }

    #[test]
    fn test_lower_function_call() {
        match lower_str("(f 1 2)") {
            CoreExpr::Call { func, args, tail } => {
                assert!(matches!(*func, CoreExpr::Var(_)));
                assert_eq!(args.len(), 2);
                assert!(!tail);
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn test_tail_position_lambda_body() {
        // (lambda () (f x)) → last body expr should be tail call
        match lower_str("(lambda () (f x))") {
            CoreExpr::Lambda(def) => match &def.body[0] {
                CoreExpr::Call { tail, .. } => assert!(*tail),
                other => panic!("expected tail Call, got {other:?}"),
            },
            other => panic!("expected Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_tail_position_begin() {
        match lower_str("(lambda () (begin 1 (f x)))") {
            CoreExpr::Lambda(def) => match &def.body[0] {
                CoreExpr::Begin(exprs) => match exprs.last().unwrap() {
                    CoreExpr::Call { tail, .. } => assert!(*tail),
                    other => panic!("expected tail Call, got {other:?}"),
                },
                other => panic!("expected Begin, got {other:?}"),
            },
            other => panic!("expected Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_non_tail_position() {
        // (lambda () (f x) 1) → call is NOT tail
        match lower_str("(lambda () (f x) 1)") {
            CoreExpr::Lambda(def) => {
                assert_eq!(def.body.len(), 2);
                match &def.body[0] {
                    CoreExpr::Call { tail, .. } => assert!(!*tail),
                    other => panic!("expected non-tail Call, got {other:?}"),
                }
            }
            other => panic!("expected Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_tail_if_branches() {
        // (lambda () (if #t (f x) (g y))) → both branches are tail
        match lower_str("(lambda () (if #t (f x) (g y)))") {
            CoreExpr::Lambda(def) => match &def.body[0] {
                CoreExpr::If { then, else_, .. } => {
                    assert!(matches!(then.as_ref(), CoreExpr::Call { tail: true, .. }));
                    assert!(matches!(else_.as_ref(), CoreExpr::Call { tail: true, .. }));
                }
                other => panic!("expected If, got {other:?}"),
            },
            other => panic!("expected Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_try_body_not_tail() {
        // (lambda () (try (f x) (catch e e))) → try body is NOT tail
        match lower_str("(lambda () (try (f x) (catch e e)))") {
            CoreExpr::Lambda(def) => match &def.body[0] {
                CoreExpr::Try { body, .. } => match &body[0] {
                    CoreExpr::Call { tail, .. } => assert!(!*tail),
                    other => panic!("expected non-tail Call in try body, got {other:?}"),
                },
                other => panic!("expected Try, got {other:?}"),
            },
            other => panic!("expected Lambda, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_quasiquote_simple() {
        // `(1 2 3) → MakeList of quotes
        match lower_str("`(1 2 3)") {
            CoreExpr::MakeList(items) => {
                assert_eq!(items.len(), 3);
            }
            other => panic!("expected MakeList, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_quasiquote_unquote() {
        // `(1 ,x 3) → MakeList with Var for x
        match lower_str("`(1 ,x 3)") {
            CoreExpr::MakeList(items) => {
                assert_eq!(items.len(), 3);
                assert!(matches!(&items[1], CoreExpr::Var(_)));
            }
            other => panic!("expected MakeList, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_quasiquote_splice() {
        // `(1 ,@xs 3) → Call to append
        match lower_str("`(1 ,@xs 3)") {
            CoreExpr::Call { func, .. } => {
                assert!(matches!(func.as_ref(), CoreExpr::Var(_)));
            }
            other => panic!("expected Call (append), got {other:?}"),
        }
    }

    #[test]
    fn test_lower_delay() {
        // delay now lowers to a Call to __vm-delay with the body as a constant
        match lower_str("(delay (+ 1 2))") {
            CoreExpr::Call { func, args, .. } => {
                assert!(matches!(*func, CoreExpr::Var(_)));
                assert_eq!(args.len(), 1);
                assert!(matches!(&args[0], CoreExpr::Const(_)));
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_force() {
        // force now lowers to a Call to __vm-force
        assert!(matches!(lower_str("(force p)"), CoreExpr::Call { .. }));
    }

    #[test]
    fn test_lower_defmacro() {
        // defmacro now lowers to a Call to __vm-defmacro-form with the full form as a constant
        match lower_str("(defmacro my-if (test then else) (list 'if test then else))") {
            CoreExpr::Call { func, args, .. } => {
                assert!(matches!(*func, CoreExpr::Var(_)));
                assert_eq!(args.len(), 1);
                assert!(matches!(&args[0], CoreExpr::Const(Value::List(_))));
            }
            other => panic!("expected Call, got {other:?}"),
        }
    }

    #[test]
    fn test_lower_import() {
        assert!(matches!(
            lower_str("(import \"lib.sema\")"),
            CoreExpr::Import { .. }
        ));
    }

    #[test]
    fn test_lower_load() {
        assert!(matches!(
            lower_str("(load \"lib.sema\")"),
            CoreExpr::Load(_)
        ));
    }

    #[test]
    fn test_lower_define_record_type() {
        match lower_str(
            "(define-record-type point (make-point x y) point? (x point-x) (y point-y))",
        ) {
            CoreExpr::DefineRecordType {
                field_names,
                field_specs,
                ..
            } => {
                assert_eq!(field_names.len(), 2);
                assert_eq!(field_specs.len(), 2);
            }
            other => panic!("expected DefineRecordType, got {other:?}"),
        }
    }
}
