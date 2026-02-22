//! Constant folding and simplification pass on CoreExpr.
//!
//! Runs after lowering, before variable resolution. Folds:
//! - Arithmetic on constants: (+ 1 2) → 3
//! - Boolean simplification: (not #t) → #f
//! - If with constant test: (if #t a b) → a
//! - And/Or with constant operands

use sema_core::{resolve as resolve_spur, Value};

use crate::core_expr::{CoreExpr, PromptEntry};

pub fn optimize(expr: CoreExpr) -> CoreExpr {
    match expr {
        CoreExpr::Call { func, args, tail } => {
            let func = Box::new(optimize(*func));
            let args: Vec<_> = args.into_iter().map(optimize).collect();
            try_fold_call(*func, args, tail)
        }
        CoreExpr::If { test, then, else_ } => {
            let test = optimize(*test);
            let then = optimize(*then);
            let else_ = optimize(*else_);
            if let CoreExpr::Const(ref v) = test {
                if v.is_truthy() {
                    return then;
                } else {
                    return else_;
                }
            }
            CoreExpr::If {
                test: Box::new(test),
                then: Box::new(then),
                else_: Box::new(else_),
            }
        }
        CoreExpr::And(exprs) => {
            let exprs: Vec<_> = exprs.into_iter().map(optimize).collect();
            fold_and(exprs)
        }
        CoreExpr::Or(exprs) => {
            let exprs: Vec<_> = exprs.into_iter().map(optimize).collect();
            fold_or(exprs)
        }
        CoreExpr::Begin(exprs) => {
            let exprs: Vec<_> = exprs.into_iter().map(optimize).collect();
            fold_begin(exprs)
        }
        CoreExpr::Let { bindings, body } => {
            let bindings = bindings
                .into_iter()
                .map(|(s, e)| (s, optimize(e)))
                .collect();
            let body = body.into_iter().map(optimize).collect();
            CoreExpr::Let { bindings, body }
        }
        CoreExpr::LetStar { bindings, body } => {
            let bindings = bindings
                .into_iter()
                .map(|(s, e)| (s, optimize(e)))
                .collect();
            let body = body.into_iter().map(optimize).collect();
            CoreExpr::LetStar { bindings, body }
        }
        CoreExpr::Letrec { bindings, body } => {
            let bindings = bindings
                .into_iter()
                .map(|(s, e)| (s, optimize(e)))
                .collect();
            let body = body.into_iter().map(optimize).collect();
            CoreExpr::Letrec { bindings, body }
        }
        CoreExpr::NamedLet {
            name,
            bindings,
            body,
        } => {
            let bindings = bindings
                .into_iter()
                .map(|(s, e)| (s, optimize(e)))
                .collect();
            let body = body.into_iter().map(optimize).collect();
            CoreExpr::NamedLet {
                name,
                bindings,
                body,
            }
        }
        CoreExpr::Lambda(mut def) => {
            def.body = def.body.into_iter().map(optimize).collect();
            CoreExpr::Lambda(def)
        }
        CoreExpr::Define(spur, expr) => CoreExpr::Define(spur, Box::new(optimize(*expr))),
        CoreExpr::Set(spur, expr) => CoreExpr::Set(spur, Box::new(optimize(*expr))),
        CoreExpr::Do(mut d) => {
            d.vars = d
                .vars
                .into_iter()
                .map(|mut v| {
                    v.init = optimize(v.init);
                    v.step = v.step.map(optimize);
                    v
                })
                .collect();
            d.test = Box::new(optimize(*d.test));
            d.result = d.result.into_iter().map(optimize).collect();
            d.body = d.body.into_iter().map(optimize).collect();
            CoreExpr::Do(d)
        }
        CoreExpr::Try {
            body,
            catch_var,
            handler,
        } => {
            let body = body.into_iter().map(optimize).collect();
            let handler = handler.into_iter().map(optimize).collect();
            CoreExpr::Try {
                body,
                catch_var,
                handler,
            }
        }
        CoreExpr::Throw(e) => CoreExpr::Throw(Box::new(optimize(*e))),
        CoreExpr::MakeList(es) => CoreExpr::MakeList(es.into_iter().map(optimize).collect()),
        CoreExpr::MakeVector(es) => CoreExpr::MakeVector(es.into_iter().map(optimize).collect()),
        CoreExpr::MakeMap(pairs) => CoreExpr::MakeMap(
            pairs
                .into_iter()
                .map(|(k, v)| (optimize(k), optimize(v)))
                .collect(),
        ),
        CoreExpr::Defmacro {
            name,
            params,
            rest,
            body,
        } => {
            let body = body.into_iter().map(optimize).collect();
            CoreExpr::Defmacro {
                name,
                params,
                rest,
                body,
            }
        }
        CoreExpr::Module {
            name,
            exports,
            body,
        } => {
            let body = body.into_iter().map(optimize).collect();
            CoreExpr::Module {
                name,
                exports,
                body,
            }
        }
        CoreExpr::Import { path, selective } => CoreExpr::Import {
            path: Box::new(optimize(*path)),
            selective,
        },
        CoreExpr::Load(e) => CoreExpr::Load(Box::new(optimize(*e))),
        CoreExpr::Eval(e) => CoreExpr::Eval(Box::new(optimize(*e))),
        CoreExpr::Prompt(entries) => CoreExpr::Prompt(
            entries
                .into_iter()
                .map(|entry| match entry {
                    PromptEntry::RoleContent { role, parts } => PromptEntry::RoleContent {
                        role,
                        parts: parts.into_iter().map(optimize).collect(),
                    },
                    PromptEntry::Expr(e) => PromptEntry::Expr(optimize(e)),
                })
                .collect(),
        ),
        CoreExpr::Message { role, parts } => CoreExpr::Message {
            role: Box::new(optimize(*role)),
            parts: parts.into_iter().map(optimize).collect(),
        },
        CoreExpr::Deftool {
            name,
            description,
            parameters,
            handler,
        } => CoreExpr::Deftool {
            name,
            description: Box::new(optimize(*description)),
            parameters: Box::new(optimize(*parameters)),
            handler: Box::new(optimize(*handler)),
        },
        CoreExpr::Defagent { name, options } => CoreExpr::Defagent {
            name,
            options: Box::new(optimize(*options)),
        },
        CoreExpr::Delay(e) => CoreExpr::Delay(Box::new(optimize(*e))),
        CoreExpr::Force(e) => CoreExpr::Force(Box::new(optimize(*e))),
        CoreExpr::Macroexpand(e) => CoreExpr::Macroexpand(Box::new(optimize(*e))),
        // Pass through: Const, Var, Quote, DefineRecordType
        other => other,
    }
}

fn try_fold_call(func: CoreExpr, args: Vec<CoreExpr>, tail: bool) -> CoreExpr {
    if let CoreExpr::Var(spur) = &func {
        let name = resolve_spur(*spur);
        if args.len() == 2 {
            if let (CoreExpr::Const(ref a), CoreExpr::Const(ref b)) = (&args[0], &args[1]) {
                if let Some(result) = fold_binary_op(&name, a, b) {
                    return CoreExpr::Const(result);
                }
            }
        } else if args.len() == 1 {
            if let CoreExpr::Const(ref a) = args[0] {
                if let Some(result) = fold_unary_op(&name, a) {
                    return CoreExpr::Const(result);
                }
            }
        }
    }
    CoreExpr::Call {
        func: Box::new(func),
        args,
        tail,
    }
}

fn fold_binary_op(name: &str, a: &Value, b: &Value) -> Option<Value> {
    let ai = a.as_int()?;
    let bi = b.as_int()?;
    match name {
        "+" => Some(Value::int(ai.wrapping_add(bi))),
        "-" => Some(Value::int(ai.wrapping_sub(bi))),
        "*" => Some(Value::int(ai.wrapping_mul(bi))),
        "/" => {
            if bi == 0 {
                None
            } else {
                Some(Value::int(ai / bi))
            }
        }
        "<" => Some(Value::bool(ai < bi)),
        ">" => Some(Value::bool(ai > bi)),
        "<=" => Some(Value::bool(ai <= bi)),
        ">=" => Some(Value::bool(ai >= bi)),
        "=" => Some(Value::bool(ai == bi)),
        _ => None,
    }
}

fn fold_unary_op(name: &str, a: &Value) -> Option<Value> {
    match name {
        "not" => Some(Value::bool(!a.is_truthy())),
        "-" => {
            let i = a.as_int()?;
            Some(Value::int(-i))
        }
        _ => None,
    }
}

fn fold_and(mut exprs: Vec<CoreExpr>) -> CoreExpr {
    while !exprs.is_empty() {
        if let CoreExpr::Const(ref v) = exprs[0] {
            if !v.is_truthy() {
                return CoreExpr::Const(Value::bool(false));
            }
            if exprs.len() > 1 {
                exprs.remove(0);
                continue;
            }
        }
        break;
    }
    if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        CoreExpr::And(exprs)
    }
}

fn fold_or(mut exprs: Vec<CoreExpr>) -> CoreExpr {
    while !exprs.is_empty() {
        if let CoreExpr::Const(ref v) = exprs[0] {
            if v.is_truthy() {
                return exprs.remove(0);
            }
            if exprs.len() > 1 {
                exprs.remove(0);
                continue;
            }
        }
        break;
    }
    if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        CoreExpr::Or(exprs)
    }
}

fn fold_begin(exprs: Vec<CoreExpr>) -> CoreExpr {
    if exprs.len() <= 1 {
        return CoreExpr::Begin(exprs);
    }
    let mut result = Vec::new();
    let last_idx = exprs.len() - 1;
    for (i, e) in exprs.into_iter().enumerate() {
        if i == last_idx || !is_pure_const(&e) {
            result.push(e);
        }
    }
    CoreExpr::Begin(result)
}

fn is_pure_const(e: &CoreExpr) -> bool {
    matches!(e, CoreExpr::Const(_) | CoreExpr::Quote(_))
}
