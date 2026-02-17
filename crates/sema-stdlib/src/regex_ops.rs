use std::collections::BTreeMap;

use regex::Regex;
use sema_core::{check_arity, SemaError, Value};

use crate::register_fn;

fn compile_regex(pattern: &str) -> Result<Regex, SemaError> {
    Regex::new(pattern).map_err(|e| SemaError::eval(format!("invalid regex: {e}")))
}

pub fn register(env: &sema_core::Env) {
    register_fn(env, "regex/match?", |args| {
        check_arity!(args, "regex/match?", 2);
        let pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let text = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let re = compile_regex(pattern)?;
        Ok(Value::bool(re.is_match(text)))
    });

    register_fn(env, "regex/match", |args| {
        check_arity!(args, "regex/match", 2);
        let pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let text = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let re = compile_regex(pattern)?;
        match re.captures(text) {
            None => Ok(Value::nil()),
            Some(caps) => {
                let full = caps.get(0).unwrap();
                let mut map = BTreeMap::new();
                map.insert(Value::keyword("match"), Value::string(full.as_str()));
                let groups: Vec<Value> = caps
                    .iter()
                    .skip(1)
                    .map(|m| match m {
                        Some(m) => Value::string(m.as_str()),
                        None => Value::nil(),
                    })
                    .collect();
                map.insert(Value::keyword("groups"), Value::list(groups));
                map.insert(Value::keyword("start"), Value::int(full.start() as i64));
                map.insert(Value::keyword("end"), Value::int(full.end() as i64));
                Ok(Value::map(map))
            }
        }
    });

    register_fn(env, "regex/find-all", |args| {
        check_arity!(args, "regex/find-all", 2);
        let pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let text = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let re = compile_regex(pattern)?;
        let matches: Vec<Value> = re
            .find_iter(text)
            .map(|m| Value::string(m.as_str()))
            .collect();
        Ok(Value::list(matches))
    });

    register_fn(env, "regex/replace", |args| {
        check_arity!(args, "regex/replace", 3);
        let pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let replacement = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let text = args[2]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
        let re = compile_regex(pattern)?;
        Ok(Value::string(&re.replace(text, replacement)))
    });

    register_fn(env, "regex/replace-all", |args| {
        check_arity!(args, "regex/replace-all", 3);
        let pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let replacement = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let text = args[2]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[2].type_name()))?;
        let re = compile_regex(pattern)?;
        Ok(Value::string(&re.replace_all(text, replacement)))
    });

    register_fn(env, "regex/split", |args| {
        check_arity!(args, "regex/split", 2);
        let pattern = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let text = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let re = compile_regex(pattern)?;
        let parts: Vec<Value> = re.split(text).map(Value::string).collect();
        Ok(Value::list(parts))
    });
}
