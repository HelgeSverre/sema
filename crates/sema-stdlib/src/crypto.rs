use std::rc::Rc;

use hmac::{Hmac, Mac};
use sema_core::{SemaError, Value};
use sha2::{Digest, Sha256};

use crate::register_fn;

type HmacSha256 = Hmac<Sha256>;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "uuid/v4", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("uuid/v4", "0", args.len()));
        }
        Ok(Value::String(Rc::new(uuid::Uuid::new_v4().to_string())))
    });

    register_fn(env, "base64/encode", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("base64/encode", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        use base64::Engine;
        let encoded = base64::engine::general_purpose::STANDARD.encode(s.as_bytes());
        Ok(Value::String(Rc::new(encoded)))
    });

    register_fn(env, "base64/decode", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("base64/decode", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        use base64::Engine;
        let bytes = base64::engine::general_purpose::STANDARD
            .decode(s.as_bytes())
            .map_err(|e| SemaError::eval(format!("base64/decode: {e}")))?;
        let decoded = String::from_utf8(bytes)
            .map_err(|e| SemaError::eval(format!("base64/decode: invalid UTF-8: {e}")))?;
        Ok(Value::String(Rc::new(decoded)))
    });

    register_fn(env, "hash/md5", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("hash/md5", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let digest = md5::compute(s.as_bytes());
        Ok(Value::String(Rc::new(format!("{:x}", digest))))
    });

    register_fn(env, "hash/hmac-sha256", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("hash/hmac-sha256", "2", args.len()));
        }
        let key = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let message = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let mut mac = HmacSha256::new_from_slice(key.as_bytes()).unwrap();
        mac.update(message.as_bytes());
        let result = mac.finalize();
        Ok(Value::String(Rc::new(hex::encode(result.into_bytes()))))
    });

    register_fn(env, "hash/sha256", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("hash/sha256", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let hash = Sha256::digest(s.as_bytes());
        let hex: String = hash.iter().map(|b| format!("{:02x}", b)).collect();
        Ok(Value::String(Rc::new(hex)))
    });
}
