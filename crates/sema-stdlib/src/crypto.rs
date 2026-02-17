use hmac::{Hmac, Mac};
use sema_core::{check_arity, SemaError, Value};
use sha2::{Digest, Sha256};

use crate::register_fn;

type HmacSha256 = Hmac<Sha256>;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "uuid/v4", |args| {
        check_arity!(args, "uuid/v4", 0);
        Ok(Value::string(&uuid::Uuid::new_v4().to_string()))
    });

    register_fn(env, "base64/encode", |args| {
        check_arity!(args, "base64/encode", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        use base64::Engine;
        let encoded = base64::engine::general_purpose::STANDARD.encode(s.as_bytes());
        Ok(Value::string(&encoded))
    });

    register_fn(env, "base64/decode", |args| {
        check_arity!(args, "base64/decode", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        use base64::Engine;
        let bytes = base64::engine::general_purpose::STANDARD
            .decode(s.as_bytes())
            .map_err(|e| SemaError::eval(format!("base64/decode: {e}")))?;
        let decoded = String::from_utf8(bytes)
            .map_err(|e| SemaError::eval(format!("base64/decode: invalid UTF-8: {e}")))?;
        Ok(Value::string(&decoded))
    });

    register_fn(env, "base64/encode-bytes", |args| {
        check_arity!(args, "base64/encode-bytes", 1);
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        use base64::Engine;
        let encoded = base64::engine::general_purpose::STANDARD.encode(bv);
        Ok(Value::string(&encoded))
    });

    register_fn(env, "base64/decode-bytes", |args| {
        check_arity!(args, "base64/decode-bytes", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        use base64::Engine;
        let bytes = base64::engine::general_purpose::STANDARD
            .decode(s.as_bytes())
            .map_err(|e| SemaError::eval(format!("base64/decode-bytes: {e}")))?;
        Ok(Value::bytevector(bytes))
    });

    register_fn(env, "hash/md5", |args| {
        check_arity!(args, "hash/md5", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let digest = md5::compute(s.as_bytes());
        Ok(Value::string(&format!("{:x}", digest)))
    });

    register_fn(env, "hash/hmac-sha256", |args| {
        check_arity!(args, "hash/hmac-sha256", 2);
        let key = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let message = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let mut mac = HmacSha256::new_from_slice(key.as_bytes()).unwrap();
        mac.update(message.as_bytes());
        let result = mac.finalize();
        Ok(Value::string(&hex::encode(result.into_bytes())))
    });

    register_fn(env, "hash/sha256", |args| {
        check_arity!(args, "hash/sha256", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let hash = Sha256::digest(s.as_bytes());
        let hex: String = hash.iter().map(|b| format!("{:02x}", b)).collect();
        Ok(Value::string(&hex))
    });
}
