use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "make-bytevector", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("make-bytevector", "1-2", args.len()));
        }
        let size = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?;
        if size < 0 {
            return Err(SemaError::eval(format!(
                "make-bytevector: size must be non-negative, got {size}"
            )));
        }
        let fill = if args.len() == 2 {
            let f = args[1]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
            if !(0..=255).contains(&f) {
                return Err(SemaError::eval(format!(
                    "make-bytevector: fill value {f} out of range 0..255"
                )));
            }
            f as u8
        } else {
            0
        };
        Ok(Value::bytevector(vec![fill; size as usize]))
    });

    register_fn(env, "bytevector", |args| {
        let mut bytes = Vec::with_capacity(args.len());
        for (i, arg) in args.iter().enumerate() {
            let n = arg
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", arg.type_name()))?;
            if !(0..=255).contains(&n) {
                return Err(SemaError::eval(format!(
                    "bytevector: byte value {n} at index {i} out of range 0..255"
                )));
            }
            bytes.push(n as u8);
        }
        Ok(Value::bytevector(bytes))
    });

    register_fn(env, "bytevector-length", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("bytevector-length", "1", args.len()));
        }
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        Ok(Value::int(bv.len() as i64))
    });

    register_fn(env, "bytevector-u8-ref", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("bytevector-u8-ref", "2", args.len()));
        }
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        let idx = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        if idx < 0 || idx as usize >= bv.len() {
            return Err(SemaError::eval(format!(
                "bytevector-u8-ref: index {idx} out of range for bytevector of length {}",
                bv.len()
            )));
        }
        Ok(Value::int(bv[idx as usize] as i64))
    });

    register_fn(env, "bytevector-u8-set!", |args| {
        if args.len() != 3 {
            return Err(SemaError::arity("bytevector-u8-set!", "3", args.len()));
        }
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        let idx = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?;
        let byte = args[2]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?;
        if idx < 0 || idx as usize >= bv.len() {
            return Err(SemaError::eval(format!(
                "bytevector-u8-set!: index {idx} out of range for bytevector of length {}",
                bv.len()
            )));
        }
        if !(0..=255).contains(&byte) {
            return Err(SemaError::eval(format!(
                "bytevector-u8-set!: byte value {byte} out of range 0..255"
            )));
        }
        let mut new_bv = bv.to_vec();
        new_bv[idx as usize] = byte as u8;
        Ok(Value::bytevector(new_bv))
    });

    register_fn(env, "bytevector-copy", |args| {
        if args.is_empty() || args.len() > 3 {
            return Err(SemaError::arity("bytevector-copy", "1-3", args.len()));
        }
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        let start = if args.len() >= 2 {
            args[1]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
                as usize
        } else {
            0
        };
        let end = if args.len() == 3 {
            args[2]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?
                as usize
        } else {
            bv.len()
        };
        if start > end || end > bv.len() {
            return Err(SemaError::eval(format!(
                "bytevector-copy: range {start}..{end} out of bounds for bytevector of length {}",
                bv.len()
            )));
        }
        Ok(Value::bytevector(bv[start..end].to_vec()))
    });

    register_fn(env, "bytevector-append", |args| {
        let mut result = Vec::new();
        for arg in args {
            let bv = arg
                .as_bytevector()
                .ok_or_else(|| SemaError::type_error("bytevector", arg.type_name()))?;
            result.extend_from_slice(bv);
        }
        Ok(Value::bytevector(result))
    });

    register_fn(env, "bytevector->list", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("bytevector->list", "1", args.len()));
        }
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        let items: Vec<Value> = bv.iter().map(|&b| Value::int(b as i64)).collect();
        Ok(Value::list(items))
    });

    register_fn(env, "list->bytevector", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("list->bytevector", "1", args.len()));
        }
        let items = args[0]
            .as_list()
            .ok_or_else(|| SemaError::type_error("list", args[0].type_name()))?;
        let mut bytes = Vec::with_capacity(items.len());
        for item in items {
            let n = item
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", item.type_name()))?;
            if !(0..=255).contains(&n) {
                return Err(SemaError::eval(format!(
                    "list->bytevector: byte value {n} out of range 0..255"
                )));
            }
            bytes.push(n as u8);
        }
        Ok(Value::bytevector(bytes))
    });

    register_fn(env, "utf8->string", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("utf8->string", "1", args.len()));
        }
        let bv = args[0]
            .as_bytevector()
            .ok_or_else(|| SemaError::type_error("bytevector", args[0].type_name()))?;
        let s = String::from_utf8(bv.to_vec())
            .map_err(|e| SemaError::eval(format!("utf8->string: invalid UTF-8: {e}")))?;
        Ok(Value::string(&s))
    });

    register_fn(env, "string->utf8", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("string->utf8", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::bytevector(s.as_bytes().to_vec()))
    });
}
