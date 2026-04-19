use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::time::Duration;

use sema_core::{check_arity, SemaError, Value};

use crate::register_fn;

// Thread-local serial port storage, keyed by an incrementing handle ID.
thread_local! {
    static PORTS: RefCell<HashMap<u64, BufReader<Box<dyn serialport::SerialPort>>>> = RefCell::new(HashMap::new());
    static NEXT_ID: RefCell<u64> = const { RefCell::new(1) };
}

fn next_handle() -> u64 {
    NEXT_ID.with(|id| {
        let h = *id.borrow();
        *id.borrow_mut() = h + 1;
        h
    })
}

pub fn register(env: &sema_core::Env) {
    // (serial/list) => list of available port names
    register_fn(env, "serial/list", |args| {
        check_arity!(args, "serial/list", 0);
        let ports = serialport::available_ports()
            .map_err(|e| SemaError::eval(format!("serial/list: {e}")))?;
        let names: Vec<Value> = ports.iter().map(|p| Value::string(&p.port_name)).collect();
        Ok(Value::list(names))
    });

    // (serial/open path baud) => handle (int)
    // (serial/open path baud timeout_ms) => handle (int)
    register_fn(env, "serial/open", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("serial/open", "2-3", args.len()));
        }
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let baud = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[1].type_name()))?
            as u32;
        let timeout_ms = if args.len() == 3 {
            args[2]
                .as_int()
                .ok_or_else(|| SemaError::type_error("int", args[2].type_name()))?
                as u64
        } else {
            2000
        };

        let port = serialport::new(path, baud)
            .timeout(Duration::from_millis(timeout_ms))
            .open()
            .map_err(|e| {
                SemaError::eval(format!("serial/open: {e}"))
                    .with_hint(format!("path={path}, baud={baud}"))
            })?;

        let handle = next_handle();
        let reader = BufReader::new(port);
        PORTS.with(|ports| ports.borrow_mut().insert(handle, reader));
        Ok(Value::int(handle as i64))
    });

    // (serial/close handle) => nil
    register_fn(env, "serial/close", |args| {
        check_arity!(args, "serial/close", 1);
        let handle = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            as u64;
        PORTS.with(|ports| {
            if ports.borrow_mut().remove(&handle).is_none() {
                return Err(SemaError::eval(format!(
                    "serial/close: invalid handle {handle}"
                )));
            }
            Ok(Value::nil())
        })
    });

    // (serial/write handle string) => nil
    register_fn(env, "serial/write", |args| {
        check_arity!(args, "serial/write", 2);
        let handle = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            as u64;
        let data = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        PORTS.with(|ports| {
            let mut ports = ports.borrow_mut();
            let reader = ports
                .get_mut(&handle)
                .ok_or_else(|| SemaError::eval(format!("serial/write: invalid handle {handle}")))?;
            let port = reader.get_mut();
            port.write_all(data.as_bytes())
                .map_err(|e| SemaError::eval(format!("serial/write: {e}")))?;
            port.flush()
                .map_err(|e| SemaError::eval(format!("serial/write flush: {e}")))?;
            Ok(Value::nil())
        })
    });

    // (serial/read-line handle) => string (reads until \n)
    register_fn(env, "serial/read-line", |args| {
        check_arity!(args, "serial/read-line", 1);
        let handle = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            as u64;
        PORTS.with(|ports| {
            let mut ports = ports.borrow_mut();
            let reader = ports.get_mut(&handle).ok_or_else(|| {
                SemaError::eval(format!("serial/read-line: invalid handle {handle}"))
            })?;
            let mut line = String::new();
            reader
                .read_line(&mut line)
                .map_err(|e| SemaError::eval(format!("serial/read-line: {e}")))?;
            // Trim trailing \r\n
            let trimmed = line.trim_end_matches(['\r', '\n']);
            Ok(Value::string(trimmed))
        })
    });

    // (serial/send handle command) => parsed JSON response
    // Sends command + \n, reads one line back, parses as JSON.
    // Convenience for the sema-bridge protocol.
    register_fn(env, "serial/send", |args| {
        check_arity!(args, "serial/send", 2);
        let handle = args[0]
            .as_int()
            .ok_or_else(|| SemaError::type_error("int", args[0].type_name()))?
            as u64;
        let cmd = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        PORTS.with(|ports| {
            let mut ports = ports.borrow_mut();
            let reader = ports
                .get_mut(&handle)
                .ok_or_else(|| SemaError::eval(format!("serial/send: invalid handle {handle}")))?;

            // Write command + newline
            let port = reader.get_mut();
            port.write_all(cmd.as_bytes())
                .map_err(|e| SemaError::eval(format!("serial/send write: {e}")))?;
            port.write_all(b"\n")
                .map_err(|e| SemaError::eval(format!("serial/send write: {e}")))?;
            port.flush()
                .map_err(|e| SemaError::eval(format!("serial/send flush: {e}")))?;

            // Read response line
            let mut line = String::new();
            reader
                .read_line(&mut line)
                .map_err(|e| SemaError::eval(format!("serial/send read: {e}")))?;

            // Parse JSON response
            let trimmed = line.trim();
            if trimmed.is_empty() {
                return Ok(Value::nil());
            }
            let json_val: serde_json::Value = serde_json::from_str(trimmed)
                .map_err(|e| SemaError::eval(format!("serial/send parse: {e}: {trimmed}")))?;
            Ok(sema_core::json::json_to_value(&json_val))
        })
    });
}
