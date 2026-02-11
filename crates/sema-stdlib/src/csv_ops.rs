use std::collections::BTreeMap;
use std::rc::Rc;

use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "csv/parse", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("csv/parse", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut rdr = csv::ReaderBuilder::new()
            .has_headers(false)
            .from_reader(s.as_bytes());
        let mut rows = Vec::new();
        for result in rdr.records() {
            let record = result.map_err(|e| SemaError::eval(format!("csv/parse: {e}")))?;
            let row: Vec<Value> = record
                .iter()
                .map(|field| Value::String(Rc::new(field.to_string())))
                .collect();
            rows.push(Value::list(row));
        }
        Ok(Value::list(rows))
    });

    register_fn(env, "csv/parse-maps", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("csv/parse-maps", "1", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut rdr = csv::ReaderBuilder::new()
            .has_headers(true)
            .from_reader(s.as_bytes());
        let headers: Vec<String> = rdr
            .headers()
            .map_err(|e| SemaError::eval(format!("csv/parse-maps: {e}")))?
            .iter()
            .map(|h| h.to_string())
            .collect();
        let mut rows = Vec::new();
        for result in rdr.records() {
            let record = result.map_err(|e| SemaError::eval(format!("csv/parse-maps: {e}")))?;
            let mut map = BTreeMap::new();
            for (i, field) in record.iter().enumerate() {
                if let Some(header) = headers.get(i) {
                    map.insert(
                        Value::keyword(header),
                        Value::String(Rc::new(field.to_string())),
                    );
                }
            }
            rows.push(Value::Map(Rc::new(map)));
        }
        Ok(Value::list(rows))
    });

    register_fn(env, "csv/encode", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("csv/encode", "1", args.len()));
        }
        let rows = match &args[0] {
            Value::List(l) => l.as_ref().clone(),
            _ => return Err(SemaError::type_error("list", args[0].type_name())),
        };
        let mut wtr = csv::WriterBuilder::new().from_writer(Vec::new());
        for row in &rows {
            let fields: Vec<String> = match row {
                Value::List(l) => l
                    .iter()
                    .map(|v| match v {
                        Value::String(s) => s.to_string(),
                        other => other.to_string(),
                    })
                    .collect(),
                Value::Vector(v) => v
                    .iter()
                    .map(|val| match val {
                        Value::String(s) => s.to_string(),
                        other => other.to_string(),
                    })
                    .collect(),
                _ => return Err(SemaError::type_error("list", row.type_name())),
            };
            wtr.write_record(&fields)
                .map_err(|e| SemaError::eval(format!("csv/encode: {e}")))?;
        }
        let bytes = wtr
            .into_inner()
            .map_err(|e| SemaError::eval(format!("csv/encode: {e}")))?;
        let s =
            String::from_utf8(bytes).map_err(|e| SemaError::eval(format!("csv/encode: {e}")))?;
        Ok(Value::String(Rc::new(s)))
    });
}
