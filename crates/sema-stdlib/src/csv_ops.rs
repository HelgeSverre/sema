use std::collections::BTreeMap;

use sema_core::{check_arity, SemaError, Value, ValueView};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "csv/parse", |args| {
        check_arity!(args, "csv/parse", 1);
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let mut rdr = csv::ReaderBuilder::new()
            .has_headers(false)
            .from_reader(s.as_bytes());
        let mut rows = Vec::new();
        for result in rdr.records() {
            let record = result.map_err(|e| SemaError::eval(format!("csv/parse: {e}")))?;
            let row: Vec<Value> = record.iter().map(Value::string).collect();
            rows.push(Value::list(row));
        }
        Ok(Value::list(rows))
    });

    register_fn(env, "csv/parse-maps", |args| {
        check_arity!(args, "csv/parse-maps", 1);
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
                    map.insert(Value::keyword(header), Value::string(field));
                }
            }
            rows.push(Value::map(map));
        }
        Ok(Value::list(rows))
    });

    register_fn(env, "csv/encode", |args| {
        check_arity!(args, "csv/encode", 1);
        let rows = match args[0].view() {
            ValueView::List(l) => l.as_ref().clone(),
            _ => return Err(SemaError::type_error("list", args[0].type_name())),
        };
        let mut wtr = csv::WriterBuilder::new().from_writer(Vec::new());
        for row in &rows {
            let fields: Vec<String> = match row.view() {
                ValueView::List(l) => l
                    .iter()
                    .map(|v| match v.as_str() {
                        Some(s) => s.to_string(),
                        None => v.to_string(),
                    })
                    .collect(),
                ValueView::Vector(v) => v
                    .iter()
                    .map(|val| match val.as_str() {
                        Some(s) => s.to_string(),
                        None => val.to_string(),
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
        Ok(Value::string(&s))
    });
}
