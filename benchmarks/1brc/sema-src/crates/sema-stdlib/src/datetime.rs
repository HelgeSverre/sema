use std::collections::BTreeMap;
use std::rc::Rc;

use chrono::{DateTime, NaiveDateTime, TimeZone, Utc};
use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    register_fn(env, "time/now", |args| {
        if !args.is_empty() {
            return Err(SemaError::arity("time/now", "0", args.len()));
        }
        let now = Utc::now();
        let secs = now.timestamp() as f64 + now.timestamp_subsec_millis() as f64 / 1000.0;
        Ok(Value::Float(secs))
    });

    register_fn(env, "time/format", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("time/format", "2", args.len()));
        }
        let ts = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        let fmt = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let dt = timestamp_to_datetime(ts)?;
        Ok(Value::string(&dt.format(fmt).to_string()))
    });

    register_fn(env, "time/parse", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("time/parse", "2", args.len()));
        }
        let s = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let fmt = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        let naive = NaiveDateTime::parse_from_str(s, fmt)
            .map_err(|e| SemaError::eval(format!("time/parse: {e}")))?;
        let dt: DateTime<Utc> = Utc.from_utc_datetime(&naive);
        Ok(Value::Float(dt.timestamp() as f64))
    });

    register_fn(env, "time/date-parts", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("time/date-parts", "1", args.len()));
        }
        let ts = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        let dt = timestamp_to_datetime(ts)?;
        use chrono::Datelike;
        use chrono::Timelike;
        let mut map = BTreeMap::new();
        map.insert(Value::keyword("year"), Value::Int(dt.year() as i64));
        map.insert(Value::keyword("month"), Value::Int(dt.month() as i64));
        map.insert(Value::keyword("day"), Value::Int(dt.day() as i64));
        map.insert(Value::keyword("hour"), Value::Int(dt.hour() as i64));
        map.insert(Value::keyword("minute"), Value::Int(dt.minute() as i64));
        map.insert(Value::keyword("second"), Value::Int(dt.second() as i64));
        map.insert(
            Value::keyword("weekday"),
            Value::string(&dt.format("%A").to_string()),
        );
        Ok(Value::Map(Rc::new(map)))
    });

    register_fn(env, "time/add", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("time/add", "2", args.len()));
        }
        let ts = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        let secs = args[1]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
        Ok(Value::Float(ts + secs))
    });

    register_fn(env, "time/diff", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("time/diff", "2", args.len()));
        }
        let t1 = args[0]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[0].type_name()))?;
        let t2 = args[1]
            .as_float()
            .ok_or_else(|| SemaError::type_error("number", args[1].type_name()))?;
        Ok(Value::Float(t1 - t2))
    });
}

fn timestamp_to_datetime(ts: f64) -> Result<DateTime<Utc>, SemaError> {
    let secs = ts as i64;
    let nanos = ((ts - secs as f64) * 1_000_000_000.0) as u32;
    Utc.timestamp_opt(secs, nanos)
        .single()
        .ok_or_else(|| SemaError::eval("time: invalid timestamp"))
}
