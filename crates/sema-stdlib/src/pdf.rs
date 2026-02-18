use std::collections::BTreeMap;

use sema_core::{check_arity, Caps, SemaError, Value};

pub fn register(env: &sema_core::Env, sandbox: &sema_core::Sandbox) {
    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "pdf/extract-text",
        &[0],
        |args| {
            check_arity!(args, "pdf/extract-text", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let bytes = std::fs::read(path)
                .map_err(|e| SemaError::Io(format!("pdf/extract-text {path}: {e}")))?;
            let text = pdf_extract::extract_text_from_mem(&bytes)
                .map_err(|e| SemaError::eval(format!("pdf/extract-text {path}: {e}")))?;
            Ok(Value::string(&text))
        },
    );

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "pdf/extract-text-pages",
        &[0],
        |args| {
            check_arity!(args, "pdf/extract-text-pages", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let pages = pdf_extract::extract_text_by_pages(path)
                .map_err(|e| SemaError::eval(format!("pdf/extract-text-pages {path}: {e}")))?;
            let values: Vec<Value> = pages.iter().map(|s| Value::string(s)).collect();
            Ok(Value::list(values))
        },
    );

    crate::register_fn_path_gated(
        env,
        sandbox,
        Caps::FS_READ,
        "pdf/page-count",
        &[0],
        |args| {
            check_arity!(args, "pdf/page-count", 1);
            let path = args[0]
                .as_str()
                .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
            let doc = lopdf::Document::load(path)
                .map_err(|e| SemaError::Io(format!("pdf/page-count {path}: {e}")))?;
            Ok(Value::int(doc.get_pages().len() as i64))
        },
    );

    crate::register_fn_path_gated(env, sandbox, Caps::FS_READ, "pdf/metadata", &[0], |args| {
        check_arity!(args, "pdf/metadata", 1);
        let path = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let doc = lopdf::Document::load(path)
            .map_err(|e| SemaError::Io(format!("pdf/metadata {path}: {e}")))?;

        let page_count = doc.get_pages().len() as i64;
        let mut map = BTreeMap::new();

        let extract_field = |dict: &lopdf::Dictionary, key: &[u8]| -> Option<String> {
            dict.get(key).ok().and_then(|val| {
                val.as_name()
                    .map(|s| String::from_utf8_lossy(s).to_string())
                    .ok()
                    .or_else(|| {
                        val.as_str()
                            .map(|s| String::from_utf8_lossy(s).to_string())
                            .ok()
                    })
            })
        };

        if let Ok(info_ref) = doc.trailer.get(b"Info") {
            if let Ok(info_obj) = doc.dereference(info_ref) {
                if let Ok(dict) = info_obj.1.as_dict() {
                    if let Some(v) = extract_field(dict, b"Title") {
                        map.insert(Value::keyword("title"), Value::string(&v));
                    }
                    if let Some(v) = extract_field(dict, b"Author") {
                        map.insert(Value::keyword("author"), Value::string(&v));
                    }
                    if let Some(v) = extract_field(dict, b"Subject") {
                        map.insert(Value::keyword("subject"), Value::string(&v));
                    }
                    if let Some(v) = extract_field(dict, b"Creator") {
                        map.insert(Value::keyword("creator"), Value::string(&v));
                    }
                    if let Some(v) = extract_field(dict, b"Producer") {
                        map.insert(Value::keyword("producer"), Value::string(&v));
                    }
                }
            }
        }

        map.insert(Value::keyword("pages"), Value::int(page_count));
        Ok(Value::map(map))
    });
}
