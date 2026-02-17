use sema_core::{SemaError, Value};

use crate::register_fn;

pub fn register(env: &sema_core::Env) {
    // (text/chunk text) or (text/chunk text {:size 1000 :overlap 200})
    register_fn(env, "text/chunk", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("text/chunk", "1-2", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        if text.is_empty() {
            return Ok(Value::list(vec![]));
        }

        let mut chunk_size: usize = 1000;
        let mut overlap: usize = 200;
        if let Some(opts) = args.get(1).and_then(|v| v.as_map_rc()) {
            if let Some(v) = opts.get(&Value::keyword("size")).and_then(|v| v.as_int()) {
                chunk_size = v.max(1) as usize;
            }
            if let Some(v) = opts
                .get(&Value::keyword("overlap"))
                .and_then(|v| v.as_int())
            {
                overlap = v.max(0) as usize;
            }
        }
        if overlap >= chunk_size {
            overlap = 0;
        }
        let chunks = recursive_chunk(text, chunk_size, overlap);
        Ok(Value::list(
            chunks.into_iter().map(|s| Value::string(&s)).collect(),
        ))
    });

    // (text/chunk-by-separator text separator)
    register_fn(env, "text/chunk-by-separator", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("text/chunk-by-separator", "2", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let sep = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;
        if text.is_empty() {
            return Ok(Value::list(vec![]));
        }
        let chunks: Vec<Value> = text
            .split(sep)
            .filter(|s| !s.is_empty())
            .map(Value::string)
            .collect();
        Ok(Value::list(chunks))
    });

    // (text/split-sentences text)
    register_fn(env, "text/split-sentences", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("text/split-sentences", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        if text.is_empty() {
            return Ok(Value::list(vec![]));
        }
        let sentences = split_sentences(text);
        Ok(Value::list(
            sentences.into_iter().map(|s| Value::string(&s)).collect(),
        ))
    });

    // --- Task 5: Text Cleaning ---

    register_fn(env, "text/clean-whitespace", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("text/clean-whitespace", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(
            &text.split_whitespace().collect::<Vec<_>>().join(" "),
        ))
    });

    register_fn(env, "text/strip-html", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("text/strip-html", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(&strip_html(text)))
    });

    register_fn(env, "text/truncate", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("text/truncate", "2-3", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let max_len = args[1]
            .as_int()
            .ok_or_else(|| SemaError::type_error("integer", args[1].type_name()))?
            as usize;
        let suffix = args
            .get(2)
            .and_then(|v| v.as_str())
            .unwrap_or("...")
            .to_string();
        let char_count = text.chars().count();
        if char_count <= max_len {
            return Ok(Value::string(text));
        }
        let suffix_len = suffix.chars().count();
        if max_len <= suffix_len {
            return Ok(Value::string(&suffix));
        }
        let take = max_len - suffix_len;
        let truncated: String = text.chars().take(take).collect();
        Ok(Value::string(&format!("{truncated}{suffix}")))
    });

    register_fn(env, "text/word-count", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("text/word-count", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::int(text.split_whitespace().count() as i64))
    });

    register_fn(env, "text/trim-indent", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("text/trim-indent", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(&trim_indent(text)))
    });

    // --- Task 6: Prompt Templates ---

    register_fn(env, "prompt/template", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("prompt/template", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(text))
    });

    register_fn(env, "prompt/render", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("prompt/render", "2", args.len()));
        }
        let template = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let vars = args[1]
            .as_map_rc()
            .ok_or_else(|| SemaError::type_error("map", args[1].type_name()))?;
        Ok(Value::string(&render_template(template, &vars)))
    });

    // --- Task 15: Document Metadata ---

    register_fn(env, "document/create", |args| {
        if args.len() != 2 {
            return Err(SemaError::arity("document/create", "2", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let metadata = args[1]
            .as_map_rc()
            .ok_or_else(|| SemaError::type_error("map", args[1].type_name()))?;
        let mut doc = std::collections::BTreeMap::new();
        doc.insert(Value::keyword("text"), Value::string(text));
        doc.insert(Value::keyword("metadata"), Value::map((*metadata).clone()));
        Ok(Value::map(doc))
    });

    register_fn(env, "document/text", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("document/text", "1", args.len()));
        }
        let map = args[0]
            .as_map_rc()
            .ok_or_else(|| SemaError::type_error("map (document)", args[0].type_name()))?;
        map.get(&Value::keyword("text"))
            .cloned()
            .ok_or_else(|| SemaError::eval("not a document: missing :text"))
    });

    register_fn(env, "document/metadata", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("document/metadata", "1", args.len()));
        }
        let map = args[0]
            .as_map_rc()
            .ok_or_else(|| SemaError::type_error("map (document)", args[0].type_name()))?;
        map.get(&Value::keyword("metadata"))
            .cloned()
            .ok_or_else(|| SemaError::eval("not a document: missing :metadata"))
    });

    register_fn(env, "document/chunk", |args| {
        if args.is_empty() || args.len() > 2 {
            return Err(SemaError::arity("document/chunk", "1-2", args.len()));
        }
        let doc = args[0]
            .as_map_rc()
            .ok_or_else(|| SemaError::type_error("map (document)", args[0].type_name()))?;
        let text = doc
            .get(&Value::keyword("text"))
            .and_then(|v| v.as_str())
            .ok_or_else(|| SemaError::eval("document/chunk: document missing :text"))?;
        let base_metadata = doc
            .get(&Value::keyword("metadata"))
            .and_then(|v| v.as_map_rc())
            .map(|m| (*m).clone())
            .unwrap_or_default();

        let mut chunk_size: usize = 1000;
        let mut overlap: usize = 200;
        if let Some(opts) = args.get(1).and_then(|v| v.as_map_rc()) {
            if let Some(v) = opts.get(&Value::keyword("size")).and_then(|v| v.as_int()) {
                chunk_size = v.max(1) as usize;
            }
            if let Some(v) = opts
                .get(&Value::keyword("overlap"))
                .and_then(|v| v.as_int())
            {
                overlap = v.max(0) as usize;
            }
        }
        if overlap >= chunk_size {
            overlap = 0;
        }

        let chunks = recursive_chunk(text, chunk_size, overlap);
        let total = chunks.len() as i64;
        let result: Vec<Value> = chunks
            .into_iter()
            .enumerate()
            .map(|(i, chunk_text)| {
                let mut meta = base_metadata.clone();
                meta.insert(Value::keyword("chunk-index"), Value::int(i as i64));
                meta.insert(Value::keyword("total-chunks"), Value::int(total));
                let mut doc_map = std::collections::BTreeMap::new();
                doc_map.insert(Value::keyword("text"), Value::string(&chunk_text));
                doc_map.insert(Value::keyword("metadata"), Value::map(meta));
                Value::map(doc_map)
            })
            .collect();

        Ok(Value::list(result))
    });

    // text/excerpt — extract a snippet around a match with omission markers
    register_fn(env, "text/excerpt", |args| {
        if args.len() < 2 || args.len() > 3 {
            return Err(SemaError::arity("text/excerpt", "2-3", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        let query = args[1]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[1].type_name()))?;

        let mut radius: usize = 100;
        let mut omission = "...".to_string();
        if let Some(opts) = args.get(2).and_then(|v| v.as_map_rc()) {
            if let Some(v) = opts.get(&Value::keyword("radius")).and_then(|v| v.as_int()) {
                radius = v.max(0) as usize;
            }
            if let Some(v) = opts
                .get(&Value::keyword("omission"))
                .and_then(|v| v.as_str())
            {
                omission = v.to_string();
            }
        }

        let lower_text = text.to_lowercase();
        let lower_query = query.to_lowercase();
        match lower_text.find(&lower_query) {
            None => Ok(Value::nil()),
            Some(byte_idx) => {
                let chars: Vec<char> = text.chars().collect();
                let char_idx = text[..byte_idx].chars().count();
                let query_char_len = query.chars().count();

                let start = char_idx.saturating_sub(radius);
                let end = (char_idx + query_char_len + radius).min(chars.len());

                let snippet: String = chars[start..end].iter().collect();

                let mut result = String::new();
                if start > 0 {
                    result.push_str(&omission);
                }
                result.push_str(&snippet);
                if end < chars.len() {
                    result.push_str(&omission);
                }
                Ok(Value::string(&result))
            }
        }
    });

    // text/normalize-newlines — convert \r\n and \r to \n
    register_fn(env, "text/normalize-newlines", |args| {
        if args.len() != 1 {
            return Err(SemaError::arity("text/normalize-newlines", "1", args.len()));
        }
        let text = args[0]
            .as_str()
            .ok_or_else(|| SemaError::type_error("string", args[0].type_name()))?;
        Ok(Value::string(
            &text.replace("\r\n", "\n").replace('\r', "\n"),
        ))
    });
}

// --- Chunking helpers ---

const SEPARATORS: &[&str] = &["\n\n", "\n", ". ", "! ", "? ", "; ", ", ", " "];

fn recursive_chunk(text: &str, max_size: usize, overlap: usize) -> Vec<String> {
    if text.len() <= max_size {
        return vec![text.to_string()];
    }
    for sep in SEPARATORS {
        let parts: Vec<&str> = text.split(sep).collect();
        if parts.len() > 1 {
            return merge_splits(&parts, sep, max_size, overlap);
        }
    }
    hard_chunk(text, max_size, overlap)
}

fn merge_splits(parts: &[&str], sep: &str, max_size: usize, overlap: usize) -> Vec<String> {
    let mut chunks = Vec::new();
    let mut current = String::new();
    for part in parts {
        let with_sep = if current.is_empty() {
            part.to_string()
        } else {
            format!("{}{}{}", current, sep, part)
        };
        if with_sep.len() <= max_size {
            current = with_sep;
        } else {
            if !current.is_empty() {
                chunks.push(current.clone());
            }
            if part.len() > max_size {
                chunks.extend(recursive_chunk(part, max_size, overlap));
                current = String::new();
            } else {
                current = part.to_string();
            }
        }
    }
    if !current.is_empty() {
        chunks.push(current);
    }
    if overlap > 0 && chunks.len() > 1 {
        apply_overlap(&chunks, overlap)
    } else {
        chunks
    }
}

fn apply_overlap(chunks: &[String], overlap: usize) -> Vec<String> {
    let mut result = vec![chunks[0].clone()];
    for i in 1..chunks.len() {
        let prev = &chunks[i - 1];
        let ov = if prev.len() > overlap {
            &prev[prev.len() - overlap..]
        } else {
            prev.as_str()
        };
        result.push(format!("{}{}", ov, chunks[i]));
    }
    result
}

fn hard_chunk(text: &str, max_size: usize, overlap: usize) -> Vec<String> {
    let mut chunks = Vec::new();
    let chars: Vec<char> = text.chars().collect();
    let step = if overlap < max_size {
        max_size - overlap
    } else {
        max_size
    };
    let mut i = 0;
    while i < chars.len() {
        let end = (i + max_size).min(chars.len());
        chunks.push(chars[i..end].iter().collect());
        i += step;
    }
    chunks
}

fn split_sentences(text: &str) -> Vec<String> {
    let mut sentences = Vec::new();
    let mut current = String::new();
    let chars: Vec<char> = text.chars().collect();
    for i in 0..chars.len() {
        current.push(chars[i]);
        if (chars[i] == '.' || chars[i] == '!' || chars[i] == '?')
            && (i + 1 >= chars.len() || chars[i + 1].is_whitespace())
        {
            let trimmed = current.trim().to_string();
            if !trimmed.is_empty() {
                sentences.push(trimmed);
            }
            current = String::new();
        }
    }
    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        sentences.push(trimmed);
    }
    sentences
}

// --- Text cleaning helpers ---

fn strip_html(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let mut in_tag = false;
    for ch in text.chars() {
        match ch {
            '<' => in_tag = true,
            '>' => in_tag = false,
            _ if !in_tag => result.push(ch),
            _ => {}
        }
    }
    result
        .replace("&amp;", "&")
        .replace("&lt;", "<")
        .replace("&gt;", ">")
        .replace("&quot;", "\"")
        .replace("&#39;", "'")
        .replace("&apos;", "'")
        .replace("&nbsp;", " ")
}

fn trim_indent(text: &str) -> String {
    if text.is_empty() {
        return String::new();
    }
    let lines: Vec<&str> = text.split('\n').collect();
    let min_indent = lines
        .iter()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);
    lines
        .iter()
        .map(|line| {
            if line.len() >= min_indent {
                &line[min_indent..]
            } else {
                line.trim_start()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

// --- Template helpers ---

fn render_template(template: &str, vars: &std::collections::BTreeMap<Value, Value>) -> String {
    let mut result = String::with_capacity(template.len());
    let mut chars = template.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '{' && chars.peek() == Some(&'{') {
            chars.next();
            let mut var_name = String::new();
            let mut found_close = false;
            while let Some(c) = chars.next() {
                if c == '}' && chars.peek() == Some(&'}') {
                    chars.next();
                    found_close = true;
                    break;
                }
                var_name.push(c);
            }
            if found_close {
                if let Some(val) = vars.get(&Value::keyword(&var_name)) {
                    if let Some(s) = val.as_str() {
                        result.push_str(s);
                    } else {
                        result.push_str(&val.to_string());
                    }
                } else {
                    result.push_str("{{");
                    result.push_str(&var_name);
                    result.push_str("}}");
                }
            } else {
                result.push_str("{{");
                result.push_str(&var_name);
            }
        } else {
            result.push(ch);
        }
    }
    result
}
