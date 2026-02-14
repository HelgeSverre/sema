use std::cell::RefCell;
use std::collections::HashMap;

use crate::types::Usage;

#[derive(Debug, serde::Deserialize)]
struct PricingResponse {
    updated_at: String,
    prices: Vec<PricingEntry>,
}

#[derive(Debug, serde::Deserialize)]
struct PricingEntry {
    id: String,
    vendor: String,
    #[allow(dead_code)]
    name: String,
    input: f64,
    output: f64,
    #[allow(dead_code)]
    input_cached: Option<f64>,
}

struct FetchedPricing {
    entries: Vec<(String, String, f64, f64)>, // (id, vendor, input, output)
    updated_at: String,
}

thread_local! {
    static CUSTOM_PRICING: RefCell<HashMap<String, (f64, f64)>> = RefCell::new(HashMap::new());
    static FETCHED_PRICING: RefCell<Option<FetchedPricing>> = const { RefCell::new(None) };
}

/// Load fetched pricing data from a JSON string.
pub fn load_fetched_pricing_from_str(json: &str) -> Result<(), String> {
    let response: PricingResponse =
        serde_json::from_str(json).map_err(|e| format!("Failed to parse pricing JSON: {}", e))?;
    let entries = response
        .prices
        .into_iter()
        .map(|e| (e.id, e.vendor, e.input, e.output))
        .collect();
    FETCHED_PRICING.with(|p| {
        *p.borrow_mut() = Some(FetchedPricing {
            entries,
            updated_at: response.updated_at,
        });
    });
    Ok(())
}

/// Clear fetched pricing data.
pub fn clear_fetched_pricing() {
    FETCHED_PRICING.with(|p| {
        *p.borrow_mut() = None;
    });
}

/// Returns the `updated_at` timestamp of the fetched pricing data, if loaded.
pub fn fetched_pricing_updated_at() -> Option<String> {
    FETCHED_PRICING.with(|p| {
        p.borrow()
            .as_ref()
            .map(|fp| fp.updated_at.clone())
    })
}

fn lookup_fetched(model: &str) -> Option<(f64, f64)> {
    FETCHED_PRICING.with(|p| {
        let p = p.borrow();
        let fp = p.as_ref()?;

        // Exact match on id
        for (id, _, input, output) in &fp.entries {
            if model == id {
                return Some((*input, *output));
            }
        }

        // Exact match on vendor/id
        for (id, vendor, input, output) in &fp.entries {
            let qualified = format!("{}/{}", vendor, id);
            if model == qualified {
                return Some((*input, *output));
            }
        }

        // Substring match: model.contains(id), longest id wins
        let mut best: Option<(usize, f64, f64)> = None;
        for (id, _, input, output) in &fp.entries {
            if model.contains(id.as_str()) {
                if best.is_none() || id.len() > best.unwrap().0 {
                    best = Some((id.len(), *input, *output));
                }
            }
        }
        best.map(|(_, input, output)| (input, output))
    })
}

/// Returns (input_cost_per_million, output_cost_per_million) for a model.
pub fn model_pricing(model: &str) -> Option<(f64, f64)> {
    // 1. Custom pricing (user overrides)
    let custom = CUSTOM_PRICING.with(|p| {
        let p = p.borrow();
        for (pattern, pricing) in p.iter() {
            if model.contains(pattern) {
                return Some(*pricing);
            }
        }
        None
    });
    if custom.is_some() {
        return custom;
    }

    // 2. Fetched pricing (from llm-prices.com)
    if let Some(result) = lookup_fetched(model) {
        return Some(result);
    }

    // 3. Hardcoded fallback (may be stale)
    match model {
        // Anthropic
        m if m.contains("claude-3-5-haiku") || m.contains("claude-haiku-4-5") => Some((1.0, 5.0)),
        m if m.contains("claude-3-5-sonnet") || m.contains("claude-sonnet-4-5") => {
            Some((3.0, 15.0))
        }
        m if m.contains("claude-3-opus") || m.contains("claude-opus-4") => Some((15.0, 75.0)),
        // OpenAI
        m if m.contains("gpt-4o-mini") => Some((0.15, 0.60)),
        m if m.contains("gpt-4o") => Some((2.50, 10.0)),
        m if m.contains("gpt-4-turbo") => Some((10.0, 30.0)),
        m if m.contains("o1-mini") => Some((3.0, 12.0)),
        m if m.contains("o1") && !m.contains("o1-mini") => Some((15.0, 60.0)),
        // Google Gemini
        m if m.contains("gemini-2.0-flash") => Some((0.10, 0.40)),
        m if m.contains("gemini-1.5-flash") => Some((0.075, 0.30)),
        m if m.contains("gemini-1.5-pro") => Some((1.25, 5.00)),
        m if m.contains("gemini") => Some((0.10, 0.40)), // fallback for other gemini models
        // Groq (free tier, but track tokens)
        m if m.contains("llama") || m.contains("mixtral") || m.contains("gemma") => {
            Some((0.0, 0.0))
        }
        // xAI
        m if m.contains("grok-3-mini") => Some((0.30, 0.50)),
        m if m.contains("grok-3") => Some((3.00, 15.00)),
        m if m.contains("grok-2") => Some((2.00, 10.00)),
        // Mistral
        m if m.contains("mistral-small") => Some((0.10, 0.30)),
        m if m.contains("mistral-medium") => Some((2.70, 8.10)),
        m if m.contains("mistral-large") => Some((2.00, 6.00)),
        // Moonshot
        m if m.contains("moonshot") => Some((0.0, 0.0)),
        _ => None,
    }
}

/// Calculate cost in USD from usage.
pub fn calculate_cost(usage: &Usage) -> Option<f64> {
    model_pricing(&usage.model).map(|(input, output)| {
        (usage.prompt_tokens as f64 * input / 1_000_000.0)
            + (usage.completion_tokens as f64 * output / 1_000_000.0)
    })
}

/// Set custom pricing for a model pattern.
pub fn set_custom_pricing(model_pattern: &str, input_per_million: f64, output_per_million: f64) {
    CUSTOM_PRICING.with(|p| {
        p.borrow_mut().insert(
            model_pattern.to_string(),
            (input_per_million, output_per_million),
        );
    });
}

/// Write pricing JSON to cache file (atomic: write temp + rename).
pub fn write_pricing_cache(path: &std::path::Path, json: &str) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
    }
    let tmp = path.with_extension("tmp");
    std::fs::write(&tmp, json).map_err(|e| e.to_string())?;
    std::fs::rename(&tmp, path).map_err(|e| e.to_string())?;
    Ok(())
}

/// Read pricing JSON from cache file. Returns Ok(None) if file doesn't exist.
pub fn read_pricing_cache(path: &std::path::Path) -> Result<Option<String>, String> {
    match std::fs::read_to_string(path) {
        Ok(content) => Ok(Some(content)),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(e.to_string()),
    }
}

pub const PRICING_URL: &str = "https://www.llm-prices.com/current-v1.json";

/// Fetch pricing from llm-prices.com with a short timeout.
/// Returns Ok(json_string) on success, Err on any failure.
/// This function uses tokio runtime internally (same pattern as LLM providers).
pub fn fetch_pricing_from_remote() -> Result<String, String> {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(|e| e.to_string())?;

    rt.block_on(async {
        let client = reqwest::Client::builder()
            .connect_timeout(std::time::Duration::from_millis(500))
            .timeout(std::time::Duration::from_secs(2))
            .build()
            .map_err(|e| e.to_string())?;

        let resp = client
            .get(PRICING_URL)
            .send()
            .await
            .map_err(|e| e.to_string())?;

        resp.text().await.map_err(|e| e.to_string())
    })
}

/// Best-effort pricing refresh. Called during llm/auto-configure.
/// 1. Load disk cache into memory (fast, sync)
/// 2. Attempt network fetch (short timeout, swallow errors)
/// 3. On success, update memory + write disk cache
pub fn refresh_pricing(cache_path: Option<&std::path::Path>) {
    let cache = cache_path.and_then(|p| {
        read_pricing_cache(p).ok().flatten()
    });

    // Load cache into memory
    if let Some(ref json) = cache {
        let _ = load_fetched_pricing_from_str(json);
    }

    // Try network fetch (best-effort)
    match fetch_pricing_from_remote() {
        Ok(json) => {
            if load_fetched_pricing_from_str(&json).is_ok() {
                if let Some(p) = cache_path {
                    let _ = write_pricing_cache(p, &json);
                }
            }
        }
        Err(_) => {
            // Network unavailable — silently continue with cache or hardcoded
        }
    }
}

/// Return the default cache path: ~/.sema/pricing-cache.json
pub fn default_cache_path() -> Option<std::path::PathBuf> {
    std::env::var("HOME")
        .ok()
        .map(|home| std::path::PathBuf::from(home).join(".sema").join("pricing-cache.json"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fetched_pricing_overrides_hardcoded() {
        let json = r#"{
            "updated_at": "2025-10-10",
            "prices": [
                {"id": "gpt-4o-mini", "vendor": "openai", "name": "GPT-4o Mini", "input": 0.10, "output": 0.40, "input_cached": null}
            ]
        }"#;
        load_fetched_pricing_from_str(json).unwrap();
        let (input, output) = model_pricing("gpt-4o-mini").unwrap();
        assert!((input - 0.10).abs() < f64::EPSILON);
        assert!((output - 0.40).abs() < f64::EPSILON);
        clear_fetched_pricing();
    }

    #[test]
    fn test_hardcoded_fallback_when_no_fetch() {
        clear_fetched_pricing();
        let result = model_pricing("gpt-4o-mini");
        assert!(result.is_some());
    }

    #[test]
    fn test_custom_pricing_wins_over_fetched() {
        let json = r#"{
            "updated_at": "2025-10-10",
            "prices": [
                {"id": "my-model", "vendor": "custom", "name": "My Model", "input": 1.0, "output": 2.0, "input_cached": null}
            ]
        }"#;
        load_fetched_pricing_from_str(json).unwrap();
        set_custom_pricing("my-model", 5.0, 10.0);
        let (input, output) = model_pricing("my-model").unwrap();
        assert!((input - 5.0).abs() < f64::EPSILON);
        assert!((output - 10.0).abs() < f64::EPSILON);
        clear_fetched_pricing();
        CUSTOM_PRICING.with(|p| p.borrow_mut().clear());
    }

    #[test]
    fn test_fetched_substring_matching() {
        let json = r#"{
            "updated_at": "2025-10-10",
            "prices": [
                {"id": "claude-sonnet-4", "vendor": "anthropic", "name": "Claude Sonnet 4", "input": 3.0, "output": 15.0, "input_cached": null}
            ]
        }"#;
        load_fetched_pricing_from_str(json).unwrap();
        let result = model_pricing("claude-sonnet-4-20250514");
        assert!(result.is_some());
        let (input, _) = result.unwrap();
        assert!((input - 3.0).abs() < f64::EPSILON);
        clear_fetched_pricing();
    }

    #[test]
    fn test_malformed_json_returns_error() {
        let result = load_fetched_pricing_from_str("not json");
        assert!(result.is_err());
    }

    #[test]
    fn test_unknown_model_returns_none() {
        clear_fetched_pricing();
        assert!(model_pricing("totally-unknown-model-xyz").is_none());
    }

    #[test]
    fn test_cache_roundtrip() {
        let dir = std::env::temp_dir().join("sema-pricing-test");
        let _ = std::fs::create_dir_all(&dir);
        let cache_path = dir.join("pricing-cache.json");

        let json = r#"{
            "updated_at": "2025-10-10",
            "prices": [
                {"id": "test-model", "vendor": "test", "name": "Test", "input": 1.5, "output": 3.0, "input_cached": null}
            ]
        }"#;

        write_pricing_cache(&cache_path, json).unwrap();
        assert!(cache_path.exists());

        let loaded = read_pricing_cache(&cache_path).unwrap();
        assert!(loaded.is_some());
        let content = loaded.unwrap();
        assert!(content.contains("test-model"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_cache_read_missing_file() {
        let result = read_pricing_cache(std::path::Path::new("/nonexistent/path/cache.json"));
        assert!(result.is_ok());
        assert!(result.unwrap().is_none());
    }

    #[test]
    fn test_fetch_pricing_url_is_correct() {
        assert_eq!(PRICING_URL, "https://www.llm-prices.com/current-v1.json");
    }

    #[test]
    fn test_refresh_pricing_loads_cache_fallback() {
        clear_fetched_pricing();
        let fake_cache = std::env::temp_dir().join("sema-pricing-noexist").join("cache.json");
        refresh_pricing(Some(&fake_cache));
        // Hardcoded should still work
        assert!(model_pricing("gpt-4o-mini").is_some());
    }
}
