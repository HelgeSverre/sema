use std::cell::RefCell;
use std::collections::HashMap;

use crate::types::Usage;

thread_local! {
    static CUSTOM_PRICING: RefCell<HashMap<String, (f64, f64)>> = RefCell::new(HashMap::new());
}

/// Returns (input_cost_per_million, output_cost_per_million) for a model.
pub fn model_pricing(model: &str) -> Option<(f64, f64)> {
    // Check custom pricing first
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

    // Built-in pricing table
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
