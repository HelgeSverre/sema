# sema-llm

LLM provider integrations for the [Sema](https://sema-lang.com) programming language.

Provides multi-provider LLM support:

- **Providers** — Anthropic, OpenAI, Gemini, Groq, xAI, Mistral, Ollama, and any OpenAI-compatible endpoint
- **Auto-configuration** — detects API keys from environment variables
- **Dynamic pricing** — fetches model pricing from [llm-prices.com](https://www.llm-prices.com) with disk cache fallback
- **Budget tracking** — per-session cost tracking with configurable limits
- **Custom providers** — define providers in Sema code with `llm/define-provider`

## Usage

This is an internal crate. If you want to embed Sema in your application, use [`sema-lang`](https://crates.io/crates/sema-lang) instead:

```toml
[dependencies]
sema-lang = "1.6"
```

📖 [LLM primitives](https://sema-lang.com/docs/llm/) · [Documentation](https://sema-lang.com/docs/) · [GitHub](https://github.com/helgesverre/sema)
