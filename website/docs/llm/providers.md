---
outline: [2, 3]
---

# Provider Management

## Auto-Configuration

Sema auto-detects and configures all available providers from environment variables on startup. No manual setup is required — just set the API key for your provider.

### `llm/auto-configure`

Manually trigger auto-configuration (runs automatically on startup unless `--no-llm` is used).

```scheme
(llm/auto-configure)
```

## Manual Configuration

### `llm/configure`

Manually configure a provider with specific options.

```scheme
(llm/configure :anthropic {:api-key "sk-..."})

;; Ollama with custom host
(llm/configure :ollama {:host "http://localhost:11434"
                         :default-model "llama3"})
```

## Runtime Provider Switching

### `llm/list-providers`

List all configured providers.

```scheme
(llm/list-providers)   ; => (:anthropic :gemini :openai ...)
```

### `llm/current-provider`

Get the currently active provider and model.

```scheme
(llm/current-provider)   ; => {:name :anthropic :model "claude-sonnet-4-20250514"}
```

### `llm/set-default`

Switch the active provider at runtime.

```scheme
(llm/set-default :openai)
```

## Supported Providers

All providers are auto-configured from environment variables. Use `(llm/configure :provider {...})` for manual setup.

| Provider          | Type           | Chat | Stream | Tools | Embeddings |
| ----------------- | -------------- | ---- | ------ | ----- | ---------- |
| **Anthropic**     | Native         | ✅   | ✅     | ✅    | —          |
| **OpenAI**        | Native         | ✅   | ✅     | ✅    | ✅         |
| **Google Gemini** | Native         | ✅   | ✅     | ✅    | —          |
| **Ollama**        | Native (local) | ✅   | ✅     | ✅    | —          |
| **Groq**          | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **xAI**           | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **Mistral**       | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **Moonshot**      | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **Jina**          | Embedding-only | —    | —      | —     | ✅         |
| **Voyage**        | Embedding-only | —    | —      | —     | ✅         |
| **Cohere**        | Embedding-only | —    | —      | —     | ✅         |

## Environment Variables

| Variable             | Description                                           |
| -------------------- | ----------------------------------------------------- |
| `ANTHROPIC_API_KEY`  | Anthropic API key                                     |
| `OPENAI_API_KEY`     | OpenAI API key                                        |
| `GROQ_API_KEY`       | Groq API key                                          |
| `XAI_API_KEY`        | xAI/Grok API key                                      |
| `MISTRAL_API_KEY`    | Mistral API key                                       |
| `MOONSHOT_API_KEY`   | Moonshot API key                                      |
| `GOOGLE_API_KEY`     | Google Gemini API key                                 |
| `OLLAMA_HOST`        | Ollama server URL (default: `http://localhost:11434`) |
| `JINA_API_KEY`       | Jina embeddings API key                               |
| `VOYAGE_API_KEY`     | Voyage embeddings API key                             |
| `COHERE_API_KEY`     | Cohere embeddings API key                             |
| `SEMA_DEFAULT_MODEL` | Default model name                                    |
| `SEMA_LLM_PROVIDER`  | Preferred provider                                    |
