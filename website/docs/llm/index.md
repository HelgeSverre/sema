---
outline: [2, 3]
---

# LLM Primitives

Sema's differentiating feature: LLM operations are first-class language primitives with prompts, conversations, tools, and agents as native data types.

## Setup

Set one or more API keys as environment variables:

```bash
export ANTHROPIC_API_KEY=sk-ant-...
export OPENAI_API_KEY=sk-...
# or any other supported provider
```

Sema auto-detects and configures all available providers on startup. Use `--no-llm` to skip auto-configuration.

See [Provider Management](./providers.md) for the full list of supported providers and configuration options.

## Features

### [Completion & Chat](./completion.md)

Simple completions, multi-message chat, and streaming responses.

### [Prompts & Messages](./prompts.md)

Prompts as composable s-expressions, message construction, and prompt inspection.

### [Conversations](./conversations.md)

Persistent, immutable conversation state with automatic LLM round-trips.

### [Tools & Agents](./tools-agents.md)

Define tools the LLM can invoke, and build agents with system prompts, tools, and multi-turn loops.

### [Embeddings & Similarity](./embeddings.md)

Generate embeddings and compute cosine similarity.

### [Structured Extraction](./extraction.md)

Extract structured data from text and classify inputs.

### [Provider Management](./providers.md)

Auto-configuration, runtime provider switching, and supported provider table.

### [Cost Tracking & Budgets](./cost.md)

Usage tracking, budget enforcement, and batch/parallel operations.
