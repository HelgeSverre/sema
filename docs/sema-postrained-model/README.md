# Sema Post-Trained Model — Research Index

Investigation into custom-training a GLM model on Fireworks.ai to create a hosted,
Sema-aware coding assistant. Assessed on 2026-06-24.

> **Note**: The directory name "postrained" is a portmanteau of "post-trained" — the
> standard term for any training applied after a model's initial pretraining (includes
> SFT, RFT, DPO, and continued pretraining).

---

## Documents

| Document | Purpose |
|----------|---------|
| [01-fireworks-platform.md](01-fireworks-platform.md) | Fireworks.ai capabilities: fine-tuning methods, supported models, pricing, deployment |
| [02-glm-model-analysis.md](02-glm-model-analysis.md) | GLM model family: architecture, open-weight status, fine-tune feasibility, the GLM-5.2 vs 5.1 constraint |
| [03-training-data-inventory.md](03-training-data-inventory.md) | Complete inventory of Sema codebase training data sources |
| [04-training-data-pipeline.md](04-training-data-pipeline.md) | How to transform codebase artifacts into training-ready JSONL datasets |
| [05-approaches-compared.md](05-approaches-compared.md) | Fine-tuning vs RAG vs system prompts vs RFT vs tool-augmented — cost/quality matrix |
| [06-recommended-strategy.md](06-recommended-strategy.md) | The recommended layered strategy with concrete steps and cost estimates |
| [07-deployment-and-integration.md](07-deployment-and-integration.md) | Hosting options, API integration with Sema's existing LLM client, self-hosting alternatives |

---

## Executive Summary

**Can we custom-train a GLM model on Fireworks.ai for Sema?**

Yes, with one critical constraint: **GLM 5.2 (the latest) is NOT fine-tunable on Fireworks**.
GLM 5.1 IS fine-tunable (200K context, LoRA + full-param SFT + RFT). Alternatively, GLM 5.2
can be fine-tuned off-Fireworks using Unsloth/LLaMA Factory and the LoRA adapter uploaded to
Fireworks for serving.

**Recommended approach** (layered, by cost tier):

1. **Tier 0 (free)**: Comprehensive system prompt with Sema syntax reference + few-shot examples
2. **Tier 1 (free)**: RFT on Fireworks using a small model (Qwen3-8B, free for <16B) with a
   grader that executes Sema code through the VM and checks output
3. **Tier 2 ($1-10)**: LoRA SFT on Fireworks using GLM 5.1, trained on ~1,000-2,000
   instruction-tuning pairs extracted from the codebase
4. **Tier 3 ($7/hr ongoing)**: Deploy as a dedicated endpoint on Fireworks, or self-host with vLLM

**Total estimated Sema training data available**: ~58,000+ lines of unique, high-quality Sema
language content across 224 example files, 2,374 test cases, 826 API docs entries, 1,024
documentation code blocks, and the prelude macros.

**Total estimated training cost**: $0.75-$4.50 for a single LoRA SFT run (depending on model size).
RFT is free for models under 16B parameters.

**Ongoing hosting cost**: $7/hr (H100 dedicated) on Fireworks, or self-hosted on cloud GPU
rental, or free for local development via Ollama.
