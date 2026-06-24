# 02 — GLM Model Family Analysis

Analysis of GLM models available on Fireworks.ai, their fine-tuning feasibility, and the
critical GLM-5.2 vs 5.1 distinction.

---

## 1. GLM Model Family on Fireworks

| Model | Serverless? | Fine-Tunable? | Context | Input $/M | Output $/M |
|-------|------------|---------------|---------|-----------|------------|
| **GLM 5.2** | ✅ | ❌ **Not supported** | 1,048,576 | $1.40 | $4.40 |
| **GLM 5.2 FP8** | ✅ | ❌ Not supported | 1,048,576 | — | — |
| **GLM 5.1** | ✅ | ✅ **Supported** | 200,000 | $1.40 | $4.40 |
| GLM-5 | ✅ | 🔴 Unknown | 202,752 | — | — |
| GLM-4.7 | ✅ | 🔴 Unknown | 202,752 | — | — |
| GLM-4.7 Flash | ✅ | 🔴 Unknown | 202,752 | — | — |
| GLM-4.6 | ✅ | 🔴 Unknown | 202,752 | — | — |
| GLM-4.5 | ✅ | 🔴 Not in registry | 131,072 | — | — |
| GLM-4.5-Air | ✅ | 🔴 Not in registry | 131,072 | — | — |

> 🟢 **Source**: [fireworks.ai/models](https://fireworks.ai/models) and
> [docs.fireworks.ai/fine-tuning/managed-finetuning-intro#supported-base-models](https://docs.fireworks.ai/fine-tuning/managed-finetuning-intro#supported-base-models)

### The Critical Constraint

**GLM 5.2 is the latest and most capable GLM model, but it is NOT fine-tunable on Fireworks.**
The model page at [fireworks.ai/models/fireworks/glm-5p2](https://fireworks.ai/models/fireworks/glm-5p2)
shows "Fine-tuning: Not supported".

**GLM 5.1 IS fine-tunable** on Fireworks with 200K context. This is the closest GLM model
that can be fine-tuned on the platform. It uses the same MoE architecture and pricing as 5.2.

---

## 2. GLM 5.2 Architecture

| Property | Value | Confidence |
|----------|-------|------------|
| Total parameters | 743B | 🟢 |
| Architecture | MoE (Mixture of Experts) | 🟢 |
| Key innovation | IndexShare (shared indexer across 4 sparse attention layers) | 🟢 |
| Context length | 1,048,576 tokens (1M) | 🟢 |
| License | MIT (fully open-weight) | 🟢 |
| Attention | Sparse attention with IndexShare, 2.9× FLOP reduction at 1M context | 🟢 |
| MTP (Multi-Token Prediction) | Yes — predicts multiple tokens per forward pass | 🟢 |
| Benchmark | SWE-bench Pro: 62.1, Terminal Bench 2.1: 81.0, AIME 2026: 99.2 | 🟢 |

> 🟢 **Source**: [huggingface.co/zai-org/GLM-5.2](https://huggingface.co/zai-org/GLM-5.2),
> arxiv:2603.12201 (IndexShare paper)

### GLM 5.2 vs GLM 5.1

GLM 5.2's main improvements over 5.1:
- **8x larger context** (1M vs 200K)
- **IndexShare architecture** for better long-context efficiency
- **Improved coding benchmarks** (SWE-bench, Terminal Bench)

For Sema fine-tuning, 200K context (GLM 5.1) is more than sufficient — no Sema program or
training example approaches even 10K tokens.

---

## 3. Open-Weight Status and External Fine-Tuning

GLM models are **open-weight under MIT license**. No regional restrictions, no technical
access barriers. The weights are downloadable from HuggingFace.

### External Fine-Tuning Tools

| Tool | GLM 5.2 Support | GLM 5.1 Support | Notes |
|------|-----------------|-----------------|-------|
| **Unsloth** | ✅ (v0.1.47-beta+) | ✅ | macOS support, 2x faster, 70% less VRAM |
| **LLaMA Factory** | ✅ | ✅ | LoRA fine-tuning; GLM-4.5 needs 16x H100 |
| **Swift (ms-swift)** | ✅ | ✅ | LoRA + full SFT; GLM-4.5 needs 16x H20 |
| **HuggingFace PEFT/TRL** | ✅ | ✅ | Standard LoRA training pipeline |
| **vLLM** | ✅ (v0.23.0+) | ✅ | Serving (not training), OpenAI-compatible API |

> 🟢 **Source**: [huggingface.co/zai-org/GLM-5.2](https://huggingface.co/zai-org/GLM-5.2),
> [github.com/unslothai/unsloth](https://github.com/unslothai/unsloth)

### Hardware Requirements for External Fine-Tuning

Based on GLM-4.5 (similar architecture, smaller):

| Model | LoRA | Full SFT |
|-------|------|----------|
| GLM-4.5 (355B) | 16x H100 | 128x H20 |
| GLM-4.5-Air (106B) | 4x H100 | 32x H20 |

GLM 5.2 (743B) and GLM 5.1 will require **more** than these estimates. For LoRA on GLM 5.2,
expect to need 16-32x H100 GPUs — this is not feasible for a small project.

**However**, the LoRA adapter can be fine-tuned on a smaller GLM variant and transferred, or
fine-tuned on GLM 5.1 (which is smaller) and the adapter uploaded to Fireworks.

---

## 4. Three Paths to a Sema-Aware GLM Model

### Path A: Fine-tune GLM 5.1 on Fireworks (RECOMMENDED)

```
GLM 5.1 (200K context, fine-tunable on Fireworks)
  → LoRA SFT or RFT with Sema training data
  → Deploy as dedicated endpoint on Fireworks
  → OpenAI-compatible API at api.fireworks.ai
```

**Pros**: No infrastructure to manage, integrated pipeline, one-click deploy
**Cons**: $7/hr ongoing hosting, GLM 5.1 (not 5.2), 200K context limit
**Cost**: $4.50 training + $7/hr hosting

### Path B: Fine-tune GLM 5.2 externally, upload LoRA to Fireworks

```
GLM 5.2 weights from HuggingFace
  → LoRA SFT with Unsloth or LLaMA Factory (needs 16-32x H100)
  → Export LoRA adapter
  → Upload to Fireworks via firectl model create --type LORA
  → Deploy on Fireworks (if GLM 5.2 has Supports Lora: true)
```

**Pros**: Uses the latest GLM 5.2, 1M context
**Cons**: Requires massive GPU resources for training, GLM 5.2 may not support LoRA serving
on Fireworks (needs verification), upload + deploy complexity
**Cost**: Very high training cost (32x H100 rental), $7+/hr hosting

### Path C: Fine-tune a smaller model, self-host

```
Qwen3-8B or Qwen3-4B (free RFT on Fireworks)
  → RFT with Sema VM grader (free for <16B)
  → Export model
  → Self-host with vLLM or Ollama
  → OpenAI-compatible API
```

**Pros**: Free training, no ongoing Fireworks cost, full control
**Cons**: Smaller model (less capable than GLM), self-hosting infrastructure
**Cost**: $0 training, GPU rental or local hardware for hosting

---

## 5. Recommendation

**For the best balance of capability, cost, and simplicity:**

1. **Start with Path A** (GLM 5.1 on Fireworks) — it's the most straightforward way to get a
   Sema-aware GLM model hosted and available via API.

2. **Also run Path C** (Qwen3-8B RFT, free) in parallel — this gives you a free fallback model
   and lets you iterate on the training data and grader without spending money.

3. **Consider Path B** (GLM 5.2 external) only if GLM 5.1's performance is insufficient AND
   you have access to significant GPU resources for external fine-tuning.

---

## 6. Non-GLM Alternatives Worth Considering

Given the GLM 5.2 fine-tuning constraint, these models are also worth evaluating:

| Model | Fine-Tunable? | RFT Free? | Context | Coding Bench |
|-------|---------------|-----------|---------|-------------|
| Qwen3-8B | ✅ | ✅ (free) | 256K | Strong |
| Qwen3-4B | ✅ | ✅ (free) | 64K | Good for size |
| Qwen3-32B | ✅ | No | 128K | Very strong |
| Qwen3-Coder-480B | Available for inference | No | 262K | Specialized for code |
| Kimi K2.6 | ✅ | No | 256K | Strong coding |
| Llama 3.3 70B | ✅ | No | 128K | General purpose |

**Qwen3-8B** is particularly attractive: it's free to RFT on Fireworks, has 256K context, and
has strong coding benchmarks. A Sema-aware Qwen3-8B could be a better practical choice than a
Sema-aware GLM 5.1, at zero training cost.
