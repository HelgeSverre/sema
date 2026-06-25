# 01 — Fireworks.ai Platform Capabilities

Assessed on 2026-06-24 against live Fireworks documentation and model library.

---

## 1. Fine-Tuning Methods

Fireworks offers four managed fine-tuning methods. All use the same set of supported base
models — once a base model is supported, every method works against it.

| Method | Description | Pricing | Best For |
|--------|-------------|---------|----------|
| **SFT (Supervised Fine-Tuning)** | Train with labeled input→output pairs | $/M training tokens | Domain adaptation, code generation |
| **RFT (Reinforcement Fine-Tuning)** | Train with a grader/reward function | **Free for <16B models** | Verifiable tasks (code execution) |
| **DPO (Direct Preference Optimization)** | Train with preference pairs | $/M training tokens | Alignment, style preference |
| **VLM SFT** | Vision-language fine-tuning | $/M training tokens | Image + text |

> 🟢 **Source**: [docs.fireworks.ai/fine-tuning/managed-finetuning-intro](https://docs.fireworks.ai/fine-tuning/managed-finetuning-intro)

### RFT — Key Details

RFT is the most compelling option for Sema because Sema code is **verifiable**: generated code
can be executed through the Sema VM and its output checked against expected results.

- **Free for models under 16B parameters** (e.g. Qwen3-4B, Qwen3-8B)
- Two modes: **single-turn** (rollouts happen on Fireworks' infrastructure) and **remote agents**
  (rollouts happen in your environment via HTTP)
- Built-in tracing, monitoring, and one-click deploy
- The grader function receives model output and returns a score; for Sema, this would:
  1. Parse the generated Sema code
  2. Execute it through the Sema VM
  3. Compare output to expected result
  4. Return a reward signal

> 🟢 **Source**: [docs.fireworks.ai/fine-tuning/reinforcement-fine-tuning-models](https://docs.fireworks.ai/fine-tuning/reinforcement-fine-tuning-models)

---

## 2. Supported Base Models (Fine-Tunable)

The table below is the live registry of fine-tunable models as of 2026-06-24.

| Base model | Max context | Tunable? |
|------------|-------------|----------|
| `gemma-4-26b-a4b-it` | 256K | ✅ |
| `gemma-4-31b-it` | 256K | ✅ |
| **`glm-5p1`** | **200K** | **✅** |
| `kimi-k2p5` | 256K | ✅ |
| `kimi-k2p6` | 256K | ✅ |
| `llama-v3p3-70b-instruct` | 128K | ✅ |
| `minimax-m2p5` | 192K | ✅ |
| `nemotron-nano-3-30b-a3b` | 256K | ✅ |
| `qwen3-235b-a22b-instruct-2507` | 128K | ✅ |
| `qwen3-30b-a3b` | 128K | ✅ |
| `qwen3-30b-a3b-instruct-2507` | 128K | ✅ |
| `qwen3-32b` | 128K | ✅ |
| **`qwen3-4b`** | **64K** | **✅ (free RFT)** |
| **`qwen3-8b`** | **256K** | **✅ (free RFT)** |
| `qwen3-vl-8b-instruct` | 256K | ✅ |
| `qwen3p5-27b` | 256K | ✅ |
| `qwen3p5-35b-a3b` | 256K | ✅ |
| `qwen3p5-397b-a17b` | 256K | ✅ |
| `qwen3p5-9b` | 256K | ✅ |
| `qwen3p6-27b` | 256K | ✅ |

> 🟢 **Source**: [docs.fireworks.ai/fine-tuning/managed-finetuning-intro#supported-base-models](https://docs.fireworks.ai/fine-tuning/managed-finetuning-intro#supported-base-models)

### Critical: GLM 5.2 is NOT in the fine-tunable list

**GLM 5.2** (`glm-5p2`) is available for **serverless inference** ($1.40/M input, $4.40/M output)
but is **NOT fine-tunable** on Fireworks. The model page shows "Fine-tuning: Not supported".

**GLM 5.1** (`glm-5p1`) IS fine-tunable with 200K context. This is the closest GLM model that
can be fine-tuned on the platform.

**Workaround for GLM 5.2**: Fine-tune off-Fireworks using Unsloth (supports GLM-5.2 as of
v0.1.47-beta) or LLaMA Factory, then upload the LoRA adapter to Fireworks for serving. See
[document 02](02-glm-model-analysis.md) for details.

---

## 3. Tuning Modes

### LoRA (Low-Rank Adaptation)

- Default and recommended approach
- Trains a small adapter (0.1-2% of parameters)
- LoRA rank must be a power of 2, up to 32 (default: 8)
- Supports **multi-LoRA deployment** (multiple adapters on one base model deployment)
- Supports **live merge** (adapter merged into base at deploy time, zero inference overhead)

### Full-Parameter Tuning

- Updates all model weights
- 2x the cost of LoRA
- Recommended for "difficult reasoning, alignment, or domain adaptation tasks"

> 🟢 **Source**: [docs.fireworks.ai/fine-tuning/fine-tuning-models](https://docs.fireworks.ai/fine-tuning/fine-tuning-models)

---

## 4. Pricing

### Training Pricing

| Model size | LoRA SFT ($/M tokens) | Full-param SFT ($/M tokens) | RFT |
|-----------|----------------------|----------------------------|-----|
| Up to 16B | $0.50 | $1.00 | **Free** |
| 16.1B-80B | $3.00 | $6.00 | Billed |
| 80B-300B | $6.00 | $12.00 | Billed |
| >300B | $10.00 | $20.00 | Billed |

**Cost estimate for Sema** (1,000 examples × ~500 tokens × 3 epochs = 1.5M tokens):
- Qwen3-8B (LoRA SFT): 1.5M × $0.50 = **$0.75**
- GLM 5.1 (LoRA SFT, 200K context): 1.5M × $3.00 = **$4.50** (GLM 5.1 is >16B)
- Qwen3-8B (RFT): **Free**

### Inference Pricing (Serverless)

| Model | Input $/M | Cached $/M | Output $/M |
|-------|-----------|------------|------------|
| GLM 5.2 | $1.40 | $0.26 | $4.40 |
| GLM 5.1 | $1.40 | $0.26 | $4.40 |
| Qwen3-8B | ~$0.20 | — | ~$0.20 |
| Qwen3-4B | ~$0.20 | — | ~$0.20 |

### Dedicated Deployment Pricing (required for fine-tuned models)

Fine-tuned LoRA models **cannot** be deployed to serverless. They require an on-demand
(dedicated) deployment:

| GPU | $/hour |
|-----|--------|
| H100 | $7.00 |
| H200 | $7.00 |
| B200 | $10.00 |
| B300 | $12.00 |

> 🟢 **Source**: [fireworks.ai/pricing](https://fireworks.ai/pricing)

---

## 5. Dataset Format

Fireworks uses the **OpenAI-compatible chat completion format** (JSONL). Each line is a JSON
object with a `messages` array:

```jsonl
{"messages": [{"role": "system", "content": "You are a Sema programming assistant."}, {"role": "user", "content": "Write a function that filters even numbers from a list."}, {"role": "assistant", "content": "(define (filter-even coll) (filter even? coll))"}]}
```

### Key Constraints

- **Minimum**: 3 examples per dataset
- **Maximum**: 3 million examples per dataset
- **Format**: `.jsonl` (one JSON object per line)
- **Roles**: `system` (optional, must be first), `user`, `assistant`
- **Content**: plain string or OpenAI-style content parts `[{type: "text", text: "..."}]`
- **Weight**: optional per-message (0 = skip in loss) or per-sample (loss multiplier)
- **Function calling**: supported via `tools` array + `tool_calls` in assistant messages
- **Thinking traces**: supported for reasoning models (`reasoning_content` field)

### Upload Methods

```bash
# CLI
firectl dataset create <DATASET_ID> /path/to/dataset.jsonl

# API
POST /v1/datasets  (create entry)
POST /v1/datasets/{id}:upload  (upload file)

# UI
Navigate to Datasets tab → Create Dataset
```

> 🟢 **Source**: [docs.fireworks.ai/fine-tuning/fine-tuning-models](https://docs.fireworks.ai/fine-tuning/fine-tuning-models)

---

## 6. Launching a Fine-Tuning Job

```bash
# SFT
firectl sftj create \
  --base-model glm-5p1 \
  --dataset sema-training-set \
  --output-model sema-glm51-v1 \
  --epochs 2.0 \
  --lora-rank 16

# Monitor
firectl sftj get sema-glm51-v1

# Deploy (creates dedicated deployment)
firectl deployment create "accounts/<ACCOUNT_ID>/models/sema-glm51-v1"
```

### Key Hyperparameters

| Parameter | Default | Notes |
|-----------|---------|-------|
| `--epochs` | 1.0 | Increase to 2-3 for small datasets |
| `--lora-rank` | 8 | Power of 2, max 32. Increase for more capacity |
| `--learning-rate` | Auto | Generally don't change |
| `--max-context-length` | 32768+ | Cut-off for training examples |
| `--batch-size` | Auto | Tokens per forward step |
| `--evaluation-dataset` | Auto carve-out | Separate eval set |

> 🟢 **Source**: [docs.fireworks.ai/fine-tuning/fine-tuning-models](https://docs.fireworks.ai/fine-tuning/fine-tuning-models)

---

## 7. Deployment Options

### Live Merge (recommended for single fine-tune)

- LoRA weights merged into base model at deploy time
- Zero inference overhead — matches base model performance
- One LoRA per deployment
- Simple: `firectl deployment create <model_id>`

### Multi-LoRA

- Multiple LoRA adapters on a single base model deployment
- Dynamic adapter loading at request time
- ~10-30% TTFT overhead, lower max throughput
- Route requests via `model` field: `<model_name>#<deployment_name>`
- **Only works with BF16 shapes** (not FP8/FP4)

### Important: Serverless NOT Supported for Fine-Tuned Models

> "Fine-tuned LoRA models, whether created on the Fireworks platform or imported, can only be
> deployed to on-demand (dedicated) deployments. Serverless deployment is not supported for
> LoRA models." — Fireworks docs

This means a **minimum ongoing cost of $7/hr** (H100) to host a fine-tuned model on Fireworks.

> 🟢 **Source**: [docs.fireworks.ai/fine-tuning/deploying-loras](https://docs.fireworks.ai/fine-tuning/deploying-loras)

---

## 8. Importing Externally Fine-Tuned LoRAs

If a model isn't fine-tunable on Fireworks (like GLM 5.2), you can:

1. Fine-tune it externally (Unsloth, LLaMA Factory, HuggingFace TRL)
2. Export the LoRA adapter
3. Upload to Fireworks via `firectl model create` with `--type LORA`
4. Deploy as usual

The base model must be available on Fireworks and must have `Supports Lora: true` (even if
`Tunable: false`).

> 🟢 **Source**: [docs.fireworks.ai/models/uploading-custom-models#importing-fine-tuned-models](https://docs.fireworks.ai/models/uploading-custom-models#importing-fine-tuned-models)

---

## 9. Data Privacy

> "Your data is your data. No prompt or generated data is logged or stored on Fireworks; only
> meta-data like the number of tokens in a request is logged, as required to deliver the
> service." — Fireworks docs

Training data uploaded for fine-tuning is used only for your job. No other users see it.

> 🟢 **Source**: [docs.fireworks.ai/models/overview](https://docs.fireworks.ai/models/overview)
