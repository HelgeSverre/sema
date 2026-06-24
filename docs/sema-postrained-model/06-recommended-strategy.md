# 06 — Recommended Strategy

The recommended layered strategy for creating a Sema-aware hosted model, with concrete steps,
cost estimates, and a phased rollout plan.

---

## Executive Recommendation

**Don't choose one approach — layer them.** Each layer adds capability at increasing cost,
and earlier layers are prerequisites for later ones.

| Phase | Approach | Cost | Timeline | Result |
|-------|----------|------|----------|--------|
| **Phase 1** | System prompt + few-shot + tools | $0 | 1-2 days | GLM 5.2 serverless, ~70% accuracy |
| **Phase 2** | RFT on Qwen3-8B (free) | $0 | 1-2 weeks | Fine-tuned small model, ~85% accuracy |
| **Phase 3** | LoRA SFT on GLM 5.1 | ~$10 | 1-2 weeks | Fine-tuned GLM, ~90% accuracy |
| **Phase 4** | Add RAG layer | ~$5 | 3-5 days | API reference retrieval, ~92% accuracy |
| **Phase 5** | Combined: FT + RAG + Tools | $7/hr | Ongoing | Full assistant, ~95% accuracy |

---

## Phase 1: System Prompt + Few-Shot + Tools (Immediate, Free)

### What

Create a comprehensive system prompt with Sema language documentation and deploy it with
GLM 5.2 on Fireworks serverless (no fine-tuning needed). Add `sema_eval` as a tool for
self-correction.

### Steps

1. **Write the system prompt** (~2,000 tokens):
   - Language overview (from AGENTS.md)
   - Syntax reference (special forms, literals, reader syntax)
   - Stdlib function list (grouped by module)
   - Naming conventions
   - 5 curated example programs
   - Common error patterns and fixes

2. **Curate few-shot examples** (~1,000 tokens):
   - `hello.sema` (basics)
   - `fibonacci.sema` (named let, TCO)
   - `threading.sema` (->, ->>, some->)
   - `multimethods.sema` (defmulti/defmethod, match)
   - `async-pipeline.sema` (async, channels)

3. **Define `sema_eval` tool**:
   ```lisp
   (deftool sema-eval
     "Evaluate Sema code and return the result or error"
     {:code "The Sema code to evaluate"}
     (fn (args)
       (let ((code (get args :code)))
         (try {:result (eval (read-many code))}
           (catch e {:error (:message e)})))))
   ```

4. **Point Sema's LLM client at GLM 5.2**:
   - Model: `accounts/fireworks/models/glm-5p2`
   - Endpoint: `https://api.fireworks.ai/inference/v1/chat/completions`
   - System prompt + few-shot + tool definition in every request

### Cost

- **Training**: $0
- **Inference**: $1.40/M input + $4.40/M output (pay per token, zero when idle)
- **Estimated monthly**: $10-50 for light usage

### Expected Quality

~70% accuracy on simple code generation, ~80% with tool-augmented self-correction (model can
run code and fix errors on the fly).

### Why This First

- Zero cost, zero infrastructure
- Immediate — can be done in 1-2 days
- Establishes the baseline and system prompt that later phases build on
- The system prompt and few-shot examples will be reused in Phase 3 training data
- The `sema_eval` tool will be reused in Phase 2 RFT grader

---

## Phase 2: RFT on Qwen3-8B (Free, 1-2 Weeks)

### What

Use Fireworks Reinforcement Fine-Tuning (free for models <16B) to train Qwen3-8B with a
grader that executes Sema code through the VM.

### Steps

1. **Build the Sema evaluator endpoint**:
   - Option A: Subprocess — `sema -e "(eval (read-many CODE))"`
   - Option B: HTTP endpoint — wrap the Sema VM in a simple HTTP server
   - Option C: Rust FFI — compile a small binary that links against `sema-core` + `sema-vm`
   - This endpoint receives Sema code, executes it, and returns the result or error

2. **Prepare RFT problem set** (200-500 problems):
   - Extract from `eval_tests!` cases: each `$input => $expected` becomes a problem
   - Format: prompt = "Evaluate this Sema expression: $input", expected = $expected
   - Also include code generation problems: "Write a function that X" with expected output

3. **Write the grader function**:
   ```python
   def grader(problem: str, completion: str, expected: str) -> float:
       try:
           result = call_sema_eval(completion)
           if "error" in result:
               return 0.0  # Failed to execute
           if result["result"] == expected:
               return 1.0  # Correct
           return 0.3  # Ran but wrong output
       except Exception:
           return 0.0  # Timeout/crash
   ```

4. **Launch RFT job on Fireworks**:
   ```bash
   # Using eval-protocol CLI
   eval-protocol rft create \
     --base-model qwen3-8b \
     --problems sema-rft-problems.jsonl \
     --grader-url https://your-grader-endpoint.com/grade \
     --output-model sema-qwen8b-rft-v1
   ```

5. **Deploy the resulting model**:
   ```bash
   firectl deployment create "accounts/<ACCOUNT_ID>/models/sema-qwen8b-rft-v1"
   ```

### Cost

- **Training**: $0 (free for <16B models on Fireworks)
- **Grader hosting**: ~$5-10/month for a small VM running the Sema eval endpoint
- **Deployment**: $7/hr (H100 dedicated) when active, $0 when not deployed

### Expected Quality

~85% accuracy. RFT is particularly effective because Sema code is verifiable — the model
gets direct feedback on whether its code runs correctly.

### Limitations

- Qwen3-8B is a smaller model — less general coding ability than GLM 5.1/5.2
- 64K-256K context (depending on training shape)
- RL training can be unstable; may need iteration on grader design

---

## Phase 3: LoRA SFT on GLM 5.1 (~$10, 1-2 Weeks)

### What

Supervised fine-tuning on GLM 5.1 (the closest fine-tunable GLM model to 5.2) using the full
training dataset from [document 04](04-training-data-pipeline.md).

### Steps

1. **Generate the training dataset**:
   - Run the extraction scripts (document 04, section 6)
   - Produce ~4,000 JSONL training pairs
   - Split 90% train / 10% eval
   - Upload to Fireworks

2. **Launch SFT job**:
   ```bash
   firectl dataset create sema-training-v1 /path/to/sema-training.jsonl

   firectl sftj create \
     --base-model glm-5p1 \
     --dataset sema-training-v1 \
     --output-model sema-glm51-v1 \
     --epochs 2.0 \
     --lora-rank 16 \
     --max-context-length 32768
   ```

3. **Evaluate the model**:
   - Use the held-out 10% eval set
   - Also run a manual eval: 50 Sema programming tasks, scored by human + `sema_eval` tool

4. **Deploy**:
   ```bash
   firectl deployment create "accounts/<ACCOUNT_ID>/models/sema-glm51-v1"
   ```

5. **Integrate with Sema**:
   - Point the `sema-llm` OpenAI client at the deployment endpoint
   - Use the same system prompt from Phase 1
   - Include `sema_eval` tool from Phase 1

### Cost

- **Dataset preparation**: ~$3 (LLM-generated synthetic pairs via GLM 5.2 serverless)
- **Training**: 1.7M tokens × $3.00/M (GLM 5.1 is >16B) = ~$5.10
- **Deployment**: $7/hr (H100 dedicated) when active

### Expected Quality

~90% accuracy on simple tasks, ~80% on complex tasks. GLM 5.1 is a large MoE model with strong
coding ability, and the SFT teaches it Sema-specific syntax and patterns.

### Why GLM 5.1 and Not GLM 5.2

GLM 5.2 is **not fine-tunable on Fireworks**. The options are:
- Use GLM 5.1 (fine-tunable, 200K context, same family) — **recommended**
- Fine-tune GLM 5.2 externally with Unsloth (needs 16-32x H100 GPUs) — **prohibitively expensive**
- Use a non-GLM model (Qwen3-8B from Phase 2 is already trained for free) — **practical fallback**

GLM 5.1 and 5.2 share the same architecture family. GLM 5.1 has 200K context (vs 1M for 5.2),
but no Sema program approaches even 10K tokens, so the context difference is irrelevant for
this use case.

---

## Phase 4: Add RAG Layer (~$5, 3-5 Days)

### What

Add a retrieval-augmented generation layer that fetches relevant Sema documentation and
examples based on the user's query.

### Steps

1. **Chunk and embed all documentation**:
   - sema-docs API entries (826 files)
   - Website docs (72 files, 1,024 code blocks)
   - Example files (224 files)
   - AGENTS.md + syntax review

2. **Store in a vector database**:
   - Simple: in-memory with `hnswlib` or `qdrant`
   - Managed: Pinecone, Weaviate
   - Fireworks-native: use Qwen3 Embedding 8B for embeddings

3. **At inference time**:
   - Embed the user's query
   - Retrieve top-5 relevant chunks
   - Include in the prompt context alongside the system prompt

### Cost

- **Embedding**: ~1,200 chunks × ~200 tokens = 240K tokens × $0.10/M = ~$0.02
- **Vector store**: $0 (in-memory) to $20/month (managed)
- **Per-query retrieval**: ~200 tokens embedded + reranking = negligible

### Expected Quality

+2-5% improvement over Phase 3, primarily for API reference questions and less common
stdlib functions.

---

## Phase 5: Combined Assistant ($7/hr Ongoing)

### What

The full stack: fine-tuned GLM 5.1 + system prompt + RAG + tool-augmented self-correction.

### Architecture

```
User query
  → RAG: retrieve relevant docs/examples (top-5)
  → System prompt + retrieved context + few-shot examples
  → Fine-tuned GLM 5.1 generates Sema code
  → sema_eval tool: execute generated code
  → If error: model reads error, fixes code, retries
  → Return verified Sema code to user
```

### Integration with Sema

Sema's existing `sema-llm` crate already has:
- OpenAI-compatible client (works with Fireworks endpoint)
- Anthropic, Gemini, Ollama clients
- Provider registry
- Tool/function calling support (via `deftool`)

The integration point is minimal:

```rust
// In sema-llm, add a Fireworks provider or use the OpenAI client with a different base URL
let client = OpenAiClient::new(
    "https://api.fireworks.ai/inference/v1",
    &fireworks_api_key,
);
// Model: "accounts/<ACCOUNT_ID>/models/sema-glm51-v1"
```

Or for self-hosted:

```rust
let client = OllamaClient::new("http://localhost:11434/v1");
// Model: "sema-glm51-v1"
```

### Cost Summary (Monthly)

| Component | Cost |
|-----------|------|
| GLM 5.1 deployment (8hr/day × 22 days) | $1,232 |
| RAG vector store | $0-20 |
| Grader/eval endpoint | $5-10 |
| Inference tokens (if serverless fallback) | $10-50 |
| **Total** | **~$1,250-1,300/month** (dedicated) |
| **Or: self-hosted on cloud GPU** | **~$400-800/month** (A100 rental) |
| **Or: self-hosted locally** | **$0 (hardware cost only)** |

### Cost Optimization

- **Auto-scale**: Deploy only during working hours, tear down at night
- **Multi-LoRA**: Serve both GLM 5.1 Sema and a general-purpose LoRA on one deployment
- **Self-host**: Use vLLM on a rented A100 ($1-2/hr) instead of Fireworks dedicated ($7/hr)
- **Hybrid**: Use serverless GLM 5.2 with system prompt for low-priority queries, route to
  fine-tuned GLM 5.1 for complex Sema tasks

---

## Fallback: If GLM 5.1 Performance Is Insufficient

If the fine-tuned GLM 5.1 doesn't meet quality requirements:

1. **Try Qwen3-32B or Qwen3-235B** (fine-tunable on Fireworks, strong coding benchmarks)
2. **Try Kimi K2.6** (fine-tunable, 256K context, strong coding)
3. **Fine-tune GLM 5.2 externally** (Unsloth, if GPU budget allows)
4. **Combine multiple models**: Use the fine-tuned Qwen3-8B (from Phase 2, free) for simple
   tasks and GLM 5.2 serverless (with system prompt) for complex reasoning

---

## Summary: What to Do This Week

1. **Day 1-2**: Write the system prompt, curate 5 few-shot examples, define `sema_eval` tool,
   point Sema's LLM client at GLM 5.2 serverless. Test it. (Phase 1)
2. **Day 3-5**: Build the Sema eval endpoint for the RFT grader. Extract eval_tests! cases
   as RFT problems. (Phase 2 prep)
3. **Week 2**: Launch RFT job on Qwen3-8B (free). While it trains, start preparing the SFT
   dataset. (Phase 2 + Phase 3 prep)
4. **Week 3**: Launch SFT job on GLM 5.1 (~$5). Deploy and evaluate. (Phase 3)
5. **Week 4**: Add RAG layer if needed. Polish and integrate. (Phase 4)
