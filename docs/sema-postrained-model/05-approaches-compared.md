# 05 — Approaches Compared

Comprehensive comparison of approaches to make a model Sema-aware, ranked by cost/quality
ratio. Assessed on 2026-06-24.

---

## 1. Approach Matrix

| Approach | Training Cost | Quality | Setup Effort | Maintenance | Best For |
|----------|--------------|---------|-------------|-------------|----------|
| **System prompt** | $0 | 🟡 Good with strong models | Low | Update prompt | Immediate, any model |
| **Few-shot prompting** | $0 | 🟡 Good for patterns | Low | Curate examples | Quick wins |
| **RFT (Reinforcement FT)** | **$0** (free <16B) | 🟢 Highest for verifiable | High (grader) | Maintain grader | Sema-specific tasks |
| **LoRA SFT** | $1-10 | 🟢 High for domain syntax | Medium | Retrain on changes | Production assistant |
| **Full-param SFT** | $2-20 | 🟢 Very high | Medium | Retrain on changes | Deep domain adaptation |
| **RAG** | Low (embedding) | 🟡 Good for factual, weaker syntax | Medium | Update index | API reference lookup |
| **Tool-augmented** | Low (tool infra) | 🟢 High (self-correcting) | Medium | Maintain tools | Verified code gen |
| **Fine-tune + system prompt** | $1-10 | 🟢 Very high | Medium | Both | Best practical result |
| **Fine-tune + RAG** | $1-10 + embedding | 🟢 Highest overall | High | Both | Comprehensive assistant |

---

## 2. Detailed Analysis

### 2.1 System Prompt (Tier 0 — Free, Immediate)

Include comprehensive Sema language documentation in the system prompt.

**What to include**:
- Syntax reference (special forms, literals, reader syntax)
- Stdlib function list with signatures
- Naming conventions (slash-namespaced, predicates end `?`, arrows for conversions)
- 5-10 representative code examples
- Common patterns (threading macros, pattern matching, async channels)
- Error hint table (veteran_hint equivalents)

**Strengths**:
- Zero cost, zero infrastructure
- Works with any model (GLM 5.2 serverless, Claude, GPT, etc.)
- Immediately updatable when Sema changes
- No training data preparation needed

**Weaknesses**:
- Consumes context window (~2-4K tokens for comprehensive docs)
- Model may still generate invalid syntax (doesn't deeply "know" the grammar)
- No weight updates — model relies on in-context learning only
- Quality depends on the base model's coding ability

**Implementation**: A ~2,000 token system prompt derived from AGENTS.md + the syntax review
document + 5 curated examples. Point Sema's existing `sema-llm` crate at GLM 5.2 serverless
with this system prompt.

**Estimated quality**: A strong model like GLM 5.2 with a good system prompt can likely
generate syntactically correct Sema for ~60-70% of simple tasks. Error recovery and complex
programs will be weaker.

> 🟢 **Confidence**: Well-established technique. Quality estimate based on general LLM
> performance with domain-specific system prompts.

### 2.2 Few-Shot Prompting (Tier 0 — Free, Immediate)

Include 3-10 curated Sema code examples in the prompt alongside the system prompt.

**What to include**:
- 1-2 simple examples (hello world, fibonacci, map/filter)
- 2-3 intermediate examples (pattern matching, async channels, macros)
- 1-2 advanced examples (LLM tools, web server, data pipeline)
- 1 error handling example (try/catch)

**Strengths**:
- Zero cost, zero infrastructure
- Demonstrates syntax patterns directly in context
- Complements system prompt (docs + examples)
- No training needed

**Weaknesses**:
- Uses context window (~1-2K additional tokens)
- Examples may not cover all patterns the user needs
- Quality limited by the base model's pattern-matching ability

**Estimated quality**: System prompt + few-shot together: ~70-80% accuracy on simple tasks.

> 🟢 **Confidence**: Standard in-context learning technique.

### 2.3 RFT — Reinforcement Fine-Tuning (Tier 1 — Free for <16B)

Use Fireworks RFT with a grader function that executes Sema code through the VM.

**How it works**:
1. Provide ~200 Sema problems (from eval_tests! cases) as the training set
2. The model generates Sema code for each problem
3. The grader function:
   - Parses the generated code (syntax check)
   - Executes it through the Sema VM
   - Compares output to expected result
   - Returns a reward score (1.0 = correct, 0.3 = ran but wrong, 0.0 = error)
4. The model is trained with RL to maximize the reward

**Strengths**:
- **Free for models under 16B** (Qwen3-4B, Qwen3-8B)
- Sema code is inherently verifiable (you can run it)
- Teaches the model to generate *correct* code, not just plausible-looking code
- The grader can be made arbitrarily sophisticated (type checking, performance, style)
- Fireworks handles all RL infrastructure

**Weaknesses**:
- Requires building a grader function (Rust FFI or subprocess to Sema VM)
- Limited to models under 16B for free tier
- RL training can be unstable; may need hyperparameter tuning
- Grader needs to handle timeouts, infinite loops, stack overflow
- Remote agent mode requires hosting a grader endpoint

**Implementation**:
1. Build a Sema evaluator HTTP endpoint (or use subprocess `sema -e "..."`)
2. Package 200-500 eval_tests! cases as RFT problems
3. Launch RFT job on Fireworks with Qwen3-8B
4. Deploy the resulting model

**Estimated quality**: ~85-90% accuracy on tasks similar to training set. RFT is particularly
effective for verifiable tasks because the model gets direct feedback on correctness.

> 🟢 **Confidence**: Fireworks documents RFT extensively. Sema's verifiability makes it an
> ideal fit. The main uncertainty is grader implementation complexity.

### 2.4 LoRA SFT (Tier 2 — $1-10)

Supervised fine-tuning with instruction-tuning pairs on Fireworks.

**How it works**:
1. Prepare ~3,000-4,000 JSONL training pairs (see [document 04](04-training-data-pipeline.md))
2. Upload dataset to Fireworks
3. Launch SFT job with LoRA rank 16, 2-3 epochs
4. Deploy as dedicated endpoint

**Strengths**:
- Low cost ($0.75 for Qwen3-8B, $4.50 for GLM 5.1)
- Teaches syntax patterns, stdlib API, idioms
- Can include diverse training examples (code gen, explanation, debugging, translation)
- LoRA adapter is small and fast to train
- Supports function calling format (for tool-augmented generation)

**Weaknesses**:
- Requires dedicated deployment ($7/hr H100) for hosting
- Needs retraining when Sema changes significantly
- Quality depends on training data quality and diversity
- No correctness verification (unlike RFT)

**Implementation**:
```bash
# Upload dataset
firectl dataset create sema-training-v1 /path/to/sema-training.jsonl

# Launch SFT on GLM 5.1
firectl sftj create \
  --base-model glm-5p1 \
  --dataset sema-training-v1 \
  --output-model sema-glm51-v1 \
  --epochs 2.0 \
  --lora-rank 16

# Deploy
firectl deployment create "accounts/<ACCOUNT_ID>/models/sema-glm51-v1"
```

**Estimated quality**: ~80-85% accuracy. SFT is better at teaching syntax patterns and API
knowledge than RFT, but doesn't verify correctness.

> 🟢 **Confidence**: Well-documented Fireworks feature. Training cost estimates are
> calculation-based from pricing page.

### 2.5 RAG — Retrieval-Augmented Generation (Tier 2 — Low Cost)

Embed Sema documentation, examples, and stdlib function signatures into a vector store.
At inference time, retrieve relevant context based on the user's query.

**How it works**:
1. Chunk all Sema docs, examples, and API entries into ~500-token segments
2. Embed using Fireworks Qwen3 Embedding 8B ($0.10/M tokens)
3. Store in a vector database (Pinecone, Weaviate, or in-memory)
4. At query time, retrieve top-K relevant chunks
5. Include retrieved chunks in the prompt context

**Strengths**:
- Always current — update the index when Sema changes, no retraining
- Good for API reference questions ("How does string/trim work?")
- Can retrieve specific examples relevant to the user's question
- Works with any base model

**Weaknesses**:
- Adds latency (~100-200ms for retrieval)
- Doesn't teach syntax patterns as deeply as fine-tuning
- May retrieve irrelevant context
- Requires maintaining a vector store and embedding pipeline
- Not as good for code generation as fine-tuning

**For Sema**: Embed the website docs, sema-docs entries, all example files, and AGENTS.md.
Use Qwen3 Reranker for better retrieval precision.

**Estimated quality**: ~70-75% for factual questions, ~50-60% for code generation (weaker
than fine-tuning for syntax-heavy tasks).

> 🟡 **Confidence**: RAG is well-established but quality for code generation specifically is
> less certain. Depends heavily on chunking strategy and retrieval quality.

### 2.6 Tool-Augmented Generation (Tier 1 — Low Cost)

Define `sema_eval` and `sema_format` as tools the model can call to verify its own output.

**How it works**:
1. Define tools in the system prompt:
   - `sema_eval(code: str)`: Execute Sema code, return result or error
   - `sema_format(code: str)`: Format Sema code
   - `sema_parse(code: str)`: Check if code parses without error
2. The model generates code, calls `sema_eval` to test it
3. If there's an error, the model reads the error message and fixes the code
4. Iterate until the code runs correctly

**Strengths**:
- Self-correcting — the model can verify its own output
- Uses Sema's own VM for ground-truth correctness
- Works with any model that supports function calling (GLM 5.2, Claude, etc.)
- No training needed
- Leverages Sema's existing error messages and hints

**Weaknesses**:
- Adds round-trips (each tool call is an API round-trip)
- Requires hosting a Sema eval endpoint (or running locally)
- Model must be good at tool use (stronger models are better at this)
- May get stuck in loops if it can't fix the error

**For Sema**: This is already partially implemented in the `sema-llm` crate and MCP support.
The `deftool` form and agent infrastructure exist. A `sema_eval` tool can be defined as a
Sema function and exposed to the model.

**Estimated quality**: ~85-90% for simple tasks (model can iterate), ~70% for complex tasks
(may get stuck).

> 🟢 **Confidence**: Tool-augmented generation is well-proven. Sema's existing tool/agent
> infrastructure makes this straightforward to implement.

### 2.7 Combined: Fine-Tune + System Prompt (Tier 2 — Best Practical)

Fine-tune a model on Sema data AND use a comprehensive system prompt at inference time.

**Why combine**: Fine-tuning teaches syntax patterns in the weights, while the system prompt
provides up-to-date reference information and examples. They complement each other:
- Fine-tuning: "I know how Sema syntax works" (in the weights)
- System prompt: "Here's the latest stdlib API and 5 examples" (in context)

**Estimated quality**: ~90% on simple tasks, ~80% on complex tasks. This is the best
cost/quality ratio for a production assistant.

> 🟢 **Confidence**: Combined approach is standard practice. Both components are
> well-understood.

### 2.8 Combined: Fine-Tune + RAG + Tool-Augmented (Tier 3 — Highest Quality)

The full stack: fine-tuned model + RAG for API reference + tool-augmented self-correction.

**Estimated quality**: ~95% on simple tasks, ~85% on complex tasks. The highest quality
achievable without a custom-trained-from-scratch model.

**Cost**: Fine-tuning ($1-10) + embedding ($0.50) + tool endpoint hosting ($5-10/month) +
dedicated model deployment ($7/hr when active).

> 🟡 **Confidence**: Each component is well-proven, but the combined system has more failure
> modes and integration complexity.

---

## 3. Decision Matrix

| Criterion | System Prompt | RFT | LoRA SFT | RAG | Tool-Aug | FT + Prompt |
|-----------|:---:|:---:|:---:|:---:|:---:|:---:|
| **Cost** | $0 | $0 | $1-10 | $1-5 | $0 | $1-10 |
| **Setup effort** | Low | High | Medium | Medium | Medium | Medium |
| **Syntax accuracy** | 70% | 90% | 85% | 60% | 85% | 90% |
| **API knowledge** | 80% | 70% | 90% | 95% | 80% | 95% |
| **Code generation** | 70% | 90% | 85% | 55% | 85% | 90% |
| **Error recovery** | 50% | 85% | 70% | 40% | 90% | 80% |
| **Updatability** | Instant | Retrain | Retrain | Update index | Instant | Partial |
| **Hosting cost** | $0 (serverless) | $7/hr | $7/hr | $0+index | $0+tools | $7/hr |
| **Best model** | GLM 5.2 | Qwen3-8B | GLM 5.1 | Any | GLM 5.2 | GLM 5.1 |

> Percentages are estimates based on general LLM performance patterns. Actual results will
> vary based on training data quality, prompt engineering, and model capabilities.

---

## 4. Why Not Full Fine-Tuning from Scratch?

Training a model from scratch on Sema code is not feasible because:

1. **Data volume**: ~58,000 lines of Sema code is far too little for pretraining (models
   need billions of tokens)
2. **Cost**: Pretraining even a small model (1B params) costs $10,000+
3. **Capability**: A model pretrained only on Sema would lack general coding ability and
   reasoning skills
4. **No benefit**: Post-training (SFT/RFT) on an existing strong model gives better results
   at a fraction of the cost

The correct approach is always to **post-train an existing strong model** on Sema-specific
data, not to train from scratch.
