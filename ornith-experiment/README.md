# Ornith SFT Experiment

Fine-tune Ornith-1.0-9B (and potentially 31B/35B) to write Sema code, using SFT on code-generation pairs + tool-repair traces — not RFT on eval-answer pairs.

## Why not the previous RFT approach

The `experiment/rft-qwen3-sema` branch trained Qwen3-8B with RFT on `Evaluate this Sema expression → expected value` pairs. Results:

| Model | Overall | L1 | L2 | L3 | L4 | L5 |
|-------|:-:|:-:|:-:|:-:|:-:|:-:|
| GLM 5.2 (no FT) | 49% | 58% | 61% | 65% | 13% | 12% |
| Qwen3-8B (RFT) | 36% | 58% | 37% | 23% | 26% | 26% |
| Qwen3-8B (RFT + tools) | 24% | 81% | 9% | 4% | 0% | 6% |
| Kimi K2.6 (tools) | 60% | 35% | 75% | 71% | 59% | 66% |

The RFT model got better at L1 eval tasks (81% with tools) but collapsed on everything else (9% L2, 4% L3, 0% L4). It learned to evaluate expressions, not write programs. It also lost tool-loop behavior — only 1.4 tool calls per task, and it wouldn't recover from errors.

## Why Ornith

Ornith-1.0 is an open-source model family (MIT licensed) specifically tuned for agentic coding:
- 9B Dense, 31B Dense, 35B MoE, 397B MoE variants
- Post-trained on Qwen 3.5 / Gemma 4 bases
- Trained on Terminal-Bench, SWE-Bench, NL2Repo, OpenClaw
- Native tool-call parsing (OpenAI-compatible), reasoning model
- 9B scores: 43.1 Terminal-Bench 2.1, 69.4 SWE-bench Verified

The bet: Ornith already knows how to be a coding agent. Teach it just enough Sema syntax that the first draft is less wrong, then let tools fix the rest.

## Three phases

### Phase 1: Benchmark raw Ornith (no training)

Run the existing 60-task benchmark against bare Ornith before any fine-tuning:

```bash
# Serve Ornith locally with vLLM (requires GPU)
vllm serve deepreinforce-ai/Ornith-1.0-9B \
  --served-model-name Ornith-1.0-9B \
  --host 0.0.0.0 --port 8000 \
  --enable-auto-tool-choice --tool-call-parser qwen3_xml \
  --reasoning-parser qwen3 \
  --trust-remote-code

# Build sema binary
cargo build

# Run benchmarks (from the worktree root)
cd ornith-experiment

# Ornith bare (no tools)
OPENAI_BASE_URL=http://localhost:8000/v1 OPENAI_API_KEY=EMPTY \
  python3 benchmark.py --model Ornith-1.0-9B --model-name "Ornith-9B bare"

# Ornith + tools
OPENAI_BASE_URL=http://localhost:8000/v1 OPENAI_API_KEY=EMPTY \
  python3 benchmark.py --model Ornith-1.0-9B --model-name "Ornith-9B + tools" \
  --tools --reasoning --temperature 0.6 --top-p 0.95

# Compare against frontier models (Fireworks)
OPENAI_BASE_URL=https://api.fireworks.ai/inference/v1 \
  python3 benchmark.py --model accounts/fireworks/models/glm-5p2 \
  --model-name "GLM 5.2 + tools" --tools
```

**Decision gate**: If raw Ornith + tools doesn't beat Qwen3-8B RFT + tools (24%), try 31B/35B before training. If it gets near 50-60%, proceed to Phase 2.

### Phase 2: SFT on code-generation pairs

Generate the SFT dataset:

```bash
cd ornith-experiment

# Without LLM descriptions (template-based, no API cost)
python3 generate_sft_dataset.py --output data/sft/

# With LLM descriptions (better quality, uses API)
python3 generate_sft_dataset.py --output data/sft/ --use-llm \
  --base-url https://api.fireworks.ai/inference/v1 \
  --api-key $FIREWORKS_API_KEY
```

Dataset format (chat-format, NOT prompt/completion):

```json
{"messages": [
  {"role": "system", "content": "You write valid Sema code. Output code only unless asked to explain. Do not use Markdown fences or backticks."},
  {"role": "user", "content": "Write a Sema function that doubles every number in a list."},
  {"role": "assistant", "content": "(define (double-all xs)\n  (map (fn (x) (* x 2)) xs))"}
]}
```

Key differences from the old dataset:
- System prompt says "Output code only, no Markdown fences" (old one said "Evaluate expressions")
- User prompt is a natural language request (old one was "Evaluate this expression: X")
- Assistant response is Sema code (old one was just the evaluated value)
- No backticks, no fenced code blocks (old one accidentally trained markdown habits)

### Phase 3: Add tool-repair traces

Generate traces where a frontier model solves tasks using eval_code, makes errors, and fixes them:

```bash
cd ornith-experiment

python3 generate_repair_traces.py \
  --base-url https://api.fireworks.ai/inference/v1 \
  --api-key $FIREWORKS_API_KEY \
  --llm-model accounts/fireworks/models/glm-5p2 \
  --output data/sft/repair_traces.jsonl \
  --max-traces 100
```

Each trace captures:
```
system: "You are a Sema coding agent. Use eval_code to test code..."
user: "Write a function that returns the first n items of a list."
assistant: [tool_call: eval_code("(define (take n xs) ...))")]
tool: "Error: Unbound variable: car"
assistant: [tool_call: eval_code("(define (take n xs) (if (= n 0) '() (cons (first xs) ...))))]
tool: "Result: (1 2 3)"
assistant: "(define (take n xs)\n  (if (= n 0)\n      '()\n      (cons (first xs) (take (- n 1) (rest xs)))))"
```

## Dataset mix

Target composition (per the analysis):

| Category | Percentage | Source |
|----------|:-:|--------|
| NL -> Sema code | 40% | Example files, eval tests, LLM-described |
| Tool-repair traces | 25% | Frontier model + eval_code loop |
| Docs/API examples | 20% | 829 doc entries with code blocks |
| Fuzzer-generated | 10% | grammar-fuzz.sema output |
| Negative corrections | 5% | Clojure/Scheme anti-patterns |

The negative examples matter because models drift into Clojure:
- `(let [x 1] ...)` -> `(let ((x 1)) ...)` (Sema uses parenthesized binding lists)
- `(defn name [args] ...)` -> `(define (name args) ...)` (Sema uses define)
- `(case x ...)` -> `(match x ...)` (Sema uses match, not case)

## Training

```bash
# 9B QLoRA (pipeline validation)
# Use Unsloth or PEFT with LoRA rank 16-32, early stopping
# Small LR (1e-4 to 2e-4), 2-3 epochs max

# 31B/35B LoRA (serious attempt, if Phase 1 looks promising)
# Rent GPU, LoRA rank 32-64
```

**Critical**: Do NOT SFT so hard that you overwrite Ornith's agentic behavior. Use a small LoRA, early stopping, and lots of tool-use traces. Think of the fine-tune as a "Sema accent pack", not a brain transplant.

## Success criteria

Judge only by held-out coding tasks, not by training loss (the previous RFT run had reward going from 76.9% to 87.9% while coding performance got worse).

Acceptance bar:
```
Ornith-9B SFT + tools > raw Ornith-9B + tools by 10+ points
and
Ornith-9B SFT + tools does not lose iterative repair behavior
```

Metrics to track:
- Parse rate (does the output parse as valid Sema?)
- Stdlib hallucination rate (does it invent functions that don't exist?)
- Repair-after-error rate (after an eval_code error, does it fix and retry?)
- Final benchmark pass rate (the 60-task score)

## Files

| File | Purpose |
|------|---------|
| `benchmark.py` | Run 60-task benchmark against any OpenAI-compatible endpoint |
| `generate_sft_dataset.py` | Generate SFT dataset from examples, docs, eval tests |
| `generate_repair_traces.py` | Generate tool-repair traces using a frontier model |
| `grader.py` | Grade Sema code completions (standalone or HTTP server) |
| `system_prompt.txt` | System prompt (no markdown fences, code-only output) |
| `sema-tools.sema` | MCP tools: eval_code + docs_search |
| `data/benchmark_tasks.jsonl` | 60 held-out benchmark tasks (L1-L5) |

## Prerequisites

```bash
# Build sema binary
cargo build

# Index docs for docs_search (one-time)
sema mcp ornith-experiment/sema-tools.sema  # or run the indexing separately

# Install Python deps
cd ornith-experiment && pip install httpx
```
