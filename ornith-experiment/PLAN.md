# Ornith SFT Experiment — Plan

## Goal

Teach Ornith-1.0-9B to write Sema code via SFT (supervised fine-tuning), not RFT.

The previous RFT experiment on Qwen3-8B trained the wrong behavior (evaluating expressions instead of writing code) and destroyed tool-loop/repair behavior. This experiment fixes both mistakes.

## Worktree

- **Branch**: `experiment/ornith-sft`
- **Worktree**: `/Users/helge/code/sema-ornith`
- **Experiment dir**: `ornith-experiment/`
- **Do not disturb**: `experiment/rft-qwen3-sema` worktree at `/Users/helge/code/sema-rft-experiment`

## What happened so far

### Phase 1: Bare benchmark (DONE)

Ran Ornith-1.0-9B (Q4_K_M via Ollama, 16k context) against the 60-task Sema benchmark:

| Model | Overall | L1 | L2 | L3 | L4 | L5 |
|-------|------:|---:|---:|---:|---:|---:|
| Ornith-9B bare | 43% | 91% | 51% | 27% | 3% | 0% |
| Ornith-9B + tools | 34% | 58% | 55% | 24% | 0% | 0% |
| Qwen3-8B RFT | 36% | 58% | 37% | 23% | 26% | 26% |
| Qwen3-8B RFT+tools | 24% | 81% | 9% | 4% | 0% | 6% |
| GLM 5.2 (no FT) | 49% | 58% | 61% | 65% | 13% | 12% |
| Kimi K2.6 + tools | 60% | 35% | 75% | 71% | 59% | 66% |

Results files: `ornith-experiment/results/benchmark_Ornith-9B_bare.json`, `benchmark_Ornith-9B_plus_tools.json`

#### Key findings

1. Bare Ornith (43%) already beats Qwen RFT (36%) with zero training.
2. Tools hurt overall (43% → 34%). On L1 eval-match tasks, the model calls eval_code, gets the right result, but its final answer echoes the expression instead of the value — grader extracts the wrong thing. Tools did slightly help L2 (51% → 55%) and the model attempts repair loops (2.1 tool calls/task vs Qwen RFT's 1.4).
3. L4/L5 near zero — model doesn't know Sema macros, async, records, quasiquote. Generates ML/Rust-style syntax for complex tasks. Several "no Sema code found" because reasoning traces exhaust token budget.
4. The bare model is strong at L1 (91%) and L2 (51%) — it picks up Sema syntax from the system prompt alone.

#### How to reproduce

```bash
# Ollama with 16k context (default 4k overflows with reasoning traces)
cat > /tmp/ornith-ctx.Modelfile << 'EOF'
FROM hf.co/deepreinforce-ai/Ornith-1.0-9B-GGUF:Q4_K_M
PARAMETER num_ctx 16384
PARAMETER temperature 0.6
PARAMETER top_p 0.95
EOF
ollama create ornith-9b -f /tmp/ornith-ctx.Modelfile

# Bare benchmark
cd ornith-experiment
OPENAI_BASE_URL=http://localhost:11434/v1 OPENAI_API_KEY=EMPTY \
  python3 benchmark.py --model ornith-9b --model-name "Ornith-9B bare" \
    --temperature 0.6 --top-p 0.95 \
    --sema-path ../target/debug/sema

# With tools
OPENAI_BASE_URL=http://localhost:11434/v1 OPENAI_API_KEY=EMPTY \
  python3 benchmark.py --model ornith-9b --model-name "Ornith-9B + tools" \
    --tools --reasoning --temperature 0.6 --top-p 0.95 \
    --sema-path ../target/debug/sema
```

### Infrastructure built (DONE)

| File | Status | Purpose |
|------|--------|---------|
| `benchmark.py` | done | OpenAI-compatible runner (vLLM/Ollama/Fireworks), eval_code+docs_search tool loop, reasoning model support |
| `generate_sft_dataset.py` | done | 2105 code-gen pairs from 4 sources, all VM-filtered |
| `generate_repair_traces.py` | done | Captures error→repair tool-call traces using a frontier model |
| `grader.py` | done | Grade Sema code completions (reused from RFT experiment) |
| `system_prompt.txt` | done | "Output code only, no Markdown fences" — fixes backtick habit |
| `sema-tools.sema` | done | MCP tools: eval_code + docs_search |
| `data/benchmark_tasks.jsonl` | done | 60 held-out tasks (L1-L5) |
| `data/sft/train.jsonl` | done | 1895 training pairs |
| `data/sft/val.jsonl` | done | 210 validation pairs |

SFT dataset breakdown (1895 train / 210 val):
- 1154 from eval tests (test names as descriptions)
- 789 from doc entries (API usage examples)
- 157 from example .sema files (function-level snippets)
- 5 negative corrections (Clojure anti-patterns → Sema)

## Remaining work

### Phase 2: Generate repair traces (NOT STARTED)

Use a frontier model (GLM 5.2 on Fireworks) with eval_code to solve tasks, capturing traces where the model makes an error, reads it, and fixes the code. Only keep traces with at least one error→repair cycle that end with a correct answer.

```bash
cd ornith-experiment
python3 generate_repair_traces.py \
  --base-url https://api.fireworks.ai/inference/v1 \
  --api-key $FIREWORKS_API_KEY \
  --llm-model accounts/fireworks/models/glm-5p2 \
  --output data/sft/repair_traces.jsonl \
  --max-traces 100
```

Target: 50-100 repair traces to mix into the SFT dataset.

### Phase 3: Build final SFT dataset (NOT STARTED)

Combine sources into the target mix:

| Category | Target % | Source | Count (est.) |
|----------|------:|--------|------:|
| NL → Sema code | 40% | eval tests + examples | ~850 |
| Tool-repair traces | 25% | Phase 2 output | ~100 |
| Docs/API examples | 20% | doc entries | ~790 |
| Fuzzer-generated | 10% | grammar-fuzz.sema | TBD |
| Negative corrections | 5% | Clojure anti-patterns | ~5 |

The fuzzer-generated portion still needs a generator script.

### Phase 4: Fine-tune (NOT STARTED — requires GPU)

```bash
# 9B QLoRA — pipeline validation (can run on single 24GB GPU)
# Use Unsloth or PEFT with LoRA rank 16-32, early stopping
# Small LR (1e-4 to 2e-4), 2-3 epochs max

# 31B/35B LoRA — serious attempt (requires rented GPU)
# Only if Phase 1 showed promise (it did: 43% bare)
```

Critical constraints:
- Small LoRA, early stopping — do not overwrite Ornith's agentic behavior
- Think of it as a "Sema accent pack", not a brain transplant
- Train on code-gen pairs + repair traces, NOT eval-answer pairs

### Phase 5: Evaluate fine-tuned model (NOT STARTED)

Run the same 60-task benchmark against:
1. SFT Ornith + tools
2. SFT Ornith without tools
3. Raw Ornith + tools (baseline from Phase 1)
4. Raw Ornith bare (baseline from Phase 1)

Acceptance bar:
```
Ornith-9B SFT + tools > raw Ornith-9B + tools by 10+ points
and
Ornith-9B SFT + tools does not lose iterative repair behavior
```

Track:
- Parse rate (does output parse as valid Sema?)
- Stdlib hallucination rate (invents nonexistent functions?)
- Repair-after-error rate (after eval_code error, fixes and retries?)
- Final benchmark pass rate (60-task score)

## Known issues to fix

1. **L1 grader extraction**: On eval-match tasks with tools, the model gets the right result via eval_code but its final answer echoes the expression. The grader needs to prefer the last eval_code result when available, or the system prompt needs to instruct "after using eval_code, state the result value only."

2. **Token budget exhaustion**: On L4/L5 tasks, Ornith's reasoning traces can consume the entire 4096 token budget before producing code. The 16k context helps but max_tokens may need to be higher for complex tasks.

3. **docs_search not indexed**: The vector store at `/tmp/sema-docs-rag.vec` doesn't exist. Need to run the indexing step before tools benchmark will fully work. eval_code works fine without it.

4. **Q4_K_M quantization**: Running Q4 quantization on M2 Max via Ollama. The real fine-tuned model should be tested at higher precision (Q6_K or bf16) to separate quantization effects from training effects.

## References

- Article: https://helgesver.re/articles/fine-tuning-failed-tools-won
- Ornith model card: https://huggingface.co/deepreinforce-ai/Ornith-1.0-9B
- Previous experiment: `experiment/rft-qwen3-sema` branch
