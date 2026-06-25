# 07 — Deployment and Integration

How to deploy a fine-tuned Sema model and integrate it with Sema's existing LLM client
infrastructure. Covers Fireworks hosting, self-hosting, and API integration.

---

## 1. Fireworks Deployment

### 1.1 Live Merge Deployment (Recommended)

After fine-tuning completes, deploy with a single command. Fireworks merges the LoRA adapter
into the base model at deploy time — zero inference overhead.

```bash
# Deploy
firectl deployment create "accounts/<ACCOUNT_ID>/models/sema-glm51-v1"

# Verify it's running
firectl deployment get sema-glm51-v1
```

The deployment provides an OpenAI-compatible endpoint:

```
https://api.fireworks.ai/inference/v1/chat/completions
```

Model name: `accounts/<ACCOUNT_ID>/models/sema-glm51-v1`

### 1.2 Multi-LoRA Deployment

If you want to serve multiple fine-tuned variants (e.g., a Sema model and a general coding
model) on one deployment:

```bash
# Deploy base model with addon support
firectl deployment create "accounts/fireworks/models/glm-5p1" --enable-addons

# Load LoRA adapters
firectl load-lora sema-glm51-v1 --deployment <DEPLOYMENT_ID>
firectl load-lora general-coder-v1 --deployment <DEPLOYMENT_ID>
```

Route requests by specifying the LoRA in the model field:
```
model: "accounts/<ACCOUNT_ID>/models/sema-glm51-v1#accounts/<ACCOUNT_ID>/deployments/<DEPLOYMENT_ID>"
```

**Constraint**: Multi-LoRA only works with BF16 shapes (not FP8/FP4).

### 1.3 Inference API

All requests use the standard OpenAI chat completions format:

```bash
curl https://api.fireworks.ai/inference/v1/chat/completions \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $FIREWORKS_API_KEY" \
  -d '{
    "model": "accounts/<ACCOUNT_ID>/models/sema-glm51-v1",
    "messages": [
      {"role": "system", "content": "You are a Sema programming assistant..."},
      {"role": "user", "content": "Write a function that reverses a list in Sema"}
    ],
    "tools": [{"type": "function", "function": {"name": "sema_eval", ...}}]
  }'
```

---

## 2. Integration with Sema's LLM Client

Sema's `sema-llm` crate already supports multiple providers via a trait-based architecture.

### 2.1 Using the OpenAI Client (Simplest)

The Fireworks API is OpenAI-compatible. Point the existing OpenAI client at Fireworks:

```rust
// In sema-llm, the OpenAI client already exists
// Just change the base URL and model name

// For serverless GLM 5.2 (Phase 1 — no fine-tuning):
let client = OpenAiClient::new(
    "https://api.fireworks.ai/inference/v1",
    &fireworks_api_key,
);
// Model: "accounts/fireworks/models/glm-5p2"

// For fine-tuned GLM 5.1 (Phase 3):
// Model: "accounts/<ACCOUNT_ID>/models/sema-glm51-v1"
```

### 2.2 Adding a Fireworks Provider

For cleaner integration, add a dedicated Fireworks provider to `sema-llm`:

```rust
// crates/sema-llm/src/fireworks.rs
pub struct FireworksClient {
    client: OpenAiClient,  // Reuse the OpenAI-compatible client
    account_id: String,
}

impl FireworksClient {
    pub fn new(api_key: &str, account_id: &str) -> Self {
        Self {
            client: OpenAiClient::new(
                "https://api.fireworks.ai/inference/v1",
                api_key,
            ),
            account_id: account_id.to_string(),
        }
    }

    pub fn model(&self, model_name: &str) -> String {
        format!("accounts/{}/models/{}", self.account_id, model_name)
    }
}
```

### 2.3 Registering in Sema's Provider Registry

```rust
// In sema-llm registration
env.set(intern("llm/fireworks"), Value::native_fn_simple(|args| {
    let api_key = args[0].as_str()?;
    let account_id = args[1].as_str()?;
    let model = args[2].as_str()?;
    // Create Fireworks client and register as a provider
    Ok(Value::nil())
}));
```

### 2.4 Using from Sema Code

```lisp
;; Set the LLM provider to the fine-tuned Sema model
(llm/set-provider :fireworks
  {:api-key (env/get "FIREWORKS_API_KEY")
   :account-id "my-account"
   :model "sema-glm51-v1"})

;; Now all LLM calls use the Sema-aware model
(define response (llm/chat
  {:messages [{:role :user :content "Write a fibonacci function in Sema"}]}))

;; With tool-augmented self-correction
(deftool sema-eval
  "Evaluate Sema code"
  {:code "Sema code to evaluate"}
  (fn (args)
    (try {:result (eval (read-many (get args :code)))}
      (catch e {:error (:message e)}))))

(define agent (defagent sema-coder
  {:model "sema-glm51-v1"
   :tools [sema-eval]
   :system "You are a Sema programming assistant..."}))

(run agent "Write a web server with three routes")
```

---

## 3. Self-Hosting Alternatives

If the $7/hr Fireworks dedicated deployment cost is too high, self-hosting is an option.

### 3.1 vLLM (Recommended for Production)

vLLM provides an OpenAI-compatible API server with high-throughput inference.

```bash
# Install vLLM
pip install vllm

# Serve the fine-tuned model
vllm serve accounts/fireworks/models/glm-5p1 \
  --enable-lora \
  --lora-modules sema-glm51-v1=/path/to/lora-adapter \
  --port 8000
```

Or with a merged model (LoRA merged into base):

```bash
# Merge LoRA into base model first (using HuggingFace PEFT)
python merge_lora.py --base-model glm-5p1 --lora-adapter /path/to/adapter --output merged-model

# Serve the merged model
vllm serve merged-model --port 8000
```

The endpoint `http://localhost:8000/v1/chat/completions` is OpenAI-compatible — Sema's
existing OpenAI client works with no changes.

### 3.2 SGLang (Best Performance for MoE Models)

SGLang is recommended by both GLM and DeepSeek for best inference performance with MoE models:

```bash
pip install sglang

# Serve with FP8 quantization for better throughput
python -m sglang.launch_server \
  --model-path glm-5p1 \
  --lora-paths sema-glm51-v1=/path/to/adapter \
  --port 30000
```

### 3.3 Ollama (Best for Local Development)

```bash
# Create a Modelfile
cat > Modelfile <<EOF
FROM glm-5p1
ADAPTER /path/to/sema-lora-adapter
SYSTEM "You are a Sema programming assistant..."
EOF

# Build and run
ollama create sema-glm51-v1 -f Modelfile
ollama run sema-glm51-v1
```

Ollama runs on macOS (Apple Silicon) and provides an OpenAI-compatible API at
`http://localhost:11434/v1`.

### 3.4 Unsloth (Training + Local Inference on macOS)

Unsloth supports GLM-5.2 fine-tuning and local inference on macOS (Apple Silicon):

```python
from unsloth import FastLanguageModel

# Load GLM 5.2
model, tokenizer = FastLanguageModel.from_pretrained("zai-org/GLM-5.2")

# Fine-tune with LoRA
model = FastLanguageModel.get_peft_model(model, r=16)
# ... train ...

# Save
model.save_pretrained("sema-glm52-lora")

# Serve locally
# Unsloth provides a built-in OpenAI-compatible server
```

---

## 4. Cost Comparison

| Hosting Option | Setup | Hourly Cost | Monthly (8hr/day × 22 days) | Best For |
|---------------|-------|------------|----------------------------|----------|
| **Fireworks serverless** (GLM 5.2, no FT) | Minutes | $0 (per-token) | $10-50 (usage-based) | Phase 1, low traffic |
| **Fireworks dedicated** (fine-tuned) | Minutes | $7 (H100) | $1,232 | Production, high traffic |
| **Cloud GPU rental** (A100, vLLM) | Hours | $1-2 | $176-352 | Cost-sensitive production |
| **Cloud GPU rental** (H100, vLLM) | Hours | $2-4 | $352-704 | High throughput |
| **Local** (Apple Silicon, Ollama) | Hours | $0 | $0 (hardware cost) | Development, privacy |
| **Local** (2x RTX 4090, vLLM) | Hours | $0 | $0 (hardware cost) | Dev/staging |

### Recommended Hosting Strategy

1. **Development / testing**: Ollama on local machine (free)
2. **Staging**: Cloud GPU rental with vLLM ($1-2/hr A100, pay only when testing)
3. **Production**: Fireworks dedicated deployment ($7/hr H100) — zero maintenance, auto-scaling
4. **Production (cost-optimized)**: Cloud GPU rental with vLLM + auto-scale ($1-2/hr A100)
5. **Hybrid**: Serverless GLM 5.2 (system prompt only) for simple queries + fine-tuned model
   for complex tasks — route based on query complexity

---

## 5. API Compatibility Summary

All hosting options provide OpenAI-compatible APIs. Sema's existing `sema-llm` infrastructure
works with all of them:

| Hosting | Endpoint | Model Name |
|---------|----------|------------|
| Fireworks serverless | `https://api.fireworks.ai/inference/v1` | `accounts/fireworks/models/glm-5p2` |
| Fireworks dedicated | `https://api.fireworks.ai/inference/v1` | `accounts/<ACCT>/models/sema-glm51-v1` |
| vLLM | `http://<host>:8000/v1` | `sema-glm51-v1` |
| SGLang | `http://<host>:30000/v1` | `sema-glm51-v1` |
| Ollama | `http://localhost:11434/v1` | `sema-glm51-v1` |

```rust
// Sema's existing OpenAI client works with any of these:
let client = OpenAiClient::new(base_url, api_key);
// Just change base_url and model name
```

---

## 6. Monitoring and Observability

### Fireworks Built-in

- Job monitoring via `firectl sftj get <model_id>`
- Weights & Biases integration for training metrics
- Deployment metrics (latency, throughput) in Fireworks dashboard

### Custom Monitoring

For the `sema_eval` tool and self-correction loop:

```lisp
;; Track how often the model needs to self-correct
(define eval-stats (atom {:first-try-correct 0 :needed-fix 0 :failed 0}))

(deftool sema-eval-tracked
  "Evaluate Sema code with tracking"
  {:code "Sema code"}
  (fn (args)
    (let ((code (get args :code)))
      (try
        (let ((result (eval (read-many code))))
          (swap! eval-stats (fn (s) (map/update s :first-try-correct inc)))
          {:result result})
        (catch e
          (swap! eval-stats (fn (s) (map/update s :needed-fix inc)))
          {:error (:message e)})))))
```

---

## 7. Security Considerations

- **API keys**: Store Fireworks API key in environment variables, never in Sema source files
- **Sandbox**: The `sema_eval` tool runs user-generated code — use Sema's sandbox capability
  system (`Sandbox` in `EvalContext`) to restrict file/network access
- **Rate limiting**: Fireworks serverless has built-in rate limits; dedicated deployments do
  not — implement your own if exposing to external users
- **Data privacy**: Fireworks does not log prompts or generated data (only token counts for
  billing). Self-hosted vLLM/Ollama keeps all data on your infrastructure
