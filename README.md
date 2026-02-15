<p align="center">
  <img src="assets/logo.png" alt="Sema" width="640">
</p>

<p align="center">
  A Lisp with first-class LLM primitives, implemented in Rust.
</p>

<p align="center">
  <a href="https://sema.run"><img src="https://img.shields.io/badge/try_it-sema.run-c8a855?style=flat" alt="Playground"></a>
  <a href="https://sema-lang.com/docs/"><img src="https://img.shields.io/badge/docs-sema--lang.com-c8a855?style=flat" alt="Docs"></a>
  <a href="https://github.com/HelgeSverre/sema/releases/latest"><img src="https://img.shields.io/github/v/tag/HelgeSverre/sema?label=version&color=c8a855&style=flat" alt="Version"></a>
  <a href="LICENSE"><img src="https://img.shields.io/badge/license-MIT-c8a855?style=flat" alt="License"></a>
</p>

Sema is a Scheme-like Lisp where **prompts are s-expressions**, **conversations are persistent data structures**, and **LLM calls are just another form of evaluation**. It combines a Scheme core with Clojure-style keywords (`:foo`), map literals (`{:key val}`), and vector literals (`[1 2 3]`).

## What It Looks Like

A coding agent with file tools, safety checks, and budget tracking — in ~40 lines:

```scheme
;; Define tools the LLM can call
(deftool read-file
  "Read a file's contents"
  {:path {:type :string :description "File path"}}
  (lambda (path)
    (if (file/exists? path) (file/read path) "File not found")))

(deftool edit-file
  "Replace text in a file"
  {:path {:type :string} :old {:type :string} :new {:type :string}}
  (lambda (path old new)
    (file/write path (string/replace (file/read path) old new))
    "Done"))

(deftool run-command
  "Run a shell command"
  {:command {:type :string :description "Shell command to run"}}
  (lambda (command) (:stdout (shell "sh" "-c" command))))

;; Create an agent with tools, system prompt, and spending limit
(llm/set-budget 0.50)

(defagent coder
  {:system (format "You are a coding assistant. Working directory: ~a" (sys/cwd))
   :tools [read-file edit-file run-command]
   :model "claude-sonnet-4-20250514"
   :max-turns 20})

;; Run it
(define result (agent/run coder "Add error handling to src/main.rs"))
(println (:response result))
(println (format "Cost: $~a" (:spent (llm/budget-remaining))))
```

## Key Features

```scheme
;; Simple completion
(llm/complete "Explain monads in one sentence")

;; Structured data extraction — returns a map, not a string
(llm/extract
  {:vendor {:type :string} :amount {:type :number} :date {:type :string}}
  "Bought coffee for $4.50 at Blue Bottle on Jan 15")
;; => {:amount 4.5 :date "2025-01-15" :vendor "Blue Bottle"}

;; Classification
(llm/classify [:positive :negative :neutral] "This product is amazing!")
;; => :positive

;; Multi-turn conversations as immutable data
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/say conv "The secret number is 7"))
(define conv (conversation/say conv "What's the secret number?"))
(conversation/last-reply conv) ;; => "The secret number is 7."

;; Streaming
(llm/stream "Tell me a story" {:max-tokens 500})

;; Batch — all prompts sent concurrently
(llm/batch ["Translate 'hello' to French"
            "Translate 'hello' to Spanish"
            "Translate 'hello' to German"])

;; Cost tracking
(llm/set-budget 1.00)
(llm/budget-remaining) ;; => {:limit 1.0 :spent 0.05 :remaining 0.95}
```

## Supported Providers

All providers are auto-configured from environment variables — just set the API key and go.

| Provider          | Chat | Stream | Tools | Embeddings |
| ----------------- | ---- | ------ | ----- | ---------- |
| **Anthropic**     | ✅   | ✅     | ✅    | —          |
| **OpenAI**        | ✅   | ✅     | ✅    | ✅         |
| **Google Gemini** | ✅   | ✅     | ✅    | —          |
| **Ollama**        | ✅   | ✅     | ✅    | —          |
| **Groq**          | ✅   | ✅     | ✅    | —          |
| **xAI**           | ✅   | ✅     | ✅    | —          |
| **Mistral**       | ✅   | ✅     | ✅    | —          |
| **Moonshot**      | ✅   | ✅     | ✅    | —          |
| **Jina**          | —    | —      | —     | ✅         |
| **Voyage**        | —    | —      | —     | ✅         |
| **Cohere**        | —    | —      | —     | ✅         |

## It's Also a Real Lisp

350+ built-in functions, tail-call optimization, macros, modules, error handling — not a toy.

```scheme
;; Closures, higher-order functions, TCO
(define (fibonacci n)
  (let loop ((i 0) (a 0) (b 1))
    (if (= i n) a (loop (+ i 1) b (+ a b)))))
(fibonacci 50) ;; => 12586269025

;; Maps, keywords-as-functions, destructuring
(define person {:name "Ada" :age 36 :langs ["Lisp" "Rust"]})
(:name person) ;; => "Ada"

;; Functional pipelines
(->> (range 1 100)
     (filter even?)
     (map (fn (x) (* x x)))
     (take 5))
;; => (4 16 36 64 100)

;; Macros
(defmacro unless (test . body)
  `(if ,test nil (begin ,@body)))

;; Modules
(module utils (export square)
  (define (square x) (* x x)))

;; HTTP, JSON, regex, file I/O, crypto, CSV, datetime...
(define data (json/decode (http/get "https://api.example.com/data")))
```

> 📖 Full language reference, stdlib docs, and more examples at **[sema-lang.com/docs](https://sema-lang.com/docs/)**

## Try It Now

> **[sema.run](https://sema.run)** — Browser-based playground with 20+ example programs.
> No install required. Runs entirely in WebAssembly.

## Installation

```bash
cargo install --git https://github.com/HelgeSverre/sema sema
```

Or build from source:

```bash
git clone https://github.com/HelgeSverre/sema
cd sema && cargo build --release
# Binary at target/release/sema
```

```bash
sema                          # REPL
sema script.sema              # Run a file
sema -e '(+ 1 2)'             # Evaluate expression
sema --no-llm script.sema     # Run without LLM (faster startup)
```

> 📖 Full CLI reference, flags, and REPL commands: **[sema-lang.com/docs/cli](https://sema-lang.com/docs/cli.html)**

## Example Programs

The [`examples/`](examples/) directory has 50+ programs:

| Example | What it does |
| --- | --- |
| [`coding-agent.sema`](examples/ai-tools/coding-agent.sema) | Full coding agent with file editing, search, and shell tools |
| [`review.sema`](examples/ai-tools/review.sema) | AI code reviewer for git diffs |
| [`commit-msg.sema`](examples/ai-tools/commit-msg.sema) | Generate conventional commit messages from staged changes |
| [`summarize.sema`](examples/ai-tools/summarize.sema) | Summarize files or piped input |
| [`game-of-life.sema`](examples/game-of-life.sema) | Conway's Game of Life |
| [`brainfuck.sema`](examples/brainfuck.sema) | Brainfuck interpreter |
| [`mandelbrot.sema`](examples/mandelbrot.sema) | ASCII Mandelbrot set |
| [`json-api.sema`](examples/json-api.sema) | Fetch and process JSON APIs |

## Why Sema?

- **LLMs as language primitives** — prompts, messages, conversations, tools, and agents are first-class data types, not string templates bolted on
- **Multi-provider** — swap between Anthropic, OpenAI, Gemini, Ollama, and more with a single config change
- **Cost-aware** — built-in budget tracking with dynamic pricing from [llm-prices.com](https://www.llm-prices.com)
- **Practical Lisp** — closures, TCO, macros, modules, error handling, HTTP, file I/O, regex, JSON, and 350+ stdlib functions
- **Embeddable** — clean Rust crate structure with a builder API

### Why Not Sema?

- No full numeric tower (rationals, bignums, complex numbers)
- No continuations (`call/cc`) or hygienic macros (`syntax-rules`)
- Single-threaded — concurrency is at the provider level only
- Young language — solid but not battle-tested at scale

## Architecture

```
crates/
  sema-core/     Value types, errors, environment
  sema-reader/   Lexer and s-expression parser
  sema-eval/     Trampoline-based evaluator, special forms, modules
  sema-stdlib/   350+ built-in functions across 19 modules
  sema-llm/      LLM provider trait + multi-provider clients
  sema/          CLI binary: REPL + file runner
```

> 🔬 Deep-dive into the internals: [Architecture](https://sema-lang.com/docs/internals/architecture.html) · [Evaluator](https://sema-lang.com/docs/internals/evaluator.html) · [Lisp Comparison](https://sema-lang.com/docs/internals/lisp-comparison.html)

## License

MIT — see [LICENSE](LICENSE).
