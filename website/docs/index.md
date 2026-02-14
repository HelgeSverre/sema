---
outline: [2, 3]
---

# Getting Started

Sema is a Scheme-like Lisp where prompts are s-expressions, conversations are persistent data structures, and LLM calls are just another form of evaluation. It combines a Scheme core with Clojure-style keywords (`:foo`), map literals (`{:key val}`), and vector literals (`[1 2 3]`).

## Why Sema?

- **LLMs as language primitives** — prompts, messages, conversations, tools, and agents are first-class data types, not string templates bolted on
- **Multi-provider** — Anthropic, OpenAI, Gemini, Groq, xAI, Mistral, Ollama, and more, all auto-configured from environment variables
- **Practical Lisp** — closures, tail-call optimization, macros, modules, error handling, HTTP, file I/O, regex, JSON — everything you need to build real programs
- **Embeddable** — clean Rust crate structure, sync interface, no global state leaks

## Installation

Install via Cargo:

```bash
cargo install --git https://github.com/HelgeSverre/sema sema
```

Or build from source:

```bash
git clone https://github.com/HelgeSverre/sema
cd sema
cargo build --release
# Binary is at target/release/sema
```

## Quick Start

```bash
sema                          # Start the REPL
sema script.sema              # Run a file
sema -e '(+ 1 2)'             # Evaluate an expression
sema -p '(map sqr (range 5))' # Evaluate and always print
```

```scheme
;; In the REPL:
sema> (define (greet name) (format "Hello, ~a!" name))
sema> (greet "world")
"Hello, world!"

sema> (map (lambda (x) (* x x)) (range 1 6))
(1 4 9 16 25)

sema> (define person {:name "Ada" :age 36})
sema> (:name person)
"Ada"
```

## Examples

### Working with Data

```scheme
;; Keywords as accessor functions
(define people [{:name "Ada" :age 36}
                {:name "Bob" :age 28}
                {:name "Cat" :age 42}])

(map (fn (p) (:name p)) people)
; => ("Ada" "Bob" "Cat")

(filter (fn (p) (> (:age p) 30)) people)
; => ({:name "Ada" :age 36} {:name "Cat" :age 42})
```

### LLM Completion

```scheme
;; Simple completion (requires an API key env var)
(llm/complete "Explain recursion in one sentence" {:max-tokens 50})

;; Structured chat with message history
(llm/chat
  [(message :system "You are a helpful assistant.")
   (message :user "What is Lisp? One sentence.")]
  {:max-tokens 100})
```

### Persistent Conversations

```scheme
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/say conv "Remember: the secret number is 7"))
(define conv (conversation/say conv "What is the secret number?"))
(conversation/last-reply conv)
; => "The secret number is 7."
```

## What's Next?

- [CLI Reference](./cli.md) — all flags, subcommands, and environment variables
- [Data Types](./language/data-types.md) — the 20 built-in types
- [Special Forms](./language/special-forms.md) — control flow, bindings, and iteration
- [Macros & Modules](./language/macros-modules.md) — metaprogramming and code organization
- [LLM Primitives](./llm/) — completions, chat, tools, agents, embeddings, and more
