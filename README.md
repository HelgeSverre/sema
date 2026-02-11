# Sema

A Lisp with first-class LLM primitives, implemented in Rust.

Sema is a Scheme-like Lisp where prompts are s-expressions, conversations are persistent data structures, and LLM calls are just another form of evaluation. It combines a Scheme core with Clojure-style keywords (`:foo`) and map literals (`{:key val}`).

## Building

```
cargo build --release
```

## Usage

Run a file:

```
cargo run -- examples/hello.sema
```

Start the REPL:

```
cargo run
```

### LLM features

Set an API key to use LLM primitives:

```
export ANTHROPIC_API_KEY=sk-...
# or
export OPENAI_API_KEY=sk-...
```

Then run the LLM demo:

```
cargo run -- examples/llm-hello.sema
```

## Language Overview

```scheme
;; Functions
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

;; Maps (Clojure-style)
(define person {:name "Ada" :age 36})
(:name person)  ; => "Ada"

;; Higher-order functions
(map (lambda (x) (* x x)) (range 1 6))  ; => (1 4 9 16 25)
(filter even? (range 1 11))              ; => (2 4 6 8 10)
(foldl + 0 (range 1 11))                ; => 55

;; LLM completion
(llm/complete "Say hello in 5 words" {:max-tokens 50})

;; Structured extraction
(llm/extract {:name {:type :string} :age {:type :number}}
             "Ada Lovelace was 36")

;; Tool use
(deftool get-weather "Get weather" {:city {:type :string}}
  (lambda (city) (format "Sunny in ~a" city)))

;; Agents
(defagent assistant {:system "You are helpful." :tools [get-weather] :max-turns 3})
(agent/run assistant "Weather in Tokyo?")
```

## Crate Structure

| Crate | Purpose |
|-------|---------|
| `sema-core` | Value types, errors, environment |
| `sema-reader` | Lexer and s-expression reader |
| `sema-eval` | Evaluator and special forms |
| `sema-llm` | LLM provider trait and API clients (Anthropic, OpenAI) |
| `sema-stdlib` | Standard library builtins |
| `sema` | Binary: REPL and file runner |

## Tests

```
cargo test
```

## License

TBD
