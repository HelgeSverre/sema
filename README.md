<p align="center">
  <img src="assets/logo.png" alt="Sema" width="640">
</p>

<p align="center">
  A Lisp with first-class LLM primitives, implemented in Rust.
</p>

Sema is a Scheme-like Lisp where prompts are s-expressions, conversations are persistent data structures, and LLM calls are just another form of evaluation. It combines a Scheme core with Clojure-style keywords (`:foo`), map literals (`{:key val}`), and vector literals (`[1 2 3]`).

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

## Language Overview

```scheme
;; Recursion
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))
(factorial 10) ; => 3628800

;; Pattern matching with cond
(define (fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

;; Higher-order functions
(map (lambda (x) (* x x)) (range 1 6))  ; => (1 4 9 16 25)
(filter even? (range 1 11))              ; => (2 4 6 8 10)
(foldl + 0 (range 1 11))                ; => 55

;; Maps (Clojure-style)
(define person {:name "Ada" :age 36 :lang "Sema"})
(:name person)                    ; => "Ada"
(assoc person :age 37)            ; => {:age 37 :lang "Sema" :name "Ada"}

;; Quasiquote with unquote
(define lang "Sema")
`("Hello from " ,lang "!")        ; => ("Hello from " "Sema" "!")

;; Let bindings
(let ((x 10) (y 20))
  (+ x y))                        ; => 30

;; Closures and composition
(define (compose f g)
  (lambda (x) (f (g x))))

(define inc-then-double
  (compose (lambda (x) (* x 2))
           (lambda (x) (+ x 1))))
(inc-then-double 5)               ; => 12
```

## LLM Primitives

Set an API key to enable LLM features:

```
export ANTHROPIC_API_KEY=sk-...
# or
export OPENAI_API_KEY=sk-...
```

```scheme
;; Auto-configure from environment
(llm/auto-configure)

;; Simple completion
(llm/complete "Say hello in 5 words" {:max-tokens 50})

;; Chat with message roles
(llm/chat
  [(message :system "You are a helpful assistant.")
   (message :user "What is Lisp? One sentence.")]
  {:max-tokens 100})

;; Prompts as composable data
(define review-prompt
  (prompt
    (system "You are a code reviewer. Be concise.")
    (user "Review this: (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))")))
(llm/send review-prompt {:max-tokens 100})

;; Structured extraction
(llm/extract
  {:vendor {:type :string} :amount {:type :number} :date {:type :string}}
  "I bought coffee for $4.50 at Blue Bottle on Jan 15, 2025")
; => {:amount 4.5 :date "2025-01-15" :vendor "Blue Bottle"}

;; Classification
(llm/classify [:positive :negative :neutral]
              "This product is amazing!")
; => :positive

;; Persistent conversations
(define conv (conversation/new {}))
(define conv (conversation/say conv "Remember: the secret number is 7"))
(define conv (conversation/say conv "What is the secret number?"))
(conversation/last-reply conv) ; => "The secret number is 7."

;; Tool use
(deftool lookup-capital
  "Look up the capital of a country"
  {:country {:type :string :description "Country name"}}
  (lambda (country)
    (cond
      ((= country "Norway") "Oslo")
      ((= country "France") "Paris")
      (else "Unknown"))))

(llm/chat
  [(message :user "What is the capital of Norway?")]
  {:tools [lookup-capital] :max-tokens 100})

;; Agents with tools and multi-turn loops
(defagent weather-bot
  {:system "You are a weather assistant."
   :tools [get-weather]
   :max-turns 3})
(agent/run weather-bot "Weather in Tokyo?")

;; Batch processing
(llm/pmap
  (lambda (word)
    (llm/complete (format "Translate to Spanish: ~a" word) {:max-tokens 10}))
  (list "sun" "moon" "star"))
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
