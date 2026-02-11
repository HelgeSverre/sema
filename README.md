<p align="center">
  <img src="assets/logo.png" alt="Sema" width="640">
</p>

<p align="center">
  A Lisp with first-class LLM primitives, implemented in Rust.
</p>

Sema is a Scheme-like Lisp where prompts are s-expressions, conversations are persistent data structures, and LLM calls are just another form of evaluation. It combines a Scheme core with Clojure-style keywords (`:foo`), map literals (`{:key val}`), and vector literals (`[1 2 3]`).

## Installation

```bash
cargo install --git https://github.com/HelgeSverre/sema
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

## Why Sema?

- **LLMs as language primitives** — prompts, messages, conversations, tools, and agents are first-class data types, not string templates bolted on
- **Multi-provider** — Anthropic, OpenAI, Gemini, Groq, xAI, Mistral, Ollama, and more, all auto-configured from environment variables
- **Practical Lisp** — closures, tail-call optimization, macros, modules, error handling, HTTP, file I/O, regex, JSON — everything you need to build real programs
- **Embeddable** — clean Rust crate structure, sync interface, no global state leaks

### Why Not Sema?

- No full numeric tower (rationals, bignums, complex numbers)
- No continuations (`call/cc`)
- No hygienic macros (`syntax-rules`)
- Single-threaded — concurrency is at the provider level only (LLM batch calls)
- Young language — stdlib is solid but not battle-tested at scale

---

## Language Reference

### Data Types

| Type         | Syntax               | Examples                        |
| ------------ | -------------------- | ------------------------------- |
| Integer      | digits               | `42`, `-7`, `0`                 |
| Float        | digits with `.`      | `3.14`, `-0.5`, `1e10`          |
| String       | double-quoted        | `"hello"`, `"line\nbreak"`      |
| Boolean      | `#t` / `#f`          | `#t`, `#f`                      |
| Nil          | `nil`                | `nil`                           |
| Symbol       | bare identifier      | `foo`, `my-var`, `+`            |
| Keyword      | colon-prefixed       | `:name`, `:type`, `:ok`         |
| List         | parenthesized        | `(1 2 3)`, `(+ a b)`            |
| Vector       | bracketed            | `[1 2 3]`, `["a" "b"]`          |
| Map          | curly-braced         | `{:name "Ada" :age 36}`         |
| Prompt       | `(prompt ...)`       | LLM prompt (see below)          |
| Message      | `(message ...)`      | LLM message (see below)         |
| Conversation | `(conversation/new)` | LLM conversation (see below)    |
| Tool         | `(deftool ...)`      | LLM tool definition (see below) |
| Agent        | `(defagent ...)`     | LLM agent (see below)           |

### Special Forms

These are built into the evaluator — they control evaluation order and cannot be redefined.

#### Definitions & Assignment

```scheme
(define x 42)                          ; bind a value
(define (square x) (* x x))           ; define a function (shorthand)
(set! x 99)                           ; mutate an existing binding
```

#### Functions

```scheme
(lambda (x y) (+ x y))                ; anonymous function
(fn (x) (* x x))                      ; fn is an alias for lambda
(fn (x . rest) rest)                   ; rest parameters with dot notation
```

#### Conditionals

```scheme
(if (> x 0) "positive" "non-positive")

(cond
  ((< x 0) "negative")
  ((= x 0) "zero")
  (else "positive"))

(case (:status response)
  ((:ok) "success")
  ((:error :timeout) "failure")
  (else "unknown"))

(when (> x 0) (println "positive"))    ; only runs body if true
(unless (> x 0) (println "not positive"))
```

#### Bindings

```scheme
(let ((x 10) (y 20))                   ; parallel bindings
  (+ x y))

(let* ((x 10) (y (* x 2)))            ; sequential bindings (y sees x)
  (+ x y))

(letrec ((even? (fn (n) (if (= n 0) #t (odd? (- n 1)))))
         (odd?  (fn (n) (if (= n 0) #f (even? (- n 1))))))
  (even? 10))

;; Named let (loop with TCO)
(let loop ((i 0) (sum 0))
  (if (= i 100)
    sum
    (loop (+ i 1) (+ sum i))))
```

#### Sequencing & Logic

```scheme
(begin expr1 expr2 ... exprN)          ; evaluate in order, return last
(do expr1 expr2 ... exprN)             ; alias for begin
(and a b c)                            ; short-circuit, returns last truthy or #f
(or a b c)                             ; short-circuit, returns first truthy or #f
```

#### Error Handling

```scheme
(try
  (/ 1 0)
  (catch e
    (println (format "Error: ~a" (:message e)))
    (:type e)))        ; => :eval

(throw "something went wrong")         ; throw any value
(throw {:code 404 :reason "not found"})
```

Error maps in `catch` have keys: `:type`, `:message`, `:stack-trace`, and variant-specific keys (`:value`, `:expected`/`:got`, `:name`).

#### Macros

```scheme
(defmacro unless2 (test . body)
  `(if ,test nil (begin ,@body)))

(unless2 #f (println "runs!"))

(macroexpand '(unless2 #f (println "x")))  ; see expansion
(gensym "tmp")                              ; hygienic-ish symbol generation
```

#### Modules

```scheme
;; math-utils.sema
(module math-utils
  (export square cube)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (define (internal-helper x) x))      ; not exported

;; main.sema
(import "math-utils.sema")
(square 5)   ; => 25
(cube 3)     ; => 27
```

#### Metaprogramming

```scheme
(eval '(+ 1 2))                        ; evaluate data as code
(read "(+ 1 2)")                       ; parse string to value
(read-many "(+ 1 2) (* 3 4)")          ; parse multiple forms
(type 42)                              ; => "integer"
(string->symbol "foo")                 ; type conversions
(keyword->string :bar)                 ; => "bar"
```

### Standard Library

#### Arithmetic & Math

```scheme
(+ 1 2 3)        ; => 6       (- 10 3)      ; => 7
(* 4 5)          ; => 20      (/ 10 2)      ; => 5
(mod 10 3)       ; => 1

(abs -5)         ; => 5       (min 1 2 3)   ; => 1
(max 1 2 3)      ; => 3       (pow 2 10)    ; => 1024
(sqrt 16)        ; => 4.0     (log 100)     ; => 4.605...
(floor 3.7)      ; => 3.0     (ceil 3.2)    ; => 4.0
(round 3.5)      ; => 4.0

;; Trigonometry
(sin pi)         (cos pi)     (math/tan pi)
(math/asin 1)    (math/acos 0)  (math/atan 1)  (math/atan2 1 1)

;; Utility
(math/exp 1)     (math/log10 100)  (math/log2 8)
(math/gcd 12 8)  (math/lcm 4 6)
(math/quotient 10 3)  (math/remainder 10 3)
(math/random)    (math/random-int 1 100)
(math/clamp 15 0 10)  (math/sign -5)

;; Constants
pi               ; => 3.14159...
e                ; => 2.71828...

;; Bitwise
(bit/and 5 3)    (bit/or 5 3)    (bit/xor 5 3)
(bit/not 5)      (bit/shift-left 1 4)  (bit/shift-right 16 2)
```

#### Strings

```scheme
(string-append "hello" " " "world")   ; => "hello world"
(string-length "hello")               ; => 5
(string-ref "hello" 0)                ; => "h"
(substring "hello" 1 3)               ; => "el"
(str 42)                              ; => "42" (any value to string)
(format "~a is ~a" "Sema" "great")    ; => "Sema is great"

;; Slash-namespaced string operations
(string/split "a,b,c" ",")            ; => ("a" "b" "c")
(string/join '("a" "b" "c") ", ")     ; => "a, b, c"
(string/trim "  hello  ")             ; => "hello"
(string/trim-left "  hi")             ; => "hi"
(string/trim-right "hi  ")            ; => "hi"
(string/upper "hello")                ; => "HELLO"
(string/lower "HELLO")                ; => "hello"
(string/contains? "hello" "ell")      ; => #t
(string/starts-with? "hello" "he")    ; => #t
(string/ends-with? "hello" "lo")      ; => #t
(string/replace "hello" "l" "r")      ; => "herro"
(string/index-of "hello" "ll")        ; => 2
(string/chars "abc")                  ; => ("a" "b" "c")
(string/repeat "ab" 3)                ; => "ababab"
(string/pad-left "42" 5 "0")          ; => "00042"
(string/pad-right "hi" 5)             ; => "hi   "
(string/number? "42")                 ; => #t

;; Type conversions
(string->number "42")                 ; => 42
(number->string 42)                   ; => "42"
(string->symbol "foo")                ; => foo
(symbol->string 'foo)                 ; => "foo"
(string->keyword "name")              ; => :name
(keyword->string :name)               ; => "name"
```

#### Lists

```scheme
(list 1 2 3)                          ; => (1 2 3)
(cons 0 '(1 2 3))                     ; => (0 1 2 3)
(car '(1 2 3))                        ; => 1
(cdr '(1 2 3))                        ; => (2 3)
(first '(1 2 3))                      ; => 1
(rest '(1 2 3))                       ; => (2 3)
(last '(1 2 3))                       ; => 3
(nth '(10 20 30) 1)                   ; => 20
(length '(1 2 3))                     ; => 3
(append '(1 2) '(3 4))                ; => (1 2 3 4)
(reverse '(1 2 3))                    ; => (3 2 1)
(range 5)                             ; => (0 1 2 3 4)
(range 1 5)                           ; => (1 2 3 4)

;; Higher-order functions
(map (fn (x) (* x x)) '(1 2 3))      ; => (1 4 9)
(map + '(1 2 3) '(10 20 30))          ; => (11 22 33)  (multi-list)
(filter even? '(1 2 3 4 5))           ; => (2 4)
(foldl + 0 '(1 2 3 4 5))             ; => 15
(foldr cons '() '(1 2 3))            ; => (1 2 3)
(reduce + '(1 2 3 4 5))              ; => 15
(for-each println '("a" "b" "c"))     ; side-effect iteration
(sort '(3 1 4 1 5))                   ; => (1 1 3 4 5)
(sort-by length '("bb" "a" "ccc"))    ; => ("a" "bb" "ccc")
(apply + '(1 2 3))                    ; => 6

;; Sublists
(take 3 '(1 2 3 4 5))                ; => (1 2 3)
(drop 2 '(1 2 3 4 5))                ; => (3 4 5)
(flatten '(1 (2 (3)) 4))             ; => (1 2 3 4)
(zip '(1 2 3) '("a" "b" "c"))        ; => ((1 "a") (2 "b") (3 "c"))
(partition even? '(1 2 3 4 5))        ; => ((2 4) (1 3 5))

;; Searching
(member 3 '(1 2 3 4))                ; => (3 4)
(any even? '(1 3 5 6))               ; => #t
(every even? '(2 4 6))               ; => #t
(list/index-of '(10 20 30) 20)       ; => 1
(list/unique '(1 2 2 3 3 3))         ; => (1 2 3)

;; Grouping
(list/group-by even? '(1 2 3 4 5))   ; => {#f (1 3 5) #t (2 4)}
(list/interleave '(1 2 3) '(a b c))  ; => (1 a 2 b 3 c)
(list/chunk 2 '(1 2 3 4 5))          ; => ((1 2) (3 4) (5))
(frequencies '(a b a c b a))          ; => {a 3 b 2 c 1}
(interpose ", " '("a" "b" "c"))      ; => ("a" ", " "b" ", " "c")
```

#### Vectors

```scheme
(vector 1 2 3)                        ; => [1 2 3]
[1 2 3]                               ; literal syntax
(vector->list [1 2 3])                ; => (1 2 3)
(list->vector '(1 2 3))               ; => [1 2 3]
```

Most list functions work on vectors too.

#### Maps

```scheme
(hash-map :a 1 :b 2)                 ; => {:a 1 :b 2}
{:a 1 :b 2}                          ; literal syntax
(get {:a 1 :b 2} :a)                 ; => 1
(:a {:a 1 :b 2})                     ; => 1  (keywords are functions)
(assoc {:a 1} :b 2)                  ; => {:a 1 :b 2}
(dissoc {:a 1 :b 2} :a)             ; => {:b 2}
(merge {:a 1} {:b 2} {:c 3})        ; => {:a 1 :b 2 :c 3}
(keys {:a 1 :b 2})                   ; => (:a :b)
(vals {:a 1 :b 2})                   ; => (1 2)
(contains? {:a 1} :a)               ; => #t
(count {:a 1 :b 2})                  ; => 2
(map/entries {:a 1 :b 2})           ; => ((:a 1) (:b 2))
(map/from-entries '((:a 1) (:b 2))) ; => {:a 1 :b 2}

;; Higher-order map operations
(map/map-vals (fn (v) (* v 2)) {:a 1 :b 2})     ; => {:a 2 :b 4}
(map/map-keys (fn (k) (string->keyword (string/upper (keyword->string k)))) {:a 1})
(map/filter (fn (k v) (> v 1)) {:a 1 :b 2 :c 3}) ; => {:b 2 :c 3}
(map/select-keys {:a 1 :b 2 :c 3} '(:a :c))      ; => {:a 1 :c 3}
(map/update {:a 1} :a (fn (v) (+ v 10)))           ; => {:a 11}
```

#### Predicates & Type Checking

```scheme
(null? '())        (nil? nil)         (empty? "")
(list? '(1))       (vector? [1])      (map? {:a 1})
(number? 42)       (integer? 42)      (float? 3.14)
(string? "hi")     (symbol? 'x)       (keyword? :k)
(bool? #t)         (fn? car)
(zero? 0)          (even? 4)          (odd? 3)
(positive? 1)      (negative? -1)
(eq? 'a 'a)        (= 1 1)

;; LLM type predicates
(prompt? p)        (message? m)       (conversation? c)
(tool? t)          (agent? a)
```

#### I/O

```scheme
;; Console
(display "no newline")                ; print without newline
(println "with newline")              ; print with newline
(print "also no newline")             ; alias for display
(newline)                             ; print a newline
(read-line)                           ; read a line from stdin

;; Files
(file/read "data.txt")               ; => file contents as string
(file/write "out.txt" "content")     ; write (overwrite)
(file/append "log.txt" "line\n")     ; append
(file/read-lines "data.txt")         ; => list of lines
(file/write-lines "out.txt" '("a" "b"))
(file/delete "tmp.txt")              ; delete file
(file/rename "old.txt" "new.txt")    ; rename/move
(file/copy "src.txt" "dst.txt")      ; copy
(file/exists? "data.txt")            ; => #t/#f
(file/is-file? "data.txt")           ; => #t
(file/is-directory? "src/")          ; => #t
(file/is-symlink? "link")            ; => #t/#f
(file/list "src/")                   ; => list of entries
(file/mkdir "new-dir")               ; create directory
(file/info "data.txt")               ; => {:size N :modified N ...}

;; Paths
(path/join "src" "main.rs")          ; => "src/main.rs"
(path/dirname "/a/b/c.txt")         ; => "/a/b"
(path/basename "/a/b/c.txt")        ; => "c.txt"
(path/extension "file.rs")          ; => "rs"
(path/absolute "file.txt")          ; => "/full/path/file.txt"
```

#### HTTP

```scheme
(http/get "https://httpbin.org/get")
; => {:status 200 :headers {...} :body "..."}

(http/post "https://httpbin.org/post"
  {:body {:key "value"}              ; maps auto-serialize as JSON
   :headers {"Authorization" "Bearer ..."}})

(http/put url {:body "data"})
(http/delete url)
(http/request {:method "PATCH" :url url :body "data"})
```

#### JSON

```scheme
(json/encode {:name "Ada" :age 36})       ; => "{\"age\":36,\"name\":\"Ada\"}"
(json/encode-pretty {:a 1 :b [2 3]})      ; => formatted JSON
(json/decode "{\"name\":\"Ada\"}")         ; => {:name "Ada"}
```

#### Regex

```scheme
(regex/match? "\\d+" "abc123")             ; => #t
(regex/match "^(\\w+)@(\\w+)" "user@host")  ; => ("user@host" "user" "host")
(regex/find-all "\\d+" "a1b2c3")           ; => ("1" "2" "3")
(regex/replace "\\d" "a1b2" "X")           ; => "aXb2" (first match)
(regex/replace-all "\\d" "a1b2" "X")       ; => "aXbX"
(regex/split "," "a,b,c")                  ; => ("a" "b" "c")
```

#### CSV

```scheme
(csv/parse "a,b\n1,2\n3,4")               ; => (("a" "b") ("1" "2") ("3" "4"))
(csv/parse-maps "name,age\nAda,36")       ; => ({:age "36" :name "Ada"})
(csv/encode '(("a" "b") ("1" "2")))       ; => "a,b\n1,2\n"
```

#### Crypto & Encoding

```scheme
(uuid/v4)                                  ; => "550e8400-e29b-41d4-..."
(base64/encode "hello")                    ; => "aGVsbG8="
(base64/decode "aGVsbG8=")                ; => "hello"
(hash/sha256 "hello")                     ; => "2cf24dba..."
```

#### Date & Time

```scheme
(time/now)                                ; => 1707955200.123 (unix secs)
(time-ms)                                 ; => 1707955200123  (unix ms)
(time/format (time/now) "%Y-%m-%d")       ; => "2025-02-15"
(time/parse "2025-01-15" "%Y-%m-%d")      ; => 1736899200.0
(time/date-parts (time/now))              ; => {:year 2025 :month 2 :day 15 ...}
(time/add (time/now) 86400)               ; add seconds
(time/diff t1 t2)                         ; difference in seconds
(sleep 1000)                              ; sleep N milliseconds
```

#### System

```scheme
(env "HOME")                              ; => "/Users/ada"
(sys/args)                                ; => ("sema" "script.sema" ...)
(sys/cwd)                                 ; => "/current/dir"
(sys/platform)                            ; => "macos" / "linux" / "windows"
(sys/env-all)                             ; => {:HOME "..." :PATH "..." ...}
(sys/set-env "KEY" "value")               ; set env var
(shell "ls -la")                          ; run shell command, return stdout
(exit 0)                                  ; exit with code
```

---

## LLM Primitives

Sema's differentiating feature: LLM operations are first-class language primitives with prompts, conversations, tools, and agents as native data types.

### Setup

Set one or more API keys as environment variables:

```bash
export ANTHROPIC_API_KEY=sk-ant-...
export OPENAI_API_KEY=sk-...
# or any other supported provider (see table below)
```

Sema auto-detects and configures all available providers on startup. Use `--no-llm` to skip auto-configuration.

### Completion

```scheme
;; Simple completion
(llm/complete "Say hello in 5 words" {:max-tokens 50})

;; With options
(llm/complete "Explain monads"
  {:model "claude-haiku-4-5-20251001"
   :max-tokens 200
   :temperature 0.3
   :system "You are a Haskell expert."})

;; Streaming (prints chunks as they arrive)
(llm/stream "Tell me a story" {:max-tokens 200})

;; Streaming with a callback
(llm/stream "Tell me a story"
  (fn (chunk) (display chunk))
  {:max-tokens 200})
```

### Messages & Chat

```scheme
;; Chat with message list
(llm/chat
  [(message :system "You are a helpful assistant.")
   (message :user "What is Lisp? One sentence.")]
  {:max-tokens 100})

;; Prompts as composable data structures
(define review-prompt
  (prompt
    (system "You are a code reviewer. Be concise.")
    (user "Review this function.")))

(llm/send review-prompt {:max-tokens 200})

;; Compose prompts
(define base (prompt (system "You are helpful.")))
(define question (prompt (user "What is 2+2?")))
(llm/send (prompt/append base question))

;; Inspect prompts and messages
(prompt/messages my-prompt)             ; => list of messages
(prompt/set-system my-prompt "new sys") ; => new prompt with replaced system msg
(message/role (message :user "hi"))     ; => :user
(message/content (message :user "hi"))  ; => "hi"
```

### Conversations

Persistent, immutable conversation state with automatic LLM round-trips:

```scheme
(define conv (conversation/new {:model "claude-haiku-4-5-20251001"}))
(define conv (conversation/say conv "Remember: the secret number is 7"))
(define conv (conversation/say conv "What is the secret number?"))
(conversation/last-reply conv)          ; => "The secret number is 7."

;; With options
(define conv (conversation/say conv "Explain more"
  {:temperature 0.5 :max-tokens 500}))

;; Inspect
(conversation/messages conv)            ; => list of message values
(conversation/model conv)              ; => "claude-haiku-4-5-20251001"

;; Build manually
(define c (conversation/new {}))
(define c (conversation/add-message c :user "hello"))
(define c (conversation/add-message c :assistant "hi there"))
```

### Structured Extraction

```scheme
(llm/extract
  {:vendor {:type :string}
   :amount {:type :number}
   :date   {:type :string}}
  "I bought coffee for $4.50 at Blue Bottle on Jan 15, 2025")
; => {:amount 4.5 :date "2025-01-15" :vendor "Blue Bottle"}

(llm/classify [:positive :negative :neutral]
              "This product is amazing!")
; => :positive
```

### Tools & Function Calling

Define tools that the LLM can invoke during a conversation:

```scheme
(deftool lookup-capital
  "Look up the capital of a country"
  {:country {:type :string :description "Country name"}}
  (lambda (country)
    (cond
      ((= country "Norway") "Oslo")
      ((= country "France") "Paris")
      (else "Unknown"))))

;; The LLM will call the tool automatically
(llm/chat
  [(message :user "What is the capital of Norway?")]
  {:tools [lookup-capital] :max-tokens 100})

;; Inspect tools
(tool/name lookup-capital)              ; => "lookup-capital"
(tool/description lookup-capital)       ; => "Look up the capital..."
(tool/parameters lookup-capital)        ; => {:country {:type :string ...}}
```

### Agents

Agents combine a system prompt, tools, and a multi-turn loop:

```scheme
(deftool get-weather
  "Get weather for a city"
  {:city {:type :string}}
  (lambda (city)
    (format "~a: 22°C, sunny" city)))

(defagent weather-bot
  {:system "You are a weather assistant. Use the get-weather tool."
   :tools [get-weather]
   :model "claude-haiku-4-5-20251001"
   :max-turns 3})

(agent/run weather-bot "What's the weather in Tokyo?")

;; Inspect agents
(agent/name weather-bot)                ; => "weather-bot"
(agent/system weather-bot)              ; => "You are a weather assistant..."
(agent/tools weather-bot)               ; => list of tool values
(agent/model weather-bot)               ; => "claude-haiku-4-5-20251001"
(agent/max-turns weather-bot)           ; => 3
```

### Embeddings & Similarity

```scheme
;; Configure an embedding provider
;; (auto-configured from JINA_API_KEY, VOYAGE_API_KEY, or COHERE_API_KEY)

(define v1 (llm/embed "hello world"))
(define v2 (llm/embed "hi there"))
(llm/similarity v1 v2)                 ; => 0.87 (cosine similarity)

;; Batch embeddings
(llm/embed ["cat" "dog" "fish"])       ; => list of vectors
```

### Provider Management

```scheme
(llm/auto-configure)                   ; auto-detect from env vars
(llm/configure :anthropic {:api-key "sk-..."})  ; manual setup

;; Runtime provider switching
(llm/list-providers)                   ; => (:anthropic :gemini :openai ...)
(llm/current-provider)                 ; => {:name :anthropic :model "claude-sonnet-4-20250514"}
(llm/set-default :openai)              ; switch active provider

;; Explicit provider config with options
(llm/configure :ollama {:host "http://localhost:11434"
                         :default-model "llama3"})
```

### Cost Tracking & Budgets

```scheme
(llm/last-usage)                       ; => {:prompt-tokens 42 :completion-tokens 15 ...}
(llm/session-usage)                    ; => cumulative usage across all calls
(llm/reset-usage)                      ; reset session counters

;; Budget enforcement
(llm/set-budget 1.00)                  ; set $1.00 spending limit
(llm/budget-remaining)                 ; => {:limit 1.0 :spent 0.05 :remaining 0.95}
(llm/clear-budget)                     ; remove limit

;; Custom pricing for unlisted models
(llm/set-pricing "my-model" 1.0 3.0)  ; input/output per million tokens
```

### Batch & Parallel

```scheme
;; Send multiple prompts concurrently
(llm/batch ["Translate 'hello' to French"
            "Translate 'hello' to Spanish"
            "Translate 'hello' to German"])

;; Map a function over items, sending all prompts in parallel
(llm/pmap
  (fn (word) (format "Define: ~a" word))
  '("serendipity" "ephemeral" "ubiquitous")
  {:max-tokens 50})
```

### Supported Providers

All providers are auto-configured from environment variables. Use `(llm/configure :provider {...})` for manual setup.

| Provider          | Type           | Chat | Stream | Tools | Embeddings |
| ----------------- | -------------- | ---- | ------ | ----- | ---------- |
| **Anthropic**     | Native         | ✅   | ✅     | ✅    | —          |
| **OpenAI**        | Native         | ✅   | ✅     | ✅    | ✅         |
| **Google Gemini** | Native         | ✅   | ✅     | ✅    | —          |
| **Ollama**        | Native (local) | ✅   | ✅     | ✅    | —          |
| **Groq**          | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **xAI**           | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **Mistral**       | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **Moonshot**      | OpenAI-compat  | ✅   | ✅     | ✅    | —          |
| **Jina**          | Embedding-only | —    | —      | —     | ✅         |
| **Voyage**        | Embedding-only | —    | —      | —     | ✅         |
| **Cohere**        | Embedding-only | —    | —      | —     | ✅         |

> **Note:** Legacy hyphenated accessor names (`tool-name`, `agent-system`, `prompt-messages`, `message-role`, etc.) still work as aliases for the slash-namespaced forms.

---

## CLI Reference

```
sema [OPTIONS] [FILE] [-- SCRIPT_ARGS...]
```

| Flag                 | Description                                  |
| -------------------- | -------------------------------------------- |
| `-e, --eval <EXPR>`  | Evaluate expression, print result if non-nil |
| `-p, --print <EXPR>` | Evaluate expression, always print result     |
| `-l, --load <FILE>`  | Load file(s) before executing (repeatable)   |
| `-q, --quiet`        | Suppress REPL banner                         |
| `-i, --interactive`  | Enter REPL after running file or eval        |
| `--no-init`          | Skip LLM auto-configuration                  |
| `--no-llm`           | Disable LLM features (same as `--no-init`)   |
| `--model <NAME>`     | Set default LLM model                        |
| `--provider <NAME>`  | Set LLM provider                             |
| `-V, --version`      | Print version                                |
| `-h, --help`         | Print help                                   |

### Examples

```bash
# Load a prelude before starting the REPL
sema -l prelude.sema

# Load helpers, then run a script
sema -l helpers.sema script.sema

# Run a script and drop into REPL to inspect state
sema -i script.sema

# Quick one-liner for shell pipelines
sema -p '(string/join (map str (range 10)) ",")'

# Run without LLM features (faster startup)
sema --no-llm script.sema

# Use a specific model
sema --model claude-haiku-4-5-20251001 -e '(llm/complete "Hello!")'

# Shebang support in scripts
#!/usr/bin/env sema
```

### Environment Variables

| Variable             | Description                                           |
| -------------------- | ----------------------------------------------------- |
| `ANTHROPIC_API_KEY`  | Anthropic API key (auto-detected)                     |
| `OPENAI_API_KEY`     | OpenAI API key (auto-detected)                        |
| `GROQ_API_KEY`       | Groq API key (auto-detected)                          |
| `XAI_API_KEY`        | xAI/Grok API key (auto-detected)                      |
| `MISTRAL_API_KEY`    | Mistral API key (auto-detected)                       |
| `MOONSHOT_API_KEY`   | Moonshot API key (auto-detected)                      |
| `GOOGLE_API_KEY`     | Google Gemini API key (auto-detected)                 |
| `OLLAMA_HOST`        | Ollama server URL (default: `http://localhost:11434`) |
| `JINA_API_KEY`       | Jina embeddings API key (auto-detected)               |
| `VOYAGE_API_KEY`     | Voyage embeddings API key (auto-detected)             |
| `COHERE_API_KEY`     | Cohere embeddings API key (auto-detected)             |
| `SEMA_DEFAULT_MODEL` | Default model name                                    |
| `SEMA_LLM_PROVIDER`  | Preferred provider                                    |

### REPL Commands

| Command        | Description                |
| -------------- | -------------------------- |
| `,quit` / `,q` | Exit the REPL              |
| `,help` / `,h` | Show help                  |
| `,env`         | Show user-defined bindings |

---

## Crate Structure

```
crates/
  sema-core/     Value types, errors, environment (Rc, BTreeMap, thiserror)
  sema-reader/   Hand-written lexer and s-expression parser
  sema-eval/     Trampoline-based evaluator, special forms, module system
  sema-stdlib/   Standard library (~200 builtins across 15 modules)
  sema-llm/      LLM provider trait + API clients (Anthropic, OpenAI, Gemini, Ollama)
  sema/          Binary: REPL (rustyline) and file runner (clap)
```

Dependency flow: `sema-core` ← `sema-reader` ← `sema-eval` ← `sema-stdlib` / `sema-llm` ← `sema`

## License

MIT — see [LICENSE](LICENSE).
