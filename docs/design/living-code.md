# Living Code: Self-Aware Programs in Sema

> Design document for Sema's introspection, doctest, and LLM-driven metaprogramming features.

## Vision

Sema is uniquely positioned at the intersection of three capabilities no other language combines:

1. **Homoiconicity** — code is data, data is code (`read` / `eval` / `quote`)
2. **LLM as a language primitive** — `llm/complete`, `llm/chat`, `llm/extract` are built-in
3. **Runtime mutability** — functions can be redefined at runtime via `define`

This document specifies six features that exploit this combination, ordered from foundational to insane. Each later feature builds on the ones before it.

---

## Table of Contents

- [Layer 0: Foundation — Docstrings & Meta](#layer-0-foundation--docstrings--meta)
- [Layer 1: Doctests — Executable Examples](#layer-1-doctests--executable-examples)
- [Layer 2: `read-source` — The Language Reads Itself](#layer-2-read-source--the-language-reads-itself)
- [Layer 3: `ask` — Conversational Introspection](#layer-3-ask--conversational-introspection)
- [Layer 4: `self-heal` — LLM-Driven Auto-Repair](#layer-4-self-heal--llm-driven-auto-repair)
- [Layer 5: `evolve` — Genetic Programming](#layer-5-evolve--genetic-programming)
- [Layer 6: `become` — Runtime Self-Modification](#layer-6-become--runtime-self-modification)
- [Implementation Roadmap](#implementation-roadmap)

---

## Layer 0: Foundation — Docstrings & Meta

### What

Docstrings become the optional second argument to `defn` and `define`. They are stored as metadata on the binding, queryable at runtime.

### Syntax

```lisp
;; Docstring is an optional string before the param list
(defn fibonacci
  "Returns the nth Fibonacci number.
   n must be a non-negative integer."
  (n)
  (if (< n 2) n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

;; Also works with define + lambda shorthand
(define (factorial n)
  "Computes n factorial recursively."
  (if (<= n 1) 1 (* n (factorial (- n 1)))))
```

### Runtime API

```lisp
(doc fibonacci)
;; Prints:
;;   fibonacci : (fn n)
;;   Returns the nth Fibonacci number.
;;   n must be a non-negative integer.

(meta fibonacci)
;; => {:name "fibonacci"
;;     :doc "Returns the nth Fibonacci number.\n   n must be a non-negative integer."
;;     :params (n)
;;     :file "math.sema"
;;     :line 3
;;     :source "(defn fibonacci ...)"
;;     :arity 1}

;; Search across all defined symbols
(doc/search "string")
;; => ({:name "string/trim" :doc "..."} {:name "string/split" :doc "..."} ...)

(doc/search :undocumented)
;; => list of all defined functions that have no docstring
```

### Implementation

**Changes required:**

1. **`sema-core/value.rs`** — Add optional `doc: Option<String>` and `source_span: Option<(String, usize)>` (file, line) fields to `Lambda` struct.

2. **`sema-eval/special_forms.rs`** — In `eval_define` and `eval_defun`, detect when `args[1]` (or the position after the name) is a string literal. If so, treat it as the docstring. Store it on the Lambda.

3. **`sema-core/env.rs`** — Add a parallel `metadata: HashMap<Spur, BTreeMap<Value, Value>>` alongside bindings. When a docstring'd function is defined, populate it.

4. **`sema-stdlib`** — New `introspect.rs` module:
   - `(doc name)` — pretty-prints documentation
   - `(meta name)` — returns metadata as a map
   - `(doc/search query)` — searches all bindings by name or doc content
   - `(source-of name)` — returns the source code as a string

**Estimated effort:** Medium. Core plumbing change but narrow scope.

---

## Layer 1: Doctests — Executable Examples

### What

Lines starting with `>>>` inside docstrings are executable tests. Lines starting with `!!` are expected errors. Run them with `sema test --doctests`.

### Syntax

```lisp
(defn string/trim
  "Remove leading and trailing whitespace.

   >>> (string/trim \"  hello  \")
   \"hello\"

   >>> (string/trim \"\")
   \"\"

   >>> (string/trim 42)
   !! type-error"
  (s)
  (string/replace s #"^\s+|\s+$" ""))
```

### Directive markers

| Marker | Meaning |
|--------|---------|
| `>>>` | Evaluate this expression |
| Next line (no marker) | Expected result (compared via `equal?`) |
| `!! error-substring` | Expect an error containing this substring |
| `>>> ...` (continuation) | Multi-line expression |
| `;;` inside doctest | Comment, ignored |
| `~>` | Expected output to stdout (instead of return value) |
| `>>>!` | Evaluate but don't check result (setup step) |

### Full example

```lisp
(defn csv/parse
  "Parse a CSV string into a list of lists.

   Basic usage:
   >>> (csv/parse \"a,b,c\n1,2,3\")
   ((\"a\" \"b\" \"c\") (\"1\" \"2\" \"3\"))

   Handles quoted fields:
   >>> (csv/parse \"name,desc\n\\\"Alice\\\",\\\"Hello, world\\\"\")
   ((\"name\" \"desc\") (\"Alice\" \"Hello, world\"))

   Custom delimiter:
   >>> (csv/parse \"a;b;c\" {:delimiter \";\"})
   ((\"a\" \"b\" \"c\"))

   Empty input:
   >>> (csv/parse \"\")
   ()

   Wrong type:
   >>> (csv/parse 42)
   !! type-error"
  (input . opts)
  ...)
```

### CLI

```bash
# Run doctests in a file
$ sema test --doctests src/csv.sema
csv/parse ..................... 5/5 ✓

# Run doctests for ALL loaded modules
$ sema test --doctests

# Run doctests + regular test files
$ sema test --all

# Verbose mode — show each example
$ sema test --doctests -v src/csv.sema
csv/parse:
  ✓ (csv/parse "a,b,c\n1,2,3") => (("a" "b" "c") ("1" "2" "3"))
  ✓ (csv/parse "name,desc\n...") => (("name" "desc") ("Alice" "Hello, world"))
  ✓ (csv/parse "a;b;c" {:delimiter ";"}) => (("a" "b" "c"))
  ✓ (csv/parse "") => ()
  ✓ (csv/parse 42) => !! type-error
```

### Implementation

1. **Doctest parser** — New module `sema-eval/src/doctest.rs`:
   - `parse_doctests(docstring: &str) -> Vec<DocTest>`
   - Each `DocTest` has: `input: String`, `expected: Expected` (Value | Error | Output | Skip)
   - Parser is simple: split on `>>>`, read next non-blank line as expected.

2. **Doctest runner** — Function that takes a `DocTest`, evaluates the input in a fresh child env, captures result/error/stdout, compares with expected.

3. **CLI integration** — `sema test --doctests` flag in `crates/sema/src/main.rs`. Walks source files, reads them, finds `defn` forms with docstrings, extracts and runs doctests.

4. **Dual eval** — Doctests should run through both tree-walker and VM, just like regular tests.

**Estimated effort:** Medium. The doctest parser is simple. The runner is mostly wiring up `eval` with capture.

---

## Layer 2: `read-source` — The Language Reads Itself

### What

`read-source` reads a `.sema` file and returns its contents as structured Sema data — the full AST as lists, plus comments and directives preserved as metadata. This is the foundation for everything above Layer 2.

### Why not just `(read-many (file/read path))`?

That already works for the raw AST! But `read-source` adds:
- **Comment preservation** — comments with directives (`;;@` prefix) are captured
- **Source locations** — each form tagged with file/line
- **Top-level structure** — returns a list of annotated definition records, not raw s-expressions

### API

```lisp
;; Basic: parse a file into a list of forms (this already works today)
(read-many (file/read "math.sema"))
;; => ((defn fibonacci "docstring" (n) ...) (defn factorial ...))

;; Enhanced: structured analysis
(read-source "math.sema")
;; => ({:type :defn
;;      :name fibonacci
;;      :doc "Returns the nth Fibonacci number."
;;      :params (n)
;;      :body ((if (< n 2) n ...))
;;      :line 3
;;      :file "math.sema"
;;      :directives {:since "1.8.0" :see ("factorial")}
;;      :doctests (...)
;;      :source "(defn fibonacci ...full text...)"}
;;     {:type :defn
;;      :name factorial
;;      ...})

;; Glob support
(read-source "src/**/*.sema")
;; => flat list of all definitions across all matching files

;; Query helpers
(defs-in "math.sema")
;; => (fibonacci factorial gcd lcm)

(undocumented-in "src/**/*.sema")
;; => (internal-helper temp-fn ...)
```

### Directive Comments

Comment lines starting with `;;@` are parsed as key-value directives and attached to the next form:

```lisp
;;@since 1.8.0
;;@see string/split, string/join
;;@deprecated Use string/trim instead
;;@tags parsing, text
;;@example
(defn old-trim (s)
  "Trim whitespace."
  (string/trim s))
```

These become the `:directives` map in the `read-source` output. This means you can build **custom linters, analyzers, migration tools, changelog generators** — all in Sema.

### Power moves — things you can build with this

```lisp
;; Find all deprecated functions across the entire codebase
(->> (read-source "src/**/*.sema")
     (filter #(get-in % [:directives :deprecated]))
     (map #(println f"⚠️  ${(:name %)} — ${(get-in % [:directives :deprecated])}")))

;; Generate a CHANGELOG from @since directives
(->> (read-source "src/**/*.sema")
     (filter #(get-in % [:directives :since]))
     (group-by #(get-in % [:directives :since]))
     (map (fn ((version fns))
            (println f"\n## ${version}")
            (each (fn (f) (println f"- ${(:name f)}: ${(:doc f)}")) fns))))

;; Auto-generate a stdlib reference page
(->> (read-source "stdlib/*.sema")
     (filter #(= (:type %) :defn))
     (sort-by :name)
     (map (fn (f)
            (println f"### `${(:name f)}`\n\n${(:doc f)}\n"))))
```

### Implementation

1. **Enhanced reader** — `sema-reader` already strips comments. Add an optional `ReadOptions { preserve_directives: bool }` mode that captures `;;@` lines and attaches them to the next form as metadata.

2. **`read-source` stdlib function** — In `sema-stdlib/src/io.rs`:
   - Read file(s) via glob
   - Parse with enhanced reader
   - Walk top-level forms, extract `defn`/`define`/`defmacro` structure
   - Return as a list of maps

3. **Directives parser** — Simple: `;;@key value` → `:key "value"` in a map. Multi-line directives use continuation indent.

**Estimated effort:** Medium-high. The reader needs a new mode for directive preservation.

---

## Layer 3: `ask` — Conversational Introspection

### What

`(ask target question)` lets you ask natural-language questions about any value, function, module, or codebase. It combines `meta`, `source-of`, `read-source`, and `llm/chat` into a single introspection primitive.

### Usage

```lisp
;; Ask about a function
(ask fibonacci "will this blow the stack for large inputs?")
;; => "Yes. This is a naive recursive implementation with O(2^n) time
;;     complexity and O(n) stack depth. For n > ~30 it'll be slow,
;;     and for n > ~10000 you'll hit a stack overflow. Consider
;;     memoization or an iterative approach:
;;
;;     (defn fibonacci (n)
;;       (loop ((a 0) (b 1) (i n))
;;         (if (= i 0) a (recur b (+ a b) (- i 1)))))"

;; Ask about a module
(ask "src/parser.sema" "what error recovery strategy does this use?")

;; Ask about a value
(def data (json/parse (file/read "config.json")))
(ask data "is there anything that looks like a hardcoded secret in here?")

;; Ask about a running program's environment
(ask *env* "what functions here could have side effects?")
```

### How it works internally

```
(ask target question)
        │
        ├── target is a function?
        │   └── collect: (meta target), (source-of target), docstring, doctests
        │
        ├── target is a string ending in .sema?
        │   └── collect: (read-source target) — all definitions + docs
        │
        ├── target is a map/list/value?
        │   └── collect: pretty-printed value (truncated to ~4k chars)
        │
        └── target is *env*?
            └── collect: all bound names + their types/arities
        
        All collected context → LLM system prompt + user question
        
        System: "You are a Sema language expert. You have been given the 
                 source code and metadata for [target]. Answer the user's
                 question about it. If suggesting code changes, write valid
                 Sema. Be concise."
        
        User: [collected context] + "\n\nQuestion: " + question
```

### Advanced: ask with action

```lisp
;; Ask and get back executable code, not just text
(ask/code fibonacci "make this iterative")
;; => (defn fibonacci (n)
;;      (loop ((a 0) (b 1) (i n))
;;        (if (= i 0) a (recur b (+ a b) (- i 1)))))
;; Returns a Sema VALUE (list), not a string. You can eval it.

;; Ask and apply
(ask/patch! fibonacci "add memoization")
;; => Redefines fibonacci in place with memoized version.
;;    Prints diff of old vs new.
;;    Runs doctests if any. Reports results.
```

### Implementation

1. **`ask` function** — In `sema-llm/src/builtins.rs` or new `sema-llm/src/ask.rs`:
   - Dispatcher that detects target type
   - Context gatherer that builds the prompt
   - Calls `llm/chat` with structured system prompt
   - Returns the response as a string

2. **`ask/code`** — Same as `ask` but system prompt requests "respond with ONLY a valid Sema expression, no markdown, no explanation". Parse response with `read`.

3. **`ask/patch!`** — `ask/code` + `eval` + doctest runner. Transactional: if doctests fail, rollback.

**Depends on:** Layer 0 (meta/doc), Layer 2 (read-source)

**Estimated effort:** Medium. Mostly prompt engineering + dispatch logic. The LLM infra already exists.

---

## Layer 4: `self-heal` — LLM-Driven Auto-Repair

### What

When a function has doctests and one or more fail, `self-heal` asks the LLM to fix the implementation to pass all doctests. The doctests are the **specification**. The LLM writes the code. The language verifies it.

### Usage

```lisp
;;@self-heal
(defn parse-date
  "Parse various date formats into a map.
   >>> (parse-date \"2024-01-15\")
   {:year 2024 :month 1 :day 15}
   >>> (parse-date \"Jan 15, 2024\")
   {:year 2024 :month 1 :day 15}
   >>> (parse-date \"15/01/2024\")
   {:year 2024 :month 1 :day 15}"
  (s)
  ;; Initial naive implementation — only handles ISO format
  (let ((parts (string/split s "-")))
    {:year (parse-int (nth parts 0))
     :month (parse-int (nth parts 1))
     :day (parse-int (nth parts 2))}))
```

### CLI

```bash
$ sema test --heal src/dates.sema
parse-date:
  ✓ (parse-date "2024-01-15") => {:year 2024 :month 1 :day 15}
  ✗ (parse-date "Jan 15, 2024") => ERROR: parse-int: invalid "Jan"
  ✗ (parse-date "15/01/2024") => {:year 15 :month 1 :day 2024}  (wrong)
  
  🔧 2 failing doctests. Healing...
  
  Attempt 1:
    LLM generated candidate (247 tokens)
    Running doctests... 3/3 ✓
  
  ✅ Healed! New implementation:
  
  (defn parse-date (s)
    (cond
      ((string/matches? s #"\d{4}-\d{2}-\d{2}")
       (let ((parts (string/split s "-")))
         {:year (parse-int (nth parts 0))
          :month (parse-int (nth parts 1))
          :day (parse-int (nth parts 2))}))
      ((string/matches? s #"[A-Z][a-z]+ \d+, \d+")
       (let ((parts (string/split s #"[\s,]+")))
         {:year (parse-int (nth parts 2))
          :month (month-number (nth parts 0))
          :day (parse-int (nth parts 1))}))
      ((string/matches? s #"\d{2}/\d{2}/\d{4}")
       (let ((parts (string/split s "/")))
         {:year (parse-int (nth parts 2))
          :month (parse-int (nth parts 1))
          :day (parse-int (nth parts 0))}))))
  
  Write to file? [y/n]
```

### Programmatic API

```lisp
;; Heal a specific function
(heal! parse-date)
;; => {:status :healed :attempts 1 :tokens-used 247}

;; Heal with constraints
(heal! parse-date {:max-attempts 5
                   :model "claude-sonnet"
                   :keep-structure true})  ; try to minimally edit, not rewrite

;; Heal all @self-heal functions in a module
(heal-module! "src/dates.sema")

;; Dry run — show what would change, don't apply
(heal? parse-date)
;; => {:would-change true :candidate "(defn ...)" :passing 3 :total 3}
```

### The Healing Loop

```
1. Extract doctests from the function's docstring
2. Run all doctests, collect failures
3. If all pass → done
4. Build prompt:
   - System: "You are writing Sema code. Fix this function to pass all tests."
   - Context: function source, docstring, ALL doctests with pass/fail status,
              error messages for failures, available stdlib functions
   - Constraint: "Return ONLY the fixed (defn ...) form. No explanation."
5. Parse LLM response with (read ...)
6. Eval the candidate in a sandboxed child environment
7. Run doctests against the candidate
8. If all pass → offer to replace (or auto-replace if @self-heal)
9. If not → retry with failure info appended (up to max-attempts)
```

### Safety

- **Sandbox** — candidates are eval'd in a child env with restricted I/O (no file write, no network, no exec)
- **Rollback** — original definition is saved; if healing fails, nothing changes
- **Max attempts** — default 3, configurable
- **Human approval** — CLI mode asks before writing. Programmatic mode returns the candidate for inspection.
- **Diff display** — always shows what changed before applying

### Implementation

1. **Doctest runner from Layer 1** — reused
2. **Healing loop** — new module, ~200 lines of orchestration
3. **Sandbox** — Sema already has `Sandbox` with capability flags; create a restricted one for healing
4. **CLI flag** — `--heal` on `sema test`

**Depends on:** Layer 0 (docstrings), Layer 1 (doctests)

**Estimated effort:** Medium. Most of the pieces exist. The hard part is prompt engineering for reliable code generation.

---

## Layer 5: `evolve` — Genetic Programming

### What

Given a specification (doctests or a fitness function), generate a population of candidate implementations using the LLM, test them, and iteratively breed/mutate the best ones to produce an optimized solution.

### Usage

```lisp
(def best-sort
  (evolve
    :name "fast-sort"
    :spec '((>>> (fast-sort '(3 1 4 1 5 9)) (1 1 3 4 5 9))
            (>>> (fast-sort '()) ())
            (>>> (fast-sort '(1)) (1))
            (>>> (fast-sort '(5 4 3 2 1)) (1 2 3 4 5))
            (>>> (fast-sort (range 100 0 -1)) (range 1 101)))
    :population 10
    :generations 5
    :fitness (fn (candidate spec-results)
               ;; spec-results is {:passed n :total n :time-ms n}
               (if (< (:passed spec-results) (:total spec-results))
                 0  ; must pass all tests to be viable
                 (/ 1000.0 (max 1 (:time-ms spec-results)))))
    :seed-prompt "Write a sort function in Sema. Be creative — try 
                  quicksort, mergesort, radix sort, or something unusual."
    :verbose true))

;; Output:
;; Generation 1: 10 candidates, 7 viable, best fitness: 142.8
;;   #1 quicksort    — 7.0ms  all tests pass
;;   #2 mergesort    — 8.2ms  all tests pass
;;   #3 insertion    — 45ms   all tests pass
;;   ...
;; Generation 2: 10 candidates (3 new, 7 mutated), 9 viable, best: 312.5
;;   #1 quicksort-v2 — 3.2ms  (mutated from gen1 #1)
;;   ...
;; Generation 5: best fitness: 500.0
;;   Winner: hybrid-sort (quicksort with insertion sort for small partitions)
;;   Time: 2.0ms on 100 elements

(best-sort '(42 7 13 1))
;; => (1 7 13 42)
```

### The Evolution Loop

```
Generation 0 (seeding):
  1. Send seed-prompt to LLM N times with temperature=1.0
  2. Parse each response, eval in sandbox
  3. Run spec doctests + fitness function
  4. Rank by fitness

Generation 1..G (evolution):
  1. Keep top 30% as elites (survive unchanged)
  2. For remaining 70% slots:
     a. Pick two parents weighted by fitness
     b. Send to LLM: "Here are two Sema functions that solve the same problem.
        Create a new function that combines the best ideas from both.
        Parent A (fitness X): [source]
        Parent B (fitness Y): [source]"
     c. With 20% probability, instead mutate a single parent:
        "Here is a Sema function. Modify it to be faster/better.
        Current performance: [metrics]
        [source]"
  3. Eval all candidates, run tests, compute fitness
  4. Repeat

Final: return the highest-fitness candidate as a lambda
```

### Fitness function API

```lisp
;; The fitness function receives:
;;   candidate — the function value (callable)
;;   spec-results — {:passed N :total N :time-ms N :error-count N}
;; Returns a number (higher = better)

;; Optimize for speed (must be correct)
:fitness (fn (f results)
           (if (< (:passed results) (:total results)) 0
               (/ 1000.0 (:time-ms results))))

;; Optimize for code size (must be correct)
:fitness (fn (f results)
           (if (< (:passed results) (:total results)) 0
               (/ 1000.0 (string/length (source-of f)))))

;; Multi-objective
:fitness (fn (f results)
           (if (< (:passed results) (:total results)) 0
               (+ (* 0.7 (/ 1000.0 (:time-ms results)))
                  (* 0.3 (/ 100.0 (string/length (source-of f)))))))
```

### Implementation

1. **`evolve` function** — new module in `sema-stdlib` or `sema-llm`:
   - Population manager
   - Tournament selection
   - LLM-based crossover and mutation (prompt templates)
   - Sandboxed eval + benchmarking
   - Generation loop with reporting

2. **Benchmarking harness** — `(bench fn iterations)` already exists via `time`; formalize it.

3. **Source extraction** — from Layer 0 `source-of`

**Depends on:** Layer 0 (meta/source), Layer 1 (doctests for spec validation)

**Estimated effort:** High. This is a real system. But each piece is simple — the complexity is orchestration.

---

## Layer 6: `become` — Runtime Self-Modification

### What

A function can replace its own definition at runtime. Combined with observation and LLM analysis, this enables programs that **adapt while running**.

### ⚠️ Danger Level: Maximum

This is the most powerful and most dangerous feature. It must be opt-in, auditable, and constrained.

### Usage

```lisp
;;@adaptive
(defn smart-memoize
  "A cache that starts with LRU eviction and evolves its strategy
   based on observed access patterns.
   
   >>> (smart-memoize :get \"a\")
   nil
   >>> (smart-memoize :set \"a\" 42)
   42
   >>> (smart-memoize :get \"a\")
   42"
  (op key . args)
  ;; Initial implementation: simple hashmap cache, no eviction
  (let ((cache (or (resolve 'smart-memoize/state) {})))
    (match op
      (:get (get cache key))
      (:set (let ((v (first args)))
              (define smart-memoize/state (assoc cache key v))
              v))
      (:stats {:size (count cache) :strategy "none"}))))
```

```lisp
;; After 1000 calls, analyze and evolve:
(observe! smart-memoize 1000
  (fn (call-log)
    (let* ((analysis (llm/extract
                       (format "Analyze this cache access pattern: ~a" call-log)
                       {:hot-keys "list of frequently accessed keys"
                        :pattern "temporal or frequency-based?"
                        :recommendation "eviction strategy"}))
           (new-impl (ask/code smart-memoize
                       (format "Optimize this cache with ~a eviction. 
                                Hot keys are: ~a"
                               (:recommendation analysis)
                               (:hot-keys analysis)))))
      ;; Validate the new implementation passes doctests
      (when (doctests-pass? new-impl)
        (become! smart-memoize new-impl)
        (log/info f"Cache evolved to ${(:recommendation analysis)} strategy")))))
```

### `become!` semantics

```lisp
(become! target new-definition)
```

1. `target` — symbol naming the function to replace
2. `new-definition` — a `defn` form (as data) or a lambda
3. **Pre-conditions checked:**
   - Target must be marked `;;@adaptive`
   - New definition must pass all doctests of the original
   - New definition must have same arity (or be a superset)
4. **On success:**
   - Old definition saved to `(history target)` — a list of all previous versions
   - New definition installed
   - Timestamp + reason logged
5. **On failure:**
   - Error returned, nothing changed
   - Failure reason available for retry logic

### `observe!` semantics

```lisp
(observe! target sample-size callback)
```

1. Wraps `target` with a transparent logger that records all calls
2. After `sample-size` calls, invokes `callback` with the call log
3. Call log format: `({:args (...) :result ... :time-ms N :timestamp T} ...)`
4. Logger is removed after callback completes

### History and Audit

```lisp
(history smart-memoize)
;; => ({:version 1 :timestamp "2024-01-15T10:30:00" :source "(defn ...)" :reason "initial"}
;;     {:version 2 :timestamp "2024-01-15T14:22:00" :source "(defn ...)" :reason "evolved to LFU"}
;;     {:version 3 :timestamp "2024-01-16T09:15:00" :source "(defn ...)" :reason "evolved to ARC"})

;; Rollback
(rollback! smart-memoize 1)
;; => Restores version 1

;; Compare versions
(diff smart-memoize 1 3)
;; => Shows textual diff between version 1 and version 3
```

### Safety Constraints

1. **Opt-in only** — function must have `;;@adaptive` directive. `become!` on non-adaptive functions is an error.
2. **Doctest gate** — new code MUST pass all existing doctests. No exceptions.
3. **Sandbox** — LLM-generated candidates are eval'd in a restricted env before promotion.
4. **Rate limit** — max 1 `become!` per function per minute (configurable).
5. **Audit log** — every mutation is logged with timestamp, diff, and reason.
6. **Rollback** — full history preserved, instant rollback to any version.
7. **Kill switch** — `(freeze! target)` permanently prevents further evolution.

### Implementation

1. **`observe!`** — wrap a function with a logging proxy. Store call log in a thread-local or atom.
2. **`become!`** — validate constraints, run doctests, swap definition in env, save history.
3. **`history`/`rollback!`** — version store per adaptive function (just a list of old Lambdas).
4. **`;;@adaptive` directive** — parsed by Layer 2's directive system.

**Depends on:** Layer 0 (meta), Layer 1 (doctests), Layer 2 (directives), Layer 3 (ask/code)

**Estimated effort:** High. But each primitive is individually simple.

---

## Implementation Roadmap

### Phase 1: Foundation (Layer 0 + 1)
**Priority: Do this first. Everything else builds on it.**

| Task | Crate | Effort |
|------|-------|--------|
| Add `doc` field to `Lambda` struct | sema-core | S |
| Parse docstrings in `defn`/`define` | sema-eval | M |
| `(doc name)` and `(meta name)` functions | sema-stdlib | S |
| `(doc/search query)` | sema-stdlib | S |
| Doctest parser (`>>>` extraction) | sema-eval | M |
| Doctest runner | sema-eval | M |
| `sema test --doctests` CLI flag | sema | S |
| Dual-eval doctest support | sema | M |

### Phase 2: Self-Reading (Layer 2)
**Makes the language programmable from within.**

| Task | Crate | Effort |
|------|-------|--------|
| `;;@directive` parser in reader | sema-reader | M |
| `(read-source path)` function | sema-stdlib | M |
| Glob support for read-source | sema-stdlib | S |
| `(source-of fn)` — return source text | sema-core + sema-eval | M |

### Phase 3: LLM-Powered Introspection (Layer 3 + 4)
**Where it gets spicy.**

| Task | Crate | Effort |
|------|-------|--------|
| `(ask target question)` | sema-llm | M |
| `(ask/code target instruction)` | sema-llm | M |
| `(ask/patch! target instruction)` | sema-llm | M |
| `(heal! fn)` healing loop | sema-llm | M |
| `sema test --heal` CLI flag | sema | S |
| Sandbox for candidate eval | sema-eval | S (reuse existing) |

### Phase 4: The Wild Stuff (Layer 5 + 6)
**Ship when 0–4 are solid.**

| Task | Crate | Effort |
|------|-------|--------|
| `(evolve ...)` genetic programming loop | sema-llm | H |
| `(bench fn iterations)` formalized | sema-stdlib | S |
| `(observe! fn n callback)` | sema-stdlib | M |
| `(become! fn new-def)` | sema-eval | M |
| `(history fn)` / `(rollback! fn n)` | sema-eval | M |
| `(freeze! fn)` | sema-eval | S |
| Audit logging | sema-stdlib | S |

### Notation
- **S** = Small (< 100 lines, < 1 day)
- **M** = Medium (100–500 lines, 1–3 days)  
- **H** = High (500+ lines, 3+ days)

---

## Design Principles

1. **Doctests are the spec.** If a function has doctests, they are the source of truth. Every automated modification must pass them.

2. **The language is the toolchain.** No external Rust-level tools needed. `sema doc`, `sema lint`, `sema migrate` are all Sema scripts using `read-source` + `meta` + `llm/*`.

3. **Opt-in danger.** Anything that modifies running code requires explicit directives. Nothing magical happens without consent.

4. **Observable.** Every LLM interaction, every code mutation, every evolution step is logged and inspectable. No black boxes.

5. **Composable.** Each primitive (`doc`, `meta`, `source-of`, `read-source`, `ask`, `heal!`, `evolve`, `become!`) works independently and composes with everything else. They're functions, not frameworks.
