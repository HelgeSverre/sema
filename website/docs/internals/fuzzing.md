# Fuzzing the VM

Sema is fuzzed two ways: byte-level fuzzers that hammer the parser frontend, and a **grammar-based fuzzer written in Sema itself** that generates *valid* programs and checks them against correctness oracles. The second is the interesting one — it found a real, shipped VM crash within minutes of being expanded.

## The hard part of fuzzing: the oracle

Generating random input is easy. The hard part is the **oracle** — the judge that decides whether a given input revealed a bug. A crash-only fuzzer has a trivial oracle ("did it panic?") but is blind to the far more common failure mode: code that runs fine and silently returns the *wrong answer*. An oracle is what catches those.

Sema's grammar fuzzer (`fuzz/grammar-fuzz.sema`) leans on **homoiconicity** — a generated program is just an ordinary Sema value — to get two sharp oracles almost for free, plus crash detection:

### 1. Round-trip oracle (printer ⇄ reader)

```scheme
(= form (read (str form)))
```

Generate arbitrary valid s-expression *data* (atoms of every kind, nested lists, vectors, maps), print it, read it back, and assert structural equality. Any asymmetry between the printer and the reader falls straight out.

### 2. Differential value oracle (compiler/VM)

For a generated *program*, compute its **expected** value bottom-up *while generating it* — applying the real primitive ops to the already-known sub-values — then `eval` the whole nested form through the full `macro-expand → lower → optimize → compile → bytecode-VM` pipeline and compare:

```scheme
(= expected (eval form))
```

The expected value is the oracle. Because it's computed by straight-line, bottom-up evaluation while the form is run through the optimizing compiler and VM, a mismatch means the **compiler/optimizer/VM disagrees with the obvious answer** — constant folding, `if`/`let` lowering, closure capture, TCO, short-circuit logic, stack management, and so on.

### 3. Crash detection

Release builds are `panic=abort`, so a VM panic kills the process. The driver (`scripts/grammar-fuzz.sh`) writes the in-flight seed to a breadcrumb file before each iteration, so even a hard abort is reproducible from a single integer.

## How the generator stays sound

Every generated program is **well-typed and closed** — it references only variables it has bound, and each sub-expression has a known type and value. The generator threads an environment of `(symbol value type)` triples so every variable reference is in scope and type-correct, and it computes each form's expected value as it builds it. Types covered: `int`, `bool`, `float`, `string`, `list`, `vector`, `map`.

Everything is driven by a small, self-contained, seedable PRNG, so **every finding reproduces from one integer**. Iteration `i` uses seed `base + i`, re-seeding each time:

```
SEMA_FUZZ_SEED=<seed> SEMA_FUZZ_COUNT=1   # reproduce a single finding
```

### What it covers

Arithmetic (`+ - *` incl. variadic, `min`/`max`, `mod`, `abs`, unary `-`, `expt`), bitwise ops (`bit/and|or|xor`, shifts, `bit/not`), all comparisons, numeric and type predicates (`even?`/`zero?`/…, `string?`/`list?`/`map?`/`vector?`/`bool?`/`nil?`), `and`/`or`/`not`, `if`, `cond`, `case`, `match` (including binding clauses), multi-binding mixed-type `let`, multi-arg and curried lambdas, `set!`, `try`/`throw`/`catch`, `apply`, named-let TCO recursion at large N, and a broad set of list/vector/map/string operations (`map`/`filter`/`foldl`/`reverse`/`append`/`cons`/`range`/`take`/`drop`/`sort`/`nth`/`length`/`last`, `assoc`/`dissoc`/`get`/`count`/`contains?`/`merge`/`keys`/`vals`, `string-append`/`substring`/`upcase`/`downcase`/`string/repeat`/`number->string`/…).

**Excluded by design:** anything non-deterministic — LLM calls, time, randomness, `uuid`, file/network I/O, async timing — because it has no stable oracle.

## Running it

```bash
make fuzz-grammar                          # default sweep (random seed)
make fuzz-grammar SEED=123 N=20000 DEPTH=6 # pinned, larger, deeper
make fuzz-grammar-emit                     # print sample generated programs
```

Exit status: `0` all clear, `1` a deterministic value/round-trip mismatch (the program prints the offending form, expected, actual, and the reproducing seed), `2` a hard crash (the driver prints the reproducing seed).

## Case study: a real bug it found

Expanding the grammar to cover `try`/`catch` immediately produced a crash. Minimized, the trigger was:

```scheme
(let ((a 1) (b (try (throw 1) (catch e 2)))) b)   ; aborted instead of returning 2
```

A throwing `try`/`catch` used as a **non-first binding in a parallel `let`** corrupted the operand stack. The compiler pushed all binding inits onto the operand stack before storing them but didn't track the stack height for those pushes, so the exception handler restored the stack *below* the earlier already-pushed bindings; the subsequent stores and local-slot reads then went out of bounds. (`let*`, `letrec`, and function calls tracked the height correctly and were unaffected.)

The fix was a few lines in `compile_let`. Afterward, **715,000 generated programs up to depth 9 ran with zero crashes and zero value-oracle mismatches.** The bug had been shipped; the fuzzer caught it the moment its grammar reached the relevant corner.

## Extending the grammar

To teach the fuzzer a new construct, add a production to the generator for the result type (`gen-int`, `gen-bool`, `gen-flt`, `gen-str`, `gen-ilist`, `gen-vec`, `gen-map-v`). The rule: build the form *and* compute its expected value with the real operation, so the oracle stays sound. The only hard constraint is determinism — if a construct's result can't be predicted while generating it, it has no oracle and doesn't belong here.
