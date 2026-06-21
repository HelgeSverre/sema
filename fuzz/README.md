# Grammar fuzzer (in Sema)

A grammar-based fuzzer for Sema, **written in Sema itself** (`grammar-fuzz.sema`).
It generates random *valid* Sema programs and checks them against correctness
oracles. It complements the byte-level `cargo-fuzz` targets in
`crates/sema-reader/fuzz` and `crates/sema-eval/fuzz`, which mutate raw bytes to
find parser/evaluator panics. This one generates *structured, valid* input, so it
reaches past the parser into the compiler and VM.

## Why generate (instead of mutate)?

Because Sema is homoiconic, a generated program *is* an ordinary Sema value, which
makes two sharp oracles nearly free:

1. **Round-trip — printer vs. reader.**
   `(= form (read (str form)))`. We generate arbitrary valid s-expression *data*
   (nested lists, vectors, maps, and every atom kind: ints, floats, bools, nil,
   strings, symbols, keywords, chars) and assert that printing then re-reading
   yields a structurally equal value. Any asymmetry between Sema's printer and
   reader falls straight out.

2. **Value oracle — compiler/VM vs. straight-line evaluation.**
   `(= expected (eval form))`. We generate *well-typed, closed* integer/boolean
   programs and compute their expected value **bottom-up while generating them**
   (applying the same primitive ops incrementally). Then we `eval` the whole
   nested form, which drives the full `macro-expand → lower → optimize → compile →
   bytecode-VM` pipeline, and compare. A mismatch means the compiled/optimized
   form disagrees with incremental evaluation — i.e. a bug in constant folding,
   `if`/`let` lowering, closure capture, TCO, or short-circuit logic.

3. **Hard crashes — VM panics.** Release builds are `panic = "abort"`, so a VM
   panic kills the process. The driver records each iteration's seed to a
   breadcrumb file *before* running it, so a crash is still reproducible from one
   integer seed.

The typed programs are deliberately a **closed, total** sublanguage (`+ - * min
max abs`, comparisons, `and/or/not`, `if`, `let`, and immediately-applied
`lambda`s over integers/booleans) so they evaluate to a known value instead of
bouncing off type/arity/unbound-variable errors. That keeps the oracle exact and
the programs reaching deep into the VM.

## Running

```bash
make fuzz-grammar                         # default sweep, random seed
make fuzz-grammar SEED=123 N=20000 DEPTH=5
make fuzz-grammar V=1                      # verbose: print passing forms too
make fuzz-grammar-emit N=10                # print sample generated programs
make fuzz-grammar-emit N=50 OUT=/tmp/progs.sema

# or call the driver directly (no rebuild):
./scripts/grammar-fuzz.sh check -n 5000 -d 4
./scripts/grammar-fuzz.sh emit  -n 10 -s 7
```

The driver exits `0` on success, `1` on a deterministic mismatch (round-trip or
value oracle), and `2` on a hard crash. In every failure case it prints the exact
reproduction command.

### Reproducing and minimizing a finding

Iteration *i* uses seed `base + i` and re-seeds the PRNG, so each finding
reproduces from a single seed with count 1:

```bash
# see the offending program:
SEMA_FUZZ_MODE=emit SEMA_FUZZ_SEED=<seed> SEMA_FUZZ_COUNT=1 \
  ./target/release/sema fuzz/grammar-fuzz.sema

# re-run just that case under the checker:
SEMA_FUZZ_SEED=<seed> SEMA_FUZZ_COUNT=1 \
  ./target/release/sema fuzz/grammar-fuzz.sema
```

Lower `SEMA_FUZZ_DEPTH` to shrink the form, then hand-minimize from the emitted
program.

## Configuration (environment variables)

| Variable               | Meaning                                              | Default  |
| ---------------------- | --------------------------------------------------- | -------- |
| `SEMA_FUZZ_SEED`       | base seed                                            | `0`      |
| `SEMA_FUZZ_COUNT`      | iterations / programs                                | `200`    |
| `SEMA_FUZZ_DEPTH`      | max generation depth                                 | `4`      |
| `SEMA_FUZZ_MODE`       | `check` (run oracles) or `emit` (print programs)     | `check`  |
| `SEMA_FUZZ_OUT`        | emit mode: output file (else stdout)                | stdout   |
| `SEMA_FUZZ_CRASH_FILE` | check mode: breadcrumb file for the in-flight seed   | (unset)  |
| `SEMA_FUZZ_VERBOSE`    | `1` to also print passing forms                     | `0`      |

## Extending the grammar

The generator is small and self-contained. To add a production:

- **New typed (evaluable) form** — add a case to `gen-int` (or `gen-bool`) and a
  helper that returns `(mk form value)`, where `form` is the generated code and
  `value` is its result computed from the children's values. Keep it **total**
  (no errors for any input) so the oracle stays exact — e.g. guard division, or
  skip partial ops. Bump the `(rng-int! N)` arms count to include it.
- **New datum kind** (round-trip coverage) — add a case to `gen-atom` or a new
  container to `gen-datum`. Make sure it round-trips (e.g. unique map keys so the
  literal doesn't collapse; avoid string contents the printer can't yet escape —
  see the note on `*str-alpha*`).

## Known limitations / deliberate exclusions

- **String escaping.** The current printer renders strings without escaping `"`
  and `\`, so generated strings draw from a safe alphabet (`*str-alpha*`). This is
  a real, separate printer gap; including those characters would mask structural
  findings. Re-enable them once the printer escapes — that itself becomes a
  round-trip test.
- **No reference interpreter.** The value oracle compares the VM against
  *incremental* evaluation using the same primitives, so it targets the
  compiler/optimizer/VM, not the arithmetic primitives themselves. Bringing in an
  external reference (e.g. another Scheme) would extend it to primitive semantics.
- **Floats** are generated as `k/1000`; they test float printing + shortest
  round-trip parsing, not the full float space.
