# Sema Lisp — Known Limitations & Gaps

Assessed against standard Scheme (R7RS) and practical Lisp expectations.
Status: as of v0.5.0 (record types, bytevectors, char comparison predicates).

---

## ~~Critical — Blocks Practical Programming~~ (RESOLVED in Phase 6)

### ~~1. No Error Handling~~ → RESOLVED

Implemented `try`/`catch`/`throw` in Phase 6.

### ~~2. No Module System~~ → RESOLVED

Implemented `module`/`import` in Phase 6.

---

## ~~High — Significantly Limits Usability~~ (RESOLVED in Phase 6+7)

### ~~3. No Named `let`~~ → RESOLVED (Phase 6)

### ~~4. No `letrec`~~ → RESOLVED (Phase 6)

### ~~5. No `eval` / `read` / `string->symbol`~~ → RESOLVED (Phase 7)

`eval` special form, `read`/`read-many` builtins, `string->symbol`, `symbol->string`, `string->keyword`, `keyword->string`.

### ~~6. No `gensym` or `macroexpand`~~ → RESOLVED (Phase 7)

`gensym` with optional prefix, `macroexpand` for single-step expansion.

### ~~7. No Regular Expressions~~ → RESOLVED (Phase 7)

Full regex suite: `regex/match?`, `regex/match`, `regex/find-all`, `regex/replace`, `regex/replace-all`, `regex/split`.

### ~~8. Limited List Operations~~ → RESOLVED (Phase 6)

---

## Medium — Nice to Have

### ~~9. `do` Aliased to `begin`~~ → RESOLVED

Proper Scheme `do` loop implemented. `begin` remains for sequencing.

```scheme
(do ((i 0 (+ i 1)) (sum 0 (+ sum i)))
    ((= i 10) sum))  ; => 45
```

### ~~10. No `case` / Pattern Matching~~ → RESOLVED (Phase 7)

`case` special form with R5RS semantics. Pattern matching still absent.

### ~~11. No Port-Based I/O~~ → PARTIALLY RESOLVED (Phase 7)

Full file namespace: `file/read`, `file/write`, `file/append`, `file/delete`, `file/rename`, `file/list`, `file/mkdir`, `file/info`, `file/exists?`, `file/is-directory?`, `file/is-file?`, `file/is-symlink?`.
Path ops: `path/join`, `path/dirname`, `path/basename`, `path/extension`, `path/absolute`.
HTTP: `http/get`, `http/post`, `http/put`, `http/delete`, `http/request`.
Still no streaming/port-based I/O.

### ~~12. No Struct/Record Types~~ → RESOLVED

R7RS `define-record-type` with constructors, type predicates, and field accessors. `record?` predicate. `type` returns record type name as keyword.

### ~~13. No Stack Traces~~ → RESOLVED

Full stack traces with call frames, file locations, and source spans. Traces are bounded for TCO'd recursion. Accessible via `:stack-trace` key in `catch` error maps.

### ~~14. Missing Math Functions~~ → RESOLVED (Phase 8)

Full math suite: `math/quotient`, `math/remainder`, `math/gcd`, `math/lcm`, `math/tan`, `math/asin`, `math/acos`, `math/atan`, `math/atan2`, `math/exp`, `math/log10`, `math/log2`, `math/random`, `math/random-int`, `math/clamp`, `math/sign`.
Bitwise: `bit/and`, `bit/or`, `bit/xor`, `bit/not`, `bit/shift-left`, `bit/shift-right`.

### 15. No `guard` (R7RS Style)

We chose `try`/`catch`/`throw` over R7RS `guard`. The error map with `:type` keyword enables pattern matching in the handler via `cond`.

---

## Low — Completeness Only

### 16. No Full Numeric Tower

Only `i64` and `f64`. No rationals (`1/3`), bignums, or complex numbers.

### ~~17. No Char Type~~ → RESOLVED

Full character type: `#\a`, `#\space`, `#\newline` literals. `char?`, `char->integer`, `integer->char`, `char-alphabetic?`, `char-numeric?`, `char-whitespace?`, `char-upper-case?`, `char-lower-case?`, `char-upcase`, `char-downcase`, `char->string`, `string->char`, `string->list`, `list->string`. `string-ref` and `string/chars` now return `Char` values.

### 18. No Continuations

No `call/cc` or `call-with-current-continuation`. The trampoline evaluator cannot capture continuations.

### 19. No Multiple Return Values

No `values` / `call-with-values`.

### 20. No Dynamic Binding

No `dynamic-wind`, `parameterize`, `make-parameter`.

### 21. No Hygienic Macros

Only `defmacro` (Lisp-style). No `syntax-rules` or `syntax-case`.

### 22. No Tail Position in `do` Body

The `do` loop evaluates its body for side effects but does not support tail calls within the body — only the result expressions are in tail position.

### 23. No `string-set!`

Strings are immutable. No in-place character mutation.

### 24. No Proper Tail Recursion in `map`/`filter`

Higher-order list functions use Rust iteration internally, not Scheme-level recursion — correct but not extensible via tail calls.

### ~~25. No `char=?` / `char<?` Comparison Predicates~~ → RESOLVED

Full R7RS character comparison: `char=?`, `char<?`, `char>?`, `char<=?`, `char>=?` and case-insensitive `char-ci=?`, `char-ci<?`, `char-ci>?`, `char-ci<=?`, `char-ci>=?`.

### 26. No `with-exception-handler`

Only `try`/`catch`/`throw`. No R7RS `with-exception-handler` / `raise` / `raise-continuable`.

### 27. No `define-values`

No destructuring bind for multiple values.

### ~~28. No Bytevectors~~ → RESOLVED

`Value::Bytevector` with `#u8(1 2 3)` reader syntax. `make-bytevector`, `bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!` (COW), `bytevector-copy`, `bytevector-append`, `bytevector->list`, `list->bytevector`, `utf8->string`, `string->utf8`, `bytevector?`.

### 29. No `let-values` / `receive`

No destructuring forms for multiple return values (related to #19).

### 30. No Tail Calls Across Mutual Recursion in Stdlib

The stdlib mini-eval (`call_function` in `list.rs`) doesn't support mutual tail calls, so `map`/`filter` callbacks can't mutually recurse with TCO.

---

## Known Backend Bugs (Audit Findings)

### 31. VM `set!` through stdlib HOF callbacks loses the mutation (C1, HIGH)

When a closure captures a let-bound variable and that closure is invoked via a stdlib higher-order function (`map`, `filter`, `for-each`, `sort-by`, `retry`, etc.), `set!` performed inside the closure is **silently dropped on the VM backend**. The tree-walker behaves correctly.

Reproduction:

```
$ sema --tw -e '(let ((c 0)) (map (fn (x) (set! c (+ c x))) (list 1 2 3)) c)'
6
$ sema      -e '(let ((c 0)) (map (fn (x) (set! c (+ c x))) (list 1 2 3)) c)'
0
```

Root cause: the VM uses an **eager-close + dual-write upvalue model**. When a closure is created, captured locals are *copied* into upvalue cells; the parent's slot keeps its own copy. The resolution pass is already Lua-style (`ParentLocal` / `ParentUpvalue`), but the runtime never opens upvalues that point back at a live parent stack slot. When the closure is then handed to a stdlib HOF, that HOF runs the closure via `NativeFn::func` on a *fresh* VM (see Decision #50). The closure's local mutation lands in the fresh VM's upvalue copy, which is discarded when control returns to the caller.

Planned fix: move to an **open-upvalue runtime** (see MEMORY.md "Upvalue model: eager-close with dual-write … Runtime is what needs changing for open upvalues" and the ADR "Move VM upvalues to open-close-on-popframe model" in `agents/DECISIONS.md`). Note that `tail_call_vm_closure` must close upvalues before replacing the frame.

Related symptoms surfaced by the same root cause:

- `(type (fn (x) x))` returns `:lambda` in TW but `:native-fn` in VM (because VM closures wrap as `NativeFn` for stdlib HOF interop).
- Caught error maps in the VM are missing `:stack-trace` (TW includes it).
- Type-error message text for `+` / `-` differs between backends.

Workaround: use tree-walker (`--tw`) for code that relies on `set!`-through-HOF, or refactor to use `foldl` with explicit accumulator threading (no captured mutation).

### 32. Bytecode stack-balance validation gap (C11, HIGH)

The VM's main dispatch loop uses `pop_unchecked` at 90+ call sites (`crates/sema-vm/src/vm.rs`). This is safe **only** because the in-process bytecode compiler is stack-balanced by construction (every emitted sequence pushes/pops by a known delta). The on-disk `.semac` format has no such guarantee: `validate_bytecode` (in `crates/sema-vm/src/serialize.rs`) currently checks magic, version, table bounds, and jump targets, but it does **not** abstract-interpret the instruction stream to verify stack balance.

A hand-crafted (or corrupted) `.semac` file with a leading `Pop`, an unbalanced `Call`, or a missing push before a binary op causes undefined behavior in release builds: `pop_unchecked` reads `stack[len - 1]` after subtracting from an empty `Vec`, calls `set_len(usize::MAX)`, and subsequent pushes/pops corrupt arbitrary memory.

**For now, `.semac` files should be treated as trusted-source-only.** Do not load `.semac` from network/untrusted sources without verification. The planned fix is a stack-depth verifier — see the ADR "Bytecode stack-depth verifier for .semac loading" in `agents/DECISIONS.md`.

---

## Gap Analysis — Remaining Items

| #   | Gap                                      | Priority | Effort    | Notes                                                                        |
| --- | ---------------------------------------- | -------- | --------- | ---------------------------------------------------------------------------- |
| 15  | No `guard` (R7RS)                        | Low      | Low       | `try`/`catch` covers the use case; `guard` is syntactic sugar                |
| 16  | No Full Numeric Tower                    | Low      | High      | Rationals/bignums require `num` crate integration throughout                 |
| 18  | No Continuations                         | Low      | Very High | Requires CPS transform or VM rewrite; trampoline can't capture continuations |
| 19  | No Multiple Return Values                | Low      | Medium    | `values`/`call-with-values` need eval changes                                |
| 20  | No Dynamic Binding                       | Low      | Medium    | `parameterize`/`make-parameter` via thread-local state                       |
| 21  | No Hygienic Macros                       | Medium   | High      | `syntax-rules` requires pattern matcher + template expander                  |
| 22  | No Tail Position in `do` Body            | Low      | Low       | Body is for side effects; result exprs already have TCO                      |
| 23  | No `string-set!`                         | Low      | Low       | Intentional — immutable strings are simpler and safer                        |
| 24  | No Proper Tail Recursion in map/filter   | Low      | Medium    | Stdlib uses Rust iteration; would need eval access                           |
| 26  | No `with-exception-handler`              | Low      | Medium    | `try`/`catch` is sufficient for most use cases                               |
| 27  | No `define-values`                       | Low      | Low       | Rarely needed without multiple return values                                 |
| 29  | No `let-values`/`receive`                | Low      | Low       | Blocked by #19                                                               |
| 30  | No Tail Calls in Stdlib Mutual Recursion | Low      | High      | Architectural: stdlib can't depend on eval                                   |

---

## Recommended Next Implementations

1. **Dynamic binding** (#20) — `make-parameter`/`parameterize` via thread-local storage fits the existing architecture.
2. **Hygienic macros** (#21) — High effort but important for library authors. Consider `syntax-rules` subset first.
3. **Multiple return values** (#19) — `values`/`call-with-values` enables `define-values` and `let-values`.
4. **`guard`** (#15) — Low effort syntactic sugar over `try`/`catch`.

---

## What Works Well

- **Closures** — Properly implemented with lexical scoping
- **Tail Call Optimization** — Trampoline-based, works for direct recursion in `if`/`cond`/`let`/`begin`/`and`/`or`/`when`/`unless` + named `let`
- **Data types** — Int, Float, String, Char, Symbol, Keyword, List, Vector, Map, Record, Bytevector, Bool, Nil, Promise + LLM types
- **Record types** — R7RS `define-record-type` with constructors, predicates, field accessors. `record?`, `type` returns record tag
- **Bytevectors** — `#u8(1 2 3)` literal syntax. `make-bytevector`, `bytevector`, `bytevector-length`, `bytevector-u8-ref`, `bytevector-u8-set!` (COW), `bytevector-copy`, `bytevector-append`, `bytevector->list`, `list->bytevector`, `utf8->string`, `string->utf8`
- **Character comparison** — R7RS `char=?`, `char<?`, `char>?`, `char<=?`, `char>=?` + case-insensitive `char-ci=?` etc.
- **Macros** — `defmacro` with quasiquote/unquote/unquote-splicing (non-hygienic but functional)
- **String operations** — split, trim, replace, contains?, format, str, index-of, chars, repeat, pad-left, pad-right
- **Map operations** — hash-map, get, assoc, dissoc, keys, vals, merge, contains?, count, entries, map-vals, filter, select-keys, update
- **List operations** — car/cdr + 12 compositions (caar through cdddr), cons, map (multi-list), filter, foldl, foldr, reduce, sort, range, take, drop, zip, flatten, partition, any, every, member, last, apply, index-of, unique, group-by, interleave, chunk, assoc/assq/assv (alist lookup)
- **Lazy evaluation** — `delay`/`force` with memoized promises, `promise?`, `promise-forced?`
- **Iteration** — proper Scheme `do` loop with parallel assignment
- **Math** — Full suite: abs, min, max, floor, ceil, round, sqrt, pow, log, sin, cos, tan, asin, acos, atan, atan2, exp, log10, log2, gcd, lcm, quotient, remainder, random, random-int, clamp, sign, pi, e
- **Bitwise** — bit/and, bit/or, bit/xor, bit/not, bit/shift-left, bit/shift-right
- **Error handling** — try/catch/throw with typed error maps
- **Module system** — module/import with exports, selective import, caching, namespace isolation
- **File I/O** — `file/read`, `file/write`, `file/append`, `file/delete`, `file/rename`, `file/list`, `file/mkdir`, `file/info`, `file/exists?`, `file/is-directory?`, `file/is-file?`, `file/is-symlink?`
- **Path ops** — `path/join`, `path/dirname`, `path/basename`, `path/extension`, `path/absolute`
- **Regex** — `regex/match?`, `regex/match`, `regex/find-all`, `regex/replace`, `regex/replace-all`, `regex/split`
- **HTTP** — `http/get`, `http/post`, `http/put`, `http/delete`, `http/request`
- **Metaprogramming** — `eval`, `read`, `read-many`, `gensym`, `macroexpand`, `case`, type conversions
- **System** — env, shell, exit, time-ms, sleep, sys/args, sys/cwd, sys/platform, sys/env-all
- **JSON** — json/encode, json/decode, json/encode-pretty
- **Crypto/encoding** — uuid/v4, base64/encode, base64/decode, hash/sha256
- **Date/time** — time/now, time/format, time/parse, time/date-parts
- **CSV** — csv/parse, csv/parse-maps, csv/encode
- **REPL** — Line editing, history, multi-line input
- **LLM integration** — Full suite: complete, stream, chat, extract, classify, batch, pmap, embed, agents, tools, conversations, cost tracking, budgets
