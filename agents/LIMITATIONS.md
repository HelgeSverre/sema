# Sema Lisp — Known Limitations & Gaps

Assessed against standard Scheme (R7RS) and practical Lisp expectations.
Status: as of Phase 8 completion (stdlib expansion: math, bitwise, crypto, datetime, csv).

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

### 9. `do` Aliased to `begin`

`do` is an alias for `begin` (sequence), which shadows the standard Scheme `do` loop:
```scheme
;; Scheme do loop — not supported:
(do ((i 0 (+ i 1))) ((= i 10)) (display i))
```

### ~~10. No `case` / Pattern Matching~~ → RESOLVED (Phase 7)
`case` special form with R5RS semantics. Pattern matching still absent.

### ~~11. No Port-Based I/O~~ → PARTIALLY RESOLVED (Phase 7)
Full file namespace: `file/read`, `file/write`, `file/append`, `file/delete`, `file/rename`, `file/list`, `file/mkdir`, `file/info`, `file/exists?`, `file/is-directory?`, `file/is-file?`, `file/is-symlink?`.
Path ops: `path/join`, `path/dirname`, `path/basename`, `path/extension`, `path/absolute`.
HTTP: `http/get`, `http/post`, `http/put`, `http/delete`, `http/request`.
Still no streaming/port-based I/O.

### 12. No Struct/Record Types

Maps-with-keywords serve as de facto structs but with no type checking, constructors, or accessor generation. No `define-record-type`.

### 13. No Stack Traces

Errors show only the message, no call stack or source location.

### ~~14. Missing Math Functions~~ → RESOLVED (Phase 8)
Full math suite: `math/quotient`, `math/remainder`, `math/gcd`, `math/lcm`, `math/tan`, `math/asin`, `math/acos`, `math/atan`, `math/atan2`, `math/exp`, `math/log10`, `math/log2`, `math/random`, `math/random-int`, `math/clamp`, `math/sign`.
Bitwise: `bit/and`, `bit/or`, `bit/xor`, `bit/not`, `bit/shift-left`, `bit/shift-right`.

### 15. No `guard` (R7RS Style)

We chose `try`/`catch`/`throw` over R7RS `guard`. The error map with `:type` keyword enables pattern matching in the handler via `cond`.

---

## Low — Completeness Only

### 16. No Full Numeric Tower

Only `i64` and `f64`. No rationals (`1/3`), bignums, or complex numbers.

### 17. No Char Type

`string-ref` returns a one-character string. No `char?`, `char->integer`, etc.

### 18. No Continuations

No `call/cc` or `call-with-current-continuation`. The trampoline evaluator cannot capture continuations.

### 19. No Multiple Return Values

No `values` / `call-with-values`.

### 20. No Dynamic Binding

No `dynamic-wind`, `parameterize`, `make-parameter`.

### 21. No Hygienic Macros

Only `defmacro` (Lisp-style). No `syntax-rules` or `syntax-case`.

---

## What Works Well

- **Closures** — Properly implemented with lexical scoping
- **Tail Call Optimization** — Trampoline-based, works for direct recursion in `if`/`cond`/`let`/`begin`/`and`/`or`/`when`/`unless` + named `let`
- **Data types** — Int, Float, String, Symbol, Keyword, List, Vector, Map, Bool, Nil + LLM types
- **Macros** — `defmacro` with quasiquote/unquote/unquote-splicing (non-hygienic but functional)
- **String operations** — split, trim, replace, contains?, format, str, index-of, chars, repeat, pad-left, pad-right
- **Map operations** — hash-map, get, assoc, dissoc, keys, vals, merge, contains?, count, entries, map-vals, filter, select-keys, update
- **List operations** — car/cdr, cons, map (multi-list), filter, foldl, foldr, reduce, sort, range, take, drop, zip, flatten, partition, any, every, member, last, apply, index-of, unique, group-by, interleave, chunk
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
