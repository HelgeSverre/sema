# Reader Internals

Sema's reader is a two-phase pipeline: a lexer tokenizes source text into `SpannedToken`s, then a recursive descent parser produces `Value` nodes directly — there is no intermediate AST. Source locations are tracked per-token and attached to compound values via an `Rc::as_ptr` trick that avoids bloating the `Value` enum.

This page documents the lexer, parser, token types, quote desugaring, span tracking, and how the evaluator recovers source positions for error reporting.

## The Lexer

The lexer in `crates/sema-reader/src/lexer.rs` is a single-pass tokenizer that walks a `Vec<char>` with a manual index `i` and tracks `line`/`col` for span information.

Character-level dispatch drives the lexer. Each iteration inspects the current character and branches:

- **Whitespace** — skipped, advances `line`/`col`
- **`;`** — comment, skip to end of line
- **`(`/`)`/`[`/`]`/`{`/`}`** — emit the corresponding bracket token
- **`'`** — emit `Token::Quote`
- **`` ` ``** — emit `Token::Quasiquote`
- **`,`** — peek ahead: `,@` emits `Token::UnquoteSplice`, otherwise `Token::Unquote`
- **`"`** — enter string mode, handle escape sequences
- **`#`** — dispatch on next char: `#t`/`#f` for booleans, `#\` for character literals, `#u8(` for bytevector start
- **`:`** — keyword (Clojure-style `:foo`)
- **Digit or `-` followed by digit** — number (integer or float)
- **Otherwise** — symbol character, accumulate until delimiter

Every token is wrapped in a `SpannedToken` that records the `Span { line, col }` where it began. This is the only place source positions enter the system — everything downstream inherits or discards them.

```rust
// crates/sema-reader/src/lexer.rs
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

pub struct Span {
    pub line: usize,
    pub col: usize,
}
```

## Token Zoo

The full `Token` enum:

| Token                   | Syntax           | Example                 |
| ----------------------- | ---------------- | ----------------------- |
| `LParen` / `RParen`     | `(` `)`          | `(+ 1 2)`               |
| `LBracket` / `RBracket` | `[` `]`          | `[1 2 3]`               |
| `LBrace` / `RBrace`     | `{` `}`          | `{:a 1 :b 2}`           |
| `Quote`                 | `'`              | `'foo`                  |
| `Quasiquote`            | `` ` ``          | `` `(a ,b) ``           |
| `Unquote`               | `,`              | `,x`                    |
| `UnquoteSplice`         | `,@`             | `,@xs`                  |
| `Int(i64)`              | digits           | `42`, `-7`              |
| `Float(f64)`            | digits with `.`  | `3.14`, `-0.5`          |
| `String(String)`        | `"..."`          | `"hello"`               |
| `Symbol(String)`        | identifier       | `define`, `string/trim` |
| `Keyword(String)`       | `:` + name       | `:key`, `:name`         |
| `Bool(bool)`            | `#t` / `#f`      | `#t`                    |
| `Char(char)`            | `#\` + char/name | `#\a`, `#\space`        |
| `BytevectorStart`       | `#u8(`           | `#u8(1 2 3)`            |
| `Dot`                   | `.`              | `(a . b)`               |

Symbol characters include alphanumeric plus `+ - * / ! ? < > = _ & % ^ ~ .` — a superset of Scheme's identifier syntax that allows operators and predicates like `nil?` or `string->number` as plain symbols.

Booleans accept both `#t`/`#f` (R7RS) and `true`/`false` (as symbol aliases resolved during tokenization).

## The Parser

The parser in `crates/sema-reader/src/reader.rs` is a recursive descent parser that consumes the `Vec<SpannedToken>` produced by the lexer. It's structured as a `Parser` struct with a position index, dispatching on the current token type:

```
parse_expr
  ├── LParen    → parse_list     → Value::List
  ├── LBracket  → parse_vector   → Value::Vector
  ├── LBrace    → parse_map      → Value::Map
  ├── Quote     → desugar        → Value::List [quote, x]
  ├── Quasiquote→ desugar        → Value::List [quasiquote, x]
  ├── Unquote   → desugar        → Value::List [unquote, x]
  ├── UnquoteSplice → desugar    → Value::List [unquote-splicing, x]
  ├── BytevectorStart → parse_bytevector → Value::Bytevector
  ├── Int       → Value::Int
  ├── Float     → Value::Float
  ├── String    → Value::String
  ├── Symbol    → Value::Symbol
  ├── Keyword   → Value::Keyword
  ├── Bool      → Value::Bool
  └── Char      → Value::Char
```

Each compound form has its own parsing method:

- **`parse_list`** — collects expressions until `)`, handling dotted pairs (see below)
- **`parse_vector`** — collects expressions until `]`, wraps in `Value::Vector(Rc::new(vec![...]))`
- **`parse_map`** — collects key-value pairs until `}`, wraps in `Value::Map(Rc::new(BTreeMap::from(...)))`. Odd element count is a parse error
- **`parse_bytevector`** — collects integers until `)`, validates each is 0–255, wraps in `Value::Bytevector(Rc::new(vec![...]))`

The parser produces `Value` nodes directly. There is no separate AST type — the same `Value` enum used at runtime is the representation of parsed code. This is the Lisp tradition: code is data, and the reader produces data.

> **Comparison:** Racket's reader is configurable with [readtables](https://docs.racket-lang.org/reference/readtables.html) — user code can define new reader syntax. Common Lisp goes further with [reader macros](https://www.lispworks.com/documentation/HyperSpec/Body/02_d.htm) that can override any character's parsing behavior. Sema has neither — quote sugar is hardcoded in the lexer, and there's no mechanism for user-defined reader extensions. This is a deliberate simplicity trade-off: the reader is predictable, the implementation is ~300 lines, and all syntax is documented in one place. See Nystrom's [_Crafting Interpreters_](https://craftinginterpreters.com/parsing-expressions.html) for a thorough treatment of recursive descent parsing, or Aho et al., _Compilers: Principles, Techniques, and Tools_ (the Dragon Book), §4.4 for the theory.

## Quote Desugaring

The reader desugars quote syntax into real lists _before the evaluator ever sees them_. This is important: `'x` is not a special syntactic form that the evaluator handles — it's reader sugar that produces a `(quote x)` list.

| Syntax   | Desugars to            | Reader token           |
| -------- | ---------------------- | ---------------------- |
| `'x`     | `(quote x)`            | `Token::Quote`         |
| `` `x `` | `(quasiquote x)`       | `Token::Quasiquote`    |
| `,x`     | `(unquote x)`          | `Token::Unquote`       |
| `,@x`    | `(unquote-splicing x)` | `Token::UnquoteSplice` |

When the parser encounters a `Quote` token, it:

1. Consumes the next expression (recursive `parse_expr` call)
2. Wraps it: `Value::List(Rc::new(vec![Value::symbol("quote"), expr]))`
3. Attaches the quote token's span to the resulting list

The evaluator then sees `(quote x)` as a normal list whose `car` is the symbol `quote` — which it handles as a special form. The same applies to `quasiquote`, which the evaluator expands recursively (handling nested `unquote` and `unquote-splicing` within templates).

The key distinction: the _syntax_ (`` ` , ,@ ' ``) is reader-level, but the _semantics_ (what `quasiquote` does with its template) is evaluator-level. The reader's job is just to produce the list structure.

## Dotted Pairs

Sema supports dotted pair notation `(a . b)` for compatibility with Scheme's cons-cell tradition, but the representation is unconventional. Since `Value::List` wraps a `Vec<Value>` (not a linked list of cons cells), dotted pairs are represented using a marker symbol:

```sema
(a . b)    ;; parses as a list of three elements: [a, ".", b]
(1 2 . 3)  ;; parses as: [1, 2, ".", 3]
```

The parser's `parse_list` method detects `Token::Dot` and inserts `Value::symbol(".")` into the element list. The evaluator and printer check for this marker when they need to distinguish `(a b c)` from `(a b . c)`.

This is a pragmatic compromise. Real Scheme implementations use linked cons cells where `(a . b)` is `cons(a, b)` — the dot is the _absence_ of a list, not a marker within one. Sema's Vec-based representation can't express improper lists natively, so the dot marker serves as an escape hatch for the few places that need it (mostly association lists and Scheme compatibility).

## String Escapes

The lexer handles common R7RS escape sequences plus Unicode extensions:

| Escape       | Character       | Notes                                       |
| ------------ | --------------- | ------------------------------------------- |
| `\n`         | newline         |                                             |
| `\t`         | tab             |                                             |
| `\r`         | carriage return |                                             |
| `\\`         | backslash       |                                             |
| `\"`         | double quote    |                                             |
| `\0`         | null            |                                             |
| `\x41;`      | `A` (hex 0x41)  | R7RS — note the trailing semicolon          |
| `\u0041`     | `A`             | 4-digit Unicode escape                      |
| `\U00000041` | `A`             | 8-digit Unicode escape (full Unicode range) |

The R7RS hex escape `\x<hex>;` uses a semicolon terminator, which is unusual — most languages use a fixed digit count. This allows variable-length hex sequences: `\x41;` and `\x041;` are both valid and produce the same character. The semicolon disambiguates where the hex digits end.

The `\uNNNN` and `\UNNNNNNNN` forms follow the C/Java/Rust convention of fixed-width escapes. These are Sema extensions not found in R7RS.

Character literals follow a similar pattern:

| Literal     | Character         |
| ----------- | ----------------- |
| `#\a`       | the character `a` |
| `#\space`   | space             |
| `#\newline` | newline           |
| `#\tab`     | tab               |
| `#\return`  | carriage return   |
| `#\nul`     | null              |

## Span Tracking

This is the most architecturally interesting part of the reader. The problem: error messages need source locations ("line 12, column 5"), but storing a `Span` in every `Value` would bloat the enum. Most values are small — `Value::Int(42)` is 16 bytes — and adding a `Span` field would double the size of every value in the system, including runtime values that were never parsed from source.

**The solution:** spans are stored in a side table keyed by `Rc` pointer addresses.

```rust
// crates/sema-reader/src/reader.rs
fn make_list_with_span(&mut self, items: Vec<Value>, span: Span) -> Result<Value, SemaError> {
    let rc = Rc::new(items);
    let ptr = Rc::as_ptr(&rc) as usize;
    self.span_map.insert(ptr, span);
    Ok(Value::List(rc))
}
```

The `SpanMap` is a `HashMap<usize, Span>` — it maps `Rc::as_ptr()` cast to `usize` to the source span. This works because:

1. **`Rc::as_ptr` is stable** — for a given `Rc`, the inner pointer doesn't change as long as the `Rc` (or any clone of it) is alive
2. **Clones share the pointer** — `Rc::clone()` increments the refcount but doesn't change the underlying pointer, so a cloned list still maps to the same span
3. **No cost to non-compound values** — atoms (integers, strings, symbols) don't get spans. Both `Value::List` and `Value::Vector` participate in span tracking — the reader inserts their `Rc::as_ptr()` addresses into the span map. However, the evaluator's `span_of_expr` currently only recovers spans from `Value::List`; vector spans are tracked by the reader but not used during error reporting

**The trade-off:** when the `Rc` is deallocated, its pointer address could be reused by a new allocation, producing a stale span lookup. In practice this is a minor diagnostic risk rather than a correctness issue — a wrong span in an error message is better than no span. The span table accumulates entries across parsed inputs; in long-running processes (REPL, embedding), stale entries could theoretically produce misleading source locations, though this has not been observed in practice. Also, only list-shaped values get spans. An error in evaluating an atom like `undefined-var` won't have a direct span — the evaluator must use the span of the enclosing list expression instead.

### Span Recovery in the Evaluator

The span table is a field in `EvalContext`, populated when source is parsed via `ctx.merge_span_table(spans)`:

```rust
// crates/sema-eval/src/eval.rs
fn span_of_expr(ctx: &EvalContext, expr: &Value) -> Option<Span> {
    match expr {
        Value::List(items) => {
            let ptr = Rc::as_ptr(items) as usize;
            ctx.lookup_span(ptr)
        }
        _ => None,
    }
}
```

The evaluator calls `span_of_expr` when constructing error messages, attaching the source position of the failing expression to the `SemaError`. This flows through the call stack and ultimately appears in error output like:

```
Error at line 12, col 5: undefined variable 'foo'
```

## Error Reporting

Errors flow through two mechanisms:

1. **`SemaError::Reader`** — carries a `Span` directly for parse-time errors (unmatched brackets, invalid escape sequences, unexpected tokens). These are produced by the lexer and parser before evaluation begins.

2. **`CallFrame` + span table** — for runtime errors, the evaluator maintains a call stack of `CallFrame`s. When an error occurs, it walks the stack, using `span_of_expr` to find source positions for each frame. This produces stack traces with source locations even though `Value` itself carries no span.

The combination means parse errors report exact positions (the lexer knows where every token starts), while runtime errors report the position of the enclosing list expression (the best available approximation from the span table).

## Public API

The reader exposes three entry points:

```rust
// crates/sema-reader/src/reader.rs

/// Parse a single expression from input
pub fn read(input: &str) -> Result<Value, SemaError>

/// Parse all expressions from input
pub fn read_many(input: &str) -> Result<Vec<Value>, SemaError>

/// Parse all expressions and return the span map for error reporting
pub fn read_many_with_spans(input: &str) -> Result<(Vec<Value>, SpanMap), SemaError>
```

`read_many_with_spans` is what the evaluator uses — it needs the span map to populate the `EvalContext`'s span table. The simpler `read` and `read_many` are convenience wrappers for contexts where error positions aren't needed (tests, REPL one-liners).

## Pipeline Summary

```
Source text
  │
  ▼
tokenize()          crates/sema-reader/src/lexer.rs
  │                 "single-pass"
  │                 "produces Vec<SpannedToken>"
  ▼
Parser::parse()     crates/sema-reader/src/reader.rs
  │                 "recursive descent"
  │                 "produces Vec<Value> + SpanMap"
  ▼
ctx.merge_span_table()  crates/sema-eval/src/eval.rs
  │                 "populates EvalContext span table"
  ▼
eval()              crates/sema-eval/src/eval.rs
  │                 "trampoline-based TCO evaluator"
  │                 "recovers spans via Rc::as_ptr lookup"
  ▼
Value               result
```
