---
outline: [2, 3]
---

# Feature Comparison

How does Sema stack up against other Lisps and Lisp-adjacent languages as a practical tool? This isn't about benchmarks (see [Lisp Dialect Benchmark](./lisp-comparison.md) for that) — it's about what you can actually _do_ out of the box.

## Languages Compared

| Language | Implementation | Primary Use Case |
| --- | --- | --- |
| **Sema** | Rust (tree-walker + bytecode VM) | LLM-native scripting, AI tooling |
| **Janet** | C (bytecode VM) | Embeddable scripting, system tools |
| **Racket** | Chez Scheme backend | Teaching, DSLs, research |
| **Clojure** | JVM | Production backend systems |
| **Fennel** | Lua transpiler | Game dev, Lua ecosystem |
| **Guile** | C (bytecode VM) | GNU extension language |
| **Common Lisp (SBCL)** | Native compiler | Production systems, HPC |

## Platform & Distribution

| Feature | Sema | Janet | Racket | Clojure | Fennel | Guile | SBCL |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Standalone executables | ✅ `sema build` | ✅ `jpm` | ✅ `raco exe` | ✅ GraalVM | ❌ needs Lua | ❌ | ✅ `save-lisp-and-die` |
| Bytecode compilation | ✅ `.semac` | ✅ images | ✅ `.zo` | ✅ `.class` | ❌ | ✅ `.go` | ✅ FASL |
| WASM / browser | ✅ [sema.run](https://sema.run) | ⚠️ community | ⚠️ WebRacket (subset) | ✅ ClojureScript | ⚠️ via Fengari | ⚠️ Hoot (R7RS subset) | ⚠️ ECL/Emscripten |
| Web playground | ✅ 20+ examples | ⚠️ community | ✅ RacketScript | ⚠️ community | ✅ on fennel-lang.org | ❌ | ❌ |
| Shebang scripts | ✅ | ✅ | ✅ | ❌ | ✅ | ✅ | ⚠️ via Roswell |
| Homebrew install | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Windows support | ✅ | ✅ | ✅ | ✅ | ✅ | ⚠️ | ⚠️ |
| Install script (curl) | ✅ | ❌ | ❌ | ✅ | ✅ | ❌ | ❌ |

## Embedding

| Feature | Sema | Janet | Racket | Clojure | Fennel | Guile | SBCL |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Embed in Rust | ✅ crate API | ⚠️ via FFI | ❌ | ❌ | ❌ | ⚠️ via FFI | ❌ |
| Embed in C/C++ | ⚠️ via FFI | ✅ single `.c`+`.h` | ✅ | ❌ | ✅ single file | ✅ `libguile` | ✅ ECL |
| Embed in JS/Node | ✅ WASM module | ⚠️ community WASM | ❌ | ✅ ClojureScript | ✅ native | ❌ | ❌ |
| Sandbox mode | ✅ `--sandbox` | ❌ | ✅ | ❌ | ❌ | ❌ | ❌ |

## Built-in Standard Library

| Feature | Sema | Janet | Racket | Clojure | Fennel | Guile | SBCL |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Stdlib functions | 460+ | 600+ | 1000+ | 700+ | ~50 (+ Lua) | 500+ | 900+ |
| HTTP client | ✅ built-in | ⚠️ via library | ✅ built-in | ⚠️ via library | ⚠️ via Lua | ⚠️ via library | ⚠️ via library |
| JSON | ✅ built-in | ⚠️ via library | ✅ built-in | ✅ built-in | ❌ | ⚠️ via library | ⚠️ via library |
| Regex | ✅ built-in | ✅ PEGs | ✅ built-in | ✅ built-in | ✅ Lua patterns | ✅ built-in | ⚠️ via library |
| CSV | ✅ built-in | ❌ | ✅ built-in | ❌ | ❌ | ❌ | ❌ |
| Crypto (SHA, HMAC) | ✅ built-in | ⚠️ via library | ✅ built-in | ⚠️ via library | ❌ | ⚠️ via library | ⚠️ via library |
| PDF extraction | ✅ built-in | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| File I/O | ✅ built-in | ✅ built-in | ✅ built-in | ✅ via Java | ✅ via Lua | ✅ built-in | ✅ built-in |
| Date/time | ✅ built-in | ✅ built-in | ✅ built-in | ✅ via Java | ✅ via Lua | ✅ built-in | ⚠️ via library |
| Shell execution | ✅ built-in | ✅ built-in | ✅ built-in | ✅ built-in | ✅ via Lua | ✅ built-in | ✅ built-in |
| KV store | ✅ built-in | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| Terminal styling | ✅ built-in | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |

## LLM & AI

This is Sema's primary differentiator. No other Lisp has LLM primitives as first-class language features.

| Feature | Sema | Janet | Racket | Clojure | Fennel | Guile | SBCL |
| --- | --- | --- | --- | --- | --- | --- | --- |
| LLM chat/completion | ✅ built-in | ❌ | ❌ | ⚠️ via library | ❌ | ❌ | ❌ |
| Multi-provider (8+) | ✅ | — | — | — | — | — | — |
| Streaming | ✅ built-in | — | — | — | — | — | — |
| Tool use / agents | ✅ `deftool` `defagent` | — | — | — | — | — | — |
| Structured extraction | ✅ `llm/extract` | — | — | — | — | — | — |
| Vision / images | ✅ built-in | — | — | — | — | — | — |
| Embeddings | ✅ 3 providers | — | — | — | — | — | — |
| Vector store (RAG) | ✅ built-in | — | — | — | — | — | — |
| Cost tracking | ✅ `llm/budget` | — | — | — | — | — | — |
| Response caching | ✅ `llm/with-cache` | — | — | — | — | — | — |
| Conversations | ✅ immutable data | — | — | — | — | — | — |
| Provider fallback | ✅ `llm/with-fallback` | — | — | — | — | — | — |
| Prompt templates | ✅ built-in | — | — | — | — | — | — |

## Language Features

| Feature | Sema | Janet | Racket | Clojure | Fennel | Guile | SBCL |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Tail-call optimization | ✅ | ✅ | ✅ | ⚠️ `recur` only | ✅ via Lua | ✅ | ⚠️ not guaranteed |
| Macros | ✅ `defmacro` | ✅ | ✅ hygienic | ✅ | ✅ | ✅ both | ✅ |
| Pattern matching | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ⚠️ via library |
| Modules | ✅ | ✅ | ✅ | ✅ namespaces | ❌ | ✅ | ✅ packages |
| Continuations | ❌ | ✅ fibers | ✅ `call/cc` | ❌ | ❌ | ✅ `call/cc` | ❌ |
| Multithreading | ❌ | ✅ | ✅ | ✅ | ✅ via Lua | ✅ | ✅ |
| Persistent data structures | ⚠️ COW maps | ❌ | ❌ | ✅ core design | ❌ | ❌ | ❌ |
| Keywords | ✅ `:foo` | ✅ `:foo` | ✅ `#:foo` | ✅ `:foo` | ✅ `:foo` | ✅ `#:foo` | ✅ `:foo` |
| Map literals | ✅ `{:a 1}` | ✅ `@{:a 1}` | ✅ `#hash` | ✅ `{:a 1}` | ✅ `{:a 1}` | ❌ | ❌ |
| Vector literals | ✅ `[1 2]` | ✅ `@[1 2]` | ✅ `#(1 2)` | ✅ `[1 2]` | ✅ `[1 2]` | ✅ `#(1 2)` | ✅ `#(1 2)` |
| F-strings | ✅ `f"${x}"` | ❌ | ❌ | ❌ | ❌ | ❌ | ❌ |
| Short lambdas | ✅ `#(+ % 1)` | ✅ `|(+ $ 1)` | ❌ | ✅ `#(+ % 1)` | ✅ `#(+ $1 1)` | ❌ | ❌ |
| Threading macros | ✅ `->` `->>` | ✅ `->` `->>` | ⚠️ via library | ✅ `->` `->>` | ✅ `->` `->>` | ❌ | ⚠️ via library |

## Developer Experience

| Feature | Sema | Janet | Racket | Clojure | Fennel | Guile | SBCL |
| --- | --- | --- | --- | --- | --- | --- | --- |
| REPL | ✅ | ✅ | ✅ DrRacket | ✅ nREPL | ✅ | ✅ | ✅ SLIME/Sly |
| Tab completion | ✅ | ✅ | ✅ | ✅ | ⚠️ | ⚠️ | ✅ |
| Editor support | VS Code, Vim, Emacs, Helix | VS Code, Vim, Emacs | DrRacket, Emacs, VS Code | Emacs, VS Code, IntelliJ | Emacs, Vim, VS Code | Emacs (Geiser) | Emacs (SLIME/Sly) |
| Package manager | ❌ | ✅ `jpm` | ✅ `raco` | ✅ deps.edn/Lein | ❌ (uses Lua) | ⚠️ Guix | ✅ Quicklisp |
| Debugger | ❌ | ✅ | ✅ | ✅ | ❌ | ✅ | ✅ |
| Documentation site | ✅ sema-lang.com | ✅ janet-lang.org | ✅ docs.racket-lang.org | ✅ clojure.org | ✅ fennel-lang.org | ✅ gnu.org/guile | ✅ cliki.net |
| Startup time | ~5ms | ~5ms | ~200ms | ~1–2s | ~5ms | ~50ms | ~50ms |

## Summary

Sema is not trying to be the fastest Lisp or the most theoretically pure. Its niche is **practical scripting with LLM primitives built into the language** — no other Lisp has `deftool`, `defagent`, `llm/extract`, or multi-provider conversations as first-class constructs.

If you need the **fastest execution**, use SBCL or Chez Scheme. If you need the **JVM ecosystem**, use Clojure. If you need **academic rigor and DSL tooling**, use Racket. If you need a **tiny embeddable C scripting engine**, use Janet.

If you want to **build AI agents, extract structured data from LLMs, or prototype LLM-powered tools** in a language that treats prompts as data — Sema is the only Lisp built for that.

<script setup>
import { onMounted } from 'vue'

onMounted(() => {
  document.querySelectorAll('table tr').forEach(row => {
    const cells = row.querySelectorAll('td, th')
    if (cells.length > 0) {
      const first = cells[0]
      if (first && first.textContent.trim().startsWith('Sema')) {
        row.classList.add('sema-row')
      }
    }
  })
})
</script>

<style>
.sema-row {
  background: linear-gradient(90deg, rgba(245, 158, 11, 0.18), rgba(245, 158, 11, 0.06)) !important;
}
.sema-row td {
  font-weight: 600;
}
.sema-row td:first-child {
  border-left: 3px solid #f59e0b !important;
}
</style>
