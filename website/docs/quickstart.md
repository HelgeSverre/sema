---
outline: [2, 3]
---

# Quickstart

Welcome to Sema! This guide will get you up and running with the Sema CLI and REPL in under 5 minutes.

## Prerequisites

Make sure you have installed the `sema` binary. If you haven't yet, follow the [Installation Instructions](./#installation) on the homepage.

To verify your installation, check the version:

```bash
sema --version
```

---

## 1. The Interactive REPL

The easiest way to explore Sema is using the interactive Read-Eval-Print Loop (REPL). Start it by running:

```bash
sema
```

You should see a prompt like `sema>`. Type an expression and press `Enter` to evaluate it:

```sema
sema> (+ 1 2)
3

sema> (define x 10)
x

sema> (* x 5)
50

sema> (exit)
```

> [!TIP]
> Use `Ctrl+D` or type `(exit)` to quit the REPL. The REPL also supports command history and auto-completions.

---

## 2. Running a Script

To write and execute scripts, save your Sema code to a file with the `.sema` extension.

Create a file named `hello.sema` with the following content:

```sema
(define name "World")
(println f"Hello, ${name}!")
```

Run the file using the `sema` command:

```bash
sema hello.sema
```

This will output:
```text
Hello, World!
```

---

## 3. Inline Evaluation

You can evaluate expressions directly from your shell without opening the REPL or creating a file.

### Evaluate an expression
Use the `-e` flag to evaluate an expression silently (it will only print if your code calls printing functions like `println`):

```bash
sema -e '(println "Sema is running!")'
```

### Evaluate and print the result
Use the `-p` flag to evaluate an expression and automatically print the result:

```bash
sema -p '(map #(* % %) (range 1 6))'
```
Output:
```text
(1 4 9 16 25)
```

---

## 4. Quick LLM Call

Sema is designed around LLM integration. To try it, make sure you have an API key set in your environment — Sema auto-detects all supported providers (Anthropic, OpenAI, Gemini, DeepSeek, Groq, xAI, Mistral, Moonshot, OpenRouter, Together AI, Fireworks AI, Cerebras, SambaNova, Perplexity, Ollama). Set any one:

Start the REPL and call the completion primitive:

```sema
sema> (llm/complete "Translate 'Lisp is beautiful' to French.")
"« Lisp est magnifique »"
```

## Next Steps

Now that you know how to run Sema, let's learn how to write it:
*   [Basic Syntax](./tutorial/basics.md) — S-expressions, variables, and collections
*   [Functions & Scope](./tutorial/functions.md) — How to define functions and run loops/recursion
*   [Concurrency & Async](./tutorial/concurrency.md) — Multi-threaded execution in the VM
