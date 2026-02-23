---
outline: [2, 3]
---

# Text Processing

Sema includes utilities for text chunking, cleaning, prompt templates, and structured documents — building blocks for LLM pipelines.

## Text Chunking

### `text/chunk`

Recursively split text into chunks, trying natural boundaries (paragraphs, sentences, words) before hard-splitting. Takes text and an optional options map.

```sema
(text/chunk "Long text here...")
(text/chunk "Long text here..." {:size 500 :overlap 100})
```

Options: `:size` (default 1000), `:overlap` (default 200). Returns a list of strings.

### `text/chunk-by-separator`

Split text by a specific separator string.

```sema
(text/chunk-by-separator "a\nb\nc" "\n")  ; => ("a" "b" "c")
```

### `text/split-sentences`

Split text into sentences at `.`, `!`, `?` boundaries.

```sema
(text/split-sentences "Hello world. How are you? Fine.")
; => ("Hello world." "How are you?" "Fine.")
```

## Text Cleaning

### `text/clean-whitespace`

Collapse multiple whitespace characters (spaces, newlines, tabs) into single spaces.

```sema
(text/clean-whitespace "  hello   world  \n\n  foo  ")
; => "hello world foo"
```

### `text/strip-html`

Remove HTML tags and decode common entities (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&#39;`, `&apos;`, `&nbsp;`).

```sema
(text/strip-html "<p>Hello <b>world</b></p>")  ; => "Hello world"
(text/strip-html "a &amp; b &lt; c")            ; => "a & b < c"
```

### `text/truncate`

Truncate text to a maximum length with a suffix. Takes text, max-length, and optional suffix (default `"..."`).

```sema
(text/truncate "hello world" 5)       ; => "he..."
(text/truncate "hello world" 8 "…")   ; => "hello w…"
(text/truncate "hi" 10)               ; => "hi"
```

### `text/word-count`

Count words in text (split by whitespace).

```sema
(text/word-count "hello world foo bar")  ; => 4
```

### `text/trim-indent`

Remove common leading indentation from all lines.

```sema
(text/trim-indent "    hello\n    world")   ; => "hello\nworld"
(text/trim-indent "    hello\n      world") ; => "hello\n  world"
```

### `text/excerpt`

Extract a snippet around a search term with omission markers. Case-insensitive search. Returns `nil` if query not found.

```sema
(text/excerpt "The quick brown fox jumps over the lazy dog" "fox" {:radius 10})
; => "...brown fox jumps ov..."

(text/excerpt "Hello world" "Hello")
; => "Hello world"

;; Custom omission marker
(text/excerpt "Long text here..." "text" {:radius 5 :omission "[…]"})
; => "[…]g text here[…]"
```

Options map (optional third argument):

- `:radius` — number of characters to show on each side (default: 100)
- `:omission` — marker string for truncated parts (default: `"..."`)

### `text/normalize-newlines`

Convert `\r\n` (Windows) and `\r` (old Mac) line endings to `\n` (Unix).

```sema
(text/normalize-newlines "line1\r\nline2\rline3")
; => "line1\nline2\nline3"
```

## Prompt Templates

### `prompt/template`

Create a template string for use with `prompt/render`.

```sema
(define tmpl (prompt/template "Hello {{name}}, welcome to {{place}}."))
```

### `prompt/render`

Render a template by substituting `{{key}}` placeholders with values from a map. Missing keys are left as-is.

```sema
(prompt/render "Hello {{name}}, welcome to {{place}}."
  {:name "Alice" :place "Wonderland"})
; => "Hello Alice, welcome to Wonderland."

(prompt/render "Hello {{name}}, {{missing}}." {:name "Bob"})
; => "Hello Bob, {{missing}}."

;; Non-string values are stringified
(prompt/render "Count: {{n}}" {:n 42})
; => "Count: 42"
```

## Documents

Structured documents with metadata, designed for use with chunking and vector stores.

### `document/create`

Create a document map with `:text` and `:metadata`.

```sema
(document/create "Hello world" {:source "test.txt" :page 1})
; => {:metadata {:page 1 :source "test.txt"} :text "Hello world"}
```

### `document/text`

Extract the text from a document.

```sema
(document/text doc)  ; => "Hello world"
```

### `document/metadata`

Extract the metadata from a document.

```sema
(document/metadata doc)  ; => {:source "test.txt" :page 1}
```

### `document/chunk`

Chunk a document, preserving and extending metadata. Each chunk gets `:chunk-index` and `:total-chunks` added to its metadata.

```sema
(document/chunk
  (document/create "long text..." {:source "paper.pdf"})
  {:size 500})
; => ({:text "chunk 1..." :metadata {:source "paper.pdf" :chunk-index 0 :total-chunks 3}}
;     {:text "chunk 2..." :metadata {:source "paper.pdf" :chunk-index 1 :total-chunks 3}}
;     ...)
```
