---
outline: [2, 3]
---

# Structured Extraction

Extract structured data from unstructured text using LLM-powered schema-based extraction and classification.

## Extraction

### `llm/extract`

Extract structured data from text according to a schema. The schema defines the expected fields and their types.

```scheme
(llm/extract
  {:vendor {:type :string}
   :amount {:type :number}
   :date   {:type :string}}
  "I bought coffee for $4.50 at Blue Bottle on Jan 15, 2025")
; => {:amount 4.5 :date "2025-01-15" :vendor "Blue Bottle"}
```

The schema map specifies field names as keys and type descriptors as values. Supported types include `:string`, `:number`, and `:boolean`.

### Options

`llm/extract` accepts an optional third argument — an options map:

```scheme
(llm/extract schema text {:model "claude-haiku-4-5-20251001"})
```

| Option       | Type     | Default | Description                                       |
| ------------ | -------- | ------- | ------------------------------------------------- |
| `:model`     | string   | —       | Override the default model                         |
| `:validate`  | boolean  | `false` | Validate response against the schema              |
| `:retries`   | integer  | `0`     | Retry on validation failure (requires `:validate`) |

### Schema Validation

With `:validate true`, the extracted result is checked against the schema:
- All schema keys must be present in the result
- Types must match: `:string` → string, `:number` → integer or float, `:boolean` → boolean, `:list`/`:array` → list or vector

```scheme
;; Strict extraction with validation
(llm/extract
  {:name {:type :string}
   :age  {:type :number}}
  "Alice is 30 years old"
  {:validate true})
; => {:age 30 :name "Alice"}
```

If validation fails, an error is raised with details about which fields didn't match.

### Retry on Mismatch

Combine `:validate` with `:retries` to automatically re-send the request when the LLM returns data that doesn't match the schema:

```scheme
(llm/extract
  {:items {:type :list}
   :total {:type :number}}
  "3 apples, 2 oranges, total 5 items"
  {:validate true :retries 2})
```

On each retry, the validation errors are fed back to the LLM to improve the next attempt. After exhausting retries, the final validation error is raised.

## Classification

### `llm/classify`

Classify text into one of a set of categories. Returns the matching keyword.

```scheme
(llm/classify [:positive :negative :neutral]
              "This product is amazing!")
; => :positive
```

Pass a vector of keyword labels and the text to classify. The LLM picks the best-matching label.

## Vision Extraction

### `llm/extract-from-image`

Extract structured data from images using vision-capable LLMs. Accepts a schema, an image source (file path or bytevector), and optional options.

```scheme
;; Extract from a file path
(llm/extract-from-image
  {:text :string :background_color :string}
  "assets/logo.png")
; => {:background_color "white" :text "Sema"}

;; Extract from a bytevector
(define img (file/read-bytes "invoice.jpg"))
(llm/extract-from-image
  {:invoice_number :string :date :string :total :string}
  img)
; => {:date "2025-03-15" :invoice_number "12345" :total "$139.96"}
```

Supported image formats (detected automatically via magic bytes): PNG, JPEG, GIF, WebP, PDF.

### Options

`llm/extract-from-image` accepts an optional third argument — an options map:

```scheme
(llm/extract-from-image schema source {:model "gpt-4o"})
```

| Option   | Type   | Default | Description              |
| -------- | ------ | ------- | ------------------------ |
| `:model` | string | —       | Override the default model |

## Multi-Modal Messages

### `message/with-image`

Create a message that includes both text and an image, for use with `llm/chat`.

```scheme
(define img (file/read-bytes "photo.jpg"))
(define msg (message/with-image :user "What do you see?" img))
(llm/chat (list msg))
```

The image must be a bytevector (use `file/read-bytes` to load from disk). The media type is detected automatically.

You can combine image messages with regular messages:

```scheme
(llm/chat
  [(message :system "You are a helpful image analyst.")
   (message/with-image :user "Describe this chart." (file/read-bytes "chart.png"))])
```

### Provider Support

Vision features work with providers that support multi-modal input:

| Provider      | `llm/extract-from-image` | `message/with-image` |
| ------------- | ------------------------ | -------------------- |
| **Anthropic** | ✅                       | ✅                   |
| **OpenAI**    | ✅                       | ✅                   |
| **Gemini**    | ✅                       | ✅                   |
| **Ollama**    | ✅ (model-dependent)     | ✅ (model-dependent) |

For Ollama, use a vision-capable model like `gemma3:4b` or `llava`.
