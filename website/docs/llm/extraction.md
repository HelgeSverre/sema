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
