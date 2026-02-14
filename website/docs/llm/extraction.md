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

## Classification

### `llm/classify`

Classify text into one of a set of categories. Returns the matching keyword.

```scheme
(llm/classify [:positive :negative :neutral]
              "This product is amazing!")
; => :positive
```

Pass a vector of keyword labels and the text to classify. The LLM picks the best-matching label.
