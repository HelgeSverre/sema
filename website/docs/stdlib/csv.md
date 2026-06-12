---
outline: [2, 3]
---

# CSV

Functions for parsing and encoding CSV (Comma-Separated Values) data. Sema uses the Rust [`csv`](https://docs.rs/csv) crate, which handles RFC 4180 edge cases like quoted fields, embedded commas, and newlines within fields.

::: tip Type mapping
All CSV values are returned as **strings**. Use `string/to-number`, `string/to-symbol`, etc. to convert fields to the types you need.
:::

## Parsing

### `csv/parse`

Parse a CSV string into a list of lists (rows of fields). No header processing — every row is returned as-is.

**Signature:** `(csv/parse csv-string) → list`

```sema
(csv/parse "a,b\n1,2\n3,4")
; => (("a" "b") ("1" "2") ("3" "4"))
```

Quoted fields with commas and newlines are handled correctly:

```sema
(csv/parse "name,bio\n\"Ada\",\"Mathematician, writer\"\n")
; => (("name" "bio") ("Ada" "Mathematician, writer"))
```

### `csv/parse-maps`

Parse a CSV string into a list of maps. The first row is used as headers, which become keyword keys in each map.

**Signature:** `(csv/parse-maps csv-string) → list`

```sema
(csv/parse-maps "name,age\nAda,36\nBob,25")
; => ({:age "36" :name "Ada"} {:age "25" :name "Bob"})
```

Access fields by keyword:

```sema
(define rows (csv/parse-maps "name,age\nAda,36\nBob,25"))
(:name (first rows))   ; => "Ada"
```

## Encoding

### `csv/encode`

Encode a list of lists (or vectors) into a CSV string. Each inner list/vector becomes one row. Non-string values are stringified automatically.

**Signature:** `(csv/encode rows) → string`

```sema
(csv/encode '(("a" "b") ("1" "2")))
; => "a,b\n1,2\n"
```

Numeric and other values are converted to strings:

```sema
(csv/encode '(("name" "score") ("Ada" 100)))
; => "name,score\nAda,100\n"
```

## Examples

### Round-trip example

```sema
(define csv-text "name,age\nAda,36\nBob,25\n")
(define parsed (csv/parse csv-text))
(csv/encode parsed)
; => "name,age\nAda,36\nBob,25\n"
```

### Pipeline: file → CSV → processing

```sema
;; Read a CSV file and extract a column
(define data (csv/parse-maps (file/read "users.csv")))
(map (lambda (row) (:name row)) data)
```
