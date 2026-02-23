---
outline: [2, 3]
---

# CSV, Crypto & Encoding

## CSV

Functions for parsing and encoding CSV (Comma-Separated Values) data. Sema uses the Rust [`csv`](https://docs.rs/csv) crate, which handles RFC 4180 edge cases like quoted fields, embedded commas, and newlines within fields.

::: tip Type mapping
All CSV values are returned as **strings**. Use `string->number`, `string->symbol`, etc. to convert fields to the types you need.
:::

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

## UUID

### `uuid/v4`

Generate a random UUID v4 string.

**Signature:** `(uuid/v4) → string`

```sema
(uuid/v4)   ; => "550e8400-e29b-41d4-a716-446655440000" (varies)
```

Each call returns a new unique identifier:

```sema
(equal? (uuid/v4) (uuid/v4))   ; => #f
```

## Base64 Encoding

Functions for Base64 encoding and decoding of strings and binary data. Uses the standard Base64 alphabet (RFC 4648).

### `base64/encode`

Encode a string to Base64.

**Signature:** `(base64/encode string) → string`

```sema
(base64/encode "hello")   ; => "aGVsbG8="
(base64/encode "")        ; => ""
```

### `base64/decode`

Decode a Base64 string back to a UTF-8 string. Errors if the decoded bytes are not valid UTF-8.

**Signature:** `(base64/decode base64-string) → string`

```sema
(base64/decode "aGVsbG8=")   ; => "hello"
```

### `base64/encode-bytes`

Encode a bytevector to Base64.

**Signature:** `(base64/encode-bytes bytevector) → string`

```sema
(base64/encode-bytes #u8(104 101 108 108 111))   ; => "aGVsbG8="
```

### `base64/decode-bytes`

Decode a Base64 string to a bytevector. Unlike `base64/decode`, this does not require valid UTF-8.

**Signature:** `(base64/decode-bytes base64-string) → bytevector`

```sema
(base64/decode-bytes "aGVsbG8=")   ; => #u8(104 101 108 108 111)
```

### Use cases

**Data URIs:**

```sema
(string-append "data:image/png;base64," (base64/encode-bytes (file/read-bytes "icon.png")))
```

**API authentication (Basic Auth):**

```sema
(define auth-header
  (string-append "Basic " (base64/encode (string-append username ":" password))))
```

## Hashing

Cryptographic hash functions that return hex-encoded strings.

::: warning Security note
**MD5** is cryptographically broken — do not use it for passwords, signatures, or any security-sensitive purpose. Use `hash/sha256` or `hash/hmac-sha256` instead. MD5 is still fine for checksums and non-security uses (cache keys, deduplication).
:::

### `hash/sha256`

Compute the SHA-256 hash of a string. Returns a 64-character hex string.

**Signature:** `(hash/sha256 string) → string`

```sema
(hash/sha256 "hello")
; => "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
```

### `hash/md5`

Compute the MD5 hash of a string. Returns a 32-character hex string.

**Signature:** `(hash/md5 string) → string`

```sema
(hash/md5 "hello")   ; => "5d41402abc4b2a76b9719d911017c592"
```

### `hash/hmac-sha256`

Compute an HMAC-SHA256 message authentication code. Returns a 64-character hex string.

**Signature:** `(hash/hmac-sha256 key message) → string`

```sema
(hash/hmac-sha256 "secret-key" "message")
; => "hex-encoded-hmac..."
```

**Webhook verification example:**

```sema
;; Verify a webhook signature from a provider
(define (verify-webhook payload secret signature)
  (equal? (hash/hmac-sha256 secret payload) signature))
```
