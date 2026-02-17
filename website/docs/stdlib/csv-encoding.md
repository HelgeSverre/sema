---
outline: [2, 3]
---

# CSV, Crypto & Encoding

## CSV

### `csv/parse`

Parse a CSV string into a list of lists (rows of fields).

```scheme
(csv/parse "a,b\n1,2\n3,4")
; => (("a" "b") ("1" "2") ("3" "4"))
```

### `csv/parse-maps`

Parse a CSV string into a list of maps, using the first row as keys.

```scheme
(csv/parse-maps "name,age\nAda,36\nBob,25")
; => ({:age "36" :name "Ada"} {:age "25" :name "Bob"})
```

### `csv/encode`

Encode a list of lists into a CSV string.

```scheme
(csv/encode '(("a" "b") ("1" "2")))
; => "a,b\n1,2\n"
```

## UUID

### `uuid/v4`

Generate a random UUID v4 string.

```scheme
(uuid/v4)   ; => "550e8400-e29b-41d4-a716-446655440000" (varies)
```

## Base64 Encoding

### `base64/encode`

Encode a string to Base64.

```scheme
(base64/encode "hello")   ; => "aGVsbG8="
(base64/encode "")        ; => ""
```

### `base64/decode`

Decode a Base64 string.

```scheme
(base64/decode "aGVsbG8=")   ; => "hello"
```

### `base64/encode-bytes`

Encode a bytevector to Base64.

```scheme
(base64/encode-bytes (file/read-bytes "image.png"))   ; => "iVBORw0KGgo..."
```

### `base64/decode-bytes`

Decode a Base64 string to a bytevector.

```scheme
(base64/decode-bytes "aGVsbG8=")   ; => #u8(104 101 108 108 111)
```

## Hashing

### `hash/sha256`

Compute the SHA-256 hash of a string. Returns a hex-encoded string.

```scheme
(hash/sha256 "hello")   ; => "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
```

### `hash/md5`

Compute the MD5 hash of a string. Returns a hex-encoded string.

```scheme
(hash/md5 "hello")   ; => "5d41402abc4b2a76b9719d911017c592"
```

### `hash/hmac-sha256`

Compute an HMAC-SHA256 of a message with a secret key. Returns a hex-encoded string.

```scheme
(hash/hmac-sha256 "secret-key" "message")   ; => "hex-encoded-hmac..."
```
