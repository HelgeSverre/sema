---
outline: [2, 3]
---

# Bytevectors

Bytevectors are sequences of unsigned 8-bit integers (0–255), useful for binary data and string encoding.

## Literal Syntax

```sema
#u8(1 2 3)       ; bytevector literal
#u8()            ; empty bytevector
#u8(255 0 128)   ; arbitrary byte values
```

## Construction

### `bytevector`

Create a bytevector from byte values.

```sema
(bytevector 1 2 3)       ; => #u8(1 2 3)
(bytevector)             ; => #u8()
```

### `make-bytevector`

Create a bytevector of a given length, optionally filled with a value.

```sema
(make-bytevector 4)       ; => #u8(0 0 0 0)
(make-bytevector 3 255)   ; => #u8(255 255 255)
```

## Access & Mutation

### `bytevector-length`

Return the length of a bytevector.

```sema
(bytevector-length #u8(1 2 3))   ; => 3
(bytevector-length #u8())        ; => 0
```

### `bytevector-u8-ref`

Return the byte at a given index.

```sema
(bytevector-u8-ref #u8(10 20 30) 1)   ; => 20
(bytevector-u8-ref #u8(10 20 30) 0)   ; => 10
```

### `bytevector-u8-set!`

Set the byte at a given index. Uses copy-on-write — the original bytevector is unchanged.

```sema
(bytevector-u8-set! #u8(1 2 3) 0 9)   ; => #u8(9 2 3)
```

## Copy & Append

### `bytevector-copy`

Copy a slice of a bytevector. `(bytevector-copy bv start end)`.

```sema
(bytevector-copy #u8(1 2 3 4 5) 1 3)   ; => #u8(2 3)
```

### `bytevector-append`

Concatenate bytevectors.

```sema
(bytevector-append #u8(1 2) #u8(3 4))   ; => #u8(1 2 3 4)
```

## List Conversion

### `bytevector->list`

Convert a bytevector to a list of integers.

```sema
(bytevector->list #u8(65 66))   ; => (65 66)
```

### `list->bytevector`

Convert a list of integers to a bytevector.

```sema
(list->bytevector '(1 2 3))   ; => #u8(1 2 3)
```

## String Conversion

### `utf8->string`

Decode a bytevector as a UTF-8 string.

```sema
(utf8->string #u8(104 105))       ; => "hi"
(utf8->string #u8(72 101 108))    ; => "Hel"
```

### `string->utf8`

Encode a string as a UTF-8 bytevector.

```sema
(string->utf8 "hi")     ; => #u8(104 105)
(string->utf8 "Hello")  ; => #u8(72 101 108 108 111)
```
