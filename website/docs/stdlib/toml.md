---
outline: [2, 3]
---

# TOML

Functions for encoding and decoding [TOML](https://toml.io/) data. Sema itself uses TOML for project configuration (`sema.toml`), making these functions useful for both general config parsing and meta-tooling.

## `toml/decode`

`(toml/decode toml-string)` → Sema value

Parse a TOML string into Sema data structures. Tables become maps with keyword keys, arrays become lists, and scalar types map to their native Sema equivalents.

```sema
(toml/decode "[package]\nname = \"my-app\"\nversion = \"1.0.0\"")
; => {:package {:name "my-app" :version "1.0.0"}}
```

### Nested Tables

TOML dotted keys and sub-tables are decoded into nested maps:

```sema
(toml/decode "
[server]
host = \"localhost\"
port = 8080

[server.tls]
enabled = true
cert = \"/path/to/cert.pem\"
")
; => {:server {:host "localhost" :port 8080 :tls {:enabled true :cert "/path/to/cert.pem"}}}
```

### Arrays and Arrays of Tables

Plain arrays become lists. `[[double-bracket]]` arrays of tables become lists of maps:

```sema
(toml/decode "
colors = [\"red\", \"green\", \"blue\"]

[[fruits]]
name = \"apple\"
color = \"red\"

[[fruits]]
name = \"banana\"
color = \"yellow\"
")
; => {:colors ("red" "green" "blue")
;     :fruits ({:color "red" :name "apple"}
;              {:color "yellow" :name "banana"})}
```

### Inline Tables

Inline tables are decoded identically to standard tables:

```sema
(toml/decode "point = { x = 1, y = 2 }")
; => {:point {:x 1 :y 2}}
```

### Datetime Handling

TOML datetime values are converted to strings. This includes offset datetimes, local datetimes, local dates, and local times:

```sema
(toml/decode "created = 2024-01-15T10:30:00Z")
; => {:created "2024-01-15T10:30:00Z"}
```

### Error Handling

Invalid TOML throws a `SemaError`:

```sema
(toml/decode "invalid = ")
; => Error: toml/decode: ...
```

## `toml/encode`

`(toml/encode map)` → TOML string

Serialize a Sema map to a TOML string. The top-level value **must** be a map — passing any other type is an error.

```sema
(toml/encode {:package {:name "my-app" :version "1.0.0"}})
; => "[package]\nname = \"my-app\"\nversion = \"1.0.0\"\n"
```

### Nested Maps

Nested maps become TOML tables:

```sema
(toml/encode {:database {:host "localhost"
                         :port 5432
                         :credentials {:user "admin"
                                       :password "secret"}}})
```

### Error Handling

The top-level value must be a map:

```sema
(toml/encode "hello")
; => Error: toml/encode: top-level value must be a map
```

`nil` values cannot be encoded (TOML has no null):

```sema
(toml/encode {:key nil})
; => Error: toml/encode: cannot encode nil
```

Non-encodable types like functions and records throw errors:

```sema
(toml/encode {:callback println})
; => Error: toml/encode: cannot encode native-fn
```

## Type Mapping

### TOML → Sema (decoding)

| TOML Type | Sema Type | Example |
|-----------|-----------|---------|
| Table | map (keyword keys) | `{:key "val"}` |
| Array | list | `("a" "b" "c")` |
| String | string | `"hello"` |
| Integer | int | `42` |
| Float | float | `3.14` |
| Boolean | bool | `#t` / `#f` |
| Datetime | string | `"2024-01-15T10:30:00Z"` |

### Sema → TOML (encoding)

| Sema Type | TOML Type | Notes |
|-----------|-----------|-------|
| map / hashmap | Table | Keys converted via `key_to_string` |
| list / vector | Array | |
| string | String | |
| int | Integer | |
| float | Float | |
| bool | Boolean | |
| keyword | String | `:foo` → `"foo"` |
| symbol | String | `'foo` → `"foo"` |
| nil | ❌ Error | TOML has no null type |
| function / record | ❌ Error | Not representable in TOML |

## Practical Examples

### Reading a Config File

```sema
(define config (-> "config.toml" file/read toml/decode))

(println "Server:" (map/get-in config [:server :host])
         ":" (map/get-in config [:server :port]))
```

### Updating Config Values

```sema
(define config (-> "config.toml" file/read toml/decode))

;; Update the port and add a new setting
(define updated (-> config
                 (map/assoc-in [:server :port] 9090)
                 (map/assoc-in [:server :debug] true)))

(file/write "config.toml" (toml/encode updated))
```

### Round-Trip

```sema
(define config-str "
[server]
host = \"0.0.0.0\"
port = 3000

[server.cors]
origins = [\"https://example.com\"]
")

(define config (toml/decode config-str))
(define new-config (map/assoc-in config [:server :port] 8080))
(toml/encode new-config)
```

## TOML vs JSON

| | TOML | JSON |
|---|------|------|
| **Use case** | Configuration files | Data interchange |
| **Comments** | ✅ Yes | ❌ No |
| **Null type** | ❌ No | ✅ `null` |
| **Date/time** | ✅ Native | ❌ Strings only |
| **Top-level** | Must be a table | Any value |
| **Sema decode** | `toml/decode` | `json/decode` |
| **Sema encode** | `toml/encode` | `json/encode` |

::: tip sema.toml
Sema uses TOML for its own project configuration file (`sema.toml`). You can read and manipulate it programmatically:

```sema
(define project (-> "sema.toml" file/read toml/decode))
(println "Project:" (map/get-in project [:package :name]))
```
:::
