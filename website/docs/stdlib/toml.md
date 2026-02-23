---
outline: [2, 3]
---

# TOML

Functions for encoding and decoding [TOML](https://toml.io/) data.

## `toml/decode`

Decode a TOML string into a Sema value. Tables become maps with keyword keys, arrays become lists, and strings, integers, floats, and booleans map to their native Sema types. TOML datetimes are returned as strings.

```sema
(toml/decode "[package]\nname = \"my-app\"\nversion = \"1.0.0\"")
; => {:package {:name "my-app" :version "1.0.0"}}
```

### Nested Tables

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

### Arrays

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
;     :fruits ({:name "apple" :color "red"}
;              {:name "banana" :color "yellow"})}
```

## `toml/encode`

Encode a Sema map as a TOML string. The top-level value must be a map.

```sema
(toml/encode {:package {:name "my-app" :version "1.0.0"}})
; => "[package]\nname = \"my-app\"\nversion = \"1.0.0\"\n"
```

### Nested Maps

```sema
(toml/encode {:database {:host "localhost"
                         :port 5432
                         :credentials {:user "admin"
                                       :password "secret"}}})
```

### Round-Trip

```sema
(def config-str "
[server]
host = \"0.0.0.0\"
port = 3000

[server.cors]
origins = [\"https://example.com\"]
")

(def config (toml/decode config-str))
(def new-config (assoc-in config [:server :port] 8080))
(toml/encode new-config)
```
