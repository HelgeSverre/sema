---
outline: [2, 3]
---

# Key-Value Store

Sema includes a persistent, JSON-backed key-value store for storing structured data across sessions. Data is automatically flushed to disk on every write.

::: tip
`kv/open`, `kv/set`, and `kv/delete` require filesystem write capabilities (they are gated by `FS_WRITE`).
:::

## Functions

### `kv/open`

Open (or create) a named KV store backed by a JSON file. If the file exists, its contents are loaded.

```scheme
(kv/open "config" "/path/to/config.json")
```

### `kv/get`

Get a value by key. Returns `nil` if the key doesn't exist.

```scheme
(kv/get "config" "api-key")  ; => "sk-..." or nil
```

### `kv/set`

Set a key-value pair. Values are serialized as JSON (strings, numbers, booleans, lists, maps are all supported). Returns the value.

```scheme
(kv/set "config" "api-key" "sk-...")
(kv/set "config" "retries" 3)
(kv/set "config" "tags" '("a" "b" "c"))
```

### `kv/delete`

Delete a key. Returns `#t` if the key existed, `#f` otherwise.

```scheme
(kv/delete "config" "api-key")  ; => #t
```

### `kv/keys`

List all keys in the store.

```scheme
(kv/keys "config")  ; => ("api-key" "retries" "tags")
```

### `kv/close`

Close a store, flushing any pending data.

```scheme
(kv/close "config")
```

## Example

```scheme
;; Create a persistent store for caching API results
(kv/open "cache" "api-cache.json")

;; Store some data
(kv/set "cache" "user:123" {:name "Alice" :email "alice@example.com"})
(kv/set "cache" "user:456" {:name "Bob" :email "bob@example.com"})

;; Retrieve it
(kv/get "cache" "user:123")
; => {:email "alice@example.com" :name "Alice"}

;; List keys
(kv/keys "cache")
; => ("user:123" "user:456")

;; Clean up
(kv/delete "cache" "user:123")
(kv/close "cache")
```
