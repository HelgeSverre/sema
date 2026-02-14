---
outline: [2, 3]
---

# HTTP & JSON

## HTTP

HTTP functions make synchronous requests and return a map with `:status`, `:headers`, and `:body` keys.

### `http/get`

Make an HTTP GET request.

```scheme
(http/get "https://httpbin.org/get")
; => {:status 200 :headers {...} :body "..."}
```

### `http/post`

Make an HTTP POST request. Maps in `:body` are automatically serialized as JSON.

```scheme
(http/post "https://httpbin.org/post"
  {:body {:key "value"}
   :headers {"Authorization" "Bearer ..."}})
; => {:status 200 :headers {...} :body "..."}
```

### `http/put`

Make an HTTP PUT request.

```scheme
(http/put "https://example.com/resource"
  {:body "data"})
```

### `http/delete`

Make an HTTP DELETE request.

```scheme
(http/delete "https://example.com/resource/123")
```

### `http/request`

Make a generic HTTP request with full control over method and options.

```scheme
(http/request {:method "PATCH"
               :url "https://example.com/resource"
               :body "data"
               :headers {"Content-Type" "text/plain"}})
```

## JSON

### `json/encode`

Encode a Sema value as a JSON string.

```scheme
(json/encode {:name "Ada" :age 36})
; => "{\"age\":36,\"name\":\"Ada\"}"

(json/encode [1 2 3])
; => "[1,2,3]"
```

### `json/encode-pretty`

Encode a Sema value as a pretty-printed JSON string.

```scheme
(json/encode-pretty {:a 1 :b [2 3]})
; => "{\n  \"a\": 1,\n  \"b\": [\n    2,\n    3\n  ]\n}"
```

### `json/decode`

Decode a JSON string into a Sema value.

```scheme
(json/decode "{\"name\":\"Ada\"}")   ; => {:name "Ada"}
(json/decode "[1,2,3]")             ; => (1 2 3)
```
