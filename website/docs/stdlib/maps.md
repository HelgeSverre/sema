---
outline: [2, 3]
---

# Maps & HashMaps

Sema provides two map types: sorted **maps** (BTreeMap-backed, deterministic ordering) and **hashmaps** (for O(1) performance-critical lookups).

## Maps

Maps use curly-brace literal syntax with keyword keys:

```scheme
{:name "Ada" :age 36}   ; map literal
{:a 1 :b 2 :c 3}       ; keywords as keys
```

Keywords are callable — when used as a function, they look up their value in a map:

```scheme
(:name {:name "Ada" :age 36})   ; => "Ada"
```

### `hash-map`

Create a map from key-value pairs.

```scheme
(hash-map :a 1 :b 2)   ; => {:a 1 :b 2}
```

### `get`

Look up a value by key. Works on both maps and hashmaps.

```scheme
(get {:a 1 :b 2} :a)   ; => 1
(get {:a 1 :b 2} :z)   ; => nil
```

### `assoc`

Add or update a key-value pair, returning a new map.

```scheme
(assoc {:a 1} :b 2)     ; => {:a 1 :b 2}
(assoc {:a 1} :a 99)    ; => {:a 99}
```

### `dissoc`

Remove a key, returning a new map. Works on both maps and hashmaps.

```scheme
(dissoc {:a 1 :b 2} :a)                     ; => {:b 2}
(dissoc (hashmap/new :a 1 :b 2) :a)         ; hashmap without :a
```

### `merge`

Merge multiple maps together. Later maps override earlier ones. Works on both maps and hashmaps — the result type matches the first argument.

```scheme
(merge {:a 1} {:b 2} {:c 3})   ; => {:a 1 :b 2 :c 3}
(merge {:a 1} {:a 99})         ; => {:a 99}
(merge (hashmap/new :a 1) {:b 2})  ; hashmap with :a and :b
```

### `keys`

Return the keys of a map as a list.

```scheme
(keys {:a 1 :b 2})   ; => (:a :b)
```

### `vals`

Return the values of a map as a list.

```scheme
(vals {:a 1 :b 2})   ; => (1 2)
```

### `contains?`

Test if a map contains a key.

```scheme
(contains? {:a 1} :a)   ; => #t
(contains? {:a 1} :b)   ; => #f
```

### `count`

Return the number of key-value pairs.

```scheme
(count {:a 1 :b 2})   ; => 2
```

### `map/entries`

Return the entries as a list of key-value pairs.

```scheme
(map/entries {:a 1 :b 2})   ; => ((:a 1) (:b 2))
```

### `map/from-entries`

Create a map from a list of key-value pairs.

```scheme
(map/from-entries '((:a 1) (:b 2)))   ; => {:a 1 :b 2}
```

## Higher-Order Map Operations

### `map/map-vals`

Apply a function to every value in a map.

```scheme
(map/map-vals (fn (v) (* v 2)) {:a 1 :b 2})   ; => {:a 2 :b 4}
```

### `map/map-keys`

Apply a function to every key in a map.

```scheme
(map/map-keys
  (fn (k) (string->keyword (string/upper (keyword->string k))))
  {:a 1})
; => {:A 1}
```

### `map/filter`

Filter entries by a predicate that takes key and value.

```scheme
(map/filter (fn (k v) (> v 1)) {:a 1 :b 2 :c 3})   ; => {:b 2 :c 3}
```

### `map/select-keys`

Select only the given keys from a map.

```scheme
(map/select-keys {:a 1 :b 2 :c 3} '(:a :c))   ; => {:a 1 :c 3}
```

### `map/update`

Update a value at a key by applying a function.

```scheme
(map/update {:a 1} :a (fn (v) (+ v 10)))   ; => {:a 11}
```

## HashMaps

For performance-critical workloads with many keys, use `hashmap` for O(1) lookups instead of the sorted `map`.

### `hashmap/new`

Create a new hashmap from key-value pairs.

```scheme
(hashmap/new :a 1 :b 2 :c 3)   ; create a hashmap
(hashmap/new)                    ; empty hashmap
```

### `hashmap/get`

Look up a value in a hashmap.

```scheme
(hashmap/get (hashmap/new :a 1) :a)   ; => 1
```

### `hashmap/assoc`

Add a key-value pair to a hashmap.

```scheme
(hashmap/assoc (hashmap/new) :a 1)   ; hashmap with :a 1
```

### `hashmap/to-map`

Convert a hashmap to a sorted map.

```scheme
(hashmap/to-map (hashmap/new :b 2 :a 1))   ; => {:a 1 :b 2}
```

### `hashmap/keys`

Return the keys of a hashmap (unordered).

```scheme
(hashmap/keys (hashmap/new :a 1 :b 2))   ; => (:a :b)
```

### `hashmap/contains?`

Test if a hashmap contains a key.

```scheme
(hashmap/contains? (hashmap/new :a 1) :a)   ; => #t
```

### Generic Operations on HashMaps

The generic functions `get`, `assoc`, `dissoc`, `keys`, `vals`, `merge`, `count`, `contains?`, and all `map/*` higher-order operations also work on hashmaps, preserving the hashmap type:

```scheme
(get (hashmap/new :a 1 :b 2) :a)       ; => 1
(assoc (hashmap/new) :x 42)            ; hashmap with :x 42
(dissoc (hashmap/new :a 1 :b 2) :a)    ; hashmap without :a
(merge (hashmap/new :a 1) {:b 2})      ; hashmap with :a and :b
(count (hashmap/new :a 1 :b 2))        ; => 2
(map/map-vals (fn (v) (* v 2)) (hashmap/new :a 1))  ; hashmap with :a 2
(map/filter (fn (k v) (> v 1)) (hashmap/new :a 1 :b 2))  ; hashmap with :b
```
