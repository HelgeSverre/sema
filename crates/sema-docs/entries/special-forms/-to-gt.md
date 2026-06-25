---
name: "->"
module: "special-forms"
syntax: "(-> val form ...)"
---

`->` is the thread-first macro. It inserts `val` as the first argument of each successive `form`, creating a left-to-right pipeline. A bare symbol `f` is treated as `(f)`. The macro expands recursively at macro-expansion time into ordinary nested calls, so there is no runtime overhead. Thread-first is especially useful for data transformations, building nested structures, and drilling into maps or records, because most accessor and update functions take the data as their primary argument.

```sema
(-> 5 (+ 3) (* 2))
;; => 16
```

You can mix function calls and bare symbols. The following example drills into nested map keys after decoding JSON:

```sema
(-> response :body json/decode :data :users)
;; equivalent to (:users (:data (json/decode (:body response))))
```

Because the threaded value always appears first, `->` pairs naturally with functions that take the data as their primary argument, such as `assoc`, `string/trim`, and `string/upper`:

```sema
(-> {} (assoc :name "Ada") (assoc :age 36) (get :name))
;; => "Ada"
```

**Choosing `->` vs `->>`:** thread-*first* fits maps/records/strings (data-first APIs); thread-*last* (`->>`) fits `map`/`filter`/`fold`, which take the collection as their *last* argument. Using `->` with `map` would insert the list where the function belongs and fail.

**Note:** `->` is a prelude macro, so it is available automatically without an import.
