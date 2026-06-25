---
name: "as->"
module: "special-forms"
syntax: "(as-> val name form ...)"
---

`as->` is the thread-as macro. It binds `val` to `name`, evaluates the first `form` with that binding, then rebinds `name` to the result and continues with the next form. This allows the threaded value to appear in any argument position, not just first or last. Implemented as a recursive prelude macro, `as->` expands into a sequence of `let` bindings, so each step runs in its own lexical scope with no runtime overhead. It is the escape hatch for pipelines where a single step does not fit the `->` or `->>` convention, or when the value must be used multiple times in one step.

```sema
(as-> 5 x (+ x 3) (* x x) (- x 1))
;; => 63
```

Because `name` is rebound after every step, you can reference the intermediate result arbitrarily:

```sema
(as-> "hello" s
  (string/upper s)
  (string/replace s "L" "X")
  (string/append s "!"))
;; => "HEXXO!"
```

The threaded value can land in any argument slot — even the second argument of `zip`, where `->`/`->>` could not place it:

```sema
(as-> '(1 2 3) xs
  (map (fn (x) (* x 2)) xs)
  (zip '(a b c) xs))
;; => ((a 2) (b 4) (c 6))
```

**Gotcha:** `as->` is itself a binding form (`val name forms...`), so it cannot be used as a *step* inside a `->`/`->>` pipeline — the outer macro would inject the threaded value into the wrong slot. Make `as->` the outermost form and call `map`/`filter` directly inside its steps, as above.

**Note:** `as->` is a prelude macro and is available automatically without an import.
