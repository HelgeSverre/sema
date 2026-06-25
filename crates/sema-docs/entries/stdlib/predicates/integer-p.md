---
name: "integer?"
module: "predicates"
section: "Numeric Predicates"
---

Test if a value is an integer. This checks the runtime *type*, not the mathematical value — a float like `3.0` is not an integer even though it has no fractional part.

```sema
(integer? 42)     ; => #t
(integer? 3.14)   ; => #f
(integer? 3.0)    ; => #f   (it's a float, not an int)
(integer? "42")   ; => #f
```

Use `number?` to accept ints and floats alike; use `float?` for the float case.
