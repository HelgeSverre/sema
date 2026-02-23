---
outline: [2, 3]
---

# Records

Records are **user-defined, named product types** created with the `define-record-type` special form. They provide constructors, type predicates, and field accessors.

::: tip Records vs Maps
If you need an *open* data shape that's easy to serialize and manipulate generically, use [maps](./maps). If you want a *closed* domain type with a predicate and fixed fields, use records.
:::

## Defining Record Types

### `define-record-type`

Define a new record type, generating a constructor, predicate, and one accessor per field.

```sema
(define-record-type point
  (make-point x y)       ; constructor (positional args)
  point?                  ; predicate
  (x point-x)            ; (field-name accessor-name)
  (y point-y))
```

General syntax:

```sema
(define-record-type <type-name>
  (<constructor> <field-name> ...)
  <predicate>
  (<field-name> <accessor>) ...)
```

### What Gets Defined

For the `point` example above:

| Binding | Signature | Purpose |
|---------|-----------|---------|
| `make-point` | `(x y) → point` | Constructor |
| `point?` | `(value) → bool` | Type predicate |
| `point-x` | `(point) → value` | Field accessor |
| `point-y` | `(point) → value` | Field accessor |

```sema
(define p (make-point 3 4))

(point? p)     ; => #t
(point? 42)    ; => #f

(point-x p)    ; => 3
(point-y p)    ; => 4
```

### Constructor Arity

The constructor is positional — its arity must match exactly:

```sema
(make-point 1 2)      ; ok
(make-point 1)        ; error: wrong arity
(make-point 1 2 3)    ; error: wrong arity
```

### Immutability

Sema records are immutable. To "update" a record, construct a new one:

```sema
(define (move-point p dx dy)
  (make-point (+ (point-x p) dx)
              (+ (point-y p) dy)))

(move-point (make-point 10 20) 5 -2)
; => a new point record with x=15, y=18
```

## Equality

Two records are `equal?` if they have the **same type** and their fields are pairwise `equal?`:

```sema
(define a (make-point 1 2))
(define b (make-point 1 2))
(define c (make-point 9 9))

(equal? a b)   ; => #t  (same type, same fields)
(equal? a c)   ; => #f  (same type, different fields)
```

Records of different types are never equal, even if they have the same field values.

## Introspection

### `record?`

Test if a value is any record instance (of any record type).

```sema
(record? (make-point 3 4))   ; => #t
(record? {:x 3 :y 4})        ; => #f
(record? 42)                 ; => #f
```

### `type`

Return the type of a value as a keyword. For records, returns the record's type name:

```sema
(type (make-point 3 4))   ; => :point
(type [1 2 3])            ; => :vector
(type {:a 1})             ; => :map
```

## Records vs Maps

Both model "structured data", but they serve different purposes.

### Use records when…

- You want a **distinct type**: `person?`, `invoice?`, `token?`
- Your data has a **fixed schema** enforced at construction
- You want named field accessors and clear domain boundaries

### Use maps when…

- You need easy **serialization** (JSON, TOML, etc.)
- You want to add/remove keys dynamically
- You want generic operations like `get`, `assoc`, `merge`, `keys`, `get-in`, `update-in`
- You're interacting with external APIs

::: tip Common pattern
**Maps at the boundary, records internally.** Parse/validate external maps into records early, and convert records back to maps for output.
:::

## Nested Records

Records can contain any values, including other records:

```sema
(define-record-type address
  (make-address line1 city country)
  address?
  (line1 address-line1)
  (city address-city)
  (country address-country))

(define-record-type user
  (make-user id name addr)
  user?
  (id user-id)
  (name user-name)
  (addr user-addr))

(define u
  (make-user 123 "Ada"
    (make-address "12 St James" "London" "UK")))

(user-name u)                    ; => "Ada"
(address-city (user-addr u))     ; => "London"
```

## Pattern Matching with Records

Records don't have a dedicated pattern form, but you can use binding patterns with `when` guards:

```sema
(define (describe v)
  (match v
    (p when (point? p)
       (string-append "point("
                      (number->string (point-x p))
                      ", "
                      (number->string (point-y p))
                      ")"))
    (_ "not a point")))

(describe (make-point 3 4))  ; => "point(3, 4)"
(describe {:x 3 :y 4})       ; => "not a point"
```

You can also match on `type`:

```sema
(define (record-type-name v)
  (match (type v)
    (:point "a point")
    (:person "a person")
    (_ "something else")))
```

## Domain Modeling Example

Use records to represent values that have been validated:

```sema
(define-record-type email
  (make-email value)
  email?
  (value email-value))

(define (parse-email s)
  (if (regex/match? #".+@.+\..+" s)
      (make-email s)
      (error "invalid email")))

(define e (parse-email "ada@example.com"))
(email? e)            ; => #t
(email-value e)       ; => "ada@example.com"
```

## Multiple Record Types

```sema
(define-record-type color
  (make-color r g b)
  color?
  (r color-r)
  (g color-g)
  (b color-b))

(define-record-type person
  (make-person name age)
  person?
  (name person-name)
  (age person-age))

(define red (make-color 255 0 0))
(define ada (make-person "Ada" 36))

(color? red)         ; => #t
(person? ada)        ; => #t
(color? ada)         ; => #f

(color-r red)        ; => 255
(person-name ada)    ; => "Ada"
(type red)           ; => :color
(type ada)           ; => :person
```

## Serialization

Records are **not JSON-encodable** directly. If you need to serialize a record, convert it to a map first:

```sema
(define (point->map p)
  {:x (point-x p) :y (point-y p)})

(json/encode (point->map (make-point 1 2)))
; => "{\"x\":1,\"y\":2}"
```

Similarly, when loading data from JSON or the KV store, convert maps to records after parsing.

## Tips & Edge Cases

- **Accessor type-checking:** calling `point-x` on a non-point value errors
- **Type tag:** the tag returned by `type` is derived from the record type name — `point` → `:point`
- **No generic field access:** you can't use `get` or keyword-as-function on records — use the generated accessors
