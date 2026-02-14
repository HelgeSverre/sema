---
outline: [2, 3]
---

# Records

Records are user-defined data types with named fields, created via the `define-record-type` special form. They provide constructors, type predicates, and field accessors.

## Defining Record Types

### `define-record-type`

Define a new record type with a constructor, predicate, and field accessors.

```scheme
(define-record-type point
  (make-point x y)       ; constructor
  point?                  ; predicate
  (x point-x)            ; field name and accessor
  (y point-y))
```

The syntax is:

```scheme
(define-record-type <type-name>
  (<constructor> <field-name> ...)
  <predicate>
  (<field-name> <accessor>) ...)
```

This defines:
- **Constructor** — `make-point` creates a new point record
- **Predicate** — `point?` tests if a value is a point
- **Accessors** — `point-x` and `point-y` extract field values

## Usage

```scheme
(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 3 4))

(point? p)       ; => #t
(point? 42)      ; => #f
(point-x p)      ; => 3
(point-y p)      ; => 4
```

## Equality

Records support structural equality with `equal?` — two records of the same type with equal fields are equal.

```scheme
(equal? (make-point 1 2) (make-point 1 2))   ; => #t
(equal? (make-point 1 2) (make-point 3 4))   ; => #f
```

## Introspection

### `record?`

Test if a value is any record instance.

```scheme
(record? (make-point 3 4))   ; => #t
(record? 42)                 ; => #f
```

### `type`

Return the type tag of a record as a keyword.

```scheme
(type (make-point 3 4))   ; => :point
```

## Example: Multiple Record Types

```scheme
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
