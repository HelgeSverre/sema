---
outline: [2, 3]
---

# Date & Time

## Current Time

### `time/now`

Return the current time as a Unix timestamp in seconds (with fractional milliseconds).

```sema
(time/now)   ; => 1707955200.123
```

### `time-ms`

Return the current time as Unix milliseconds (integer).

```sema
(time-ms)   ; => 1707955200123
```

## Formatting & Parsing

### `time/format`

Format a Unix timestamp using a format string. Uses `strftime`-style directives.

```sema
(time/format (time/now) "%Y-%m-%d")        ; => "2025-02-15"
(time/format (time/now) "%H:%M:%S")        ; => "14:30:00"
(time/format (time/now) "%Y-%m-%d %H:%M")  ; => "2025-02-15 14:30"
```

### `time/parse`

Parse a date string into a Unix timestamp using a format string.

```sema
(time/parse "2025-01-15" "%Y-%m-%d")        ; => 1736899200.0
(time/parse "2025-01-15 10:30" "%Y-%m-%d %H:%M")
```

## Date Decomposition

### `time/date-parts`

Decompose a Unix timestamp into a map of date/time components.

```sema
(time/date-parts (time/now))
; => {:year 2025 :month 2 :day 15 :hour 14 :minute 30 :second 0 ...}
```

## Arithmetic

### `time/add`

Add seconds to a timestamp.

```sema
(time/add (time/now) 86400)     ; one day later
(time/add (time/now) 3600)      ; one hour later
(time/add (time/now) -86400)    ; one day earlier
```

### `time/diff`

Compute the difference between two timestamps in seconds.

```sema
(define t1 (time/now))
;; ... some work ...
(define t2 (time/now))
(time/diff t1 t2)   ; => elapsed seconds
```

## Delay

### `sleep`

Sleep for a given number of milliseconds.

```sema
(sleep 1000)   ; sleep for 1 second
(sleep 500)    ; sleep for 500ms
```
