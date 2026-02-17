---
outline: [2, 3]
---

# Terminal Styling

## Text Styling

### Individual Style Functions

These wrap text in ANSI escape codes and return the styled string.

**Modifiers:** `term/bold`, `term/dim`, `term/italic`, `term/underline`, `term/inverse`, `term/strikethrough`

**Colors:** `term/black`, `term/red`, `term/green`, `term/yellow`, `term/blue`, `term/magenta`, `term/cyan`, `term/white`, `term/gray`

```scheme
(term/bold "important")     ; => bold text
(term/red "error")          ; => red text
(println (term/green "success"))
```

### `term/style`

Apply multiple styles at once using keywords.

```scheme
(term/style "warning" :bold :yellow)
(term/style "error" :bold :red :underline)
```

Available keywords: `:bold`, `:dim`, `:italic`, `:underline`, `:inverse`, `:strikethrough`, `:black`, `:red`, `:green`, `:yellow`, `:blue`, `:magenta`, `:cyan`, `:white`, `:gray`

### `term/rgb`

Apply 24-bit true color to text. Takes text, red, green, blue (0-255).

```scheme
(term/rgb "custom color" 255 128 0)   ; => orange text
```

### `term/strip`

Remove ANSI escape sequences from a string.

```scheme
(term/strip (term/bold "hello"))   ; => "hello"
```

## Spinners

Animated terminal spinners for long-running operations.

### `term/spinner-start`

Start a spinner with a message. Returns a spinner ID.

```scheme
(define id (term/spinner-start "Processing..."))
```

### `term/spinner-update`

Update the spinner message while it's running.

```scheme
(term/spinner-update id "Still processing...")
```

### `term/spinner-stop`

Stop a spinner. Optionally show a final status.

```scheme
(term/spinner-stop id)

;; With final status
(term/spinner-stop id {:symbol "✔" :text "Done"})
```
