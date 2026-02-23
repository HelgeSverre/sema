---
outline: [2, 3]
---

# Terminal Styling

Functions for styling terminal output with ANSI escape codes, true color, and animated spinners.

All style functions take a string and return a new string wrapped in ANSI escape sequences. The styled text is reset after the content, so styles don't bleed into subsequent output.

::: tip Terminal output
Styled output renders correctly in terminals that support ANSI escape codes. When piping or redirecting output (e.g., to a file), the raw escape sequences are included in the output. Use `term/strip` to produce clean text for non-terminal destinations.
:::

## Modifiers

Modifier functions change how text is displayed without altering its color.

### `term/bold`

Render text in **bold** (increased intensity).

```sema
(term/bold "important")
(println (term/bold "Warning: check your input"))
```

### `term/dim`

Render text with decreased intensity.

```sema
(term/dim "less important")
```

### `term/italic`

Render text in *italic*.

```sema
(term/italic "emphasis")
```

### `term/underline`

Render text with an underline.

```sema
(term/underline "click here")
```

### `term/inverse`

Swap foreground and background colors.

```sema
(term/inverse "highlighted")
```

### `term/strikethrough`

Render text with a ~~strikethrough~~.

```sema
(term/strikethrough "deprecated")
```

## Colors

Color functions set the foreground (text) color.

### `term/black`

```sema
(term/black "dark text")
```

### `term/red`

```sema
(term/red "error message")
```

### `term/green`

```sema
(term/green "success")
```

### `term/yellow`

```sema
(term/yellow "warning")
```

### `term/blue`

```sema
(term/blue "info")
```

### `term/magenta`

```sema
(term/magenta "special")
```

### `term/cyan`

```sema
(term/cyan "highlight")
```

### `term/white`

```sema
(term/white "bright text")
```

### `term/gray`

```sema
(term/gray "muted text")
```

## Combined Styles

### `term/style`

Apply multiple styles at once using keywords. The first argument is the text, followed by one or more style keywords.

```sema
(term/style "danger" :bold :red)
(term/style "notice" :italic :yellow :underline)
(term/style "subtle" :dim :gray)
```

Internally, `term/style` combines ANSI codes with `;` separators into a single escape sequence (e.g., `ESC[1;31m` for bold red), which is more efficient than nesting individual style functions.

If called with no style keywords, the text is returned unstyled.

```sema
(term/style "plain text")   ; => "plain text" (no ANSI codes)
```

An unknown keyword produces an error:

```sema
(term/style "text" :blink)  ; Error: unknown style keyword :blink
```

#### Style keyword reference

| Keyword          | Effect         | ANSI Code |
|------------------|----------------|-----------|
| `:bold`          | Bold           | 1         |
| `:dim`           | Dim            | 2         |
| `:italic`        | Italic         | 3         |
| `:underline`     | Underline      | 4         |
| `:inverse`       | Inverse        | 7         |
| `:strikethrough` | Strikethrough  | 9         |
| `:black`         | Black text     | 30        |
| `:red`           | Red text       | 31        |
| `:green`         | Green text     | 32        |
| `:yellow`        | Yellow text    | 33        |
| `:blue`          | Blue text      | 34        |
| `:magenta`       | Magenta text   | 35        |
| `:cyan`          | Cyan text      | 36        |
| `:white`         | White text     | 37        |
| `:gray`          | Gray text      | 90        |

### Composing Styles

There are two ways to combine styles:

**Using `term/style` (recommended):** produces a single escape sequence with combined codes.

```sema
(term/style "alert" :bold :red :underline)
;; Produces: ESC[1;31;4m alert ESC[0m
```

**Nesting individual functions:** each function wraps the text in its own escape sequence. This works but produces more verbose output.

```sema
(term/bold (term/red (term/underline "alert")))
;; Produces: ESC[1m ESC[31m ESC[4m alert ESC[0m ESC[0m ESC[0m
```

Both approaches render identically in terminals, but `term/style` is cleaner.

## True Color

### `term/rgb`

Apply 24-bit true color to text. Takes the text followed by red, green, and blue values (integers 0–255).

```sema
(term/rgb "orange" 255 165 0)
(term/rgb "coral" 255 127 80)
(term/rgb "teal" 0 128 128)
(term/rgb "hot pink" 255 105 180)
```

Uses the `ESC[38;2;r;g;bm` escape sequence format, which is supported by most modern terminals.

```sema
;; Build a gradient
(for-each
  (lambda (i)
    (print (term/rgb "█" (* i 25) 50 (- 255 (* i 25)))))
  (range 11))
(println)
```

## Stripping ANSI Codes

### `term/strip`

Remove all ANSI escape sequences from a string, returning plain text.

```sema
(term/strip (term/bold "hello"))         ; => "hello"
(term/strip (term/style "hi" :red :bold)) ; => "hi"
(term/strip (term/rgb "color" 255 0 0))  ; => "color"
(term/strip "no codes here")            ; => "no codes here"
```

This is useful when you need plain text for logging to files, comparisons, or passing to functions that don't understand ANSI codes:

```sema
;; Write clean text to a file, styled text to terminal
(define msg (term/green "Build succeeded"))
(println msg)                          ; styled on terminal
(file/write "build.log" (term/strip msg))  ; clean in log file
```

## Spinners

Animated terminal spinners for indicating progress during long-running operations. Spinners use braille animation frames (`⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏`) cycling at 80ms intervals, and render to **stderr** so they don't interfere with stdout output.

### `term/spinner-start`

Start a spinner with a message. Returns an integer spinner ID used to update or stop it.

```sema
(define id (term/spinner-start "Loading data..."))
```

### `term/spinner-update`

Update the message displayed next to a running spinner.

```sema
(term/spinner-update id "Processing records...")
(term/spinner-update id "Almost done...")
```

### `term/spinner-stop`

Stop a running spinner and optionally display a final status line. The spinner line is cleared from the terminal before the final status is printed.

**Without options** — just clears the spinner:

```sema
(term/spinner-stop id)
```

**With options map** — displays a final symbol and text:

```sema
(term/spinner-stop id {:symbol "✔" :text "Done"})
```

The options map supports two keys:

| Key       | Type   | Description                          |
|-----------|--------|--------------------------------------|
| `:symbol` | string | Symbol to display (e.g., `"✔"`, `"✗"`, `"⚠"`) |
| `:text`   | string | Final status message                 |

Both keys are optional. The final line is printed to stderr as `symbol text`.

### Spinner Lifecycle Example

```sema
;; Start spinner
(define spinner (term/spinner-start "Fetching data..."))

;; ... do some work ...
(term/spinner-update spinner "Processing results...")

;; ... do more work ...
(term/spinner-update spinner "Writing output...")

;; Stop with success indicator
(term/spinner-stop spinner {:symbol "✔" :text "Complete"})
```

Multiple spinners can run concurrently — each gets a unique ID:

```sema
(define s1 (term/spinner-start "Task A..."))
(define s2 (term/spinner-start "Task B..."))
;; ... work ...
(term/spinner-stop s1 {:symbol "✔" :text "Task A done"})
(term/spinner-stop s2 {:symbol "✔" :text "Task B done"})
```

## Common Patterns

### Colored Log Levels

```sema
(define (log-error msg)   (println (term/style "✗ ERROR" :bold :red) " " msg))
(define (log-warn msg)    (println (term/style "⚠ WARN " :bold :yellow) " " msg))
(define (log-info msg)    (println (term/style "ℹ INFO " :bold :blue) " " msg))
(define (log-success msg) (println (term/style "✔ OK   " :bold :green) " " msg))

(log-error "Connection refused")
(log-warn "Retrying in 5s")
(log-info "Connecting to server")
(log-success "Connected")
```

### CLI Status Output

```sema
(define (print-step label detail)
  (println (term/style label :bold :cyan) " " (term/dim detail)))

(print-step "Compile" "src/main.sema")
(print-step "Link" "3 modules")
(print-step "Write" "build/output")
```

### Progress with Spinners

```sema
(define steps '("Downloading" "Extracting" "Installing" "Configuring"))

(define sp (term/spinner-start "Starting..."))
(for-each
  (lambda (step)
    (term/spinner-update sp (string-append step "..."))
    (sleep 1000))
  steps)
(term/spinner-stop sp {:symbol "✔" :text "Installation complete"})
```

### Conditional Styling

```sema
(define (color-status code)
  (cond
    ((< code 300) (term/green (number->string code)))
    ((< code 400) (term/yellow (number->string code)))
    (else         (term/red (number->string code)))))

(println "Status: " (color-status 200))  ; green "200"
(println "Status: " (color-status 301))  ; yellow "301"
(println "Status: " (color-status 404))  ; red "404"
```
