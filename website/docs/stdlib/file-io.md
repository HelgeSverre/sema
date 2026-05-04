---
outline: [2, 3]
---

# File I/O & Paths

## Console I/O

### `display`

Print a value without a trailing newline.

```sema
(display "no newline")
(display 42)
```

### `println`

Print a value followed by a newline.

```sema
(println "with newline")
(println 42)
```

### `print`

Alias for `display`. Print without a trailing newline.

```sema
(print "also no newline")
```

### `io/print-error`

Print to stderr without a trailing newline.

```sema
(io/print-error "warning: something happened")
```

### `io/println-error`

Print to stderr with a trailing newline.

```sema
(io/println-error "error: file not found")
```

### `newline`

Print a newline character.

```sema
(newline)
```

### `io/read-line`

Read a line of input from stdin (trailing `\n` / `\r\n` stripped).

```sema
(define name (io/read-line))
```

Returns `nil` when stdin is closed (Ctrl-D in cooked mode, end of a piped file). Use this to distinguish "user pressed Enter on an empty line" (returns `""`) from "stdin is exhausted" (returns `nil`).

```sema
(let loop ()
  (let ((line (io/read-line)))
    (cond
      ((nil? line)         (println "(eof)"))
      ((= line "")         (loop))            ; blank line, keep reading
      (else                (println "got: " line) (loop)))))
```

::: warning Breaking change in 1.14.0
Previously `io/read-line` returned `""` on both EOF and empty input, making them indistinguishable. It now returns `nil` on EOF. If you don't want to refactor for this, use `io/eof?` after the call instead.
:::

### `io/read-stdin`

Read all of stdin as a string (until EOF).

```sema
(define input (io/read-stdin))
```

### `io/eof?`

Return `#t` after any stdin read (`io/read-line`, `io/read-stdin`, `io/read-key`) has signalled EOF. Non-breaking alternative to checking `io/read-line` for `nil`.

```sema
(define line (io/read-line))
(when (io/eof?)
  (println "stdin closed"))
```

### `io/flush`

Flush stdout. Useful when writing a prompt without a trailing newline before reading input.

```sema
(display "name> ")
(io/flush)
(define name (io/read-line))
```

## File Operations

### `file/read`

Read the entire contents of a file as a string.

```sema
(file/read "data.txt")   ; => "file contents..."
```

### `file/write`

Write a string to a file, overwriting any existing content.

```sema
(file/write "out.txt" "content")
```

### `file/append`

Append a string to a file.

```sema
(file/append "log.txt" "new line\n")
```

### `file/read-lines`

Read a file as a list of lines. Handles both `\n` and `\r\n` line endings. An empty file returns an empty list.

```sema
(file/read-lines "data.txt")   ; => ("line 1" "line 2" "line 3")
(file/read-lines "empty.txt")  ; => ()
```

### `file/write-lines`

Write a list of strings to a file, one per line.

```sema
(file/write-lines "out.txt" '("a" "b" "c"))
```

### `file/for-each-line`

Iterate over lines of a file, calling a function on each line. Memory-efficient for large files.

```sema
(file/for-each-line "data.txt"
  (fn (line) (println line)))
```

### `file/fold-lines`

Fold over lines of a file with an accumulator. Uses a 256KB buffer for high throughput on large files.

```sema
(file/fold-lines "data.csv"
  (fn (acc line) (+ acc 1))
  0)
; => number of lines
```

### `file/delete`

Delete a file.

```sema
(file/delete "tmp.txt")
```

### `file/rename`

Rename or move a file.

```sema
(file/rename "old.txt" "new.txt")
```

### `file/copy`

Copy a file.

```sema
(file/copy "src.txt" "dst.txt")
```

## Binary File I/O

### `file/read-bytes`

Read a file as a bytevector (binary data).

```sema
(file/read-bytes "image.png")   ; => #u8(137 80 78 71 ...)
```

### `file/write-bytes`

Write a bytevector to a file.

```sema
(file/write-bytes "output.bin" my-bytes)
```

## File Predicates

### `file/exists?`

Test if a file or directory exists.

```sema
(file/exists? "data.txt")   ; => #t or #f
```

### `file/is-file?`

Test if a path is a regular file.

```sema
(file/is-file? "data.txt")   ; => #t
```

### `file/is-directory?`

Test if a path is a directory.

```sema
(file/is-directory? "src/")   ; => #t
```

### `file/is-symlink?`

Test if a path is a symbolic link.

```sema
(file/is-symlink? "link")   ; => #t or #f
```

## Directory Operations

### `file/list`

List entries in a directory.

```sema
(file/list "src/")   ; => ("main.rs" "lib.rs" ...)
```

### `file/mkdir`

Create a directory.

```sema
(file/mkdir "new-dir")
```

### `file/glob`

Find files matching a glob pattern.

```sema
(file/glob "src/**/*.rs")      ; => ("src/main.rs" "src/lib.rs" ...)
(file/glob "*.txt")            ; => ("readme.txt" "notes.txt")
```

### `file/info`

Get file metadata. Returns a map with `:size`, `:modified`, and other keys.

```sema
(file/info "data.txt")   ; => {:size 1234 :modified 1707955200 ...}
```

## Path Manipulation

### `path/join`

Join path components.

```sema
(path/join "src" "main.rs")   ; => "src/main.rs"
(path/join "a" "b" "c.txt")  ; => "a/b/c.txt"
```

### `path/dirname`

Return the directory portion of a path.

```sema
(path/dirname "/a/b/c.txt")   ; => "/a/b"
```

### `path/basename`

Return the filename portion of a path.

```sema
(path/basename "/a/b/c.txt")   ; => "c.txt"
```

### `path/extension`

Return the file extension (without the dot).

```sema
(path/extension "file.rs")     ; => "rs"
(path/extension "Makefile")    ; => ""
```

### `path/absolute`

Return the absolute path.

```sema
(path/absolute ".")   ; => "/full/path/to/current/dir"
```

### `path/ext`

Return the file extension (without the dot).

```sema
(path/ext "file.rs")     ; => "rs"
(path/ext "Makefile")    ; => ""
```

### `path/stem`

Return the filename without extension.

```sema
(path/stem "file.rs")      ; => "file"
(path/stem "archive.tar.gz")  ; => "archive.tar"
```

### `path/dir`

Return the directory portion of a path.

```sema
(path/dir "/a/b/c.txt")   ; => "/a/b"
```

### `path/filename`

Return the filename portion of a path.

```sema
(path/filename "/a/b/c.txt")   ; => "c.txt"
```

### `path/absolute?`

Test if a path is absolute.

```sema
(path/absolute? "/usr/bin")   ; => #t
(path/absolute? "relative")  ; => #f
```
