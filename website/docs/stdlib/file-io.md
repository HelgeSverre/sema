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

### `print-error`

Print to stderr without a trailing newline.

```sema
(print-error "warning: something happened")
```

### `println-error`

Print to stderr with a trailing newline.

```sema
(println-error "error: file not found")
```

### `newline`

Print a newline character.

```sema
(newline)
```

### `read-line`

Read a line of input from stdin.

```sema
(define name (read-line))
```

### `read-stdin`

Read all of stdin as a string (until EOF).

```sema
(define input (read-stdin))
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

Read a file as a list of lines.

```sema
(file/read-lines "data.txt")   ; => ("line 1" "line 2" "line 3")
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
