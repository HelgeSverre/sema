---
name: "file/for-each-line"
module: "file-io"
section: "File Operations"
---

Iterate over the lines of a file, calling a function on each line (newline stripped) for its side effects. Streams one line at a time, so it stays memory-efficient on files too large to hold in RAM — unlike `(for-each f (file/read-lines path))`, which builds the full list first.

Use `file/for-each-line` when you only need side effects (printing, writing elsewhere, counting via mutation); use `file/fold-lines` when you want to thread a running accumulator through.

```sema
;; Print each non-empty line, uppercased.
(file/for-each-line "data.txt"
  (fn (line)
    (unless (= line "")
      (println (string/upper line)))))
```
