; Run the entire Sema source file.
(source_file) @run (#set! tag "sema-run")

; Run individual top-level definitions (defn, defun, define, defmacro, defagent, deftool)
(source_file
  (list
    . (symbol) @_f
    . (symbol) @run
    (#any-of? @_f "defun" "defn" "defmacro" "defagent" "deftool" "define")
  ) @_source (#set! tag "sema-run-form"))

; Run any top-level list expression (non-definition)
(source_file
  (list
    . (symbol) @run
    (#not-any-of? @run "defun" "defn" "defmacro" "defagent" "deftool" "define" "define-record-type" "module" "import" "load" "export")
  ) @_source (#set! tag "sema-run-form"))
