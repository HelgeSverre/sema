; (defun name ...) / (defmacro name ...) / (defagent name ...) / (deftool name ...)
(list
  .
  (symbol) @_f
  .
  (symbol) @name
  (#any-of? @_f "defun" "defmacro" "defagent" "deftool" "define-record-type")
) @item

; (define (name args...) body)
(list
  .
  (symbol) @_f
  .
  (list
    .
    (symbol) @name)
  (#eq? @_f "define")
) @item

; (define name value)
(list
  .
  (symbol) @_f
  .
  (symbol) @name
  (#eq? @_f "define")
) @item

; (lambda (args) body) / (fn (args) body)
(list
  .
  (symbol) @name
  (#any-of? @name "lambda" "fn")
) @item

; Block forms: let, let*, letrec, begin, do, cond, case, when, unless, try
(list
  .
  (symbol) @name
  (#any-of? @name "let" "let*" "letrec" "begin" "do" "cond" "case" "when" "unless" "try")
) @item

; Module/import forms
(list
  .
  (symbol) @name
  (#any-of? @name "module" "import")
) @item
