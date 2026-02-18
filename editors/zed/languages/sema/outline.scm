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
