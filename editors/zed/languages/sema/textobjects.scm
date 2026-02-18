; Sema textobjects for Zed (Vim mode)

; Function definitions: defun, define, lambda, fn, defmacro
(list
  .
  (symbol) @_f
  .
  (_) @function.inside
  (#any-of? @_f "define" "defun" "lambda" "fn" "defmacro")
) @function.around

; Agent/tool definitions
(list
  .
  (symbol) @_f
  .
  (_) @class.inside
  (#any-of? @_f "defagent" "deftool")
) @class.around

; Comments
(comment) @comment.inside
(comment)+ @comment.around
(block_comment) @comment.inside
(block_comment)+ @comment.around
