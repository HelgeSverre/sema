; Sema highlight queries for Helix
; Helix uses LAST-MATCH-WINS semantics, so generic fallbacks go FIRST
; and increasingly specific patterns follow.
; See: https://docs.helix-editor.com/themes.html for capture names.

; =====================================================================
; GENERIC FALLBACKS (least specific — put first so they lose to later)
; =====================================================================

; --- Fallback: any remaining symbol is a variable ---

(symbol) @variable

; --- Generic function call ---

(list
  .
  (symbol) @function)

; =====================================================================
; INCREASINGLY SPECIFIC PATTERNS (override the fallbacks above)
; =====================================================================

; --- Literals ---

(number) @constant.numeric
(boolean) @constant.builtin.boolean
(character) @constant.character
(string) @string
(escape_sequence) @constant.character.escape

; --- Comments ---

(comment) @comment.line
(block_comment) @comment.block

; --- Punctuation ---

["(" ")" "[" "]" "{" "}"] @punctuation.bracket

; --- Dot as punctuation (variadic args, dotted pairs) ---

((symbol) @punctuation.delimiter
  (#eq? @punctuation.delimiter "."))

; --- Ellipsis ---

((symbol) @variable.builtin
  (#eq? @variable.builtin "..."))

; --- Operators ---

((symbol) @operator
  (#any-of? @operator
    "+" "-" "*" "/" "%" "=" ">" "<" ">=" "<="
    "eq?" "equal?" "eqv?"))

; --- Keyword literals :foo (map keys, keyword args) ---

(keyword) @string.special.symbol

; --- Keyword accessor in call position: (:name person) ---

(list
  .
  (keyword) @function.method)

; --- nil ---

((symbol) @constant.builtin
  (#eq? @constant.builtin "nil"))

; --- Quote operators ---

(quote "'" @operator)
(quasiquote "`" @operator)
(unquote "," @operator)
(unquote_splicing ",@" @operator)

; --- Define simple variable: (define name value) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @variable
  (#eq? @_f "define"))

; --- set! target: (set! name value) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @variable
  (#eq? @_f "set!"))

; --- Define function: (defun name ...) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @function
  (#eq? @_f "defun"))

; --- Define macro: (defmacro name ...) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @function
  (#eq? @_f "defmacro"))

; --- Define agent: (defagent name ...) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @function
  (#eq? @_f "defagent"))

; --- Define tool: (deftool name ...) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @function
  (#eq? @_f "deftool"))

; --- Lambda / fn parameter lists ---

(list
  .
  (symbol) @_f
  .
  (list
    (symbol) @variable.parameter)
  (#any-of? @_f "lambda" "fn"))

; --- let-binding variables ---

(list
  .
  (symbol) @_f
  .
  (list
    (list
      (symbol) @variable.parameter))
  (#any-of? @_f "let" "let*" "letrec" "do"))

; --- Define with inline parameter list: (define (name args...) body) ---

(list
  .
  (symbol) @_f
  .
  (list
    .
    (symbol) @function
    (symbol) @variable.parameter)
  (#eq? @_f "define"))

; --- Core builtin functions (fallback when LSP semantic tokens unavailable) ---
; The full set of builtins is classified by the LSP via semantic tokens.
; This curated list covers the most common functions so files look reasonable
; when editing without the LSP (e.g., quick file viewing).

(list
  .
  (symbol) @function.builtin
  (#any-of? @function.builtin
    ; Higher-order / functional
    "map" "filter" "foldl" "foldr" "reduce" "for-each" "apply" "flat-map"
    ; I/O
    "display" "print" "println" "format" "read" "read-line"
    ; Lists
    "list" "cons" "car" "cdr" "first" "rest" "nth"
    "append" "reverse" "length" "sort" "range"
    ; Hash maps
    "hash-map" "get" "assoc" "keys" "vals" "merge"
    ; Type predicates
    "number?" "string?" "symbol?" "pair?" "boolean?" "nil?" "list?" "map?"
    ; Conversions
    "string->number" "number->string" "string->symbol" "symbol->string"
    ; Math
    "abs" "min" "max" "round" "floor" "ceiling" "sqrt"
    ; Strings
    "string-append" "substring" "string-length"
    ; Misc
    "not" "error" "gensym" "type"))

; --- Threading macros ---

(list
  .
  (symbol) @keyword.operator
  (#any-of? @keyword.operator
    "->" "->>" "as->"))

; --- Module / import ---

(list
  .
  (symbol) @keyword.import
  (#any-of? @keyword.import
    "import" "module" "export" "load"))

; --- Exception handling ---

(list
  .
  (symbol) @keyword.exception
  (#any-of? @keyword.exception
    "try" "catch" "throw"))

; --- Conditionals ---

(list
  .
  (symbol) @keyword.conditional
  (#any-of? @keyword.conditional
    "if" "cond" "case" "when" "unless" "else"))

; --- LLM-specific special forms ---

(list
  .
  (symbol) @keyword.function
  (#any-of? @keyword.function
    "defagent" "deftool"))

; --- Sema keywords / special forms (most specific — last wins) ---

(list
  .
  (symbol) @keyword
  (#any-of? @keyword
    "define" "defun" "lambda" "fn" "set!"
    "let" "let*" "letrec" "begin" "do"
    "and" "or"
    "quote" "quasiquote" "unquote" "unquote-splicing"
    "define-record-type" "defmacro"
    "delay" "force" "eval" "macroexpand"
    "with-budget"
    "prompt" "message"))
