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

; --- Quote operators ---

(quote "'") @operator
(unquote_splicing ",@") @operator
(unquote ",") @operator
(quasiquote "`") @operator

; --- Special variables ---

((symbol) @variable.builtin
  (#any-of? @variable.builtin "..." "."))

; --- Operators ---

((symbol) @operator
  (#any-of? @operator "+" "-" "*" "/" "=" ">" "<" ">=" "<=" "!=" "eq?" "equal?"))

; --- Keyword literals :foo ---

((symbol) @string.special.symbol
  (#match? @string.special.symbol "^:"))

; --- nil ---

((symbol) @constant.builtin
  (#eq? @constant.builtin "nil"))

; --- Boolean symbols (Scheme grammar parses true/false as symbols) ---

((symbol) @constant.builtin.boolean
  (#any-of? @constant.builtin.boolean "true" "false"))

; --- Define simple variable: (define name value) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @variable
  (#eq? @_f "define"))

; --- Define function: (defun name ...) ---

(list
  .
  (symbol) @_f
  .
  (symbol) @function
  (#eq? @_f "defun"))

; --- Lambda / fn parameter lists ---

(list
  .
  (symbol) @_f
  .
  (list
    (symbol) @variable.parameter)
  (#any-of? @_f "lambda" "fn" "define-values"))

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

; --- Sema builtin functions ---

(list
  .
  (symbol) @function.builtin
  (#any-of? @function.builtin
    ; Higher-order / functional
    "map" "filter" "foldl" "foldr" "reduce" "for-each" "apply"
    ; LLM primitives
    "conversation/new" "conversation/say"
    "llm/complete" "llm/chat" "llm/stream" "llm/send"
    "llm/extract" "llm/classify" "llm/batch" "llm/pmap"
    "llm/embed" "llm/auto-configure" "llm/configure"
    "llm/set-budget" "llm/budget-remaining"
    "llm/define-provider" "llm/last-usage" "llm/session-usage"
    "llm/similarity" "llm/clear-budget"
    "conversation/messages" "conversation/last-reply" "conversation/fork"
    "prompt/append" "prompt/messages" "prompt/set-system"
    "message/role" "message/content"
    "agent/run"
    ; I/O
    "display" "print" "println" "newline" "format"
    "read" "read-line" "read-many"
    ; Lists
    "list" "cons" "car" "cdr" "first" "rest" "nth"
    "append" "reverse" "length" "null?" "list?" "member"
    "vector" "sort" "sort-by" "take" "drop" "zip" "flatten"
    "range" "make-list" "flat-map" "take-while" "drop-while"
    "every" "any" "partition" "last" "iota"
    ; Strings
    "string-append" "string/join" "string/split"
    "string/trim" "string/upper" "string/lower" "string/replace"
    "string/contains?" "string/starts-with?" "string/ends-with?"
    "string/capitalize" "string/empty?" "string/index-of"
    "string/reverse" "string/repeat"
    "string/pad-left" "string/pad-right"
    "str" "substring" "string-length" "string-ref"
    ; Math
    "abs" "min" "max" "round" "floor" "ceiling" "sqrt" "expt"
    "pow" "log" "sin" "cos" "ceil" "int" "float"
    "truncate" "mod" "modulo"
    "math/remainder" "math/gcd" "math/lcm" "math/pow"
    "math/tan" "math/random" "math/random-int" "math/clamp"
    "math/sign" "math/exp" "math/log10" "math/log2"
    ; Hash maps
    "hash-map" "get" "assoc" "keys" "vals"
    "dissoc" "merge" "contains?" "count" "empty?"
    ; Type predicates
    "number?" "string?" "symbol?" "pair?" "boolean?" "procedure?"
    "integer?" "float?" "keyword?" "nil?" "fn?" "map?" "record?" "type"
    "equal?" "eq?" "zero?" "positive?" "negative?"
    "even?" "odd?"
    ; Conversions
    "string->number" "number->string" "string->symbol" "symbol->string"
    "string->keyword" "keyword->string"
    ; File I/O
    "file/read" "file/write" "file/exists?"
    "file/append" "file/delete" "file/list" "file/rename"
    "file/mkdir" "file/info" "file/read-lines" "file/write-lines"
    "file/copy" "file/is-directory?" "file/is-file?"
    ; JSON / HTTP
    "json/decode" "json/encode"
    "http/get" "http/post" "http/put" "http/delete" "http/request"
    ; Regex
    "regex/match?" "regex/match" "regex/find-all"
    "regex/replace" "regex/replace-all" "regex/split"
    ; Crypto
    "uuid/v4" "base64/encode" "base64/decode" "hash/md5" "hash/sha256"
    "hash/hmac-sha256"
    ; DateTime
    "time/now" "time/format" "time/parse" "time/date-parts"
    "time/add" "time/diff"
    ; CSV
    "csv/parse" "csv/encode" "csv/parse-maps"
    ; Bitwise
    "bit/and" "bit/or" "bit/xor" "bit/not"
    "bit/shift-left" "bit/shift-right"
    ; System
    "env" "shell" "exit" "time-ms" "sleep"
    "sys/args" "sys/cwd" "sys/platform"
    ; Misc
    "not" "error" "gensym"))

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
