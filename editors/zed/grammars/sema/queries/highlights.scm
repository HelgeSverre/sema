; Sema canonical highlight queries for tree-sitter-sema
; Uses LAST-MATCH-WINS ordering: generic fallbacks first, specific overrides last.
; Standard tree-sitter capture names for cross-editor compatibility
; (Zed, Helix, Neovim, etc.)

; =====================================================================
; GENERIC FALLBACKS
; =====================================================================

(symbol) @variable

(list . (symbol) @function)

; =====================================================================
; LITERALS
; =====================================================================

(number) @number
(boolean) @constant.builtin
(character) @constant.character
(string) @string
(escape_sequence) @string.escape
(keyword) @string.special.symbol

; =====================================================================
; COMMENTS
; =====================================================================

(comment) @comment
(block_comment) @comment

; =====================================================================
; PUNCTUATION
; =====================================================================

["(" ")" "[" "]" "{" "}"] @punctuation.bracket

; =====================================================================
; QUOTE OPERATORS
; =====================================================================

; The quote/quasiquote/unquote nodes themselves, target the punctuation
(quote "'" @operator)
(quasiquote "`" @operator)
(unquote "," @operator)
(unquote_splicing ",@" @operator)

; =====================================================================
; OPERATORS
; =====================================================================

(list . (symbol) @operator
  (#any-of? @operator "+" "-" "*" "/" "%" "=" ">" "<" ">=" "<=" "eq?" "equal?" "eqv?"))

; =====================================================================
; KEYWORD ACCESSOR IN CALL POSITION
; =====================================================================

; (:name person) — keyword used as function
(list . (keyword) @function)

; =====================================================================
; SPECIAL SYMBOLS
; =====================================================================

; nil
((symbol) @constant.builtin (#eq? @constant.builtin "nil"))

; dot in dotted pairs
((symbol) @punctuation.delimiter (#eq? @punctuation.delimiter "."))

; ellipsis
((symbol) @variable.builtin (#eq? @variable.builtin "..."))

; =====================================================================
; DEFINITION NAMES
; =====================================================================

; (define name value)
(list . (symbol) @_f . (symbol) @variable (#eq? @_f "define"))

; (set! name value)
(list . (symbol) @_f . (symbol) @variable (#eq? @_f "set!"))

; (defun name ...)
(list . (symbol) @_f . (symbol) @function (#eq? @_f "defun"))

; (defmacro name ...)
(list . (symbol) @_f . (symbol) @function (#eq? @_f "defmacro"))

; (defagent name ...)
(list . (symbol) @_f . (symbol) @function (#eq? @_f "defagent"))

; (deftool name ...)
(list . (symbol) @_f . (symbol) @function (#eq? @_f "deftool"))

; =====================================================================
; PARAMETERS
; =====================================================================

; (lambda (x y) ...) / (fn (x y) ...)
(list . (symbol) @_f . (list (symbol) @variable.parameter)
  (#any-of? @_f "lambda" "fn"))

; (let ((x 1) (y 2)) ...)
(list . (symbol) @_f . (list (list (symbol) @variable.parameter))
  (#any-of? @_f "let" "let*" "letrec" "do"))

; (define (name x y) body)
(list . (symbol) @_f . (list . (symbol) @function (symbol) @variable.parameter)
  (#eq? @_f "define"))

; =====================================================================
; BUILTIN FUNCTIONS
; =====================================================================

(list . (symbol) @function.builtin
  (#any-of? @function.builtin
    ; Higher-order / functional
    "map" "filter" "foldl" "foldr" "reduce" "for-each" "apply"
    ; LLM primitives
    "conversation/new" "conversation/say"
    "conversation/messages" "conversation/last-reply" "conversation/fork"
    "conversation/add-message" "conversation/model"
    "llm/complete" "llm/chat" "llm/stream" "llm/send"
    "llm/extract" "llm/classify" "llm/batch" "llm/pmap"
    "llm/embed" "llm/auto-configure" "llm/configure"
    "llm/set-budget" "llm/budget-remaining"
    "llm/define-provider" "llm/last-usage" "llm/session-usage"
    "llm/similarity" "llm/clear-budget"
    "llm/configure-embeddings" "llm/current-provider" "llm/list-providers"
    "llm/pricing-status" "llm/reset-usage" "llm/set-default" "llm/set-pricing"
    "prompt/append" "prompt/messages" "prompt/set-system"
    "message/role" "message/content"
    "agent/run" "agent/max-turns" "agent/model"
    "agent/name" "agent/system" "agent/tools"
    ; Embedding functions
    "embedding/->list" "embedding/length"
    "embedding/list->embedding" "embedding/ref"
    ; Tool query functions
    "tool/name" "tool/description" "tool/parameters"
    ; I/O
    "display" "print" "println" "newline" "format"
    "read" "read-line" "read-many"
    "print-error" "println-error" "read-stdin"
    ; Lists
    "list" "cons" "car" "cdr" "first" "rest" "nth"
    "append" "reverse" "length" "null?" "list?" "member"
    "vector" "sort" "sort-by" "take" "drop" "zip" "flatten"
    "range" "make-list" "flat-map" "take-while" "drop-while"
    "every" "any" "partition" "last" "iota"
    ; ca*r/cd*r variants
    "caar" "cadr" "cdar" "cddr"
    "caaar" "caadr" "cadar" "caddr" "cdaar" "cdadr" "cddar" "cdddr"
    ; list/* namespaced functions
    "list/chunk" "list/dedupe" "list/drop-while" "list/group-by"
    "list/index-of" "list/interleave" "list/max" "list/min"
    "list/pick" "list/repeat" "list/shuffle" "list/split-at"
    "list/sum" "list/take-while" "list/unique"
    "list->bytevector" "list->string" "list->vector"
    ; Additional list functions
    "assq" "assv" "flatten-deep" "frequencies" "interpose" "vector->list"
    ; Strings
    "string-append" "string/join" "string/split"
    "string/trim" "string/upper" "string/lower" "string/replace"
    "string/contains?" "string/starts-with?" "string/ends-with?"
    "string/capitalize" "string/empty?" "string/index-of"
    "string/reverse" "string/repeat"
    "string/pad-left" "string/pad-right"
    "str" "substring" "string-length" "string-ref"
    "string->keyword" "keyword->string"
    "string->char" "string->float" "string->list" "string->utf8"
    "string-ci=?"
    "string/byte-length" "string/chars" "string/codepoints"
    "string/foldcase" "string/from-codepoints" "string/last-index-of"
    "string/map" "string/normalize" "string/number?"
    "string/title-case" "string/trim-left" "string/trim-right"
    ; Char functions
    "char->integer" "char->string" "integer->char"
    "char-alphabetic?" "char-ci<?" "char-ci<=?" "char-ci=?"
    "char-ci>?" "char-ci>=?" "char-downcase" "char-lower-case?"
    "char-numeric?" "char-upcase" "char-upper-case?"
    "char-whitespace?" "char<?" "char<=?" "char=?" "char>?" "char>=?"
    ; Math
    "abs" "min" "max" "round" "floor" "ceiling" "sqrt" "expt"
    "pow" "log" "sin" "cos" "ceil" "int" "float"
    "truncate" "mod" "modulo"
    "math/remainder" "math/gcd" "math/lcm" "math/pow"
    "math/tan" "math/random" "math/random-int" "math/clamp"
    "math/sign" "math/exp" "math/log10" "math/log2"
    "math/acos" "math/asin" "math/atan" "math/atan2"
    "math/cosh" "math/degrees->radians" "math/infinite?" "math/lerp"
    "math/map-range" "math/nan?" "math/quotient"
    "math/radians->degrees" "math/sinh" "math/tanh"
    ; Hash maps
    "hash-map" "get" "assoc" "keys" "vals"
    "dissoc" "merge" "contains?" "count" "empty?"
    ; map/* functions
    "map/entries" "map/filter" "map/from-entries"
    "map/map-keys" "map/map-vals" "map/select-keys" "map/update"
    ; hashmap/* functions
    "hashmap/new" "hashmap/get" "hashmap/assoc"
    "hashmap/keys" "hashmap/contains?" "hashmap/to-map"
    ; Type predicates
    "number?" "string?" "symbol?" "pair?" "boolean?" "procedure?"
    "integer?" "float?" "keyword?" "nil?" "fn?" "map?" "record?" "type"
    "equal?" "eq?" "zero?" "positive?" "negative?"
    "even?" "odd?" "bool?" "bytevector?" "char?" "vector?" "promise?"
    "agent?" "conversation?" "message?" "prompt?" "tool?" "promise-forced?"
    ; Conversions
    "string->number" "number->string" "string->symbol" "symbol->string"
    ; File I/O
    "file/read" "file/write" "file/exists?"
    "file/append" "file/delete" "file/list" "file/rename"
    "file/mkdir" "file/info" "file/read-lines" "file/write-lines"
    "file/copy" "file/is-directory?" "file/is-file?"
    "file/fold-lines" "file/for-each-line" "file/is-symlink?"
    ; Path functions
    "path/absolute" "path/basename" "path/dirname"
    "path/extension" "path/join"
    ; JSON / HTTP
    "json/decode" "json/encode" "json/encode-pretty"
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
    ; Terminal functions
    "term/style" "term/strip" "term/rgb"
    "term/spinner-start" "term/spinner-stop" "term/spinner-update"
    ; Bytevector functions
    "bytevector" "make-bytevector" "bytevector-length"
    "bytevector-u8-ref" "bytevector-u8-set!" "bytevector-copy"
    "bytevector-append" "bytevector->list" "utf8->string"
    ; System
    "env" "shell" "exit" "time-ms" "sleep"
    "sys/args" "sys/cwd" "sys/platform" "sys/set-env" "sys/env-all"
    "sys/arch" "sys/elapsed" "sys/home-dir" "sys/hostname"
    "sys/interactive?" "sys/os" "sys/pid" "sys/temp-dir"
    "sys/tty" "sys/user" "sys/which"
    ; Misc
    "not" "error" "gensym"))

; =====================================================================
; THREADING MACROS
; =====================================================================

(list . (symbol) @keyword (#any-of? @keyword "->" "->>" "as->"))

; =====================================================================
; MODULE / IMPORT
; =====================================================================

(list . (symbol) @keyword (#any-of? @keyword "import" "module" "export" "load"))

; =====================================================================
; EXCEPTION HANDLING
; =====================================================================

(list . (symbol) @keyword (#any-of? @keyword "try" "catch" "throw"))

; =====================================================================
; CONDITIONALS
; =====================================================================

(list . (symbol) @keyword (#any-of? @keyword "if" "cond" "case" "when" "unless" "else"))

; =====================================================================
; SPECIAL FORMS (most specific — last wins)
; =====================================================================

(list . (symbol) @keyword
  (#any-of? @keyword
    "define" "defun" "lambda" "fn" "set!"
    "let" "let*" "letrec" "begin" "do"
    "and" "or"
    "quote" "quasiquote" "unquote" "unquote-splicing"
    "define-record-type" "defmacro" "defagent" "deftool"
    "delay" "force" "eval" "macroexpand"
    "with-budget" "prompt" "message"))
