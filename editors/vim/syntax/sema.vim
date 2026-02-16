if exists('b:current_syntax')
  finish
endif

setlocal iskeyword+=!,?,/,-,>,*,<,#

" ---------- Parens / Brackets -------------------------------------------

syn match semaParens /[(){}\[\]]/

" ---------- Comments ----------------------------------------------------

syn match semaComment /;.*$/ contains=semaTodo
syn keyword semaTodo TODO FIXME XXX HACK NOTE contained

" ---------- Strings & Characters ----------------------------------------

syn region semaString start=/"/ skip=/\\./ end=/"/ contains=semaStringEscape
syn match semaStringEscape /\\./ contained

" ---------- Numbers -----------------------------------------------------

syn match semaNumber /\v<-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?>/
syn match semaNumber /\v<-?\.[0-9]+([eE][+-]?[0-9]+)?>/

" ---------- Booleans & Constants ----------------------------------------

syn match semaBoolean /\v<#[tf]>/
syn keyword semaBoolean true false
syn keyword semaConstant nil

" ---------- Character Literals ------------------------------------------

syn match semaCharacter /\v#\\%(space|newline|tab|return|nul|alarm|backspace|delete|escape)/
syn match semaCharacter /\v#\\./

" ---------- Keywords (colon-prefixed) -----------------------------------

syn match semaKeyword /\v:[a-zA-Z0-9_/!?\-><*]+/

" ---------- Special Forms -----------------------------------------------

syn keyword semaSpecial define defun lambda fn if cond case when unless
syn keyword semaSpecial let let* letrec begin do and or
syn keyword semaSpecial set! quote quasiquote unquote unquote-splicing
syn keyword semaSpecial define-record-type defmacro defagent deftool
syn keyword semaSpecial try catch throw
syn keyword semaSpecial import module export load
syn keyword semaSpecial delay force eval macroexpand with-budget else
syn keyword semaSpecial prompt message

" ---------- Threading Macros --------------------------------------------

syn match semaThreading /\v%(^|[( \t])\zs(-\>|-\>\>|as-\>)\ze%([) \t\n]|$)/

" ---------- Builtin Functions -------------------------------------------

" Higher-order / control
syn keyword semaBuiltin map filter foldl foldr reduce apply
syn match semaBuiltin /\v<for-each>/

" LLM primitives
syn match semaBuiltin /\v<conversation\/new>/
syn match semaBuiltin /\v<conversation\/say>/
syn match semaBuiltin /\v<conversation\/messages>/
syn match semaBuiltin /\v<conversation\/last-reply>/
syn match semaBuiltin /\v<conversation\/fork>/
syn match semaBuiltin /\v<llm\/complete>/
syn match semaBuiltin /\v<llm\/chat>/
syn match semaBuiltin /\v<llm\/stream>/
syn match semaBuiltin /\v<llm\/send>/
syn match semaBuiltin /\v<llm\/extract>/
syn match semaBuiltin /\v<llm\/classify>/
syn match semaBuiltin /\v<llm\/batch>/
syn match semaBuiltin /\v<llm\/pmap>/
syn match semaBuiltin /\v<llm\/embed>/
syn match semaBuiltin /\v<llm\/auto-configure>/
syn match semaBuiltin /\v<llm\/configure>/
syn match semaBuiltin /\v<llm\/set-budget>/
syn match semaBuiltin /\v<llm\/budget-remaining>/
syn match semaBuiltin /\v<llm\/define-provider>/
syn match semaBuiltin /\v<llm\/last-usage>/
syn match semaBuiltin /\v<llm\/session-usage>/
syn match semaBuiltin /\v<llm\/similarity>/
syn match semaBuiltin /\v<llm\/clear-budget>/
syn match semaBuiltin /\v<agent\/run>/
syn match semaBuiltin /\v<prompt\/append>/
syn match semaBuiltin /\v<prompt\/messages>/
syn match semaBuiltin /\v<prompt\/set-system>/
syn match semaBuiltin /\v<message\/role>/
syn match semaBuiltin /\v<message\/content>/

" I/O
syn keyword semaBuiltin display print println newline format read
syn match semaBuiltin /\v<read-line>/
syn match semaBuiltin /\v<read-many>/

" Lists
syn keyword semaBuiltin list cons car cdr first rest nth
syn keyword semaBuiltin append reverse length
syn keyword semaBuiltin vector sort take drop zip flatten range last
syn match semaBuiltin /\v<sort-by>/
syn match semaBuiltin /\v<flat-map>/
syn match semaBuiltin /\v<make-list>/
syn match semaBuiltin /\v<take-while>/
syn match semaBuiltin /\v<drop-while>/
syn keyword semaBuiltin every any partition iota

" Predicates
syn match semaBuiltin /\v<null\?>/
syn match semaBuiltin /\v<list\?>/
syn keyword semaBuiltin member not error

" Strings
syn match semaBuiltin /\v<string-append>/
syn match semaBuiltin /\v<string\/join>/
syn match semaBuiltin /\v<string\/split>/
syn match semaBuiltin /\v<string\/trim>/
syn match semaBuiltin /\v<string\/upper>/
syn match semaBuiltin /\v<string\/lower>/
syn match semaBuiltin /\v<string\/replace>/
syn match semaBuiltin /\v<string\/contains\?>/
syn match semaBuiltin /\v<string\/starts-with\?>/
syn match semaBuiltin /\v<string\/ends-with\?>/
syn match semaBuiltin /\v<string\/capitalize>/
syn match semaBuiltin /\v<string\/empty\?>/
syn match semaBuiltin /\v<string\/index-of>/
syn match semaBuiltin /\v<string\/reverse>/
syn match semaBuiltin /\v<string\/repeat>/
syn keyword semaBuiltin substring
syn match semaBuiltin /\v<string-length>/
syn match semaBuiltin /\v<string-ref>/
syn match semaBuiltin /\v<string\/pad-left>/
syn match semaBuiltin /\v<string\/pad-right>/
syn match semaBuiltin /\v<string-\>keyword>/
syn match semaBuiltin /\v<keyword-\>string>/

" Math
syn keyword semaBuiltin abs min max round floor ceiling sqrt expt
syn keyword semaBuiltin pow log sin cos ceil int float truncate mod modulo
syn match semaBuiltin /\v<math\/remainder>/
syn match semaBuiltin /\v<math\/gcd>/
syn match semaBuiltin /\v<math\/lcm>/
syn match semaBuiltin /\v<math\/pow>/
syn match semaBuiltin /\v<math\/tan>/
syn match semaBuiltin /\v<math\/random>/
syn match semaBuiltin /\v<math\/random-int>/
syn match semaBuiltin /\v<math\/clamp>/
syn match semaBuiltin /\v<math\/sign>/
syn match semaBuiltin /\v<math\/exp>/
syn match semaBuiltin /\v<math\/log10>/
syn match semaBuiltin /\v<math\/log2>/

" Hash maps
syn keyword semaBuiltin get assoc dissoc keys vals merge count
syn match semaBuiltin /\v<hash-map>/
syn match semaBuiltin /\v<contains\?>/
syn match semaBuiltin /\v<empty\?>/

" Type predicates
syn match semaBuiltin /\v<number\?>/
syn match semaBuiltin /\v<string\?>/
syn match semaBuiltin /\v<symbol\?>/
syn match semaBuiltin /\v<pair\?>/
syn match semaBuiltin /\v<boolean\?>/
syn match semaBuiltin /\v<procedure\?>/
syn match semaBuiltin /\v<integer\?>/
syn match semaBuiltin /\v<float\?>/
syn match semaBuiltin /\v<keyword\?>/
syn match semaBuiltin /\v<nil\?>/
syn match semaBuiltin /\v<fn\?>/
syn match semaBuiltin /\v<map\?>/
syn match semaBuiltin /\v<record\?>/
syn match semaBuiltin /\v<equal\?>/
syn match semaBuiltin /\v<eq\?>/
syn match semaBuiltin /\v<zero\?>/
syn match semaBuiltin /\v<positive\?>/
syn match semaBuiltin /\v<negative\?>/
syn match semaBuiltin /\v<even\?>/
syn match semaBuiltin /\v<odd\?>/
syn match semaBuiltin /\v<char\?>/
syn match semaBuiltin /\v<vector\?>/
syn match semaBuiltin /\v<promise\?>/
syn match semaBuiltin /\v<bool\?>/
syn match semaBuiltin /\v<bytevector\?>/

" Conversions
syn match semaBuiltin /\v<string-\>number>/
syn match semaBuiltin /\v<number-\>string>/
syn match semaBuiltin /\v<string-\>symbol>/
syn match semaBuiltin /\v<symbol-\>string>/

" File I/O
syn match semaBuiltin /\v<file\/read>/
syn match semaBuiltin /\v<file\/write>/
syn match semaBuiltin /\v<file\/exists\?>/
syn match semaBuiltin /\v<file\/delete>/
syn match semaBuiltin /\v<file\/list>/
syn match semaBuiltin /\v<file\/append>/
syn match semaBuiltin /\v<file\/rename>/
syn match semaBuiltin /\v<file\/mkdir>/
syn match semaBuiltin /\v<file\/info>/
syn match semaBuiltin /\v<file\/read-lines>/
syn match semaBuiltin /\v<file\/write-lines>/
syn match semaBuiltin /\v<file\/is-directory\?>/
syn match semaBuiltin /\v<file\/is-file\?>/
syn match semaBuiltin /\v<file\/copy>/

" JSON
syn match semaBuiltin /\v<json\/decode>/
syn match semaBuiltin /\v<json\/encode>/

" HTTP
syn match semaBuiltin /\v<http\/get>/
syn match semaBuiltin /\v<http\/post>/
syn match semaBuiltin /\v<http\/put>/
syn match semaBuiltin /\v<http\/delete>/
syn match semaBuiltin /\v<http\/request>/

" Regex
syn match semaBuiltin /\v<regex\/match\?>/
syn match semaBuiltin /\v<regex\/match>/
syn match semaBuiltin /\v<regex\/find-all>/
syn match semaBuiltin /\v<regex\/replace>/
syn match semaBuiltin /\v<regex\/replace-all>/
syn match semaBuiltin /\v<regex\/split>/

" Crypto
syn match semaBuiltin /\v<uuid\/v4>/
syn match semaBuiltin /\v<base64\/encode>/
syn match semaBuiltin /\v<base64\/decode>/
syn match semaBuiltin /\v<hash\/md5>/
syn match semaBuiltin /\v<hash\/sha256>/
syn match semaBuiltin /\v<hash\/hmac-sha256>/

" DateTime
syn match semaBuiltin /\v<time\/now>/
syn match semaBuiltin /\v<time\/format>/
syn match semaBuiltin /\v<time\/parse>/
syn match semaBuiltin /\v<time\/date-parts>/
syn match semaBuiltin /\v<time\/add>/
syn match semaBuiltin /\v<time\/diff>/

" CSV
syn match semaBuiltin /\v<csv\/parse>/
syn match semaBuiltin /\v<csv\/parse-maps>/
syn match semaBuiltin /\v<csv\/encode>/

" Bitwise
syn match semaBuiltin /\v<bit\/and>/
syn match semaBuiltin /\v<bit\/or>/
syn match semaBuiltin /\v<bit\/xor>/
syn match semaBuiltin /\v<bit\/not>/
syn match semaBuiltin /\v<bit\/shift-left>/
syn match semaBuiltin /\v<bit\/shift-right>/

" System
syn keyword semaBuiltin env shell exit sleep
syn match semaBuiltin /\v<time-ms>/
syn match semaBuiltin /\v<sys\/args>/
syn match semaBuiltin /\v<sys\/cwd>/
syn match semaBuiltin /\v<sys\/platform>/
syn match semaBuiltin /\v<sys\/set-env>/
syn match semaBuiltin /\v<sys\/env-all>/

" Meta
syn keyword semaBuiltin gensym type str

" ---------- Quoting shortcuts -------------------------------------------

syn match semaQuote /\v'[^ \t()\[\]{}]+/
syn match semaQuote /\v`[^ \t()\[\]{}]+/
syn match semaUnquote /,@\?/

" ---------- Highlight Links ---------------------------------------------

hi def link semaComment Comment
hi def link semaTodo Todo
hi def link semaString String
hi def link semaStringEscape SpecialChar
hi def link semaNumber Number
hi def link semaBoolean Boolean
hi def link semaConstant Constant
hi def link semaKeyword Type
hi def link semaSpecial Keyword
hi def link semaThreading Keyword
hi def link semaBuiltin Function
hi def link semaQuote Special
hi def link semaUnquote Special
hi def link semaCharacter Character
hi def link semaParens Delimiter

let b:current_syntax = 'sema'
