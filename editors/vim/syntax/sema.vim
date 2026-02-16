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
syn match semaBuiltin /\v<conversation\/add-message>/
syn match semaBuiltin /\v<conversation\/model>/
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
syn match semaBuiltin /\v<llm\/configure-embeddings>/
syn match semaBuiltin /\v<llm\/current-provider>/
syn match semaBuiltin /\v<llm\/list-providers>/
syn match semaBuiltin /\v<llm\/pricing-status>/
syn match semaBuiltin /\v<llm\/reset-usage>/
syn match semaBuiltin /\v<llm\/set-default>/
syn match semaBuiltin /\v<llm\/set-pricing>/
syn match semaBuiltin /\v<agent\/run>/
syn match semaBuiltin /\v<agent\/max-turns>/
syn match semaBuiltin /\v<agent\/model>/
syn match semaBuiltin /\v<agent\/name>/
syn match semaBuiltin /\v<agent\/system>/
syn match semaBuiltin /\v<agent\/tools>/
syn match semaBuiltin /\v<prompt\/append>/
syn match semaBuiltin /\v<prompt\/messages>/
syn match semaBuiltin /\v<prompt\/set-system>/
syn match semaBuiltin /\v<message\/role>/
syn match semaBuiltin /\v<message\/content>/

" Embedding functions
syn match semaBuiltin /\v<embedding\/[>]list>/
syn match semaBuiltin /\v<embedding\/length>/
syn match semaBuiltin /\v<embedding\/list-[>]embedding>/
syn match semaBuiltin /\v<embedding\/ref>/

" Tool query functions
syn match semaBuiltin /\v<tool\/name>/
syn match semaBuiltin /\v<tool\/description>/
syn match semaBuiltin /\v<tool\/parameters>/

" I/O
syn keyword semaBuiltin display print println newline format read
syn match semaBuiltin /\v<read-line>/
syn match semaBuiltin /\v<read-many>/
syn match semaBuiltin /\v<print-error>/
syn match semaBuiltin /\v<println-error>/
syn match semaBuiltin /\v<read-stdin>/

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

" ca*r/cd*r variants
syn keyword semaBuiltin caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr

" list/* namespaced functions
syn match semaBuiltin /\v<list\/chunk>/
syn match semaBuiltin /\v<list\/dedupe>/
syn match semaBuiltin /\v<list\/drop-while>/
syn match semaBuiltin /\v<list\/group-by>/
syn match semaBuiltin /\v<list\/index-of>/
syn match semaBuiltin /\v<list\/interleave>/
syn match semaBuiltin /\v<list\/max>/
syn match semaBuiltin /\v<list\/min>/
syn match semaBuiltin /\v<list\/pick>/
syn match semaBuiltin /\v<list\/repeat>/
syn match semaBuiltin /\v<list\/shuffle>/
syn match semaBuiltin /\v<list\/split-at>/
syn match semaBuiltin /\v<list\/sum>/
syn match semaBuiltin /\v<list\/take-while>/
syn match semaBuiltin /\v<list\/unique>/
syn match semaBuiltin /list-[>]bytevector/
syn match semaBuiltin /list-[>]string/
syn match semaBuiltin /list-[>]vector/

" Additional list functions
syn keyword semaBuiltin assq assv interpose frequencies
syn match semaBuiltin /\v<flatten-deep>/
syn match semaBuiltin /vector-[>]list/

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
syn match semaBuiltin /string-[>]keyword/
syn match semaBuiltin /keyword-[>]string/
syn match semaBuiltin /\v<string\/byte-length>/
syn match semaBuiltin /\v<string\/chars>/
syn match semaBuiltin /\v<string\/codepoints>/
syn match semaBuiltin /\v<string\/foldcase>/
syn match semaBuiltin /\v<string\/from-codepoints>/
syn match semaBuiltin /\v<string\/last-index-of>/
syn match semaBuiltin /\v<string\/map>/
syn match semaBuiltin /\v<string\/normalize>/
syn match semaBuiltin /\v<string\/number\?>/
syn match semaBuiltin /\v<string\/title-case>/
syn match semaBuiltin /\v<string\/trim-left>/
syn match semaBuiltin /\v<string\/trim-right>/
syn match semaBuiltin /\v<string-ci\=\?>/
syn match semaBuiltin /string-[>]char/
syn match semaBuiltin /string-[>]float/
syn match semaBuiltin /string-[>]list/
syn match semaBuiltin /string-[>]utf8/

" Char functions
syn match semaBuiltin /char-[>]integer/
syn match semaBuiltin /char-[>]string/
syn match semaBuiltin /integer-[>]char/
syn match semaBuiltin /\v<char-alphabetic\?>/
syn match semaBuiltin /\v<char-ci\<\?>/
syn match semaBuiltin /\v<char-ci\<\=\?>/
syn match semaBuiltin /\v<char-ci\=\?>/
syn match semaBuiltin /\v<char-ci[>]\?>/
syn match semaBuiltin /\v<char-ci[>]\=\?>/
syn match semaBuiltin /\v<char-downcase>/
syn match semaBuiltin /\v<char-lower-case\?>/
syn match semaBuiltin /\v<char-numeric\?>/
syn match semaBuiltin /\v<char-upcase>/
syn match semaBuiltin /\v<char-upper-case\?>/
syn match semaBuiltin /\v<char-whitespace\?>/
syn match semaBuiltin /\v<char\<\?>/
syn match semaBuiltin /\v<char\<\=\?>/
syn match semaBuiltin /\v<char\=\?>/
syn match semaBuiltin /\v<char[>]\?>/
syn match semaBuiltin /\v<char[>]\=\?>/

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
syn match semaBuiltin /\v<math\/acos>/
syn match semaBuiltin /\v<math\/asin>/
syn match semaBuiltin /\v<math\/atan>/
syn match semaBuiltin /\v<math\/atan2>/
syn match semaBuiltin /\v<math\/cosh>/
syn match semaBuiltin /\v<math\/degrees-[>]radians>/
syn match semaBuiltin /\v<math\/infinite\?>/
syn match semaBuiltin /\v<math\/lerp>/
syn match semaBuiltin /\v<math\/map-range>/
syn match semaBuiltin /\v<math\/nan\?>/
syn match semaBuiltin /\v<math\/quotient>/
syn match semaBuiltin /\v<math\/radians-[>]degrees>/
syn match semaBuiltin /\v<math\/sinh>/
syn match semaBuiltin /\v<math\/tanh>/

" Hash maps
syn keyword semaBuiltin get assoc dissoc keys vals merge count
syn match semaBuiltin /\v<hash-map>/
syn match semaBuiltin /\v<contains\?>/
syn match semaBuiltin /\v<empty\?>/

" map/* namespaced functions
syn match semaBuiltin /\v<map\/entries>/
syn match semaBuiltin /\v<map\/filter>/
syn match semaBuiltin /\v<map\/from-entries>/
syn match semaBuiltin /\v<map\/map-keys>/
syn match semaBuiltin /\v<map\/map-vals>/
syn match semaBuiltin /\v<map\/select-keys>/
syn match semaBuiltin /\v<map\/update>/

" hashmap/* functions
syn match semaBuiltin /\v<hashmap\/new>/
syn match semaBuiltin /\v<hashmap\/get>/
syn match semaBuiltin /\v<hashmap\/assoc>/
syn match semaBuiltin /\v<hashmap\/keys>/
syn match semaBuiltin /\v<hashmap\/contains\?>/
syn match semaBuiltin /\v<hashmap\/to-map>/

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
syn match semaBuiltin /\v<agent\?>/
syn match semaBuiltin /\v<conversation\?>/
syn match semaBuiltin /\v<message\?>/
syn match semaBuiltin /\v<prompt\?>/
syn match semaBuiltin /\v<tool\?>/
syn match semaBuiltin /\v<promise-forced\?>/

" Conversions (use [>] to match literal > in \v mode)
syn match semaBuiltin /string-[>]number/
syn match semaBuiltin /number-[>]string/
syn match semaBuiltin /string-[>]symbol/
syn match semaBuiltin /symbol-[>]string/
syn match semaBuiltin /string-[>]keyword/
syn match semaBuiltin /keyword-[>]string/

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
syn match semaBuiltin /\v<file\/fold-lines>/
syn match semaBuiltin /\v<file\/for-each-line>/
syn match semaBuiltin /\v<file\/is-symlink\?>/

" Path functions
syn match semaBuiltin /\v<path\/absolute>/
syn match semaBuiltin /\v<path\/basename>/
syn match semaBuiltin /\v<path\/dirname>/
syn match semaBuiltin /\v<path\/extension>/
syn match semaBuiltin /\v<path\/join>/

" JSON
syn match semaBuiltin /\v<json\/decode>/
syn match semaBuiltin /\v<json\/encode>/
syn match semaBuiltin /\v<json\/encode-pretty>/

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
syn match semaBuiltin /\v<sys\/arch>/
syn match semaBuiltin /\v<sys\/elapsed>/
syn match semaBuiltin /\v<sys\/home-dir>/
syn match semaBuiltin /\v<sys\/hostname>/
syn match semaBuiltin /\v<sys\/interactive\?>/
syn match semaBuiltin /\v<sys\/os>/
syn match semaBuiltin /\v<sys\/pid>/
syn match semaBuiltin /\v<sys\/temp-dir>/
syn match semaBuiltin /\v<sys\/tty>/
syn match semaBuiltin /\v<sys\/user>/
syn match semaBuiltin /\v<sys\/which>/

" Terminal functions
syn match semaBuiltin /\v<term\/style>/
syn match semaBuiltin /\v<term\/strip>/
syn match semaBuiltin /\v<term\/rgb>/
syn match semaBuiltin /\v<term\/spinner-start>/
syn match semaBuiltin /\v<term\/spinner-stop>/
syn match semaBuiltin /\v<term\/spinner-update>/

" Bytevector functions
syn keyword semaBuiltin bytevector make-bytevector
syn match semaBuiltin /\v<bytevector-length>/
syn match semaBuiltin /\v<bytevector-u8-ref>/
syn match semaBuiltin /\v<bytevector-u8-set!>/
syn match semaBuiltin /\v<bytevector-copy>/
syn match semaBuiltin /\v<bytevector-append>/
syn match semaBuiltin /bytevector-[>]list/
syn match semaBuiltin /utf8-[>]string/

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
