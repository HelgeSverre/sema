;;; sema-mode.el --- Major mode for editing Sema files -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Helge Sverre

;; Author: Helge Sverre
;; URL: https://github.com/helgesverre/sema
;; Homepage: https://sema-lang.com
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, lisp

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A major mode for editing Sema (.sema) files — a Lisp dialect with
;; first-class LLM primitives.  Provides syntax highlighting, indentation,
;; and REPL integration.
;;
;; Install:
;;   (add-to-list 'load-path "/path/to/sema/editors/emacs")
;;   (require 'sema-mode)
;;
;; Or with use-package:
;;   (use-package sema-mode
;;     :load-path "/path/to/sema/editors/emacs"
;;     :mode "\\.sema\\'")
;;
;; Homepage: https://sema-lang.com

;;; Code:

(require 'lisp-mode)
(require 'comint)

;; ── Customization ──────────────────────────────────────────────────────

(defgroup sema nil
  "Major mode for Sema, a Lisp with LLM primitives."
  :group 'languages
  :prefix "sema-")

(defcustom sema-program "sema"
  "Path to the Sema interpreter executable."
  :type 'string
  :group 'sema)

;; ── Syntax table ───────────────────────────────────────────────────────

(defvar sema-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Semicolon starts a comment, newline ends it
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; Double quotes for strings
    (modify-syntax-entry ?\" "\"" table)
    ;; Backslash is escape
    (modify-syntax-entry ?\\ "\\" table)
    ;; Parentheses
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    ;; Characters that are part of symbols
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    (modify-syntax-entry ?/ "_" table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?* "_" table)
    (modify-syntax-entry ?+ "_" table)
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?> "_" table)
    (modify-syntax-entry ?= "_" table)
    (modify-syntax-entry ?: "_" table)
    ;; Block comments: #| ... |#
    (modify-syntax-entry ?# "_ 14b" table)
    (modify-syntax-entry ?| "_ 23b" table)
    ;; Quote-like prefixes for proper sexp handling
    (modify-syntax-entry ?' "'" table)
    (modify-syntax-entry ?` "'" table)
    (modify-syntax-entry ?, "'" table)
    table)
  "Syntax table for `sema-mode'.")

;; ── Font-lock (syntax highlighting) ────────────────────────────────────

(defvar sema-special-forms
  '("define" "defun" "lambda" "fn" "if" "cond" "case" "when" "unless"
    "let" "let*" "letrec" "begin" "do" "and" "or"
    "set!" "quote" "quasiquote" "unquote" "unquote-splicing"
    "define-record-type" "defmacro" "defagent" "deftool"
    "try" "catch" "throw"
    "import" "module" "export" "load"
    "delay" "force" "eval" "macroexpand" "with-budget"
    "prompt" "message"
    "else")
  "Sema special forms and core keywords.")

(defvar sema-builtin-functions
  '(;; Threading macros
    "->" "->>" "as->"
    ;; Higher-order
    "map" "filter" "foldl" "foldr" "reduce" "for-each" "apply"
    ;; LLM / Agent
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
    ;; Embedding functions
    "embedding/->list" "embedding/length"
    "embedding/list->embedding" "embedding/ref"
    ;; Tool query functions
    "tool/name" "tool/description" "tool/parameters"
    ;; I/O
    "display" "print" "println" "newline" "format"
    "read" "read-line" "read-many"
    "print-error" "println-error" "read-stdin"
    "not" "error"
    ;; Lists
    "list" "cons" "car" "cdr" "first" "rest" "nth"
    "append" "reverse" "length" "null?" "list?" "member"
    "vector" "sort" "sort-by" "take" "drop" "zip" "flatten"
    "range" "make-list" "flat-map" "take-while" "drop-while"
    "every" "any" "partition" "iota" "last"
    ;; ca*r/cd*r variants
    "caar" "cadr" "cdar" "cddr"
    "caaar" "caadr" "cadar" "caddr" "cdaar" "cdadr" "cddar" "cdddr"
    ;; list/* namespaced functions
    "list/chunk" "list/dedupe" "list/drop-while" "list/group-by"
    "list/index-of" "list/interleave" "list/max" "list/min"
    "list/pick" "list/repeat" "list/shuffle" "list/split-at"
    "list/sum" "list/take-while" "list/unique"
    "list->bytevector" "list->string" "list->vector"
    ;; Additional list functions
    "assq" "assv" "flatten-deep" "frequencies" "interpose" "vector->list"
    ;; Strings
    "string-append" "string/join" "string/split"
    "string/trim" "string/upper" "string/lower"
    "string/contains?" "string/starts-with?" "string/ends-with?"
    "string/replace" "substring" "string-length" "string-ref"
    "string/capitalize" "string/empty?" "string/index-of"
    "string/reverse" "string/repeat"
    "string/pad-left" "string/pad-right"
    "str" "string->keyword" "keyword->string"
    "string->char" "string->float" "string->list" "string->utf8"
    "string-ci=?"
    "string/byte-length" "string/chars" "string/codepoints"
    "string/foldcase" "string/from-codepoints" "string/last-index-of"
    "string/map" "string/normalize" "string/number?"
    "string/title-case" "string/trim-left" "string/trim-right"
    ;; Char functions
    "char->integer" "char->string" "integer->char"
    "char-alphabetic?" "char-ci<?" "char-ci<=?" "char-ci=?"
    "char-ci>?" "char-ci>=?" "char-downcase" "char-lower-case?"
    "char-numeric?" "char-upcase" "char-upper-case?"
    "char-whitespace?" "char<?" "char<=?" "char=?" "char>?" "char>=?"
    ;; Math
    "abs" "min" "max" "round" "floor" "ceiling"
    "sqrt" "expt" "math/remainder" "modulo" "math/gcd" "math/lcm"
    "pow" "log" "sin" "cos" "ceil" "int" "float" "truncate" "mod" "%"
    "math/pow" "math/tan" "math/random" "math/random-int"
    "math/clamp" "math/sign" "math/exp" "math/log10" "math/log2"
    "math/acos" "math/asin" "math/atan" "math/atan2"
    "math/cosh" "math/degrees->radians" "math/infinite?" "math/lerp"
    "math/map-range" "math/nan?" "math/quotient"
    "math/radians->degrees" "math/sinh" "math/tanh"
    ;; Hash maps
    "hash-map" "get" "assoc" "dissoc" "keys" "vals"
    "contains?" "merge" "count" "empty?"
    ;; map/* functions
    "map/entries" "map/filter" "map/from-entries"
    "map/map-keys" "map/map-vals" "map/select-keys" "map/update"
    ;; hashmap/* functions
    "hashmap/new" "hashmap/get" "hashmap/assoc"
    "hashmap/keys" "hashmap/contains?" "hashmap/to-map"
    ;; Predicates
    "number?" "string?" "symbol?" "pair?" "boolean?"
    "procedure?" "char?" "vector?" "map?" "zero?"
    "positive?" "negative?" "equal?" "eq?" "eqv?"
    "integer?" "float?" "keyword?" "nil?" "fn?" "record?" "promise?"
    "bool?" "bytevector?" "even?" "odd?"
    "agent?" "conversation?" "message?" "prompt?" "tool?" "promise-forced?"
    "type"
    ;; Type conversions
    "string->number" "number->string" "string->symbol" "symbol->string"
    ;; File I/O
    "file/read" "file/write" "file/append" "file/exists?"
    "file/delete" "file/list"
    "file/rename" "file/copy" "file/info" "file/mkdir"
    "file/is-directory?" "file/is-file?"
    "file/read-lines" "file/write-lines"
    "file/fold-lines" "file/for-each-line" "file/is-symlink?"
    ;; Path functions
    "path/absolute" "path/basename" "path/dirname"
    "path/extension" "path/join"
    ;; JSON
    "json/decode" "json/encode" "json/encode-pretty"
    ;; HTTP
    "http/get" "http/post" "http/put" "http/delete"
    "http/request"
    ;; Regex
    "regex/match?" "regex/match" "regex/find-all"
    "regex/replace" "regex/replace-all" "regex/split"
    ;; Crypto
    "uuid/v4" "base64/encode" "base64/decode"
    "hash/md5" "hash/sha256" "hash/hmac-sha256"
    ;; DateTime
    "time/now" "time/format" "time/parse"
    "time/date-parts" "time/add" "time/diff"
    ;; CSV
    "csv/parse" "csv/parse-maps" "csv/encode"
    ;; Bitwise
    "bit/and" "bit/or" "bit/xor" "bit/not"
    "bit/shift-left" "bit/shift-right"
    ;; Terminal functions
    "term/style" "term/strip" "term/rgb"
    "term/spinner-start" "term/spinner-stop" "term/spinner-update"
    ;; Bytevector functions
    "bytevector" "make-bytevector" "bytevector-length"
    "bytevector-u8-ref" "bytevector-u8-set!" "bytevector-copy"
    "bytevector-append" "bytevector->list" "utf8->string"
    ;; System
    "env" "shell" "exit" "time-ms" "sleep"
    "sys/args" "sys/cwd" "sys/platform" "sys/set-env" "sys/env-all"
    "sys/arch" "sys/elapsed" "sys/home-dir" "sys/hostname"
    "sys/interactive?" "sys/os" "sys/pid" "sys/temp-dir"
    "sys/tty" "sys/user" "sys/which"
    ;; Meta
    "gensym")
  "Sema built-in standard library functions.")

(defvar sema-font-lock-keywords
  (let ((special-forms-re
         (concat "(" (regexp-opt sema-special-forms 'symbols)))
        (builtins-re
         (concat "(" (regexp-opt sema-builtin-functions 'symbols))))
    `(;; Special forms — after opening paren
      (,special-forms-re 1 font-lock-keyword-face)
      ;; Builtin functions — after opening paren
      (,builtins-re 1 font-lock-builtin-face)
      ;; Keyword literals :foo
      ("\\_<:\\(?:\\sw\\|\\s_\\)+" . font-lock-constant-face)
      ;; Boolean literals
      ("\\_<#[tf]\\_>" . font-lock-constant-face)
      ("\\_<\\(?:true\\|false\\)\\_>" . font-lock-constant-face)
      ;; Character literals #\space #\a etc.
      ("\\_<#\\\\\\(?:space\\|newline\\|tab\\|return\\|nul\\|alarm\\|backspace\\|delete\\|escape\\)\\_>"
       . font-lock-constant-face)
      ("\\_<#\\\\.\\_>" . font-lock-constant-face)
      ;; nil
      ("\\_<nil\\_>" . font-lock-constant-face)
      ;; Numeric literals
      ("\\_<-?[0-9]+\\(?:\\.[0-9]+\\)?\\_>" . font-lock-constant-face)
      ;; define/defun name
      ("(\\(?:define\\|defun\\)\\s-+(?\\(\\(?:\\sw\\|\\s_\\)+\\)"
       1 font-lock-function-name-face)
      ;; defmacro name
      ("(defmacro\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       1 font-lock-function-name-face)
      ;; defagent name
      ("(defagent\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       1 font-lock-function-name-face)
      ;; deftool name
      ("(deftool\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       1 font-lock-function-name-face)
      ;; define-record-type name
      ("(define-record-type\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       1 font-lock-type-face)
      ;; set! target name
      ("(set!\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)"
       1 font-lock-variable-name-face)))
  "Font-lock keywords for `sema-mode'.")

;; ── Indentation ────────────────────────────────────────────────────────

(defvar sema--indent-forms
  '((define . 1) (defun . 1) (lambda . 1) (fn . 1)
    (let . 1) (let* . 1) (letrec . 1)
    (defmacro . 1) (defagent . 1) (deftool . 1) (define-record-type . 1)
    (when . 1) (unless . 1) (with-budget . 1) (module . 1)
    (set! . 1) (import . 1) (delay . 1) (throw . 1)
    (prompt . 1) (message . 1)
    (if . 1) (case . 1) (try . 1)
    (do . 0) (begin . 0) (cond . 0))
  "Alist of Sema forms and their indent levels.")

(defun sema--indent-function (indent-point state)
  "Sema-specific indentation function.
Falls back to `lisp-indent-function' after checking Sema-specific forms."
  (let* ((normal-indent (current-column))
         (containing-sexp (nth 1 state)))
    (when containing-sexp
      (goto-char (1+ containing-sexp))
      (when (looking-at "\\(?:\\sw\\|\\s_\\)+")
        (let* ((sym (intern-soft (match-string 0)))
               (indent (cdr (assq sym sema--indent-forms))))
          (when indent
            (lisp-indent-specform indent state indent-point normal-indent)))))))

;; ── REPL ───────────────────────────────────────────────────────────────

(defun sema-repl ()
  "Start an inferior Sema REPL process."
  (interactive)
  (let ((buffer (make-comint "sema" sema-program)))
    (pop-to-buffer buffer)
    (setq-local comint-prompt-regexp "^sema> *")))

(defun sema-send-region (start end)
  "Send the region between START and END to the Sema REPL."
  (interactive "r")
  (let ((proc (get-buffer-process "*sema*")))
    (unless proc
      (error "No Sema REPL running — start one with M-x sema-repl"))
    (comint-send-string proc (buffer-substring-no-properties start end))
    (comint-send-string proc "\n")))

(defun sema-send-last-sexp ()
  "Send the sexp before point to the Sema REPL."
  (interactive)
  (sema-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun sema-send-buffer ()
  "Send the entire buffer to the Sema REPL."
  (interactive)
  (sema-send-region (point-min) (point-max)))

(defun sema-run-file ()
  "Run the current file with the Sema interpreter."
  (interactive)
  (unless buffer-file-name
    (error "Buffer is not visiting a file"))
  (compile (concat sema-program " " (shell-quote-argument buffer-file-name))))

;; ── Keymap ─────────────────────────────────────────────────────────────

(defvar sema-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-z") #'sema-repl)
    (define-key map (kbd "C-c C-r") #'sema-send-region)
    (define-key map (kbd "C-c C-e") #'sema-send-last-sexp)
    (define-key map (kbd "C-c C-b") #'sema-send-buffer)
    (define-key map (kbd "C-c C-l") #'sema-run-file)
    map)
  "Keymap for `sema-mode'.")

;; ── Major mode definition ──────────────────────────────────────────────

;;;###autoload
(define-derived-mode sema-mode prog-mode "Sema"
  "Major mode for editing Sema files.

Sema is a Lisp dialect with first-class LLM primitives.
See https://sema-lang.com for documentation.

\\{sema-mode-map}"
  :syntax-table sema-mode-syntax-table
  :group 'sema
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+\\s-*")
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'sema--indent-function)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local font-lock-defaults '(sema-font-lock-keywords))
  (setq-local electric-pair-pairs '((?\( . ?\))
                                     (?\[ . ?\])
                                     (?\{ . ?\})
                                     (?\" . ?\")))
  (setq-local imenu-generic-expression
              '(("Functions" "^\\s-*(defun\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
                ("Functions" "^\\s-*(define\\s-+(\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
                ("Variables" "^\\s-*(define\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
                ("Macros" "^\\s-*(defmacro\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
                ("Agents" "^\\s-*(defagent\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
                ("Tools" "^\\s-*(deftool\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1)
                ("Records" "^\\s-*(define-record-type\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)" 1))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sema\\'" . sema-mode))

(provide 'sema-mode)

;;; sema-mode.el ends here
