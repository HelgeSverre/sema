if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal commentstring=;\ %s
setlocal comments=:;
setlocal lisp
setlocal lispwords=define,defun,lambda,fn,if,cond,case,when,unless
setlocal lispwords+=let,let*,letrec,begin,do
setlocal lispwords+=define-record-type,defmacro,defagent,deftool
setlocal lispwords+=try,catch
setlocal lispwords+=module
setlocal lispwords+=set!,throw,import,load,delay,force
setlocal lispwords+=prompt,message
setlocal lispwords+=with-budget
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal expandtab
setlocal iskeyword+=!,?,/,-,>,*,<,#

let b:undo_ftplugin = 'setlocal commentstring< comments< lisp< lispwords<'
      \ . ' tabstop< shiftwidth< softtabstop< expandtab< iskeyword<'
