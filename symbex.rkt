#lang br/quicklang
(require simpl/tokenizer
         simpl/parser
         simpl/expander
         simpl/sym-os
         racket/pretty)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module simpl-parser-mod simpl/symbex
       #,parse-tree)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb AST)
  #'(#%module-begin
     (pretty-print (symbex (expand-prgrm 'AST)))))

(provide (rename-out [parser-only-mb #%module-begin]))
