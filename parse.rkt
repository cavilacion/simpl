#lang br/quicklang
(require simpl/tokenizer
         simpl/parser
         simpl/expander)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module simpl-parser-mod simpl/parse
       #,parse-tree)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     (expand-prgrm 'PARSE-TREE)))
(provide (rename-out [parser-only-mb #%module-begin]))
