#lang racket

(require simpl/parser 
         simpl/tokenizer 
         simpl/expander 
         simpl/operational-semantics
         brag/support 
         rackunit)

(define exec
  (lambda (x)
    (execute-simpl (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))

(module+ test
  (test-case
   "Assignment then return"
   (define p "x := 42; return x")
   (check-equal? (exec p) 42))
  
  (test-case
   "Assignments and operator precedence in terms"
   (define p "x := 0*2^5; y := 1-16/2^3/2*x+2; z ~ bern(0.01); return y")
   (check-equal? (exec p) 3)))
