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
   (check-equal? (exec p) 3))
   
  (test-case
   "Bernoulli sampling"
   (define p "x ~ bern(0.999); return x")
   (let ((result (exec p)))
     (test-equal? "Pr(success)=99.9%" result 1)))
  
  (test-case
   "Binomial sampling"
   (define p "x ~ binom(2/3,300); return x")
   (let ((result (exec p)))
     (test-within "Pr(fail) is low" result 200 20)))

  (test-case
   "Poisson sampling"
   (define p "r := 100; x ~ poisson(r); return x")
   (test-within "Pr (fail) is low" (exec p) 100 20)))

