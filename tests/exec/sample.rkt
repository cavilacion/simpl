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
   "Bernoulli sampling: 1% failure probability"
   (define p "x ~ bern(0.99); return x")
   (check-equal? (exec p) 1))
  
  (test-case
   "Binomial: less than 5% failure probability)"
   (define p "x ~ binom(2/3,300); return x")
   (check-within (exec p) 200 20))

  (test-case
   "Poisson sampling: less than 5% failure probability"
   (define p "r := 100; x ~ poisson(r); return x")
   (check-within (exec p) 100 20))
  
  (test-case
   "Normal sampling: less than 5% failure probability"
   (define p "x ~ normal (300,4); return x")
   (check-within (exec p) 300 4)))
