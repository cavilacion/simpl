#lang racket

(require simpl/parser 
         simpl/tokenizer 
         simpl/expander 
         simpl/sampling
         simpl/substeval
         brag/support 
         rackunit)

(define v (make-immutable-hash))
(define rho random)

(module+ test
  (test-case
   "Bernoulli sampling: 1% failure probability"
   (define S (list 'sample 'x 'bern 0.99))
   (check-equal? (retrieve-val (sample S v rho) 'x) 1))
  
  (test-case
   "Binomial: less than 5% failure probability)"
   (define S (list 'sample 'x 'binom '(300 (/ 2 3))))
   (check-within (retrieve-val (sample S v rho) 'x) 200 20))

  (test-case
   "Poisson sampling: less than 5% failure probability"
   (define S (list 'sample 'x 'poisson 100))
   (check-within (retrieve-val (sample S v rho) 'x) 100 20))
  
  (test-case
   "Normal sampling: less than 5% failure probability"
   (define S (list 'sample 'x 'normal '(300 4)))
   (check-within (retrieve-val (sample S v rho) 'x) 300 4)))
