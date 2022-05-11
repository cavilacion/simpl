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
   "Array creation"
   (define p "hoi:={3,3,0,4};return hoi")
   (check-equal? (exec p) '(3 3 0 4)))
  (test-case
   "Assignment to array"
   (define p "doei:={3,3,0,4};doei[1]:=42;return doei")
   (check-equal? (exec p) '(3 42 0 4)))
  (test-case
   "Sample to array: less than 5% failure probability"
   (define p "x:={3,3,0,4};x[3] ~ normal(10,2);return x")
   (let ((pr (exec p)))
     (check-equal? (take pr 3) '(3 3 0))
     (check-within (cadddr pr) 10 3)))
  (test-case
   "Accessing array"
   (define p "x:={3,3,42,4}; y:=x[2]; return y")
   (let ((pr (exec p)))
     (check-equal? pr 42))))
