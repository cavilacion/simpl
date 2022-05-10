#lang br
(require simpl/parser simpl/tokenizer simpl/expander brag/support rackunit)

(define expand (compose expand-prgrm))

(module+ test
  (test-case
    "Test 1: assignment, sequentiality, sampling"
  (define program #<<HERE
x ~ bern(0.8); 
if (x == 0) { 
  x:=1 
} else { 
  x:=0 
}
HERE
    )

  (define expected
    '(seq (sample x bern 0.8)
      (if (equal? x 0) (assign x 1) (assign x 0))))
  (check-equal? 
   (expand (parse-to-datum (apply-tokenizer make-tokenizer program)))
   expected)))
