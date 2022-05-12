#lang br
(require simpl/parser simpl/tokenizer simpl/expander brag/support rackunit)

(define expand (compose expand-prgrm))

(module+ test
  (test-case
    "Test 1: assignment, sequentiality, sampling"
  (define program #<<HERE
x := 0;
y ~ bern(0.5);
score (y/1*1)
HERE
    )

  (define expected
    '(seq (assign x 0)
      (seq (sample y bern 0.5)
           (score (* (/ y 1) 1)))))
  (check-equal? 
   (expand (parse-to-datum (apply-tokenizer make-tokenizer program)))
   expected)))
