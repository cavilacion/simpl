#lang br
(require simpl/parser simpl/tokenizer simpl/expander brag/support rackunit)

(define expand (compose expand-prgrm))

(module+ test
  (test-case
   "Test array()"
   (define expected
     '(seq (assign x (array 5 (0 0 0 0 0)))
           (return x)))
   (check-equal? 
    (expand (parse-to-datum (apply-tokenizer make-tokenizer "x := array(5); return x")))
    expected))

  (test-case
   "Test {1,2,3,4}"
   (define expected
     '(seq (assign x (array 4 (1 2 3 4)))
           (return x)))
   (check-equal? 
    (expand (parse-to-datum (apply-tokenizer make-tokenizer "x := {1,2,3,4}; return x")))
    expected))

  (test-case
   "Test assignment to array"
   (define expected
     '(seq (assign x (array 4 (0 0 0 0)))
           (seq (assign-array x 2 42)
                (return x))))
   (check-equal? 
    (expand (parse-to-datum (apply-tokenizer make-tokenizer "x := array(4); x[2] := 42; return x")))
    expected))

  (test-case
   "Test sampling in array"
   (define expected
     '(seq (assign x (array 4 (1 2 3 4)))
           (seq (sample-array x 3 normal (10 0.5))
                (return x))))
   (check-equal? 
    (expand (parse-to-datum (apply-tokenizer make-tokenizer "x := {1,2,3,4}; x[3]~normal(10,0.5);return x")))
    expected))

  (test-case
   "Test assignment with array"
   (define expected
     '(assign y (acc-arr x 2)))
   (check-equal? 
    (expand (parse-to-datum (apply-tokenizer make-tokenizer "y := x[2]")))
    expected)))
