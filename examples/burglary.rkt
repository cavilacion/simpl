#lang racket

(require simpl/parser 
         simpl/tokenizer 
         simpl/expander 
         simpl/operational-semantics
         brag/support 
         rackunit)

(let ((exec
  (lambda (x)
    (execute-simpl (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))
      (code #<<HERE
b ~ bern(0.0001);
if (b == 1) {
  a := 0.95
} else {
  a := 0.001
};
score (a);
return b
HERE
))
  (exec code))
