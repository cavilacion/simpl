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
m := 0;
n := 1000;
x := array(1000);
i := 0;
while (i < n) {
  sample ~ normal (100,20);
  x[i] := sample;
  m    := m + sample/n;
  i:=i+1
};
var := 0;
i := 0;
while (i < n) {
  var := var + (m - x[i])^2/(n-1);
  i := i + 1
};
return (m,var)
HERE
))
  (exec code))
