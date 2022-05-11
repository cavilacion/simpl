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
a ~ normal(300,4);
b ~ normal(300,4);
c ~ normal(300,4);
d ~ normal(300,4);
e ~ normal(300,4);
f ~ normal(300,4);
g ~ normal(300,4);
h ~ normal(300,4);
i ~ normal(300,4);
j ~ normal(300,4);
return (a,b,c,d,e,f,g,h,i,j)
HERE
))
  (exec code))
