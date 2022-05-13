#lang br/quicklang


(provide binom-pdf poisson-pdf)

(define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))
(define comb (lambda (n k) (/ (fac n) (* (fac k) (fac (- n k))))))
(define gamma (lambda (s t) (* (fac (- s 1)) 1)))
(define e 2.7182818284590452353602874713526624977572)

;;; bernoulli (k; p)
(define bern-pdf
  (lambda (k p)
    (* (expt p k) (expt (- 1 p) (- 1 k)))))
(define bern-cdf bern-pdf)

;;; uniform (x; a,b)
(define uniform-pdf
  (lambda (x a b)
    (if (or (< x a) (> x b)) 0 (/ 1 (- b a)))))
(define uniform-cdf
  (lambda (x a b)
    (or (and (< x a) 0)
        (and (> x b) 1)
        (/ (- x a) (- b a)))))

;;; binomial (k; n,p)
(define binom-pdf
  (lambda (k n p)
    (* (comb n k) (* (expt p k) (expt (- 1 p) (- n k))))))
(define binom-cdf
  (lambda (k n p)
    (if (= k 0)
        (binom-pdf 0 n p)
        (+ (binom-pdf k n p) (binom-cdf (- k 1) n p)))))

;;; poisson (k; r)
(define poisson-pdf
  (lambda (k r)
    (* (expt e (- r)) (/ (expt r k) (fac k)))))
(define poisson-cdf
  (lambda (n r)
    (letrec ((S (lambda (k s)
                  (if (= k 0)
                      (+ s (poisson-pdf 0 r))
                      (S (- k 1) (+ s (poisson-pdf k r)))))))
      (S n 0))))
