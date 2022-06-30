#lang racket

(require simpl/utils
         simpl/parser
         simpl/tokenizer 
         simpl/expander
         simpl/substeval
         simpl/distributions
         brag/support 
         rackunit)

(provide sample)

(define N (void)) ; spare stdnormal sampled value

(define sample
  (lambda (ast v rho)
    (match ast
      [(list 'sample x 'uniform e)
       (if (pair? e) ; uniform(a,b) requires two parameters
           (letrec ([a (subst-eval v (car e))]   
                    [b (subst-eval v (cadr e))]
                    [u (or (and (number? a) (number? b) (< a b) (+ a (* (- b a) (rho))))
                           (error "a < b is required in uniform distributions"))])
             (update-val v x u))
           (error "uniform distributions require two parameters `uniform(a,b)`"))]
      [(list 'sample x 'bern e)
       (update-val v x (if (< (rho) (subst-eval v e)) 1 0))]
      [(list 'sample x 'binom e)
       (if (pair? e) ; binom(n,p) requires two arguments
           (letrec ([n (subst-eval v (car e))]
                    [p (subst-eval v (cadr e))]
                    [u (rho)]
                    [loop (lambda (k s)
                            (if (> s u)
                                k
                                (loop (+ k 1)
                                      (+ s (binom-pdf (+ k 1) n p)))))]
                    [X (loop 0 (binom-pdf 0 n p))])
             (update-val v x X))
           (error "binomial distributions require two parameters `binom(n,p)`"))]
      [(list 'sample x 'poisson e)
       (letrec ([r (subst-eval v e)]
                [p (exp (- r))]
                [u (rho)] ; for inverse transform
                [S (lambda (s k)
                     (if (> u s)
                         (S (+ s (poisson-pdf (+ k 1) r)) (+ k 1))
                         k))]
                [X (S (poisson-pdf 0 r) 0)])
         (update-val v x X))]
      [(list 'sample x 'normal params)
       (and (or (pair? params) (error "normal distributions require two parameters `normal(mean,var)`"))
            (let ([mu (car params)]    ; population mean (to be sampled)
                  [var (cadr params)]) ; population variance (to be sampled)
              (if (void? N) 
                  (letrec ([U (rho)] ; draw two uniform samples to perform Box-Muller
                           [V (rho)] 
                           [X (* (sqrt (* -2 (log U))) (cos (* 2 (* pi V))))]
                           [Y (* (sqrt (* -2 (log U))) (sin (* 2 (* pi V))))])
                    (set! N Y)
                    (update-val v x (+ mu (* X (sqrt var)))))
                  (let ((v* (update-val v x (+ mu (* N (sqrt var))))))
                    ; we use the old sample and remove it
                    (set! N (void))
                    v*))))]
      [(list 'sample x 'exp e)
       (letrec ([lbd (subst-eval v e)]
                [u (rho)]
                [X (- (* (/ 1 lbd) (log (- 1 u))))])
         (update-val v x X))])))
