#lang racket

(require simpl/utils
         simpl/parser
         simpl/tokenizer 
         simpl/expander
         simpl/distributions
         brag/support 
         rackunit)

(provide execute-simpl config-stmt config-val config-ss config-w)

(define ns (make-base-namespace))
(struct config (stmt val ss w) #:transparent)

(define N (void)) ; spare stdnormal sampled value

(define execute-simpl
  (lambda (ast)
    (let ((prgrm ast)
          (val (make-immutable-hash))
          (stream random))
      (execute-stmts prgrm val stream 1))))

(define execute-stmts
  (lambda (ast v rho w)
    ;(displayln (format "ast: ~a\nval: ~a\n" ast v))
    (match ast
      ['skip v]
      [(list 'return e)
       (list w (subst-eval v e))]
      [else 
       (let ([step (execute-stmt ast v rho w)])
         (execute-stmts (config-stmt step) 
                        (config-val step)
                        (config-ss step)
                        (config-w step)))])))

(define execute-stmt
  (lambda (ast v rho w)
    (match ast
      [(list 'assign x e)
       (letrec ([e* (subst v e)]
                [rhs (match e*
                       [(list 'array n a) (list 'array n a)]
                       [else (evaluate e*)])]
                [v* (update-val v x rhs)])
         (config 'skip v* rho w))]
      [(list 'assign-array x i e)
       (letrec ([x* (retrieve-val v x)]
                [n* (list-ref x* 1)]
                [a* (list-ref x* 2)]
                [i* (subst-eval v i)]
                [ic (or (and (<= 0 i*) (< i* n*)) (error "index out of bound"))]
                [e* (subst-eval v e)]
                [a (append (take a* i*) (list e*) (drop a* (+ i* 1)))]
                [v* (update-val v x (list 'array n* a))])
         (config 'skip v* rho w))]
      [(list 'sample-array x i d e)
       (let ([S `(seq (sample s* ,d ,e) (assign-array ,x ,i s*))])
         (config S v rho w))]
      [(list 'sample x 'uniform e)
       (if (pair? e) ; uniform(a,b) requires two parameters
           (letrec ([a (subst-eval v (car e))]   
                    [b (subst-eval v (cadr e))]
                    [u (or (and (number? a) (number? b) (< a b) (+ a (* (- b a) (rho))))
                           (error "a < b is required in uniform distributions"))]
                    [v* (update-val v x u)])
             (config 'skip v* rho w))
           (error "uniform distributions require two parameters `uniform(a,b)`"))]
      [(list 'sample x 'bern e)
       (let ([v* (update-val v x (if (< (rho) (subst-eval v e)) 1 0))])
         (config 'skip v* rho w))]
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
                    [X (loop 0 (binom-pdf 0 n p))]
                    [v* (update-val v x X)])
             (config 'skip v* rho w))
           (error "binomial distributions require two parameters `binom(n,p)`"))]
      [(list 'sample x 'poisson e)
       (letrec ([r (subst-eval v e)]
                [p (exp (- r))]
                [u (rho)] ; for inverse transform
                [S (lambda (s k)
                     (if (> u s)
                         (S (+ s (poisson-pdf (+ k 1) r)) (+ k 1))
                         k))]
                [X (S (poisson-pdf 0 r) 0)]
                [v* (update-val v x X)])
         (config 'skip v* rho w))]
      [(list 'sample x 'normal params)
       (and (or (pair? params) (error "normal distributions require two parameters `normal(mean,var)`"))
            (let ([mu (car params)]    ; population mean (to be sampled)
                  [var (cadr params)]) ; population variance (to be sampled)
              (if (void? N) 
                  (letrec ([U (rho)] ; draw two uniform samples to perform Box-Muller
                           [V (rho)] 
                           [X (* (sqrt (* -2 (log U))) (cos (* 2 (* pi V))))]
                           [Y (* (sqrt (* -2 (log U))) (sin (* 2 (* pi V))))]
                           [v* (update-val v x (+ mu (* X (sqrt var))))])
                    (set! N Y)
                    (config 'skip v* rho w))
                  (let ([v* (update-val v x (+ mu (* N (sqrt var))))])
                    ; we use the old sample and remove it
                    (set! N (void))
                    (config 'skip v* rho w)))))]
      [(list 'sample x 'exp e)
       (letrec ([lbd (subst-eval v e)]
                [u (rho)]
                [X (- (* (/ 1 lbd) (log (- 1 u))))]
                [v* (update-val x X)])
         (config 'skip v* rho w))]
      [(list 'seq 'skip S)
       (config S v rho w)]
      [(list 'seq S T)
       (letrec ([cnf (execute-stmt S v rho w)]
                [S*   (config-stmt cnf)]
                [v*   (config-val  cnf)]
                [rho* (config-ss   cnf)]
                [w*   (config-w    cnf)])
         (config (list 'seq S* T) v* rho* w*))]
      [(list 'if e S)
       (if (subst-eval v e)
           (config S v rho w)
           (config 'skip v rho w))]
      [(list 'if e S1 S2)
       (if (subst-eval v e)
           (config S1 v rho w)
           (config S2 v rho w))]
      [(list 'while e S)
       (let ([T (list 'if e (list 'seq S (list 'while e S)) 'skip)])
         (config T v rho w))]
      [(list 'score e)
       (config 'skip v rho (* w (subst-eval v e)))]
      [(list (list 'return e))
       (subst-eval v e)])))

(define subst
  (lambda (val e)
    ;(displayln (format "val in subtst-eval: ~a\ne in subst-eval: ~a\n" val e))
    (match e
      [(list 'array n values)
       (list 'array n (subst val values))]
      [(list 'acc-arr x e)
       (letrec ((ar (retrieve-val val x))
                (i (subst val e))
                (ic (or (and (<= 0 i) (< i (list-ref ar 1))) (error "index out of bound"))))
         (subst val (list-ref (list-ref ar 2) i)))]
      [(list (? bin-op? op) left right)
       (list op (subst val left) (subst val right))]
      [(list (? un-op? op) opnd)
       (eval (list op (subst val opnd)) ns)]
      [(? list? x) (map (lambda (y) (subst val y)) x)]
      [(? number? x) x]
      [(? boolean? x) x]
      [(? string? x) x]
      [(? symbol? x) (retrieve-val val x)])))

(define evaluate
  (lambda (e)
    (match e
      [(list 'array n values) (evaluate values)]
      [(list (? bin-op? op) _ _) (eval e ns)]
      [(list (? un-op? op) _) (eval e ns)]
      [(? list? tuple) (map evaluate tuple)]
      [(? number? x) x]
      [(? boolean? x) x]
      [(? string? x) x])))

(define subst-eval
  (lambda (v e)
    (evaluate (subst v e))))

(define update-val
  (lambda (val x e)
    (hash-set val x e)))
(define retrieve-val
  (lambda (val x)
    (hash-ref val x)))

(define exec
  (lambda (x)
    (execute-simpl (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))

(define p "x:={3,3,42,4}; y:=x[2]; return y")