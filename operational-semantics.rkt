#lang racket

(require simpl/utils)

(define ns (make-base-namespace))
(struct config (stmt val ss) #:transparent)

(define execute-simpl
  (lambda (ast)
    (let ((prgrm ast)
          (val (make-immutable-hash))
          (stream random))
      (execute-stmts prgrm val stream))))

(define execute-stmts
  (lambda (ast v rho)
    (match ast
      ['skip v]
      [(list 'return e)
       (evaluate (subst v e))]
      [else 
       (let ([step (execute-stmt ast v rho)])
         ;(displayln (config-val step))
         (execute-stmts (config-stmt step) 
                        (config-val step) 
                        (config-ss step)))])))

(define execute-stmt
  (lambda (ast v rho)
    (match ast
      [(list 'assign x e)
       (let ([v* (hash-set v x (eval (subst v e) ns))])
         (config 'skip v* rho))]
      [(list 'sample x 'bern e)
       (letrec ([v* (hash-set v x (rho))]
                [bernoulli `(if (< x ,(eval (subst v e) ns)) (assign x 1) (assign x 0))])
         (config bernoulli v* rho))]
      [(list 'sample x 'binom e)
       (let ()
         (if (pair? e) ; binom(p,n) requires two arguments
             (letrec ([v* (hash-set v 'i* 0)]   ; sample index
                      [v** (hash-set v* 'k* 0)] ; number of successes
                      [v*** (hash-set v** 'n* (cadr e))] ; number of trials
                      [val (hash-set v*** 'p* (car e))]  ; probability of a single trial
                      [binom `(seq (while (< i* n*) (seq (sample x bern p*) (seq (assign k* (+ k* x)) (assign i* (+ i* 1)))))
                                   (assign x k*))])
               (config binom val rho))
             (displayln "binomial distributions require two parameters `binom(p,n)`")))]
      [(list 'sample x 'poisson e)
       (letrec ([lbd (eval (subst v e) ns)]
                [p (exp (- lbd))]               ; initial probability value
                [v* (hash-set v x 0)]           ; running outcome of the rv sample
                [v** (hash-set v* 'p* p)]       ; running probability
                [v*** (hash-set v** 's* p)]     ; running sum probability
                [val (hash-set v*** 'u* (rho))] ; uniform sample for cdf inverse transform
                [poisson `(while (> u* s*) (seq (assign x (+ x 1)) (seq (assign p* (/ (* p* ,lbd) x)) (assign s* (+ s* p*)))))])
         (config poisson val rho))]
      [(list 'sample x 'normal params)
       (if (pair? params) ; normal(mu,sigma2) requires two arguments
           (letrec ([v* (hash-set v 'i* 0)]   ; sample index
                    [v** (hash-set v* 'k* 0)] ; number of successes
                    [v*** (hash-set v** 'n* 1000)] ; number of trials
                    [val (hash-set v*** 'p* 0.5)]  ; probability of a single trial
                    [approx-binom `(seq (while (< i* n*) (seq (sample x bern p*) (seq (assign k* (+ k* x)) (assign i* (+ i* 1)))))
                                        (assign x (+ ,(car params) (* ,(cadr params) (/ (- k* (* p* n*)) (sqrt (* n* (* p* (- 1 p*)))))))))])
             (config approx-binom val rho))
           (displayln "binomial distributions require two parameters `binom(p,n)`"))]
      [(list 'seq 'skip S)
       (config S v rho)]
      [(list 'seq S T)
       (letrec ([cnf (execute-stmt S v rho)]
                [S*   (config-stmt cnf)]
                [v*   (config-val  cnf)]
                [rho* (config-ss   cnf)])
         (config (list 'seq S* T) v* rho*))]
      [(list 'if e S)
       (if (eval (subst v e) ns)
           (config S v rho)
           (config 'skip v rho))]
      [(list 'if e S1 S2)
       (if (eval (subst v e) ns)
           (config S1 v rho)
           (config S2 v rho))]
      [(list 'while e S)
       (let ([T (list 'if e (list 'seq S (list 'while e S)) 'skip)])
         (config T v rho))]
      [(list (list 'return e))
       (evaluate (subst v e))])))

(define subst
  (lambda (val e)
    (match e
      [(list 'array n) (letrec ((zeroes (lambda (x) (if (eq? x 0) '() (cons 0 (zeroes (- x 1)))))))
                         (zeroes n))]
      [(list (? bin-op? op) left right) (list op (subst val left) (subst val right))]
      [(list (? un-op? op)  opnd) (list op (subst val opnd))]
      [(? list? x) (map (lambda (y) (subst val y)) x)]
      [(? number? x) x]
      [(? boolean? x) x]
      [(? string? x) x]
      [(? symbol? x) (hash-ref val x)])))

(define evaluate
  (lambda (e)
    (match e
      [(list (? bin-op? op) _ _) (eval e ns)]
      [(list (? un-op? op) _) (eval e ns)]
      [(? list? tuple) (map evaluate tuple)]
      [(? number? x) x]
      [(? boolean? x) x]
      [(? string? x) x])))

(provide execute-simpl config-stmt config-val config-ss)
