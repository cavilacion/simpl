#lang racket

(require simpl/utils simpl/parser 
         simpl/tokenizer 
         simpl/expander 
         brag/support 
         rackunit)

(define exec
  (lambda (x)
    (execute-simpl (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))

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
    (match ast
      ['skip v]
      [(list 'return e)
       (list w (evaluate (subst v e)))]
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
       (letrec ([ev (subst v e)]
                [rhs (match ev
                       [(list 'array n values)
                        (list 'array (eval n ns) (evaluate values))]
                       [else (eval ev ns)])]
                [v* (hash-set v x rhs)])
         (config 'skip v* rho w))]
      [(list 'assign-array x i e)
       (letrec ([x* (hash-ref v x)]
                [n* (list-ref x* 1)]
                [a* (list-ref x* 2)]
                [i* (eval (subst v i) ns)]
                [ic (or (and (<= 0 i*) (< i* n*)) (error "index out of bound"))]
                [e* (eval (subst v e) ns)]
                [a (append (take a* i*) (list e*) (drop a* (+ i* 1)))]
                [val (hash-set v x (list 'array n* a))])
         (config 'skip val rho w))]
      [(list 'sample-array x i d e)
       (letrec ([S `(seq (sample s* ,d ,e) (assign-array ,x ,i s*))])
         (config S v rho w))]
      [(list 'sample x 'bern e)
       (letrec ([v* (hash-set v x (rho))]
                [bernoulli `(if (< ,x ,(eval (subst v e) ns)) (assign ,x 1) (assign ,x 0))])
         (config bernoulli v* rho w))]
      [(list 'sample x 'binom e)
       (if (pair? e) ; binom(p,n) requires two argumentsexec
           (letrec ([v* (hash-set v 'i* 0)]   ; sample index
                    [v** (hash-set v* 'k* 0)] ; number of successes
                    [v*** (hash-set v** 'n* (cadr e))] ; number of trials
                    [val (hash-set v*** 'p* (car e))]  ; probability of a single trial
                    [binom `(seq (while (< i* n*) (seq (sample ,x bern p*) (seq (assign k* (+ k* ,x)) (assign i* (+ i* 1)))))
                                 (assign ,x k*))])
             (config binom val rho w))
           (error "binomial distributions require two parameters `binom(p,n)`"))]
      [(list 'sample x 'poisson e)
       (letrec ([lbd (eval (subst v e) ns)]
                [p (exp (- lbd))]               ; initial probability value
                [v* (hash-set v x 0)]           ; running outcome of the rv sample
                [v** (hash-set v* 'p* p)]       ; running probability
                [v*** (hash-set v** 's* p)]     ; running sum probability
                [val (hash-set v*** 'u* (rho))] ; uniform sample for cdf inverse transform
                [poisson `(while (> u* s*) (seq (assign ,x (+ ,x 1)) (seq (assign p* (/ (* p* ,lbd) ,x)) (assign s* (+ s* p*)))))])
         (config poisson val rho w))]
      [(list 'sample x 'normal params)
       (and (or (pair? params) (error "normal distributions require two parameters `normal(mean,var)`"))
            (let ([mu (car params)]    ; population mean (to be sampled)
                  [var (cadr params)]) ; population variance (to be sampled)
              (if (void? N) 
                  (letrec ([U (rho)] ; draw two uniform samples to perform Box-Muller
                           [V (rho)] 
                           [X (* (sqrt (* -2 (log U))) (cos (* 2 (* pi V))))]
                           [Y (* (sqrt (* -2 (log U))) (sin (* 2 (* pi V))))]
                           [normal `(assign ,x (+ ,mu (* ,X (sqrt ,var))))])
                    (set! N Y)
                    (config normal v rho w))
                  (let ([normal `(assign ,x (+ ,mu (* ,N (sqrt ,var))))])
                    ; we use the old sample and remove it
                    (set! N (void))
                    (config normal v rho w)))))]
      [(list 'sample x 'exp e)
       (letrec ([lbd (eval (subst v e) ns)]
                [u (rho)]
                [X (- (* (/ 1 lbd) (log (- 1 u))))]
                [exponential `(assign ,x ,X)])
         (config exponential v rho w))]
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
       (if (eval (subst v e) ns)
           (config S v rho w)
           (config 'skip v rho w))]
      [(list 'if e S1 S2)
       (if (eval (subst v e) ns)
           (config S1 v rho w)
           (config S2 v rho w))]
      [(list 'while e S)
       (let ([T (list 'if e (list 'seq S (list 'while e S)) 'skip)])
         (config T v rho w))]
      [(list 'score e)
       (config 'skip v rho (* w (eval (subst v e) ns)))]
      [(list (list 'return e))
       (evaluate (subst v e))])))

(define subst
  (lambda (val e)
    ;(displayln (format "~a" val))
    (match e
      [(list 'array n values)
       (list 'array (evaluate (subst val n)) (evaluate (subst val values)))]
      [(list 'acc-arr x e)
       (letrec ((ar (hash-ref val x))
                (i (eval (subst val e) ns))
                (ic (or (and (<= 0 i) (< i (list-ref ar 1))) (error "index out of bound"))))
         (list-ref (list-ref ar 2) i))]
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
      [(list 'array n values) (evaluate values)]
      [(list (? bin-op? op) _ _) (eval e ns)]
      [(list (? un-op? op) _) (eval e ns)]
      [(? list? tuple) (map evaluate tuple)]
      [(? number? x) x]
      [(? boolean? x) x]
      [(? string? x) x])))

(define p "x:={3,3,42,4}; y:=x[2]; return y")

(provide execute-simpl config-stmt config-val config-ss config-w)
