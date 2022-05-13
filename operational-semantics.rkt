#lang racket

(require simpl/utils
         simpl/parser
         simpl/tokenizer 
         simpl/expander
         simpl/substeval
         simpl/sampling
         brag/support 
         rackunit)

(provide execute-simpl config-stmt config-val config-ss config-w)

(struct config (stmt val ss w) #:transparent)

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
      [(list 'sample x dist param)
       (config 'skip (sample (list 'sample x dist param) v rho) rho w)]
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

(define exec
  (lambda (x)
    (execute-simpl (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))

(define p "x:={3,3,0,4};x[3] ~ normal(10,2);return x")