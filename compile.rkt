#lang racket

(require simpl/utils
         simpl/parser
         simpl/tokenizer 
         simpl/expander
         simpl/substeval
         simpl/sampling
         brag/support 
         rackunit)

(provide compile-simpl)

(define global-rv-index 0)
(define stream
  (lambda ()
    (set! global-rv-index (add1 global-rv-index))
    global-rv-index))

(struct config (stmt val ss w) #:transparent)

(define compile-simpl
  (lambda (ast)
    (let ((prgrm ast)
          (val (make-immutable-hash))
          (rho stream))
      (compile-stmts prgrm val rho 1))))

(define compile-stmts
  (lambda (ast v rho w)
    ;(displayln (format "ast: ~a\nval: ~a\n" ast v))
    (match ast
      ['skip v]
      [(list 'return e)
       (displayln (format "return ~a;" (subst v e)))]
      [else 
       (let ([step (compile-stmt ast v rho w)])
         (compile-stmts (config-stmt step) 
                        (config-val step)
                        (config-ss step)
                        (config-w step)))])))

(define compile-stmt
  (lambda (ast v rho w)
    (match ast
      [(list 'assign x e)
       (letrec ([e* (subst-eval v e)]
                [rhs (match e*
                       [(list 'array n a) (list 'array n a)]
                       [else (evaluate e*)])]
                [v* (update-val v x rhs)])
         (if (contains-val? v x)
             (displayln (format "~a = ~a;" x rhs))
             (displayln (format "double ~a = ~a;" x rhs)))
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
         (displayln "Warning: arrays not yet supported")
         (config 'skip v* rho w))]
      [(list 'sample-array x i d e)
       (let ([S `(seq (sample s* ,d ,e) (assign-array ,x ,i s*))])
         (displayln "Warning: arrays not yet supported")
         (config S v rho w))]
      [(list 'sample x dist param)
       (let ((v* (update-symbolic-val v x (rho))))
         (compile-sample v x dist param)
         (config 'skip v* rho w))]
      [(list 'seq 'skip S)
       (config S v rho w)]
      [(list 'seq S T)
       (letrec ([cnf (compile-stmt S v rho w)]
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
       (let ()
         (displayln (format "if (~a) {" (subst v e)))
         (compile-stmt S1 v rho w)
         (displayln "} else {")
         (compile-stmt S2 v rho w)
         (displayln "}"))]
      [(list 'while e S)
       (let ([T (list 'if e (list 'seq S (list 'while e S)) 'skip)])
         (config T v rho w))]
      [(list 'score e)
       (config 'skip v rho (* w (subst-eval v e)))]
      [(list (list 'return e))
       (subst v e)])))

(define compile-sample
  (lambda (v x dist params)
    (when (contains-val? v x)
      (display "double "))
    (display (format "~a = ~aSample (" x dist))
    (compile-params params)
    (displayln ");")))

(define compile-params
  (lambda (params)
    (or (and (number? params) (display (format "~a" params)))
        (and (null? params) #t)
        (and (null? (cdr params)) (display (format "~a" (car params))) #t)
        (and (display (format "~a, " (car params)))
             (compile-params (cdr params))))))

(define compile
  (lambda (x)
    (compile-simpl (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))

(define p "x ~ bern(0.0001); if (x==1) { a := 0.95 } else { a := 0.001 }; y ~ bern(a); return x")