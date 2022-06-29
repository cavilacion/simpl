#lang racket

(require simpl/utils
         simpl/parser
         simpl/tokenizer 
         simpl/expander
         simpl/substeval
         simpl/sampling
         brag/support 
         rackunit)

(provide symbex config-r config-l config-S config-sub config-k config-B)

(struct config (r l S sub k B) #:transparent)

(define symbex
  (lambda (ast)
    (let ((r '())
          (l 1)
          (S ast)
          (sub (make-immutable-hash))
          (k random)
          (B (list #t)))
      (symbex-stmts r l S sub k B))))

(define symbex-stmts
  (lambda (r l S sub k B)
    ;(displayln (format "ast: ~a\nval: ~a\n" ast v))
    (match S
      ['skip sub]
      [(list 'return e)
       (list l (subst-eval sub e))]
      [else 
       (let ([step (symbex-stmt r l S sub k B)])
         (symbex-stmts (config-r step) 
                       (config-l step)
                       (config-S step)
                       (config-sub step)
                       (config-k step)
                       (config-B step)))])))

(define symbex-stmt
  (lambda (r l S sub k B)
    (match S
      [(list 'assign x e)
       (letrec ([e* (subst sub e)]
                [rhs (match e*
                       [(list 'array n a) (list 'array n a)]
                       [else (evaluate e*)])]
                [s* (update-val sub x rhs)])
         (config r l 'skip s* k B))]
      [(list 'assign-array x i e)
       (letrec ([x* (retrieve-val sub x)]
                [n* (list-ref x* 1)]
                [a* (list-ref x* 2)]
                [i* (subst-eval sub i)]
                [ic (or (and (<= 0 i*) (< i* n*)) (error "index out of bound"))]
                [e* (subst-eval sub e)]
                [a (append (take a* i*) (list e*) (drop a* (+ i* 1)))]
                [s* (update-val sub x (list 'array n* a))])
         (config r l 'skip s* k B))]
      [(list 'sample-array x i d e)
       (let ([S `(seq (sample s* ,d ,e) (assign-array ,x ,i s*))])
         (config r l S sub k B))]
      [(list 'sample x dist param)
       (config r l 'skip (sample (list 'sample x dist param) sub k) k B)]
      [(list 'seq 'skip S)
       (config r l S sub k B)]
      [(list 'seq S T)
       (letrec ([cnf (symbex-stmt r l S sub k B)]
                [r* (config-r cnf)]
                [l* (config-l cnf)]
                [S* (config-S cnf)]
                [s* (config-sub cnf)]
                [k* (config-k cnf)]
                [B* (config-B cnf)])
         (config r* l* (list 'seq S* T) s* k* B*))]
      [(list 'if e S)
       (if (subst-eval sub e)
           (config r l S sub k B)
           (config r l 'skip sub k B))]
      [(list 'if e S1 S2)
       (if (subst-eval sub e)
           (config r l S1 sub k B)
           (config r l S2 sub k B))]
      [(list 'while e S)
       (let ([T (list 'if e (list 'seq S (list 'while e S)) 'skip)])
         (config r l T sub k B))]
      [(list 'score e)
       (config r (* l (subst-eval sub e)) 'skip sub k B)]
      [(list (list 'return e))
       (subst-eval sub e)])))

(define exec
  (lambda (x)
    (symbex (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))
