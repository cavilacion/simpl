#lang racket

(require simpl/utils
         simpl/parser
         simpl/tokenizer 
         simpl/expander
         simpl/sym-substeval
         brag/support 
         rackunit)

(provide symbex config-r config-l config-S config-sub config-k config-B)

(struct config (r l S sub k B) #:transparent)
(struct end-config (r l B) #:transparent)

(define symbex
  (lambda (ast)
    (let ((r '())
          (l 1)
          (S ast)
          (sub (make-immutable-hash))
          (k 0)
          (B (list #t)))
      (filter (lambda (cnf) (list? (end-config-B cnf)))
              (flatten (symbex-stmts r l S sub k B))))))

(define flatten
  (lambda (a)
    (cond
      [(null? a)
       '()]
      [(struct? a)
       (list a)]
      [(pair? a)
       (append (flatten (car a)) (flatten (cdr a)))]
      [else
       (error (format "bad argument for flatten: ~a" a))])))

(define check-pc
  (lambda (B)
    (letrec ((check-pc-rec
              (lambda (B B*)
                (if (null? B)
                    B*
                    (let ((c (car B))
                          (cs (cdr B)))
                      (if (has-sym? c)
                          (check-pc-rec cs (cons c B*))
                          (if (evaluate c)
                              (check-pc-rec cs B*)
                              #f)))))))
      (check-pc-rec B '()))))

(define symbex-stmts
  (lambda (r l S sub k B)
    ;(displayln (format "ast: ~a\nval: ~a\n" ast v))
    (match S
      ;['skip (config r l S sub k B)]
      [(list 'return e)
       (letrec ((r (subst sub e))
                (r* (if (has-sym? r) r (evaluate r)))
                (l* (if (has-sym? l) l (evaluate l))))
         (end-config r* l* (check-pc B)))]
      [else 
       (letrec ([all-steps (symbex-stmt r l S sub k B)]
                [set-of-steps (flatten all-steps)])
         (for/list ([step all-steps])
           (symbex-stmts (config-r step) 
                         (config-l step)
                         (config-S step)
                         (config-sub step)
                         (config-k step)
                         (config-B step))))])))

(define symbex-stmt
  (lambda (r l S sub k B)
    (match S
      [(list 'assign x e)
       (letrec ([e* (subst sub e)]
                [rhs (match e*
                       [(list 'array n a) (list 'array n a)]
                       [else (evaluate e*)])]
                [s* (update-sub sub x rhs)])
         (list (config r l 'skip s* k B)))]
      [(list 'assign-array x i e)
       (letrec ([x* (retrieve-sub sub x)]
                [n* (list-ref x* 1)]
                [a* (list-ref x* 2)]
                [i* (subst sub i)]
                [ic (or (and (<= 0 i*) (< i* n*)) (error "index out of bound"))]
                [e* (subst sub e)]
                [a (append (take a* i*) (list e*) (drop a* (+ i* 1)))]
                [s* (update-sub sub x (list 'array n* a))])
         (list (config r l 'skip s* k B)))]
      [(list 'sample-array x i d e)
       (let ([S `(seq (sample s* ,d ,e) (assign-array ,x ,i s*))])
         (list (config r l S sub k B)))]
      [(list 'sample x 'rnd)
       (list (config r l 'skip (update-sub sub x (make-symbolic-var k)) (+ k 1) B))]
      [(list 'sample x 'bern e)
       (let ((S* `(seq (sample ,x rnd) (if (< ,x ,e) (assign ,x 1) (assign ,x 0)))))
         (list (config r l S* sub k B)))]
      [(list 'seq 'skip S)
       (list (config r l S sub k B))]
      [(list 'seq S T)
       (letrec ([all-steps (symbex-stmt r l S sub k B)]
                [set-of-steps (flatten all-steps)])
         (for/list ([cnf set-of-steps])
           (letrec ([r* (config-r cnf)]
                    [l* (config-l cnf)]
                    [S* (config-S cnf)]
                    [s* (config-sub cnf)]
                    [k* (config-k cnf)]
                    [B* (config-B cnf)])
             (config r* l* (list 'seq S* T) s* k* B*))))]
      [(list 'if e S)
       (let ((new-conjunct (subst sub e)))
         (list (config r l S sub k (cons new-conjunct B))
               (config r l 'skip sub k (cons (list 'not new-conjunct) B))))]
      [(list 'if e S1 S2)
       (let ((new-conjunct (subst sub e)))
         (list (config r l S1 sub k (cons new-conjunct B))
               (config r l S2 sub k (cons (list 'not new-conjunct) B))))]
      [(list 'while e S)
       (let ([T (list 'if e (list 'seq S (list 'while e S)) 'skip)])
         (list (config r l T sub k B)))]
      [(list 'score e)
       (list (config r `(* ,l ,(subst sub e)) 'skip sub k B))])))

(define make-symbolic-var
  (lambda (k)
    (string->symbol (string-append "X_" (number->string k)))))

(define exec
  (lambda (x)
    (symbex (expand-prgrm (parse-to-datum (apply-tokenizer make-tokenizer x))))))
