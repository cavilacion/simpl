#lang br/quicklang

(require simpl/utils)

(provide subst evaluate subst-eval update-sub retrieve-sub has-sym?)

(define ns (make-base-namespace))

(define subst
  (lambda (sub e)
    ;(displayln (format "val in subtst-eval: ~a\ne in subst-eval: ~a\n" val e))
    (match e
      [(list 'array n values)
       (list 'array n (subst sub values))]
      [(list 'acc-arr x e)
       (letrec ((ar (retrieve-sub sub x))
                (i (subst sub e))
                (ic (or (and (<= 0 i) (< i (list-ref ar 1))) (error "index out of bound"))))
         (subst sub (list-ref (list-ref ar 2) i)))]
      [(list (? bin-op? op) left right)
       (list op (subst sub left) (subst sub right))]
      [(list (? un-op? op) opnd)
       (eval (list op (subst sub opnd)) ns)]
      [(? list? x) (map (lambda (y) (subst sub y)) x)]
      [(? number? x) x]
      [(? boolean? x) x]
      [(? string? x) x]
      [(? symbol? x)
       (retrieve-sub sub x)])))

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

(define update-sub
  (lambda (sub x e)
    (hash-set sub x e)))
(define retrieve-sub
  (lambda (sub x)
    ;(displayln (format "val: ~a\nx: ~a" val x))
    (hash-ref sub x)))

(define has-sym?
  (lambda (e)
    (match e
      [(list 'array n values) (ormap has-sym? values)]
      [(list (? bin-op? op) a b) (or (has-sym? a) (has-sym? b))]
      [(list (? un-op? op) a) (has-sym? a)]
      [(? list? tuple) (ormap has-sym? tuple)]
      [(? number? x) #f]
      [(? boolean? x) #f]
      [(? symbol? x) #t]
      [(? string? x) #f])))


