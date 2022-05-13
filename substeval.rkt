#lang br/quicklang

(require simpl/utils)

(provide subst evaluate subst-eval update-val retrieve-val)

(define ns (make-base-namespace))

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
    ;(displayln (format "val: ~a\nx: ~a" val x))
    (hash-ref val x)))