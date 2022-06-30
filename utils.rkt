#lang racket

(define add-op? (lambda (x) (memq x '(+ -))))
(define mul-op? (lambda (x) (memq x '(* /))))
(define bin-op? (lambda (x) (memq x '(+ - * / || && < <= > >= equal? expt))) )
(define un-op?  (lambda (x) (memq x '(not - sqrt fac))))

(define fac
  (lambda (x)
    (apply * (build-list x add1))))

(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define make-symbolic
  (lambda (var)
    (string->symbol (string-append "@" (symbol->string var)))))
(define symbolic-variable? 
  (lambda (var)
    (and (symbol? var) (equal? #\@ (string-ref (symbol->string var) 0)))))

(define symbolic-expr?
  (lambda (e)
    (match e
      [(list (? bin-op? op) left right)
       (or (symbolic-expr? left) (symbolic-expr? right))]
      [(list (? un-op? op) opnd) 
       (or (symbolic-expr? opnd))]
      [(? list? x) #t]
      [(? number? x) #f]
      [(? boolean? x) #f]
      [(? string? x) #t]
      [(? symbol? x) #t])))

(provide add-op? mul-op? bin-op? un-op? 
         round-off fac
         make-symbolic symbolic-variable? symbolic-expr?)
