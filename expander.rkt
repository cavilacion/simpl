#lang br/quicklang

(require simpl/utils)
(provide expand-prgrm)

(define expand-prgrm
  (lambda (prgrm)
    (expand-stmts (list-ref prgrm 1))))

(define expand-stmts
  (lambda (stmts)
    (match stmts
      [(list 'stmts stmt) (expand-stmt stmt)]
      [(list 'stmts stmt ";" stmts*)
       (list 'seq (expand-stmt stmt) (expand-stmts stmts*))])))

(define expand-stmt
  (lambda (stmt)
    ;(displayln (format "~a\n" stmt))
    (match stmt
      [(list 'stmt "skip") 'skip]
      [(list 'stmt x ":=" e)
       `(assign ,x ,(expand-expr e))]
      [(list 'stmt x "[" i "]" ":=" e)
       `(assign-array ,x ,(expand-expr i) ,(expand-expr e))]
      [(list 'stmt x "~" d e)
       `(sample ,x ,(string->symbol d) ,(expand-expr e))]
      [(list 'stmt x "[" i "]" "~" d e)
       `(sample-array ,x ,(expand-expr i)
                      ,(string->symbol d) ,(expand-expr e))]
      [(list 'stmt "if" "(" c ")" "{" body "}")
       `(if ,(expand-expr c) ,(expand-stmts body))]
      [(list 'stmt "if" "(" c ")" "{" bodyT "}" "else" "{" bodyF "}")
       `(if ,(expand-expr c) ,(expand-stmts bodyT) ,(expand-stmts bodyF))]
      [(list 'stmt "while" "(" c ")" "{" body "}")
       `(while ,(expand-expr c) ,(expand-stmts body))]
      [(list 'stmt "score" "(" e ")")
       `(score ,(expand-expr e))]
      [(list 'stmt "observe" e1 "from" d e2)
       `(score ,(score-sugar d (expand-expr e1) (expand-expr e2)))]
      [(list 'stmt "return" e)
       (list 'return (expand-expr e))])))

(define expand-expr
  (lambda (e*) 
    (let ((expand (lambda (expr)
                    (match expr
                      [(list 'be left "||" right)
                       `(or ,(expand-expr left) ,(expand-expr right))]
                      [(list 'be be1)
                       (expand-expr be1)]
                      [(list 'be1 left "&&" right)
                       `(and ,(expand-expr left) ,(expand-expr right))]
                      [(list 'be1 be2)
                       (expand-expr be2)]
                      [(list 'be2 "!" opnd)
                       `(not ,(expand-expr opnd))]
                      [(list 'be2 "false") #f]
                      [(list 'be2 "true") #t]
                      [(list 'be2 re) (expand-expr re)]
                      [(list 're left relop right)
                       (let ((left*  (expand-expr left))
                             (right* (expand-expr right))
                             (op (string->symbol relop)))
                         (match op
                           ['==  `(equal? ,left* ,right*)]
                           ['!=  `(not (equal? ,left* ,right*))]
                           [else `(,op ,left* ,right*)]))]
                      [(list 're e)
                       (expand-expr e)]
                      [(list 'e "array" "(" n ")")
                       (letrec ((r (lambda (i) (if (eq? i 0)
                                                   '()
                                                   (cons 0 (r (- i 1)))))))
                         `(array ,n ,(r n)))]
                      [(list 'e "{" ar)
                       (append '(array) (expand-array ar))]
                      [(list 'e left "+" right)
                       `(+ ,(expand-expr left) ,(expand-expr right))]
                      [(list 'e left "-" right)
                       `(- ,(expand-expr left) ,(expand-expr right))]
                      [(list 'e e2) (expand-expr e2)]
                      [(list 'e2 left "*" right)
                       `(* ,(expand-expr left) ,(expand-expr right))]
                      [(list 'e2 left "/" right)
                       `(/ ,(expand-expr left) ,(expand-expr right))]
                      [(list 'e2 left "%" right)
                       `(remainder ,(expand-expr left) ,(expand-expr right))]
                      [(list 'e2 e3) (expand-expr e3)]
                      [(list 'e3 "-" opnd) `(- ,(expand-expr opnd))]
                      [(list 'e3 e4) (expand-expr e4)]
                      [(list 'e4 left "^" right)
                       `(expt ,(expand-expr left) ,(expand-expr right))]
                      [(list 'e4 e5) (expand-expr e5)]
                      [(list 'e5 "true") #t]
                      [(list 'e5 "false") #f]
                      [(list 'e5 lit) lit]
                      [(list 'e5 lit "[" i "]") `(acc-arr ,lit ,(expand-expr i))]
                      [(list 'e5 "(" e (list 'e6 ")")) `(() ,(expand-expr e))]
                      [(list 'e5 "(" e e6)
                       (cons (expand-expr e) (expand-expr e6))]
                      [(list 'e6 ")") '()]
                      [(list 'e6 "," e e6)
                       (cons (expand-expr e) (expand-expr e6))]
                      [else (error (format "this syntax tree is not an expression: ~a" expr))]))))
      (e-fix-assoc (expand e*)))))

(define expand-array
  (lambda (ast)
    (match ast
      [(list 'ar "}") '(0 ())]
      [(list 'ar e ar2)
       (expand-array2 ar2 (list (expand-expr e)) 1)])))
(define expand-array2
  (lambda (ast es n)
    (match ast
      [(list 'ar2 "," e ar2)
       (expand-array2 ar2 (append es (list (expand-expr e))) (+ n 1))]
      [(list 'ar2 "}") `(,n ,es)]
      [else (error (format "not an array expression: ~a" ast))])))

(define e-fix-assoc
  (lambda (ast)
    (match ast
      [(list '() rest)
       (e-fix-assoc rest)]
      [(list (? mul-op? op1) a right)
       (let ([A (e-fix-assoc a)])
         (match right
           [(list (? mul-op? op2) b c)
            (e-fix-assoc (list op2 (list op1 A (e-fix-assoc b))
                               (e-fix-assoc c)))]
           [else (list op1 A (e-fix-assoc right))]))]
      [(list (? add-op? op1) a right)
       (let ([A (e-fix-assoc a)])
         (match right
           [(list (? add-op? op2) b c)
            (e-fix-assoc (list op2 (list op1 A (e-fix-assoc b))
                               (e-fix-assoc c)))]
           [else (list op1 A (e-fix-assoc right))]))]
      [(list (? bin-op? op) a b) (list op (e-fix-assoc a) (e-fix-assoc b))]
      [(list (? un-op?  op) a  ) (list op (e-fix-assoc a))]
      [(list '() rest) (e-fix-assoc rest)]
      [(? list? x) (map e-fix-assoc x)]
      [(? number? x) x]
      [(? symbol? x) x]
      [(? boolean? x) x]
      [(? string? x) x])))

(define score-sugar
  (lambda (d e1 e2)
    (match d
      ("bern"
       (if (list? e2)
           (error (format "observe statement with bernoulli should have one parameter: `~a`" e2))
           `(* (expt ,e2 ,e1) (expt (- 1 ,e2) (- 1 ,e1)))))
      ("poisson"
       (if (list? e2)
           (error (format "observe statement with exponential distribution requires one parameter: `~a`" e2))
           (let ((e 2.71828182845904523536))
             `(* (/ (expt ,e2 ,e1) (fac ,e1)) (expt ,e (- ,e2))))))
      ("normal"
       (if (not (and (list? e2) (eq? (length e2) 2)))
           (error (format "observe statement with normal should have two parameters: `~a`" e2))
           (let ((mu (car e2))
                 (var (cadr e2))
                 (sqr (lambda (x) (* x x)))
                 (e 2.71828182845904523536)
                 (pi 3.14159265358979323846))
             `(/ (expt ,e (- (/ (* (- ,e1 ,mu) (- ,e1 ,mu)) (* 2 ,var)))) (sqrt (* (* 2 ,pi) ,var))))))
      (else
       (error (format "observe from ~a not implemented" d))))))
