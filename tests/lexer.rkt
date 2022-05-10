#lang br
(require SIMPL/lexer brag/support rackunit)

(define (lex str)
  (apply-port-proc simpl-lexer str))

(module+ test
  (test-case
   ""
   (check-equal? (lex "") empty))
  (test-case
   ""
   (check-equal?
    (lex " ")
    (list (srcloc-token (token " " #:skip? #t)
                        (srcloc 'string 1 0 1 1)))))
  (test-case
   ""
   (check-equal?
    (first (lex "//ignored\n"))
    (srcloc-token (token '//ignored #:skip? #t)
                  (srcloc 'string 1 0 1 9))))
  (test-case
   ""
   (check-equal?
    (lex "/*//ignored\n*/")
    (list (srcloc-token (token '|/*//ignored
*/| #:skip? #t)
                        (srcloc 'string 1 0 1 14)))))
  (test-case
   ""
   (check-equal?
    (lex "/*while\nabort*/skip")
    (list (srcloc-token (token '|/*while
abort*/| #:skip? #t)
                        (srcloc 'string 1 0 1 15))
          (srcloc-token (token "skip" "skip")
                        (srcloc 'string 2 7 16 4)))))
  (test-case
   ""
   (check-equal?
    (lex "while")
    (list (srcloc-token (token "while" "while")
                        (srcloc 'string 1 0 1 5)))))
  (test-case
   ""
   (check-equal?
    (lex "abort")
    (list (srcloc-token (token "abort" "abort")
                        (srcloc 'string 1 0 1 5)))))
  (test-case
   ""
   (check-equal?
    (lex "return")
    (list (srcloc-token (token "return" "return")
                        (srcloc 'string 1 0 1 6)))))
  (test-case
   ""
   (check-equal?
    (lex "else")
    (list (srcloc-token (token "else" "else")
                        (srcloc 'string 1 0 1 4)))))
  (test-case
   ""
   (check-equal?
    (lex "if")
    (list (srcloc-token (token "if" "if")
                        (srcloc 'string 1 0 1 2)))))
  (test-case
   ""
   (check-equal?
    (lex "{()}")
    (list (srcloc-token (token "{" "{")
                        (srcloc 'string 1 0 1 1))
          (srcloc-token (token "(" "(")
                        (srcloc 'string 1 1 2 1))
          (srcloc-token (token ")" ")")
                        (srcloc 'string 1 2 3 1))
          (srcloc-token (token "}" "}")
                        (srcloc 'string 1 3 4 1)))))
  (test-case
   ""
   (check-equal?
    (lex ":=;~,")
    (list (srcloc-token (token ":=" ":=")
                        (srcloc 'string 1 0 1 2))
          (srcloc-token (token ";" ";")
                        (srcloc 'string 1 2 3 1))
      	  (srcloc-token (token "~" "~")
			                  (srcloc 'string 1 3 4 1))
	        (srcloc-token (token "," ",")
			                  (srcloc 'string 1 4 5 1)))))
  (test-case
   ""
   (check-equal?
    (lex "bern(poisson")
    (list (srcloc-token (token 'DIST "bern")
                        (srcloc 'string 1 0 1 4))
          (srcloc-token (token "(" "(")
                        (srcloc 'string 1 4 5 1))
          (srcloc-token (token 'DIST "poisson")
                        (srcloc 'string 1 5 6 7)))))
  (test-case
   ""
   (check-equal?
    (lex "<<===>>=!=")
    (list (srcloc-token (token 'RELOP "<")
                        (srcloc 'string 1 0 1 1))
          (srcloc-token (token 'RELOP "<=")
                        (srcloc 'string 1 1 2 2))
          (srcloc-token (token 'RELOP "==")
                        (srcloc 'string 1 3 4 2))
          (srcloc-token (token 'RELOP ">")
                        (srcloc 'string 1 5 6 1))
          (srcloc-token (token 'RELOP ">=")
                        (srcloc 'string 1 6 7 2))
          (srcloc-token (token 'RELOP "!=")
                        (srcloc 'string 1 8 9 2)))))
  (test-case
   ""
   (check-equal?
    (lex "12+.12-*/^")
    (list (srcloc-token (token 'INTEGER 12)
                        (srcloc 'string 1 0 1 2))
          (srcloc-token (token "+" "+")
                        (srcloc 'string 1 2 3 1))
          (srcloc-token (token 'DECIMAL 0.12)
                        (srcloc 'string 1 3 4 3))
          (srcloc-token (token "-" "-")
                        (srcloc 'string 1 6 7 1))
          (srcloc-token (token "*" "*")
                        (srcloc 'string 1 7 8 1))
          (srcloc-token (token "/" "/")
                        (srcloc 'string 1 8 9 1))
          (srcloc-token (token "^" "^")
                        (srcloc 'string 1 9 10 1)))))
  (test-case
   ""
   (check-exn exn:fail:read? (lambda () (lex "<=="))))
  (test-case
   ""
   (check-exn exn:fail:read? (lambda () (lex "12."))))
  (test-case
   ""
   (check-equal?
    (lex "&&false||true!!=")
    (list (srcloc-token (token "&&" "&&")
                        (srcloc 'string 1 0 1 2))
          (srcloc-token (token "false" "false")
                        (srcloc 'string 1 2 3 5))
          (srcloc-token (token "||" "||")
                        (srcloc 'string 1 7 8 2))
          (srcloc-token (token "true" "true")
                        (srcloc 'string 1 9 10 4))
          (srcloc-token (token "!" "!")
                        (srcloc 'string 1 13 14 1))
          (srcloc-token (token 'RELOP "!=")
                        (srcloc 'string 1 14 15 2)))))
  (test-case
   ""
   (check-exn exn:fail:read? (lambda () (lex "!=="))))
  (test-case
   ""
   (check-equal?
    (lex "1.2")
    (list (srcloc-token (token 'DECIMAL 1.2)
                        (srcloc 'string 1 0 1 3)))))
  (test-case
   ""
   (check-equal?
    (lex "\"foo\"")
    (list (srcloc-token (token 'STRING "foo")
                        (srcloc 'string 1 0 1 5)))))
  (test-case
   ""
   (check-equal?
    (lex "'foo'")
    (list (srcloc-token (token 'STRING "foo")
                        (srcloc 'string 1 0 1 5)))))
  (test-case
   ""
   (check-exn exn:fail:read? (lambda () (lex "±"))))
  (test-case
   ""
   (check-exn exn:fail:read? (lambda () (lex "?"))))
  (test-case
   ""
   (check-exn exn:fail:read? (lambda () (lex "\\"))))
  (test-case
   ""
   (check-exn exn:fail:read? (lambda () (lex "&")))))
