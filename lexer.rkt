#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev 
  alpha (:+ (char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
  
(define simpl-lexer
  (lexer-srcloc
    ["\n" (token lexeme #:skip? #t)]
    [whitespace (token lexeme #:skip? #t)]
    [(:or (from/stop-before "//" "\n")
          (from/to "/*" "*/"))
     (token lexeme #:skip? #t)]
    [(:or "skip" "abort" "if" "else" "while" "true" "false" "return" "array" "score")
     (token lexeme lexeme)]
    [(:or "uniform" "bern" "binom" "poisson" "normal" "exp")
     (token 'DIST lexeme)]
    [(:or "@" ":=" "~" ";" "(" ")" "{" "}" "[" "]" ",")
     (token lexeme lexeme)]
    [(:or "&&" "||" "!")
     (token lexeme lexeme)]
    [(:or "<=" ">=" "==" "!=" "<" ">")
     (token 'RELOP lexeme)]
    [(:or "+" "-" "*" "/" "**" "^")
     (token lexeme lexeme)]
    [alpha (token 'VARIABLE (string->symbol lexeme))]
    [digits (token 'INTEGER (string->number lexeme))]
    [(:seq (:? digits) "." digits)
     (token 'DECIMAL (string->number lexeme))]
    [(:or (from/to "\"" "\"") (from/to "'" "'"))
     (token 'STRING
            (substring lexeme
                       1 (sub1 (string-length lexeme))))]))

(provide simpl-lexer)
