#lang brag

prgrm : stmts 
stmts : stmt [";" stmts]

symbs  : ["@" VARIABLE symbs2]
symbs2 : ";"
       | "," VARIABLE symbs2

stmt : "skip"
     | VARIABLE ":=" e 
     | VARIABLE "~" DIST e
     | "if" "(" be ")" "{" stmts "}" [ "else" "{" stmts "}" ]
     | "while" "(" be ")" "{" stmts "}"
     | "return" e

be  : be1 [ "||" be ]
be1 : be2 [ "&&" be1 ]
be2 : "!" be2
    | re | boolean 

re  : e RELOP e

e   : "array" "(" INTEGER ")"
    | e2 [ e-op e]  
e2  : e3 [ mul-op e2 ]
e3  : e4 | "-" e3
e4  : e5 [ "^" e4 ]
e5  : INTEGER | DECIMAL | VARIABLE | STRING | boolean
    | "(" e e6
e6  : ")" 
    | "," e e6

@e-op : "+" | "-"
@mul-op : "*" | "/" | "%"
@boolean : "true" | "false"
