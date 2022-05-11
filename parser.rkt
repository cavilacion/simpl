#lang brag

prgrm : stmts 
stmts : stmt [";" stmts]

stmt : "skip"
     | VARIABLE [ "[" e "]" ] ":=" e 
     | VARIABLE [ "[" e "]" ] "~" DIST e
     | "if" "(" be ")" "{" stmts "}" [ "else" "{" stmts "}" ]
     | "while" "(" be ")" "{" stmts "}"
     | "return" e

be  : be1 [ "||" be ]
be1 : be2 [ "&&" be1 ]
be2 : "!" be2
    | re | boolean 

re  : e RELOP e

e   : "array" "(" INTEGER ")"
    | "{" ar
    | e2 [ e-op e]
    | ")"
e2  : e3 [ mul-op e2 ]
e3  : e4 | un-op e3
e4  : e5 [ "^" e4 ]
e5  : INTEGER | DECIMAL | STRING | boolean
    | VARIABLE [ "[" e "]" ]
    | "(" e e6
e6  : ")" 
    | "," e e6

ar  : "}"
    | e ar2
ar2 : "," e ar2
    | "}"

@e-op : "+" | "-"
@mul-op : "*" | "/" | "%"
@boolean : "true" | "false"
@un-op : "sqrt" | "-"
