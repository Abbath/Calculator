{
module Calculator.HappyParser where
import Calculator.AlexLexer
import Calculator.Types
}

%name parse 
%tokentype { Token }
%error { parseError }

%token 
    num { TNumber $$}
    var { TIdent $$ }
    op { TOp $$}
    '(' { TLPar }
    ')' { TRPar }
    ',' { TComma }
    '=' { TOp "=" }
    '-' { TOp "-" }

%%

Number: 
    num {Number $1}

Asgn:
    var '=' Expr { Asgn $1 $3} 

UDF: 
    var '(' vars ')' '=' Expr { UDF $1 (reverse $3) $6}

vars : var { [$1]}
    | vars ',' var {$3 : $1}

UDO :
    var '(' num ',' num ')' '=' Expr {UDO $1 (truncate $3) (if $5 == 0 then L else R) $8 } 

OpCall:
    Expr op Expr { OpCall $2 $1 $3}

UMinus:
    '-' Expr { UMinus $2}

Par:
    '(' Expr ')' {Par $2}

FunCall:
    var '(' exprs ')' {FunCall $1 (reverse $3)}

exprs : Expr {[$1]}
    | exprs ',' Expr {$3 : $1}

Id:
    var {Id $1}

Expr:
    Number {Number $1}
    | Asgn {Asgn $1}
    | UDF {UDF $1}
    | UDO {UDO $1}
    | OpCall {OpCall $1}
    | UMinus {UMinus $1}
    | Par {Par $1}
    | FunCall {FunCall $1}
    | Id {Id $1}

{
parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
}