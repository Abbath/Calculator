{
module Calculator.HappyParser where
import Calculator.AlexLexer
import Calculator.Types (Token(..), Expr(..), Assoc(..))
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
    '=' { TEqual }
    '-' { TMinus }
    let { TLet }
    fun { TFun }
    fop { TEnd }
    fn  { TFIdent $$}

%%

All:
    let var '=' Expr { Asgn $2 $4} 
    | fun fn vars ')' '=' Expr { UDF $2 (reverse $3) $6}
    | fop var '(' num ',' num ')' '=' Expr {UDO $2 (truncate $4) (if $6 == 0 then L else R) $9 }
    | Exprs {$1}

Exprs: Expr {$1}
    | Exprs op Expr {OpCall $2 $1 $3}

Expr:
    '-' Expr { UMinus $2}
    | '(' Exprs ')' {Par $2}
    | fn exprs ')' {FunCall $1 (reverse $2)}
    | num {Number $1}
    | var {Id $1}

vars : var {[$1]}
    | vars ',' var {$3 : $1}

exprs : Exprs {[$1]}
    | exprs ',' Exprs {$3 : $1}

{
parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts
}