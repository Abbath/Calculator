{
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-imports #-}
module Calculator.AlexLexer where
import Calculator.Types
}

%wrapper "posn"

$ascdigit = 0-9
$alpha = [a-zA-Z_]
$decdigit = $ascdigit
@decimal = $decdigit+
@exponent = [eE] [\-\+]? @decimal
@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent | @decimal

tokens :-

    $white  ;
    let {\_ _ -> TLet }
    fun {\_ _ -> TFun }
    op {\_ _ -> TEnd }
    @floating_point {\_ s -> TNumber $ readNumber s}
    \(  {\_ _ -> TLPar}
    \)  {\_ _ -> TRPar}
    $alpha+ {\_ s -> TIdent s}
    $alpha+\( {\_ s -> TFIdent (init s)}
    \= {\_ _ -> TEqual}
    ^\- {\_ _ -> TMinus}
    [\+\-\/\\\*\%\^\$\!\~\&\|\>\<]+ {\_ s -> TOp s}
    \, {\_ _ -> TComma}

{

readNumber :: String -> Rational 
readNumber str = let z = (reads :: String -> [(Double, String)]) str in toRational . fst . head $ z
}
