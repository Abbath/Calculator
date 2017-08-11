{
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Calculator.AlexLexer where
import Calculator.Types
}

%wrapper "posn"

$ascdigit = 0-9
$alpha = [a-zA-Z_]
$decdigit = $ascdigit
@decimal = $decdigit+
@exponent = [eE] [\-\+]? @decimal
@floating_point = @decimal \. @decimal @exponent? | @decimal @exponent

tokens :-

    $white  ;
    @floating_point {\s -> TNumber $ readNumber s}
    \(  {\_ -> TLPar}
    \)  {\_ -> TRPar}
    $alpha+ {\s -> TIdent s}
    [\+\-\/\\\*\%\^\$\!\~\&\|\=\>\<]+ {\s -> TOp s}
    \, {\_ -> TComma}

{

readNumber :: String -> Rational 
readNumber str = let z = (reads :: String -> [(Double, String)]) str in toRational . fst . head $ z
}
