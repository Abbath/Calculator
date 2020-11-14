{
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-imports #-}
module Calculator.AlexLexer where
import Calculator.Types
import Data.Bifunctor (bimap)
import qualified Data.Text as T
}

%wrapper "posn"

$ascdigit = 0-9
$alpha = [a-zA-Z_\.]
$alphanum = [$ascdigit $alpha]
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
    $alpha$alphanum* {\_ s -> TIdent (T.pack s)}
    $alpha$alphanum*\( {\_ s -> TFIdent (T.init . T.pack $ s)}
    \= {\_ _ -> TEqual}
    [\+\-\/\\\*\%\^\$\!\~\&\|\>\<]+ {\_ s -> TOp (T.pack s)}
    \, {\_ _ -> TComma}

{

readNumber :: String -> Rational 
readNumber str = let z = (reads :: String -> [(Double, String)]) str in toRational . fst . head $ z
}
