module Calculator.Lexer (tokenize) where

import Data.Maybe (fromJust)
import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, find)
import Control.Applicative ((<|>))
import Calculator.Types (Token(..), Operator(..), Function(..))

operator :: Char -> Operator
operator c = fromJust $ lookup c ops

ops = [('=',Assign), ('+',Plus), ('-',Minus), ('*',Mult), ('/',Div), ('%',Mod), ('^',Power)]

function :: String -> Function
function f = case f of
    "sin" -> Sin
    "cos" -> Cos
    "tan" -> Tan
    "asin"-> Asin
    "acos"-> Acos
    "atan"-> Atan
    "log" -> Log
    "exp" -> Exp
    "sqrt"-> Sqrt

infixl 4 <&>
(<&>) = flip (<$>)

tokenize :: String -> Either String [Token]
tokenize = fromJust . tokenize' where
 tokenize' [] = return $ Right []
 tokenize' s@(x:xs) =
    oper x <&> (\op -> f (TOp $ operator op) xs) <|>
    match '(' x <&> (\_ -> f TLPar xs) <|>
    match ')' x <&> (\_ -> f TRPar xs) <|>
    match ',' x <&> (\_ -> f TComma xs) <|>
    space x <&> (\_ -> fromJust $ tokenize' xs) <|>
    readFun s <&> (\(fun, rest) -> f (TFun fun) rest) <|>
    readIdentifier s <&> (\(i,rest) -> f (TIdent i) rest) <|>
    readNumber s <&> (\(n,rest) -> f (TNumber n ) rest) <|>
    Just (Left $ "Cannot tokenize: " ++ s)
    where f out inp = (out:) <$> (fromJust . tokenize' $ inp)
          oper x = if x `elem` map fst ops then Just x else Nothing
          match c x = if c == x then Just x else Nothing
          space x = if isSpace x then Just x else Nothing
          readFun s = do
            ss <- find (`isPrefixOf` s) funs
            return (function . init $ ss, drop (length ss - 1) s)
          readIdentifier s@(x:_) = if isAlpha x || x == '_'
            then Just $ break (\x -> not (isAlpha x || isDigit x || (x == '_'))) s
            else Nothing
          readNumber s = let x = (reads :: String -> [(Double, String)]) s
                         in if null x then Nothing else Just $ head x

funs :: [String]
funs = ["sin(", "cos(", "asin(", "acos(", "tan(", "atan(", "log(", "exp(", "sqrt("]