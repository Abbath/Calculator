module Calculator.Lexer (tokenize) where

import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, find)
import Control.Applicative ((<|>))
import Calculator.Types (Token(..), Operator(..))
import Safe (headMay)

ops = longOps ++ shortOps
  where
  longOps = [("<=",Le), (">=",Ge), ("==",Eq), ("/=",Ne)]
  shortOps = [("=",Assign), ("+",Plus), ("-",Minus), ("*",Mult), ("/",Div), ("%",Mod), ("^",Power), ("<",Lt), (">",Gt)]

infixl 4 <&>
(<&>) = flip (<$>)

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize s@(x:xs) = fromMaybe (Left ("Cannot tokenize: " ++ s)) $
    oper s <&> (\(opStr, op) -> f (TOp op) (drop (length opStr) s)) <|>
    match '(' x <&> (\_ -> f TLPar xs) <|>
    match ')' x <&> (\_ -> f TRPar xs) <|>
    match ',' x <&> (\_ -> f TComma xs) <|>
    space x <&> (\_ -> tokenize xs) <|>
    readIdentifier s <&> (\(i,rest) -> f (TIdent i) rest) <|>
    readNumber s <&> (\(n,rest) -> f (TNumber n ) rest)
  where
    f out inp = (out:) <$> tokenize inp
    oper s =
        let mayOper (opStr, op) = if opStr `isPrefixOf` s then Just (opStr, op) else Nothing
        in headMay (catMaybes (map mayOper ops))
    match c x = if c == x then Just x else Nothing
    space x = if isSpace x then Just x else Nothing
    readIdentifier s@(x:_) = if isAlpha x || x == '_'
      then Just $ break (\x -> not (isAlpha x || isDigit x || (x == '_'))) s
      else Nothing
    readNumber s = let x = (reads :: String -> [(Double, String)]) s
      in if null x then Nothing else Just $ head x
