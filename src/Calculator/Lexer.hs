module Calculator.Lexer (tokenize) where

import Data.Maybe (fromJust)
import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, find)
import Control.Applicative ((<|>))
import Calculator.Types (Token(..), Operator(..))

operator :: String -> Operator
operator s = fromJust $ lookup s ops

ops = longOps ++ shortOps
  where
  longOps = [("<=",Le), (">=",Ge), ("==",Eq), ("/=",Ne)]
  shortOps = [("=",Assign), ("+",Plus), ("-",Minus), ("*",Mult), ("/",Div), ("%",Mod), ("^",Power), ("<",Lt), (">",Gt)]

infixl 4 <&>
(<&>) = flip (<$>)

tokenize :: String -> Either String [Token]
tokenize = fromJust . tokenize' where
  tokenize' [] = return $ Right []
  tokenize' s@(x:xs) =
    oper s <&> (\op -> f (TOp $ operator op) (drop (length op) s)) <|>
    match '(' x <&> (\_ -> f TLPar xs) <|>
    match ')' x <&> (\_ -> f TRPar xs) <|>
    match ',' x <&> (\_ -> f TComma xs) <|>
    space x <&> (\_ -> fromJust $ tokenize' xs) <|>
    readIdentifier s <&> (\(i,rest) -> f (TIdent i) rest) <|>
    readNumber s <&> (\(n,rest) -> f (TNumber n ) rest) <|>
    Just (Left $ "Cannot tokenize: " ++ s)
    where
      f out inp = (out:) <$> (fromJust . tokenize' $ inp)
      oper s = find (`isPrefixOf` s) (map fst ops)
      match c x = if c == x then Just x else Nothing
      space x = if isSpace x then Just x else Nothing
      readIdentifier s@(x:_) = if isAlpha x || x == '_'
        then Just $ break (\x -> not (isAlpha x || isDigit x || (x == '_'))) s
        else Nothing
      readNumber s = let x = (reads :: String -> [(Double, String)]) s
        in if null x then Nothing else Just $ head x