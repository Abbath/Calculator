module Calculator.Lexer (tokenize) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Char (isSpace, isAlpha, isDigit)
import Data.List (isPrefixOf, find)
import Control.Applicative ((<|>))
import Calculator.Types (Token(..), Operator(..))
import Safe (headMay)

opSymbols = "+-/*%^!~&|=><"

isOp = (`elem` opSymbols)

infixl 4 <&>
(<&>) = flip (<$>)

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize s@(x:xs) = fromMaybe (Left ("Cannot tokenize: " ++ s)) $
    match '(' x <&> (\_ -> f TLPar xs) <|>
    match ')' x <&> (\_ -> f TRPar xs) <|>
    match ',' x <&> (\_ -> f TComma xs) <|>
    space x <&> (\_ -> tokenize xs) <|>
    readOperator s <&> (\op -> f (TStrOp op) (drop (length op) s)) <|>
    readIdentifier s <&> (\(i,rest) -> f (TIdent i) rest) <|>
    readNumber s <&> (\(n,rest) -> f (TNumber n ) rest)
  where
    f out inp = (out:) <$> tokenize inp
    match c x = if c == x then Just x else Nothing
    space x = if isSpace x then Just x else Nothing
    readOperator s = let s1 = takeWhile isOp s
      in if null s1 then Nothing else Just s1
    readIdentifier s@(x:_) = if isAlpha x || x == '_'
      then Just $ break (\x -> not (isAlpha x || isDigit x || (x == '_'))) s
      else Nothing
    readNumber s = let x = (reads :: String -> [(Double, String)]) s
      in if null x then Nothing else Just $ head x
