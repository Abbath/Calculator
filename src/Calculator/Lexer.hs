module Calculator.Lexer (tokenize) where

import Data.Maybe (fromMaybe)
import Data.Char (isSpace, isAlpha, isDigit)
import Control.Applicative ((<|>))
import Calculator.Types (Token(..))

opSymbols :: String
opSymbols = "+-/*%^!~&|=><"

isOp :: Char -> Bool
isOp = (`elem` opSymbols)

infixl 4 <&>
(<&>) :: Maybe a -> (a -> b) -> Maybe b
(<&>) = flip (<$>)

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize s@(x:xs) = fromMaybe (Left $ "Cannot tokenize: " ++ s) $
    match '(' x <&> (\_ -> f TLPar xs) <|>
    match ')' x <&> (\_ -> f TRPar xs) <|>
    match ',' x <&> (\_ -> f TComma xs) <|>
    space x <&> (\_ -> tokenize xs) <|>
    readOperator s <&> (\op -> f (TOp op) (drop (length op) s)) <|>
    readIdentifier s <&> (\(i,rest) -> f (TIdent i) rest) <|>
    readNumber s <&> (\(n,rest) -> f (TNumber n ) rest)
  where
    f out inp = (out:) <$> tokenize inp
    match c h = if c == h then Just h else Nothing
    space h = if isSpace h then Just h else Nothing
    readOperator str = let s1 = takeWhile isOp str
      in if null s1 then Nothing else Just s1
    readIdentifier [] = Nothing
    readIdentifier str@(h:_) = if isAlpha h || h == '_'
      then Just $ break (\z -> not (isAlpha z || isDigit z || (z == '_'))) str
      else Nothing
    readNumber str = let z = (reads :: String -> [(Double, String)]) str
      in if null z then Nothing else Just $ head z
