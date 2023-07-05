{-# LANGUAGE OverloadedStrings #-}
module Calculator.Lexer (tokenize) where

import           Calculator.Types    (Token (..))
import           Control.Applicative ((<|>))
import           Data.Bifunctor      (bimap)
import           Data.Char           (isAlpha, isDigit, isSpace)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T

opSymbols :: String
opSymbols = "+-/*%^$!~&|=><"

isOp :: Char -> Bool
isOp = (`elem` opSymbols)

infixl 4 <&>
(<&>) :: Maybe a -> (a -> b) -> Maybe b
(<&>) = flip (<$>)

readT :: Text -> [(Double, String)]
readT = reads . T.unpack

tokenize :: Text -> Either Text [Token]
tokenize s = case T.uncons s of
  Nothing -> Right []
  Just (x, xs) -> fromMaybe (Left $ "Cannot tokenize: " <> s) $
    match '(' x <&> (\_ -> f TLPar xs) <|>
    match ')' x <&> (\_ -> f TRPar xs) <|>
    match ',' x <&> (\_ -> f TComma xs) <|>
    space x <&> (\_ -> tokenize xs) <|>
    readOperator s <&> (\op -> f (TOp op) (T.drop (T.length op) s)) <|>
    readIdentifier s <&> (\(i,rest) -> f (TIdent i) rest) <|>
    readNumber s <&> (\(n,rest) -> f (TNumber n 0) rest)
  where
    f out inp = (out:) <$> tokenize inp
    match c h = if c == h then Just h else Nothing
    space h = if isSpace h then Just h else Nothing
    readOperator str = let s1 = T.takeWhile isOp str
      in if T.null s1 then Nothing else Just s1
    readIdentifier str = case T.uncons str of
      Nothing -> Nothing
      Just (h, _) -> if isAlpha h || h == '_'
                      then Just $ T.break (\z -> not (isAlpha z || isDigit z || (z == '_'))) str
                      else Nothing
    readNumber str = let z = readT str
      in if null z then Nothing else Just $ bimap toRational T.pack . head $ z
