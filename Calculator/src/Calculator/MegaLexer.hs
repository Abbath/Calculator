{-# LANGUAGE OverloadedStrings  #-}

module Calculator.MegaLexer where

import           Calculator.Types
import           Control.Monad.Reader
import           Data.Map.Strict        (Map)
import           Data.Scientific
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer  as L
import           Text.Megaparsec.Char
import           Data.Void
import           Data.Text    (Text)
import qualified Data.Text    as T

type Parser = Parsec Void Text

type PReader = ReaderT (Map Text (Int, Assoc)) Parser

sc :: PReader ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{" "}")

lexeme :: PReader a -> PReader a
lexeme = L.lexeme sc

symbol :: Text -> PReader Text
symbol = L.symbol sc

parens :: PReader a -> PReader a
parens = between (symbol "(") (symbol ")")

number :: PReader Scientific
number = lexeme L.scientific

comma :: PReader Text
comma = symbol ","

identifier :: PReader Text
identifier = lexeme p
  where p = T.cons <$> (letterChar  <|> char '_') <*> (T.pack <$> many (alphaNumChar <|> char '_'))

opSymbols :: String
opSymbols = "+-/*%$^!~&|=><"

operator :: PReader Text
operator = lexeme ( T.pack <$> (many . oneOf $ opSymbols))

exactOper :: Text -> PReader Text
exactOper s = symbol s <* notFollowedBy (oneOf opSymbols)

eq :: PReader Text
eq = symbol "=" <* notFollowedBy (symbol "=")
