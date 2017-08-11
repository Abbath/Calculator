module Calculator.MegaLexer where

import           Calculator.Types
import           Control.Monad.Reader
import           Data.Map.Strict        (Map)
import           Data.Scientific
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String

type PReader = ReaderT (Map String (Int,Assoc)) Parser

sc :: PReader ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{" "}")

lexeme :: PReader a -> PReader a
lexeme = L.lexeme sc

symbol :: String -> PReader String
symbol = L.symbol sc

parens :: PReader a -> PReader a
parens = between (symbol "(") (symbol ")")

number :: PReader Scientific
number = lexeme L.number

comma :: PReader String
comma = symbol ","

identifier :: PReader String
identifier = lexeme p
  where p = (:) <$> (letterChar  <|> char '_') <*> many (alphaNumChar <|> char '_')

opSymbols :: String
opSymbols = "+-/*%$^!~&|=><"

operator :: PReader String
operator = lexeme (many . oneOf $ opSymbols)

exactOper :: String -> PReader String
exactOper s = symbol s <* notFollowedBy (oneOf opSymbols)

eq :: PReader String
eq = symbol "=" <* notFollowedBy (symbol "=")
