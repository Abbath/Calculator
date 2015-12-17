module Calculator.MegaLexer where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{" "}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

number :: Parser (Either Integer Double)
number = lexeme L.number

comma :: Parser String
comma = symbol ","

identifier :: Parser String
identifier = lexeme p
  where p = (:) <$> (letterChar  <|> char '_') <*> many (alphaNumChar <|> char '_')

