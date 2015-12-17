module Calculator.MegaParser (parser) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Calculator.Types
import Calculator.MegaLexer

parser :: Parser Expr
parser = sc *> expr <* eof

expr :: Parser Expr
expr =  try expr2 <|> try udfExpr <|> assignExpr

expr2 :: Parser Expr
expr2 =  try funcallExpr <|> try parExpr <|> try opcallExpr <|> try idExpr <|> numExpr

expr3 :: Parser Expr
expr3 = try idExpr <|> try numExpr <|> try funcallExpr <|> try parExpr

numExpr :: Parser Expr
numExpr = do
  n <- number
  case n of
    Left int -> return $ Number (fromIntegral int)
    Right doub -> return $ Number doub

idExpr :: Parser Expr
idExpr = do
  name <- identifier <* notFollowedBy (symbol "(" <|> symbol "=")
  return $ Id name

parExpr :: Parser Expr
parExpr = do
  ex <- parens expr2
  return $ Par ex

udfExpr :: Parser Expr
udfExpr = do
  name <- identifier
  args <- parens $ sepBy1 identifier comma
  void $ symbol "="
  e    <- expr2
  return $ UDF name args e

assignExpr :: Parser Expr
assignExpr = do
  name <- identifier
  void $ symbol "="
  e <- expr2
  return $ Asgn name e

uminusExpr :: Parser Expr
uminusExpr = do
  void $ symbol "-"
  e <- expr2
  return $ UMinus e

funcallExpr :: Parser Expr
funcallExpr = do
  fname <- identifier
  args <- parens (sepBy1 expr2 comma) <* notFollowedBy (symbol "=")
  return $ FunCall fname args

opcallExpr :: Parser Expr
opcallExpr = makeExprParser expr3 operators

operators :: [[Operator Parser Expr]]
operators =
  [[Prefix (symbol "-" *> pure UMinus)]
  ,[InfixR (symbol "^" *> pure (OpCall "^"))]
  ,[InfixL (symbol "*" *> pure (OpCall "*")), InfixL (symbol "/" *> pure (OpCall "/"))]
  ,[InfixL (symbol "+" *> pure (OpCall "+")), InfixL (symbol "-" *> pure (OpCall "-"))]
  ,[InfixL (symbol "<" *> pure (OpCall "<")), InfixL (symbol ">" *> pure (OpCall ">"))
  ,InfixL (symbol "<=" *> pure (OpCall "<=")), InfixL (symbol ">=" *> pure (OpCall ">="))
  ,InfixL (symbol "==" *> pure (OpCall "==")), InfixL (symbol "/=" *> pure (OpCall "/="))]
  ]