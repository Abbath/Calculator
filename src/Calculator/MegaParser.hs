module Calculator.MegaParser (parser) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Calculator.Types
import Calculator.MegaLexer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Lens
import Control.Lens.At

parser :: PReader Expr
parser = sc *> expr <* eof

expr :: PReader Expr
expr = try expr2 <|> try udfExpr <|> try udoExpr <|> assignExpr

expr2 :: PReader Expr
expr2 =  try funcallExpr <|> try parExpr <|> try opcallExpr <|> try idExpr <|> numExpr

expr3 :: PReader Expr
expr3 = try idExpr <|> try numExpr <|> try funcallExpr <|> try parExpr

numExpr :: PReader Expr
numExpr = do
  n <- number
  case n of
    Left int -> return $ Number (fromIntegral int)
    Right doub -> return $ Number doub

idExpr :: PReader Expr
idExpr = do
  name <- identifier <* notFollowedBy (symbol "(" <|> symbol "=")
  return $ Id name

parExpr :: PReader Expr
parExpr = do
  ex <- parens expr2
  return $ Par ex

udfExpr :: PReader Expr
udfExpr = do
  name <- identifier
  args <- parens $ sepBy1 identifier comma
  void $ symbol "="
  e    <- expr2
  return $ UDF name args e

udoExpr :: PReader Expr
udoExpr = do
  name <- operator
  void $ symbol "("
  p <- number
  void comma
  a <- number
  void $ symbol ")"
  void $ symbol "="
  e <- expr2
  case (p, a) of
    (Left in1, Left in2) -> ret (fromIntegral in1) (fromIntegral in2) name e
    (Right db, Left int) -> ret (floor db) (fromIntegral int)  name e
    (Left int, Right db) -> ret (fromIntegral int) db  name e
    (Right d1, Right d2) -> ret (floor d1) d2  name e
  where ret a b n e = return $ UDO n a (if b == 0 then L else R) e

assignExpr :: PReader Expr
assignExpr = do
  name <- identifier
  void $ symbol "="
  e <- expr2
  return $ Asgn name e

uminusExpr ::  PReader Expr
uminusExpr = do
  void $ symbol "-"
  e <- expr2
  return $ UMinus e

funcallExpr :: PReader Expr
funcallExpr = do
  fname <- identifier
  args <- parens (sepBy1 expr2 comma) <* notFollowedBy (symbol "=")
  return $ FunCall fname args

opcallExpr :: PReader Expr
opcallExpr = do
  opMap <- ask
  makeExprParser expr3 (insertOps operators opMap)

operators :: [[Operator PReader Expr]]
operators =
  [[Prefix (try (symbol "-") *> pure UMinus)]
  ,[sop InfixR "^"]
  ,[sop InfixL "*", InfixL (try (symbol "/" <* notFollowedBy (symbol "=")) *> pure (OpCall "/"))]
  ,[sop InfixL "+", sop InfixL "-"]
  ,[sop InfixL "<=", sop InfixL ">="
  , sop InfixL "<", sop InfixL ">"
  , sop InfixL "==", sop InfixL "/="]
  ]
  where sop i s = i (try(symbol s ) *> pure (OpCall s))

genOp :: String -> (Int, Assoc) -> Operator PReader Expr
genOp s (_,L) = InfixL (try (symbol s) *> pure (OpCall s))
genOp s (_,R) = InfixR (try (symbol s) *> pure (OpCall s))

insertOps :: [[Operator PReader Expr]] -> Map String (Int, Assoc) -> [[Operator PReader Expr]]
insertOps [[]] _ =  [[]]
insertOps ops m | M.null m = ops
insertOps ops m = let (k,a) = M.elemAt 0 m
                      op = genOp k a
                  in insertOps (ops & ix (fst a) %~ (op:)) (M.deleteAt 0 m)