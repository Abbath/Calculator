module Calculator.MegaParser (parser) where

import Text.Megaparsec
import Text.Megaparsec.Expr
import Calculator.Types
import Calculator.MegaLexer
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Monad.Reader
import Control.Lens ((&), (%~))
import Control.Lens.At

parser :: PReader Expr
parser = sc *> expr <* eof

expr :: PReader Expr
expr =  try udfExpr <|> try udoExpr <|> try assignExpr <|> try opAliasExpr <|> expr2

expr2 :: PReader Expr
expr2 =  try opcallExpr <|> try parExpr <|> try funcallExpr <|> expr3

expr3 :: PReader Expr
expr3 = try parExpr <|> try funcallExpr <|> try idExpr <|> numExpr

opAliasExpr :: PReader Expr
opAliasExpr = do
  op1 <- operator
  void eq
  op2 <- operator
  return $ UDO op1 (-1) L (OpCall op2 (Id "@x") (Id "@y"))

numExpr :: PReader Expr
numExpr = do
  n <- number
  case n of
    Left int -> return $ Number (fromIntegral int)
    Right doub -> return $ Number doub

idExpr :: PReader Expr
idExpr = do
  name <- identifier <* notFollowedBy (symbol "(")
  return $ Id name

parExpr :: PReader Expr
parExpr = do
  ex <- parens expr2
  return $ Par ex

udfExpr :: PReader Expr
udfExpr = do
  name <- identifier
  args <- parens $ sepBy1 identifier comma
  void eq
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
  void eq
  e <- expr2
  case (p, a) of
    (Left in1, Left in2) -> ret in1 (fromInteger in2) name e
    (Right db, Left int) -> ret (floor db) (fromInteger int)  name e
    (Left int, Right db) -> ret int db  name e
    (Right d1, Right d2) -> ret (floor d1) d2  name e
  where
    ret :: Integer -> Double -> String -> Expr -> PReader Expr
    ret a b n e = return $ UDO n (fromInteger a) (if b == 0 then L else R) e

assignExpr :: PReader Expr
assignExpr = do
  name <- identifier
  void eq
  e <- expr2
  return $ Asgn name e

funcallExpr :: PReader Expr
funcallExpr = do
  fname <- identifier
  args <- parens (sepBy1 expr2 comma) <* notFollowedBy (symbol "=" <* notFollowedBy (symbol "="))
  return $ FunCall fname args

opcallExpr :: PReader Expr
opcallExpr = do
  opMap <- ask
  makeExprParser expr3 (insertOps operators opMap)

operators :: [[Operator PReader Expr]]
operators =
  [[Prefix (try (symbol "-") *> pure UMinus)]
  ,[sop InfixR "^"]
  ,[sop InfixL "*", sop InfixL "/" {-((symbol "/" <* notFollowedBy (symbol "=")) *> pure (OpCall "/"))-}]
  ,[sop InfixL "+", sop InfixL "-"]
  ,[sop InfixL "<=", sop InfixL ">="
  , sop InfixL "<", sop InfixL ">"
  , sop InfixL "==", sop InfixL "!="]
  ]
  where sop i s = i (try (exactOper s ) *> pure (OpCall s))

genOp :: String -> (Int, Assoc) -> Operator PReader Expr
genOp s (_,L) = InfixL (try (symbol s) *> pure (OpCall s))
genOp s (_,R) = InfixR (try (symbol s) *> pure (OpCall s))

insertOps :: [[Operator PReader Expr]] -> Map String (Int, Assoc) -> [[Operator PReader Expr]]
insertOps [[]] _ =  [[]]
insertOps ops m | M.null m = ops
insertOps ops m = let (k,a) = M.elemAt 0 m
                      op = genOp k a
                  in insertOps (ops & ix (5 - fst a) %~ (op:)) (M.deleteAt 0 m)