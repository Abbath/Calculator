{-# LANGUAGE OverloadedStrings, TypeApplications  #-}
module Calculator.MegaParser (parser) where

import Calculator.MegaLexer
    ( comma,
      eq,
      exactOper,
      identifier,
      number,
      number2,
      operator,
      parens,
      sc,
      symbol,
      PReader )
import Calculator.Types ( Assoc(..), Expr(..) )
import           Control.Lens         ((%~), (&))
import Control.Lens.At ( Ixed(ix) )
import Control.Monad.Reader ( void, MonadReader(ask) )
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import Data.Scientific ( floatingOrInteger )
import Text.Megaparsec
    ( (<|>), sepBy, MonadParsec(try, eof, notFollowedBy) )
import Control.Monad.Combinators.Expr
    ( makeExprParser, Operator(InfixR, Prefix, InfixL) )
import           Data.Functor (($>))
import           Data.Text    (Text)

parser :: PReader Expr
parser = sc *> expr <* eof

expr :: PReader Expr
expr =  try udfExpr <|> try udoExpr <|> try assignExpr <|> try opAliasExpr <|> expr2

expr2 :: PReader Expr
expr2 =  try opcallExpr <|> try parExpr <|> try funcallExpr <|> expr3

expr3 :: PReader Expr
expr3 = try parExpr <|> try funcallExpr <|> try idExpr <|> try numExpr2 <|> numExpr

opAliasExpr :: PReader Expr
opAliasExpr = do
  op1 <- operator
  void eq
  op2 <- operator
  return $ UDO op1 (-1) L (Call op2 [Id "@x", Id "@y"])

numExpr :: PReader Expr
numExpr = do
  n <- number
  return . Number . either (toRational @Double) (fromIntegral @Integer) 
    $ floatingOrInteger n

numExpr2 :: PReader Expr
numExpr2 = Number <$> number2

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
  args <- parens $ sepBy identifier comma
  void eq
  UDF name args <$> expr2 

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
  case (floatingOrInteger p :: Either Double Integer,  floatingOrInteger a :: Either Double Integer) of
    (Right in1, Right in2) -> ret in1 (fromInteger in2) name e
    (Left db, Right int)   -> ret (floor db) (fromInteger int) name e
    (Right int, Left db)   -> ret int db  name e
    (Left d1, Left d2)     -> ret (floor d1) d2 name e
  where
    ret :: Integer -> Double -> Text -> Expr -> PReader Expr
    ret a b n e = return $ UDO n (fromInteger a) (if b == 0 then L else R) e

assignExpr :: PReader Expr
assignExpr = do
  name <- identifier
  void eq
  Asgn name <$> expr2

funcallExpr :: PReader Expr
funcallExpr = do
  fname <- identifier
  args <- parens (sepBy expr2 comma) <* notFollowedBy (symbol "=" <* notFollowedBy (symbol "="))
  return $ Call fname args

opcallExpr :: PReader Expr
opcallExpr = do
  opMap <- ask
  makeExprParser expr3 (insertOps operators opMap)

operators :: [[Operator PReader Expr]]
operators =
  [[Prefix (try (symbol "-") $> UMinus)]
  ,[sop InfixR "^"]
  ,[sop InfixL "*", sop InfixL "/" {-((symbol "/" <* notFollowedBy (symbol "=")) *> pure (OpCall "/"))-}]
  ,[sop InfixL "+", sop InfixL "-"]
  ,[sop InfixL "<=", sop InfixL ">="
  , sop InfixL "<", sop InfixL ">"
  , sop InfixL "==", sop InfixL "!="
  , sop InfixL "&", sop InfixL "|"]
  ]
  where sop i s = i (try (exactOper s ) $> (\a b -> Call s [a, b]))

genOp :: Text -> (Int, Assoc) -> Operator PReader Expr
genOp s (_,L) = InfixL (try (symbol s) $> (\a b -> Call s [a, b]))
genOp s (_,R) = InfixR (try (symbol s) $> (\a b -> Call s [a, b]))

insertOps :: [[Operator PReader Expr]] -> Map Text(Int, Assoc) -> [[Operator PReader Expr]]
insertOps [[]] _ =  [[]]
insertOps ops m | M.null m = ops
insertOps ops m = let (k,a) = M.elemAt 0 m
                      op = genOp k a
                  in insertOps (ops & ix (5 - fst a) %~ (op:)) (M.deleteAt 0 m)
