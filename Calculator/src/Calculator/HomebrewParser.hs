{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Calculator.HomebrewParser (parser) where

import           Control.Applicative

import           Calculator.HomebrewLexer 
import           Calculator.Types
-- import           Control.Lens         ((%~), (&))
-- import           Control.Lens.At
import           Control.Monad.Reader
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M
import           Data.Scientific
-- import           Control.Monad.Combinators.Expr
-- import           Data.Functor (($>))
import qualified Data.Text as T
import           Data.Text    (Text)

data Input = Input
  { inputLoc :: Int
  , inputStr :: [Token]
  } deriving (Show, Eq)

inputUncons :: Input -> Maybe (Token, Input)
inputUncons (Input _ [])   = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

data ParserError = ParserError Int Text deriving (Show)

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (Input, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance {-# OVERLAPPING #-} Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

parser :: Parser Expr
parser = expr 

expr :: Parser Expr
expr =  udfExpr <|> udoExpr <|> assignExpr <|> opAliasExpr <|> expr2

expr2 :: Parser Expr
expr2 =  opcallExpr <|> parExpr <|> funcallExpr <|> expr3

expr3 :: Parser Expr
expr3 = parExpr <|> funcallExpr <|> idExpr <|> numExpr

opAliasExpr :: Parser Expr
opAliasExpr = do
  op1 <- operator
  void eq
  op2 <- operator
  return $ UDO op1 (-1) L (OpCall op2 (Id "@x") (Id "@y"))

operator :: Parser Text
operator = exctractOp <$> parseIf "operator" isTOp
  where isTOp (TOp _) = True
        isTOp _ = False
        exctractOp (TOp op) = op
        exctractOp _ = ""

parseIf :: Text -> (Token -> Bool) -> Parser Token
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
          Left $
          ParserError
            (inputLoc input)
            ("Expected " <> desc <> ", but found shit")
      _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected " <> desc <> ", but reached end of string")

numExpr :: Parser Expr
numExpr = Number <$> number <*> 0

number :: Parser Rational
number = exctractNum <$> parseIf "number" isTNumber
  where isTNumber (TNumber _ _) = True
        isTNumber _ = False
        exctractNum (TNumber n _) = n
        exctractNum _ = ""

idExpr :: Parser Expr
idExpr = Id <$> identifier 

identifier :: Parser Text
identifier = exctractId <$> parseIf "id" isTIdent
  where isTIdent (TIdent _) = True
        isTIdent _ = False
        exctractId (TIdent _id) = _id
        exctractId _ = ""

parExpr :: Parser Expr
parExpr = do
  parLeft
  ex <- expr2
  parRight
  return $ Par ex

parLeft :: Parser ()
parLeft = do
    parseIf "(" isParLeft
    return ()
  where isParLeft TLPar = True
        isParLeft _ = False

parRight :: Parser ()
parRight = do
    parseIf "(" isParRight
    return ()
  where isParRight TRPar = True
        isParRight _ = False

-- udfExpr :: PReader Expr
-- udfExpr = do
--   name <- identifier
--   args <- parens $ sepBy1 identifier comma
--   void eq
--   UDF name args <$> expr2 

-- udoExpr :: PReader Expr
-- udoExpr = do
--   name <- operator
--   void $ symbol "("
--   p <- number
--   void comma
--   a <- number
--   void $ symbol ")"
--   void eq
--   e <- expr2
--   case (floatingOrInteger p :: Either Double Integer,  floatingOrInteger a :: Either Double Integer) of
--     (Right in1, Right in2) -> ret in1 (fromInteger in2) name e
--     (Left db, Right int)   -> ret (floor db) (fromInteger int)  name e
--     (Right int, Left db)   -> ret int db name e
--     (Left d1, Left d2)     -> ret (floor d1) d2 name e
--   where
--     ret :: Integer -> Double -> Text -> Expr -> PReader Expr
--     ret a b n e = return $ UDO n (fromInteger a) (if b == 0 then L else R) e

-- assignExpr :: PReader Expr
-- assignExpr = do
--   name <- identifier
--   void eq
--   Asgn name <$> expr2

-- funcallExpr :: PReader Expr
-- funcallExpr = do
--   fname <- identifier
--   args <- parens (sepBy1 expr2 comma) <* notFollowedBy (symbol "=" <* notFollowedBy (symbol "="))
--   return $ FunCall fname args

-- opcallExpr :: PReader Expr
-- opcallExpr = do
--   opMap <- ask
--   makeExprParser expr3 (insertOps operators opMap)

-- operators :: [[Operator PReader Expr]]
-- operators =
--   [[Prefix (try (symbol "-") $> UMinus)]
--   ,[sop InfixR "^"]
--   ,[sop InfixL "*", sop InfixL "/" {-((symbol "/" <* notFollowedBy (symbol "=")) *> pure (OpCall "/"))-}]
--   ,[sop InfixL "+", sop InfixL "-"]
--   ,[sop InfixL "<=", sop InfixL ">="
--   , sop InfixL "<", sop InfixL ">"
--   , sop InfixL "==", sop InfixL "!="]
--   ]
--   where sop i s = i (try (exactOper s ) $> OpCall s)

-- genOp :: Text -> (Int, Assoc) -> Operator PReader Expr
-- genOp s (_,L) = InfixL (try (symbol s) $> OpCall s)
-- genOp s (_,R) = InfixR (try (symbol s) $> OpCall s)

-- insertOps :: [[Operator PReader Expr]] -> Map Text(Int, Assoc) -> [[Operator PReader Expr]]
-- insertOps [[]] _ =  [[]]
-- insertOps ops m | M.null m = ops
-- insertOps ops m = let (k,a) = M.elemAt 0 m
--                       op = genOp k a
--                   in insertOps (ops & ix (5 - fst a) %~ (op:)) (M.deleteAt 0 m)

