{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Calculator.HomebrewParser (parser, runParser, Input(..)) where

import           Control.Applicative

import           Calculator.Types
-- import           Control.Lens         ((%~), (&))
-- import           Control.Lens.At
import           Control.Monad.Reader
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as M

import qualified Data.Text as T
import           Data.Text    (Text)
import Data.Ratio

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

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (Parser p) f = Parser $ \input -> let x = p input in case x of
    Left err -> Left err
    Right (i, a) -> runParser (f a) i

data OpMapWithPrecedence = OMWP {
  ops :: OpMap,
  prec :: Int
}

parser :: OpMap -> Parser Expr
parser om = expr $ OMWP om 0

expr :: OpMapWithPrecedence -> Parser Expr
expr om = udfExpr om <|> udoExpr om <|> assignExpr om <|> opAliasExpr <|> expr2 om

expr2 :: OpMapWithPrecedence -> Parser Expr
expr2 om = opcallExpr om <|> parExpr om <|> funcallExpr om <|> expr3 om

expr3 :: OpMapWithPrecedence -> Parser Expr
expr3 om = parExpr om <|> funcallExpr om <|> idExpr <|> numExpr

eq :: Parser Token
eq = parseIf "=" (==TOp "=")

opAliasExpr :: Parser Expr
opAliasExpr = do
  op1 <- operator Nothing
  void eq
  op2 <- operator Nothing
  return $ UDO op1 (-1) L (Call op2 [Id "@x", Id "@y"])

operator :: Maybe OpMapWithPrecedence -> Parser Text
operator Nothing = exctractOp <$> parseIf "operator" isTOp
  where isTOp (TOp _) = True
        isTOp _ = False
        exctractOp (TOp op) = op
        exctractOp _ = ""
operator (Just (OMWP om p)) = exctractOp <$> parseIf "operator2" isTOp
  where
    isTOp (TOp op) = precedence (om M.! op) == p
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
numExpr = do
  n <- number
  return $ Number n 0

number :: Parser Rational
number = exctractNum <$> parseIf "number" isTNumber
  where isTNumber (TNumber _ _) = True
        isTNumber _ = False
        exctractNum (TNumber n _) = n
        exctractNum _ = 0

idExpr :: Parser Expr
idExpr = Id <$> identifier

identifier :: Parser Text
identifier = exctractId <$> parseIf "id" isTIdent
  where isTIdent (TIdent _) = True
        isTIdent _ = False
        exctractId (TIdent _id) = _id
        exctractId _ = ""

parExpr :: OpMapWithPrecedence -> Parser Expr
parExpr om = do
  parLeft
  ex <- expr2 om
  parRight
  return $ Par ex

parLeft :: Parser ()
parLeft = do
    _ <- parseIf "(" isParLeft
    return ()
  where isParLeft TLPar = True
        isParLeft _ = False

parRight :: Parser ()
parRight = do
    _ <- parseIf "(" isParRight
    return ()
  where isParRight TRPar = True
        isParRight _ = False

parens :: Parser a -> Parser a
parens x = parLeft *> x <* parRight

comma :: Parser Token
comma = parseIf "," (==TComma)

sepBy1 :: Parser a -> Parser Token -> Parser [a]
sepBy1 p s = concat <$> many ps
  where ps = some p <* s

udfExpr :: OpMapWithPrecedence -> Parser Expr
udfExpr om = do
  name <- identifier
  args <- parens $ sepBy1 identifier comma
  void eq
  UDF name args <$> expr2 om

udoExpr :: OpMapWithPrecedence -> Parser Expr
udoExpr om = do
  name <- operator Nothing
  void parLeft
  p <- number
  void comma
  a <- number
  void parLeft
  void eq
  UDO name (fromInteger . numerator $ p) (if a == 0 then L else R) <$> expr2 om

assignExpr :: OpMapWithPrecedence -> Parser Expr
assignExpr om = do
  name <- identifier
  void eq
  Asgn name <$> expr2 om

funcallExpr :: OpMapWithPrecedence -> Parser Expr
funcallExpr om = do
  fname <- identifier
  args <- parens (sepBy1 (expr2 om) comma) <* parseIf "=" (/=TEqual)
  return $ Call fname args

opcallExpr :: OpMapWithPrecedence -> Parser Expr
opcallExpr omwp@(OMWP om p) = do
  a <- expr3 omwp{prec = p + 1}
  op <- operator (Just omwp)
  b <- expr3 omwp{prec = p + 1}
  return $ Call op [a, b]

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
