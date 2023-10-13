{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Calculator.HomebrewParser where

import Calculator.HomebrewLexer (tloop)
import Calculator.Types
import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Ratio
import Data.Text (Text)
import Debug.Trace

data Input = Input
  { inputLoc :: Int
  , inputTok :: [Token]
  } deriving (Show, Eq)

inputUncons :: Input -> Maybe (Token, Input)
inputUncons (Input _ [])   = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

inputCons :: Token -> Input -> Input
inputCons x (Input loc xs) = Input (loc - 1) (x:xs)

data ParserError = ParserError { pe_loc :: Int, pe_msg :: Text } deriving (Show)

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
  (>>=) p f = Parser $ \input -> case runParser p input of
    Left err -> Left err
    Right (i, a) -> runParser (f a) i

eof :: Parser ()
eof = Parser $ \input -> case input of 
  (inputUncons -> Just (_, _)) -> Left $ ParserError (inputLoc input) "DUPA"
  _ -> return (input, ())

parse :: OpMap -> [Token] -> Either ParserError Expr
parse om ts = runParser (expr om) (Input 0 ts) >>= \(i, e) -> return e

expr :: OpMap -> Parser Expr
expr om = udfExpr om <|> udoExpr om <|> assignExpr om <|> opAliasExpr <|> (expr2 0.0 om <* eof)

expr2 :: Double -> OpMap -> Parser Expr
expr2 bp om = opcallExpr bp om <|> funcallExpr om <|> expr3 om

expr3 :: OpMap -> Parser Expr
expr3 om = parExpr om <|> idExpr <|> numExpr

eq :: Parser Token
eq = parseIf "=" (==TOp "=")

opAliasExpr :: Parser Expr
opAliasExpr = do
  op1 <- operator
  void eq
  op2 <- operator
  return $ UDO op1 (-1) L (Call op2 [Id "@x", Id "@y"])

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
            ("Expected " <> desc <> ", but found " <> showT y)
      _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected " <> desc <> ", but reached end of token list")

numExpr :: Parser Expr
numExpr = do
  (n, m) <- number
  return $ Number n m

number :: Parser (Rational, Rational)
number = exctractNum <$> parseIf "number" isTNumber
  where isTNumber (TNumber _ _) = True
        isTNumber _ = False
        exctractNum (TNumber n m) = (n, m)
        exctractNum _ = (0, 0)

idExpr :: Parser Expr
idExpr = Id <$> identifier

identifier :: Parser Text
identifier = exctractId <$> parseIf "id" isTIdent
  where isTIdent (TIdent _) = True
        isTIdent _ = False
        exctractId (TIdent _id) = _id
        exctractId _ = ""

parExpr :: OpMap -> Parser Expr
parExpr om = do
  parLeft
  ex <- expr2 0.0 om
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
    _ <- parseIf ")" isParRight
    return ()
  where isParRight TRPar = True
        isParRight _ = False

parens :: Parser a -> Parser a
parens x = parLeft *> x <* parRight

comma :: Parser Token
comma = parseIf "," (==TComma)

sepBy :: Parser a -> Parser Token -> Parser [a]
sepBy p s = liftA2 (:) p (concat <$> many ps) <|> pure []
  where ps = s *> some p

udfExpr :: OpMap -> Parser Expr
udfExpr om = do
  name <- identifier
  args <- parens $ sepBy identifier comma
  void eq
  UDF name args <$> expr2 0.0 om

udoExpr :: OpMap -> Parser Expr
udoExpr om = do
  name <- operator
  void parLeft
  (p, _) <- number
  void comma
  (a, _) <- number
  void parRight
  void eq
  UDO name (fromInteger . numerator $ p) (if a == 0 then L else R) <$> expr2 0.0 om

assignExpr :: OpMap -> Parser Expr
assignExpr om = do
  name <- identifier
  void eq
  Asgn name <$> expr2 0.0 om

funcallExpr :: OpMap -> Parser Expr
funcallExpr om = do
  name <- identifier
  void parLeft
  args <- sepBy (expr2 0.0 om) comma
  void parRight
  return $ Call name args

opcallExpr :: Double -> OpMap -> Parser Expr
opcallExpr min_bp o = Parser $ \input ->
  case input of
    (inputUncons -> Just (t, ts)) -> case t of
      TOp op -> case runParser (expr2 (prefix_binding_power op o) o) ts of
                  Left err -> Left err
                  Right (i, e) -> inner_loop (Call op [e]) min_bp o i
      TNumber a b -> inner_loop (Number a b) min_bp o ts
      TIdent a -> inner_loop (Id a) min_bp o ts
      TLPar -> case runParser (expr2 0.0 o) ts of
                 Left err -> Left err
                 Right (i, e) -> case i of 
                   (inputUncons -> Just (TRPar, i1)) -> inner_loop (Par e) min_bp o i1
                   _ -> Left $ ParserError (inputLoc i) "No closing bracket"
      _ -> Left $
        ParserError
          (inputLoc input)
          "Only numbers in the building"
    _ -> Left $
        ParserError
          (inputLoc input)
          "Expected token, but the list is empty"
  where
    inner_loop :: Expr -> Double -> OpMap -> Input -> Either ParserError (Input, Expr)
    inner_loop lhs bp om ts = case ts of
      (inputUncons -> Just (t, ts1)) -> case t of
        TRPar -> Right (ts, lhs)
        TOp op -> let (l_bp, r_bp) = infix_binding_power op om
                  in if l_bp < bp
                     then Right (ts, lhs)
                     else case runParser (expr2 r_bp om) ts1 of
                            Left err -> Left err
                            Right (ts2, e) -> inner_loop (Call op [lhs, e]) bp om ts2
        tok -> Left $ ParserError (inputLoc ts) $ "Wrong token: " <> showT tok
      _ -> Right (ts, lhs)
    infix_binding_power :: Text -> OpMap -> (Double, Double)
    infix_binding_power op om =
      let (Op pr asoc _) = om M.! op
          p = fromIntegral pr
      in case asoc of
        L -> (p, p + 0.25)
        R -> (p + 0.25, p)
    prefix_binding_power :: Text -> OpMap -> Double
    prefix_binding_power op om = let (Op pr _ _) = om M.! op
                                 in fromIntegral pr

testParser :: Parser a -> Text -> Either ParserError (Input, a)
testParser p input = case tloop input of
    Left err -> Left (ParserError 0 err)
    Right i -> runParser p (Input 0 i)
