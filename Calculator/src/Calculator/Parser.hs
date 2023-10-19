{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE InstanceSigs #-}
module Calculator.Parser where

import Calculator.Lexer (tloop)
import Calculator.Types
import Control.Applicative
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens ((^.), _3)

data Input = Input
  { inputLoc :: Int
  , inputTok :: [Token]
  } deriving (Show, Eq)

inputUncons :: Input -> Maybe (Token, Input)
inputUncons (Input _ [])   = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

inputCons :: Token -> Input -> Input
inputCons x (Input loc xs) = Input (loc - 1) (x:xs)

inputPeek :: Input -> Maybe Token
inputPeek (Input _ []) = Nothing
inputPeek (Input _ (x:_)) = Just x

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

parse :: Maps -> [Token] -> Either ParserError Expr
parse m ts = runParser (stmt m) (Input 0 ts) >>= \(i, e) -> return e

stmt :: Maps -> Parser Expr
stmt m = udfStmt m <|> udoStmt m <|> assignStmt m <|> opAliasStmt <|> (expr 0.0 m <* eof)

eq :: Parser Token
eq = parseIf "=" (==TOp "=")

opAliasStmt :: Parser Expr
opAliasStmt = do
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

number :: Parser (Rational, Rational)
number = exctractNum <$> parseIf "number" isTNumber
  where isTNumber (TNumber _ _) = True
        isTNumber _ = False
        exctractNum (TNumber n m) = (n, m)
        exctractNum _ = (0, 0)

identifier :: Parser Text
identifier = exctractId <$> parseIf "id" isTIdent
  where isTIdent (TIdent _) = True
        isTIdent _ = False
        exctractId (TIdent _id) = _id
        exctractId _ = ""

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

udfStmt :: Maps -> Parser Expr
udfStmt m = do
  name <- identifier
  args <- parens $ sepBy identifier comma
  void eq
  UDF name args <$> expr 0.0 m

udoStmt :: Maps -> Parser Expr
udoStmt m = do
  name <- operator
  void parLeft
  (p, _) <- number
  void comma
  (a, _) <- number
  void parRight
  void eq
  UDO name (fromInteger . numerator $ p) (if a == 0 then L else R) <$> expr 0.0 m

assignStmt :: Maps -> Parser Expr
assignStmt m = do
  name <- identifier
  void eq
  Asgn name <$> expr 0.0 m

expr :: Double -> Maps -> Parser Expr
expr min_bp m = Parser $ \input ->
  case input of
    (inputUncons -> Just (t, ts)) -> case t of
      TOp op -> case inputPeek ts of
        Just TLPar -> do
          (i, e) <- runParser (parens (sepBy (expr 0.0 m) comma)) ts
          inner_loop (Call op e) min_bp m i
        _ -> do
          (i, e) <- runParser (expr (prefix_binding_power op m) m) ts
          funop <- selectOp op
          inner_loop (Call funop [e]) min_bp m i
      TNumber a b -> inner_loop (Number a b) min_bp m ts
      TIdent a -> case inputPeek ts of
        Just TLPar -> do
          (i, e) <- runParser (parens (sepBy (expr 0.0 m) comma)) ts
          inner_loop (Call a e) min_bp m i
        _ -> inner_loop (Id a) min_bp m ts
      TLPar -> do
        (i, e) <- runParser (expr 0.0 m) ts
        case i of
          (inputUncons -> Just (TRPar, i1)) -> inner_loop (Par e) min_bp m i1
          _ -> Left $ ParserError (inputLoc i) "No closing bracket"
      tok -> Left $
        ParserError
          (inputLoc input)
          ("Only numbers in the building " <> showT tok)
    _ -> Left $
        ParserError
          (inputLoc input)
          "Expected token, but the list is empty"
  where
    inner_loop :: Expr -> Double -> Maps -> Input -> Either ParserError (Input, Expr)
    inner_loop lhs bp om ts = case ts of
      (inputUncons -> Just (t, ts1)) -> case t of
        TRPar -> Right (ts, lhs)
        TComma -> Right (ts, lhs)
        TOp op -> case infix_binding_power op om of
          Nothing -> Left $ ParserError (inputLoc ts) $ "Operator does not exist: " <> showT op
          Just (l_bp, r_bp) ->
            if l_bp < bp
            then Right (ts, lhs)
            else do
              (ts2, e) <- runParser (expr r_bp om) ts1
              inner_loop (Call op [lhs, e]) bp om ts2
        tok -> Left $ ParserError (inputLoc ts) $ "Wrong token: " <> showT tok
      _ -> Right (ts, lhs)
    infix_binding_power :: Text -> Maps -> Maybe (Double, Double)
    infix_binding_power op ms =
      if T.all (`elem` opSymbols) op
        then do
          (Op pr asoc _) <- op `M.lookup` (ms^._3)
          let p = fromIntegral pr
          return $ case asoc of
            L -> (p, p + 0.25)
            R -> (p + 0.25, p)
        else return (13, 13.25)
    prefix_binding_power :: Text -> Maps -> Double
    prefix_binding_power op ms = let (Op pr _ _) = (ms^._3) M.! op
                                 in fromIntegral pr
    selectOp :: Text -> Either ParserError Text
    selectOp op = let ops = [("~", "comp"), ("!", "fact"), ("-", "-")]
                  in case M.lookup op ops of
                    Nothing -> Left $ ParserError 0 ("No such operator: " <> op)
                    Just fun -> return fun

testParser :: Parser a -> Text -> Either ParserError (Input, a)
testParser p input = case tloop input of
    Left err -> Left (ParserError 0 err)
    Right i -> runParser p (Input 0 i)
