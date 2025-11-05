{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Calculator.Parser where

import Calculator.Builtins (maxPrecedence)
import Calculator.Lexer (tloop)
import Calculator.Types (
  Assoc (..),
  Expr (..),
  Maps,
  Op (Op, associativity, precedence),
  OpArity (..),
  Token (..),
  Unit (Unitless),
  isSpaceFun,
  numToText,
  opSymbols,
  opmap,
  renderTokens,
  showT,
 )
import Control.Applicative (Alternative (..))
import Control.Lens (at, (^.))
import Control.Monad (void)
import Data.Complex
import Data.Ratio (numerator)
import Data.Text (Text)
import Data.Text qualified as T

data Input = Input
  { inputLoc :: Int
  , inputTok :: [Token]
  }
  deriving (Show, Eq)

inputUncons :: Input -> Maybe (Token, Input)
inputUncons (Input _ []) = Nothing
inputUncons (Input loc (x : xs)) = Just (x, Input (loc + 1) xs)

inputCons :: Token -> Input -> Input
inputCons x (Input loc xs) = Input (loc - 1) (x : xs)

inputPeek :: Input -> Maybe Token
inputPeek (Input _ []) = Nothing
inputPeek (Input _ (x : _)) = Just x

inputTail :: Input -> Input
inputTail (Input x []) = Input x []
inputTail (Input x (_ : xs)) = Input x xs

newtype Parser a = Parser
  { runParser :: Input -> Either Text (Input, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      pure (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      pure (input'', f a)

instance {-# OVERLAPPING #-} Alternative (Either Text) where
  empty = Left "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p f = Parser $ \input -> do
    (i, a) <- runParser p input
    runParser (f a) i

parse :: Maps -> [Token] -> Either Text Expr
parse m ts =
  runParser (stmt m) (Input 0 ts) >>= \(Input n ts1, e) -> case ts1 of
    [] -> pure e
    _ -> Left $ "Extra input at " <> showT n <> ": " <> renderTokens ts1

stmt :: Maps -> Parser Expr
stmt m = udfStmt m <|> udoStmt m <|> assignStmt m <|> labelStmt <|> opAliasStmt <|> imprtStmt <|> expr 0.0 m

eq :: Parser Token
eq = parseIf "=" (== TOp "=") <|> parseIf "<-" (== TOp "<-")

eq2 :: Parser Token
eq2 = parseIf "=" (== TOp "=") <|> parseIf "->" (== TOp "->")

labelStmt :: Parser Expr
labelStmt = Label . extractLabel <$> parseIf "label" isLabel
 where
  isLabel (TLabel _) = True
  isLabel _ = False
  extractLabel (TLabel l) = l
  extractLabel _ = ""

opAliasStmt :: Parser Expr
opAliasStmt = do
  op1 <- operator
  void eq
  op2 <- operator
  pure $ UDO op1 (-1) L (Call op2 [Id "#x", Id "#y"])

operator :: Parser Text
operator = extractOp <$> parseIf "operator" isTOp
 where
  isTOp (TOp _) = True
  isTOp _ = False
  extractOp (TOp op) = op
  extractOp _ = ""

parseIf :: Text -> (Token -> Bool) -> Parser Token
parseIf desc f =
  Parser $
    \case
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
            Left ("Expected " <> desc <> ", but found " <> showT y)
      _ ->
        Left ("Expected " <> desc <> ", but reached end of token list")

number :: Parser (Rational, Rational)
number = extractNum <$> parseIf "number" isTNumber
 where
  isTNumber (TNumber _ _) = True
  isTNumber _ = False
  extractNum (TNumber n m) = (n, m)
  extractNum _ = (0, 0)

identifier :: Parser Text
identifier = extractId <$> parseIf "id" isTIdent
 where
  isTIdent (TIdent _) = True
  isTIdent _ = False
  extractId (TIdent _id) = _id
  extractId _ = ""

parLeft :: Parser ()
parLeft = void $ parseIf "(" (== TLPar)

parRight :: Parser ()
parRight = void $ parseIf ")" (== TRPar)

braLeft :: Parser ()
braLeft = void $ parseIf "[" (== TLBracket)

braRight :: Parser ()
braRight = void $ parseIf "]" (== TRBracket)

parens :: Parser a -> Parser a
parens x = parLeft *> x <* parRight

brackets :: Parser a -> Parser a
brackets x = braLeft *> x <* braRight

comma :: Parser Token
comma = parseIf "," (== TComma)

dots :: Parser Text
dots = "..." <$ parseIf "..." (== TDots)

sepBy :: Parser a -> Parser Token -> Parser [a]
sepBy p s = (:) <$> p <*> (concat <$> many ps) <|> pure []
 where
  ps = s *> some p

trimTick :: Text -> Text
trimTick name = if "'" `T.isSuffixOf` name then T.init name else name

udfStmt :: Maps -> Parser Expr
udfStmt m = do
  let ldots = (: []) <$> dots
  name <- identifier
  args <- parens $ ldots <|> (++) <$> sepBy identifier comma <*> (comma *> ldots <|> pure [])
  void eq2
  UDF (trimTick name) args <$> expr 0.0 m

copyPrec :: Maps -> Parser (Rational, Rational)
copyPrec m = do
  void $ parseIf "p" (== TIdent "p")
  void parLeft
  name <- operator
  void parRight
  let p1 = precedence <$> (m ^. (opmap . at (name, Ar1)))
  let p2 = precedence <$> (m ^. (opmap . at (name, Ar2)))
  pure . (,0) . toRational $ case (p1, p2) of
    (Nothing, Nothing) -> -2
    (Just p, Nothing) -> p
    (Nothing, Just p) -> p
    (Just _, Just p) -> p

copyAssoc :: Maps -> Parser (Rational, Rational)
copyAssoc m = do
  void $ parseIf "a" (== TIdent "a")
  void parLeft
  name <- operator
  void parRight
  let p1 = associativity <$> (m ^. (opmap . at (name, Ar1)))
  let p2 = associativity <$> (m ^. (opmap . at (name, Ar2)))
  pure . (,0) . toRational . fromEnum $ case (p1, p2) of
    (Nothing, Nothing) -> N
    (Just p, Nothing) -> p
    (Nothing, Just p) -> p
    (Just _, Just p) -> p

assocLetter :: Parser (Rational, Rational)
assocLetter = do
  name <- identifier
  pure . (,0) $ case name of
    "L" -> 0
    "R" -> 1
    _ -> 2

udoStmt :: Maps -> Parser Expr
udoStmt m = do
  name <- operator
  void parLeft
  (p, _) <- number <|> copyPrec m
  void comma
  (a, _) <- number <|> copyAssoc m <|> assocLetter
  void parRight
  void eq2
  UDO name (fromInteger . numerator $ p) (toEnum (fromInteger $ numerator a)) <$> expr 0.0 m

assignStmt :: Maps -> Parser Expr
assignStmt m = do
  name <- identifier
  void eq
  Asgn (trimTick name) <$> expr 0.0 m

imprtStmt :: Parser Expr
imprtStmt = do
  void $ parseIf "import" (== TIdent "import")
  (n1, n2) <- number
  pure $ Imprt . either id id . numToText $ (n1 :+ n2)

keyValuePair :: Maps -> Parser (Text, Expr)
keyValuePair m = do
  i <- identifier
  void $ parseIf "=>" (== TOp "=>")
  e <- expr 0.0 m
  pure (i, e)

keyValuePairs :: Maps -> Parser [(Text, Expr)]
keyValuePairs = flip sepBy comma . keyValuePair

eid :: Parser Expr
eid = Id <$> identifier

enumber :: Parser Expr
enumber = uncurry Number <$> number <*> pure Unitless

expr :: Double -> Maps -> Parser Expr
expr min_bp m = Parser $
  \case
    (inputUncons -> Just (t, ts)) -> case t of
      TOp op -> case inputPeek ts of
        Just TLPar -> do
          (i, e) <- runParser (parens (sepBy (expr 0.0 m) comma)) ts
          inner_loop (Call (handleNegation op) e) i
        _ -> case prefix_binding_power op m of
          Just bp1 -> do
            (i, e) <- runParser (expr bp1 m) ts
            inner_loop (Call (handleNegation op) [e]) i
          Nothing -> Left $ "No such operator: " <> op
      TNumber a b -> case inputPeek ts of
        Just (TUnit u) -> inner_loop (Number a b u) (inputTail ts)
        _ -> inner_loop (Number a b Unitless) ts
      TIdent a -> case inputPeek ts of
        Just TLPar | not ("'" `T.isSuffixOf` a) -> do
          (i, e) <- runParser (parens (sepBy (expr 0.0 m) comma)) ts
          inner_loop (Call a e) i
        Just TLBracket -> do
          (i, e) <- runParser (brackets (sepBy identifier comma)) ts
          inner_loop (ChairSit a e) i
        Just x | isSpaceFun x -> do
          (i, e) <- runParser (many (eid <|> enumber <|> parens (expr 0.0 m))) ts
          inner_loop (Call (trimTick a) e) i
        _ -> inner_loop (Id a) ts
      TLPar -> do
        (i, e) <- runParser (expr 0.0 m) ts
        case i of
          (inputUncons -> Just (TRPar, i1)) -> inner_loop (Par e) i1
          (inputUncons -> Just (TComma, _)) -> Left "Argument list without a function name"
          _ -> Left "No closing parenthesis"
      TLBrace -> do
        (i, e) <- runParser (keyValuePairs m) ts
        case i of
          (inputUncons -> Just (TRBrace, i1)) -> inner_loop (ChairLit e) i1
          _ -> Left "No closing brace"
      TRBrace -> Left "No opening brace"
      TRBracket -> Left "No opening bracket"
      TRPar -> Left "No opening parenthesis"
      tok -> Left ("Only numbers in the building " <> showT tok)
    _ -> Left "Expected token, but the list is empty"
 where
  inner_loop :: Expr -> Input -> Either Text (Input, Expr)
  inner_loop lhs ts = case ts of
    (inputUncons -> Just (t, ts1)) -> case t of
      TRPar -> rtl
      TRBrace -> rtl
      TRBracket -> rtl
      TComma -> rtl
      TOp op -> case postfix_binding_power op m of
        Just bp ->
          if bp < min_bp
            then rtl
            else case inputPeek ts1 of
              Just (TOp op1) -> inner_loop (Call (handleNegation op) [lhs]) ts1
              Nothing -> Right (ts1, Call (handleNegation op) [lhs])
              _ -> do
                (ts2, e) <- runParser (expr bp m) ts1
                inner_loop (Call op [lhs, e]) ts2
        Nothing -> case infix_binding_power op m of
          Nothing -> Left $ "Operator does not exist: " <> showT op
          Just (l_bp, r_bp) ->
            if l_bp < min_bp
              then rtl
              else do
                (ts2, e) <- runParser (expr r_bp m) ts1
                inner_loop (Call op [lhs, e]) ts2
      TLPar -> Left "Calling a ticked function as a normal one"
      tok -> Left $ "Wrong token: " <> showT tok
    _ -> rtl
   where
    rtl = Right (ts, lhs)
  infix_binding_power :: Text -> Maps -> Maybe (Double, Double)
  infix_binding_power op ms =
    if T.all (`elem` opSymbols) op
      then do
        (Op pr asoc _) <- ms ^. (opmap . at (op, Ar2))
        let p = fromIntegral pr
        pure $ case asoc of
          L -> (p, p + 0.25)
          R -> (p + 0.25, p)
          N -> (p - 0.25, p + 0.25)
      else let mp = fromIntegral (maxPrecedence + 1) in pure (mp, mp + 0.25)
  prefix_binding_power :: Text -> Maps -> Maybe Double
  prefix_binding_power op ms = do
    (Op pr _ _) <- ms ^. (opmap . at (op, Ar1))
    pure $ fromIntegral pr
  postfix_binding_power :: Text -> Maps -> Maybe Double
  postfix_binding_power op ms = do
    (Op pr _ _) <- ms ^. (opmap . at (op, Ar1))
    pure $ fromIntegral pr
  handleNegation :: Text -> Text
  handleNegation "-" = "neg"
  handleNegation op = op

testParser :: Parser a -> Text -> Either Text (Input, a)
testParser = flip ((>>=) . tloop) . (. Input 0) . runParser
