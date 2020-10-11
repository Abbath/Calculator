{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Calculator.HomebrewLexer (tloop) where

import           Control.Applicative
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Calculator.Types    (Token (..))

data Input = Input
  { inputLoc :: Int
  , inputStr :: Text
  } deriving (Show, Eq)

inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ t) | T.null t = Nothing
inputUncons (Input loc t )         = T.uncons t >>= \(x, xs) -> Just (x, Input (loc + 1) xs)

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

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
        Left $
        ParserError
          (inputLoc input)
          ("Expected '" <> T.singleton x <> "', but found '" <>  T.singleton  y <> "'")
    f input =
      Left $
      ParserError
        (inputLoc input)
        ("Expected '" <> T.singleton x <> "', but reached end of string")

ws :: Parser Text
ws = spanP "whitespace character" isSpace
-- textP :: Text -> Parser Text
-- textP str = Parser f
--     where
--         f input@(inputStr -> input_txt)
--             | T.null input_txt = 
--                 Left $
--                 ParserError
--                     (inputLoc input)
--                     ("Expected '" <> str <> "', but reached end of string")
--             | T.isPrefixOf str input_txt = 
--                 Right
--                 (Input (inputLoc input + T.length str) $ fromJust $ T.stripPrefix str input_txt, str)
--             | otherwise = 
--                 Left $
--                 ParserError
--                     (inputLoc input)
--                     ("Expected '" <> str <> "', but found '" <>  input_txt <> "'")

spanP :: Text -> (Char -> Bool) -> Parser Text
spanP desc = (\f g -> T.pack <$> f g) $ many . parseIf desc

parseIf :: Text -> (Char -> Bool) -> Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
          Left $
          ParserError
            (inputLoc input)
            ("Expected " <> desc <> ", but found '" <> T.singleton y <> "'")
      _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected " <> desc <> ", but reached end of string")

doubleLiteral :: Parser Double
doubleLiteral =
  (\sign int frac expo ->
       sign * (int + frac) * (10 ** expo))
    <$> (minus <|> pure 1)
    <*> (read <$> digits)
    <*> opt (read <$> (('0':) <$> ((:) <$> charP '.' <*> digits)))
    <*> opt (e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits)))
  where
    digits = some $ parseIf "digit" isDigit
    minus = (-1) <$ charP '-'
    plus = 1 <$ charP '+'
    e = charP 'e' <|> charP 'E'
    opt = (<|> pure 0)

wsBracket :: Parser a -> Parser a
wsBracket p = ws *> p <* ws

numba :: Parser Token
numba = TNumber . toRational <$> wsBracket doubleLiteral

lpar :: Parser Token
lpar = TLPar <$ wsBracket (charP '(')

rpar :: Parser Token
rpar = TRPar <$ wsBracket (charP ')')

alfa :: Parser Char
alfa = parseIf "letters and _" ((||) <$> isAlpha <*> (== '_'))

alfaNum :: Parser Char
alfaNum = alfa <|> parseIf "alfas and numbas" isDigit

ident :: Parser Token
ident = TIdent <$> wsBracket (T.cons <$> alfa <*> (T.pack <$> many alfaNum))

opSymbols :: String
opSymbols = "+-/*%^$!~&|=><"

opsym :: Parser Char
opsym = parseIf "operator" (`elem` opSymbols)

operata :: Parser Token
operata = TOp . T.pack <$> wsBracket (some opsym)

comma :: Parser Token
comma = TComma <$ wsBracket (charP ',')

-- equal :: Parser Token
-- equal = TEqual <$ charP '='

-- lett :: Parser Token
-- lett = TLet <$ textP "let"

-- funn :: Parser Token
-- funn = TLet <$ textP "fun"

tokah :: Parser Token
tokah = lpar <|> rpar <|> comma <|> operata <|> ident <|> numba

tloop :: Text -> Either Text [Token]
tloop = go [] . Input 0
    where
      go acc (Input _ x) | T.null x = Right $ reverse acc
      go acc input = case runParser tokah input of
        Left (ParserError n s) -> Left ("Error: " <> s <> " at " <> (T.pack . show $ n))
        Right (input1, tok) -> go (tok:acc) input1

