{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Calculator.Lexer (tloop) where

import Calculator.Types (Token (..), opSymbols, showT, textToNum)
import Control.Applicative (Alternative (..))
import Data.Char (
  isAlpha,
  isDigit,
  isHexDigit,
  isOctDigit,
  isSpace,
  ord,
 )
import Data.Scientific qualified as S
import Data.Text (Text)
import Data.Text qualified as T

data Input = Input
  { inputLoc :: Int
  , inputStr :: Text
  }
  deriving (Show, Eq)

inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ t) | T.null t = Nothing
inputUncons (Input loc t) = T.uncons t >>= \(x, xs) -> Just (x, Input (loc + 1) xs)

data ParserError = ParserError Int Text deriving (Show)

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (Input, a)
  }

instance (Semigroup a) => Semigroup (Parser a) where
  (Parser p1) <> (Parser p2) = Parser $ \input -> do
    (input', a1) <- p1 input
    (input'', a2) <- p2 input'
    pure (input'', a1 <> a2)

instance (Monoid a) => Monoid (Parser a) where
  mempty = Parser $ \input -> pure (input, mempty)

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
            ("Expected '" <> T.singleton x <> "', but found '" <> T.singleton y <> "'")
  f input =
    Left $
      ParserError
        (inputLoc input)
        ("Expected '" <> T.singleton x <> "', but reached end of string")

ws :: Parser Text
ws = spanP "whitespace character" isSpace

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

doubleLiteral :: Parser Rational
doubleLiteral =
  ( \sign int frac expo ->
      toRational . read @S.Scientific $ (sign : int ++ frac ++ expo)
  )
    <$> plusminus
    <*> digits
    <*> opt ((:) <$> charP '.' <*> digits)
    <*> opt ((:) <$> e <*> ((:) <$> plusminus <*> digits))
 where
  digits = (++) <$> some (parseIf "digit" isDigit) <*> (concat <$> many ud)
  ud = some (parseIf "underscore" (== '_')) *> some (parseIf "digit" isDigit)
  plusminus = charP '+' <|> charP '-' <|> pure '+'
  e = charP 'e' <|> charP 'E'
  opt = (<|> pure "")

basedLiteral :: (Char -> Bool) -> Char -> Parser Rational
basedLiteral f p = (\_ _ n -> toRational (n :: Integer)) <$> zero <*> x <*> (read . (['0', p] ++) <$> digits)
 where
  digits = (++) <$> some (parseIf "digit" f) <*> (concat <$> many ud)
  ud = some (parseIf "underscore" (== '_')) *> some (parseIf "digit" f)
  zero = charP '0'
  x = charP p

hexLiteral :: Parser Rational
hexLiteral = basedLiteral isHexDigit 'x'

octLiteral :: Parser Rational
octLiteral = basedLiteral isOctDigit 'o'

binLiteral :: Parser Rational
binLiteral =
  (\_ _ n -> toRational (n :: Integer))
    <$> charP '0'
    <*> charP 'b'
    <*> ( foldl
            ( \b c -> case c of
                '_' -> b
                n -> b * 2 + toInteger (ord n - ord '0')
            )
            0
            <$> binDigitOrUnderscore
        )
 where
  binDigitOrUnderscore = some $ parseIf "bin digit or underscore" (`elem` ("01_" :: String))

specialCharacters :: Parser Char -> Parser Char
specialCharacters p = f <$> p
 where
  f '"' = '"'
  f 'n' = '\n'
  f 't' = '\t'
  f _ = error "Unacceptable character"

stringLiteral :: Parser Rational
stringLiteral =
  charP '"'
    *> ( fromInteger . textToNum 0
          <$> many
            ( charP '\\'
                *> specialCharacters (parseIf "\"nt" (`elem` ("\"nt" :: String)))
                  <|> parseIf "anything except \"" (/= '"')
            )
       )
    <* charP '"'

wsBracket :: Parser a -> Parser a
wsBracket p = ws *> p <* ws

complexLiteral :: Parser (Rational, Rational)
complexLiteral = (,) <$> (doubleLiteral <* (charP 'j' <|> charP 'i')) <*> doubleLiteral

numba :: Parser Token
numba =
  uncurry TNumber
    <$> wsBracket
      ( (,0)
          <$> hexLiteral
            <|> (,0)
          <$> octLiteral
            <|> (,0)
          <$> binLiteral
            <|> complexLiteral
            <|> (,0)
          <$> doubleLiteral
            <|> (,0)
          <$> stringLiteral
      )

lpar :: Parser Token
lpar = TLPar <$ wsBracket (charP '(')

rpar :: Parser Token
rpar = TRPar <$ wsBracket (charP ')')

lbrace :: Parser Token
lbrace = TLBrace <$ wsBracket (charP '{')

rbrace :: Parser Token
rbrace = TRBrace <$ wsBracket (charP '}')

lbracket :: Parser Token
lbracket = TLBracket <$ wsBracket (charP '[')

rbracket :: Parser Token
rbracket = TRBracket <$ wsBracket (charP ']')

alfa :: Parser Char
alfa = parseIf "letters and _" ((||) <$> isAlpha <*> (== '_'))

alfaNum :: Parser Char
alfaNum = alfa <|> parseIf "alfas and numbas" isDigit

alfaNumDot :: Parser Char
alfaNumDot = alfaNum <|> parseIf "dot" (== '.')

ident :: Parser Token
ident = TIdent <$> wsBracket ((T.singleton <$> alfa) <> (T.pack <$> ((<>) <$> many alfaNumDot <*> ((: []) <$> charP '\'' <|> pure []))))

label :: Parser Token
label = TLabel <$> wsBracket (T.pack <$> many alfaNum <* parseIf ":" (== ':'))

opsym :: Parser Char
opsym = parseIf "operator" (`elem` opSymbols)

operata :: Parser Token
operata = TOp . T.pack <$> wsBracket (some opsym <|> charP '`' *> ((:) <$> alfa <*> many alfaNumDot) <* charP '`')

comma :: Parser Token
comma = TComma <$ wsBracket (charP ',')

dots :: Parser Token
dots = TDots <$ wsBracket (charP '.' <* charP '.' <* charP '.')

tokah :: Parser Token
tokah = lpar <|> rpar <|> lbrace <|> rbrace <|> lbracket <|> rbracket <|> comma <|> operata <|> numba <|> label <|> ident <|> dots

tloop :: Text -> Either Text [Token]
tloop = go [] . Input 0
 where
  go acc (Input _ x) | T.null x = Right $ reverse acc
  go acc input = case runParser tokah input of
    Left (ParserError n s) -> Left (s <> " at " <> showT n)
    Right (input1, tok) -> go (tok : acc) input1
