{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Calculator.Types
  ( Expr (..),
    Token (..),
    Assoc (..),
    Op (..),
    ExecOp (..),
    FunOp (..),
    getPrA,
    exprToString,
    unTOp,
    isOp,
    preprocess,
    ListTuple,
    Fun (..),
    ExecFn (..),
    FunFun (..),
    showRational,
    showT,
    opSymbols,
    Maps (..),
    OpMap,
    VarMap,
    FunMap,
    opsFromList,
    opsToList,
    funsFromList,
    funsToList,
    showComplex,
    showFraction,
    EvalState (..),
    maps,
    gen,
    mem,
    isNumber,
    isId,
    numToText,
    textToNum,
    extractFormat,
    zipFormat,
    isFormat,
    showComplexBase,
    varmap,
    opmap,
    funmap,
    chairmap,
    ChairVal (..),
    Chair,
    showChair
  )
where

import Control.Arrow (second)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Complex
import qualified Data.Scientific as S
import Data.Maybe (fromMaybe)
import Control.Lens.TH
import System.Random (StdGen)
import Data.Char (chr, ord)
import Data.Bits (shiftR, (.&.), shiftL)
import Numeric (showHex, showOct, showBin)

data Token = TNumber Rational Rational
           | TLPar
           | TRPar
           | TLBrace
           | TRBrace
           | TLBracket
           | TRBracket
           | TIdent Text
           | TOp Text
           | TComma
           | TEqual
           | TLabel Text
           deriving (Show, Eq)

data Assoc = L | R deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data Expr = Number Rational Rational
          | ChairLit
          | ChairSit Text Text
          | Imprt Text
          | Asgn Text Expr
          | UDF Text [Text] Expr
          | UDO Text Int Assoc Expr
          | Call Text [Expr]
          | Par Expr
          | Id Text
          | Seq [Expr]
          | Label Text
          deriving (Eq, Show, Read, Generic)

isNumber :: Expr -> Bool
isNumber (Number _ _) = True
isNumber _ = False

isId :: Expr -> Bool
isId (Id _) = True
isId _ = False

instance ToJSON Expr

instance FromJSON Expr

type ListTuple =  ([(Text, (Rational, Rational))], [((Text, Int), ([Text], Expr))], [(Text, ((Int, Assoc), Expr))])

data FunFun = CmpFn (Rational -> Rational -> Bool) |
              FracFn1 (Complex Rational -> Complex Rational) |
              MathFn1 (Complex Double -> Complex Double) |
              MathFn2 (Complex Rational -> Complex Rational -> Complex Rational) |
              IntFn1 (Double -> Integer) |
              IntFn2 (Integer -> Integer -> Integer) |
              BitFn (Integer -> Integer)

instance Show FunFun where
  show (CmpFn _) = "CmpFn"
  show (FracFn1 _) = "FracFn1"
  show (MathFn1 _) = "MathFn1"
  show (MathFn2 _) = "MathFn2"
  show (IntFn1 _) = "IntFn1"
  show (IntFn2 _) = "IntFn2"
  show (BitFn _) = "BitFn"

instance Show FunOp where
  show (CmpOp _) = "CmpOp"
  show (MathOp _) = "MathOp"
  show (BitOp _) = "BitOp"

data ExecFn = NFn | ExFn Expr | FnFn FunFun deriving Show

data ExecOp = NOp | ExOp Expr | FnOp FunOp | AOp Text deriving Show

data FunOp = CmpOp (Rational -> Rational -> Bool) | MathOp (Complex Rational -> Complex Rational -> Complex Rational) | BitOp (Integer -> Integer -> Integer)

data Fun = Fun {
  params :: [Text],
  fexec :: ExecFn
} deriving Show

data Op = Op {
  precedence :: Int,
  associativity :: Assoc,
  oexec :: ExecOp
} deriving Show

type Chair = Map Text ChairVal
data ChairVal = DickVal (Complex Rational) | PikeVal Chair deriving Show

type FunMap = Map (Text, Int) Fun
type VarMap = Map Text (Complex Rational)
type OpMap = Map Text Op
type ChairMap = Map Text Chair

data Maps = Maps { _varmap :: VarMap, _funmap :: FunMap, _opmap :: OpMap, _chairmap :: ChairMap } deriving Show

makeLenses ''Maps

isExOp :: ExecOp -> Bool
isExOp (ExOp _) = True
isExOp _ = False

unpackExOp :: ExecOp -> Expr
unpackExOp (ExOp e) = e
unpackExOp _ = error "Not an expression"

opsToList :: OpMap -> [(Text, ((Int, Assoc), Expr))]
opsToList = map (\(k, v) -> (k, ((precedence v, associativity v), unpackExOp . oexec $ v))) . filter (\(_, v) -> isExOp . oexec $ v) . M.toList

opsFromList :: [(Text, ((Int, Assoc), Expr))] -> OpMap
opsFromList = M.fromList . map (\(k, ((p, a), e)) -> (k, Op p a (ExOp e)))

getPrA :: OpMap -> Map Text (Int, Assoc)
getPrA om = let lst = M.toList om
                ps = M.fromList $ map (second (\o -> (precedence o, associativity o))) lst
            in ps

isExFn :: ExecFn -> Bool
isExFn (ExFn _) = True
isExFn _ = False

unpackExFn :: ExecFn -> Expr
unpackExFn (ExFn e) = e
unpackExFn _ = error "Not an expression"

funsToList :: FunMap -> [((Text, Int), ([Text], Expr))]
funsToList = map (\(k, v) -> (k, (params v, unpackExFn . fexec $ v))) . filter (\(_, v) -> isExFn . fexec $ v) . M.toList

funsFromList :: [((Text, Int), ([Text], Expr))] -> FunMap
funsFromList = M.fromList . map (\(k, (p, e)) -> (k, Fun p (ExFn e)))

-- instance Show Expr where
--   show = showExpr 0

-- showExpr :: Int -> Expr -> String
-- showExpr n ex =
--   let suf = case ex of
--         UDF name a e    -> name ++ "("++ intercalate ", " a ++ ")" ++ "\n" ++ s e
--         UDO name p a e  -> name ++ "("++ show p ++ ", " ++ show a ++ ")" ++ "\n" ++ s e
--         Asgn i e        -> "Assign " ++ i ++ "\n" ++ s e
--         Number x        -> "Number " ++ show x
--         Par e           -> "Par \n" ++ s e
--         UMinus e        -> "UMinus \n" ++ s e
--         OpCall op e1 e2 -> "OpCall " ++ op ++ "\n" ++ s e1 ++ "\n" ++ s e2
--         FunCall name e  -> "FunCall " ++ name ++ "\n" ++ intercalate "\n" (map s e)
--         Id name         -> "Id " ++ name
--   in replicate n ' ' ++ suf
--   where s = showExpr (n+1)

showT :: Show a => a -> Text
showT = T.pack . show

exprToString :: Expr -> Text
exprToString ex = case ex of
  UDF n a e -> n <> "(" <> T.intercalate ", " a <> ")" <> " = " <> exprToString e
  UDO n p a e -> n <> "(" <> showT p <> ", " <> showT (if a == L then 0 :: Double else 1) <> ")" <> " = " <> exprToString e
  Asgn i e -> i <> " = " <> exprToString e
  Number x _ -> showT . (fromRational :: Rational -> Double) $ x
  Par e -> "(" <> exprToString e <> ")"
  Call op [e] -> "(" <> op <> exprToString e <> ")"
  Call op [e1, e2] | isOp op -> "(" <> exprToString e1 <> op <> exprToString e2 <> ")"
  Call n e -> n <> "(" <> T.intercalate ", " (map exprToString e) <> ")"
  Id s -> s
  Seq es -> T.intercalate "\n" (map exprToString es)
  Imprt t -> "import " <> t
  Label l -> l <> ":"
  ChairLit -> "{}"
  ChairSit a x -> a <> "[" <> x <> "]"

opSymbols :: String
opSymbols = "+-/*%^$!~&|=><:"

isOp :: Text -> Bool
isOp = T.all (`elem` opSymbols)

unTOp :: Token -> Text
unTOp (TOp op) = op
unTOp _        = error "Not a TOp"

preprocess :: Expr -> Expr
preprocess ex = simplify' ex ex
  where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr :: Expr -> Expr
simplifyExpr ex = case ex of
  Asgn s e -> Asgn s (simplifyExpr e)
  UDF n s e -> UDF n s (simplifyExpr e)
  UDO n p a e -> UDO n p a (simplifyExpr e)
  Par (Par e) -> Par (simplifyExpr e)
  Par e -> Par (simplifyExpr e)
  Call "+" [Number 0.0 0.0, n] -> simplifyExpr n
  Call op [n, Number 0.0 0.0] | op `elem` ["+", "-"] -> simplifyExpr n
  Call "*" [Number 1.0 0.0, n] -> simplifyExpr n
  Call "*" [Number 0.0 0.0, n] -> Number 0.0 0.0
  Call op [n, Number 1.0 0.0] | op `elem` ["*", "/", "%"] -> simplifyExpr n
  Call "^" [n, Number 1.0 0.0] -> simplifyExpr n
  Call "^" [Call "sqrt" [e], Number 2.0 0.0] -> simplifyExpr e
  Call "exp" [Call "log" [e]] -> simplifyExpr e
  Call "log" [Call "exp" [e]] -> simplifyExpr e
  Call "sqrt" [Call "^" [e, Number 2.0 0.0]] -> simplifyExpr e
  Call name e -> Call name (map simplifyExpr e)
  x -> x

showScientific :: S.Scientific -> Text
showScientific = T.pack . S.formatScientific S.Fixed Nothing

showRational :: Rational -> Text
showRational r = if denominator r == 1
  then showT $ numerator r
  else case S.fromRationalRepetend (Just 1024) r of
    Left (s, rest) -> showT s
    Right (s, Nothing) -> showT s
    Right (s, Just n) -> let st = showScientific s
                             idx = (+(n+1)) . fromMaybe 0 . T.findIndex (=='.') $ st
                         in T.take idx st <> "(" <> T.drop idx st <> ")"

showFraction :: Rational -> Text
showFraction t = showT (numerator t) <> " / " <> showT (denominator t)

showComplexBase :: Int -> Complex Rational -> Either Text Text
showComplexBase base cr | base `elem` [2, 8, 16] = if (denominator . realPart $ cr) == 1 && (denominator . imagPart $ cr) == 1
  then
    let function = case base of
          2 -> showBin
          8 -> showOct
          16 -> showHex
          _ -> error "Unreachable"
    in Right (T.pack . (`function` ""). numerator . realPart $ cr)
  else Left "Can't show fractions yet"
showComplexBase _ cr = Right $ showComplex cr

showComplex :: Complex Rational -> Text
showComplex c =
  let cr = realPart c
      ci = imagPart c
  in showRational cr <> if ci /= 0 then "j" <> showRational ci else ""

showChair :: Chair -> Text
showChair ch = "{" <> T.intercalate ", " (map (\(k, v) -> k <> " => " <> showElem v) . M.toList $ ch) <> "}"
  where
    showElem (DickVal v) = showComplex v
    showElem (PikeVal v) = showChair v

numToText :: Complex Rational -> Either Text Text
numToText n | denominator (realPart n) /= 1 = Left "Can't convert rational to string!"
numToText n = Right $ go T.empty (abs . numerator . realPart $ n)
  where go t 0 = t
        go t m = go (T.cons (chr . fromInteger $ (m .&. 0xff)) t) (m `shiftR` 8)

textToNum :: Integer -> [Char] -> Integer
textToNum n [] = n
textToNum n (c:cs) =
  let o = ord c
      b = if o > 255 || o < 0 then ord ' ' else o
  in textToNum (n `shiftL` 8 + toInteger b) cs

data FormatChunk = FormatTxt Text | FormatFmt Text deriving Show

isFormat :: FormatChunk -> Bool
isFormat (FormatFmt _) = True
isFormat _ = False

extractFormat :: Text -> Either Text [FormatChunk]
extractFormat = go T.empty []
  where
    go chunk acc t | T.null t = Right . reverse $ if T.null chunk then acc else FormatTxt chunk:acc
    go chunk acc t =
      let (c, cs) = fromMaybe ('a', "") $ T.uncons t
      in case c of
           '%' -> let (c1, cs1) = fromMaybe ('%', "") $ T.uncons cs
                  in case c1 of
                       '%' -> go (T.snoc chunk '%') acc cs1
                       _ | c1 `T.elem` "sfrhbo" -> go T.empty (FormatFmt (T.singleton c1):FormatTxt chunk:acc) cs1
                       _ -> Left "Wrong format string!"
           _ -> go (T.snoc chunk c) acc cs

zipFormat :: [FormatChunk] -> [Complex Rational] -> Either Text Text
zipFormat = go T.empty
  where
    go acc [] [] = Right acc
    go _ [] (_:_) = Left "Too many things to format"
    go _ (FormatFmt _:_) [] = Left "Too few things to format"
    go acc (FormatTxt t:fs) rs = go (acc <> t) fs rs
    go acc (FormatFmt t:fs) (r:rs)
      | t == "s" = do
        txt <- numToText r
        go (acc <> txt) fs rs
      | t == "f" = go (acc <> showComplex r) fs rs
      | t == "h" = showBase 16 r
      | t == "b" = showBase 2 r
      | t == "o" = showBase 8 r
      | t == "r" = go (acc <> showFraction (realPart r)) fs rs
      | otherwise = Left $ "Wrong format: " <> t
      where
        showBase b cr = do
          formatted <- showComplexBase b cr
          go (acc <> formatted) fs rs

data EvalState = EvalState {_maps :: Maps, _gen :: StdGen, _mem :: Map (Text, Int) (Map (Complex Rational) (Complex Rational))} deriving (Show)

makeLenses ''EvalState