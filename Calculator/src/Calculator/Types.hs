{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calculator.Types (
  Expr (..),
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
  showChair,
  Arity (..),
  ar2int,
  isSpaceFun,
  Precise,
  prec,
  OpArity (..),
  renderToken,
  renderTokens,
)
where

import Control.Arrow (second)
import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits as B (bit, shiftL, shiftR, unsafeShiftL, (.&.))
import Data.CReal (CReal)
import Data.CReal.Internal as CRI (atPrecision, atanBounded, crMemoize, crealPrecision, recipBounded, shiftL)
import Data.Char (chr, ord)
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Scientific qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Real (Ratio (..))
import Numeric (showBin, showHex, showOct)
import System.Random (Random, StdGen)

newtype Precise = Precise {unreal :: CReal 256}
  deriving (Show, Eq, Ord)
  deriving (Num, Fractional, Floating, Real, RealFrac, Random) via CReal 256

piBy4 :: CReal n
piBy4 = atanBounded (recipBounded 5) `CRI.shiftL` 2 - atanBounded (recipBounded 239) -- Machin Formula

piBy2 :: CReal n
piBy2 = piBy4 `CRI.shiftL` 1

instance RealFloat Precise where
  floatRadix :: Precise -> Integer
  floatRadix _ = 2
  floatDigits :: Precise -> Int
  floatDigits _ = error "Data.CReal.Internal floatDigits"
  floatRange :: Precise -> (Int, Int)
  floatRange _ = error "Data.CReal.Internal floatRange"
  decodeFloat :: Precise -> (Integer, Int)
  decodeFloat x =
    let p = crealPrecision x.unreal
     in (x.unreal `atPrecision` p, -p)
  encodeFloat :: Integer -> Int -> Precise
  encodeFloat m n =
    if n <= 0
      then fromRational (m % bit (negate n))
      else fromRational (unsafeShiftL m n :% 1)
  exponent :: Precise -> Int
  exponent _ = 0
  significand :: Precise -> Precise
  significand = error "Data.CReal.Internal significand"
  scaleFloat :: Int -> Precise -> Precise
  scaleFloat n x = Precise $ x.unreal `CRI.shiftL` n
  isNaN :: Precise -> Bool
  isNaN _ = False
  isInfinite :: Precise -> Bool
  isInfinite _ = False
  isDenormalized :: Precise -> Bool
  isDenormalized _ = False
  isNegativeZero :: Precise -> Bool
  isNegativeZero _ = False
  isIEEE :: Precise -> Bool
  isIEEE _ = False
  atan2 :: Precise -> Precise -> Precise
  atan2 y x =
    Precise $
      crMemoize
        ( \p ->
            let y' = y.unreal `atPrecision` p
                x' = x.unreal `atPrecision` p
                θ =
                  if
                    | x' > 0 -> atan (y.unreal / x.unreal)
                    | x' == 0 && y' > 0 -> piBy2
                    | x' < 0 && y' > 0 -> pi + atan (y.unreal / x.unreal)
                    | x' <= 0 && y' < 0 -> -atan2 (-y.unreal) x.unreal
                    | y' == 0 && x' < 0 -> pi -- must be after the previous test on zero y
                    | x' == 0 && y' == 0 -> 0 -- must be after the other double zero tests
                    | otherwise -> error "Data.CReal.Internal atan2"
             in θ `atPrecision` p
        )

showT :: (Show a) => a -> Text
showT = T.pack . show

showScientific :: S.Scientific -> Text
showScientific = T.pack . S.formatScientific S.Fixed Nothing

showRational :: Rational -> Text
showRational r =
  if denominator r == 1
    then showT $ numerator r
    else case S.fromRationalRepetend (Just 1024) r of
      Left (s, rest) -> showT s
      Right (s, Nothing) -> showT s
      Right (s, Just n) ->
        let st = showScientific s
            idx = (+ (n + 1)) . fromMaybe 0 . T.findIndex (== '.') $ st
         in T.take idx st <> "(" <> T.drop idx st <> ")"

showComplex :: Complex Rational -> Text
showComplex c =
  let cr = realPart c
      ci = imagPart c
   in showRational cr <> if ci /= 0 then "j" <> showRational ci else ""

data Token
  = TNumber Rational Rational
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
  | TDots
  deriving (Show, Eq)

renderToken :: Token -> Text
renderToken (TNumber n d) = showComplex $ n :+ d
renderToken TLPar = "("
renderToken TRPar = ")"
renderToken TLBrace = "{"
renderToken TRBrace = "}"
renderToken TLBracket = "["
renderToken TRBracket = "]"
renderToken (TIdent i) = i
renderToken (TOp o) = o
renderToken TComma = ","
renderToken TEqual = "="
renderToken (TLabel l) = l
renderToken TDots = "..."

renderTokens :: [Token] -> Text
renderTokens = T.concat . map renderToken

isSpaceFun :: Token -> Bool
isSpaceFun (TIdent _) = True
isSpaceFun (TNumber _ _) = True
isSpaceFun TLPar = True
isSpaceFun _ = False

data Assoc = L | R | N deriving (Show, Read, Eq, Ord, Enum, Generic, ToJSON, FromJSON)

data Expr
  = Number Rational Rational
  | ChairLit [(Text, Expr)]
  | ChairSit Text [Text]
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

type ListTuple = ([(Text, (Rational, Rational))], [((Text, Arity), ([Text], Expr))], [((Text, OpArity), ((Int, Assoc), Expr))])

data FunFun
  = CmpFn (Rational -> Rational -> Bool)
  | FracFn1 (Complex Rational -> Complex Rational)
  | MathFn1 (Complex Precise -> Complex Precise)
  | MathFn2 (Complex Rational -> Complex Rational -> Complex Rational)
  | IntFn1 (Precise -> Integer)
  | IntFn2 (Integer -> Integer -> Integer)
  | BitFn (Integer -> Integer)

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
  show (UnOp _) = "UnOp"

data ExecFn = NFn | ExFn Expr | FnFn FunFun deriving (Show)

data ExecOp = NOp | ExOp Expr | FnOp FunOp | AOp Text deriving (Show)

data FunOp = CmpOp (Rational -> Rational -> Bool) | MathOp (Complex Rational -> Complex Rational -> Complex Rational) | BitOp (Integer -> Integer -> Integer) | UnOp (Integer -> Integer)

data Fun = Fun
  { params :: [Text]
  , fexec :: ExecFn
  }
  deriving (Show)

data Op = Op
  { precedence :: Int
  , associativity :: Assoc
  , oexec :: ExecOp
  }
  deriving (Show)

type Chair = Map Text ChairVal
data ChairVal = DickVal (Complex Rational) | PikeVal Chair deriving (Show)

data Arity = ArFixed Int | ArVar Int deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)
data OpArity = Ar1 | Ar2 deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

ar2int :: Arity -> Int
ar2int (ArFixed n) = n
ar2int (ArVar n) = n

type FunMap = Map (Text, Arity) Fun
type VarMap = Map Text (Complex Rational)
type OpMap = Map (Text, OpArity) Op
type ChairMap = Map Text Chair

data Maps = Maps {_varmap :: VarMap, _funmap :: FunMap, _opmap :: OpMap, _chairmap :: ChairMap} deriving (Show)

makeLenses ''Maps

isExOp :: ExecOp -> Bool
isExOp (ExOp _) = True
isExOp _ = False

unpackExOp :: ExecOp -> Expr
unpackExOp (ExOp e) = e
unpackExOp _ = error "Not an expression"

opsToList :: OpMap -> [((Text, OpArity), ((Int, Assoc), Expr))]
opsToList = map (\(k, v) -> (k, ((precedence v, associativity v), unpackExOp . oexec $ v))) . filter (\(_, v) -> isExOp . oexec $ v) . M.toList

opsFromList :: [((Text, OpArity), ((Int, Assoc), Expr))] -> OpMap
opsFromList = M.fromList . map (\(k, ((p, a), e)) -> (k, Op p a (ExOp e)))

getPrA :: OpMap -> Map (Text, OpArity) (Int, Assoc)
getPrA om =
  let lst = M.toList om
      ps = M.fromList $ map (second (\o -> (precedence o, associativity o))) lst
   in ps

isExFn :: ExecFn -> Bool
isExFn (ExFn _) = True
isExFn _ = False

unpackExFn :: ExecFn -> Expr
unpackExFn (ExFn e) = e
unpackExFn _ = error "Not an expression"

funsToList :: FunMap -> [((Text, Arity), ([Text], Expr))]
funsToList = map (\(k, v) -> (k, (params v, unpackExFn . fexec $ v))) . filter (\(_, v) -> isExFn . fexec $ v) . M.toList

funsFromList :: [((Text, Arity), ([Text], Expr))] -> FunMap
funsFromList = M.fromList . map (\(k, (p, e)) -> (k, Fun p (ExFn e)))

exprToString :: Expr -> Text
exprToString ex = case ex of
  UDF n a e -> n <> "(" <> T.intercalate ", " a <> ")" <> " = " <> exprToString e
  UDO n p a e -> n <> "(" <> showT p <> ", " <> showT (if a == L then 0 :: Precise else 1) <> ")" <> " = " <> exprToString e
  Asgn i e -> i <> " = " <> exprToString e
  Number x _ -> showT . (fromRational :: Rational -> Precise) $ x
  Par e -> "(" <> exprToString e <> ")"
  Call op [e] | isOp op -> "(" <> op <> exprToString e <> ")"
  Call op [e1, e2] | isOp op -> "(" <> exprToString e1 <> op <> exprToString e2 <> ")"
  Call n e -> n <> "(" <> T.intercalate ", " (map exprToString e) <> ")"
  Id s -> s
  Seq es -> T.intercalate "\n" (map exprToString es)
  Imprt t -> "import " <> t
  Label l -> l <> ":"
  ChairLit _ -> "{}"
  ChairSit a x -> a <> "[" <> T.intercalate "," x <> "]"

opSymbols :: String
opSymbols = "+-/*%^$!~&|=><:?"

isOp :: Text -> Bool
isOp = T.all (`elem` opSymbols)

unTOp :: Token -> Text
unTOp (TOp op) = op
unTOp _ = error "Not a TOp"

preprocess :: Expr -> Expr
preprocess ex = simplify' ex ex
 where
  simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr :: Expr -> Expr
simplifyExpr ex = case ex of
  Asgn s e -> Asgn s (simplifyExpr e)
  UDF n s e -> UDF n s (simplifyExpr e)
  UDO n p a e -> UDO n p a (simplifyExpr e)
  Par (Par e) -> Par (simplifyExpr e)
  Par e -> Par (simplifyExpr e)
  Call "+" [Number 0.0 0.0, n] -> simplifyExpr n
  Call "+" [n, Number 0.0 0.0] -> simplifyExpr n
  Call op [n, Number 0.0 0.0] | op `elem` ["+", "-"] -> simplifyExpr n
  Call "*" [Number 1.0 0.0, n] -> simplifyExpr n
  Call "*" [Number 0.0 0.0, n] -> Number 0.0 0.0
  Call "*" [n, Number 1.0 0.0] -> simplifyExpr n
  Call "*" [n, Number 0.0 0.0] -> Number 0.0 0.0
  Call op [n, Number 1.0 0.0] | op `elem` ["*", "/", "%"] -> simplifyExpr n
  Call "^" [n, Number 1.0 0.0] -> simplifyExpr n
  Call "^" [Call "sqrt" [e], Number 2.0 0.0] -> simplifyExpr e
  Call "exp" [Call "log" [e]] -> simplifyExpr e
  Call "log" [Call "exp" [e]] -> simplifyExpr e
  Call "sqrt" [Call "^" [e, Number 2.0 0.0]] -> simplifyExpr e
  Call name e -> Call name (map simplifyExpr e)
  x -> x

showFraction :: Rational -> Text
showFraction t = showT (numerator t) <> " / " <> showT (denominator t)

showComplexBase :: Int -> Complex Rational -> Either Text Text
showComplexBase base cr
  | base `elem` [2, 8, 16] =
      if (denominator . realPart $ cr) == 1 && (denominator . imagPart $ cr) == 1
        then
          let function = case base of
                2 -> showBin
                8 -> showOct
                16 -> showHex
                _ -> error "Unreachable"
           in Right (T.pack . (`function` "") . numerator . realPart $ cr)
        else Left "Can't show fractions yet"
showComplexBase _ cr = Right $ showComplex cr

showChair :: Chair -> Text
showChair ch = "{" <> T.intercalate ", " (map (\(k, v) -> k <> " => " <> showElem v) . M.toList $ ch) <> "}"
 where
  showElem (DickVal v) = showComplex v
  showElem (PikeVal v) = showChair v

numToText :: Complex Rational -> Either Text Text
numToText n | denominator (realPart n) /= 1 = Left "Can't convert rational to string!"
numToText n = Right $ go T.empty (abs . numerator . realPart $ n)
 where
  go t 0 = t
  go t m = go (T.singleton (chr . fromInteger $ (m .&. 0xff)) <> t) (m `B.shiftR` 8)

textToNum :: Integer -> [Char] -> Integer
textToNum n [] = n
textToNum n (c : cs) =
  let o = ord c
      b = if o > 255 || o < 0 then ord ' ' else o
   in textToNum (n `B.shiftL` 8 + toInteger b) cs

data FormatChunk = FormatTxt Text | FormatFmt Text deriving (Show)

isFormat :: FormatChunk -> Bool
isFormat (FormatFmt _) = True
isFormat _ = False

extractFormat :: Text -> Either Text [FormatChunk]
extractFormat = go T.empty []
 where
  go chunk acc t | T.null t = Right . reverse $ if T.null chunk then acc else FormatTxt chunk : acc
  go chunk acc t =
    let (c, cs) = fromMaybe ('a', "") $ T.uncons t
     in case c of
          '%' ->
            let (c1, cs1) = fromMaybe ('%', "") $ T.uncons cs
             in case c1 of
                  '%' -> go (T.snoc chunk '%') acc cs1
                  _ | c1 `T.elem` "sfrhbo" -> go T.empty (FormatFmt (T.singleton c1) : FormatTxt chunk : acc) cs1
                  _ -> Left "Wrong format string!"
          _ -> go (T.snoc chunk c) acc cs

zipFormat :: [FormatChunk] -> [Complex Rational] -> Either Text Text
zipFormat = go T.empty
 where
  go acc [] [] = Right acc
  go _ [] (_ : _) = Left "Too many things to format"
  go _ (FormatFmt _ : _) [] = Left "Too few things to format"
  go acc (FormatTxt t : fs) rs = go (acc <> t) fs rs
  go acc (FormatFmt t : fs) (r : rs)
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

data EvalState = EvalState {_maps :: Maps, _gen :: StdGen, _prec :: Int} deriving (Show)

makeLenses ''EvalState