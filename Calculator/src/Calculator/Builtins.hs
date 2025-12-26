{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Calculator.Builtins where

import Calculator.Types (Arity (..), Assoc (..), EvalState (EvalState), ExecFn (..), ExecOp (..), Expr (..), Fun (..), FunFun (..), FunMap, FunOp (..), Maps (..), Op (..), OpArity (..), OpMap, Precise, Unit (..), VarMap, defaultFun, unitlessNumber, unitlessOne, unitlessValue, unitlessZero)
import Calculator.Utils (splitRational)
import Control.Applicative ((<|>))
import Control.Arrow (second)
import Data.Bits (Bits (complement), complement, popCount, shift, xor, (.&.), (.|.))
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ratio (numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Math.NumberTheory.Primes.Testing
import Numeric (expm1, log1mexp, log1p, log1pexp)
import System.Random (mkStdGen)

operators :: OpMap
operators =
  [ (("=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("<-", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("+=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("-=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("*=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("/=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("%=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("^=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("|=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , (("&=", Ar2), Op{precedence = 0, associativity = R, oexec = NOp})
  , ((":", Ar2), Op{precedence = 1, associativity = L, oexec = NOp})
  , ((":=", Ar2), Op{precedence = 2, associativity = R, oexec = NOp})
  , (("|", Ar2), Op{precedence = 3, associativity = L, oexec = FnOp (BitOp (.|.))})
  , (("&", Ar2), Op{precedence = 4, associativity = L, oexec = FnOp (BitOp (.&.))})
  , (("==", Ar2), Op{precedence = 5, associativity = L, oexec = FnOp (CmpOp (==))})
  , (("<=", Ar2), Op{precedence = 5, associativity = L, oexec = FnOp (CmpOp (<=))})
  , ((">=", Ar2), Op{precedence = 5, associativity = L, oexec = FnOp (CmpOp (>=))})
  , (("!=", Ar2), Op{precedence = 5, associativity = L, oexec = FnOp (CmpOp (/=))})
  , (("<", Ar2), Op{precedence = 5, associativity = L, oexec = FnOp (CmpOp (<))})
  , ((">", Ar2), Op{precedence = 5, associativity = L, oexec = FnOp (CmpOp (>))})
  , (("<<", Ar2), Op{precedence = 6, associativity = L, oexec = FnOp (BitOp \n s -> shift n (fromInteger s))})
  , ((">>", Ar2), Op{precedence = 6, associativity = L, oexec = FnOp (BitOp \n s -> shift n ((-1) * fromInteger s))})
  , (("+", Ar2), Op{precedence = 7, associativity = L, oexec = FnOp (MathOp $ fmath (+))})
  , (("-", Ar2), Op{precedence = 7, associativity = L, oexec = FnOp (MathOp $ fmath (-))})
  , (("*", Ar2), Op{precedence = 8, associativity = L, oexec = FnOp (MathOp mult)})
  , (("/", Ar2), Op{precedence = 8, associativity = L, oexec = FnOp (MathOp divide)})
  , (("%", Ar2), Op{precedence = 8, associativity = L, oexec = FnOp (MathOp fmod)})
  , (("^", Ar2), Op{precedence = 10, associativity = R, oexec = FnOp (MathOp pow)})
  , (("<=>", Ar2), Op{precedence = 11, associativity = L, oexec = FnOp (MathOp fcmp)})
  , (("|>", Ar2), Op{precedence = 12, associativity = L, oexec = NOp})
  , (("::=", Ar2), Op{precedence = 13, associativity = R, oexec = NOp})
  ]

opMember :: Text -> Bool
opMember op = M.member (op, Ar2) operators || M.member (op, Ar1) unaryOperators

opLookup :: Text -> Maybe Op
opLookup op = M.lookup (op, Ar2) operators <|> M.lookup (op, Ar1) unaryOperators

unaryOperators :: OpMap
unaryOperators =
  [ (("?", Ar1), Op{precedence = maxPrecedence + 1, associativity = N, oexec = NOp})
  , (("!", Ar1), Op{precedence = 9, associativity = N, oexec = FnOp (UnOp prod)})
  , (("~", Ar1), Op{precedence = 9, associativity = N, oexec = FnOp (UnOp complement)})
  , (("-", Ar1), Op{precedence = 9, associativity = N, oexec = NOp})
  ]

maxPrecedence :: Int
maxPrecedence = precedence $ maximumBy (compare `on` precedence) (M.elems operators)

linearOperators :: V.Vector ((Text, OpArity), Op)
linearOperators = V.fromList $ M.assocs operators

functions :: FunMap
functions =
  [ (("prat", ArFixed 1), defaultFun)
  , (("str", ArFixed 1), defaultFun)
  , (("smp", ArFixed 1), defaultFun)
  , (("fmt", ArVar 1), defaultFun)
  , (("quit", ArFixed 0), defaultFun)
  , (("id", ArFixed 1), defaultFun)
  , (("if", ArFixed 3), defaultFun)
  , (("loop", ArFixed 2), defaultFun)
  , (("df", ArFixed 2), defaultFun)
  , (("int", ArFixed 4), defaultFun)
  , (("log", ArFixed 2), defaultFun)
  , (("hex", ArFixed 1), defaultFun)
  , (("oct", ArFixed 1), defaultFun)
  , (("bin", ArFixed 1), defaultFun)
  , (("undef", ArFixed 1), defaultFun)
  , (("opt", ArFixed 1), defaultFun)
  , (("opt", ArFixed 2), defaultFun)
  , (("neg", ArFixed 1), defaultFun)
  , (("lt", ArFixed 2), defaultFun{fexec = FnFn (CmpFn (<))})
  , (("gt", ArFixed 2), defaultFun{fexec = FnFn (CmpFn (>))})
  , (("eq", ArFixed 2), defaultFun{fexec = FnFn (CmpFn (==))})
  , (("ne", ArFixed 2), defaultFun{fexec = FnFn (CmpFn (/=))})
  , (("le", ArFixed 2), defaultFun{fexec = FnFn (CmpFn (<=))})
  , (("ge", ArFixed 2), defaultFun{fexec = FnFn (CmpFn (>=))})
  , (("sin", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 sin)})
  , (("cos", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 cos)})
  , (("asin", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 asin)})
  , (("acos", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 acos)})
  , (("tan", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 tan)})
  , (("atan", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 atan)})
  , (("log", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 log)})
  , (("exp", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 exp)})
  , (("log1p", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 log1p)})
  , (("expm1", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 expm1)})
  , (("log1pexp", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 log1pexp)})
  , (("log1mexp", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 log1mexp)})
  , (("sqrt", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 (** 0.5))})
  , (("abs", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 abs)})
  , (("sinh", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 sinh)})
  , (("cosh", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 cosh)})
  , (("tanh", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 tanh)})
  , (("asinh", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 asinh)})
  , (("acosh", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 acosh)})
  , (("atanh", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 atanh)})
  , (("sign", ArFixed 1), defaultFun{fexec = FnFn (MathFn1 signum)})
  , (("hypot", ArFixed 2), defaultFun{fexec = FnFn (MathFn2 hypot)})
  , (("atan2", ArFixed 2), defaultFun{fexec = FnFn (MathFn2 atan3)})
  , (("log2", ArFixed 2), defaultFun{fexec = FnFn (MathFn2 log2)})
  , (("log", ArFixed 2), defaultFun{fexec = FnFn (MathFn2 turboLog)})
  , (("pow", ArFixed 2), defaultFun{fexec = FnFn (MathFn2 pow)})
  , (("rad", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "*" [Id "x", Call "/" [Id "m.pi", unitlessNumber 180]])})
  , (("deg", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "*" [Id "x", Call "/" [unitlessNumber 180, Id "m.pi"]])})
  , (("cot", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "/" [unitlessOne, Call "tan" [Id "x"]])})
  , (("sec", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "/" [unitlessOne, Call "sin" [Id "x"]])})
  , (("csc", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "/" [unitlessOne, Call "cos" [Id "x"]])})
  , (("coth", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "/" [unitlessOne, Call "tanh" [Id "x"]])})
  , (("sech", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "/" [unitlessOne, Call "sinh" [Id "x"]])})
  , (("csch", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "/" [unitlessOne, Call "cosh" [Id "x"]])})
  , (("acot", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "atan" [Call "/" [unitlessOne, Id "x"]])})
  , (("asec", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "acos" [Call "/" [unitlessOne, Id "x"]])})
  , (("acsc", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "asin" [Call "/" [unitlessOne, Id "x"]])})
  , (("acoth", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "atanh" [Call "/" [unitlessOne, Id "x"]])})
  , (("asech", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "acosh" [Call "/" [unitlessOne, Id "x"]])})
  , (("acsch", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "asinh" [Call "/" [unitlessOne, Id "x"]])})
  , (("sinc", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", unitlessZero], Call "/" [Call "sin" [Id "x"], Id "x"], unitlessOne])})
  , (("cosc", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", unitlessZero], Call "-" [Call "/" [Call "cos" [Id "x"], Id "x"], Call "/" [Call "sin" [Id "x"], Call "^" [Id "x", unitlessNumber 2]]], unitlessOne])})
  , (("sincn", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", unitlessZero], Call "/" [Call "sin" [Call "*" [Id "m.pi", Id "x"]], Par (Call "*" [Id "m.pi", Id "x"])], unitlessOne])})
  , (("coscn", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", unitlessZero], Call "-" [Call "/" [Call "cos" [Call "*" [Id "m.pi", Id "x"]], Id "x"], Call "/" [Call "sin" [Call "*" [Id "m.pi", Id "x"]], Par (Call "*" [Id "m.pi", Call "^" [Id "x", unitlessNumber 2]])]], unitlessOne])})
  , (("hav", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "/" [Call "-" [unitlessOne, Call "cos" [Id "x"]], unitlessNumber 2])})
  , (("ahav", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "*" [unitlessNumber 2, Call "asin" [Call "sqrt" [Id "x"]]])})
  , (("trunc", ArFixed 1), defaultFun{fexec = FnFn (IntFn1 truncate)})
  , (("round", ArFixed 1), defaultFun{fexec = FnFn (IntFn1 round)})
  , (("floor", ArFixed 1), defaultFun{fexec = FnFn (IntFn1 floor)})
  , (("ceil", ArFixed 1), defaultFun{fexec = FnFn (IntFn1 ceiling)})
  , (("frac", ArFixed 1), defaultFun{fexec = FnFn (FracFn1 frac)})
  , (("gcd", ArFixed 2), defaultFun{fexec = FnFn (IntFn2 gcd)})
  , (("lcm", ArFixed 2), defaultFun{fexec = FnFn (IntFn2 lcm)})
  , (("div", ArFixed 2), defaultFun{fexec = FnFn (IntFn2 div)})
  , (("mod", ArFixed 2), defaultFun{fexec = FnFn (IntFn2 mod)})
  , (("quot", ArFixed 2), defaultFun{fexec = FnFn (IntFn2 quot)})
  , (("rem", ArFixed 2), defaultFun{fexec = FnFn (IntFn2 rem)})
  , (("xor", ArFixed 2), defaultFun{fexec = FnFn (IntFn2 xor)})
  , (("pop", ArFixed 1), defaultFun{fexec = FnFn (BitFn $ fromIntegral . popCount)})
  , (("comp", ArFixed 1), defaultFun{fexec = FnFn (BitFn complement)})
  , (("fact", ArFixed 1), defaultFun{fexec = FnFn (BitFn prod)})
  , (("not", ArFixed 1), defaultFun{params = ["x"], fexec = ExFn (Call "if" [Id "x", unitlessZero, unitlessOne])})
  , (("isprime", ArFixed 1), defaultFun{fexec = FnFn (BitFn isprime)})
  , (("P", ArFixed 2), defaultFun{fexec = FnFn (MathFn2 perm)})
  , (("C", ArFixed 2), defaultFun{fexec = FnFn (MathFn2 comb)})
  , (("dms", ArFixed 3), defaultFun{fexec = FnFn (MathFn3 dms)})
  , (("smd", ArFixed 1), defaultFun{fexec = FnFn (MultiFn smd)})
  , (("quad", ArFixed 3), defaultFun{fexec = FnFn (MultiFn quad)})
  ]

quad :: [Complex Rational] -> [Complex Rational]
quad [a, b, c] = let d = fmath (-) (pow b (2 :+ 0)) (mult (mult (4 :+ 0) a) c) in map (`divide` mult a (2 :+ 0)) [fmath (-) (mult b ((-1) :+ 0)) $ pow d (0.5 :+ 0), fmath (+) (mult b ((-1) :+ 0)) $ pow d (0.5 :+ 0)]
quad _ = []

smd :: [Complex Rational] -> [Complex Rational]
smd [r :+ _] =
  let
    t = round @_ @Integer $ fromRational @Precise (r * 3600)
    d = t `div` 3600
    m = (t - d * 3600) `div` 60
    s = (t - d * 3600) `mod` 60
   in
    (:+ 0) . toRational <$> [d, m, s]
smd _ = []

dms :: Complex Rational -> Complex Rational -> Complex Rational -> Complex Rational
dms (d :+ _) (m :+ _) (s :+ _) = (:+ 0) $ d + (m / 60) + (s / 3600)

perm :: Complex Rational -> Complex Rational -> Complex Rational
perm (n :+ _) (k :+ _) = (:+ 0) . toRational $ (fromInteger @Precise $ prod (numerator n)) / (fromInteger @Precise $ prod (numerator n - numerator k))

comb :: Complex Rational -> Complex Rational -> Complex Rational
comb (n :+ _) (k :+ _) = (:+ 0) . toRational $ (fromInteger @Precise $ prod (numerator n)) / fromInteger @Precise (prod (numerator n - numerator k) * prod (numerator k))

isprime :: Integer -> Integer
isprime n = if isPrime n then 1 else 0

turboLog :: Complex Rational -> Complex Rational -> Complex Rational
turboLog a b = toRational <$> logBase (fromRational <$> a :: Complex Precise) (fromRational <$> b :: Complex Precise)

prod :: Integer -> Integer
prod = product . enumFromTo 1

opMap :: OpMap
opMap = operators <> unaryOperators

funMap :: FunMap
funMap = functions

fmod :: Complex Rational -> Complex Rational -> Complex Rational
fmod x y =
  let a = realPart x
      b = realPart y
   in (:+ 0) $ a - fromInteger (truncate (a / b)) * b

fcmp :: Complex Rational -> Complex Rational -> Complex Rational
fcmp x y =
  let rx = realPart x
      ry = realPart y
   in case compare rx ry of
        GT -> 1 :+ 0
        EQ -> 0 :+ 0
        LT -> (-1) :+ 0

frac :: Complex Rational -> Complex Rational
frac x = let rp = realPart x in (rp - toRational @Integer (floor rp)) :+ 0

fmath :: (Rational -> Rational -> Rational) -> Complex Rational -> Complex Rational -> Complex Rational
fmath op x y = op <$> x <*> y

mult :: Complex Rational -> Complex Rational -> Complex Rational
mult (a :+ b) (c :+ d) = (a * c - b * d) :+ (a * d + b * c)

divide :: Complex Rational -> Complex Rational -> Complex Rational
divide (a :+ b) (c :+ d) = let dm = c * c + d * d in (a * c + b * d) / dm :+ (b * c - a * d) / dm

pow :: Complex Rational -> Complex Rational -> Complex Rational
pow a b
  | imagPart a == 0 && imagPart b == 0 =
      let (na, da) = splitRational (realPart a)
          (nb, db) = splitRational (realPart b)
       in if da == 1 && db == 1 && nb < 0
            then toRational <$> (fromRational <$> a :: Complex Precise) ^^ nb
            else
              if da == 1 && db == 1
                then (:+ 0) . toRational $ na ^ nb
                else pow'
  | otherwise = pow'
 where
  pow' = toRational <$> (fromRational <$> a :: Complex Precise) ** (fromRational <$> b :: Complex Precise)

hypot :: Complex Rational -> Complex Rational -> Complex Rational
hypot cx cy =
  let (x, y) = (realPart cx, realPart cy)
      min' = min x y
      max' = max x y
      r = min' / max'
   in max' * realPart (pow ((1 + r * r) :+ 0) (1 % 2 :+ 0)) :+ 0

wrapRealFun2 :: (Precise -> Precise -> Precise) -> Complex Rational -> Complex Rational -> Complex Rational
wrapRealFun2 f x y = (:+ 0) . toRational @Precise $ f (fromRational $ realPart x) (fromRational $ realPart y)

atan3 :: Complex Rational -> Complex Rational -> Complex Rational
atan3 = wrapRealFun2 atan2

log2 :: Complex Rational -> Complex Rational -> Complex Rational
log2 = wrapRealFun2 logBase

names :: [String]
names = T.unpack <$> map fst (M.keys operators) ++ map fst (M.keys functions)

defVar :: VarMap
defVar =
  [ ("m.pi", unitlessValue $ toRational (pi :: Precise) :+ 0)
  , ("π", unitlessValue $ toRational (pi :: Precise) :+ 0)
  , ("m.e", unitlessValue $ (toRational . exp $ (1 :: Precise)) :+ 0)
  , ("m.phi", unitlessValue $ toRational ((1 + sqrt 5) / 2 :: Precise) :+ 0)
  , ("φ", unitlessValue $ toRational ((1 + sqrt 5) / 2 :: Precise) :+ 0)
  , ("m.r", unitlessValue $ 0.0 :+ 0)
  , ("b.true", unitlessValue $ 1.0 :+ 0)
  , ("b.false", unitlessValue $ 0.0 :+ 0)
  , ("_", unitlessValue $ 0.0 :+ 0)
  ]

defaultMaps :: Maps
defaultMaps = Maps defVar funMap opMap M.empty

defaultEvalState :: EvalState
defaultEvalState = EvalState defaultMaps (mkStdGen 0) 16

getPrecedences :: OpMap -> Map (Text, OpArity) Int
getPrecedences = M.fromList . map (second precedence) . M.toList

getFakePrecedences :: FunMap -> Map (Text, OpArity) Int
getFakePrecedences = M.fromList . map (\(t, _) -> ((t, Ar2), maxPrecedence + 1)) . filter ((== ArFixed 2) . snd) . M.keys

derivative :: Expr -> Expr -> Either Text Expr
derivative e x = case e of
  Par ex -> Par <$> derivative ex x
  Call "-" [ex] -> Call "-" . (: []) <$> derivative ex x
  Number{} -> pure unitlessZero
  i@(Id _) | i == x -> pure unitlessOne
  (Id _) -> pure unitlessZero
  Call "^" [i, Number n _ _]
    | i == x ->
        pure $ Call "*" [Number n 0 Unitless, Call "^" [i, unitlessNumber (n - 1)]]
  Call "^" [Number a _ _, i] | i == x -> pure $ Call "*" [e, Call "log" [unitlessNumber a]]
  Call op [ex1, ex2] | op == "-" || op == "+" -> do
    a1 <- derivative ex1 x
    a2 <- derivative ex2 x
    pure $ Call op [a1, a2]
  Call "*" [ex1, ex2] -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    pure $ Call "+" [Call "*" [d1, ex2], Call "*" [d2, ex1]]
  Call "/" [ex1, ex2] -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    pure $ Call "/" [Call "-" [Call "*" [d1, ex2], Call "*" [d2, ex1]], Call "^" [ex2, unitlessNumber 2]]
  ex@(Call "exp" [i]) | i == x -> pure ex
  Call "log" [i] | i == x -> pure $ Call "/" [unitlessOne, i]
  Call "sin" [i] | i == x -> pure $ Call "cos" [i]
  Call "cos" [i] | i == x -> pure $ Call "-" [Call "sin" [i]]
  Call "tan" [i]
    | i == x ->
        pure $ Call "/" [unitlessOne, Call "^" [Call "cos" [i], unitlessNumber 2]]
  ex@(Call _ [i]) -> do
    a1 <- derivative ex i
    a2 <- derivative i x
    pure $ Call "*" [a1, a2]
  _ -> Left "No such derivative"
