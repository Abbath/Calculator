{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator.Builtins where

import Calculator.Types (Arity (..), Assoc (..), ExecFn (..), ExecOp (..), Expr (..), Fun (..), FunFun (..), FunMap, FunOp (..), Maps (..), Op (..), OpMap, Precise, VarMap)
import Control.Arrow (second)
import Data.Bits (Bits (complement), complement, popCount, shift, xor, (.&.), (.|.))
import Data.Complex (Complex (..), imagPart, realPart)
import Data.Foldable (maximumBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Numeric (expm1, log1mexp, log1p, log1pexp)

operators :: OpMap
operators =
  [ ("=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("<-", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("+=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("-=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("*=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("/=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("%=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("^=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("|=", Op{precedence = 0, associativity = R, oexec = NOp})
  , ("&=", Op{precedence = 0, associativity = R, oexec = NOp})
  , (":", Op{precedence = 1, associativity = L, oexec = NOp})
  , (":=", Op{precedence = 2, associativity = R, oexec = NOp})
  , ("==", Op{precedence = 3, associativity = L, oexec = FnOp (CmpOp (==))})
  , ("<=", Op{precedence = 3, associativity = L, oexec = FnOp (CmpOp (<=))})
  , (">=", Op{precedence = 3, associativity = L, oexec = FnOp (CmpOp (>=))})
  , ("!=", Op{precedence = 3, associativity = L, oexec = FnOp (CmpOp (/=))})
  , ("<", Op{precedence = 3, associativity = L, oexec = FnOp (CmpOp (<))})
  , (">", Op{precedence = 3, associativity = L, oexec = FnOp (CmpOp (>))})
  , ("<<", Op{precedence = 4, associativity = R, oexec = FnOp (BitOp (\n s -> shift n (fromInteger s)))})
  , (">>", Op{precedence = 4, associativity = R, oexec = FnOp (BitOp (\n s -> shift n ((-1) * fromInteger s)))})
  , ("+", Op{precedence = 5, associativity = L, oexec = FnOp (MathOp $ fmath (+))})
  , ("-", Op{precedence = 5, associativity = L, oexec = FnOp (MathOp $ fmath (-))})
  , ("*", Op{precedence = 6, associativity = L, oexec = FnOp (MathOp $ fmath (*))})
  , ("/", Op{precedence = 6, associativity = L, oexec = FnOp (MathOp $ fmath (/))})
  , ("%", Op{precedence = 6, associativity = L, oexec = FnOp (MathOp fmod)})
  , ("^", Op{precedence = 7, associativity = R, oexec = FnOp (MathOp pow)})
  , ("|", Op{precedence = 8, associativity = R, oexec = FnOp (BitOp (.|.))})
  , ("&", Op{precedence = 9, associativity = R, oexec = FnOp (BitOp (.&.))})
  , ("cmp", Op{precedence = 10, associativity = L, oexec = FnOp (MathOp fcmp)})
  , ("|>", Op{precedence = 11, associativity = L, oexec = NOp})
  , ("::=", Op{precedence = 12, associativity = R, oexec = NOp})
  , ("?", Op{precedence = 13, associativity = L, oexec = NOp})
  , ("!", Op{precedence = 13, associativity = L, oexec = NOp})
  , ("~", Op{precedence = 13, associativity = L, oexec = NOp})
  ]

maxPrecedence :: Int
maxPrecedence = precedence . snd $ maximumBy (\a b -> precedence (snd a) `compare` precedence (snd b)) (M.toList operators)

linearOperators :: V.Vector (Text, Op)
linearOperators = V.fromList $ M.assocs operators

functions :: FunMap
functions =
  [ (("prat", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("str", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("fmt", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("quit", ArFixed 0), Fun{params = [], fexec = NFn})
  , (("id", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("if", ArFixed 3), Fun{params = [], fexec = NFn})
  , (("loop", ArFixed 2), Fun{params = [], fexec = NFn})
  , (("df", ArFixed 2), Fun{params = [], fexec = NFn})
  , (("int", ArFixed 4), Fun{params = [], fexec = NFn})
  , (("log", ArFixed 2), Fun{params = [], fexec = NFn})
  , (("hex", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("oct", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("bin", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("undef", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("opt", ArFixed 1), Fun{params = [], fexec = NFn})
  , (("opt", ArFixed 2), Fun{params = [], fexec = NFn})
  , (("lt", ArFixed 2), Fun{params = [], fexec = FnFn (CmpFn (<))})
  , (("gt", ArFixed 2), Fun{params = [], fexec = FnFn (CmpFn (>))})
  , (("eq", ArFixed 2), Fun{params = [], fexec = FnFn (CmpFn (==))})
  , (("ne", ArFixed 2), Fun{params = [], fexec = FnFn (CmpFn (/=))})
  , (("le", ArFixed 2), Fun{params = [], fexec = FnFn (CmpFn (<=))})
  , (("ge", ArFixed 2), Fun{params = [], fexec = FnFn (CmpFn (>=))})
  , (("sin", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 sin)})
  , (("cos", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 cos)})
  , (("asin", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 asin)})
  , (("acos", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 acos)})
  , (("tan", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 tan)})
  , (("atan", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 atan)})
  , (("log", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 log)})
  , (("exp", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 exp)})
  , (("log1p", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 log1p)})
  , (("expm1", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 expm1)})
  , (("log1pexp", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 log1pexp)})
  , (("log1mexp", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 log1mexp)})
  , (("sqrt", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 sqrt)})
  , (("abs", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 abs)})
  , (("sinh", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 sinh)})
  , (("cosh", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 cosh)})
  , (("tanh", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 tanh)})
  , (("asinh", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 asinh)})
  , (("acosh", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 acosh)})
  , (("atanh", ArFixed 1), Fun{params = [], fexec = FnFn (MathFn1 atanh)})
  , (("hypot", ArFixed 2), Fun{params = [], fexec = FnFn (MathFn2 hypot)})
  , (("atan2", ArFixed 2), Fun{params = [], fexec = FnFn (MathFn2 atan3)})
  , (("log2", ArFixed 2), Fun{params = [], fexec = FnFn (MathFn2 log2)})
  , (("pow", ArFixed 2), Fun{params = [], fexec = FnFn (MathFn2 pow)})
  , (("cot", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "tan" [Id "x"]])})
  , (("sec", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "sin" [Id "x"]])})
  , (("csc", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "cos" [Id "x"]])})
  , (("coth", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "tanh" [Id "x"]])})
  , (("sech", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "sinh" [Id "x"]])})
  , (("csch", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "cosh" [Id "x"]])})
  , (("acot", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "atan" [Call "/" [Number 1 0, Id "x"]])})
  , (("asec", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "acos" [Call "/" [Number 1 0, Id "x"]])})
  , (("acsc", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "asin" [Call "/" [Number 1 0, Id "x"]])})
  , (("acoth", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "atanh" [Call "/" [Number 1 0, Id "x"]])})
  , (("asech", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "acosh" [Call "/" [Number 1 0, Id "x"]])})
  , (("acsch", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "asinh" [Call "/" [Number 1 0, Id "x"]])})
  , (("sinc", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", Number 0 0], Call "/" [Call "sin" [Id "x"], Id "x"], Number 1 0])})
  , (("cosc", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", Number 0 0], Call "-" [Call "/" [Call "cos" [Id "x"], Id "x"], Call "/" [Call "sin" [Id "x"], Call "^" [Id "x", Number 2 0]]], Number 1 0])})
  , (("sincn", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", Number 0 0], Call "/" [Call "sin" [Call "*" [Id "m.pi", Id "x"]], Par (Call "*" [Id "m.pi", Id "x"])], Number 1 0])})
  , (("coscn", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "if" [Call "!=" [Id "x", Number 0 0], Call "-" [Call "/" [Call "cos" [Call "*" [Id "m.pi", Id "x"]], Id "x"], Call "/" [Call "sin" [Call "*" [Id "m.pi", Id "x"]], Par (Call "*" [Id "m.pi", Call "^" [Id "x", Number 2 0]])]], Number 1 0])})
  , (("hav", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "/" [Call "-" [Number 1 0, Call "cos" [Id "x"]], Number 2 0])})
  , (("ahav", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "*" [Number 2 0, Call "asin" [Call "sqrt" [Id "x"]]])})
  , (("trunc", ArFixed 1), Fun{params = [], fexec = FnFn (IntFn1 truncate)})
  , (("round", ArFixed 1), Fun{params = [], fexec = FnFn (IntFn1 round)})
  , (("floor", ArFixed 1), Fun{params = [], fexec = FnFn (IntFn1 floor)})
  , (("ceil", ArFixed 1), Fun{params = [], fexec = FnFn (IntFn1 ceiling)})
  , (("frac", ArFixed 1), Fun{params = [], fexec = FnFn (FracFn1 frac)})
  , (("gcd", ArFixed 2), Fun{params = [], fexec = FnFn (IntFn2 gcd)})
  , (("lcm", ArFixed 2), Fun{params = [], fexec = FnFn (IntFn2 lcm)})
  , (("div", ArFixed 2), Fun{params = [], fexec = FnFn (IntFn2 div)})
  , (("mod", ArFixed 2), Fun{params = [], fexec = FnFn (IntFn2 mod)})
  , (("quot", ArFixed 2), Fun{params = [], fexec = FnFn (IntFn2 quot)})
  , (("rem", ArFixed 2), Fun{params = [], fexec = FnFn (IntFn2 rem)})
  , (("xor", ArFixed 2), Fun{params = [], fexec = FnFn (IntFn2 xor)})
  , (("pop", ArFixed 1), Fun{params = [], fexec = FnFn (BitFn $ fromIntegral . popCount)})
  , (("comp", ArFixed 1), Fun{params = [], fexec = FnFn (BitFn complement)})
  , (("fact", ArFixed 1), Fun{params = [], fexec = FnFn (BitFn prod)})
  , (("not", ArFixed 1), Fun{params = ["x"], fexec = ExFn (Call "if" [Id "x", Number 0 0, Number 1 0])})
  ]

prod :: Integer -> Integer
prod = product . enumFromTo 1

opMap :: OpMap
opMap = operators

funMap :: FunMap
funMap = functions

fmod :: Complex Rational -> Complex Rational -> Complex Rational
fmod x y = (:+ 0) . fromInteger $ mod (floor . realPart $ x) (floor . realPart $ y)

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

pow :: Complex Rational -> Complex Rational -> Complex Rational
pow a b
  | imagPart a == 0 && imagPart b == 0 =
      let da = denominator (realPart a)
          db = denominator (realPart b)
          na = numerator (realPart a)
          nb = numerator (realPart b)
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
names = T.unpack <$> M.keys operators ++ map fst (M.keys functions)

defVar :: VarMap
defVar =
  [ ("m.pi", toRational (pi :: Precise) :+ 0)
  , ("m.e", (toRational . exp $ (1 :: Precise)) :+ 0)
  , ("m.phi", toRational ((1 + sqrt 5) / 2 :: Precise) :+ 0)
  , ("m.r", 0.0 :+ 0)
  , ("b.true", 1.0 :+ 0)
  , ("b.false", 0.0 :+ 0)
  , ("_", 0.0 :+ 0)
  ]

defaultMaps :: Maps
defaultMaps = Maps defVar funMap opMap M.empty

getPrecedences :: OpMap -> Map Text Int
getPrecedences = M.fromList . map (second precedence) . M.toList

getFakePrecedences :: FunMap -> Map Text Int
getFakePrecedences = M.fromList . map (second $ const (maxPrecedence + 1)) . filter ((== ArFixed 2) . snd) . M.keys

derivative :: Expr -> Expr -> Either Text Expr
derivative e x = case e of
  Par ex -> Par <$> derivative ex x
  Call "-" [ex] -> Call "-" . (: []) <$> derivative ex x
  Number _ _ -> return $ Number 0 0
  i@(Id _) | i == x -> return $ Number 1 0
  (Id _) -> return $ Number 0 0
  Call "^" [i, Number n _]
    | i == x ->
        return $ Call "*" [Number n 0, Call "^" [i, Number (n - 1) 0]]
  Call "^" [Number a _, i] | i == x -> return $ Call "*" [e, Call "log" [Number a 0]]
  Call op [ex1, ex2] | op == "-" || op == "+" -> do
    a1 <- derivative ex1 x
    a2 <- derivative ex2 x
    return $ Call op [a1, a2]
  Call "*" [ex1, ex2] -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    return $ Call "+" [Call "*" [d1, ex2], Call "*" [d2, ex1]]
  Call "/" [ex1, ex2] -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    return $ Call "/" [Call "-" [Call "*" [d1, ex2], Call "*" [d2, ex1]], Call "^" [ex2, Number 2 0]]
  ex@(Call "exp" [i]) | i == x -> return ex
  Call "log" [i] | i == x -> return $ Call "/" [Number 1 0, i]
  Call "sin" [i] | i == x -> return $ Call "cos" [i]
  Call "cos" [i] | i == x -> return $ Call "-" [Call "sin" [i]]
  Call "tan" [i]
    | i == x ->
        return $ Call "/" [Number 1 0, Call "^" [Call "cos" [i], Number 2 0]]
  ex@(Call _ [i]) -> do
    a1 <- derivative ex i
    a2 <- derivative i x
    return $ Call "*" [a1, a2]
  _ -> Left "No such derivative"
