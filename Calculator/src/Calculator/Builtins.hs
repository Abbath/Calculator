{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator.Builtins where

import Calculator.Types (Assoc (..), OpMap, FunMap, VarMap, Op(..), ExecOp(..), FunOp(..), Fun(..), ExecFn(..), FunFun(..), Expr(..))
import Data.Bits ((.|.), (.&.), xor, shift, popCount, complement, Bits (complement))
import Data.Ratio (denominator, numerator)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow (second)
import Data.Complex

operators :: Map Text Op
operators =
    [
       ("=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("+=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("-=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("*=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("/=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("%=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("^=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("|=", Op { precedence = 0, associativity = R, oexec = NOp }),
       ("&=", Op { precedence = 0, associativity = R, oexec = NOp }),
       (":=", Op { precedence = 1, associativity = R, oexec = NOp }),
       ("==", Op { precedence = 2, associativity = L, oexec = FnOp (CmpOp (==)) }),
       ("<=", Op { precedence = 2, associativity = L, oexec = FnOp (CmpOp (<=)) }),
       (">=", Op { precedence = 2, associativity = L, oexec = FnOp (CmpOp (>=)) }),
       ("!=", Op { precedence = 2, associativity = L, oexec = FnOp (CmpOp (/=)) }),
       ("<", Op { precedence = 2, associativity = L, oexec = FnOp (CmpOp (<)) }),
       (">", Op { precedence = 2, associativity = L, oexec = FnOp (CmpOp (>)) }),
       ("<<", Op { precedence = 3, associativity = R, oexec = FnOp (BitOp (\n s -> shift n (fromInteger s))) }),
       (">>", Op { precedence = 3, associativity = R, oexec = FnOp (BitOp (\n s -> shift n (-1 * fromInteger s))) }),
       ("+", Op { precedence = 4, associativity = L, oexec = FnOp (MathOp $ fmath (+)) }),
       ("-", Op { precedence = 4, associativity = L, oexec = FnOp (MathOp $ fmath (-)) }),
       ("*", Op { precedence = 5, associativity = L, oexec = FnOp (MathOp $ fmath (*)) }),
       ("/", Op { precedence = 5, associativity = L, oexec = FnOp (MathOp $ fmath (/)) }),
       ("%", Op { precedence = 5, associativity = L, oexec = FnOp (MathOp fmod) }),
       ("^", Op { precedence = 6, associativity = R, oexec = FnOp (MathOp pow) }),
       ("|", Op { precedence = 7, associativity = R, oexec = FnOp (BitOp (.|.)) }),
       ("&", Op { precedence = 8, associativity = R, oexec = FnOp (BitOp (.&.)) }),
       ("cmp", Op { precedence = 9, associativity = L, oexec = FnOp (MathOp fcmp) }),
       ("|>", Op { precedence = 10, associativity = L, oexec = NOp }),
       ("::=", Op { precedence = 11, associativity = R, oexec = NOp })
    ]

functions :: Map (Text, Int) Fun
functions =
    [
       (("prat", 1), Fun { params = [], fexec = NFn }),
       (("str", 1), Fun { params = [], fexec = NFn }),
       (("fmt", 1), Fun { params = [], fexec = NFn }),
       (("quit", 0), Fun { params = [], fexec = NFn }),
       (("if", 3), Fun { params = [], fexec = NFn }),
       (("loop", 2), Fun { params = [], fexec = NFn }),
       (("df", 2), Fun { params = [], fexec = NFn }),
       (("int", 4), Fun { params = [], fexec = NFn }),
       (("log", 2), Fun { params = [], fexec = NFn }),
       (("hex", 1), Fun { params = [], fexec = NFn }),
       (("oct", 1), Fun { params = [], fexec = NFn }),
       (("bin", 1), Fun { params = [], fexec = NFn }),
       (("lt", 2), Fun { params = [], fexec = FnFn (CmpFn (<)) }),
       (("gt", 2), Fun { params = [], fexec = FnFn (CmpFn (>)) }),
       (("eq", 2), Fun { params = [], fexec = FnFn (CmpFn (==)) }),
       (("ne", 2), Fun { params = [], fexec = FnFn (CmpFn (/=)) }),
       (("le", 2), Fun { params = [], fexec = FnFn (CmpFn (<=)) }),
       (("ge", 2), Fun { params = [], fexec = FnFn (CmpFn (>=)) }),
       (("sin", 1), Fun { params = [], fexec = FnFn (MathFn sin) }),
       (("cos", 1), Fun { params = [], fexec = FnFn (MathFn cos) }),
       (("asin", 1), Fun { params = [], fexec = FnFn (MathFn asin) }),
       (("acos", 1), Fun { params = [], fexec = FnFn (MathFn acos) }),
       (("tan", 1), Fun { params = [], fexec = FnFn (MathFn tan) }),
       (("atan", 1), Fun { params = [], fexec = FnFn (MathFn atan) }),
       (("log", 1), Fun { params = [], fexec = FnFn (MathFn log) }),
       (("exp", 1), Fun { params = [], fexec = FnFn (MathFn exp) }),
       (("sqrt", 1), Fun { params = [], fexec = FnFn (MathFn sqrt) }),
       (("abs", 1), Fun { params = [], fexec = FnFn (MathFn abs) }),
       (("sinh", 1), Fun { params = [], fexec = FnFn (MathFn sinh) }),
       (("cosh", 1), Fun { params = [], fexec = FnFn (MathFn cosh) }),
       (("tanh", 1), Fun { params = [], fexec = FnFn (MathFn tanh) }),
       (("asinh", 1), Fun { params = [], fexec = FnFn (MathFn asinh) }),
       (("acosh", 1), Fun { params = [], fexec = FnFn (MathFn acosh) }),
       (("atanh", 1), Fun { params = [], fexec = FnFn (MathFn atanh) }),
       (("cot", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "tan" [Id "x"]]) }),
       (("sec", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "sin" [Id "x"]]) }),
       (("csc", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "cos" [Id "x"]]) }),
       (("coth", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "tanh" [Id "x"]]) }),
       (("sech", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "sinh" [Id "x"]]) }),
       (("csch", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Number 1 0, Call "cosh" [Id "x"]]) }),
       (("acot", 1), Fun { params = ["x"], fexec = ExFn (Call "atan" [Call "/" [Number 1 0, Id "x"]]) }),
       (("asec", 1), Fun { params = ["x"], fexec = ExFn (Call "acos" [Call "/" [Number 1 0, Id "x"]]) }),
       (("acsc", 1), Fun { params = ["x"], fexec = ExFn (Call "asin" [Call "/" [Number 1 0, Id "x"]]) }),
       (("acoth", 1), Fun { params = ["x"], fexec = ExFn (Call "atanh" [Call "/" [Number 1 0, Id "x"]]) }),
       (("asech", 1), Fun { params = ["x"], fexec = ExFn (Call "acosh" [Call "/" [Number 1 0, Id "x"]]) }),
       (("acsch", 1), Fun { params = ["x"], fexec = ExFn (Call "asinh" [Call "/" [Number 1 0, Id "x"]]) }),
       (("sinc", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Call "sin" [Id "x"], Id "x"]) }),
       (("cosc", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Call "cos" [Id "x"], Id "x"]) }),
       (("sincn", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Call "sin" [Call "*" [Id "m.pi", Id "x"]], Par (Call "*" [Id "m.pi", Id "x"])]) }),
       (("coscn", 1), Fun { params = ["x"], fexec = ExFn (Call "/" [Call "cos" [Call "*" [Id "m.pi", Id "x"]], Par (Call "*" [Id "m.pi", Id "x"])]) }),
       (("trunc", 1), Fun { params = [], fexec = FnFn (IntFn1 truncate) }),
       (("round", 1), Fun { params = [], fexec = FnFn (IntFn1 round) }),
       (("floor", 1), Fun { params = [], fexec = FnFn (IntFn1 floor) }),
       (("ceil", 1), Fun { params = [], fexec = FnFn (IntFn1 ceiling) }),
       (("gcd", 2), Fun { params = [], fexec = FnFn (IntFn2 gcd) }),
       (("lcm", 2), Fun { params = [], fexec = FnFn (IntFn2 lcm) }),
       (("div", 2), Fun { params = [], fexec = FnFn (IntFn2 div) }),
       (("mod", 2), Fun { params = [], fexec = FnFn (IntFn2 mod) }),
       (("quot", 2), Fun { params = [], fexec = FnFn (IntFn2 quot) }),
       (("rem", 2), Fun { params = [], fexec = FnFn (IntFn2 rem) }),
       (("xor", 2), Fun { params = [], fexec = FnFn (IntFn2 xor) }),
       (("pop", 1), Fun { params = [], fexec = FnFn (BitFn $ fromIntegral . popCount) }),
       (("comp", 1), Fun { params = [], fexec = FnFn (BitFn complement) }),
       (("not", 1), Fun { params = ["x"], fexec = ExFn (Call "if" [Id "x", Number 0 0, Number 1 0])})
    ]

opMap :: OpMap
opMap = operators

funMap :: FunMap
funMap = functions

fmod :: Complex Rational -> Complex Rational -> Complex Rational
fmod x y = (:+0) . fromInteger $ mod (floor . realPart $ x) (floor . realPart $ y)

fcmp :: Complex Rational -> Complex Rational -> Complex Rational
fcmp x y =
  let rx = realPart x
      ry = realPart y
  in case compare rx ry of
    GT -> 1:+0
    EQ -> 0:+0
    LT -> (-1):+0

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
      then (:+0) . toRational $ (fromRational (realPart a) :: Double) ^^ nb
      else if da == 1 && na == 1
        then (:+0) . toRational $ na ^ nb
        else (:+0) . toRational $ (fromRational (realPart a) :: Double) ** (fromRational (realPart b) :: Double)
pow a b = toRational <$> (fromRational <$> a :: Complex Double) ** (fromRational <$> b :: Complex Double)

names :: [String]
names = T.unpack <$> M.keys operators ++ map fst (M.keys functions)

defVar :: VarMap
defVar = [("m.pi", toRational (pi :: Double) :+ 0),
          ("m.e", (toRational . exp $ (1 :: Double)) :+ 0),
          ("m.phi", toRational ((1 + sqrt 5) / 2 :: Double) :+ 0),
          ("m.r", 0.0 :+ 0),
          ("b.true", 1.0 :+ 0),
          ("b.false", 0.0 :+ 0),
          ("_", 0.0 :+ 0)]

getPrecedences :: OpMap -> Map Text Int
getPrecedences om = let lst = M.toList om
                    in M.fromList $ map (second precedence) lst

getFakePrecedences :: FunMap -> Map Text Int
getFakePrecedences fm = let lst = M.keys fm
                        in M.fromList . map (\(name, argnum) -> (name, 14)) . filter (\(_, argnum) -> argnum == 2) $ lst
