{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator.Builtins where

import Calculator.Types (Assoc (..), OpMap, FunMap, VarMap, Op(..), ExecOp(..), FunOp(..), Fun(..), ExecFn(..), FunFun(..), Expr(..))
import Data.Bits ((.|.), (.&.), xor, shift)
import Data.Ratio (denominator, numerator)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

operators :: Map Text Op
operators =
    [
       ("=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("+=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("-=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("*=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("/=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("%=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("^=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("|=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("&=", Op { priority = 0, associativity = R, oexec = NOp }),
       ("::=", Op { priority = 1, associativity = R, oexec = NOp }),
       ("==", Op { priority = 2, associativity = L, oexec = FnOp (CmpOp (==)) }),
       ("<=", Op { priority = 2, associativity = L, oexec = FnOp (CmpOp (<=)) }),
       (">=", Op { priority = 2, associativity = L, oexec = FnOp (CmpOp (>=)) }),
       ("!=", Op { priority = 2, associativity = L, oexec = FnOp (CmpOp (/=)) }),
       ("<", Op { priority = 2, associativity = L, oexec = FnOp (CmpOp (<)) }),
       (">", Op { priority = 2, associativity = L, oexec = FnOp (CmpOp (>)) }),
       ("<<", Op { priority = 3, associativity = R, oexec = FnOp (BitOp (\n s -> shift n (fromInteger s))) }),
       (">>", Op { priority = 3, associativity = R, oexec = FnOp (BitOp (\n s -> shift n (-1 * fromInteger s))) }),
       ("+", Op { priority = 4, associativity = L, oexec = FnOp (MathOp (+)) }),
       ("-", Op { priority = 4, associativity = L, oexec = FnOp (MathOp (-)) }),
       ("*", Op { priority = 5, associativity = L, oexec = FnOp (MathOp (*)) }),
       ("/", Op { priority = 5, associativity = L, oexec = FnOp (MathOp (/)) }),
       ("%", Op { priority = 5, associativity = L, oexec = FnOp (MathOp fmod) }),
       ("^", Op { priority = 6, associativity = R, oexec = FnOp (MathOp pow) }),
       ("|", Op { priority = 7, associativity = R, oexec = FnOp (BitOp (.|.)) }),
       ("&", Op { priority = 8, associativity = R, oexec = FnOp (BitOp (.&.)) }),
       ("cmp", Op { priority = 9, associativity = L, oexec = FnOp (MathOp fcmp) }),
       (":=", Op { priority = 10, associativity = R, oexec = NOp })
    ]

functions :: Map (Text, Int) Fun
functions =
    [
       (("prat", 1), Fun { params = [], fexec = NFn }),
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
       (("not", 1), Fun { params = ["x"], fexec = ExFn (Call "if" [Id "x", Number 0, Number 1])})
    ]

opMap :: OpMap
opMap = operators

funMap :: FunMap
funMap = functions

fmod :: Rational -> Rational -> Rational
fmod x y = fromInteger $ mod (floor x) (floor y)

fcmp :: Rational -> Rational -> Rational
fcmp x y = case compare x y of
  GT -> 1
  EQ -> 0
  LT -> -1

pow :: Rational -> Rational -> Rational
pow a b 
  | denominator a == 1 && denominator b == 1 && numerator b < 0 = toRational $ (fromRational a :: Double) ^^ numerator b
  | denominator a == 1 && denominator b == 1 = toRational $ numerator a ^ numerator b
pow a b = toRational $ (fromRational a :: Double) ** (fromRational b :: Double)

names :: [String]
names = T.unpack <$> M.keys operators ++ map fst (M.keys functions)

defVar :: VarMap
defVar = [("m.pi", toRational (pi :: Double)), 
          ("m.e", toRational . exp $ (1 :: Double)), 
          ("m.phi", toRational ((1 + sqrt 5) / 2 :: Double)), 
          ("m.r", 0.0),
          ("_", 0.0)]
