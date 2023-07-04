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
       ("+", Op { precedence = 4, associativity = L, oexec = FnOp (MathOp (+)) }),
       ("-", Op { precedence = 4, associativity = L, oexec = FnOp (MathOp (-)) }),
       ("*", Op { precedence = 5, associativity = L, oexec = FnOp (MathOp (*)) }),
       ("/", Op { precedence = 5, associativity = L, oexec = FnOp (MathOp (/)) }),
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
          ("b.true", 1.0),
          ("b.false", 0.0),
          ("_", 0.0)]

getPrecedences :: OpMap -> Map Text Int
getPrecedences om = let lst = M.toList om
                    in M.fromList $ map (second precedence) lst

getFakePrecedences :: FunMap -> Map Text Int
getFakePrecedences fm = let lst = M.keys fm
                        in M.fromList . map (\(name, argnum) -> (name, 14)) . filter (\(_, argnum) -> argnum == 2) $ lst
