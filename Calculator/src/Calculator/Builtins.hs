{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator.Builtins where

import Calculator.Types (Assoc (..), OpMap, Op(..), ExecOp(..), FunOp(..), Fun(..), ExecFn(..), FunFun(..))
import Data.Bits ((.|.), (.&.), xor)
import Data.Ratio (denominator, numerator)
import Data.Map.Strict (Map)
import Data.Text (Text)

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
       ("==", Op { priority = 1, associativity = L, oexec = FnOp (CmpOp (==)) }),
       ("<=", Op { priority = 1, associativity = L, oexec = FnOp (CmpOp (<=)) }),
       (">=", Op { priority = 1, associativity = L, oexec = FnOp (CmpOp (>=)) }),
       ("!=", Op { priority = 1, associativity = L, oexec = FnOp (CmpOp (/=)) }),
       ("<", Op { priority = 1, associativity = L, oexec = FnOp (CmpOp (<)) }),
       (">", Op { priority = 1, associativity = L, oexec = FnOp (CmpOp (>)) }),
       ("+", Op { priority = 2, associativity = L, oexec = FnOp (MathOp (+)) }),
       ("-", Op { priority = 2, associativity = L, oexec = FnOp (MathOp (-)) }),
       ("*", Op { priority = 3, associativity = L, oexec = FnOp (MathOp (*)) }),
       ("/", Op { priority = 3, associativity = L, oexec = FnOp (MathOp (/)) }),
       ("%", Op { priority = 3, associativity = L, oexec = FnOp (MathOp fmod) }),
       ("^", Op { priority = 4, associativity = R, oexec = FnOp (MathOp pow) }),
       ("|", Op { priority = 5, associativity = R, oexec = FnOp (BitOp (.|.)) }),
       ("&", Op { priority = 6, associativity = R, oexec = FnOp (BitOp (.&.)) }),
       ("cmp", Op { priority = 7, associativity = L, oexec = FnOp (MathOp fcmp) })
    ]

functions :: Map (Text, Int) Fun
functions =
    [
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
       (("xor", 2), Fun { params = [], fexec = FnFn (IntFn2 xor) })
    ]

opMap :: OpMap
opMap = operators

compFuns :: Map Text (Rational -> Rational -> Bool)
compFuns =
  [ ("lt", (<)),
    ("gt", (>)),
    ("eq", (==)),
    ("ne", (/=)),
    ("le", (<=)),
    ("ge", (>=))
  ]

mathFuns :: Map Text (Double -> Double)
mathFuns =
  [ ("sin", sin),
    ("cos", cos),
    ("asin", asin),
    ("acos", acos),
    ("tan", tan),
    ("atan", atan),
    ("log", log),
    ("exp", exp),
    ("sqrt", sqrt),
    ("abs", abs),
    ("sinh", sinh),
    ("cosh", cosh),
    ("tanh", tanh),
    ("asinh", asinh),
    ("acosh", acosh),
    ("atanh", atanh)
  ]

intFuns1 :: Map Text (Double -> Integer)
intFuns1 = [("trunc", truncate), ("round", round), ("floor", floor), ("ceil", ceiling)]

intFuns2 :: Map Text (Integer -> Integer -> Integer)
intFuns2 = [("gcd", gcd), ("lcm", lcm), ("div", div), ("mod", mod), ("quot", quot), ("rem", rem), ("xor", xor)]

fmod :: Rational -> Rational -> Rational
fmod x y = fromInteger $ mod (floor x) (floor y)

fcmp :: Rational -> Rational -> Rational
fcmp x y = case compare x y of
  GT -> 1
  EQ -> 0
  LT -> -1

pow :: Rational -> Rational -> Rational
pow a b | denominator a == 1 && denominator b == 1 && numerator b < 0 = toRational $ (fromRational a :: Double) ^^ numerator b
pow a b | denominator a == 1 && denominator b == 1 = toRational $ numerator a ^ numerator b
pow a b = toRational $ (fromRational a :: Double) ** (fromRational b :: Double)

names :: [String]
names = ["!=","%","*","+","-","/","<","<=","=","==",">",">=","^","&","|","trunc","round","floor","ceil",
  "sin","cos","tan","asin","acos","atan","sinh","cosh","tanh","asinh","acosh","atanh","log","sqrt","exp",
  "abs","xor","not","int","df","hex","oct","bin","lt","gt","le","ge","eq","ne","if","df","gcd","lcm","div",
  "mod","quot","rem","prat","quit","cmp"]