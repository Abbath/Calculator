module Calculator.Types (Expr(..), Token(..), Operator(..), Function(..)) where

import Data.List (intercalate)

data Operator = Assign | Plus | Minus | Mult | Div | Mod | Power deriving (Show, Eq)

data Function = Sin | Cos | Tan | Asin | Acos | Atan | Log | Exp | Sqrt | Abs deriving (Show, Eq)

data Token = TNumber Double
           | TOp Operator
           | TLPar
           | TRPar
           | TFun Function
           | TIdent String
           | TComma
           | TEnd
           deriving (Show, Eq)

data Expr = Number Double
          | Asgn String Expr
          | UDF String [String] Expr
          | Sum Operator Expr Expr
          | Prod Operator Expr Expr
          | Pow Expr Expr
          | Fun Function Expr
          | UMinus Expr
          | Par Expr
          | FunCall String [Expr]
          | Id String
          deriving Eq

instance Show Expr where
    show = showExpr 0

showExpr :: Int -> Expr -> String
showExpr n e =
    let suf = case e of
            (UDF n a e)     -> n ++ "("++ intercalate ", " a ++ ")" ++ "\n" ++ s e
            (Asgn i e)      -> "Assign " ++ i ++ "\n" ++ s e
            (Sum op e1 e2)  -> "Sum " ++ show op ++ "\n" ++ s e1 ++ "\n" ++ s e2
            (Prod op e1 e2) -> "Prod " ++ show op ++ "\n" ++ s e1 ++ "\n" ++ s e2
            (Pow e1 e2)     -> "Pow \n" ++ s e1 ++ "\n" ++ s e2
            (Number x )     -> "Number " ++ show x
            (Par e)         -> "Par \n" ++ s e
            (UMinus e)      -> "UMinus \n" ++ s e
            (Fun f e)       -> "Fun " ++ show f ++ "\n" ++ s e
            (FunCall n e)   -> "FunCall " ++ n ++ "\n" ++ intercalate "\n" (map s e)
            (Id s)          -> "Id " ++ s
    in replicate n ' ' ++ suf
    where s = showExpr (n+1)