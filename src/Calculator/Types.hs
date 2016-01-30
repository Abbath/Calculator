module Calculator.Types (Expr(..), Token(..), Assoc(..), exprToString, unTOp) where

import Data.List (intercalate)

data Token = TNumber Double
           | TLPar
           | TRPar
           | TIdent String
           | TOp String
           | TComma
           | TEnd
           deriving (Show, Eq, Ord)

data Assoc = L | R deriving (Show, Eq, Ord)

data Expr = Number Double
          | Asgn String Expr
          | UDF String [String] Expr
          | UDO String Int Assoc Expr
          | OpCall String Expr Expr
          | UMinus Expr
          | Par Expr
          | FunCall String [Expr]
          | Id String
          deriving Eq

instance Show Expr where
  show = showExpr 0

showExpr :: Int -> Expr -> String
showExpr n ex =
  let suf = case ex of
        (UDF name a e)    -> name ++ "("++ intercalate ", " a ++ ")" ++ "\n" ++ s e
        (UDO name p a e)  -> name ++ "("++ show p ++ ", " ++ show a ++ ")" ++ "\n" ++ s e
        (Asgn i e)        -> "Assign " ++ i ++ "\n" ++ s e
        (Number x )       -> "Number " ++ show x
        (Par e)           -> "Par \n" ++ s e
        (UMinus e)        -> "UMinus \n" ++ s e
        (OpCall op e1 e2) -> "OpCall " ++ op ++ "\n" ++ s e1 ++ "\n" ++ s e2
        (FunCall name e)  -> "FunCall " ++ name ++ "\n" ++ intercalate "\n" (map s e)
        (Id name)         -> "Id " ++ name
  in replicate n ' ' ++ suf
  where s = showExpr (n+1)

exprToString :: Expr -> String
exprToString ex = case ex of
  (UDF n a e)       -> n ++ "("++ intercalate ", " a ++ ")" ++ " = " ++ exprToString e
  (UDO n p a e)     -> n ++ "("++ show p ++ ", " ++ show (if a == L then 0 :: Double else 1) ++ ")" ++ " = " ++  exprToString e
  (Asgn i e)        -> i ++ " = " ++ exprToString e
  (Number x )       -> show x
  (Par e)           -> "(" ++ exprToString e ++ ")"
  (UMinus e)        -> "(-" ++ exprToString e ++ ")"
  (OpCall op e1 e2) -> "(" ++ exprToString e1 ++ op ++ exprToString e2 ++ ")"
  (FunCall n e)     -> n ++ "(" ++ intercalate ", " (map exprToString e) ++ ")"
  (Id s)            -> s

unTOp :: Token -> String
unTOp (TOp op) = op
unTOp _ = error "Not a TOp"
