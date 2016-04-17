{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Calculator.Types (Expr(..), Token(..), Assoc(..), exprToString, unTOp, preprocess, ListTuple, showRational) where

import Data.List (intercalate)
import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Data.Ratio

data Token = TNumber Rational
           | TLPar
           | TRPar
           | TIdent String
           | TOp String
           | TComma
           | TEnd
           deriving (Show, Eq, Ord)

data Assoc = L | R deriving (Show, Read, Eq, Ord, Generic,  ToJSON, FromJSON)

data Expr = Number Rational
          | Asgn String Expr
          | UDF String [String] Expr
          | UDO String Int Assoc Expr
          | OpCall String Expr Expr
          | UMinus Expr
          | Par Expr
          | FunCall String [Expr]
          | Id String
          deriving (Eq, Show, Read, Generic)

instance ToJSON Expr  

instance FromJSON Expr

type ListTuple =  ([(String, Rational)], [((String, Int), ([String], Expr))], [(String, ((Int, Assoc), Expr))])

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

exprToString :: Expr -> String
exprToString ex = case ex of
  UDF n a e       -> n ++ "("++ intercalate ", " a ++ ")" ++ " = " ++ exprToString e
  UDO n p a e     -> n ++ "("++ show p ++ ", " ++ show (if a == L then 0 :: Double else 1) ++ ")" ++ " = " ++  exprToString e
  Asgn i e        -> i ++ " = " ++ exprToString e
  Number x        -> show . (fromRational :: Rational -> Double) $ x
  Par e           -> "(" ++ exprToString e ++ ")"
  UMinus e        -> "(-" ++ exprToString e ++ ")"
  OpCall op e1 e2 -> "(" ++ exprToString e1 ++ op ++ exprToString e2 ++ ")"
  FunCall n e     -> n ++ "(" ++ intercalate ", " (map exprToString e) ++ ")"
  Id s            -> s

unTOp :: Token -> String
unTOp (TOp op) = op
unTOp _ = error "Not a TOp"

preprocess :: Expr -> Expr
preprocess ex = simplify' ex ex
  where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr :: Expr -> Expr
simplifyExpr ex = case ex of
  Asgn s e                              -> Asgn s (simplifyExpr e)
  UDF n s e                             -> UDF n s (simplifyExpr e)
  UDO n p a e                           -> UDO n p a (simplifyExpr e)
  Par (Par e)                           -> Par (simplifyExpr e)
  Par (UMinus e)                        -> UMinus (simplifyExpr e)
  Par e                                 -> Par (simplifyExpr e)
  UMinus (Par (UMinus e))               -> Par (simplifyExpr e)
  UMinus (OpCall "^" e1 e2)             -> OpCall "^" (UMinus (simplifyExpr e1)) (simplifyExpr e2)
  UMinus e                              -> UMinus (simplifyExpr e)
  OpCall "-" (Number 0.0) (OpCall op e1 e2) | op `elem` ["+","-"] ->
    OpCall op (simplifyExpr . UMinus $ e1) (simplifyExpr e2)
  OpCall "-" (Number 0.0) n             -> UMinus (simplifyExpr n)
  OpCall "+" (Number 0.0) n             -> simplifyExpr n
  OpCall op n (Number 0.0) | op `elem` ["+","-"] -> simplifyExpr n
  OpCall "*" (Number 1.0) n             -> simplifyExpr n
  OpCall op n (Number 1.0) | op `elem` ["*","/", "%"] -> simplifyExpr n
  OpCall "^" n (Number 1.0)             -> simplifyExpr n
  OpCall "^" (FunCall "sqrt" [e]) (Number 2.0) -> simplifyExpr e
  OpCall op e1 e2                       -> OpCall op (simplifyExpr e1) (simplifyExpr e2)
  FunCall "exp" [FunCall "log" [e]]     -> simplifyExpr e
  FunCall "log" [FunCall "exp" [e]]     -> simplifyExpr e
  FunCall "sqrt" [OpCall "^" e (Number 2.0)] -> simplifyExpr e
  FunCall name e                        -> FunCall name (map simplifyExpr e)
  x                                     -> x

showRational :: Rational -> String
showRational r = if denominator r == 1 then show $ numerator r else show (fromRational r :: Double)