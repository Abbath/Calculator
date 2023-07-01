{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Calculator.Types (Expr (..), Token (..), Assoc (..), Op (..), ExecOp (..), FunOp (..), getPrA,
                         exprToString, unTOp, isOp, preprocess, ListTuple, Fun (..), ExecFn (..), FunFun (..),
                         showRational, showT, opSymbols, Maps, OpMap, VarMap, FunMap, opsFromList, opsToList,
                         funsFromList, funsToList) where

import Control.Arrow (second)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data Token = TNumber Rational
           | TLPar
           | TRPar
           | TIdent Text
           | TFIdent Text
           | TOp Text
           | TComma
           | TEqual
           | TMinus
           | TEnd
           | TLet
           | TFun
           deriving (Show, Eq, Ord)

data Assoc = L | R deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data Expr = Number Rational
          | Asgn Text Expr
          | UDF Text [Text] Expr
          | UDO Text Int Assoc Expr
          | Call Text [Expr]
          | UMinus Expr
          | Par Expr
          | Id Text
          deriving (Eq, Show, Read, Generic)

instance ToJSON Expr

instance FromJSON Expr

type ListTuple =  ([(Text, Rational)], [((Text, Int), ([Text], Expr))], [(Text, ((Int, Assoc), Expr))])

type FunMap = Map (Text, Int) Fun
type VarMap = Map Text Rational
type OpMap = Map Text Op
type Maps = (VarMap, FunMap, OpMap)

data FunOp = CmpOp (Rational -> Rational -> Bool) | MathOp (Rational -> Rational -> Rational) | BitOp (Integer -> Integer -> Integer )

instance Show FunOp where
  show (CmpOp _) = "CmpOp"
  show (MathOp _) = "MathOp"
  show (BitOp _) = "BitOp"

data ExecOp = NOp | ExOp Expr | FnOp FunOp | AOp Text deriving Show

isExOp :: ExecOp -> Bool
isExOp (ExOp _) = True
isExOp _ = False

unpackExOp :: ExecOp -> Expr
unpackExOp (ExOp e) = e
unpackExOp _ = error "Not an expression"

data Op = Op {
  precedence :: Int,
  associativity :: Assoc,
  oexec :: ExecOp
} deriving Show

opsToList :: OpMap -> [(Text, ((Int, Assoc), Expr))]
opsToList = map (\(k, v) -> (k, ((precedence v, associativity v), unpackExOp . oexec $ v))) . filter (\(_, v) -> isExOp . oexec $ v) . M.toList

opsFromList :: [(Text, ((Int, Assoc), Expr))] -> OpMap
opsFromList = M.fromList . map (\(k, ((p, a), e)) -> (k, Op p a (ExOp e)))

getPrA :: OpMap -> Map Text (Int, Assoc)
getPrA om = let lst = M.toList om
                ps = M.fromList $ map (second (\o -> (precedence o, associativity o))) lst
            in ps

data FunFun = CmpFn (Rational -> Rational -> Bool) | MathFn (Double -> Double) | IntFn1 (Double -> Integer) | IntFn2 (Integer -> Integer -> Integer) | BitFn (Integer -> Integer)

instance Show FunFun where
  show (CmpFn _) = "CmpFn"
  show (MathFn _) = "MathFn"
  show (IntFn1 _) = "IntFn1"
  show (IntFn2 _) = "IntFn2"
  show (BitFn _) = "BitFn"

data ExecFn = NFn | ExFn Expr | FnFn FunFun deriving Show

isExFn :: ExecFn -> Bool
isExFn (ExFn _) = True
isExFn _ = False

unpackExFn :: ExecFn -> Expr
unpackExFn (ExFn e) = e
unpackExFn _ = error "Not an expression"

data Fun = Fun {
  params :: [Text],
  fexec :: ExecFn
} deriving Show

funsToList :: FunMap -> [((Text, Int), ([Text], Expr))]
funsToList = map (\(k, v) -> (k, (params v, unpackExFn . fexec $ v))) . filter (\(_, v) -> isExFn . fexec $ v) . M.toList

funsFromList :: [((Text, Int), ([Text], Expr))] -> FunMap
funsFromList = M.fromList . map (\(k, (p, e)) -> (k, Fun p (ExFn e)))

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

showT :: Show a => a -> Text
showT = T.pack . show

exprToString :: Expr -> Text
exprToString ex = case ex of
  UDF n a e       -> n <> "(" <> T.intercalate ", " a <> ")" <> " = " <> exprToString e
  UDO n p a e     -> n <> "(" <> showT p <> ", " <> showT (if a == L then 0 :: Double else 1) <> ")" <> " = " <>  exprToString e
  Asgn i e        -> i <> " = " <> exprToString e
  Number x        -> T.pack $ show . (fromRational :: Rational -> Double) $ x
  Par e           -> "(" <> exprToString e <> ")"
  UMinus e        -> "(-" <> exprToString e <> ")"
  Call op [e1, e2] | isOp op -> "(" <> exprToString e1 <> op <> exprToString e2 <> ")"
  Call n e     -> n <> "(" <> T.intercalate ", " (map exprToString e) <> ")"
  Id s            -> s

opSymbols :: String
opSymbols = "+-/*%^$!~&|=><:"

isOp :: Text -> Bool
isOp = T.all (`elem` opSymbols)

unTOp :: Token -> Text
unTOp (TOp op) = op
unTOp _        = error "Not a TOp"

preprocess :: Expr -> Expr
preprocess ex = simplify' ex ex
  where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr :: Expr -> Expr
simplifyExpr ex = case ex of
  Asgn s e -> Asgn s (simplifyExpr e)
  UDF n s e -> UDF n s (simplifyExpr e)
  UDO n p a e -> UDO n p a (simplifyExpr e)
  Par (Par e) -> Par (simplifyExpr e)
  Par (UMinus e) -> UMinus (simplifyExpr e)
  Par e -> Par (simplifyExpr e)
  UMinus (Par (UMinus e)) -> Par (simplifyExpr e)
  UMinus (Call op [e1, e2]) | isOp op -> Call op [UMinus (simplifyExpr e1), simplifyExpr e2]
  UMinus e -> UMinus (simplifyExpr e)
  Call "-" [Number 0.0, Call op [e1, e2]]
    | op `elem` ["+", "-"] ->
        Call op [simplifyExpr . UMinus $ e1, simplifyExpr e2]
  Call "-" [Number 0.0, n] -> UMinus (simplifyExpr n)
  Call "+" [Number 0.0, n] -> simplifyExpr n
  Call op [n, Number 0.0] | op `elem` ["+", "-"] -> simplifyExpr n
  Call "*" [Number 1.0, n] -> simplifyExpr n
  Call "*" [Number 0.0, n] -> Number 0.0
  Call op [n, Number 1.0] | op `elem` ["*", "/", "%"] -> simplifyExpr n
  Call "^" [n, Number 1.0] -> simplifyExpr n
  Call "^" [Call "sqrt" [e], Number 2.0] -> simplifyExpr e
  Call "exp" [Call "log" [e]] -> simplifyExpr e
  Call "log" [Call "exp" [e]] -> simplifyExpr e
  Call "sqrt" [Call "^" [e, Number 2.0]] -> simplifyExpr e
  Call name e -> Call name (map simplifyExpr e)
  x -> x

showRational :: Rational -> Text
showRational r = if denominator r == 1 then showT $ numerator r else showT (fromRational r :: Double)
