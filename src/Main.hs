module Main where

import Data.Char (isSpace)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf, find)

data Operator = Plus | Minus | Mult | Div | Mod | Power deriving (Show, Eq)

data Function = Sin | Cos | Tan | Asin | Acos | Atan | Log | Exp | Sqrt deriving (Show, Eq)

data Token = TNumber Double
           | TOp Operator
           | TLPar
           | TRPar
           | TFun Function
           | TEnd
           deriving (Show, Eq)

data Expr = Number Double
          | Sum Operator Expr Expr
          | Prod Operator Expr Expr
          | Pow Expr Expr
          | Fun Function Expr
          | UMinus Expr
          | Par Expr
          deriving Eq

operator c = case c of
    '+' -> Plus
    '-' -> Minus
    '*' -> Mult
    '/' -> Div
    '%' -> Mod
    '^' -> Power

isOp (TOp _) = True
isOp _ = False

unOp (TOp op) = op

checkOps t = if snd . foldl f (TEnd, True) $ t
             then t
             else error "Two operators in a row"
             where f (old, res) new  = (new, if isOp new && old == new then res && False else res && True)

function f = case f of
    "sin" -> Sin
    "cos" -> Cos
    "tan" -> Tan
    "asin"-> Asin
    "acos"-> Acos
    "atan"-> Atan
    "log" -> Log
    "exp" -> Exp
    "sqrt"-> Sqrt
    _     -> error "Bad function name!"

tokenize [] = []
tokenize s@(x:xs)
    |x `elem` "+-*/%^" = TOp (operator x) : tokenize xs
    |x == '(' = TLPar : tokenize xs
    |x == ')' = TRPar : tokenize xs
    |isSpace x = tokenize xs
    |tryNumber s = let (n, rest) = readNumber s in TNumber n : tokenize rest
    |tryFun s = let (f, rest) = readFun s in TFun f : tokenize rest
    |otherwise = error $ "Cannot tokenize " ++ s

tryFun s = any (\x -> init x `isPrefixOf` s) funs

readFun s =
    let ss = fromMaybe "bad(" $ find (`isPrefixOf` s) funs
        rest = drop (length ss - 1) s
    in (function . init $ ss, rest)

tryNumber s = let x = (reads :: String -> [(Double, String)]) s
              in not . null $ x

readNumber s = let [(x,y)] = (reads :: String -> [(Double, String)]) s
               in (x,y)

funs = ["sin(", "cos(", "asin(", "acos(", "tan(", "atan(", "log(", "exp(", "sqrt("]

instance Show Expr where
    show = showExpr 0

showExpr n e =
    let suf = case e of
            (Sum op e1 e2)  -> "Sum " ++ show op ++ "\n" ++ s e1 ++ "\n" ++ s e2
            (Prod op e1 e2) -> "Prod " ++ show op ++ "\n" ++ s e1 ++ "\n" ++ s e2
            (Pow e1 e2)     -> "Pow \n" ++ s e1 ++ "\n" ++ s e2
            (Number x )     -> "Number " ++ show x
            (Par e)         -> "Par \n" ++ s e
            (UMinus e)      -> "UMinus \n" ++ s e
            (Fun f e)       -> "Fun " ++ show f ++ "\n" ++ s e
        pref = replicate n ' '
    in pref ++ suf
    where s = showExpr (n+1)

simplify e = simplify' e e
    where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr e = case e of
    (Par e)                         -> Par (simplifyExpr e)
    (UMinus (Pow e1 e2))            -> Pow (UMinus (simplifyExpr e1)) (simplifyExpr e2)
    (UMinus e)                      -> UMinus (simplifyExpr e)
    (Sum Minus (Number 0.0) n)      -> UMinus (simplifyExpr n)
    (Sum Plus (Number 0.0) n)       -> simplifyExpr n
    (Sum _ n (Number 0.0))          -> simplifyExpr n
    (Prod Mult (Number 1.0) n)      -> simplifyExpr n
    (Prod _ n (Number 1.0))         -> simplifyExpr n
    (Pow n (Number 1.0))            -> simplifyExpr n
    (Sum op e1 e2)                  -> Sum op (simplifyExpr e1) (simplifyExpr e2)
    (Prod op e1 e2)                 -> Prod op (simplifyExpr e1) (simplifyExpr e2)
    (Pow (Fun Sqrt e) (Number 2.0)) -> simplifyExpr e
    (Pow e1 e2)                     -> Pow (simplifyExpr e1) (simplifyExpr e2)
    (Fun Exp (Fun Log e))           -> simplifyExpr e
    (Fun Log (Fun Exp e))           -> simplifyExpr e
    (Fun Asin (Fun Sin e))          -> simplifyExpr e
    (Fun Acos (Fun Cos e))          -> simplifyExpr e
    (Fun Atan (Fun Tan e))          -> simplifyExpr e
    (Fun Sqrt (Pow e (Number 2.0))) -> simplifyExpr e
    (Fun f e)                       -> Fun f (simplifyExpr e)
    x                               -> x

eval :: Expr -> Double
eval e = case e of
   (Number x)                   -> x
   (Sum Plus x y)               -> eval x + eval y
   (Sum Minus x (Sum op y z))   -> eval $ Sum op (Sum Minus x y) z
   (Sum Minus x y)              -> eval x - eval y
   (Prod Mult x y)              -> eval x * eval y
   (Prod Div x (Prod op y z))   -> eval $ Prod op (Prod Div x y) z
   (Prod Mod x (Prod op y z))   -> eval $ Prod op (Prod Mod x y) z
   (Prod Div x y) -> let n = eval y
                    in if n == 0 then error "Div by zero" else eval x / n
   (Prod Mod x y) -> let n = eval y
                    in if n == 0
                    then error "Div by zero"
                    else fromIntegral $ mod (floor . eval $ x) (floor n)
   (Pow x y)  -> eval x ** eval y
   (UMinus x) -> -(eval x)
   (Par e)    -> eval e
   (Fun Atan (Prod Div e1 e2)) -> atan2 (eval e1) (eval e2)
   (Fun f e)  -> case f of
        Sin  -> sin (eval e)
        Cos  -> cos (eval e)
        Tan  -> tan (eval e)
        Asin -> asin (eval e)
        Acos -> acos (eval e)
        Atan -> atan (eval e)
        Log  -> log (eval e)
        Exp  -> exp (eval e)
        Sqrt -> sqrt (eval e)

parseExpr s = let (s1, s2) = breakPar (`elem` [TOp Plus, TOp Minus]) s
                  e1 = if null s1 then Number 0 else parseTerm s1
                  op = if null s2 then Plus else unOp $ tryHead "Missing second term" s2
                  e2 = if null s2 then Number 0 else parseExpr . tail $ s2
              in Sum op e1 e2

parseTerm s = let (s1, s2) = breakPar (`elem` [TOp Mult, TOp Div, TOp Mod]) s
                  e1 = parsePow s1
                  op = if null s2 then Mult else unOp $ tryHead "Missing second factor" s2
                  e2 = if null s2 then Number 1 else parseExpr . tail $ s2
              in Prod op e1 e2

parsePow s = let (s1, s2) = breakPar (`elem` [TOp Power]) s
                 e1 = parseToken s1
                 e2 = if null s2 then Number 1 else parseExpr . tail $ s2
             in if (not . null $ s2) && (null . tail $ s2)
                then error "Missing exponent"
                else Pow e1 e2

parseToken [] = error "Syntax error"
parseToken [TNumber n] = Number n
parseToken (TFun f: TLPar : rest) =
    let ss = init . fst . takePar $ rest
    in Fun f (parseExpr ss)
parseToken (TLPar : rest) =
    let ss = init . fst . takePar $ rest
    in Par (parseExpr ss)
parseToken x = error "Syntax error!"

takePar s = takePar' 1 s []

takePar' n [] acc = if n == 0 then (reverse acc,[]) else error $ "Parentheses mismatch!" ++ show n
takePar' n (x:xs) acc = if n == 0
                       then (reverse acc, x:xs)
                       else case x of
                           TRPar -> takePar' (n-1) xs (x:acc)
                           TLPar -> takePar' (n+1) xs (x:acc)
                           _     -> takePar' n xs (x:acc)

breakPar _ xs@[]        =  (xs, xs)
breakPar p xs@(x:xs')
           | x == TLPar = let (a, b) = takePar xs'
                              (y, z) = breakPar p b
                          in ([x] ++ a ++ y, z)
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = breakPar p xs' in (x:ys,zs)

tryHead s l = if null l then error s else head l

parse = simplify . parseExpr . checkOps

main = do
    putStr "> "
    hFlush stdout
    x <- getLine
    if not (null x)
    then do let y = tokenize x
            let z = parse y
            print y
            print z
            print . eval $ z
    else putStrLn "Empty!"
    main