module Main where

import Data.Char
import System.IO
import qualified Text.Read as R
import Data.Maybe

data Expr = Number Double
            | Sum Char Expr Expr
            | Mul Char Expr Expr
            | UMinus Expr
            | Par Expr
            deriving Eq

instance Show Expr where
    show = showExpr 0

showExpr n (Sum op e1 e2) = replicate n ' ' ++ "Sum " ++ [op] ++ "\n" ++ showExpr (n+1) e1 ++ "\n" ++ showExpr (n+1) e2
showExpr n (Mul op e1 e2) = replicate n ' ' ++ "Mul " ++ [op] ++ "\n" ++ showExpr (n+1) e1 ++ "\n" ++ showExpr (n+1) e2
showExpr n (Number x ) =  replicate n ' ' ++ "Number " ++ show x
showExpr n (Par e) = replicate n ' ' ++ "Par \n" ++ showExpr (n+1) e
showExpr n (UMinus e) =  replicate n ' ' ++ "UMinus \n" ++ showExpr (n+1) e

simplify e = simplify' e e
    where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr (Par e) = Par (simplifyExpr e)
simplifyExpr (UMinus e) = UMinus (simplifyExpr e)
simplifyExpr (Sum '+' (Number 0.0) n) = simplifyExpr n
simplifyExpr (Sum _ n (Number 0.0)) = simplifyExpr n
simplifyExpr (Mul '*' (Number 1.0) n) = simplifyExpr n
simplifyExpr (Mul _ n (Number 1.0)) = simplifyExpr n
simplifyExpr (Sum op e1 e2) = Sum op (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr (Mul op e1 e2) = Mul op (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr x = x

eval :: Expr -> Double
eval (Number x) = x
eval (Sum '+' x y) = eval x + eval y
eval (Sum '-' x y) = eval x - eval y
eval (Mul '*' x y) = eval x * eval y
eval (Mul '/' x (Mul op y z)) = let n = eval y
                                    w = if n == 0 then error "Div by zero" else eval x / eval y
                                in eval (Mul op (Number w) z)
eval (Mul '/' x y) = let n = eval y
                     in if n == 0 then error "Div by zero" else eval x / n
eval (Mul '%' x y) = let n = eval y
                     in if n == 0 then error "Div by zero" else fromIntegral $ mod (floor . eval $ x) (floor n)
eval (UMinus x) = -(eval x)
eval (Par e) = eval e

parseExpr s = let b = head s == '-'
                  ss = if b then tail s else s
                  (s1, s2) = breakPar (`elem` "+-") ss
                  e1 = parseTerm s1
                  op = if null s2 then '+' else head s2
                  e2 = if null s2 then Number 0 else case op of
                     '+' -> parseExpr . tail $ s2
                     '-' -> parseExpr . ('-':) . tail $ s2
              in Sum '+' (if b then UMinus e1 else e1) e2

parseTerm ('-':xs) = UMinus (parseTerm xs)
parseTerm s = let (s1, s2) = breakPar (`elem` "*/%") s
                  e1 = parseNumber s1
                  op = if null s2 then '*' else head s2
                  e2 = if null s2 then Number 1 else parseExpr . tail $ s2
              in  Mul op e1 e2

checkNumber s = let x = (reads :: String -> [(Double, String)]) s
                in not (null x) && ("" == (snd . head $ x))

parseNumber s |checkNumber s =
                    let x = fst . head . (reads :: String -> [(Double, String)]) $ s
                    in Number x
              |head s == '(' =
                    let ss = init . fst . takePar . tail $ s
                    in Par (parseExpr ss)
              |otherwise = parseExpr s

takePar s = takePar' 1 s []

takePar' n [] acc = if n == 0 then (reverse acc,[]) else error "Wrong!"
takePar' n (x:xs) acc = if n == 0
                       then (reverse acc, x:xs)
                       else case x of
                                    ')' -> takePar' (n-1) xs (x:acc)
                                    '(' -> takePar' (n+1) xs (x:acc)
                                    _ -> takePar' n xs (x:acc)

breakPar _ xs@[]           =  (xs, xs)
breakPar p xs@(x:xs')
           | x == '('   = let (a, b) = takePar xs'
                              (y, z) = breakPar p b
                          in ([x] ++ a ++ y, z)
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = breakPar p xs' in (x:ys,zs)

main = do
    putStr "> "
    hFlush stdout
    x <- getLine
    let y =  simplify $ parseExpr $ filter (not . isSpace) x
    print y
    print $ eval y
    main