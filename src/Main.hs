module Main where

import Data.Char
import System.IO
import qualified Text.Read as R
import Data.Maybe

data Expr = Number Double
            | Sum Char Expr Expr
            | Mul Char Expr Expr
            | Pow Expr Expr
            | UMinus Expr
            | Par Expr
            deriving Eq

instance Show Expr where
    show = showExpr 0

showExpr n (Sum op e1 e2) = replicate n ' ' ++ "Sum " ++ [op] ++ "\n" ++ showExpr (n+1) e1 ++ "\n" ++ showExpr (n+1) e2
showExpr n (Mul op e1 e2) = replicate n ' ' ++ "Mul " ++ [op] ++ "\n" ++ showExpr (n+1) e1 ++ "\n" ++ showExpr (n+1) e2
showExpr n (Pow e1 e2) = replicate n ' ' ++ "Pow \n" ++ showExpr (n+1) e1 ++ "\n" ++ showExpr (n+1) e2
showExpr n (Number x ) =  replicate n ' ' ++ "Number " ++ show x
showExpr n (Par e) = replicate n ' ' ++ "Par \n" ++ showExpr (n+1) e
showExpr n (UMinus e) =  replicate n ' ' ++ "UMinus \n" ++ showExpr (n+1) e

simplify e = simplify' e e
    where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr e = case e of
    (Par e)                     -> Par (simplifyExpr e)
    (UMinus (Pow e1 e2))        -> Pow (UMinus (simplifyExpr e1)) (simplifyExpr e2)
    (UMinus e)                  -> UMinus (simplifyExpr e)
    (Sum '+' (Number 0.0) n)    -> simplifyExpr n
    (Sum _ n (Number 0.0))      -> simplifyExpr n
    (Mul '*' (Number 1.0) n)    -> simplifyExpr n
    (Mul _ n (Number 1.0))      -> simplifyExpr n
    (Pow n (Number 1.0))        -> simplifyExpr n
    (Sum op e1 e2)              -> Sum op (simplifyExpr e1) (simplifyExpr e2)
    (Mul op e1 e2)              -> Mul op (simplifyExpr e1) (simplifyExpr e2)
    (Pow e1 e2)                 -> Pow (simplifyExpr e1) (simplifyExpr e2)
    x                           -> x

eval :: Expr -> Double
eval e = case e of
   (Number x)       -> x
   (Sum '+' x y)    -> eval x + eval y
   (Sum '-' x y)    -> eval x - eval y
   (Mul '*' x y)    -> eval x * eval y
   (Mul '/' x (Mul op y z)) ->
        let n = eval y
            w = if n == 0 then error "Div by zero" else eval x / eval y
        in eval (Mul op (Number w) z)
   (Mul '%' x (Mul op y z)) ->
        let n = eval y
            w = if n == 0
                then error "Div by zero"
                else fromIntegral $ mod (floor . eval $ x) (floor n)
        in eval (Mul op (Number w) z)
   (Mul '/' x y) -> let n = eval y
                    in if n == 0 then error "Div by zero" else eval x / n
   (Mul '%' x y) -> let n = eval y
                    in if n == 0
                    then error "Div by zero"
                    else fromIntegral $ mod (floor . eval $ x) (floor n)
   (Pow x y) -> eval x ** eval y
   (UMinus x)   -> -(eval x)
   (Par e)      -> eval e

parseExpr s = let b = head s == '-'
                  ss = if b then tail s else s
                  (s1, s2) = breakPar (`elem` "+-") ss
                  e1 = parseTerm s1
                  op = if null s2 then '+' else tryHead "parExp" s2
                  e2 = if null s2 then Number 0 else case op of
                     '+' -> parseExpr . tail $ s2
                     '-' -> parseExpr . ('-':) . tail $ s2
              in Sum '+' (if b then UMinus e1 else e1) e2

parseTerm s = let (s1, s2) = breakPar (`elem` "*/%") s
                  e1 = parsePow s1
                  op = if null s2 then '*' else tryHead "parTer" s2
                  e2 = if null s2 then Number 1 else parseExpr . tail $ s2
              in  Mul op e1 e2

parsePow s = let (s1, s2) = breakPar (`elem` "^") s
                 e1 = parseNumber s1
                 e2 = if null s2 then Number 1 else parseExpr . tail $ s2
             in  Pow e1 e2

checkNumber s = let x = (reads :: String -> [(Double, String)]) s
                in not (null x) && ("" == (snd . head $ x))

parseNumber s |null s = error "Syntax error!"
              |checkNumber s =
                let x = fst . head . (reads :: String -> [(Double, String)]) $ s
                in Number x
              |tryHead "parNum" s == '(' =
                 let ss = init . fst . takePar . tail $ s
                 in Par (parseExpr ss)
              |otherwise = parseExpr s

takePar s = takePar' 1 s []

takePar' n [] acc = if n == 0 then (reverse acc,[]) else error "Parentheses mismatch!"
takePar' n (x:xs) acc = if n == 0
                       then (reverse acc, x:xs)
                       else case x of
                           ')'   -> takePar' (n-1) xs (x:acc)
                           '('   -> takePar' (n+1) xs (x:acc)
                           _     -> takePar' n xs (x:acc)

breakPar _ xs@[]           =  (xs, xs)
breakPar p xs@(x:xs')
           | x == '('   = let (a, b) = takePar xs'
                              (y, z) = breakPar p b
                          in ([x] ++ a ++ y, z)
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = breakPar p xs' in (x:ys,zs)

tryHead s l = if null l then error s else head l

parse = simplify . parseExpr . filter (not . isSpace)

main = do
    putStr "> "
    hFlush stdout
    x <- getLine
    if not (null x)
    then print $ eval . parse $ x
    else putStrLn "Empty!"
    main