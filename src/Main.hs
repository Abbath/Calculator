module Main where

import Data.Char (isSpace, isAlpha, isDigit)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (isPrefixOf, find)
import Data.Map (Map)
import qualified Data.Map as M

data Operator = Assign | Plus | Minus | Mult | Div | Mod | Power deriving (Show, Eq)

data Function = Sin | Cos | Tan | Asin | Acos | Atan | Log | Exp | Sqrt deriving (Show, Eq)

data Token = TNumber Double
           | TOp Operator
           | TLPar
           | TRPar
           | TFun Function
           | TIdent String
           | TEnd
           deriving (Show, Eq)

data Expr = Number Double
          | Asgn String Expr
          | Sum Operator Expr Expr
          | Prod Operator Expr Expr
          | Pow Expr Expr
          | Fun Function Expr
          | UMinus Expr
          | Par Expr
          | Id String
          deriving Eq

operator :: Char -> Operator
operator c = fromJust $ lookup c ops
    where ops = [('=',Assign), ('+',Plus), ('-',Minus), ('*',Mult), ('/',Div), ('%',Mod), ('^',Power)]

checkOps :: [Token] -> [Token]
checkOps t = if snd . foldl f (TEnd, True) $ t
             then t
             else error "Two operators in a row"
             where f (old, res) new  = (new, res && not (isOp new && old == new))
                   isOp (TOp Assign) = False
                   isOp (TOp _) = True
                   isOp _ = False

function :: String -> Function
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

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(x:xs)
    |x `elem` "+-*/%^=" = TOp (operator x) : tokenize xs
    |x == '(' = TLPar : tokenize xs
    |x == ')' = TRPar : tokenize xs
    |isSpace x = tokenize xs
    |tryFun s = let (f, rest) = readFun s in TFun f : tokenize rest
    |tryIdentifier s = let (i, rest) = readIdentifier s in TIdent i : tokenize rest
    |tryNumber s = let (n, rest) = readNumber s in TNumber n : tokenize rest
    |otherwise = error $ "Cannot tokenize " ++ s
    where
        tryIdentifier (x:xs) = isAlpha x || x == '_'
        readIdentifier = break (\x -> not (isAlpha x || isDigit x || (x == '_')))
        tryFun s = any (`isPrefixOf` s) funs
        readFun s =
            let ss = fromJust $ find (`isPrefixOf` s) funs
                rest = drop (length ss - 1) s
            in (function . init $ ss, rest)
        tryNumber s = let x = (reads :: String -> [(Double, String)]) s
                      in not . null $ x
        readNumber s = let [(x,y)] = (reads :: String -> [(Double, String)]) s
                       in (x,y)

funs :: [String]
funs = ["sin(", "cos(", "asin(", "acos(", "tan(", "atan(", "log(", "exp(", "sqrt("]

instance Show Expr where
    show = showExpr 0

showExpr :: Int -> Expr -> String
showExpr n e =
    let suf = case e of
            (Asgn i e)      -> "Assign " ++ i ++ "\n" ++ s e
            (Sum op e1 e2)  -> "Sum " ++ show op ++ "\n" ++ s e1 ++ "\n" ++ s e2
            (Prod op e1 e2) -> "Prod " ++ show op ++ "\n" ++ s e1 ++ "\n" ++ s e2
            (Pow e1 e2)     -> "Pow \n" ++ s e1 ++ "\n" ++ s e2
            (Number x )     -> "Number " ++ show x
            (Par e)         -> "Par \n" ++ s e
            (UMinus e)      -> "UMinus \n" ++ s e
            (Fun f e)       -> "Fun " ++ show f ++ "\n" ++ s e
            (Id s)          -> "Id " ++ s
    in replicate n ' ' ++ suf
    where s = showExpr (n+1)

preprocess :: Expr -> Expr
preprocess e = simplify' e e
    where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr :: Expr -> Expr
simplifyExpr e = case e of
    (Asgn s e)                              -> Asgn s (simplifyExpr e)
    (Par (Par e))                           -> Par (simplifyExpr e)
    (Par e)                                 -> Par (simplifyExpr e)
    (UMinus (Par (UMinus e)))               -> Par (simplifyExpr e)
    --(UMinus (Pow e1 e2))                    -> Pow (UMinus (simplifyExpr e1)) (simplifyExpr e2)
    (UMinus e)                              -> UMinus (simplifyExpr e)
    (Sum Minus (Number 0.0) (Sum op e1 e2)) -> Sum op (simplifyExpr . UMinus $ e1) (simplifyExpr e2)
    (Sum Minus (Number 0.0) n)              -> UMinus (simplifyExpr n)
    (Sum Plus (Number 0.0) n)               -> simplifyExpr n
    (Sum _ n (Number 0.0))                  -> simplifyExpr n
    (Prod Mult (Number 1.0) n)              -> simplifyExpr n
    (Prod _ n (Number 1.0))                 -> simplifyExpr n
    (Pow n (Number 1.0))                    -> simplifyExpr n
    (Sum op e1 e2)                          -> Sum op (simplifyExpr e1) (simplifyExpr e2)
    (Prod op e1 e2)                         -> Prod op (simplifyExpr e1) (simplifyExpr e2)
    (Pow (Fun Sqrt e) (Number 2.0))         -> simplifyExpr e
    (Pow e1 e2)                             -> Pow (simplifyExpr e1) (simplifyExpr e2)
    (Fun Exp (Fun Log e))                   -> simplifyExpr e
    (Fun Log (Fun Exp e))                   -> simplifyExpr e
    (Fun Asin (Fun Sin e))                  -> simplifyExpr e
    (Fun Acos (Fun Cos e))                  -> simplifyExpr e
    (Fun Atan (Fun Tan e))                  -> simplifyExpr e
    (Fun Sqrt (Pow e (Number 2.0)))         -> simplifyExpr e
    (Fun f e)                               -> Fun f (simplifyExpr e)
    x                                       -> x

eval :: Map String Double -> Expr -> (Double, Map String Double)
eval m e = case e of
   (Asgn s _) | s `elem` ["pi","e"] -> error $ "Can not change constant value " ++ s
   (Asgn s e)                       -> let (r,_) = eval m e in (r, M.insert s r m)
   (Id s)                           -> (fromMaybe (error "No such variable!") (M.lookup s m :: Maybe Double),m)
   (Number x)                       -> (x,m)
   (Sum Plus x y)                   -> eval' (+) x y
   (Sum Minus x (Sum op y z))       -> eval m $ Sum op (Sum Minus x y) z
   (Sum Minus x y)                  -> eval' (-) x y
   (Prod Mult x y)                  -> eval' (*) x y
   (Prod Div x (Prod op y z))       -> eval m $ Prod op (Prod Div x y) z
   (Prod Mod x (Prod op y z))       -> eval m $ Prod op (Prod Mod x y) z
   (Prod Div x y) -> let (n,_) = eval m y
                    in if n == 0 then error "Div by zero" else (fst (eval m x) / n, m)
   (Prod Mod x y) -> let (n,_) = eval m y
                    in if n == 0
                    then error "Div by zero"
                    else (fromIntegral $ mod (floor . fst $ eval m x) (floor n), m)
   (Pow x y)                    -> eval' (**) x y
   (UMinus (Pow x y))           -> eval m $ Pow (UMinus x) y
   (UMinus x)                   -> let (n,_) = eval m x in (-n,m)
   (Par e)                      -> eval m e
   (Fun Atan (Prod Div e1 e2))  -> eval' atan2 e1 e2
   (Fun f e)                    -> ((fromMaybe id $ lookup f fns) (fst $ eval m e),m)
   where
    fns = [(Sin,sin), (Cos,cos), (Tan,tan), (Asin,asin), (Acos,acos), (Atan, atan), (Log,log), (Exp,exp), (Sqrt,sqrt)]
    eval' f x y =
        let (t1,_) = eval m x
            (t2,_) = eval m y
        in (f t1 t2, m)

parseAssign :: [Token] -> Expr
parseAssign (TIdent s : TOp Assign : rest) = Asgn s (parseExpr rest)
parseAssign s = parseExpr s

parseExpr :: [Token] -> Expr
parseExpr s = let (s1, s2) = breakPar (`elem` [TOp Plus, TOp Minus]) s
                  e1 = if null s1 then Number 0 else parseTerm s1
                  op = if null s2 then Plus else (\(TOp op) -> op) $ tryHead "Missing second term" s2
                  e2 = if null s2 then Number 0 else parseExpr . tail $ s2
              in Sum op e1 e2

parseTerm :: [Token] -> Expr
parseTerm s = let (s1, s2) = breakPar (`elem` [TOp Mult, TOp Div, TOp Mod]) s
                  e1 = parsePow s1
                  op = if null s2 then Mult else (\(TOp op) -> op) $ tryHead "Missing second factor" s2
                  e2 = if null s2 then Number 1 else parseExpr . tail $ s2
              in Prod op e1 e2

parsePow :: [Token] -> Expr
parsePow s = let (s1, s2) = breakPar (`elem` [TOp Power]) s
                 e1 = parseToken s1
                 e2 = if null s2 then Number 1 else parseExpr . tail $ s2
             in if (not . null $ s2) && (null . tail $ s2)
                then error "Missing exponent"
                else Pow e1 e2

parseToken :: [Token] -> Expr
parseToken s = case s of
    [] -> error "Syntax error"
    [TIdent s]  -> Id s
    [TNumber n] -> Number n
    (TFun f: TLPar : rest) -> Fun f $ ps rest
    (TLPar : rest) -> Par $ ps rest
    x -> error $ "Syntax error!" ++ show x
    where ps = parseExpr . init . fst . takePar

takePar :: [Token] -> ([Token], [Token])
takePar = takePar' 1 [] where
    takePar' 0 acc s = (reverse acc, s)
    takePar' n acc [] = error $ "Parentheses mismatch!" ++ show n
    takePar' n acc (x:xs) = case x of
        TRPar -> tp (n-1)
        TLPar -> tp (n+1)
        _     -> tp n
        where tp m = takePar' m (x:acc) xs

breakPar :: (Token -> Bool) -> [Token] -> ([Token], [Token])
breakPar _ []           = ([], [])
breakPar p xs@(x:xs')
           | x == TLPar = let (a, b) = takePar xs'
                              (y, z) = breakPar p b
                          in ([x] ++ a ++ y, z)
           | p x        = ([],xs)
           | otherwise  = let (ys,zs) = breakPar p xs' in (x:ys,zs)

tryHead :: String -> [a] -> a
tryHead s l = if null l then error s else head l

parse :: [Token] -> Expr
parse = preprocess . parseAssign . checkOps

loop :: Map String Double -> IO()
loop m = do
    putStr "> " >> hFlush stdout
    x <- getLine
    if not (null x)
    then do
        let y = tokenize x
        let z = parse y
        let (res, m1) = eval m z
        print y
        print z
        print res
        loop m1
    else do
        putStrLn "Empty!"
        loop m

defVar :: Map String Double
defVar = M.fromList [("pi",pi), ("e",exp 1)]

main :: IO ()
main = loop defVar
