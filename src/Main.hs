module Main where

import Data.Char (isSpace, isAlpha, isDigit)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (isPrefixOf, find)
import Data.Map (Map)
import Control.Arrow (first)
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

checkEither inp f = let t = inp
                    in case t of
                        Left err -> Left err
                        Right r -> Right (f r)

operator :: Char -> Operator
operator c = fromJust $ lookup c ops
    where ops = [('=',Assign), ('+',Plus), ('-',Minus), ('*',Mult), ('/',Div), ('%',Mod), ('^',Power)]

checkOps :: [Token] -> Either String [Token]
checkOps t = if snd . foldl f (TEnd, True) $ t
             then Right t
             else Left "Two operators in a row"
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

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize s@(x:xs)
    |x `elem` "+-*/%^=" = f (TOp (operator x)) xs
    |x == '(' = f TLPar xs
    |x == ')' = f TRPar xs
    |isSpace x = tokenize xs
    |tryFun s = let (fun, rest) = readFun s in f (TFun fun) rest
    |tryIdentifier s = let (i, rest) = readIdentifier s in f (TIdent i)  rest
    |tryNumber s = let (n, rest) = readNumber s in f (TNumber n) rest
    |otherwise = Left $ "Cannot tokenize " ++ s
    where
        f out inp = checkEither (tokenize inp) ((:) out)
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
    (UMinus (Pow e1 e2))                    -> Pow (UMinus (simplifyExpr e1)) (simplifyExpr e2)
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
   (Asgn s _) | s `elem` ["pi","e","_"] -> error $ "Can not change constant value " ++ s
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

parseAssign :: [Token] -> Either String Expr
parseAssign (TIdent s : TOp Assign : rest) = checkEither (parseExpr rest) (Asgn s)
parseAssign s = parseExpr s

parseExpr :: [Token] -> Either String Expr
parseExpr s = let t = breakPar (`elem` [TOp Plus, TOp Minus]) s
              in case t of
                Left err -> Left err
                Right r ->
                    let (s1, s2) = r
                        e1 = if null s1 then Right $ Number 0 else parseTerm s1
                        op = if null s2 then Right Plus else case tryHead "Missing second term" s2 of
                          Left err -> Left err
                          Right o -> Right $ (\(TOp op) -> op) o
                        e2 = if null s2 then Right $ Number 0 else parseExpr . tail $ s2
                    in case (e1, op, e2) of
                      (Left err, _, _) -> Left err
                      (_, Left err, _) -> Left err
                      (_, _, Left err) -> Left err
                      (Right a, Right b, Right c) -> Right $ Sum b a c

parseTerm :: [Token] -> Either String Expr
parseTerm s = let t = breakPar (`elem` [TOp Mult, TOp Div, TOp Mod]) s
              in case t of
                Left err -> Left err
                Right r ->
                    let (s1, s2) = r
                        e1 = parsePow s1
                        op = if null s2 then Right Mult else case tryHead "Missing second factor" s2 of
                          Left err -> Left err
                          Right o -> Right $ (\(TOp op) -> op) o
                        e2 = if null s2 then Right $ Number 1 else parseExpr . tail $ s2
                    in case (e1, op, e2) of
                      (Left err, _, _) -> Left err
                      (_, Left err, _) -> Left err
                      (_, _, Left err) -> Left err
                      (Right a, Right b, Right c) -> Right $ Prod b a c

parsePow :: [Token] -> Either String Expr
parsePow s = let t = breakPar (`elem` [TOp Power]) s
             in case t of
                Left err -> Left err
                Right r ->
                    let (s1, s2) = r
                        e1 = parseToken s1
                        e2 = if null s2 then Right $ Number 1 else parseExpr . tail $ s2
                    in if (not . null $ s2) && (null . tail $ s2)
                       then Left "Missing exponent"
                       else case (e1, e2) of
                           (Left err, _) -> Left err
                           (_, Left err) -> Left err
                           (Right a, Right b) -> Right $ Pow a b

parseToken :: [Token] -> Either String Expr
parseToken s = case s of
    [] -> Left "Syntax error"
    [TIdent s]  -> Right $ Id s
    [TNumber n] -> Right $ Number n
    (TFun f: TLPar : rest) -> checkEither (ps rest) (Fun f)
    (TLPar : rest) -> checkEither (ps rest) Par
    x -> Left $ "Syntax error!" ++ show x
    where ps ss = let t = takePar ss
                  in case t of
                    Left err -> Left err
                    Right r -> parseExpr . init . fst $ r

takePar :: [Token] -> Either String ([Token], [Token])
takePar = takePar' 1 [] where
    takePar' 0 acc s = Right (reverse acc, s)
    takePar' n acc [] = Left $ "Parentheses mismatch!" ++ show n
    takePar' n acc (x:xs) = case x of
        TRPar -> tp (n-1)
        TLPar -> tp (n+1)
        _     -> tp n
        where tp m = takePar' m (x:acc) xs


breakPar :: (Token -> Bool) -> [Token] -> Either String ([Token], [Token])
breakPar _ []           = Right ([], [])
breakPar p xs@(x:xs')
           | x == TLPar = let t = takePar xs'
                          in case t of
                            Left err -> t
                            Right r -> let tt = breakPar p b
                                           (a,b) = r
                                       in case tt of
                                            Left err -> t
                                            Right rr -> let (y, z) = rr
                                                        in Right ([x] ++ a ++ y, z)
           | p x        = Right ([],xs)
           | otherwise  = checkEither (breakPar p xs') (first ((:) x))

tryHead :: String -> [Token] -> Either String Token
tryHead s l = if length l < 2 then Left s else Right $ head l

parse :: [Token] -> Either String Expr
parse s = checkEither (checkOps s >>= parseAssign) preprocess

loop :: Map String Double -> IO()
loop m = do
    putStr "> " >> hFlush stdout
    x <- getLine
    if not (null x)
    then do
        let t = tokenize x >>= parse
        case t of
           Left err -> do
            putStrLn err
            loop m
           Right r -> do
            let (res, m1) = eval m r
            print res
            loop (M.insert "_" res m1)
    else do
        putStrLn "Empty!"
        loop m

defVar :: Map String Double
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_", 0.0)]

main :: IO ()
main = loop defVar
