module Main where

import Data.Char (isSpace, isAlpha, isDigit)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (isPrefixOf, find, intercalate, concatMap)
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

checkEither inp f = do
    t <- inp
    return $ f t

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
    |x == ',' = f TComma xs
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

substitute :: ([String], [Expr]) -> Expr -> Either String Expr
substitute ([],[]) e = return e
substitute (x,y) _ | length x /= length y = Left "Bad argument number"
substitute (x:xs, y:ys) (Id i) = if i == x then return y else substitute (xs, ys) (Id i)
substitute s ex = case ex of
    (Sum op e1 e2) -> wrk e1 e2 $ Sum op
    (Prod op e1 e2) -> wrk e1 e2 $ Prod op
    (Pow e1 e2) -> wrk e1 e2 Pow
    (Par e) -> do { ss <- st e ; return $ Par ss}
    (Fun f e) -> do { ss <- st e ; return $ Fun f ss}
    (UMinus e) -> do { ss <- st e ; return $ UMinus ss}
    e -> return e
    where st = substitute s
          wrk e1 e2 ret = do { s1 <- st e1; s2 <- st e2; return $ ret s1 s2}

localize :: [String] -> Expr -> Expr
localize [] e = e
localize (x:xs) (Id i) = if i == x then Id ('$':i) else localize xs (Id i)
localize s ex = case ex of
    (Sum op e1 e2) -> Sum op (st e1) (st e2)
    (Prod op e1 e2) -> Prod op (st e1) (st e2)
    (Pow e1 e2) -> Pow (st e1) (st e2)
    (Par e) -> Par (st e)
    (Fun f e) -> Fun f (st e)
    (UMinus e) -> UMinus (st e)
    e -> e
    where st = localize s

preprocess :: Expr -> Expr
preprocess e = simplify' e e
    where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr :: Expr -> Expr
simplifyExpr e = case e of
    (Asgn s e)                              -> Asgn s (simplifyExpr e)
    (UDF n s e)                             -> UDF n s (simplifyExpr e)
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
    (FunCall name e)                        -> FunCall name (map simplifyExpr e)
    x                                       -> x

eval :: (Map String Double, Map String ([String],Expr)) -> Expr -> Either String (Double, Map String Double, Map String ([String],Expr))
eval (m,m1) e = case e of
   (Asgn s _) | s `elem` ["pi","e","_"] -> Left $ "Can not change constant value " ++ s
   (Asgn s e)                       -> do {(r,_,_) <- eval (m,m1) e; return (r, M.insert s r m, m1)}
   (UDF n s e)                      -> return (fromIntegral $ M.size m1,m, M.insert n (map ((:) '$') s, localize s e) m1)
   (FunCall name e)                 ->
        case (M.lookup name m1 :: Maybe ([String],Expr)) of
            Just (al, expr) -> do
                expr1 <- substitute (al, e) expr
                (a,_,_) <- eval (m,m1) expr1
                return (a, m, m1)
            Nothing -> Left $ "No such function " ++ name
   (Id s)                           -> mte ("No such variable! " ++ s) (M.lookup s m :: Maybe Double,m,m1)
   (Number x)                       -> return (x,m,m1)
   (Sum Plus x y)                   -> eval' (+) x y
   (Sum Minus x (Sum op y z))       -> eval (m,m1) $ Sum op (Sum Minus x y) z
   (Sum Minus x y)                  -> eval' (-) x y
   (Prod Mult x y)                  -> eval' (*) x y
   (Prod Div x (Prod op y z))       -> eval (m,m1) $ Prod op (Prod Div x y) z
   (Prod Mod x (Prod op y z))       -> eval (m,m1) $ Prod op (Prod Mod x y) z
   (Prod Div x y) -> do (n,_,_) <- eval (m,m1) y
                        (n1,_,_) <- eval (m,m1) x
                        if n == 0 then Left "Div by zero" else return (n1 / n, m,m1)
   (Prod Mod x y) -> do (n,_,_) <- eval (m,m1) y
                        (n1,_,_) <- eval (m,m1) x
                        if n == 0
                        then Left "Div by zero"
                        else return (fromIntegral $ mod (floor n1) (floor n), m,m1)
   (Pow x y)                    -> eval' (**) x y
   (UMinus (Pow x y))           -> eval (m,m1) $ Pow (UMinus x) y
   (UMinus x)                   -> do {(n,_,_) <- eval (m,m1) x; return (-n,m,m1)}
   (Par e)                      -> eval (m,m1) e
   (Fun Atan (Prod Div e1 e2))  -> eval' atan2 e1 e2
   (Fun f e)                    -> do
        (n,_,_) <- eval (m,m1) e
        return ((fromMaybe id $ lookup f fns) n,m,m1)
   where
    mte _ (Just x, m,m1) = Right (x,m,m1)
    mte s (Nothing,_,_) = Left s
    fns = [(Sin,sin), (Cos,cos), (Tan,tan), (Asin,asin), (Acos,acos), (Atan, atan), (Log,log), (Exp,exp), (Sqrt,sqrt)]
    eval' f x y = do
        (t1,_,_) <- eval (m,m1) x
        (t2,_,_) <- eval (m,m1) y
        return (f t1 t2, m, m1)

parseAssign :: [Token] -> Either String Expr
parseAssign x@(TIdent _ : TLPar : TIdent _ : _ ) = do
    (a,b) <- breakPar (== TOp Assign) x
    if null b then parseExpr x
    else do
        (name, args) <- parseFunDec a
        ex <- parseExpr (tail b)
        return $ UDF name args ex
parseAssign (TIdent s : TOp Assign : rest) = checkEither (parseExpr rest) (Asgn s)
parseAssign s = parseExpr s

parseFunDec :: [Token] -> Either String (String, [String])
parseFunDec [TIdent name, TLPar, TIdent a, TRPar] = return (name, [a])
parseFunDec (TIdent name : TLPar : TIdent a : TComma : rest) = do
    x <- parseFunDec' rest
    return (name, a:x)
    where parseFunDec' y = case y of
            [TIdent _] -> Left "Missing bracket!"
            [TIdent _, TComma] -> Left "Missing bracket!"
            [TIdent n, TRPar] -> return [n]
            (TIdent n : TComma : rest) -> do
                ni <- parseFunDec' rest
                return (n:ni)
            x -> Left $ "Bad parse " ++ show x

parseExpr :: [Token] -> Either String Expr
parseExpr s = do
    t <- breakPar (`elem` [TOp Plus, TOp Minus]) s
    let (s1, s2) = t
    let e1 = if null s1 then Right $ Number 0 else parseTerm s1
    let op = if null s2 then Right Plus else checkEither (tryHead "Missing second term" s2) (\(TOp op) -> op)
    let e2 = if null s2 then Right $ Number 0 else parseExpr . tail $ s2
    a <- e1
    b <- op
    c <- e2
    return $ Sum b a c

parseTerm :: [Token] -> Either String Expr
parseTerm s = do
    t <- breakPar (`elem` [TOp Mult, TOp Div, TOp Mod]) s
    let (s1, s2) = t
    e1 <- parsePow s1
    let op = if null s2 then Right Mult else checkEither (tryHead "Missing second factor" s2) (\(TOp op) -> op)
    let e2 = if null s2 then Right $ Number 1 else parseExpr . tail $ s2
    a <- op
    b <- e2
    return $ Prod a e1 b

parsePow :: [Token] -> Either String Expr
parsePow s = do
    t <- breakPar (`elem` [TOp Power]) s
    let (s1, s2) = t
    e1 <- parseToken s1
    let e2 = if null s2 then Right $ Number 1 else parseExpr . tail $ s2
    if (not . null $ s2) && (null . tail $ s2)
    then Left "Missing exponent"
    else do
        a <- e2
        return $ Pow e1 a

parseToken :: [Token] -> Either String Expr
parseToken s = case s of
    [] -> Left "Syntax error"
    [TIdent s]  -> Right $ Id s
    (TIdent name : TLPar : rest) -> do { x <- parseFuncall rest; return $ FunCall name x}
    [TNumber n] -> Right $ Number n
    (TFun f: TLPar : rest) -> checkEither (ps rest) (Fun f)
    (TLPar : rest) -> checkEither (ps rest) Par
    x -> Left $ "Syntax error! " ++ show x
    where ps ss = let t = takePar ss
                  in case t of
                    Left err -> Left err
                    Right r -> parseExpr . init . fst $ r

parseFuncall :: [Token] -> Either String [Expr]
parseFuncall [TRPar] = return []
parseFuncall s = do
    (s1, s2) <- breakPar (== TComma) s
    if null s2
    then do
        e <- if length s1 > 1 && last s1 == TRPar then parseExpr (init s1) else parseExpr s1
        return [e]
    else do
        e <- parseExpr s1
        t <- parseFuncall (tail s2)
        return (e : t)

takePar :: [Token] -> Either String ([Token], [Token])
takePar = takePar' 1 [] where
    takePar' 0 acc s = Right (reverse acc, s)
    takePar' n acc [] = Left $ "Parentheses mismatch! " ++ show n
    takePar' n acc (x:xs) = case x of
        TRPar -> tp (n-1)
        TLPar -> tp (n+1)
        _     -> tp n
        where tp m = takePar' m (x:acc) xs


breakPar :: (Token -> Bool) -> [Token] -> Either String ([Token], [Token])
breakPar _ []           = Right ([], [])
breakPar p xs@(x:xs')
           | x == TLPar = do
            (a,b) <- takePar xs'
            (y,z) <- breakPar p b
            return ([x] ++ a ++ y, z)
           | p x        = Right ([],xs)
           | otherwise  = checkEither (breakPar p xs') (first ((:) x))

tryHead :: String -> [Token] -> Either String Token
tryHead s l = if length l < 2 then Left s else Right $ head l

parse :: [Token] -> Either String Expr
parse s = checkEither (checkOps s >>= parseAssign) preprocess

loop :: (Map String Double,Map String ([String],Expr)) -> IO()
loop (m,mm) = do
    putStr "> " >> hFlush stdout
    x <- getLine
    if not (null x)
    then do
        let t = tokenize x >>= parse >>= eval (m, mm)
        case t of
           Left err -> do
            putStrLn err
            loop (m,mm)
           Right (r,m1,m2) -> do
            print r
            loop (M.insert "_" r m1,m2)
    else do
        putStrLn "Empty!"
        loop (m,mm)

defVar :: Map String Double
defVar = M.fromList [("pi",pi), ("e",exp 1), ("_", 0.0)]

main :: IO ()
main = loop (defVar, M.fromList [])
