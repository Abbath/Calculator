{-# LANGUAGE FlexibleContexts #-}

module Calculator.Parser (parse) where

import Calculator.Types
import Control.Arrow (first)
import Data.Map.Strict (Map)
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map.Strict as M

checkOps :: [Token] -> Either String [Token]
checkOps t = if snd . foldl f (TEnd, True) $ t
  then Right t
  else Left "Two operators in a row"
  where
    f (old, res) new  = (new, res && not (isOp new && old == new))
    isOp (TOp "=") = False
    isOp (TOp _) = True
    isOp _ = False

priorities :: Map String Int
priorities = M.fromList [("=", 0), ("==", 1), ("<=", 1), (">=", 1), ("/=", 1)
  ,("<", 1), (">", 1), ("+", 2), ("-", 2), ("*", 3), ("/", 3), ("%", 3)
  ,("^", 4)]

takeWithPriorities :: Int -> Map String Int -> [Token]
takeWithPriorities n m = map (TOp . fst) . M.toList $ M.filter (== n) (M.union priorities m)

preprocess :: Expr -> Expr
preprocess e = simplify' e e
  where simplify' e o = if simplifyExpr e == o then o else simplify' (simplifyExpr e) e

simplifyExpr :: Expr -> Expr
simplifyExpr e = case e of
  (Asgn s e)                              -> Asgn s (simplifyExpr e)
  (UDF n s e)                             -> UDF n s (simplifyExpr e)
  (UDO n p a e)                           -> UDO n p a (simplifyExpr e)
  (Par (Par e))                           -> Par (simplifyExpr e)
  (Par e)                                 -> Par (simplifyExpr e)
  (UMinus (Par (UMinus e)))               -> Par (simplifyExpr e)
  (UMinus (OpCall "^" e1 e2))             -> OpCall "^" (UMinus (simplifyExpr e1)) (simplifyExpr e2)
  (UMinus e)                              -> UMinus (simplifyExpr e)
  (OpCall "-" (Number 0.0) (OpCall op e1 e2)) | op `elem` ["+","-"] ->
    OpCall op (simplifyExpr . UMinus $ e1) (simplifyExpr e2)
  (OpCall "-" (Number 0.0) n)             -> UMinus (simplifyExpr n)
  (OpCall "+" (Number 0.0) n)             -> simplifyExpr n
  (OpCall op n (Number 0.0)) | op `elem` ["+","-"] -> simplifyExpr n
  (OpCall "*" (Number 1.0) n)             -> simplifyExpr n
  (OpCall op n (Number 1.0)) | op `elem` ["*","/", "%"] -> simplifyExpr n
  (OpCall "^" n (Number 1.0))             -> simplifyExpr n
  (OpCall "^" (FunCall "sqrt" [e]) (Number 2.0)) -> simplifyExpr e
  (OpCall op e1 e2)                       -> OpCall op (simplifyExpr e1) (simplifyExpr e2)
  (FunCall "exp" [FunCall "log" [e]])     -> simplifyExpr e
  (FunCall "log" [FunCall "exp" [e]])     -> simplifyExpr e
  (FunCall "sqrt" [OpCall "^" e (Number 2.0)]) -> simplifyExpr e
  (FunCall name e)                        -> FunCall name (map simplifyExpr e)
  x                                       -> x

type ParseReader = ReaderT (Map String Int) (Except String) Expr

parseOp :: Int -> [Token] -> ParseReader
parseOp 0 x@(TOp op : TLPar : TNumber p : TComma : TNumber a : TRPar : TOp "=" : _) = do
  (_,s2) <- lift $ breakPar (== TOp "=") x
  if length s2 == 1  then throwError "Empty operator definition"
  else UDO op (floor p) (if a == 0 then L else R) <$> parseOp 1 (tail s2)
parseOp 0 x@(TIdent _ : TLPar : TIdent _ : _ ) = do
  (a,b) <- lift $ breakPar (== TOp "=") x
  if null b then parseOp 1 x
  else do
    (name, args) <- lift $ parseFunDec a
    UDF name args <$> parseOp 1 (tail b)
parseOp 0 (TIdent s : TOp "=" : rest) = Asgn s <$> parseOp 1 rest
parseOp 0 s = parseOp 1 s
parseOp 1 s = do
  m <- ask
  t <- lift $ breakPar (`elem` takeWithPriorities 1 m) s
  let (s1, s2) = t
  let e1 = if null s1 then throwError "Missing first arg" else parseOp 2 s1
  if null s2
  then e1
  else do
    let op = lift $ (\(TOp op) -> op) <$> tryHead "Missing second arg" s2
    let e2 = parseOp 1 . tail $ s2
    OpCall <$> op <*> e1 <*> e2
parseOp 2 s = do
  m <- ask
  t <- lift $ breakPar (`elem` takeWithPriorities 2 m) s
  let (s1, s2) = t
  let e1 = if null s1 then return $ Number 0 else parseOp 3 s1
  let op = if null s2 then return "+" else lift $ (\(TOp op) -> op) <$> tryHead "Missing second arg" s2
  let e2 = if null s2 then return $ Number 0 else parseOp 1 . tail $ s2
  OpCall <$> op <*> e1 <*> e2
parseOp 3 s = do
  m <- ask
  t <- lift $ breakPar (`elem` takeWithPriorities 3 m) s
  let (s1, s2) = t
  let e1 = if null s1 then throwError "Missing first arg" else parseOp 4 s1
  let op = if null s2 then return "*" else lift $ (\(TOp op) -> op) <$> tryHead "Missing second arg" s2
  let e2 = if null s2 then return $ Number 1 else parseOp 1 . tail $ s2
  OpCall <$> op <*> e1 <*> e2
parseOp 4 s = do
  m <- ask
  t <- lift $ breakPar (`elem` takeWithPriorities 4 m) s
  let (s1, s2) = t
  let e1 = if null s1 then throwError "Missing first arg" else parseToken s1
  let op = if null s2 then return "^" else lift $ (\(TOp op) -> op) <$> tryHead "Missing second arg" s2
  let e2 = if null s2 then return $ Number 1 else parseOp 1 . tail $ s2
  OpCall <$> op <*> e1 <*> e2
parseOp n _ = throwError $ "Bad priority" ++ show n

parseFunDec :: [Token] -> Except String (String, [String])
parseFunDec [TIdent name, TLPar, TIdent a, TRPar] = return (name, [a])
parseFunDec (TIdent name : TLPar : TIdent a : TComma : rest) = do
  x <- parseFunDec' rest
  return (name, a:x)
  where
  parseFunDec' y = case y of
    [TIdent _] -> throwError "Missing bracket"
    [TIdent _, TComma] -> throwError "Missing bracket"
    [TIdent n, TRPar] -> return [n]
    (TIdent n : TComma : rest) -> (n:) <$> parseFunDec' rest
    x -> throwError $ "Bad parse: " ++ show x

parseToken :: [Token] -> ParseReader
parseToken s = case s of
  [] -> throwError "Syntax error"
  [TIdent s]  -> return $ Id s
  (TIdent name : TLPar : rest) -> FunCall name <$> parseFuncall rest
  [TNumber n] -> return $ Number n
  (TLPar : rest) -> Par <$> ps rest
  x -> throwError $ "Syntax error: " ++ show x
  where
    ps ss = let t = runExcept $ takePar ss
     in case t of
       Left err -> throwError err
       Right r -> parseOp 1 . init . fst $ r

parseFuncall ::  [Token] -> ReaderT (Map String Int) (Except String) [Expr]
parseFuncall [TRPar] = return []
parseFuncall s = do
  (s1, s2) <- lift $ breakPar (== TComma) s
  if s2 == [TComma, TRPar] || null s1 then throwError "Missing function argument"
  else
    if null s2
    then (:[]) <$> if length s1 > 1 && last s1 == TRPar then parseOp 1 (init s1) else parseOp 1 s1
    else (:) <$> parseOp 1 s1 <*> parseFuncall (tail s2)

takePar :: [Token] -> Except String ([Token], [Token])
takePar = takePar' 1 [] where
  takePar' 0 acc s = return (reverse acc, s)
  takePar' n acc [] = throwError $ "Parentheses mismatch: " ++ show n
  takePar' n acc (x:xs) = case x of
    TRPar -> tp (n-1)
    TLPar -> tp (n+1)
    _     -> tp n
    where tp m = takePar' m (x:acc) xs

breakPar :: (Token -> Bool) -> [Token] -> Except String ([Token], [Token])
breakPar _ []  = return ([], [])
breakPar p xs@(x:xs')
  | x == TLPar = do
   (a,b) <- takePar xs'
   (y,z) <- breakPar p b
   return ([x] ++ a ++ y, z)
  | p x        = return ([],xs)
  | otherwise  = first (x:) <$> breakPar p xs'

tryHead :: String -> [Token] -> Except String Token
tryHead s l = if length l < 2 then throwError s else return $ head l

parse :: Map String Int -> [Token] -> Either String Expr
parse m s = do
  s1 <- checkOps s
  preprocess <$> runExcept (runReaderT (parseOp 0 s1) m)