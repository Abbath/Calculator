module Calculator.Parser (parse) where

import Calculator.Types
import Control.Arrow (first)
import Data.Map (Map)
import qualified Data.Map as M

checkOps :: [Token] -> Either String [Token]
checkOps t = if snd . foldl f (TEnd, True) $ t
  then Right t
  else Left "Two operators in a row"
  where
    f (old, res) new  = (new, res && not (isOp new && old == new))
    isOp (TStrOp "=") = False
    isOp (TStrOp _) = True
    isOp _ = False

priorities :: Map Token Int
priorities = M.fromList [(TStrOp "=", 0), (TStrOp "==", 1), (TStrOp "<=", 1), (TStrOp ">=", 1), (TStrOp "/=", 1)
  ,(TStrOp "<", 1), (TStrOp ">", 1), (TStrOp "+", 2), (TStrOp "-", 2), (TStrOp "*", 3), (TStrOp "/", 3), (TStrOp "%", 3)
  ,(TStrOp "^", 4)]

takeWithPriorities :: Int -> [Token]
takeWithPriorities n = map fst . M.toList $ M.filter (== n) priorities

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

parseOp :: Int -> [Token] -> Either String Expr
parseOp 0 x@(TIdent _ : TLPar : TIdent _ : _ ) = do
            (a,b) <- breakPar (== TStrOp "=") x
            if null b then parseOp 1 x
            else do
              (name, args) <- parseFunDec a
              UDF name args <$> parseOp 1 (tail b)
parseOp 0 (TIdent s : TStrOp "=" : rest) = Asgn s <$> parseOp 1 rest
parseOp 0 s = parseOp 1 s
parseOp 1 s = do
  t <- breakPar (`elem` takeWithPriorities 1) s
  let (s1, s2) = t
  let e1 = if null s1 then Left "Missing first arg" else parseOp 2 s1
  if null s2
  then e1
  else do
    let op = (\(TStrOp op) -> op) <$> tryHead "Missing second arg" s2
    let e2 = parseOp 1 . tail $ s2
    OpCall <$> op <*> e1 <*> e2
parseOp 2 s = do
  t <- breakPar (`elem` takeWithPriorities 2) s
  let (s1, s2) = t
  let e1 = if null s1 then Right $ Number 0 else parseOp 3 s1
  let op = if null s2 then Right "+" else (\(TStrOp op) -> op) <$> tryHead "Missing second term" s2
  let e2 = if null s2 then Right $ Number 0 else parseOp 1 . tail $ s2
  OpCall <$> op <*> e1 <*> e2
parseOp 3 s = do
  t <- breakPar (`elem` takeWithPriorities 3) s
  let (s1, s2) = t
  let e1 = if null s1 then Left "Missing first arg" else parseOp 4 s1
  let op = if null s2 then Right "*" else (\(TStrOp op) -> op) <$> tryHead "Missing second arg" s2
  let e2 = if null s2 then Right $ Number 1 else parseOp 1 . tail $ s2
  OpCall <$> op <*> e1 <*> e2
parseOp 4 s = do
  t <- breakPar (`elem` takeWithPriorities 4) s
  let (s1, s2) = t
  let e1 = if null s1 then Left "Missing first arg" else parseToken s1
  let op = if null s2 then Right "^" else (\(TStrOp op) -> op) <$> tryHead "Missing second arg" s2
  let e2 = if null s2 then Right $ Number 1 else parseOp 1 . tail $ s2
  OpCall <$> op <*> e1 <*> e2
parseOp n _ = Left $ "Bad priority" ++ show n

parseFunDec :: [Token] -> Either String (String, [String])
parseFunDec [TIdent name, TLPar, TIdent a, TRPar] = return (name, [a])
parseFunDec (TIdent name : TLPar : TIdent a : TComma : rest) = do
  x <- parseFunDec' rest
  return (name, a:x)
  where
  parseFunDec' y = case y of
    [TIdent _] -> Left "Missing bracket"
    [TIdent _, TComma] -> Left "Missing bracket"
    [TIdent n, TRPar] -> return [n]
    (TIdent n : TComma : rest) -> (n:) <$> parseFunDec' rest
    x -> Left $ "Bad parse: " ++ show x

parseToken :: [Token] -> Either String Expr
parseToken s = case s of
  [] -> Left "Syntax error"
  [TIdent s]  -> Right $ Id s
  (TIdent name : TLPar : rest) -> FunCall name <$> parseFuncall rest
  [TNumber n] -> Right $ Number n
  (TLPar : rest) -> Par <$> ps rest
  x -> Left $ "Syntax error: " ++ show x
  where
    ps ss = let t = takePar ss
     in case t of
       Left err -> Left err
       Right r -> parseOp 1 . init . fst $ r

parseFuncall :: [Token] -> Either String [Expr]
parseFuncall [TRPar] = return []
parseFuncall s = do
  (s1, s2) <- breakPar (== TComma) s
  if s2 == [TComma, TRPar] || null s1 then Left "Missing function argument"
  else
    if null s2
    then (:[]) <$> if length s1 > 1 && last s1 == TRPar then parseOp 1 (init s1) else parseOp 1 s1
    else (:) <$> parseOp 1 s1 <*> parseFuncall (tail s2)

takePar :: [Token] -> Either String ([Token], [Token])
takePar = takePar' 1 [] where
  takePar' 0 acc s = Right (reverse acc, s)
  takePar' n acc [] = Left $ "Parentheses mismatch: " ++ show n
  takePar' n acc (x:xs) = case x of
    TRPar -> tp (n-1)
    TLPar -> tp (n+1)
    _     -> tp n
    where tp m = takePar' m (x:acc) xs

breakPar :: (Token -> Bool) -> [Token] -> Either String ([Token], [Token])
breakPar _ []  = Right ([], [])
breakPar p xs@(x:xs')
  | x == TLPar = do
   (a,b) <- takePar xs'
   (y,z) <- breakPar p b
   return ([x] ++ a ++ y, z)
  | p x        = Right ([],xs)
  | otherwise  = first (x:) <$> breakPar p xs'

tryHead :: String -> [Token] -> Either String Token
tryHead s l = if length l < 2 then Left s else Right $ head l

parse :: [Token] -> Either String Expr
parse s = preprocess <$> (checkOps s >>= parseOp 0)