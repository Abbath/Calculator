module Calculator.Parser (parse) where

import Calculator.Types
import Control.Arrow (first)

checkOps :: [Token] -> Either String [Token]
checkOps t = if snd . foldl f (TEnd, True) $ t
  then Right t
  else Left "Two operators in a row"
  where
    f (old, res) new  = (new, res && not (isOp new && old == new))
    isOp (TOp Assign) = False
    isOp (TOp _) = True
    isOp _ = False

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
  (Pow (FunCall "sqrt" [e]) (Number 2.0)) -> simplifyExpr e
  (Pow e1 e2)                             -> Pow (simplifyExpr e1) (simplifyExpr e2)
  (FunCall "exp" [FunCall "log" [e]])     -> simplifyExpr e
  (FunCall "log" [FunCall "exp" [e]])     -> simplifyExpr e
  (FunCall "sqrt" [Pow e (Number 2.0)])   -> simplifyExpr e
  (FunCall name e)                        -> FunCall name (map simplifyExpr e)
  x                                       -> x

parseAssign :: [Token] -> Either String Expr
parseAssign x@(TIdent _ : TLPar : TIdent _ : _ ) = do
  (a,b) <- breakPar (== TOp Assign) x
  if null b then parseExpr x
  else do
    (name, args) <- parseFunDec a
    UDF name args <$> parseExpr (tail b)
parseAssign (TIdent s : TOp Assign : rest) = Asgn s <$> parseExpr rest
parseAssign s = parseExpr s

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

parseExpr :: [Token] -> Either String Expr
parseExpr s = do
  t <- breakPar (`elem` [TOp Plus, TOp Minus]) s
  let (s1, s2) = t
  let e1 = if null s1 then Right $ Number 0 else parseTerm s1
  let op = if null s2 then Right Plus else (\(TOp op) -> op) <$> tryHead "Missing second term" s2
  let e2 = if null s2 then Right $ Number 0 else parseExpr . tail $ s2
  Sum <$> op <*> e1 <*> e2

parseTerm :: [Token] -> Either String Expr
parseTerm s = do
  t <- breakPar (`elem` [TOp Mult, TOp Div, TOp Mod]) s
  let (s1, s2) = t
  let e1 = parseFactor s1
  let op = if null s2 then Right Mult else (\(TOp op) -> op) <$> tryHead "Missing second factor" s2
  let e2 = if null s2 then Right $ Number 1 else parseExpr . tail $ s2
  Prod <$> op <*> e1 <*> e2

parseFactor :: [Token] -> Either String Expr
parseFactor s = do
  t <- breakPar (`elem` [TOp Power]) s
  let (s1, s2) = t
  e1 <- parseToken s1
  let e2 = if null s2 then Right $ Number 1 else parseExpr . tail $ s2
  if (not . null $ s2) && (null . tail $ s2)
  then Left "Missing exponent"
  else Pow e1 <$> e2

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
       Right r -> parseExpr . init . fst $ r

parseFuncall :: [Token] -> Either String [Expr]
parseFuncall [TRPar] = return []
parseFuncall s = do
  (s1, s2) <- breakPar (== TComma) s
  if null s2
  then (:[]) <$> if length s1 > 1 && last s1 == TRPar then parseExpr (init s1) else parseExpr s1
  else (:) <$> parseExpr s1 <*> parseFuncall (tail s2)

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
parse s = preprocess <$> (checkOps s >>= parseAssign)