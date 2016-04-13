module Calculator.Evaluator (eval, getPriorities, FunMap, VarMap, OpMap, Maps) where

import           Calculator.Types (Assoc (..), Expr (..), exprToString,
                                   preprocess)
import           Control.Lens     ((%~), (&), (.~), (^.), _1, _2, _3)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M
import           Data.Ratio

type FunMap = Map (String, Int) ([String], Expr)
type VarMap = Map String Rational
type OpMap = Map String ((Int, Assoc), Expr)
type Maps = (VarMap, FunMap, OpMap)

goInside :: (Expr -> Either String Expr) -> Expr -> Either String Expr
goInside f ex = case ex of
  (OpCall op e1 e2) -> OpCall op <$> f e1 <*> f e2
  (Par e) -> Par <$> f e
  (UMinus e) -> UMinus <$> f e
  (FunCall n e) -> FunCall n <$> mapM f e
  e -> return e

substitute :: ([String], [Expr]) -> Expr -> Either String Expr
substitute ([],[]) e = return e
substitute (x,y) _ | length x /= length y =
  Left $ "Bad argument number: " ++ show(length y) ++ " instead of " ++ show(length x)
substitute (x:xs, y:ys) (Id i) = if i == x then return $ Par y else substitute (xs, ys) (Id i)
substitute s@(x:xs, Id fname:ys) (FunCall n e) = if n == x
  then FunCall fname <$> mapM (substitute s) e
  else do
    t <- mapM (substitute s) e
    substitute (xs, ys) (FunCall n t)
substitute s@(_:xs, _:ys) (FunCall n e) = do
  t <- mapM (substitute s) e
  substitute (xs, ys) (FunCall n t)
substitute s ex = goInside (substitute s) ex

localize :: [String] -> Expr -> Either String Expr
localize [] e = return e
localize (x:xs) (Id i) = if i == x then return $ Id ('@':i) else localize xs (Id i)
localize s@(x:xs) (FunCall nm e) = if nm == x
  then FunCall ('@':nm) <$> mapM (localize s) e
  else do
    t <- mapM (localize s) e
    localize xs (FunCall nm t)
localize s ex = goInside (localize s) ex

catchVar :: VarMap -> Expr -> Either String Expr
catchVar m ex = case ex of
  (Id i@('@':_)) -> return $ Id i
  (Id i) ->
    case M.lookup i m :: Maybe Rational of
      Just n -> return $ Number (n)
      Nothing -> Left $ "No such variable: " ++ i
  e -> goInside st e
  where st = catchVar m

compFuns :: Map String (Rational -> Rational -> Bool)
compFuns = M.fromList [("lt",(<)), ("gt",(>)), ("eq",(==))
  ,("ne",(/=)), ("le",(<=)), ("ge",(>=))]

compOps :: Map String (Rational -> Rational -> Bool)
compOps = M.fromList [("<",(<)), (">",(>)), ("==",(==))
  ,("!=",(/=)), ("<=",(<=)), (">=",(>=))]

mathFuns :: Map String (Double -> Double)
mathFuns = M.fromList [("sin",sin), ("cos",cos), ("asin",asin), ("acos",acos), ("tan",tan), ("atan",atan)
        ,("log",log), ("exp",exp), ("sqrt",sqrt), ("abs",abs)]
        
intFuns :: Map String (Integer -> Integer -> Integer)
intFuns = M.fromList [("gcd", gcd), ("lcm", lcm), ("div", div), ("mod", mod), ("quot", quot), ("rem", rem)] 

fmod :: Rational -> Rational -> Rational
fmod x y = fromInteger $ mod (floor x) (floor y)

mathOps :: Map String (Rational -> Rational -> Rational)
mathOps = M.fromList [("+",(+)), ("-",(-)), ("*",(*)), ("/",(/)), ("%",fmod), ("^",pow)]

pow :: Rational -> Rational -> Rational
pow a b | denominator a == 1 && denominator b == 1 = toRational $ numerator a ^ numerator b
pow a b = toRational $ (fromRational a :: Double) ** (fromRational b :: Double)

getPriorities :: OpMap -> Map String Int
getPriorities om = let lst = M.toList om
                       ps = M.fromList $ map (\(s,((p,_),_)) -> (s,p)) lst
                   in ps

eval :: Maps -> Expr -> Either (String, Maps) (Rational, Maps)
eval maps ex = case ex of
  Asgn s _ | s `elem` ["pi","e","_"] -> msgmap maps $ "Can not change constant value: " ++ s
  Asgn s e                           -> do 
    (r,_) <- evm e
    Left ("Constant " ++ s ++ "=" ++ (if denominator r == 1 then show $ numerator r else show (fromRational r :: Double)), maps & _1 %~ M.insert s r)
  UDF n [s] (FunCall "df" [e, Id x]) | s == x -> do
    let de = derivative e (Id x)
    case de of
      Left err -> Left . mps $ err
      Right r -> evm (UDF n [s] r)
  UDF n s e                          -> do
    let newe = localize s e >>= catchVar (maps^._1)
    case newe of 
      Left err -> Left . mps $ err 
      Right r -> do 
        let newmap = M.insert (n, length s) (map ('@':) s, r) $ maps^._2
        Left ("Function " ++ n ++ "/" ++ show (length s), maps & _2 .~ newmap)
  UDO n (-1) _ e@(OpCall op _ _) ->
    case M.lookup op (maps^._3) of
      Just ((p, a), _) -> do
        let newmap = M.insert n ((p, a), e) (maps^._3)
        Left ("Operator alias " ++ n ++ " = " ++ op, maps & _3 .~ newmap)
      Nothing -> Left . mps $ "No such operator: " ++ op
  UDO n p a e
    | M.member n mathOps || M.member n compOps || n == "=" -> Left . mps $ "Can not redefine embedded operator: " ++ n
    | p < 1 || p > 4 ->  Left . mps $ "Bad priority: " ++ show p
    |otherwise -> do
        let t = localize ["x","y"] e >>= catchVar (maps^._1)
        case t of 
          Left err -> Left . mps $ err
          Right r -> do 
            let newmap = M.insert n ((p, a), r) (maps^._3)
            Left ("Operator " ++ n ++ " p=" ++ show p ++ " a=" ++ (if a == L then "left" else "right"), maps & _3 .~ newmap)
  FunCall "df" [a,x] -> do 
      let e = derivative a x
      case e of 
        Left err -> Left . mps $ err 
        Right r ->  Left . mps . exprToString . preprocess $ r
  FunCall "atan" [OpCall "/" e1 e2] -> do
    (t1,_) <- evm e1
    (t2,_) <- evm e2
    return (toRational $ atan2 (fromRational t1 :: Double) (fromRational t2 :: Double), maps)
  FunCall "prat" [e] -> do
    (t1,_) <- evm e
    Left . mps $ show (numerator t1) ++ " / " ++ show (denominator t1)
  FunCall f [e1, e2] | M.member f intFuns -> evalInt f e1 e2 
  FunCall name [a]   | M.member name mathFuns -> do
    let fun = mathFuns M.! name
    (n,_) <- evm a
    return $ mps $ toRational . (\x -> if abs x <= sin pi then 0 else x) . fun . fromRational $ n
  FunCall n [a,b] | M.member n compFuns -> cmp n a b compFuns
  FunCall "if" [a,b,c] -> do
    (cond,_) <- evm a
    if cond /= 0
    then evm b
    else evm c
  FunCall name e ->
    case (M.lookup (name, length e) (maps^._2) :: Maybe ([String],Expr)) of
      Just (al, expr) -> do
        let expr1 = substitute (al, e) expr
        case expr1 of 
          Right r -> do 
            (a,_) <- evm r
            return $ mps a
          Left err -> Left . mps $ err  
      Nothing -> case name of
        ('@':r) -> Left . mps $ "Expression instead of function name: " ++ r ++ "/" ++ show (length e)
        _ -> Left . mps $ "No such function: " ++ name ++ "/" ++ show (length e)
  Id s     -> mte ("No such variable: " ++ s) $ mps (M.lookup s (maps^._1) :: Maybe Rational)
  Number x -> return $ mps x
  OpCall op1 x s@(OpCall op2 y z) -> do
    let pr = getPriorities (maps^._3)
    let a = M.lookup op1 pr
    let b = M.lookup op2 pr
    if a == b
    then case a of
      Nothing -> Left . mps $ "No such operators: " ++ op1 ++ " " ++ op2
      Just _ -> do
        let ((_, asc1), _) = (maps^._3) M.! op1
        let ((_, asc2), _) = (maps^._3) M.! op2
        case (asc1, asc2) of
          (L, L) -> evm $ OpCall op2 (OpCall op1 x y) z
          (R, R) -> do
            (tmp,_) <- evm s
            evm $ OpCall op1 x (Number tmp)
          _ -> Left . mps $ "Operators with different associativity: " ++ op1 ++ " and " ++ op2
    else do
      (tmp,_) <- evm s
      evm $ OpCall op1 x (Number tmp)
  oc@(OpCall "/" x y) -> do
    (n,_) <- evm y
    (n1,_) <- evm x
    if n == 0 then Left . mps $ "Div by zero: " ++ exprToString oc else return $ mps (n1 / n)
  oc@(OpCall "%" x y) -> do
    (n,_) <- evm y
    (n1,_) <- evm x
    if n == 0
    then Left . mps $ "Div by zero: " ++ exprToString oc
    else return $ mps (fromInteger $ mod (floor n1) (floor n))
  OpCall op x y | M.member op compOps -> cmp op x y compOps
  OpCall op x y | M.member op mathOps -> eval' ( mathOps M.! op) op x y
  OpCall op x y  ->
    case (M.lookup op (maps^._3) :: Maybe ((Int, Assoc),Expr)) of
      Just (_, expr) -> do
        let expr1 = substitute (["@x", "@y"], [x,y]) expr
        case expr1 of 
         Right e -> do 
           (a,_) <- evm e
           return $ mps a
         Left err -> Left . mps $ err 
      Nothing -> case op of
        ('@':r) -> Left . mps $ "Expression instead of function name: " ++ r ++ "/2"
        _ -> Left . mps $ "No such operator: " ++ op ++ "/2"
  UMinus (OpCall "^" x y) -> evm $ OpCall "^" (UMinus x) y
  UMinus x         -> do {(n,_) <- evm x; return $ mps (-n)}
  Par e            -> evm e
  where
    mte _ (Just x, m) = Right (x, m)
    mte s (Nothing, _) = Left . mps $ s
    evm x = do 
      (r,m) <- eval maps x
      if r > (2^(8000000 :: Integer) :: Rational)
         then Left . mps $ "Too much!"
         else return (r,m)
    mps x = (x, maps)
    cmp op x y mp = do
      let fun = mp M.! op
      (n,_) <- evm x
      (n1,_) <- evm y
      return $ if fun n n1
        then mps 1
        else mps 0
    eval' :: (Rational -> Rational -> Rational) -> String -> Expr -> Expr -> Either (String, Maps) (Rational, Maps)
    eval' f op x y = do
      (t1,_) <- evm x
      (t2,_) <- evm y
      if ( op == "^" && (fromRational t1 :: Double) * logBase 10 (fromRational t2 :: Double) > 2408240) || f t1 t2 > (2^(8000000 :: Integer) :: Rational)
         then Left . mps $ "Too much!"
         else return (f t1 t2, maps)
    evalInt f x y = do
      (t1,_) <- evm x
      (t2,_) <- evm y
      if denominator t1 == 1 && denominator t2 == 1
         then return $ mps (toRational $ (intFuns M.! f) (numerator t1) (numerator t2))
         else Left $ mps "Cannot use integral function on real numbers!"
    msgmap m s = Left (s, m)     

derivative :: Expr -> Expr -> Either String Expr
derivative e x = case e of
  Par ex -> Par <$> (derivative ex x)
  UMinus ex -> UMinus <$> (derivative ex x)
  Number _ -> return $ Number 0
  i@(Id _) | i == x -> return $ Number 1
  (Id _) -> return $ Number 0
  OpCall "^" i (Number n) | i == x->
    return $ OpCall "*" (Number n) (OpCall "^" i (Number (n-1)))
  OpCall "^" (Number a) i | i == x -> return $ OpCall "*" e (FunCall "log" [Number a])
  OpCall op ex1 ex2 | op == "-" || op == "+" -> OpCall op <$> derivative ex1 x <*> derivative ex2 x
  OpCall "*" ex1 ex2 -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    return $ OpCall "+" (OpCall "*" d1 ex2) (OpCall "*" d2 ex1)
  OpCall "/" ex1 ex2 -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    return $ OpCall "/" (OpCall "-" (OpCall "*" d1 ex2) (OpCall "*" d2 ex1)) (OpCall "^" ex2 (Number 2))
  ex@(FunCall "exp" [i]) | i == x -> return ex
  FunCall "log" [i] | i == x -> return $ OpCall "/" (Number 1) i
  FunCall "sin" [i] | i == x -> return $ FunCall "cos" [i]
  FunCall "cos" [i] | i == x -> return $ UMinus (FunCall "sin" [i])
  FunCall "tan" [i] | i == x ->
    return $ OpCall "/" (Number 1) (OpCall "^" (FunCall "cos" [i]) (Number 2))
  ex@(FunCall _ [i]) -> OpCall "*" <$> derivative ex i <*> derivative i x
  _ -> Left "No such derivative"
