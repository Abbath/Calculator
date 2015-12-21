module Calculator.Evaluator (eval, getPriorities, FunMap, VarMap, OpMap, Maps) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Calculator.Types (Expr(..), Assoc(..), exprToString)
import Control.Lens ((^.), (.~), (&), (%~), _1, _2, _3)
import Data.AEq ((~==))

type FunMap = Map (String, Int) ([String], Expr)
type VarMap = Map String Double
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
localize s@(x:xs) (FunCall nm e) = do
  t <- mapM (localize s) e
  if nm == x
  then return $ FunCall ('@':nm) t
  else localize xs (FunCall nm t)
localize s ex = goInside (localize s) ex

catchVar :: VarMap -> Expr -> Either String Expr
catchVar m ex = case ex of
  (Id i@('@':_)) -> return $ Id i
  (Id i) ->
    case M.lookup i m :: Maybe Double of
      Just n -> return $ Number n
      Nothing -> Left $ "No such variable: " ++ i
  e -> goInside st e
  where st = catchVar m

compFuns :: Map String (Double -> Double -> Bool)
compFuns = M.fromList [("lt",(<)), ("gt",(>)), ("eq",(~==))
  ,("ne", \x y -> not $ x ~== y ), ("le",(<=)), ("ge",(>=))]

compOps :: Map String (Double -> Double -> Bool)
compOps = M.fromList [("<",(<)), (">",(>)), ("==",(~==))
  ,("!=", \x y -> not $ x ~== y ), ("<=",(<=)), (">=",(>=))]

mathFuns :: Map String (Double -> Double)
mathFuns = M.fromList [("sin",sin), ("cos",cos), ("asin",asin), ("acos",acos), ("tan",tan), ("atan",atan)
        ,("log",log), ("exp",exp), ("sqrt",sqrt), ("abs",abs)]

fmod :: Double -> Double -> Double
fmod x y = fromInteger $ mod (floor x) (floor y)

mathOps :: Map String (Double -> Double -> Double)
mathOps = M.fromList [("+",(+)), ("-",(-)), ("*",(*)), ("/",(/)), ("%",fmod), ("^",(**))]

getPriorities :: OpMap -> Map String Int
getPriorities om = let lst = M.toList om
                       ps = M.fromList $ map (\(s,((p,_),_)) -> (s,p)) lst
                   in ps

eval :: Maps -> Expr -> Either String (Double, Maps)
eval maps ex = case ex of
  (Asgn s _) | s `elem` ["pi","e","_"] -> Left $ "Can not change constant value: " ++ s
  (Asgn s e)                           -> do {(r,_) <- evm e; return (r, maps & _1 %~ M.insert s r)}
  (UDF n s e)                          -> do
    newe <- localize s e >>= catchVar (maps^._1)
    let newmap = M.insert (n, length s) (map ('@':) s, newe) $ maps^._2
    return (fromIntegral $ M.size (maps^._2), maps & _2 .~ newmap)
  (UDO n (-1) _ e@(OpCall op _ _)) ->
    case M.lookup op (maps^._3) of
      Just ((p, a), _) -> do
        let newmap = M.insert n ((p, a), e) (maps^._3)
        return (fromIntegral $ M.size (maps^._3), maps & _3 .~ newmap)
      Nothing -> Left $ "No such operator: " ++ op
  (UDO n p a e )
    | M.member n mathOps || M.member n compOps || n == "=" -> Left $ "Can not redefine embedded operator: " ++ n
    | p < 1 || p > 4 ->  Left $ "Bad priority: " ++ show p
    |otherwise -> do
        newe <- localize ["x","y"] e >>= catchVar (maps^._1)
        let newmap = M.insert n ((p, a), newe) (maps^._3)
        return (fromIntegral $ M.size (maps^._3), maps & _3 .~ newmap)
  (FunCall "atan" [OpCall "/" e1 e2])       -> eval' atan2 e1 e2
  (FunCall name [a])   | M.member name mathFuns -> do
    let fun = mathFuns M.! name
    (n,_) <- evm a
    return $ mps $ fun n
  (FunCall n [a,b]) | M.member n compFuns -> cmp n a b compFuns
  (FunCall "if" [a,b,c]) -> do
    (cond,_) <- evm a
    if cond /= 0
    then evm b
    else evm c
  (FunCall name e) ->
    case (M.lookup (name, length e) (maps^._2) :: Maybe ([String],Expr)) of
      Just (al, expr) -> do
        expr1 <- substitute (al, e) expr
        (a,_) <- evm expr1
        return $ mps a
      Nothing -> case name of
        ('@':r) -> Left $ "Expression instead of function name: " ++ r ++ "/" ++ show (length e)
        _ -> Left $ "No such function: " ++ name ++ "/" ++ show (length e)
  (Id s)     -> mte ("No such variable: " ++ s) $ mps (M.lookup s (maps^._1) :: Maybe Double)
  (Number x) -> return $ mps x
  (OpCall op1 x s@(OpCall op2 y z)) -> do
    let pr = getPriorities (maps^._3)
    let a = M.lookup op1 pr
    let b = M.lookup op2 pr
    if a == b
    then case a of
      Nothing -> Left $ "No such operators: " ++ op1 ++ " " ++ op2
      Just _ -> do
        let ((_, asc1), _) = (maps^._3) M.! op1
        let ((_, asc2), _) = (maps^._3) M.! op2
        case (asc1, asc2) of
          (L, L) -> evm $ OpCall op2 (OpCall op1 x y) z
          (R, R) -> do
            (tmp,_) <- evm s
            evm $ OpCall op1 x (Number tmp)
          _ -> Left $ "Operators with different associativity: " ++ op1 ++ " and " ++ op2
    else do
      (tmp,_) <- evm s
      evm $ OpCall op1 x (Number tmp)
  oc@(OpCall "/" x y) -> do
    (n,_) <- evm y
    (n1,_) <- evm x
    if n ~== 0 then Left $ "Div by zero: " ++ exprToString oc else return $ mps (n1 / n)
  oc@(OpCall "%" x y) -> do
    (n,_) <- evm y
    (n1,_) <- evm x
    if n ~== 0
    then Left $ "Div by zero: " ++ exprToString oc
    else return $ mps (fromInteger $ mod (floor n1) (floor n))
  (OpCall op x y) | M.member op compOps -> cmp op x y compOps
  (OpCall op x y) | M.member op mathOps -> eval' ( mathOps M.! op) x y
  (OpCall op x y)  ->
    case (M.lookup op (maps^._3) :: Maybe ((Int, Assoc),Expr)) of
      Just (_, expr) -> do
        expr1 <- substitute (["@x", "@y"], [x,y]) expr
        (a,_) <- evm expr1
        return $ mps a
      Nothing -> case op of
        ('@':r) -> Left $ "Expression instead of function name: " ++ r ++ "/2"
        _ -> Left $ "No such operator: " ++ op ++ "/2"
  (UMinus (OpCall "^" x y)) -> evm $ OpCall "^" (UMinus x) y
  (UMinus x)         -> do {(n,_) <- evm x; return $ mps (-n)}
  (Par e)            -> evm e
  where
    mte _ (Just x, m) = Right (x, m)
    mte s (Nothing, _) = Left s
    evm = eval maps
    mps x = (x, maps)
    cmp op x y mp = do
      let fun = mp M.! op
      (n,_) <- evm x
      (n1,_) <- evm y
      return $ if fun n n1
        then mps 1
        else mps 0
    eval' f x y = do
      (t1,_) <- eval maps x
      (t2,_) <- eval maps y
      return (f t1 t2, maps)