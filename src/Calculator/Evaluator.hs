module Calculator.Evaluator (eval, FunMap, VarMap, Maps) where

import Data.Maybe (fromMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow (first, second)
import Calculator.Types (Expr(..), Operator(..))

type FunMap = Map (String, Int) ([String],Expr)
type VarMap = Map String Double
type Maps = (VarMap, FunMap)

goInside :: (Expr -> Either String Expr) -> Expr -> Either String Expr
goInside f e = case e of
  (OpCall op e1 e2) -> OpCall op <$> f e1 <*> f e2
  (Par e) -> Par <$> f e
  (UMinus e) -> UMinus <$> f e
  (FunCall n e) -> FunCall n <$> mapM f e
  e -> return e

substitute :: ([String], [Expr]) -> Expr -> Either String Expr
substitute ([],[]) e = return e
substitute (x,y) _ | length x /= length y = Left "Bad argument number"
substitute (x:xs, y:ys) (Id i) = if i == x then return $ Par y else substitute (xs, ys) (Id i)
substitute s@(x:xs, Id fname:ys) (FunCall n e) = do
  t <- mapM (substitute s) e
  if n == x
  then return $ FunCall fname t
  else substitute (xs, ys) (FunCall n t)
substitute s@(x:xs, y:ys) (FunCall n e) = do
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

catchVar :: Maps -> Expr -> Either String Expr
catchVar (m, m1) ex = case ex of
  (Id i@('@':_)) -> return $ Id i
  (Id i) ->
    case M.lookup i m :: Maybe Double of
      Just n -> return $ Number n
      Nothing -> Left $ "No such variable: " ++ i
  e -> goInside st e
  where st = catchVar (m, m1)

cmpDoubles :: Double -> Double -> Bool
cmpDoubles x y = abs(x-y) < 2*eps
  where eps = 1e-16

compFuns :: Map String (Double -> Double -> Bool)
compFuns = M.fromList [("lt",(<)), ("gt",(>)), ("eq",cmpDoubles)
  ,("ne",\x y -> not $ cmpDoubles x y ), ("le",(<=)), ("ge",(>=))]

compOps :: Map Operator (Double -> Double -> Bool)
compOps = M.fromList [(Lt,(<)), (Gt,(>)), (Eq,cmpDoubles)
  ,(Ne,\x y -> not $ cmpDoubles x y ), (Le,(<=)), (Ge,(>=))]

compOpsStr :: Map String (Double -> Double -> Bool)
compOpsStr = M.fromList [("<",(<)), (">",(>)), ("==",cmpDoubles)
  ,("/=",\x y -> not $ cmpDoubles x y ), ("<=",(<=)), (">=",(>=))]

mathFuns :: Map String (Double -> Double)
mathFuns = M.fromList [("sin",sin), ("cos",cos), ("asin",asin), ("acos",acos), ("tan",tan), ("atan",atan)
        ,("log",log), ("exp",exp), ("sqrt",sqrt), ("abs",abs)]

fmod :: Double -> Double -> Double
fmod x y = fromIntegral $ mod (floor x) (floor y)

mathOps :: Map String (Double -> Double -> Double)
mathOps = M.fromList [("+",(+)), ("-",(-)), ("*",(*)), ("/",(/)), ("%",fmod), ("^",(**))]

eval :: Maps -> Expr -> Either String (Double, Maps)
eval maps e = case e of
  (Asgn s _) | s `elem` ["pi","e","_"] -> Left $ "Can not change constant value: " ++ s
  (Asgn s e)                           -> do {(r,_) <- evm e; return (r, first (M.insert s r) maps)}
  (UDF n s e)                          -> do
    newe <- localize s e >>= catchVar maps
    return (fromIntegral $ M.size (snd maps), second (M.insert (n, length s) (map ('@':) s, newe)) maps)
  (FunCall "atan" [OpCall "/" e1 e2])       -> eval' atan2 e1 e2
  (FunCall n [a])   | M.member n mathFuns -> do
    let fun = fromJust (M.lookup n mathFuns :: Maybe (Double -> Double))
    (n,_) <- evm a
    return $ mps $ fun n
  (FunCall n [a,b]) | M.member n compFuns -> cmp n a b compFuns
  (FunCall "if" [a,b,c]) -> do
    (cond,_) <- evm a
    if cond /= 0
    then evm b
    else evm c
  (FunCall name e) ->
    case (M.lookup (name, length e) (snd maps) :: Maybe ([String],Expr)) of
      Just (al, expr) -> do
        expr1 <- substitute (al, e) expr
        (a,_) <- evm expr1
        return $ mps a
      Nothing -> case name of
        ('@':r) -> Left $ "Expression instead of function name: " ++ r ++ "/" ++ show (length e)
        _ -> Left $ "No such function: " ++ name ++ "/" ++ show (length e)
  (Id s)                     -> mte ("No such variable: " ++ s) $ mps (M.lookup s (fst maps) :: Maybe Double)
  (Number x)                 -> return $ mps x
  (OpCall "-" x (OpCall op y z)) | op `elem` ["+","-"] -> evm $ OpCall op (OpCall "-" x y) z
  (OpCall "/" x (OpCall op y z)) | op `elem` ["*","/", "%"]-> evm $ OpCall op (OpCall "/" x y) z
  (OpCall "%" x (OpCall op y z)) | op `elem` ["*","/", "%"]-> evm $ OpCall op (OpCall "%" x y) z
  (OpCall "/" x y) -> do
    (n,_) <- evm y
    (n1,_) <- evm x
    if n == 0 then Left "Div by zero" else return $ mps (n1 / n)
  (OpCall "%" x y) -> do
    (n,_) <- evm y
    (n1,_) <- evm x
    if n == 0
    then Left "Div by zero"
    else return $ mps (fromIntegral $ mod (floor n1) (floor n))
  (OpCall op x y) | M.member op compOpsStr -> cmp op x y compOpsStr
  (OpCall op x y) | M.member op mathOps -> eval' (fromJust $ M.lookup op mathOps) x y
  (UMinus (OpCall "^" x y)) -> evm $ OpCall "^" (UMinus x) y
  (UMinus x)         -> do {(n,_) <- evm x; return $ mps (-n)}
  (Par e)            -> evm e
  where
    mte _ (Just x, m) = Right (x, m)
    mte s (Nothing, _) = Left s
    evm = eval maps
    mps x = (x, maps)
    cmp op x y map = do
      let fun = fromJust (M.lookup op map :: Maybe (Double -> Double -> Bool))
      (n,_) <- evm x
      (n1,_) <- evm y
      return $ if fun n n1
        then mps 1
        else mps 0
    eval' f x y = do
      (t1,_) <- eval maps x
      (t2,_) <- eval maps y
      return (f t1 t2, maps)