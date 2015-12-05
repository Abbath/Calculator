module Calculator.Evaluator (eval) where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Calculator.Types

goInside :: (Expr -> Either String Expr) -> Expr -> Either String Expr
goInside f e = case e of
    (Sum op e1 e2) -> Sum op <$> f e1 <*> f e2
    (Prod op e1 e2) -> Prod op <$> f e1 <*> f e2
    (Pow e1 e2) -> Pow <$> f e1 <*> f e2
    (Par e) -> Par <$> f e
    (Fun fun e) -> Fun fun <$> f e
    (UMinus e) -> UMinus <$> f e
    e -> return e

substitute :: ([String], [Expr]) -> Expr -> Either String Expr
substitute ([],[]) e = return e
substitute (x,y) _ | length x /= length y = Left "Bad argument number"
substitute (x:xs, y:ys) (Id i) = if i == x then return y else substitute (xs, ys) (Id i)
substitute s ex = case ex of
    (FunCall n e) -> FunCall n <$> mapM st e
    e -> goInside st e
    where st = substitute s

localize :: (String,Int) -> [String] -> Expr -> Either String Expr
localize (n,a) [] e = return e
localize (n,a) (x:xs) (Id i) = if i == x then return $ Id ('$':i) else localize (n,a) xs (Id i)
localize (n,a) s ex = case ex of
    (FunCall nm e) -> if nm == n && a == length e
        then Left "Recursion is not supported yet"
        else FunCall nm <$> mapM st e
    e -> goInside st e
    where st = localize (n,a) s

catchVar :: (Map String Double, Map (String, Int) ([String],Expr)) -> Expr -> Either String Expr
catchVar (m,m1) ex = case ex of
    (Id i@('$':_)) -> return $ Id i
    (Id i) ->
        case M.lookup i m :: Maybe Double of
            Just n -> return $ Number n
            Nothing -> Left $ "No such variable: " ++ i
    (FunCall n e) ->
        if M.member (n,length e) m1
        then FunCall n <$> mapM st e
        else Left $ "No such function: " ++ n ++ "/" ++ show (length e)
    e -> goInside st e
    where st = catchVar (m,m1)

eval :: (Map String Double, Map (String, Int) ([String],Expr)) -> Expr -> Either String (Double, Map String Double, Map (String,Int) ([String],Expr))
eval (m,m1) e = case e of
   (Asgn s _) | s `elem` ["pi","e","_"] -> Left $ "Can not change constant value: " ++ s
   (Asgn s e)                       -> do {(r,_,_) <- eval (m,m1) e; return (r, M.insert s r m, m1)}
   (UDF n s e)                      -> do
        newe <- localize (n,length s) s e >>= catchVar (m,m1)
        return (fromIntegral $ M.size m1,m, M.insert (n, length s) (map ('$':) s, newe) m1)
   (FunCall name e)                 ->
        case (M.lookup (name, length e) m1 :: Maybe ([String],Expr)) of
            Just (al, expr) -> do
                expr1 <- substitute (al, e) expr
                (a,_,_) <- eval (m,m1) expr1
                return (a, m, m1)
            Nothing -> Left $ "No such function: " ++ name ++ "/" ++ show (length e)
   (Id s)                           -> mte ("No such variable: " ++ s) (M.lookup s m :: Maybe Double,m,m1)
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