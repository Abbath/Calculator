{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Calculator.Evaluator (evalS, getPriorities, extractNames, FunMap, VarMap, OpMap, Maps, Result2) where

import           Calculator.Types (Assoc (..), Expr (..), exprToString,
                                   preprocess, showRational, showT, isOp)
import           Control.Lens     ((%~), (.~), (^.), _1, _2, _3)
import           Data.Either      (fromRight)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M
import Data.Ratio ( denominator, numerator )
import           Control.Arrow    (second)
import Control.Monad.State
    ( MonadState(get), modify, evalState, State, gets )
import Control.Monad.Except
    ( ExceptT, runExceptT, MonadError(throwError) )
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Data.Bits        ((.|.), (.&.), xor)
import           Numeric          (showHex, showOct, showBin, showInt)
import System.Random (StdGen, Random (randomR))
import qualified Data.Bifunctor

type FunMap = Map (Text, Int) ([Text], Expr)
type VarMap = Map Text Rational
type OpMap = Map Text ((Int, Assoc), Expr)
type Maps = (VarMap, FunMap, OpMap)

extractNames :: Maps -> [String]
extractNames (v, f, o) = map T.unpack $ M.keys o <> M.keys v <> map fst (M.keys f)

funNames :: FunMap -> [(Text, Text)]
funNames = map (\(f,_) -> (f, f)) . M.keys

goInside :: (Expr -> Either Text Expr) -> Expr -> Either Text Expr
goInside f ex = case ex of
  (Par e)           -> Par <$> f e
  (UMinus e)        -> UMinus <$> f e
  (Call n e)        -> Call n <$> mapM f e
  e                 -> return e

substitute :: ([Text], [Expr]) -> Expr -> Either Text Expr
substitute ([], []) e = return e
substitute (x, y) _
  | length x /= length y =
      Left $ "Bad argument number: " <> showT (length y) <> " instead of " <> showT (length x)
substitute (x : xs, y : ys) (Id i) =
  if i == x
    then return $ case y of
      n@(Number _) -> n
      iD@(Id _) -> iD
      p@(Par _) -> p
      t -> Par t
    else substitute (xs, ys) (Id i)
substitute s@(x : xs, Id fname : ys) (Call n e) =
  if n == x
    then Call fname <$> mapM (substitute s) e
    else do
      t <- mapM (substitute s) e
      substitute (xs, ys) (Call n t)
substitute s@(_ : xs, _ : ys) (Call n e) = do
  t <- mapM (substitute s) e
  substitute (xs, ys) (Call n t)
substitute s ex = goInside (substitute s) ex

localize :: [Text] -> Expr -> Either Text Expr
localize [] e = return e
localize (x:xs) (Id i) = if i == x then return $ Id (T.cons '@' i) else localize xs (Id i)
localize s@(x:xs) (Call nm e) = if nm == x
  then Call (T.cons '@' nm) <$> mapM (localize s) e
  else do
    t <- mapM (localize s) e
    localize xs (Call nm t)
localize s ex = goInside (localize s) ex

catchVar :: (VarMap, FunMap) -> Expr -> Either Text Expr
catchVar (vm, fm) ex = case ex of
  (Id i) | T.head i == '@' -> return $ Id i
  (Id i) ->
    let a = M.lookup i vm :: Maybe Rational
        getNames = map (\f -> (f,f)) . M.keys
        fNames = getNames mathFuns ++ getNames intFuns1 ++ getNames intFuns2 ++ getNames compFuns ++ [("m.r", "m.r")]
        b = lookup i (funNames fm ++ fNames) :: Maybe Text
    in case a of
         Just n -> return $ Number n
         Nothing -> case b of
                     Just s  -> return $ Id s
                     Nothing -> Left $ "No such variable: " <> i
  e -> goInside st e
    where st = catchVar (vm, fm)

compFuns :: Map Text (Rational -> Rational -> Bool)
compFuns = [("lt",(<)), ("gt",(>)), ("eq",(==))
  ,("ne",(/=)), ("le",(<=)), ("ge",(>=))]

compOps :: Map Text (Rational -> Rational -> Bool)
compOps = [("<",(<)), (">",(>)), ("==",(==))
  ,("!=",(/=)), ("<=",(<=)), (">=",(>=))]

mathFuns :: Map Text (Double -> Double)
mathFuns = [("sin",sin), ("cos",cos), ("asin",asin), ("acos",acos), ("tan",tan), ("atan",atan)
        ,("log",log), ("exp",exp), ("sqrt",sqrt), ("abs",abs)]

intFuns1 :: Map Text (Double -> Integer)
intFuns1 = [("trunc", truncate), ("round", round), ("floor", floor), ("ceil", ceiling)]


intFuns2 :: Map Text (Integer -> Integer -> Integer)
intFuns2 = [("gcd", gcd), ("lcm", lcm), ("div", div), ("mod", mod), ("quot", quot), ("rem", rem), ("xor", xor)]

fmod :: Rational -> Rational -> Rational
fmod x y = fromInteger $ mod (floor x) (floor y)

mathOps :: Map Text (Rational -> Rational -> Rational)
mathOps = [("+",(+)), ("-",(-)), ("*",(*)), ("/",(/)), ("%",fmod), ("^",pow)]

bitOps :: Map Text (Integer -> Integer -> Integer)
bitOps = [("|", (.|.)), ("&", (.&.))]

pow :: Rational -> Rational -> Rational
pow a b | denominator a ==1 && denominator b == 1 && numerator b < 0 = toRational $ (fromRational a :: Double) ^^ numerator b
pow a b | denominator a == 1 && denominator b == 1 = toRational $ numerator a ^ numerator b
pow a b = toRational $ (fromRational a :: Double) ** (fromRational b :: Double)

getPriorities :: OpMap -> Map Text Int
getPriorities om = let lst = M.toList om
                       ps = M.fromList $ map (second (fst . fst)) lst
                   in ps

type Result2 = ExceptT Text (State (Maps, StdGen))

evalS :: Expr -> Result2 Rational
evalS ex = case ex of
  Asgn s _ | s `elem` (["m.pi", "m.e", "m.phi", "_"] :: [T.Text]) -> throwError $ "Cannot change a constant value: " <> s
  Asgn s e -> do
    r <- evm e
    modify (Data.Bifunctor.first (_1 %~ M.insert s r))
    throwError ("Constant " <> s <> "=" <> showRational r)
  UDF n [s] (Call "df" [e, Id x]) | s == x -> do
    let de = derivative e (Id x)
    either throwError (evm . UDF n [s]) de
  UDF n s e -> do
    maps <- gets fst
    let newe = localize s e >>= catchVar (maps^._1, maps ^._2)
    either throwError (\r -> do
      let newmap = M.insert (n, length s) (map (T.cons '@') s, r) $ maps^._2
      modify (Data.Bifunctor.first (_2 .~ newmap))
      throwError ("Function " <> n <> "/" <> showT (length s))) newe
  UDO n (-1) _ e@(Call op _) -> do
    maps <- gets fst
    case M.lookup op (maps^._3) of
      Just ((p, a), _) -> do
        let newmap = M.insert n ((p, a), e) (maps^._3)
        modify (Data.Bifunctor.first (_3 .~ newmap))
        throwError ("Operator alias " <> n <> " = " <> op)
      Nothing -> throwError $ "No such operator: " <> op
  UDO n p a e
    | M.member n mathOps || M.member n compOps || M.member n bitOps || n == "=" -> throwError $ "Can not redefine the embedded operator: " <> n
    | p < 1 || p > 6 ->  throwError $ "Bad priority: " <> showT p
    | otherwise -> do
        maps <- gets fst
        let t = localize ["x","y"] e >>= catchVar (maps^._1, maps^._2)
        either throwError (\r -> do
          let newmap = M.insert n ((p, a), r) (maps^._3)
          modify (Data.Bifunctor.first (_3 .~ newmap))
          throwError ("Operator " <> n <> " p=" <> showT p <> " a=" <> (if a == L then "left" else "right"))) t
  Call "debug" [e] -> throwError . T.pack . show . preprocess $ e
  Call "df" [a,x] -> do
      let e = derivative a x
      either throwError (throwError . exprToString . preprocess) e
  Call "int" [Id fun, a, b, eps] -> do
      maps <- get
      a1 <- evm a
      b1 <- evm b
      e1 <- evm eps
      return $ integrate (fromRight 0 . procListElem maps fun) a1 b1 e1
  Call "atan" [Call "/" [e1, e2]] -> do
    t1 <- evm e1
    t2 <- evm e2
    return . toRational $ atan2 (fromRational t1 :: Double) (fromRational t2 :: Double)
  Call "log" [e1, e2] -> do
    t1 <- evm e1
    t2 <- evm e2
    return . toRational $ logBase (fromRational t1 :: Double) (fromRational t2 :: Double)
  Call "prat" [e] -> do
    t1 <- evm e
    throwError $ showT (numerator t1) <> " / " <> showT (denominator t1)
  Call f [e] | f `elem` (["hex", "oct", "bin"] :: [Text]) -> do
    t1 <- evm e
    if denominator t1 == 1
      then let (function, p) = case f of
                 "hex" -> (showHex, 'x')
                 "oct" -> (showOct, 'o')
                 "bin" -> (showBin, 'b')
                 _ -> (showInt, ' ')
           in throwError . T.pack . (['0', p] ++) . (`function` "") . numerator $ t1
      else throwError "Cant convert float to hex yet"
  Call op1 [x, s@(Call op2 [y, z])] | isOp op1 && isOp op2 -> do
    maps <- gets fst
    let pr = getPriorities (maps^._3)
    let a = M.lookup op1 pr
    let b = M.lookup op2 pr
    if a == b
    then case a of
      Nothing -> throwError $ "No such operators: " <> op1 <> " " <> op2
      Just _ -> do
        let ((_, asc1), _) = (maps^._3) M.! op1
        let ((_, asc2), _) = (maps^._3) M.! op2
        case (asc1, asc2) of
          (L, L) -> evm $ Call op2 [Call op1 [x, y], z]
          (R, R) -> evm s >>= evm . (\yy -> Call op1 [x, yy]) . Number
          _ -> throwError $ "Operators with a different associativity: " <> op1 <> " and " <> op2
    else evm s >>= evm . (\yy -> Call op1 [x, yy]) . Number
  oc@(Call "/" [x, y]) -> do
    n <- evm y
    n1 <- evm x
    if n == 0 then throwError $ "Division by zero: " <> exprToString oc else return (n1 / n)
  oc@(Call "%" [x, y]) -> do
    n <- evm y
    n1 <- evm x
    if n == 0
    then throwError $ "Division by zero: " <> exprToString oc
    else return (fromInteger $ mod (floor n1) (floor n))
  Call op [x, y] | M.member op compOps -> cmp op x y compOps
  Call op [x, y] | M.member op mathOps -> eval' ( mathOps M.! op) op x y
  Call op [x, y] | M.member op bitOps  -> bitEval ( bitOps M.! op) x y
  Call op [x, y] | isOp op -> do
    maps <- gets fst
    case (M.lookup op (maps^._3) :: Maybe ((Int, Assoc),Expr)) of
      Just (_, expr) -> do
        let expr1 = substitute (["@x", "@y"], [x,y]) expr
        either throwError evm expr1
      Nothing -> case op of
        opn | T.head opn == '@' -> throwError $ "Expression instead of a function name: " <> T.tail opn <> "/2"
        _ -> throwError $ "No such operator: " <> op <> "/2"
  Call f [e1, e2] | M.member f intFuns2 -> evalInt f e1 e2
  Call f [e1] | M.member f intFuns1 -> evalInt1 f e1
  Call name [a]   | M.member name mathFuns -> do
    let fun = mathFuns M.! name
    n <- evm a
    let r = (\x -> if abs x <= sin pi then 0 else x) . fun . fromRational $ n
    if isNaN r
      then throwError "NaN"
      else return $ toRational r
  Call n [a,b] | M.member n compFuns -> cmp n a b compFuns
  Call "if" [a,b,c] -> do
    cond <- evm a
    if cond /= 0
      then evm b
      else evm c
  Call name e -> do
    maps <- gets fst
    case (M.lookup (name, length e) (maps^._2) :: Maybe ([Text],Expr)) of
      Just (al, expr) -> do
        let expr1 = substitute (al, e) expr
        either throwError evm expr1
      Nothing -> case name of
        x | T.head x == '@' -> throwError $ "Expression instead of a function name: " <> T.tail x <> "/" <> showT (length e)
        _ -> throwError $ "No such function: " <> name <> "/" <> showT (length e)
  Id "m.r" -> do
    gen <- gets snd
    let (randomNumber, newGen) = randomR (0.0, 1.0 :: Double) gen
    modify (Data.Bifunctor.second (const newGen))
    return $ toRational randomNumber
  Id s     -> do
    maps <- gets fst
    mte ("No such variable: " <> s) (M.lookup s (maps^._1) :: Maybe Rational)
  Number x -> return x
  UMinus (Call "^" [x, y]) -> evm $ Call "^" [UMinus x, y]
  UMinus x         -> (0-) <$> evm x
  Par e            -> evm e
  where
    mte s = maybe (throwError s) return
    tooBig = 2^(8000000 :: Integer) :: Rational
    cmp op x y mp = do
      let fun = mp M.! op
      n <- evm x
      n1 <- evm y
      return $ if fun n n1
        then 1
        else 0
    bitEval op x y = do
      n <- evm x
      n1 <- evm y
      if denominator n == 1 && denominator n1 == 1
        then return . toRational $ op (numerator n) (numerator n1)
        else throwError "Cannot perform bitwise operator on a float"
    evm x = do
      r <- evalS x
      if r > tooBig
         then throwError "Too much!"
         else return r
    evalInt f x y = do
      t1 <- evm x
      t2 <- evm y
      if denominator t1 == 1 && denominator t2 == 1
         then return . toRational $ (intFuns2 M.! f) (numerator t1) (numerator t2)
         else throwError "Cannot use integral function on real numbers!"
    evalInt1 f x = do
      t1 <- evm x
      return . toRational $ (intFuns1 M.! f) (fromRational t1)
    eval' :: (Rational -> Rational -> Rational) -> Text -> Expr -> Expr -> Result2 Rational
    eval' f op x y = do
      t1 <- evm x
      t2 <- evm y
      if ( op == "^" && logBase 10 (fromRational t1 :: Double) * (fromRational t2 :: Double) > 2408240) ||
         f t1 t2 > tooBig
         then throwError "Too much!"
         else return (f t1 t2)
    procListElem m fun n = evalState (runExceptT (evm (Call fun [Number n]))) m

derivative :: Expr -> Expr -> Either Text Expr
derivative e x = case e of
  Par ex -> Par <$> derivative ex x
  UMinus ex -> UMinus <$> derivative ex x
  Number _ -> return $ Number 0
  i@(Id _) | i == x -> return $ Number 1
  (Id _) -> return $ Number 0
  Call "^" [i, Number n] | i == x->
    return $ Call "*" [Number n, Call "^" [i, Number (n-1)]]
  Call "^" [Number a, i] | i == x -> return $ Call "*" [e, Call "log" [Number a]]
  Call op [ex1, ex2] | op == "-" || op == "+" -> do
    a1 <- derivative ex1 x
    a2 <- derivative ex2 x
    return $ Call op [a1, a2]
  Call "*" [ex1, ex2] -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    return $ Call "+" [Call "*" [d1, ex2], Call "*" [d2, ex1]]
  Call "/" [ex1, ex2] -> do
    d1 <- derivative ex1 x
    d2 <- derivative ex2 x
    return $ Call "/" [Call "-" [Call "*" [d1, ex2], Call "*" [d2, ex1]], Call "^" [ex2, Number 2]]
  ex@(Call "exp" [i]) | i == x -> return ex
  Call "log" [i] | i == x -> return $ Call "/" [Number 1, i]
  Call "sin" [i] | i == x -> return $ Call "cos" [i]
  Call "cos" [i] | i == x -> return $ UMinus (Call "sin" [i])
  Call "tan" [i] | i == x ->
    return $ Call "/" [Number 1, Call "^" [Call "cos" [i], Number 2]]
  ex@(Call _ [i]) -> do
    a1 <- derivative ex i
    a2 <- derivative i x
    return $ Call "*" [a1, a2]
  _ -> Left "No such derivative"


s1_ :: (Rational -> Rational) -> Rational -> Rational -> Rational -> Rational -> (Rational, Rational, Rational)
s1_ f a b fa fb = let m = (a + b) / 2
                      r = f m
                      g = (abs (b - a) / 6) * (fa + 4 * r + fb)
                  in (m, r, g)

s2_ :: (Rational -> Rational) -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational
s2_ f a b fa fb eps whole m fm = let (ml, rl, gl) = s1_ f a m fa fm
                                     (mr, rr, gr) = s1_ f m b fm fb
                                     delta = gl + gr - whole
                                 in if abs delta <= 15 * eps
                                    then gl + gr + (delta / 15)
                                    else s2_ f a m fa fm (eps / 2) gl ml rl +
                                         s2_ f m b fm fb (eps / 2) gr mr rr

integrate :: (Rational -> Rational) -> Rational -> Rational -> Rational -> Rational
integrate f a b eps = let fa = f a
                          fb = f b
                          (m, r, g) = s1_ f a b fa fb
                      in s2_ f a b fa fb eps g m r

