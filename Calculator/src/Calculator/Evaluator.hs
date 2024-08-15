{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator.Evaluator (evalS, FunMap, VarMap, OpMap, Maps, Result, MessageType (..)) where

import Calculator.Builtins (
  defVar,
  derivative,
  functions,
  getFakePrecedences,
  getPrecedences,
  maxPrecedence,
  operators,
 )
import Calculator.Generator
import Calculator.Types (
  Arity (ArFixed, ArVar),
  Assoc (..),
  Chair,
  ChairVal (..),
  EvalState (..),
  ExecFn (..),
  ExecOp (..),
  Expr (..),
  Fun (..),
  FunFun (..),
  FunMap,
  FunOp (..),
  Maps (..),
  Op (..),
  OpMap,
  VarMap,
  ar2int,
  chairmap,
  exprToString,
  extractFormat,
  funmap,
  gen,
  isOp,
  maps,
  numToText,
  opmap,
  preprocess,
  showChair,
  showComplex,
  showFraction,
  showT,
  varmap,
  zipFormat,
 )
import Control.Applicative (asum)
import Control.Arrow (Arrow (second))
import Control.Lens (at, use, (.=), (?=), (^.))
import Control.Monad.Except (
  ExceptT,
  MonadError (throwError),
  runExceptT,
 )
import Control.Monad.State (
  MonadState (get),
  State,
  evalState,
 )
import Data.Complex (Complex (..), conjugate, imagPart, realPart)
import Data.Either (fromRight)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Metrics (damerauLevenshteinNorm)
import Numeric (showBin, showHex, showInt, showOct)
import System.Random (Random (randomR))

-- import Debug.Trace

funNames :: FunMap -> [(Text, Text)]
funNames = map (\(f, _) -> (f, f)) . M.keys

goInside :: (Expr -> Either Text Expr) -> Expr -> Either Text Expr
goInside f ex = case ex of
  (Par e) -> Par <$> f e
  (Call n e) -> Call n <$> mapM f e
  e -> return e

substitute :: [(Text, Expr)] -> Expr -> Either Text Expr
substitute [] e = return e
substitute ((x, y) : xys) (Id i) =
  if i == x
    then return $ case y of
      (Number _ _) -> y
      (Id _) -> y
      (Par _) -> y
      t -> Par t
    else substitute xys (Id i)
substitute s@((x, Id fname) : xys) (Call n e) =
  if n == x
    then Call fname <$> mapM (substitute s) e
    else do
      t <- mapM (substitute s) e
      substitute xys (Call n t)
substitute ((x, Id sname) : xys) (ChairSit n e) =
  if n == T.tail x
    then Right $ ChairSit sname e
    else substitute xys (ChairSit n e)
substitute s@(_ : xys) (Call n e) = do
  t <- mapM (substitute s) e
  substitute xys (Call n t)
substitute s ex = goInside (substitute s) ex

localize :: [Text] -> Expr -> Either Text Expr
localize [] e = return e
localize _ (Id i) | "v." `T.isPrefixOf` i = return $ Id (T.cons '@' i)
localize (x : xs) (Id i) = if i == x then return $ Id (T.cons '@' i) else localize xs (Id i)
localize s@(x : xs) (Call nm e) =
  if nm == x
    then Call (T.cons '@' nm) <$> mapM (localize s) e
    else do
      t <- mapM (localize s) e
      localize xs (Call nm t)
localize s ex = goInside (localize s) ex

catchVar :: (VarMap, FunMap) -> Expr -> Either Text Expr
catchVar (vm, fm) ex = case ex of
  (Id i) | T.head i == '@' -> return $ Id i
  (Id i) ->
    let a = M.lookup i vm :: Maybe (Complex Rational)
        getNames = map (\(f, _) -> (f, f)) . M.keys
        fNames = getNames functions
        b = lookup i (funNames fm ++ fNames) :: Maybe Text
     in case a of
          Just n -> return $ randomCheck (realPart n) i
          Nothing -> case b of
            Just s -> return $ Id s
            Nothing -> Left $ "No such variable: " <> i
   where
    randomCheck n i_ = if i_ == "m.r" then Id "m.r" else Number n 0
  e -> goInside st e
   where
    st = catchVar (vm, fm)

data MessageType = ErrMsg Text | MsgMsg Text deriving (Show, Eq)

type Result = ExceptT MessageType (State EvalState)

throwErr :: (MonadError MessageType m) => Text -> m a
throwErr = throwError . ErrMsg

throwMsg :: (MonadError MessageType m) => Text -> m a
throwMsg = throwError . MsgMsg

findFunction :: Text -> Int -> FunMap -> Maybe Fun
findFunction n a fm =
  let f = M.lookup (n, ArFixed a) fm
   in case f of
        Just _ -> f
        Nothing -> fst <$> M.minView (M.filterWithKey helper fm)
 where
  helper (k, ArFixed _) _ = False
  helper (k, ArVar a1) _ = k == n && a1 <= a

turboZip :: [Text] -> [Expr] -> [(Text, Expr)]
turboZip t e | length t == length e = zip t e
turboZip t e = turboZip' 0 t e
 where
  turboZip' :: Int -> [Text] -> [Expr] -> [(Text, Expr)]
  turboZip' _ [] [] = []
  turboZip' n (x : xs) [] = (x, Number 0 0) : turboZip' n xs []
  turboZip' n [] (y : ys) = ("@v." <> showT n, y) : turboZip' (n + 1) [] ys
  turboZip' n (x : xs) (y : ys) = (x, y) : turboZip' n xs ys

evalS :: Expr -> Result (Complex Rational)
evalS ex = case ex of
  Asgn s (ChairLit e) -> do
    es <- mapM (\(k,v) -> (k,) . DickVal <$> evm v) e
    maps . chairmap . at s ?= M.fromList es
    throwMsg "New chair"
  Asgn s _ | M.member s defVar -> throwErr $ "Cannot change a constant value: " <> s
  Asgn f e@(Call "/" [Id g, Number n _]) -> do
    let n1 = ArFixed . fromInteger . numerator $ n
    fm <- use (maps . funmap)
    if M.member (g, n1) fm
      then do
        maps . funmap . at (f, n1) ?= (fm M.! (g, n1))
        throwMsg ("Function " <> f <> "/" <> showT n1)
      else createVar f e
  Asgn s (Id "undef") -> evm (Call "undef" [Id s])
  Asgn s e -> createVar s e
  UDF f [s] (Call "df" [e, Id x]) | s == x -> do
    let de = derivative e (Id x)
    either throwErr (evm . UDF f [s]) de
  UDF f s e -> do
    mps <- use maps
    let newe = localize s e >>= catchVar (mps ^. varmap, mps ^. funmap)
    let len = length s
    let is_var = len > 0 && last s == "..."
    let arity = if is_var then ArVar (len - 1) else ArFixed len
    let args = if is_var then init s else s
    either
      throwErr
      ( \r -> do
          maps . funmap . at (f, arity) ?= Fun (map (T.cons '@') args) (ExFn r)
          throwMsg ("Function " <> f <> "/" <> showT (ar2int arity))
      )
      newe
  UDO n (-1) _ e@(Call op _) -> do
    mps <- use maps
    case M.lookup op (mps ^. opmap) of
      Just o@Op{} -> do
        maps . opmap . at n ?= o{oexec = AOp op}
        throwMsg $ "Operator alias " <> n <> " = " <> op
      Nothing -> throwErr $ "No such operator: " <> op
  UDO n p a e
    | M.member n operators -> throwErr $ "Can not redefine the built-in operator: " <> n
    | p < 1 || p > maxPrecedence -> throwErr $ "Bad precedence: " <> showT p
    | otherwise -> do
        mps <- use maps
        let t = localize ["x", "y"] e >>= catchVar (mps ^. varmap, mps ^. funmap)
        either
          throwErr
          ( \r -> do
              maps . opmap . at n ?= Op{precedence = p, associativity = a, oexec = ExOp r}
              throwMsg ("Operator " <> n <> " p=" <> showT p <> " a=" <> (if a == L then "left" else "right"))
          )
          t
  Call "debug" [e] -> throwMsg . showT . preprocess $ e
  Call "undef" [Id x] -> removeVar x
  Call "undef" [Id x, e] -> do
    n <- evm e
    removeFun x (fromInteger . numerator . realPart $ n)
  Call "opt" [Id x] -> extractId x >>= maybe (return $ 0 :+ 0) return
  Call "opt" [e] -> evm e
  Call op [e1, e2] | op `elem` (["opt", "?"] :: [Text]) -> do
    case e1 of
      Id x -> do
        res <- evm e2
        extractId x >>= maybe (return res) return
      _ -> evm e1
  Call "str" [e] -> evm e >>= (either throwErr (throwMsg . (\s -> "\"" <> s <> "\"")) . numToText)
  Call "fmt" (Number n ni : es) -> do
    let format = numToText (n :+ ni) >>= extractFormat
    case format of
      Left err -> throwError (ErrMsg err)
      Right fs -> do
        rs <- traverse evm es
        throwMsg $ either id id $ zipFormat fs rs
  Call "generate" [e] -> throwMsg . T.init . T.concat . map ((<> "\n") . showT) . generateTac $ e
  Call "id" [x] -> evm x
  Call "df" [a, x] -> do
    let e = derivative a x
    either throwErr (throwMsg . exprToString . preprocess) e
  Call "int" [Id fun, a, b] -> evm $ Call "int" [Id fun, a, b, Number 1e-10 0]
  Call "int" [Id fun, a, b, eps] -> do
    mps <- get
    a1 <- evm a
    b1 <- evm b
    e1 <- evm eps
    return $ integrate (realPart . fromRight (0 :+ 0) . procListElem mps fun) (realPart a1) (realPart b1) (realPart e1)
  Call "atan" [Call "/" [e1, e2]] -> do
    t1 <- evm e1
    t2 <- evm e2
    return $ toComplex $ atan2 (fromComplex t1 :: Double) (fromComplex t2 :: Double)
  Call "log" [e1, e2] -> do
    t1 <- evm e1
    t2 <- evm e2
    return $ toRational <$> logBase (fromRational <$> t1 :: Complex Double) (fromRational <$> t2 :: Complex Double)
  Call "prat" [e] -> do
    t1 <- evm e
    throwMsg $ showFraction (realPart t1)
  Call f [e] | f `elem` (["real", "imag", "conj"] :: [Text]) -> do
    t1 <- evm e
    case f of
      "real" -> return $ realPart t1 :+ 0
      "imag" -> return $ imagPart t1 :+ 0
      "conj" -> return $ conjugate t1
      _ -> throwErr $ "No such complex function: " <> f
  Call f [e] | f `elem` (["hex", "oct", "bin"] :: [Text]) -> do
    t1 <- evm e
    if denominator (realPart t1) == 1
      then
        let (function, p) = case f of
              "hex" -> (showHex, 'x')
              "oct" -> (showOct, 'o')
              "bin" -> (showBin, 'b')
              _ -> (showInt, ' ')
            sign = signum . numerator . realPart $ t1
         in throwMsg
              . T.pack
              . ((if sign == 1 then "" else "-") <>)
              . (['0', p] <>)
              . (`function` "")
              . abs
              . numerator
              . realPart
              $ t1
      else throwError (ErrMsg "Can't convert rational yet")
  Call ":" [a, b] -> evm (Call "if" [a, b, b])
  Call op1 [x, s@(Call op2 [y, z])] | isOp op1 && isOp op2 -> do
    mps <- use maps
    let pr = getPrecedences (mps ^. opmap) <> getFakePrecedences (mps ^. funmap)
    let a = M.lookup op1 pr
    let b = M.lookup op2 pr
    if a == b
      then case a of
        Nothing -> throwErr $ "No such operators: " <> op1 <> " " <> op2
        Just _ -> do
          let Op{associativity = asc1} = (mps ^. opmap) M.! op1
          let Op{associativity = asc2} = (mps ^. opmap) M.! op2
          case (asc1, asc2) of
            (L, L) -> evm $ Call op2 [Call op1 [x, y], z]
            (R, R) -> evm s >>= evm . (\yy -> Call op1 [x, yy]) . (\c -> Number (realPart c) (imagPart c))
            _ -> throwErr $ "Operators with a different associativity: " <> op1 <> " and " <> op2
      else evm s >>= evm . (\yy -> Call op1 [x, yy]) . (\c -> Number (realPart c) (imagPart c))
  oc@(Call "/" [x, y]) -> do
    n <- evm y
    n1 <- evm x
    if n == 0 :+ 0
      then throwErr $ "Division by zero: " <> exprToString oc
      else return . toComplex $ (realPart n1 / realPart n)
  oc@(Call "%" [x, y]) -> do
    n <- evm y
    n1 <- evm x
    if n == 0 :+ 0
      then throwErr $ "Division by zero: " <> exprToString oc
      else return ((:+ 0) . toRational $ mod (floor . realPart $ n1 :: Integer) (floor . realPart $ n :: Integer))
  Call "|>" [x, Id y] -> evm $ Call y [x]
  Call ":=" [ChairSit a xs, ChairSit b ys] -> do
    chair1 <- use $ maps . chairmap . at a
    chair2 <- use $ maps . chairmap . at b
    if isNothing chair1 || isNothing chair2
      then throwErr "No such chair"
      else case extractChair ys (fromMaybe M.empty chair2) of
        Nothing -> throwErr "No such key"
        Just v -> do
          maps . chairmap . at a ?= sitChair xs v (fromMaybe M.empty chair1)
          throwMsg "Sitting chair"
  Call ":=" [Id x, ChairSit a xs] -> do
    chair <- use $ maps . chairmap . at a
    case chair of
      Nothing -> throwErr "No such chair"
      Just ch -> case extractChair xs ch of
        Nothing -> throwErr "No such key"
        Just (DickVal v) -> evm $ Call ":=" [Id x, Number (realPart v) (imagPart v)]
        Just (PikeVal v) -> do
          maps . chairmap . at x ?= v
          throwMsg "Sitting chair"
  Call ":=" [ChairSit a xs, y] -> do
    val <- case y of
      ChairLit _ -> return $ PikeVal M.empty
      ChairSit b ys -> do
        chair <- use $ maps . chairmap . at b
        case chair of
          Nothing -> throwMsg "No such chair"
          Just ch -> do
            return $ fromMaybe (PikeVal M.empty) (extractChair ys ch)
      _ -> DickVal <$> evm y
    chair <- use $ maps . chairmap . at a
    case chair of
      Nothing -> throwMsg "No such chair"
      Just ch -> do
        maps . chairmap . at a ?= sitChair xs val ch
        throwMsg "Sitting chair"
  Call op [Id x, y] | op `elem` ([":=", "::="] :: [Text]) -> do
    if "c." `T.isPrefixOf` x || M.member x defVar
      then throwError (ErrMsg "I'm afraid you can't do that.")
      else do
        n <- evm y
        mps <- use maps
        maps
          . varmap
          . at
            ( if x `M.member` (mps ^. varmap)
                then x
                else "_." <> x
            )
          ?= n
        return n
  Call op [Id x, y] | op `elem` (["+=", "-=", "*=", "/=", "%=", "^=", "|=", "&="] :: [Text]) -> evm (Asgn x (Call (T.init op) [Id x, y]))
  Call op [x, y] | op `elem` (["+=", "-=", "*=", "/=", "%=", "^=", "|=", "&=", ":=", "::="] :: [Text]) -> throwErr $ "Cannot assign to an expression with: " <> op
  Call op [x, y] | M.member op operators -> evalBuiltinOp op x y
  Call op [x, y] | isOp op -> do
    mps <- use maps
    case (M.lookup op (mps ^. opmap) :: Maybe Op) of
      Just Op{oexec = ExOp expr} -> do
        let expr1 = substitute (zip ["@x", "@y"] [x, y]) expr
        either throwErr evm expr1
      Just Op{oexec = AOp aop} -> evm (Call aop [x, y])
      Nothing -> case op of
        opn | T.head opn == '@' -> throwErr $ "Expression instead of a function name: " <> T.tail opn <> "/2"
        _ -> throwErr $ "No such operator: " <> op <> "/2"
      _ -> throwErr $ "Suspicious operator: " <> op
  Call "if" [a, b, c] -> do
    cond <- evm a
    if cond /= 0 :+ 0
      then evm b
      else evm c
  Call "loop" [i, c, a] -> do
    _ <- evm i
    evm $ Call "loop" [c, a]
  Call "loop" [c, a] -> do
    n <- evm c
    res <- evm a
    if n == 0 :+ 0
      then return res
      else evm $ Call "loop" [c, a]
  Call "-" [Call "^" [x, y]] -> evm $ Call "^" [Call "-" [x], y]
  Call "-" [x] -> evm $ Call "-" [Number 0 0, x]
  Call f ps | M.member (f, ArFixed . length $ ps) functions -> do
    let builtin_fun = functions M.! (f, ArFixed . length $ ps)
    case fexec builtin_fun of
      FnFn (CmpFn fun) -> cmp fun (head ps) (ps !! 1)
      FnFn (IntFn1 fun) -> evalInt1 fun (head ps)
      FnFn (IntFn2 fun) -> evalInt fun (head ps) (ps !! 1)
      FnFn (BitFn fun) -> evalBit fun (head ps)
      FnFn (FracFn1 fun) -> evm (head ps) >>= \x -> return $ fun x
      FnFn (MathFn1 fun) -> do
        n <- evm (head ps)
        let r = (\x -> if abs (realPart x) <= sin pi && imagPart x == 0 then 0 else x) . fun . fmap fromRational $ n
        return $ toRational <$> r
      FnFn (MathFn2 fun) -> do
        n <- evm (head ps)
        m <- evm (ps !! 1)
        return . fun n $ m
      ExFn expr -> do
        let expr1 = substitute (zip (params builtin_fun) ps) expr
        either throwErr evm expr1
      _ -> throwError (ErrMsg "Misteriously missing function")
  Call name e -> do
    mps <- use maps
    case findFunction name (length e) (mps ^. funmap) of
      Just (Fun al (ExFn expr)) -> do
        let expr1 = substitute (("@v.n", Number (fromIntegral . length $ e) 0):turboZip al e) expr
        either throwErr evm expr1
      Nothing -> case name of
        x | T.head x == '@' -> throwErr $ "Expression instead of a function name: " <> T.tail x <> "/" <> showT (length e)
        _ ->
          let (wa, wn) = findSimilar (name, length e) (M.keys (mps ^. funmap))
              cvt_nls txt nls =
                if not (null nls)
                  then txt <> T.init (T.concat (map (\(n, l) -> "\t" <> n <> "/" <> showT l <> "\n") nls))
                  else ""
              wat = cvt_nls "\nFunctions with the same name:\n" wa
              wnt = cvt_nls "\nFunctions with similar names:\n" wn
           in throwErr $ "No such function: " <> name <> "/" <> showT (length e) <> wat <> wnt
      _ -> throwErr $ "Suspicious function: " <> name
  Id "m.r" -> do
    rgen <- use gen
    let (randomNumber, newGen) = randomR (0.0, 1.0 :: Double) rgen
    gen .= newGen
    return . (:+ 0) . toRational $ randomNumber
  Id s -> extractId s >>= maybe (throwError (ErrMsg $ "No such variable : " <> s)) return
  ChairLit _ -> return $ 0 :+ 0
  ChairSit a xs -> do
    val <- use $ maps . chairmap . at a
    case val of
      Nothing -> throwErr "No such chair!"
      Just ch -> case extractChair xs ch of
        Nothing -> throwErr "No such key!"
        Just (DickVal d) -> return d
        Just (PikeVal d) -> throwMsg $ showChair d
  Number x xi -> return $ x :+ xi
  Par e -> evm e
  Seq _ -> throwErr "Sequences are not supported in this mode!"
  Imprt _ -> throwErr "Imports are not supported in this mode!"
  Label _ -> throwErr "Label are not supported in this mode!"
 where
  extractId s = do
    chairs <- use $ maps . chairmap
    vars <- use $ maps . varmap
    if M.member s chairs
      then
        throwMsg $ showChair (chairs M.! s)
      else do
        let val = asum ([M.lookup ("_." <> s) vars, M.lookup s vars] :: [Maybe (Complex Rational)])
        return val
  createVar f e = do
    r <- evm e
    maps . varmap . at f ?= r
    throwMsg ((if "c." `T.isPrefixOf` f then "Constant " else "Variable ") <> f <> "=" <> showComplex r)
  removeVar f =
    if M.member f defVar
      then throwErr "Can't remove that"
      else do
        vm <- use $ maps . varmap
        maps . varmap .= M.delete f vm
        throwMsg $ "Removed: " <> f
  removeFun f a =
    if M.member (f, ArFixed a) functions
      then throwErr "Can't remove that"
      else do
        vm <- use $ maps . funmap
        maps . funmap .= M.delete (f, if a >= 0 then ArFixed a else ArVar (-a)) vm
        throwMsg $ "Removed: " <> f <> "/" <> showT a
  evalBuiltinOp bop x y = do
    let builtin_op = operators M.! bop
    case oexec builtin_op of
      FnOp (CmpOp fun) -> cmp fun x y
      FnOp (MathOp fun) -> eval' fun bop x y
      FnOp (BitOp fun) -> (:+ 0) <$> bitEval fun x y
      ExOp e -> evm e
      _ -> throwError (ErrMsg "Misteriously missing operator")
  tooBig = 2 ^ (8000000 :: Integer) :: Rational
  cmp fun x y = do
    n <- evm x
    n1 <- evm y
    return $
      if fun (realPart n) (realPart n1)
        then 1 :+ 0
        else 0 :+ 0
  bitEval op x y = do
    n <- evm x
    n1 <- evm y
    if denominator (realPart n) == 1 && denominator (realPart n1) == 1
      then return . toRational $ op (numerator (realPart n)) (numerator (realPart n1))
      else throwError (ErrMsg "Cannot perform bitwise operator on a rational")
  evm x = do
    r <- evalS x
    if realPart r > tooBig
      then throwError (ErrMsg "Too much!")
      else return r
  evalInt f x y = do
    t1 <- evm x
    t2 <- evm y
    if denominator (realPart t1) == 1 && denominator (realPart t2) == 1
      then return . toComplex $ f (numerator (realPart t1)) (numerator (realPart t2))
      else throwError (ErrMsg "Cannot use integral function on rational numbers!")
  evalInt1 :: (Double -> Integer) -> Expr -> Result (Complex Rational)
  evalInt1 f x = do
    t1 <- evm x
    return . toComplex $ f (fromComplex t1)
  evalBit f x = do
    t <- evm x
    if denominator (realPart t) == 1
      then return . toComplex $ f (numerator (realPart t))
      else throwError (ErrMsg "Cannot use bitwise function on rational numbers!")
  eval' :: (Complex Rational -> Complex Rational -> Complex Rational) -> Text -> Expr -> Expr -> Result (Complex Rational)
  eval' f op x y = do
    t1 <- evm x
    t2 <- evm y
    if (op == "^" && logBase 10 (fromComplex t1 :: Double) * (fromComplex t2 :: Double) > 2408240)
      || realPart (f t1 t2) > tooBig
      then throwError (ErrMsg "Too much!")
      else return $ f t1 t2
  procListElem m fun n = evalState (runExceptT (evm (Call fun [Number n 0]))) m
  findSimilar :: (Text, Int) -> [(Text, Arity)] -> ([(Text, Int)], [(Text, Int)])
  findSimilar (name, arity) funs =
    let extractArity = map (second ar2int)
        wrongArgNum = extractArity $ filter (\(n, a) -> n == name && arity /= ar2int a) funs
        wrongName = extractArity $ filter (\(n, _) -> let dln = damerauLevenshteinNorm n name in dln >= 0.5 && dln < 1) funs
     in (wrongArgNum, wrongName)
  fromComplex = fromRational . realPart
  toComplex :: (Real a) => a -> Complex Rational
  toComplex = (:+ 0.0) . toRational
  extractChair :: [Text] -> Chair -> Maybe ChairVal
  extractChair [] _ = Nothing
  extractChair (x : xs) ch = case M.lookup x ch of
    Nothing -> Nothing
    Just val@(DickVal v) -> Just val
    Just val@(PikeVal v) -> case xs of
      [] -> Just val
      _ -> extractChair xs v
  sitChair :: [Text] -> ChairVal -> Chair -> Chair
  sitChair [] _ ch = ch
  sitChair (x : xs) value ch =
    if M.member x ch
      then case xs of
        [] -> M.insert x value ch
        _ -> case M.lookup x ch of
          Nothing -> ch
          Just (DickVal _) -> ch
          Just (PikeVal ch1) -> M.insert x (PikeVal (sitChair xs value ch1)) ch
      else case xs of
        [] -> M.insert x value ch
        _ -> M.insert x (PikeVal (sitChair xs value M.empty)) ch

s1_ :: (Rational -> Rational) -> Rational -> Rational -> Rational -> Rational -> (Rational, Rational, Rational)
s1_ f a b fa fb =
  let m = (a + b) / 2
      r = f m
      g = (abs (b - a) / 6) * (fa + 4 * r + fb)
   in (m, r, g)

s2_ :: (Rational -> Rational) -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational
s2_ f a b fa fb eps whole m fm =
  let (ml, rl, gl) = s1_ f a m fa fm
      (mr, rr, gr) = s1_ f m b fm fb
      delta = gl + gr - whole
   in if abs delta <= 15 * eps
        then gl + gr + (delta / 15)
        else
          s2_ f a m fa fm (eps / 2) gl ml rl
            + s2_ f m b fm fb (eps / 2) gr mr rr

integrate :: (Rational -> Rational) -> Rational -> Rational -> Rational -> Complex Rational
integrate f a b eps =
  let fa = f a
      fb = f b
      (m, r, g) = s1_ f a b fa fb
   in s2_ f a b fa fb eps g m r :+ 0
