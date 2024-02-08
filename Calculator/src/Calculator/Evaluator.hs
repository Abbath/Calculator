{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Calculator.Evaluator (evalS, FunMap, VarMap, OpMap, Maps, Result, MessageType(..)) where

import Calculator.Builtins
    ( defVar,
      functions,
      getFakePrecedences,
      getPrecedences,
      operators,
      derivative )
import Calculator.Generator
import Calculator.Types
  ( Assoc (..),
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
    exprToString,
    isOp,
    preprocess,
    showComplex,
    showFraction,
    showT,
    EvalState (..),
    maps,
    gen,
    numToText,
    extractFormat,
    zipFormat,
    varmap,
    funmap,
    opmap,
    chairmap,
    ChairVal(..),
    Chair,
    showChair
  )
import Control.Lens ((%~), (.~), (^.), at)
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.State
  ( MonadState (get),
    State,
    evalState,
    gets,
    modify,
  )
import Data.Either (fromRight)
import qualified Data.Map.Strict as M
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Metrics (damerauLevenshteinNorm)
import Numeric (showBin, showHex, showInt, showOct)
import System.Random (Random (randomR))
import Control.Applicative (asum)
import Data.Complex ( conjugate, imagPart, realPart, Complex(..) )
import Data.Maybe (fromMaybe, isNothing)

-- import Debug.Trace

funNames :: FunMap -> [(Text, Text)]
funNames = map (\(f,_) -> (f, f)) . M.keys

goInside :: (Expr -> Either Text Expr) -> Expr -> Either Text Expr
goInside f ex = case ex of
  (Par e)    -> Par <$> f e
  (Call n e) -> Call n <$> mapM f e
  e          -> return e

substitute :: ([Text], [Expr]) -> Expr -> Either Text Expr
substitute ([], []) e = return e
substitute (x, y) _
  | length x /= length y =
      Left $ "Bad argument number: " <> showT (length y) <> " instead of " <> showT (length x)
substitute (x : xs, y : ys) (Id i) =
  if i == x
    then return $ case y of
      (Number _ _) -> y
      (Id _) -> y
      (Par _) -> y
      t -> Par t
    else substitute (xs, ys) (Id i)
substitute s@(x : xs, Id fname : ys) (Call n e) =
  if n == x
    then Call fname <$> mapM (substitute s) e
    else do
      t <- mapM (substitute s) e
      substitute (xs, ys) (Call n t)
substitute (x : xs, Id sname : ys) (ChairSit n e) =
  if n == T.tail x
    then Right $ ChairSit sname e
    else substitute (xs, ys) (ChairSit n e)
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

throwErr :: MonadError MessageType m => Text -> m a
throwErr = throwError . ErrMsg

throwMsg :: MonadError MessageType m => Text -> m a
throwMsg = throwError . MsgMsg

evalS :: Expr -> Result (Complex Rational)
evalS ex = case ex of
  Asgn s (ChairLit _) -> do
    modify (maps . chairmap %~ M.insert s M.empty)
    throwMsg "New chair"
  Asgn s _ | M.member s defVar -> throwErr $ "Cannot change a constant value: " <> s
  Asgn s e -> do
    r <- evm e
    modify (maps . varmap %~ M.insert s r)
    throwMsg ((if "c." `T.isPrefixOf` s then "Constant " else "Variable ") <> s <> "=" <> showComplex r)
  UDF n [s] (Call "df" [e, Id x]) | s == x -> do
    let de = derivative e (Id x)
    either throwErr (evm . UDF n [s]) de
  UDF n s e -> do
    mps <- gets (^. maps)
    let newe = localize s e >>= catchVar (mps^.varmap, mps ^.funmap)
    either throwErr (\r -> do
      let newmap = M.insert (n, length s) (Fun (map (T.cons '@') s) (ExFn r)) $ mps^.funmap
      modify (maps . funmap .~ newmap)
      throwMsg ("Function " <> n <> "/" <> showT (length s))) newe
  UDO n (-1) _ e@(Call op _) -> do
    mps <- gets (^. maps)
    case M.lookup op (mps^.opmap) of
      Just o@Op{} -> do
        let newmap = M.insert n (o { oexec = AOp op }) (mps^.opmap)
        modify (maps . opmap .~ newmap)
        throwMsg $ "Operator alias " <> n <> " = " <> op
      Nothing -> throwErr $ "No such operator: " <> op
  UDO n p a e
    | M.member n operators -> throwErr $ "Can not redefine the built-in operator: " <> n
    | p < 1 || p > 14 ->  throwErr $ "Bad precedence: " <> showT p
    | otherwise -> do
        mps <- gets (^. maps)
        let t = localize ["x","y"] e >>= catchVar (mps^.varmap, mps^.funmap)
        either throwErr (\r -> do
          let newmap = M.insert n Op {precedence = p, associativity = a, oexec = ExOp r} (mps^.opmap)
          modify (maps . opmap .~ newmap)
          throwMsg $ ("Operator " <> n <> " p=" <> showT p <> " a=" <> (if a == L then "left" else "right"))) t
  Call "debug" [e] -> throwMsg . showT . preprocess $ e
  Call "str" [e] -> evm e >>= (either throwErr (throwMsg . (\s -> "\"" <> s <> "\"")) . numToText)
  Call "fmt" (Number n ni:es) -> do
    let format = numToText (n:+ni) >>= extractFormat
    case format of
      Left err -> throwError (ErrMsg err)
      Right fs -> do
        rs <- traverse evm es
        throwMsg $ case zipFormat fs rs of
          Left err -> err
          Right txt -> txt
  Call "generate" [e] -> throwMsg . T.init . T.concat . map ((<> "\n") . showT) . generate $ e
  Call "id" [x] -> evm x
  Call "df" [a,x] -> do
      let e = derivative a x
      either throwErr (throwMsg . exprToString . preprocess) e
  Call "int" [Id fun, a, b] -> evm $ Call "int" [Id fun, a, b, Number 1e-10 0]
  Call "int" [Id fun, a, b, eps] -> do
      mps <- get
      a1 <- evm a
      b1 <- evm b
      e1 <- evm eps
      return $ integrate (realPart . fromRight (0:+0) . procListElem mps fun) (realPart a1) (realPart b1) (realPart e1)
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
  Call f [e] | f `elem` (["real", "imag", "conj"] :: [Text])-> do
    t1 <- evm e
    case f of
      "real" -> return $ realPart t1 :+ 0
      "imag" -> return $ imagPart t1 :+ 0
      "conj" -> return $ conjugate t1
      _ -> throwErr $ "No such complex function: " <> f
  Call f [e] | f `elem` (["hex", "oct", "bin"] :: [Text]) -> do
    t1 <- evm e
    if denominator (realPart t1) == 1
      then let (function, p) = case f of
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
                         . abs . numerator . realPart $ t1
      else throwError (ErrMsg "Can't convert rational yet")
  Call ":" [a, b] -> evm (Call "if" [a, b, b])
  Call op1 [x, s@(Call op2 [y, z])] | isOp op1 && isOp op2 -> do
    mps <- gets (^. maps)
    let pr = getPrecedences (mps^.opmap) <> getFakePrecedences (mps^.funmap)
    let a = M.lookup op1 pr
    let b = M.lookup op2 pr
    if a == b
      then case a of
        Nothing -> throwErr $ "No such operators: " <> op1 <> " " <> op2
        Just _ -> do
          let Op {associativity = asc1 } = (mps^.opmap) M.! op1
          let Op {associativity = asc2 } = (mps^.opmap) M.! op2
          case (asc1, asc2) of
            (L, L) -> evm $ Call op2 [Call op1 [x, y], z]
            (R, R) -> evm s >>= evm . (\yy -> Call op1 [x, yy]) . (\c -> Number (realPart c) (imagPart c))
            _ -> throwErr $ "Operators with a different associativity: " <> op1 <> " and " <> op2
      else evm s >>= evm . (\yy -> Call op1 [x, yy]) . (\c -> Number (realPart c) (imagPart c))
  oc@(Call "/" [x, y]) -> do
    n <- evm y
    n1 <- evm x
    if n == 0:+0
      then throwErr $ "Division by zero: " <> exprToString oc
      else return . toComplex $ (realPart n1 / realPart n)
  oc@(Call "%" [x, y]) -> do
    n <- evm y
    n1 <- evm x
    if n == 0:+0
      then throwErr $ "Division by zero: " <> exprToString oc
      else return ((:+0) . toRational $ mod (floor . realPart $ n1 :: Integer) (floor . realPart $ n :: Integer))
  Call "|>" [x, Id y] -> evm $ Call y [x]
  Call ":=" [ChairSit a xs, ChairSit b ys] -> do
    chair1 <- gets (^. maps . chairmap . at a)
    chair2 <- gets (^. maps . chairmap . at b)
    if isNothing chair1 || isNothing chair2
      then throwErr "No such chair"
      else case extractChair ys (fromMaybe M.empty chair2) of
        Nothing -> throwErr "No such key"
        Just v -> do
          modify (maps . chairmap %~ M.insert a (sitChair xs v (fromMaybe M.empty chair1)))
          throwMsg "Sitting chair"
  Call ":=" [Id x, ChairSit a xs] -> do
    chair <- gets (^. maps . chairmap . at a)
    case chair of
      Nothing -> throwErr "No such chair"
      Just ch -> case extractChair xs ch of
        Nothing -> throwErr "No such key"
        Just (DickVal v) -> evm $ Call ":=" [Id x, Number (realPart v) (imagPart v)]
        Just (PikeVal v) -> do
          modify (maps . chairmap %~ M.insert x v)
          throwMsg "Sitting chair"
  Call ":=" [ChairSit a xs, y] -> do
    val <- case y of
      ChairLit _ -> return $ PikeVal M.empty
      ChairSit b ys -> do
        chair <- gets (^. maps . chairmap . at b)
        case chair of
          Nothing -> throwMsg "No such chair"
          Just ch -> do
            return $ fromMaybe (PikeVal M.empty) (extractChair ys ch)
      _ -> DickVal <$> evm y
    chair <- gets (^. maps . chairmap . at a)
    case chair of
      Nothing -> throwMsg "No such chair"
      Just ch -> do
        modify (maps . chairmap %~ M.insert a (sitChair xs val ch))
        throwMsg "Sitting chair"
  Call op [Id x, y] | op `elem` ([":=", "::="] :: [Text]) -> do
    if "c." `T.isPrefixOf` x || M.member x defVar
      then throwError (ErrMsg "I'm afraid you can't do that.")
      else do
        n <- evm y
        mps <- gets (^. maps)
        if x `M.member` (mps^.varmap)
          then modify (maps . varmap %~ M.insert x n)
          else modify (maps . varmap %~ M.insert ("_." <> x) n)
        return n
  Call op [Id x, y] | op `elem` (["+=", "-=", "*=", "/=", "%=", "^=", "|=", "&="] :: [Text]) -> evm (Asgn x (Call (T.init op) [Id x, y]))
  Call op [x, y] | op `elem` (["+=", "-=", "*=", "/=", "%=", "^=", "|=", "&=", ":=", "::="] :: [Text]) -> throwErr $ "Cannot assign to an expression with: " <> op
  Call op [x, y] | M.member op operators -> evalBuiltinOp op x y
  Call op [x, y] | isOp op -> do
    mps <- gets (^. maps)
    case (M.lookup op (mps^.opmap) :: Maybe Op) of
      Just Op{ oexec = ExOp expr } -> do
        let expr1 = substitute (["@x", "@y"], [x,y]) expr
        either throwErr evm expr1
      Just Op {oexec = AOp aop } -> evm (Call aop [x, y])
      Nothing -> case op of
        opn | T.head opn == '@' -> throwErr $ "Expression instead of a function name: " <> T.tail opn <> "/2"
        _ -> throwErr $ "No such operator: " <> op <> "/2"
      _ -> throwErr $ "Suspicious operator: " <> op
  Call "if" [a, b, c] -> do
    cond <- evm a
    if cond /= 0:+0
      then evm b
      else evm c
  Call "loop" [i, c, a] -> do
    _ <- evm i
    evm $ Call "loop" [c, a]
  Call "loop" [c, a] -> do
    n <- evm c
    res <- evm a
    if n == 0:+0
      then return res
      else evm $ Call "loop" [c, a]
  Call "-" [Call "^" [x, y]] -> evm $ Call "^" [Call "-" [x], y]
  Call "-" [x]         -> evm $ Call "-" [Number 0 0, x]
  Call f ps | M.member (f, length ps) functions -> do
    let builtin_fun = functions M.! (f, length ps)
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
        let expr1 = substitute (params builtin_fun, ps) expr
        either throwErr evm expr1
      _ -> throwError (ErrMsg "Misteriously missing function")
  Call name e -> do
    mps <- gets (^. maps)
    case (M.lookup (name, length e) (mps^.funmap) :: Maybe Fun) of
      Just (Fun al (ExFn expr)) -> do
        let expr1 = substitute (al, e) expr
        either throwErr evm expr1
      Nothing -> case name of
        x | T.head x == '@' -> throwErr $ "Expression instead of a function name: " <> T.tail x <> "/" <> showT (length e)
        _ -> let
               (wa, wn) = findSimilar (name, length e) (M.keys (mps^.funmap))
               cvt_nls txt nls = if not (null nls)
                  then txt <> T.init (T.concat (map (\(n, l) -> "\t" <> n <> "/" <> showT l <> "\n") nls))
                  else ""
               wat = cvt_nls "\nFunctions with the same name:\n" wa
               wnt = cvt_nls "\nFunctions with similar names:\n" wn
             in throwErr $ "No such function: " <> name <> "/" <> showT (length e) <> wat <> wnt
      _ -> throwErr $ "Suspicious function: " <> name
  Id "m.r" -> do
    rgen <- gets (^. gen)
    let (randomNumber, newGen) = randomR (0.0, 1.0 :: Double) rgen
    modify (gen %~ const newGen)
    return . (:+0) . toRational $ randomNumber
  Id s     -> do
    mps <- gets (^. maps)
    chairs <- gets (^. maps . chairmap)
    if M.member s chairs
      then
        throwMsg $ showChair (chairs M.! s)
      else do
        let val = asum ([M.lookup ("_." <> s) (mps^.varmap), M.lookup s (mps^.varmap)] :: [Maybe (Complex Rational)])
        maybe (throwError (ErrMsg $ "No such variable: " <> s)) return val
  ChairLit _ -> return $ 0 :+ 0
  ChairSit a xs -> do
    val <- gets (^. maps . chairmap . at a)
    case val of
      Nothing -> throwErr "No such chair!"
      Just ch -> case extractChair xs ch of
        Nothing -> throwErr "No such key!"
        Just (DickVal d) -> return d
        Just (PikeVal d) -> throwMsg $ showChair d
  Number x xi -> return $ x :+ xi
  Par e            -> evm e
  Seq _ -> throwErr "Sequences are not supported in this mode!"
  Imprt _ -> throwErr "Imports are not supported in this mode!"
  Label _ -> throwErr "Label are not supported in this mode!"
  where
    evalBuiltinOp bop x y = do
      let builtin_op = operators M.! bop
      case oexec builtin_op of
        FnOp (CmpOp fun)-> cmp fun x y
        FnOp (MathOp fun) -> eval' fun bop x y
        FnOp (BitOp fun)-> (:+0) <$> bitEval fun x y
        ExOp e -> evm e
        _ -> throwError (ErrMsg "Misteriously missing operator")
    tooBig = 2^(8000000 :: Integer) :: Rational
    cmp fun x y = do
      n <- evm x
      n1 <- evm y
      return $ if fun (realPart n) (realPart n1)
        then 1:+0
        else 0:+0
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
      if ( op == "^" && logBase 10 (fromComplex t1 :: Double) * (fromComplex t2 :: Double) > 2408240) ||
         realPart (f t1 t2) > tooBig
         then throwError (ErrMsg "Too much!")
         else return $ f t1 t2
    procListElem m fun n = evalState (runExceptT (evm (Call fun [Number n 0]))) m
    findSimilar :: (Text, Int) -> [(Text, Int)] -> ([(Text, Int)], [(Text, Int)])
    findSimilar (name, len) funs = let wrongArgNum = filter (\(n, l) -> n == name && len /= l) funs
                                       wrongName = filter (\(n, _) -> damerauLevenshteinNorm n name >= 0.5) funs
                                   in (wrongArgNum, wrongName)
    fromComplex = fromRational . realPart
    toComplex :: (Real a) => a -> Complex Rational
    toComplex = (:+0.0) . toRational
    extractChair :: [Text] -> Chair -> Maybe ChairVal
    extractChair [] _ = Nothing
    extractChair (x:xs) ch = case M.lookup x ch of
      Nothing -> Nothing
      Just val@(DickVal v) -> Just val
      Just val@(PikeVal v) -> case xs of
        [] -> Just val
        _ -> extractChair xs v
    sitChair :: [Text] -> ChairVal -> Chair -> Chair
    sitChair [] _ ch = ch
    sitChair (x:xs) value ch = if M.member x ch
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
