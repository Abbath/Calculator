{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Calculator.Evaluator (evalS, FunMap, VarMap, OpMap, Maps, Result, MessageType (..), applyPrecision, Cmd (..)) where

import Calculator.Builtins (
  defVar,
  derivative,
  divide,
  fmod,
  functions,
  getFakePrecedences,
  getPrecedences,
  maxPrecedence,
  opMember,
  operators,
  unaryOperators,
 )
import Calculator.Generator
import Calculator.Types (
  Arity (ArFixed, ArVar),
  Assoc (..),
  ChLit (..),
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
  OpArity (..),
  OpMap,
  Precise,
  SingleUnit (..),
  Unit (..),
  Value (..),
  VarMap,
  ar2int,
  chairmap,
  combineUnits,
  expandUnits,
  exprToString,
  extractFormat,
  funmap,
  gen,
  imagValue,
  isOp,
  maps,
  numToText,
  opmap,
  prec,
  preprocess,
  realValue,
  showChair,
  showDegMinSec,
  showFraction,
  showMultipleOfPi,
  showT,
  showValue,
  textToNum,
  unitlessNumber,
  unitlessValue,
  unitlessZero,
  varmap,
  zipFormat,
 )
import Calculator.Utils (hashify, isWhole)
import Control.Applicative (asum)
import Control.Arrow (Arrow (second))
import Control.Lens (at, use, (.=), (?=), (^.))
import Control.Monad (zipWithM)
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
import Data.Complex (Complex (..), conjugate, imagPart, magnitude, realPart)
import Data.Either (fromRight)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Ratio (numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Metrics (damerauLevenshteinNorm)
import Data.Vector qualified as V
import GHC.IsList qualified
import Numeric (showBin, showHex, showInt, showOct)
import System.Random (Random (randomR))

-- import Debug.Trace

goInside :: (Expr -> Either Text Expr) -> Expr -> Either Text Expr
goInside f ex = case ex of
  (Par e) -> Par <$> f e
  (Call n e) -> Call n <$> mapM f e
  (Lambda a e) -> Lambda a <$> f e
  e -> pure e

substitute :: [(Text, Expr)] -> Expr -> Either Text Expr
substitute [] e = pure e
substitute ((x, y) : xys) (Id i) =
  if i == x
    then pure $ case y of
      Number{} -> y
      (Id _) -> y
      (Par _) -> y
      t -> Par t
    else substitute xys (Id i)
substitute ((x, Lambda a ex) : xys) expr@(Call n e) =
  if n == x
    then substitute (zip a e) ex
    else substitute xys expr
substitute s@((x, Id fname) : xys) (Call n e) =
  if n == x
    then Call fname <$> mapM (substitute s) e
    else mapM (substitute s) e >>= substitute xys . Call n
substitute ((x, Id sname) : xys) (ChairSit n e) =
  if n == T.tail x
    then Right $ ChairSit sname e
    else substitute xys (ChairSit n e)
substitute s@(_ : xys) (Call n e) = mapM (substitute s) e >>= substitute xys . Call n
substitute s ex = goInside (substitute s) ex

localize :: [Text] -> Expr -> Either Text Expr
localize [] e = pure e
localize _ (Id i) | "v." `T.isPrefixOf` i = pure $ Id (hashify i)
localize (x : xs) (Id i) = if i == x then pure $ Id (hashify i) else localize xs (Id i)
localize s@(x : xs) (Call nm e) =
  if nm == x
    then Call (hashify nm) <$> mapM (localize s) e
    else mapM (localize s) e >>= localize xs . Call nm
localize s ex = goInside (localize s) ex

catchVar :: (VarMap, FunMap) -> Expr -> Either Text Expr
catchVar (vm, fm) ex = case ex of
  Call op [Id i, e] | op `elem` ([":=", "::="] :: [Text]) -> st e >>= \ne -> pure $ Call op [Id i, ne]
  Id i | T.head i == '#' -> pure ex
  Id i ->
    let a = M.lookup i vm :: Maybe Value
     in case a of
          Just n -> pure $ randomCheck (value n, unit n) i
          Nothing -> pure $ Id i
   where
    randomCheck (re :+ im, u) i_ = if i_ == "m.r" then Id "m.r" else Number re im u
  e -> goInside st e
 where
  st = catchVar (vm, fm)

newtype Cmd = Cmd Text deriving (Show, Eq)
data MessageType = ErrMsg Text | MsgMsg Text | CmdMsg Cmd deriving (Show, Eq)

type Result = ExceptT MessageType (State EvalState)

throwErr :: (MonadError MessageType m) => Text -> m a
throwErr = throwError . ErrMsg

throwMsg :: (MonadError MessageType m) => Text -> m a
throwMsg = throwError . MsgMsg

throwCmd :: (MonadError MessageType m) => Cmd -> m a
throwCmd = throwError . CmdMsg

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
  turboZip' n (x : xs) [] = (x, unitlessZero) : turboZip' n xs []
  turboZip' n [] (y : ys) = ("#v." <> showT n, y) : turboZip' (n + 1) [] ys
  turboZip' n (x : xs) (y : ys) = (x, y) : turboZip' n xs ys

pattern Debug :: GHC.IsList.Item [Expr] -> Expr
pattern Debug e = Call "debug" [e]
pattern Undef :: [Expr] -> Expr
pattern Undef es = Call "undef" es
pattern Atan2 :: GHC.IsList.Item [Expr] -> GHC.IsList.Item [Expr] -> Expr
pattern Atan2 e1 e2 = Call "atan" [Call "/" [e1, e2]]

checkUnitCompatibility :: Text -> Value -> Value -> Bool
checkUnitCompatibility _ (Value _ Unitless) _ = True
checkUnitCompatibility _ _ (Value _ Unitless) = True
checkUnitCompatibility op v1 v2 | op `elem` (["+", "-"] :: [Text]) && unit v1 == unit v2 = True
checkUnitCompatibility op v1 v2 | op `elem` (["*", "/"] :: [Text]) = True
checkUnitCompatibility op v1 v2 = False

applyPrecision :: Value -> Result Value
applyPrecision v@(Value r u) = do
  pr <- use prec
  if
    | pr == -1 -> pure v
    | pr > 0 -> pure $ Value ((/ (10 ^ pr)) . fromInteger . truncate <$> ((*) <$> r <*> pure (10 ^ pr))) u
    | otherwise -> pure $ Value (fromInteger . truncate <$> r) u

evalS :: Expr -> Result Value
evalS ex = case ex of
  Asgn [s] [ChairLit e] -> do
    es <- evalChairLit e
    maps . chairmap . at s ?= M.fromList es
    throwMsg "New chair"
  Asgn [s] _ | M.member s defVar -> throwErr $ "Cannot change a constant value: " <> s
  Asgn [f] [e@(Call "/" [Id g, Number n _ _])] -> do
    let n1 = ArFixed . fromInteger . numerator $ n
    fm <- use (maps . funmap)
    if M.member (g, n1) fm
      then do
        maps . funmap . at (f, n1) ?= (fm M.! (g, n1))
        throwMsg ("Function " <> f <> "/" <> showT n1)
      else createVar f e >>= throwMsg
  Asgn [s] [Id "undef"] -> evm (Call "undef" [Id s])
  Asgn [s] [Lambda a e] -> evm (UDF s a e)
  Asgn ss [Call "ret" es] -> zipWithM createVar ss es >>= throwMsgConcat
  Asgn ss [call@(Call f ps)] | M.member (f, ArFixed . length $ ps) functions -> do
    let fun = functions M.! (f, ArFixed . length $ ps)
    case fexec fun of
      FnFn (MultiFn fn) -> do
        args <- map (\(r :+ i) -> Number r i Unitless) . fn . map value <$> mapM evm ps
        zipWithM createVar ss args >>= throwMsgConcat
      ExFn (Call "ret" es) -> zipWithM createVar ss es >>= throwMsgConcat
      _ -> createVar (head ss) call >>= throwMsg
  Asgn ss whole@[Call name e] -> do
    mps <- use maps
    case findFunction name (length e) (mps ^. funmap) of
      Just Fun{params = al, fexec = ExFn expr@(Call "ret" _)} -> do
        case substitute (("#v.n", unitlessNumber . fromIntegral . length $ e) : turboZip al e) expr of
          Left txt -> throwErr txt
          Right (Call "ret" es) -> zipWithM createVar ss es >>= throwMsgConcat
          Right e2 -> createVar (head ss) e2 >>= throwMsg
      _ -> zipWithM createVar ss whole >>= throwMsgConcat
  Asgn ss es -> zipWithM createVar ss es >>= throwMsgConcat
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
          maps . funmap . at (f, arity) ?= Fun (map hashify args) (ExFn r)
          throwMsg ("Function " <> f <> "/" <> showT (ar2int arity))
      )
      newe
  UDO n (-1) _ e@(Call op _) -> do
    mps <- use maps
    case M.lookup (op, Ar2) (mps ^. opmap) of
      Just o@Op{} -> do
        maps . opmap . at (n, Ar2) ?= o{oexec = AOp op}
        throwMsg $ "Operator alias " <> n <> " = " <> op
      Nothing -> throwErr $ "No such operator: " <> op
  UDO n p a e
    | opMember n -> throwErr $ "Can not redefine the built-in operator: " <> n
    | p == -2 -> throwErr "Operator does not exist"
    | p < 1 || p > maxPrecedence -> throwErr $ "Bad precedence: " <> showT p
    | otherwise -> do
        mps <- use maps
        let t = localize (if a == N then ["x"] else ["x", "y"]) e >>= catchVar (mps ^. varmap, mps ^. funmap)
        either
          throwErr
          ( \r -> do
              maps . opmap . at (n, if a == N then Ar1 else Ar2) ?= Op{precedence = p, associativity = a, oexec = ExOp r}
              throwMsg ("Operator " <> n <> " p=" <> showT p <> " a=" <> (if a == L then "left" else (if a == R then "right" else "none")))
          )
          t
  Debug e -> throwMsg . showT . preprocess $ e
  Undef [Id x] -> removeVar x
  Undef [Id x, e] -> evm e >>= removeFun x . fromInteger . numerator . realPart . value
  Call "prec" [] -> prec .= 16 >> throwMsg "Precision is set to: 16"
  Call "prec" [e] -> do
    pr <- evm e
    let num = fromInteger . numerator . realPart . value $ pr
    if num >= -1 && num <= 85
      then do
        prec .= num
        throwMsg $ "Precision is set to: " <> showValue pr
      else
        throwErr "Precision should be between 0 and 85"
  Call f [Id x]
    | f `elem` (["opt", "?"] :: [Text]) ->
        if x == "m.r"
          then evm (Id "m.r")
          else extractId x >>= maybe (pure $ unitlessValue (0 :+ 0)) pure
  Call f [e] | f `elem` (["opt", "?"] :: [Text]) -> evm e
  Call op [e1, e2] | op `elem` (["opt", "?"] :: [Text]) -> do
    case e1 of
      Id x ->
        if x == "m.r"
          then evm (Id "m.r")
          else do
            res <- evm e2
            extractId x >>= maybe (pure res) pure
      _ -> evm e1
  Call "str" [e] -> evm e >>= (either throwErr (throwMsg . \s -> "\"" <> s <> "\"") . numToText)
  Call "fmt" (Number n ni u : es) -> do
    let format = numToText (Value (n :+ ni) u) >>= extractFormat
    case format of
      Left err -> throwErr err
      Right fs -> do
        rs <- traverse evm es
        case zipFormat fs rs of
          Left err -> throwErr err
          Right t -> pure . flip Value u . (:+ 0) . (% 1) . textToNum $ t
  Call "generate" [e] -> throwMsg . T.init . T.concat . map ((<> "\n") . showT) . generateTac $ e
  Call "id" [x] -> evm x
  Call "df" [a, x] -> either throwErr (throwMsg . exprToString . preprocess) $ derivative a x
  Call "int" [Id fun, a, b] -> evm $ Call "int" [Id fun, a, b, unitlessNumber 1e-10]
  Call "int" [Id fun, a, b, eps] -> do
    mps <- get
    a1 <- evm a
    b1 <- evm b
    e1 <- evm eps
    pure $ integrate (realValue . fromRight (unitlessValue $ 0 :+ 0) . procListElem mps fun) (realValue a1) (realValue b1) (realValue e1)
  Call "int" [Lambda as e, a, b] -> evm $ Call "int" [Lambda as e, a, b, unitlessNumber 1e-10]
  Call "int" [Lambda as e, a, b, eps] -> do
    mps <- use maps
    let newe = localize as e >>= catchVar (mps ^. varmap, mps ^. funmap)
    let arity = ArFixed (length as)
    either throwErr (\r -> maps . funmap . at ("#temp", arity) ?= Fun (map hashify as) (ExFn r)) newe
    res <- evm (Call "int" [Id "#temp", a, b, eps])
    maps . funmap .= M.delete ("#temp", arity) (mps ^. funmap)
    pure res
  Atan2 e1 e2 -> evm $ Call "atan2" [e1, e2]
  Call "prat" [e] -> evm e >>= throwMsg . showFraction . realValue
  Call "smp" [e] -> evm e >>= throwMsg . showMultipleOfPi
  Call "smd" [e] -> evm e >>= throwMsg . showDegMinSec
  Call f [e] | f `elem` (["real", "imag", "conj"] :: [Text]) -> do
    t1 <- evm e
    let (Value t2 u) = t1
    case f of
      "real" -> pure $ Value (realPart t2 :+ 0) u
      "imag" -> pure $ Value (imagPart t2 :+ 0) u
      "conj" -> pure $ Value (conjugate t2) u
      _ -> throwErr $ "No such complex function: " <> f
  Call "^" [Call "neg" [x], y] -> evm $ Call "neg" [Call "^" [x, y]]
  Call f [e] | f `elem` (["hex", "oct", "bin"] :: [Text]) -> do
    t1 <- evm e
    if isWhole . value $ t1
      then
        let (function, p) = case f of
              "hex" -> (showHex, 'x')
              "oct" -> (showOct, 'o')
              "bin" -> (showBin, 'b')
              _ -> (showInt, ' ')
            sign = signum . numerator . realValue $ t1
         in throwMsg
              . T.pack
              . ((if sign == 1 then "" else "-") <>)
              . (['0', p] <>)
              . (`function` "")
              . abs
              . numerator
              . realValue
              $ t1
      else throwErr "Can't convert rational yet"
  Call ":" [a, b] -> evm (Call "if" [a, b, b])
  Call op1 [x, s@(Call op2 [y, z])] | isOp op1 && isOp op2 -> do
    mps <- use maps
    let pr = getPrecedences (mps ^. opmap) <> getFakePrecedences (mps ^. funmap)
    let a = M.lookup (op1, Ar2) pr
    let b = M.lookup (op2, Ar2) pr
    if a == b
      then case a of
        Nothing -> throwErr $ "No such operators: " <> op1 <> " " <> op2
        Just _ -> do
          let Op{associativity = asc1} = (mps ^. opmap) M.! (op1, Ar2)
          let Op{associativity = asc2} = (mps ^. opmap) M.! (op2, Ar2)
          case (asc1, asc2) of
            (L, L) -> evm $ Call op2 [Call op1 [x, y], z]
            (R, R) -> evm s >>= evm . (\yy -> Call op1 [x, yy]) . \c -> Number (realValue c) (imagValue c) (unit c)
            _ -> throwErr $ "Operators with a different associativity: " <> op1 <> " and " <> op2
      else evm s >>= evm . (\yy -> Call op1 [x, yy]) . \c -> Number (realValue c) (imagValue c) (unit c)
  oc@(Call op [x, y]) | op `elem` (["/", "div"] :: [Text]) -> do
    n <- evm y
    n1 <- evm x
    if value n == 0 :+ 0
      then throwErr $ "Division by zero: " <> exprToString oc
      else
        if op == "/"
          then pure . unitlessValue $ value n1 `divide` value n
          else pure . unitlessValue $ (:+ 0) . toRational $ (toInteger . numerator . realValue $ n1) `div` (toInteger . numerator . realValue $ n)
  oc@(Call "%" [x, y]) -> do
    n <- evm y
    n1 <- evm x
    if realValue n == 0
      then throwErr $ "Division by zero: " <> exprToString oc
      else pure . unitlessValue $ value n1 `fmod` value n
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
        Just (DickVal v) -> evm $ Call ":=" [Id x, Number (realValue v) (imagValue v) (unit v)]
        Just val@(ForkVal v) -> do
          maps . chairmap . at x ?= M.singleton "a" val
          throwMsg "Sittong chair"
        Just (PikeVal v) -> do
          maps . chairmap . at x ?= v
          throwMsg "Sitting chair"
  Call ":=" [ChairSit a xs, y] -> do
    val <- case y of
      ChairLit e -> PikeVal . M.fromList <$> evalChairLit e
      ChairSit b ys -> do
        chair <- use $ maps . chairmap . at b
        case chair of
          Nothing -> throwMsg "No such chair"
          Just ch -> do
            pure $ fromMaybe (PikeVal M.empty) (extractChair ys ch)
      _ -> DickVal <$> evm y
    chair <- use $ maps . chairmap . at a
    case chair of
      Nothing -> throwMsg "No such chair"
      Just ch -> do
        maps . chairmap . at a ?= sitChair xs val ch
        throwMsg "Sitting chair"
  Call op [Id x, y] | op `elem` ([":=", "::="] :: [Text]) -> do
    if "c." `T.isPrefixOf` x || M.member x defVar
      then throwErr "I'm afraid you can't do that."
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
        pure n
  Call op [Id x, y] | op `elem` (["+=", "-=", "*=", "/=", "%=", "^=", "|=", "&="] :: [Text]) -> evm (Asgn [x] [Call (T.init op) [Id x, y]])
  Call op [x, y] | op `elem` (["+=", "-=", "*=", "/=", "%=", "^=", "|=", "&=", ":=", "::="] :: [Text]) -> throwErr $ "Cannot assign to an expression with: " <> op
  Call op [x] | M.member (op, Ar1) unaryOperators -> evalBuiltinOp1 (op, Ar1) x
  Call op [x, y] | M.member (op, Ar2) operators -> evalBuiltinOp2 (op, Ar2) x y
  Call op [x] | isOp op -> do
    mps <- use maps
    case (M.lookup (op, Ar1) (mps ^. opmap) :: Maybe Op) of
      Just Op{oexec = ExOp expr} -> either throwErr evm $ substitute (zip ["#x"] [x]) expr
      Just Op{oexec = AOp aop} -> evm (Call aop [x])
      Nothing -> case op of
        opn | T.head opn == '#' -> throwErr $ "Expression instead of a function name: " <> T.tail opn <> "/1"
        _ -> throwErr $ "No such operator: " <> op <> "/1"
      _ -> throwErr $ "Suspicious operator: " <> op
  Call op [x, y] | isOp op -> do
    mps <- use maps
    case (M.lookup (op, Ar2) (mps ^. opmap) :: Maybe Op) of
      Just Op{oexec = ExOp expr} -> either throwErr evm $ substitute (zip ["#x", "#y"] [x, y]) expr
      Just Op{oexec = AOp aop} -> evm (Call aop [x, y])
      Nothing -> case op of
        opn | T.head opn == '#' -> throwErr $ "Expression instead of a function name: " <> T.tail opn <> "/2"
        _ -> throwErr $ "No such operator: " <> op <> "/2"
      _ -> throwErr $ "Suspicious operator: " <> op
  Call "if" [a, b, c] -> do
    cond <- evm a
    if value cond /= 0 :+ 0
      then evm b
      else evm c
  Call "loop" [i, c, a] -> evm i >> evm (Call "loop" [c, a])
  Call "loop" [c, a] -> do
    n <- evm c
    if value n == 0 :+ 0
      then evm a
      else evm $ Call "loop" [c, a]
  Call "neg" [x] -> evm $ Call "*" [Number (-1) 0 Unitless, x]
  Call "ret" (e : _) -> evm e
  Call f ps | M.member (f, ArFixed . length $ ps) functions -> do
    let builtin_fun = functions M.! (f, ArFixed . length $ ps)
    pr <- use prec
    case fexec builtin_fun of
      FnFn (EqFn fun) -> eq pr fun (head ps) (ps !! 1)
      FnFn (OrdFn fun) -> cmp fun (head ps) (ps !! 1)
      FnFn (IntFn1 fun) -> evalInt1 fun (head ps)
      FnFn (IntFn2 fun) -> evalInt fun (head ps) (ps !! 1)
      FnFn (BitFn fun) -> evalBit fun (head ps)
      FnFn (FracFn1 fun) -> unitlessValue . fun . value <$> evm (head ps)
      FnFn (MathFn1 fun) -> do
        n <- evm (head ps)
        let r = (\x -> if magnitude x <= sin pi then 0 else x) . fun . fmap fromRational . value $ n
        pure . unitlessValue $ toRational <$> r
      FnFn (MathFn2 fun) -> unitlessValue <$> (fun . value <$> evm (head ps) <*> (value <$> evm (ps !! 1)))
      FnFn (MathFn3 fun) -> unitlessValue <$> (fun . value <$> evm (head ps) <*> (value <$> evm (ps !! 1)) <*> (value <$> evm (ps !! 2)))
      FnFn (MultiFn fun) -> unitlessValue . head . fun . map value <$> mapM evm ps
      ExFn expr -> either throwErr evm $ substitute (zip (params builtin_fun) ps) expr
      _ -> throwErr "Misteriously missing function"
  Call name e -> do
    mps <- use maps
    case findFunction name (length e) (mps ^. funmap) of
      Just Fun{params = al, fexec = (ExFn expr)} -> either throwErr evm $ substitute (("#v.n", unitlessNumber . fromIntegral . length $ e) : turboZip al e) expr
      Nothing -> case name of
        x | T.head x == '#' -> throwErr $ "Expression instead of a function name: " <> T.tail x <> "/" <> showT (length e)
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
    let (randomNumber, newGen) = randomR (0.0, 1.0 :: Precise) rgen
    gen .= newGen
    pure . unitlessValue . (:+ 0) . toRational $ randomNumber
  Id s -> extractId s >>= maybe (throwErr $ "No such variable : " <> s) pure
  ChairLit _ -> pure $ unitlessValue (0 :+ 0)
  ChairSit a [] -> do
    val <- use $ maps . chairmap . at a
    case val of
      Nothing -> throwErr "No such chair!"
      Just ch -> throwMsg $ showChair ch
  ChairSit a xs -> do
    val <- use $ maps . chairmap . at a
    case val of
      Nothing -> throwErr "No such chair!"
      Just ch -> case extractChair xs ch of
        Nothing -> throwErr "No such key!"
        Just (DickVal d) -> pure d
        Just (ForkVal d) -> throwMsg $ "{" <> (T.intercalate "," . map showValue $ V.toList d) <> "}"
        Just (PikeVal d) -> throwMsg $ showChair d
  Number x xi u -> pure $ expandUnits $ Value (x :+ xi) u
  Par e -> evm e
  Seq _ -> throwErr "Sequences are not supported in this mode!"
  Imprt f -> throwCmd $ Cmd f
  Label _ -> throwErr "Label are not supported in this mode!"
  Lambda _ _ -> pure . unitlessValue $ 0 :+ 0
 where
  evalChairLit :: ChLit -> Result [(Text, ChairVal)]
  evalChairLit (ChMap m) = mapM (\(k, v) -> (k,) . DickVal <$> evm v) m
  evalChairLit (ChArr m) = do
    vs <- mapM evm m
    pure [("a", ForkVal (V.fromList vs))]
  extractId s = do
    chairs <- use $ maps . chairmap
    vars <- use $ maps . varmap
    if M.member s chairs
      then
        throwMsg $ showChair (chairs M.! s)
      else do
        let val = asum ([M.lookup ("_." <> s) vars, M.lookup s vars] :: [Maybe Value])
        pure val
  createVar f e = do
    r <- evm e
    maps . varmap . at f ?= r
    pure $ (if "c." `T.isPrefixOf` f then "Constant " else "Variable ") <> f <> "=" <> showValue r
  throwMsgConcat = throwMsg . T.intercalate "\n"
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
  evalBuiltinOp1 uop x = do
    let builtin_op = unaryOperators M.! uop
    case oexec builtin_op of
      FnOp (UnOp fun) -> unitlessValue . (:+ 0) . toRational . fun . numerator . realValue <$> evm x
      _ -> throwErr "Misteriously missing operator"
  evalBuiltinOp2 bop x y = do
    let builtin_op = operators M.! bop
    pr <- use prec
    case oexec builtin_op of
      FnOp (OrdOp fun) -> cmp fun x y
      FnOp (EqOp fun) -> eq pr fun x y
      FnOp (MathOp fun) -> do
        v1 <- evm x
        v2 <- evm y
        if not $ checkUnitCompatibility (fst bop) v1 v2
          then throwErr "Incompatible units"
          else
            pure $
              if unit v1 == UProd [SUnit "m" 1] && fst bop == "^" && unit v2 == Unitless && value v2 `elem` ([2 :+ 0, 3 :+ 0] :: [Complex Rational])
                then Value (fun (value v1) (value v2)) (UProd [SUnit "m" (fromIntegral . numerator . realValue $ v2)])
                else Value (fun (value v1) (value v2)) (combineUnits (fst bop) (unit v1) (unit v2))
      FnOp (BitOp fun) -> unitlessValue . (:+ 0) <$> bitEval fun x y
      ExOp e -> evm e
      _ -> throwErr "Misteriously missing operator"
  tooBig = 2 ^ (8000000 :: Integer) :: Rational
  eq pr fun x y = do
    n <- evm x
    n1 <- evm y
    pure . unitlessValue . (:+ 0) . fromIntegral . fromEnum $ fun pr (value n) (value n1)
  cmp fun x y = do
    n <- evm x
    n1 <- evm y
    pure . unitlessValue . (:+ 0) . fromIntegral . fromEnum $ fun (realValue n) (realValue n1)
  bitEval op x y = do
    n <- evm x
    n1 <- evm y
    if isWhole (value n) && isWhole (value n1)
      then pure . toRational $ op (numerator (realValue n)) (numerator (realValue n1))
      else throwErr "Cannot perform bitwise operator on a rational"
  evm x = do
    v <- evalS x
    let (Value r u) = v
    if abs (realPart r) > tooBig || abs (imagPart r) > tooBig
      then throwErr "Too much!"
      else pure $ Value r u
  evalInt f x y = do
    t1 <- evm x
    t2 <- evm y
    if isWhole (value t1) && isWhole (value t2)
      then pure . toValue $ f (numerator (realValue t1)) (numerator (realValue t2))
      else throwErr "Cannot use integral function on rational numbers!"
  evalInt1 :: (Precise -> Integer) -> Expr -> Result Value
  evalInt1 f x = toValue . f . fromValue <$> evm x
  evalBit f x = do
    t <- evm x
    if isWhole (value t)
      then pure . toValue $ f (numerator (realValue t))
      else throwErr "Cannot use bitwise function on rational numbers!"
  procListElem m fun n = evalState (runExceptT (evm (Call fun [unitlessNumber n]))) m
  findSimilar :: (Text, Int) -> [(Text, Arity)] -> ([(Text, Int)], [(Text, Int)])
  findSimilar (name, arity) funs =
    let extractArity = map (second ar2int)
        wrongArgNum = extractArity $ filter (\(n, a) -> n == name && arity /= ar2int a) funs
        wrongName = extractArity $ filter (\(n, _) -> let dln = damerauLevenshteinNorm n name in dln >= 0.5 && dln < 1) funs
     in (wrongArgNum, wrongName)
  fromComplex = fromRational . realPart
  fromValue = fromComplex . value
  toComplex :: (Real a) => a -> Complex Rational
  toComplex = (:+ 0.0) . toRational
  toValue :: (Real a) => a -> Value
  toValue = unitlessValue . toComplex
  extractChair :: [Text] -> Chair -> Maybe ChairVal
  extractChair [] _ = Nothing
  extractChair (x : xs) ch = case M.lookup x ch of
    Nothing -> Nothing
    Just val@(DickVal v) -> Just val
    Just (ForkVal v) -> case xs of
      [n] -> Just (DickVal (v V.! read (T.unpack n)))
      _ -> Nothing
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
          Just (ForkVal _) -> ch
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

integrate :: (Rational -> Rational) -> Rational -> Rational -> Rational -> Value
integrate f a b eps =
  let fa = f a
      fb = f b
      (m, r, g) = s1_ f a b fa fb
   in unitlessValue $ s2_ f a b fa fb eps g m r :+ 0
