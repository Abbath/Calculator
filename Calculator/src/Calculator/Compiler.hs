{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Calculator.Compiler where

import Calculator.Types
import Calculator.Builtins
import Control.Lens ((%~), (^.), _1, _2, _3, makeLenses)
import Control.Monad.Except
import Control.Monad.State
import Data.Bits
import Data.ByteString qualified as B
import Data.Complex
import Data.Map (Map)
import Data.Map qualified as M
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector qualified as V
import Data.Word
import Debug.Trace (trace)

data OpCode = OpReturn | OpConstant | OpCall deriving (Show, Bounded, Enum)

newtype UserVar = UV {
  _uval :: Expr 
} deriving Show

makeLenses ''UserVar

data UserFun = UF
  { _params :: [Text],
    _ufexec :: Expr
  } deriving Show

makeLenses ''UserFun

data UserOp = UO
  { _precedence :: Int,
    _associativity :: Assoc,
    _uoexec :: Expr
  } deriving Show

makeLenses ''UserOp

data UserMaps = UM
  { _ufuns :: Map (Text, Int) UserFun,
    _uops :: Map Text UserOp,
    _uvars :: Map Text UserVar
  } deriving Show

makeLenses ''UserMaps

type Value = Complex Rational

newtype ValueArray = ValueArray {unarray :: V.Vector Value} deriving (Show)

data Chunk = Chunk {_code :: V.Vector Word8, _constants :: ValueArray, _umaps :: UserMaps} deriving (Show)

makeLenses ''Chunk

type StateChunk = ExceptT Text (State Chunk)

data VM = VM {chunks :: Chunk, ip :: Int, stack :: [Value]} deriving (Show)

data InterpretResult = IrOk | IrCompileError | IrRuntimeError deriving (Show)

writeChunk :: (Enum a) => a -> StateChunk ()
writeChunk v = modify (\(Chunk c s m) -> Chunk (V.snoc c (toWord8 v)) s m)

disassembleChunk :: Chunk -> String
disassembleChunk = show

writeValueArray :: ValueArray -> Value -> ValueArray
writeValueArray (ValueArray v) w = ValueArray (V.snoc v w)

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
      n@(Number _ _) -> n
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
    let a = M.lookup i vm :: Maybe (Complex Rational)
        getNames = map (\(f, _) -> (f, f)) . M.keys
        b = lookup i (getNames fm ++ getNames functions) :: Maybe Text
    in case a of
         Just n -> return $ randomCheck n i
         Nothing -> case b of
           Just s -> return $ Id s
           Nothing -> Left $ "No such variable: " <> i
    where
      randomCheck n i_ = if i_ == "m.r" then Id "m.r" else Number (realPart n) (imagPart n)
  e -> goInside st e
    where
      st = catchVar (vm, fm)

addConstant :: Value -> StateChunk Int
addConstant v = do
  (Chunk c s m) <- get
  let i = V.length (unarray s)
  put (Chunk c (writeValueArray s v) m)
  return i

addVar :: Text -> Expr -> StateChunk ()
addVar name expr = modify $ (umaps . uvars) %~ M.insert name (UV expr)

addFun :: Text -> [Text] -> Expr -> StateChunk ()
addFun name args expr = do
  new_expr <- liftEither $ localize args expr
  modify $ (umaps . ufuns) %~ M.insert (name, length args) (UF args new_expr)

addOp :: Text -> Int -> Assoc -> Expr -> StateChunk ()
addOp name prec assoc expr = modify $ (umaps . uops) %~ M.insert name (UO prec assoc expr)

toWord8 :: (Enum a) => a -> Word8
toWord8 = fromIntegral . fromEnum

fromWord8 :: (Enum a) => Word8 -> a
fromWord8 = toEnum . fromIntegral

interpretBc :: Maps -> Chunk -> (VM, InterpretResult)
interpretBc m c = run m $ VM c 0 []

push :: Value -> [Value] -> [Value]
push = (:)

pop :: [Value] -> (Value, [Value])
pop [] = error "Stack underflow!"
pop (x : xs) = (x, xs)

run :: Maps -> VM -> (VM, InterpretResult)
run m vm@(VM c i s) =
  if i >= V.length (c^.code)
    then (vm, IrRuntimeError)
    else
      let new_i = i + 1
       in case fromWord8 $ (c^.code) V.! i of
            OpReturn -> let (v, s1) = pop s in trace (show v) (VM c i s1, IrOk)
            OpConstant ->
              let n = fromWord8 @Int $ (c^.code) V.! new_i
                  v = readConstant c n
               in runNext new_i (push v s)
            OpCall ->
              let n = fromWord8 @Int $ (c^.code) V.! new_i
               in if n > 127
                    then
                      let (_, fun) = M.elemAt (n .&. 0x7f) (m ^. _2)
                       in case fexec fun of
                            FnFn (CmpFn f) ->
                              let (v1, s1) = pop s
                                  (v2, s2) = pop s1
                               in runNext new_i (push (if f (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0) s2)
                            FnFn (MathFn1 f) ->
                              let (v1, s1) = pop s
                               in runNext new_i (push (fmap toRational . f . fmap fromRational $ v1) s1)
                            FnFn (MathFn2 f) ->
                              let (v1, s1) = pop s
                                  (v2, s2) = pop s1
                               in runNext new_i (push (f v2 v1) s2)
                            FnFn (IntFn1 f) ->
                              let (v1, s1) = pop s
                               in runNext new_i (push (toRational . f . fromRational <$> v1) s1)
                            FnFn (IntFn2 f) ->
                              let (v1, s1) = pop s
                                  (v2, s2) = pop s1
                               in runNext new_i (push (f (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0) s2)
                            FnFn (BitFn f) ->
                              let (v1, s1) = pop s
                               in runNext new_i (push (toRational . f . numerator . fromRational <$> v1) s1)
                            _ -> error "Function is not computable yet"
                    else
                      let (_, op) = M.elemAt n (m ^. _3)
                       in case oexec op of
                            FnOp (CmpOp o) ->
                              let (v1, s1) = pop s
                                  (v2, s2) = pop s1
                               in runNext new_i (push (if o (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0) s2)
                            FnOp (MathOp o) ->
                              let (v1, s1) = pop s
                                  (v2, s2) = pop s1
                               in runNext new_i (push (o v2 v1) s2)
                            FnOp (BitOp o) ->
                              let (v1, s1) = pop s
                                  (v2, s2) = pop s1
                               in runNext new_i (push (o (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0) s2)
                            _ -> error "Operator is not computable yet"
  where
    readConstant cc n = (V.! n) . unarray . (^.constants) $ cc
    runNext ni val = run m $ VM c (ni + 1) val

emptyChunk :: Chunk
emptyChunk = Chunk V.empty (ValueArray V.empty) (UM M.empty M.empty M.empty)

compile :: Maps -> Expr -> Either Text Chunk
compile m e =
  let (a, s) = runState (runExceptT (compile' m e >> writeChunk OpReturn)) emptyChunk
   in case a of
        Left err -> Left err
        Right () -> trace (show (s^.code)) $ Right s

compile' :: Maps -> Expr -> StateChunk ()
compile' m = go
  where
    go (Asgn name expr) = addVar name expr
    go (UDF name args body) = addFun name args body
    go (UDO name prec assoc body) = addOp name prec assoc body
    go (Par e) = do
      go e
    go (Call op [x, y]) | M.member op (m ^. _3) = do
      go x
      go y
      writeChunk OpCall
      writeChunk (op2Code (m ^. _3) op)
    go (Call fun args) = do
      forM_ args go
      writeChunk OpCall
      writeChunk (fun2Code (m ^. _2) (fun, length args))
    go (Number a b) = do
      writeChunk OpConstant
      addConstant (a :+ b) >>= writeChunk
    go (Id a) = do
      writeChunk OpConstant
      addConstant ((m ^. _1) M.! a) >>= writeChunk
    go (Seq es) = forM_ es go

op2Code :: OpMap -> Text -> Word8
op2Code m op = toWord8 $ M.findIndex op m

fun2Code :: FunMap -> (Text, Int) -> Word8
fun2Code m (fun, l) = 0x80 .|. toWord8 (M.findIndex (fun, l) m)

vec2Bytes :: V.Vector Word8 -> B.ByteString
vec2Bytes = B.unfoldr V.uncons