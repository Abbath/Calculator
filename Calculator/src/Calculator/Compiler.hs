{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

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
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)

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

data Value = NumVal (Complex Rational) | StrVal Text deriving (Show, Eq)

type Val = Complex Rational

newtype ValueArray = ValueArray {unarray :: V.Vector Value} deriving (Show)

data Chunk = Chunk {_code :: V.Vector Word8, _constants :: ValueArray, _umaps :: UserMaps} deriving (Show)

makeLenses ''Chunk

type StateChunk = ExceptT Text (State Chunk)

data VM = VM {_chunk :: Chunk, _ip :: Int, _stack :: [Val], _vars :: Map Text Val} deriving (Show)

makeLenses ''VM

data InterpretResult = IrOk | IrCompileError | IrRuntimeError Int deriving (Show)

data OpCode = OpReturn | OpConstant | OpCall | OpJmp | OpSet | OpGet deriving (Show, Bounded, Enum, Eq)

writeChunk :: (Enum a) => a -> StateChunk ()
writeChunk v = modify $ code %~ flip V.snoc (toWord8 v)

disassembleChunk :: V.Vector Word8 -> String
disassembleChunk v = case V.uncons v of
  Nothing -> ""
  Just (n, ns) ->
    if | n > 5 -> "err (too much)"
       | fromWord8 n == OpSet -> case V.uncons ns of
            Nothing -> "err (no constant)"
            Just (n1, ns1) -> "set (" <> show n1 <> ")\n" <> disassembleChunk ns1
       | fromWord8 n == OpGet -> case V.uncons ns of
            Nothing -> "err (no constant)"
            Just (n1, ns1) -> "get (" <> show n1 <> ")\n" <> disassembleChunk ns1
       | fromWord8 n == OpReturn -> "ret"
       | fromWord8 n == OpConstant ->
          case V.uncons ns of
            Nothing -> "err (no constant)"
            Just (n1, ns1) -> "const (" <> show n1 <> ")\n" <> disassembleChunk ns1
       | fromWord8 n == OpCall ->
          case V.uncons ns of
            Nothing -> "err (no callee number)"
            Just (n1, ns1) -> "call (" <> show n1 <> ")\n" <> disassembleChunk ns1
       | fromWord8 n == OpJmp ->
          case V.uncons ns of
            Nothing -> "err (no offset 1)"
            Just (n1, ns1) ->
              case V.uncons ns1 of
                Nothing -> "err (no offset 1)"
                Just (n2, ns2) -> "jmp (" <> show n1 <> ", " <> show n2 <> ")\n" <> disassembleChunk ns2
       | otherwise -> "err (unknown)"

writeValueArray :: ValueArray -> Value -> ValueArray
writeValueArray (ValueArray v) w = ValueArray (V.snoc v w)

indexValueArray :: ValueArray -> Value -> Maybe Int
indexValueArray (ValueArray v) val = V.elemIndex val v

goInside :: (Expr -> Either Text Expr) -> Expr -> Either Text Expr
goInside f ex = case ex of
  (Par e)    -> Par <$> f e
  (Call n e) -> Call n <$> mapM f e
  e          -> return e

substitute :: [(Text, Expr)] -> Expr -> Either Text Expr
substitute [] e = return e
substitute ((x, y) : xs) (Id i) =
  if i == x
    then return $ case y of
      Number _ _ -> y
      Id _ -> y
      Par _ -> y
      t -> Par t
    else substitute xs (Id i)
substitute s@((x, Id fname) : xs) (Call n e) =
  if n == x
    then Call fname <$> mapM (substitute s) e
    else do
      t <- mapM (substitute s) e
      substitute xs (Call n t)
substitute s (Call ":=" [Id z, e]) = do
  ne <- substitute s e
  return $ Call ":=" [Id z, ne]
substitute s@(_ : xs) (Call n e) = do
  t <- mapM (substitute s) e
  substitute xs (Call n t)
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

catchVar :: Set Text -> Maps -> Expr -> Either Text Expr
catchVar locals ms@(vm, fm, _) ex = case ex of
  (Call ":=" [Id nm, e]) -> do
    ne <- goInside (catchVar (S.insert nm locals) ms) e
    return $ Call ":=" [Id nm, ne]
  (Id i) | T.head i == '@' -> return $ Id i
  (Id i) ->
    let a = M.lookup i vm :: Maybe (Complex Rational)
        getNames = map (\(f, _) -> (f, f)) . M.keys
        b = lookup i (getNames fm ++ getNames functions) :: Maybe Text
        c = S.member i locals
    in case a of
         Just n -> return $ randomCheck n i
         Nothing -> case b of
           Just s -> return $ Id s
           Nothing -> if c
              then return $ Id i
              else Left $ "No such variable: " <> i
    where
      randomCheck n i_ = if i_ == "m.r" then Id "m.r" else Number (realPart n) (imagPart n)
  e -> goInside st e
    where
      st = catchVar locals ms

addConstant :: Value -> StateChunk ()
addConstant = doVar OpConstant

getVar :: Value -> StateChunk ()
getVar = doVar OpGet

setVar :: Value -> StateChunk ()
setVar = doVar OpSet

doVar :: OpCode -> Value -> StateChunk ()
doVar oc name = do
  writeChunk oc
  (Chunk c s m) <- get
  case indexValueArray s name of
    Nothing -> do
      let i = V.length (unarray s)
      put (Chunk c (writeValueArray s name) m)
      writeChunk i
    Just idx -> writeChunk idx

addVar :: Text -> Expr -> StateChunk ()
addVar name expr = modify $ (umaps . uvars) %~ M.insert name (UV expr)

addFun :: Maps -> Text -> [Text] -> Expr -> StateChunk ()
addFun ms name args expr = do
  new_expr <- liftEither $ localize args expr >>= catchVar S.empty ms
  modify $ (umaps . ufuns) %~ M.insert (name, length args) (UF (map (T.cons '@') args) new_expr)

addOp :: Maps -> Text -> Int -> Assoc -> Expr -> StateChunk ()
addOp ms name prec assoc expr = do
  new_expr <- liftEither $ localize ["x", "y"] expr >>= catchVar S.empty ms
  modify $ (umaps . uops) %~ M.insert name (UO prec assoc new_expr)

toWord8 :: (Enum a) => a -> Word8
toWord8 = fromIntegral . fromEnum

fromWord8 :: (Enum a) => Word8 -> a
fromWord8 = toEnum . fromIntegral

extendWord8 :: Word8 -> Int
extendWord8 = fromIntegral

interpretBc :: Maps -> Chunk -> Either Text InterpretResult
interpretBc m c = evalState (runExceptT $ run m) $ VM c 0 [] M.empty

type StateVM = ExceptT Text (State VM)

run :: Maps -> StateVM InterpretResult
run m = do
  ok <- sanityCheck
  if not ok
    then gets (IrRuntimeError . (^. ip))
    else do
      opcode <- readOpcode
      case fromWord8 opcode of
        OpSet -> do
          v <- peek
          n <- readWord
          c <- readConstant n
          case c of
            NumVal _ -> throwError "Not a var name"
            StrVal name -> trace ("set " <> T.unpack name <> " " <> show v) $ setvar name v
          runNext
        OpGet -> do
          n <- readWord
          c <- readConstant n
          case c of
            NumVal _ -> throwError "Not a var name"
            StrVal name -> trace ("get " <> T.unpack name) $ getvar name >>= push
          runNext
        OpReturn -> do
          v <- pop
          return (trace (show v) IrOk)
        OpConstant -> do
          n <- readWord
          c <- readConstant n
          case c of
            NumVal nv -> push nv
            StrVal _ -> throwError "Can't push strings"
          trace ("constant " <> show c) runNext
        OpJmp -> do
          cond <- pop
          offset <- readOffset
          when (cond == 0 :+ 0) $ trace ("jump " <> show offset) $ jump offset
          runNext
        OpCall -> do
          n <- readWord
          if n > 127
            then
              let (k, fun) = M.elemAt (fromWord8 n .&. 0x7f) (m ^. _2)
                in do
                  case fexec fun of
                    FnFn (CmpFn f) -> do
                      v1 <- pop
                      v2 <- pop
                      push (if f (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0)
                    FnFn (MathFn1 f) -> do
                      v1 <- pop
                      push (fmap toRational . f . fmap fromRational $ v1)
                    FnFn (MathFn2 f) -> do
                      v1 <- pop
                      v2 <- pop
                      push (f v2 v1)
                    FnFn (IntFn1 f) -> do
                      v1 <- pop
                      push (toRational . f . fromRational <$> v1)
                    FnFn (IntFn2 f) -> do
                      v1 <- pop
                      v2 <- pop
                      push (f (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0)
                    FnFn (BitFn f) -> do
                      v1 <- pop
                      push (toRational . f . numerator . fromRational <$> v1)
                    _ -> throwError "Function is not computable yet"
                  trace ("call " <> show k <> " " <> show fun) runNext
            else
              let (k, op) = M.elemAt (fromWord8 n) (m ^. _3)
                in do
                  case oexec op of
                    FnOp (CmpOp o) -> do
                      v1 <- pop
                      v2 <- pop
                      push (if o (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0)
                    FnOp (MathOp o) -> do
                      v1 <- pop
                      v2 <- pop
                      push (o v2 v1)
                    FnOp (BitOp o) -> do
                      v1 <- pop
                      v2 <- pop
                      push (o (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0)
                    _ -> throwError $ "Operator is not computable yet: " <> showT k <> " " <> showT op
                  trace ("call " <> show k <> " " <> show op) runNext
  where
    jump offset = modify $ ip %~ (+offset)
    runNext = run m
    push v = trace ("pushed " <> show v) $ modify $ stack %~ (v:)
    readWord = do
      oc <- gets (\vm -> (vm ^. chunk . code) V.! (vm ^. ip))
      step
      return oc
    readOffset = do
      msb <- extendWord8 <$> readWord
      lsb <- extendWord8 <$> readWord
      return $ (shiftL (msb .&. 0x7f) 8 .|. lsb) * if msb .&. 0x80 == 0x80 then -1 else 1
    pop = do
      s <- gets (^.stack)
      if null s
        then throwError "Stack underflow!"
        else do
          modify $ stack %~ const (tail s)
          return . head $ trace ("poped " <> show (head s)) s
    peek = do
      s <- gets (^.stack)
      if null s
        then throwError "Stack underflow!"
        else return . head $ trace ("peeked " <> show (head s)) s
    readOpcode = do
      oc <- gets (\vm -> (vm ^. chunk . code) V.! (vm ^. ip))
      step
      return oc
    step = modify $ ip %~ (+ 1)
    sanityCheck = do
      i <- gets (^. ip)
      l <- gets (\vm -> V.length $ vm ^. chunk . code)
      return $ i < l
    readConstant n = gets $ (V.! fromWord8 n) . unarray . (^.chunk . constants)
    getvar name = do
      mv <- gets $ M.lookup name . (^.vars)
      case mv of
        Nothing -> throwError $ "No such variable: " <> name
        Just v -> return v
    setvar name val = modify $ vars %~ M.insert name val

emptyChunk :: Chunk
emptyChunk = Chunk V.empty (ValueArray V.empty) (UM M.empty M.empty M.empty)

compile :: Maps -> Expr -> Either Text Chunk
compile m e =
  let (a, s) = runState (runExceptT (compile' m e >> writeChunk OpReturn)) emptyChunk
    in case a of
      Left err -> Left err
      Right () -> trace (disassembleChunk (s^.code)) $ Right s

getOffset :: StateChunk Int
getOffset = gets $ V.length . (^.code)

writeOffset :: Int -> Int -> StateChunk ()
writeOffset addr value = do
  let val = abs value
  let msb = toWord8 (val `shiftR` 8) .|. if value < 0 then 0x80 else 0x0
  let lsb = toWord8 (val .&. 0xff)
  modify (code %~ (V.// [(addr, msb), (addr+1, lsb)]))

unfinishedJump :: StateChunk (Int, Int)
unfinishedJump = do
  writeChunk OpJmp
  off1 <- getOffset
  writeChunk (0 :: Word8)
  writeChunk (0 :: Word8)
  off2 <- getOffset
  return (off1, off2)

compile' :: Maps -> Expr -> StateChunk ()
compile' m = go
  where
    go (Asgn name expr) = go expr >> setVar (StrVal name)
    go (UDF name args body) = addFun m name args body
    go (UDO name prec assoc body) = addOp m name prec assoc body
    go (Par e) = go e
    go (Call ":=" [Id name, expr]) = trace (show name <> " " <> show expr) $ go expr >> setVar (StrVal name)
    go (Call ":" [a, b]) = go a >> go b
    go (Call "if" [cond, t, f]) = do
      go cond
      (off1, off2) <- unfinishedJump
      go t
      addConstant (NumVal $ 0 :+ 0)
      (off3, off4) <- unfinishedJump
      go f
      off5 <- getOffset
      writeOffset off1 (off4 - off2)
      writeOffset off3 (off5 - off4)
    go (Call "loop" [s, c, a]) = do
      go s
      off1 <- getOffset
      go c
      (off2, off3) <- unfinishedJump
      go a
      addConstant (NumVal $ 0 :+ 0)
      (off4, off5) <- unfinishedJump
      writeOffset off2 (off5 - off3)
      writeOffset off4 (off1 - off5)
    go (Call op [x, y]) | M.member op (m ^. _3) = do
      go x
      go y
      writeChunk OpCall
      writeChunk (op2Code (m ^. _3) op)
    go (Call fun args) | M.member (fun, length args) ( m ^. _2) = do
      forM_ args go
      writeChunk OpCall
      writeChunk (fun2Code (m ^. _2) (fun, length args))
    go (Call callee args) = do
      UM fm om _ <- gets (^.umaps)
      when (M.member (callee, length args) fm) $ do
        let UF ps e = fm M.! (callee, length args)
        ne <- liftEither $ substitute (zip ps args) e
        go ne
      when (M.member callee om) $ do
        let UO _ _ e = om M.! callee
        ne <- liftEither $ substitute (zip ["@x", "@y"] args) e
        go ne
      when (callee == "-" && length args == 1) $ do
        go (Call "-" [Number 0 0, head args])
    go (Number a b) = do
      addConstant (NumVal $ a :+ b)
    go (Id a) = do
      if M.member a (m ^. _1)
        then addConstant (NumVal $ (m ^. _1) M.! a)
        else getVar (StrVal a)
    go (Seq es) = forM_ es go

op2Code :: OpMap -> Text -> Word8
op2Code m op = toWord8 $ M.findIndex op m

fun2Code :: FunMap -> (Text, Int) -> Word8
fun2Code m (fun, l) = 0x80 .|. toWord8 (M.findIndex (fun, l) m)

vec2Bytes :: V.Vector Word8 -> B.ByteString
vec2Bytes = B.unfoldr V.uncons