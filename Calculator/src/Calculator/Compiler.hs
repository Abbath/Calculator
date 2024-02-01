{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Calculator.Compiler where

import Calculator.Builtins
import Calculator.Types
  ( Assoc,
    ExecFn (FnFn),
    ExecOp (FnOp),
    Expr (..),
    Fun (fexec),
    FunFun (..),
    FunMap,
    FunOp (BitOp, CmpOp, MathOp),
    Maps(..),
    Op (oexec),
    OpMap,
    numToText,
    showT,
    varmap,
    opmap,
    funmap
  )
import Control.Lens (makeLenses, use, uses, (%=), (%~), (+=), (.=), (^.))
import Control.Monad.Except
import Control.Monad.State
import Data.Aeson qualified as AE
import Data.Bits
import Data.ByteString.Lazy qualified as B
import Data.Complex
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ratio
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lens (unpacked)
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import System.Random (Random (randomR), StdGen)

-- import Debug.Trace

data Value = NumVal (Complex Rational) | StrVal Text deriving (Show, Eq, Generic)

type Val = Complex Rational

newtype ValueArray = ValueArray {unarray :: V.Vector Value} deriving (Show, Generic)

data Chunk = Chunk {_code :: V.Vector Word8, _constants :: ValueArray} deriving (Show, Generic)

makeLenses ''Chunk
newtype UserVar = UV
  { _uval :: Expr
  }
  deriving (Show, Generic)

makeLenses ''UserVar

data UserFun = UF
  { _params :: [Text],
    _ufexpr :: Expr,
    _ufchunk :: Chunk
  }
  deriving (Show, Generic)

makeLenses ''UserFun

data UserOp = UO
  { _precedence :: Int,
    _associativity :: Assoc,
    _uoexec :: Expr
  }
  deriving (Show, Generic)

makeLenses ''UserOp

data UserLabel = UL
  { _uoffset :: Int,
    _locations :: [Int]
  }
  deriving (Show)

makeLenses ''UserLabel

data UserMaps = UM
  { _ufuns :: Map (Text, Int) UserFun,
    _uops :: Map Text UserOp,
    _uvars :: Map Text UserVar,
    _ulbs :: Map Text UserLabel
  }
  deriving (Show, Generic)

makeLenses ''UserMaps

data CompileState = CS {_chunkc :: Chunk, _umaps :: UserMaps} deriving (Show)

makeLenses ''CompileState

type StateChunk = ExceptT Text (State CompileState)

type Addr = Int

data VM = VM {_chunke :: Chunk, _ip :: Int, _stack :: [Val], _cstack :: [Addr], _vars :: Map Text Val, _gen :: StdGen} deriving (Show)

makeLenses ''VM

data InterpretResult = IrOk | IrIO OpEject (Maybe Text) deriving (Show)

data OpInternal = OpReal | OpImag | OpConj | OpRandom | OpUnder deriving (Show, Bounded, Enum, Eq)

data OpEject = OpInput | OpOutput | OpFmt deriving (Show, Bounded, Enum, Eq)

data OpCode = OpReturn | OpConstant | OpBuiltin | OpJmp | OpSet | OpGet | OpInternal | OpEject | OpCall deriving (Show, Bounded, Enum, Eq)

instance AE.ToJSON UserVar

instance AE.ToJSON UserOp

instance AE.ToJSON UserFun

instance AE.ToJSON Value where
  toJSON (NumVal n) = AE.object ["tag" AE..= ("num" :: String), "value" AE..= (show (realPart n) <> "j" <> show (imagPart n))]
  toJSON (StrVal s) = AE.object ["tag" AE..= ("str" :: String), "value" AE..= s]

instance AE.ToJSON ValueArray

instance AE.ToJSON Chunk where
  toEncoding = AE.genericToEncoding AE.defaultOptions

parseComplex :: Text -> Complex Rational
parseComplex t = case T.split (== 'j') t of
  [r, i] -> read (r ^. unpacked) :+ read (i ^. unpacked)
  _ -> error "AAAA"

instance AE.FromJSON Value where
  parseJSON = AE.withObject "Value" $ \v -> do
    tag :: String <- v AE..: "tag"
    value <- v AE..: "value"
    return $ case tag of
      "num" -> NumVal $ parseComplex value
      "str" -> StrVal value
      _ -> error "AAAAA"

instance AE.FromJSON ValueArray

instance AE.FromJSON Chunk

emitByte :: (Enum a) => a -> StateChunk ()
emitByte v = chunkc . code %= flip V.snoc (toWord8 v)

disassembleChunk :: V.Vector Word8 -> String
disassembleChunk v = case V.uncons v of
  Nothing -> ""
  Just (n, ns) ->
    if
      | n > 7 -> "err (too much)"
      | n >= 0 && n <= 7 -> case fromWord8 n of
        OpSet -> case V.uncons ns of
          Nothing -> "err (no constant)"
          Just (n1, ns1) -> "set (" <> show n1 <> ")\n" <> disassembleChunk ns1
        OpGet -> case V.uncons ns of
          Nothing -> "err (no constant)"
          Just (n1, ns1) -> "get (" <> show n1 <> ")\n" <> disassembleChunk ns1
        OpReturn -> "ret"
        OpConstant ->
          case V.uncons ns of
            Nothing -> "err (no constant)"
            Just (n1, ns1) -> "const (" <> show n1 <> ")\n" <> disassembleChunk ns1
        OpBuiltin ->
          case V.uncons ns of
            Nothing -> "err (no callee number)"
            Just (n1, ns1) -> "call (" <> show n1 <> ")\n" <> disassembleChunk ns1
        OpJmp ->
          case V.uncons ns of
            Nothing -> "err (no offset 1)"
            Just (n1, ns1) ->
              case V.uncons ns1 of
                Nothing -> "err (no offset 2)"
                Just (n2, ns2) -> "jmp (" <> show n1 <> ", " <> show n2 <> ")\n" <> disassembleChunk ns2
        OpInternal ->
          case V.uncons ns of
            Nothing -> "err (no op number)"
            Just (n1, ns1) -> "internal (" <> show n1 <> ")\n" <> disassembleChunk ns1
        OpEject ->
          case V.uncons ns of
            Nothing -> "err (no eject opcode)"
            Just (n1, ns1) -> case fromWord8 n1 of
              OpInput -> "input\n" <> disassembleChunk ns1
              OpOutput -> "output\n" <> disassembleChunk ns1
              OpFmt -> case V.uncons ns1 of
                Nothing -> "err (no format string)"
                Just (n2, ns2) -> "fmt (" <> show n2 <> ")\n" <> disassembleChunk ns2
        OpCall -> disassembleChunk ns
      | otherwise -> "err (unknown)"

writeValueArray :: ValueArray -> Value -> ValueArray
writeValueArray (ValueArray v) w = ValueArray (V.snoc v w)

indexValueArray :: ValueArray -> Value -> Maybe Int
indexValueArray (ValueArray v) val = V.elemIndex val v

goInside :: (Expr -> Either Text Expr) -> Expr -> Either Text Expr
goInside f ex = case ex of
  (Par e) -> Par <$> f e
  (Call n e) -> Call n <$> mapM f e
  e -> return e

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
localize (x : xs) (Id i) = if i == x then return $ Id (T.cons '@' i) else localize xs (Id i)
localize s@(x : xs) (Call nm e) =
  if nm == x
    then Call (T.cons '@' nm) <$> mapM (localize s) e
    else do
      t <- mapM (localize s) e
      localize xs (Call nm t)
localize s ex = goInside (localize s) ex

catchVar :: Set Text -> Maps -> Expr -> Either Text Expr
catchVar locals ms ex = case ex of
  (Call ":=" [Id nm, e]) -> do
    ne <- goInside (catchVar (S.insert nm locals) ms) e
    return $ Call ":=" [Id nm, ne]
  (Id i) | T.head i == '@' -> return $ Id i
  (Id i) ->
    let a = M.lookup i (ms^.varmap) :: Maybe (Complex Rational)
        getNames = map (\(f, _) -> (f, f)) . M.keys
        b = lookup i (getNames (ms^.funmap) ++ getNames functions) :: Maybe Text
        c = S.member i locals
     in case a of
          Just n -> return $ randomCheck n i
          Nothing -> case b of
            Just s -> return $ Id s
            Nothing ->
              if c
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
  emitByte oc
  i <- findPlace name
  emitByte i

findPlace :: Value -> StateChunk Int
findPlace name = do
  (Chunk c s) <- use chunkc
  case indexValueArray s name of
    Nothing -> do
      let i = V.length (unarray s)
      chunkc .= Chunk c (writeValueArray s name)
      return i
    Just idx -> return idx

addVar :: Text -> Expr -> StateChunk ()
addVar name expr = umaps . uvars %= M.insert name (UV expr)

addFun :: Maps -> Text -> [Text] -> Expr -> StateChunk ()
addFun ms name args expr = do
  new_expr <- liftEither $ localize args expr >>= catchVar S.empty ms
  let chunk = compile ms expr
  case chunk of
    Left err -> throwError err
    Right c -> umaps . ufuns %= M.insert (name, length args) (UF (map (T.cons '@') args) new_expr c)

addOp :: Maps -> Text -> Int -> Assoc -> Expr -> StateChunk ()
addOp ms name opprec assoc expr = do
  new_expr <- liftEither $ localize ["x", "y"] expr >>= catchVar S.empty ms
  umaps . uops %= M.insert name (UO opprec assoc new_expr)

addLabel :: Text -> Int -> StateChunk ()
addLabel label off = umaps . ulbs %= M.insertWith (\(UL o _) (UL _ a) -> UL o a) label (UL off [])

useLabel :: Text -> Int -> StateChunk ()
useLabel label loc = umaps . ulbs %= M.insertWith (\(UL _ a) (UL o b) -> UL o (a <> b)) label (UL 0 [loc])

toWord8 :: (Enum a) => a -> Word8
toWord8 = fromIntegral . fromEnum

fromWord8 :: (Enum a) => Word8 -> a
fromWord8 = toEnum . fromIntegral

extendWord8 :: Word8 -> Int
extendWord8 = fromIntegral

emptyVM :: Chunk -> StdGen -> VM
emptyVM c = VM c 0 [] [] M.empty

interpretBc :: Maps -> Chunk -> StdGen -> (Either Text InterpretResult, VM)
interpretBc m c g = runState (runExceptT $ run m) $ emptyVM c g

interpretBcVM :: Maps -> VM -> (Either Text InterpretResult, VM)
interpretBcVM m = runState (runExceptT $ run m)

vec2Bytes :: V.Vector Word8 -> B.ByteString
vec2Bytes = B.unfoldr V.uncons

bytes2Vec :: B.ByteString -> V.Vector Word8
bytes2Vec = V.unfoldr B.uncons

storeBc :: FilePath -> Chunk -> IO ()
storeBc fp c = B.writeFile fp (AE.encode c)

loadBc :: FilePath -> IO (Maybe Chunk)
loadBc fp = AE.decode <$> B.readFile fp

type StateVM = ExceptT Text (State VM)

run :: Maps -> StateVM InterpretResult
run m = do
  ok <- sanityCheck
  if not ok
    then throwError "The program is insane"
    else do
      opcode <- readOpcode
      case fromWord8 opcode of
        OpCall -> return IrOk
        OpEject -> do
          n <- fromWord8 <$> readWord
          case n of
            OpFmt -> do
              n1 <- readWord
              n2 <- readConstant n1
              return . IrIO OpFmt . Just $ case n2 of
                StrVal s -> s
                NumVal num -> either id id $ numToText num
            op -> return $ IrIO op Nothing
        OpInternal -> do
          n <- readWord
          handleInternal (fromWord8 n)
          runNext
        OpSet -> do
          v <- peek
          n <- readWord
          c <- readConstant n
          case c of
            NumVal _ -> throwError "Not a var name"
            StrVal name -> setvar name v
          runNext
        OpGet -> do
          n <- readWord
          c <- readConstant n
          case c of
            NumVal _ -> throwError "Not a var name"
            StrVal name -> getvar name >>= push
          runNext
        OpReturn -> return IrOk
        OpConstant -> do
          n <- readWord
          c <- readConstant n
          case c of
            NumVal nv -> push nv
            StrVal _ -> throwError "Can't push strings"
          runNext
        OpJmp -> do
          cond <- pop
          offset <- readOffset
          when (cond == 0 :+ 0) $ jump offset
          runNext
        OpBuiltin -> do
          n <- readWord
          if n > 127
            then
              let (k, fun) = M.elemAt (fromWord8 n .&. 0x7f) (m ^. funmap)
               in do
                    v1 <- pop
                    case fexec fun of
                      FnFn (CmpFn f) -> do
                        v2 <- pop
                        push (if f (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0)
                      FnFn (FracFn1 f) -> do
                        push (f v1)
                      FnFn (MathFn1 f) -> do
                        push (fmap toRational . f . fmap fromRational $ v1)
                      FnFn (MathFn2 f) -> do
                        v2 <- pop
                        push (f v2 v1)
                      FnFn (IntFn1 f) -> do
                        push (toRational . f . fromRational <$> v1)
                      FnFn (IntFn2 f) -> do
                        v2 <- pop
                        push (f (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0)
                      FnFn (BitFn f) -> do
                        push ((:+ 0) . toRational . f . numerator . fromRational . realPart $ v1)
                      _ -> throwError $ "Function is not computable yet: " <> showT k <> " " <> showT fun
                    runNext
            else
              let (k, op) = linearOperators V.! fromWord8 n
               in do
                    v1 <- pop
                    v2 <- pop
                    if
                      | k == "/" -> do
                          push . (:+ 0) $ (realPart v2 / realPart v1)
                      | k == "%" -> do
                          push . (:+ 0) $ toRational $ mod (floor . realPart $ v2 :: Integer) (floor . realPart $ v1 :: Integer)
                      | otherwise ->
                          case oexec op of
                            FnOp (CmpOp o) -> do
                              push (if o (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0)
                            FnOp (MathOp o) -> do
                              push (o v2 v1)
                            FnOp (BitOp o) -> do
                              push (o (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0)
                            _ -> throwError $ "Operator is not computable yet: " <> showT k <> " " <> showT op
                    runNext
  where
    jump offset = ip += offset
    runNext = run m
    push v = stack %= (v :)
    readWord = do
      oc <- gets (\vm -> (vm ^. chunke . code) V.! (vm ^. ip))
      step
      return oc
    readOffset = do
      msb <- extendWord8 <$> readWord
      lsb <- extendWord8 <$> readWord
      return $ (shiftL (msb .&. 0x7f) 8 .|. lsb) * if testBit msb 7 then -1 else 1
    pop = do
      s <- use stack
      if null s
        then throwError "Stack underflow!"
        else do
          stack .= tail s
          return . head $ s
    peek = do
      s <- use stack
      if null s
        then throwError "Stack underflow!"
        else return . head $ s
    peekSafe = do
      s <- use stack
      return $
        if null s
          then 0 :+ 0
          else head s
    readOpcode = readWord
    step = ip += 1
    sanityCheck = do
      i <- use ip
      l <- uses (chunke . code) V.length
      return $ i < l
    readConstant n = gets $ (V.! fromWord8 n) . unarray . (^. chunke . constants)
    getvar name = do
      mv <- uses vars $ M.lookup name
      case mv of
        Nothing -> throwError $ "No such variable: " <> name
        Just v -> return v
    setvar name val = vars %= M.insert name val
    handleInternal op = do
      case op of
        OpReal -> pop >>= push . (:+ 0) . realPart
        OpImag -> pop >>= push . (:+ 0) . imagPart
        OpConj -> pop >>= push . conjugate
        OpUnder -> peekSafe >>= setvar "_"
        OpRandom -> do
          rgen <- use gen
          let (randomNumber, newGen) = randomR (0.0, 1.0 :: Double) rgen
          gen .= newGen
          push $ (:+ 0) . toRational $ randomNumber

emptyChunk :: Chunk
emptyChunk = Chunk V.empty (ValueArray V.empty)

emptyCompileState :: CompileState
emptyCompileState = CS emptyChunk (UM M.empty M.empty [("_", UV $ Number 0 0)] M.empty)

compile :: Maps -> Expr -> Either Text Chunk
compile m e =
  let (a, s) = runState (runExceptT (compile' m e >> processLabels >> emitByte OpReturn)) emptyCompileState
   in case a of
        Left err -> Left err
        Right () -> Right (s ^. chunkc)

getOffset :: StateChunk Int
getOffset = gets $ V.length . (^. chunkc . code)

writeOffset :: Int -> Int -> StateChunk ()
writeOffset addr value =
  if value < -2 ^ (15 :: Int) || value > 2 ^ (15 :: Int) - 1
    then throwError "Jump is too long"
    else do
      let val = abs value
      let msb = toWord8 (val `shiftR` 8) .|. if value < 0 then bit 7 else 0x0
      let lsb = toWord8 (val .&. 0xff)
      chunkc . code %= (V.// [(addr-2, msb), (addr -1, lsb)])

unfinishedJump :: StateChunk Int
unfinishedJump = do
  emitByte OpJmp
  emitByte (0 :: Word8)
  emitByte (0 :: Word8)
  getOffset

processLabels :: StateChunk ()
processLabels = do
  labels <- use (umaps . ulbs)
  forM_ labels (\(UL o ls) -> do
    forM_ ls (\loc -> do
      writeOffset loc (o - loc)))

compile' :: Maps -> Expr -> StateChunk ()
compile' m = go
  where
    go ex = case ex of
      Imprt filename -> return ()
      Asgn name expr -> go expr >> setVar (StrVal name)
      UDF name args (Call "df" [body, var]) -> case derivative body var of
        Left err -> throwError err
        Right der -> addFun m name args der
      UDF name args body -> addFun m name args body
      UDO name opprec assoc body -> addOp m name opprec assoc body
      Par e -> go e
      Call ":=" [Id name, expr] ->
        if "c." `T.isPrefixOf` name
          then throwError "Can't change constant"
          else go expr >> setVar (StrVal name)
      Call ":" [a, b] -> go a >> go b
      Call "|>" [x, Id f] -> go $ Call f [x]
      Call "input" [] -> do
        emitByte OpEject
        emitByte OpInput
      Call "print" [x] -> do
        go x
        emitByte OpEject
        emitByte OpOutput
      Call "print" (Number n ni : values) -> do
        let format = numToText (n :+ ni)
        forM_ values go
        emitByte OpEject
        emitByte OpFmt
        findPlace (StrVal $ either id id format) >>= emitByte
      Call "atan" [Call "/" [x, y]] -> go $ Call "atan2" [x, y]
      Call "log" [x, y] -> go $ Call "log2" [x, y]
      Call "if" [cond, t] -> go (Call "if" [cond, t, Number 0 0])
      Call "if" [cond, t, f] -> do
        go cond
        off1 <- unfinishedJump
        go t
        addConstant (NumVal $ 0 :+ 0)
        off2 <- unfinishedJump
        go f
        off3 <- getOffset
        writeOffset off1 (off2 - off1)
        writeOffset off2 (off3 - off2)
      Call "loop" [c, a] -> do
        off1 <- getOffset
        go c
        off2 <- unfinishedJump
        go a
        addConstant (NumVal $ 0 :+ 0)
        off3 <- unfinishedJump
        writeOffset off2 (off3 - off2)
        writeOffset off3 (off1 - off3)
      Call "loop" [s, c, a] -> do
        go s
        go $ Call "loop" [c, a]
      Call "goto" [Id l] -> do
        addConstant (NumVal $ 0 :+ 0)
        off <- unfinishedJump
        useLabel l off
      Call f [x] | f `V.elem` ["real", "imag", "conj"] -> do
        go x
        emitByte OpInternal
        let idx = fromMaybe 0 $ V.elemIndex f ["real", "imag", "conj"]
        emitByte (toEnum idx :: OpInternal)
      Call op [x, y] | M.member op (m ^. opmap) -> do
        go x
        go y
        emitByte OpBuiltin
        emitByte (op2Code (m ^. opmap) op)
      Call fun args | M.member (fun, length args) (m ^. funmap) -> do
        forM_ args go
        emitByte OpBuiltin
        emitByte (fun2Code (m ^. funmap) (fun, length args))
      Call callee args -> do
        UM fm om _ _ <- use umaps
        if
          | (M.member (callee, length args) fm) -> do
              let UF ps e _ = fm M.! (callee, length args)
              ne <- liftEither $ substitute (zip ps args) e
              go ne
          | (M.member callee om) -> do
              let UO _ _ e = om M.! callee
              ne <- liftEither $ substitute (zip ["@x", "@y"] args) e
              go ne
          | (callee == "-" && length args == 1) -> go (Call "-" [Number 0 0, head args])
          | (callee == "~" && length args == 1) -> go (Call "comp" args)
          | (callee == "!" && length args == 1) -> go (Call "fact" args)
          | otherwise -> throwError $ "Callee does not exist: " <> callee
      Number a b -> addConstant (NumVal $ a :+ b)
      Id a -> do
        if
          | a == "m.r" -> emitByte OpInternal >> emitByte OpRandom
          | M.member a (m ^. varmap) && a /= "_" -> addConstant (NumVal $ (m ^. varmap) M.! a)
          | otherwise -> getVar (StrVal a)
      Seq es -> forM_ es $ \e -> do
        go e
        emitByte OpInternal >> emitByte OpUnder
      Label l -> do
        off <- getOffset
        addLabel l off

op2Code :: OpMap -> Text -> Word8
op2Code m op = toWord8 $ M.findIndex op m

fun2Code :: FunMap -> (Text, Int) -> Word8
fun2Code m (fun, l) = 0x80 .|. toWord8 (M.findIndex (fun, l) m)

injectValue :: Val -> VM -> VM
injectValue i = stack %~ (i :)

ejectValue :: VM -> (Maybe Val, VM)
ejectValue vm =
  let st = vm ^. stack
   in case st of
        [] -> (Nothing, vm)
        (x : xs) -> (Just x, vm {_stack = xs})

ejectValues :: Int -> VM -> ([Val], VM)
ejectValues n vm =
  let st = vm ^. stack
      (s, f) = splitAt n st
   in (reverse s, vm {_stack = f})
