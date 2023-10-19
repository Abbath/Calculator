{-# LANGUAGE OverloadedStrings #-}
module Calculator.Compiler where

import Data.Word
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Complex
import Data.Bits
import Debug.Trace ( trace )
import qualified Data.ByteString as B
import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import Calculator.Types
import Control.Lens ((^.), _3, _1, _2)
import Data.Ratio

type StateChunk = ExceptT Text (State Chunk)

data OpCode = OpReturn | OpConstant | OpCall deriving (Show, Bounded, Enum)

data Chunk = Chunk { code :: V.Vector Word8, constants :: ValueArray } deriving Show

type Value = Complex Rational

newtype ValueArray = ValueArray { unarray :: V.Vector Value } deriving Show

data VM = VM { chunks :: Chunk, ip :: Int , stack :: [Value]} deriving Show

data InterpretResult = IrOk | IrCompileError | IrRuntimeError deriving Show

writeChunk :: Enum a => a -> StateChunk ()
writeChunk v = modify (\(Chunk c s) -> Chunk (V.snoc c (toWord8 v)) s)

disassembleChunk :: Chunk -> String
disassembleChunk = show

writeValueArray :: ValueArray -> Value -> ValueArray
writeValueArray (ValueArray v) w = ValueArray (V.snoc v w)

addConstant :: Value -> StateChunk Int
addConstant v = do
      (Chunk c s) <- get
      let i = V.length (unarray s)
      put (Chunk c $ writeValueArray s v)
      return i

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
pop (x:xs) = (x, xs)

run :: Maps -> VM -> (VM, InterpretResult)
run m vm@(VM c i s) =
  if i >= V.length (code c)
    then (vm, IrRuntimeError)
    else let new_i = i + 1
         in case fromWord8 $ code c V.! i of
      OpReturn -> let (v, s1) = pop s in trace (show v) (VM c i s1, IrOk)
      OpConstant -> let n = fromWord8 @Int $ code c V.! new_i
                        v = readConstant c n
                    in runNext new_i (push v s)
      OpCall -> let n = fromWord8 @Int $ code c V.! new_i
                in if n > 127
                  then let (_, fun) = M.elemAt (n .&. 0x7f) (m^._2)
                       in case fexec fun of
                         FnFn (CmpFn f)   -> let (v1, s1) = pop s
                                                 (v2, s2) = pop s1
                                             in runNext new_i (push (if f (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0) s2)
                         FnFn (MathFn1 f) -> let (v1, s1) = pop s
                                             in runNext new_i (push (fmap toRational . f . fmap fromRational $ v1) s1)
                         FnFn (MathFn2 f) -> let (v1, s1) = pop s
                                                 (v2, s2) = pop s1
                                             in runNext new_i (push (f v2 v1) s2)
                         FnFn (IntFn1 f)  -> let (v1, s1) = pop s
                                             in runNext new_i (push (toRational . f . fromRational <$> v1) s1)
                         FnFn (IntFn2 f)  -> let (v1, s1) = pop s
                                                 (v2, s2) = pop s1
                                             in runNext new_i (push (f (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0) s2)
                         FnFn (BitFn f)   -> let (v1, s1) = pop s
                                             in runNext new_i (push (toRational . f . numerator . fromRational <$> v1) s1)
                         _ -> error "Function is not computable yet"
                  else let (_, op) = M.elemAt n (m^._3)
                       in case oexec op of
                         FnOp (CmpOp o)  -> let (v1, s1) = pop s
                                                (v2, s2) = pop s1
                                            in runNext new_i (push (if o (realPart v2) (realPart v1) then 1 :+ 0 else 0 :+ 0) s2)
                         FnOp (MathOp o) -> let (v1, s1) = pop s
                                                (v2, s2) = pop s1
                                            in runNext new_i (push (o v2 v1) s2)
                         FnOp (BitOp o)  -> let (v1, s1) = pop s
                                                (v2, s2) = pop s1
                                            in runNext new_i (push (o (numerator . realPart $ v2) (numerator . realPart $ v1) % 1 :+ 0) s2)
                         _ -> error "Operator is not computable yet"
  where readConstant cc n = (V.!n) . unarray . constants $ cc
        runNext ni val = run m $ VM c (ni + 1) val

emptyChunk :: Chunk
emptyChunk = Chunk V.empty (ValueArray V.empty)

compile :: Maps -> Expr -> Either Text Chunk
compile m e = let (a, s) = runState (runExceptT (compile' m e >> writeChunk OpReturn)) emptyChunk
               in case a of
                Left err -> Left err
                Right () -> trace (show (code s)) $ Right s

compile' :: Maps -> Expr -> StateChunk ()
compile' m = go
  where
    go (Par e) = do
      go e
    go (Call op [x, y]) | M.member op (m^._3) = do
      go x
      go y
      writeChunk OpCall
      writeChunk (op2Code (m^._3) op)
    go (Call fun args) = do
      forM_ args go
      writeChunk OpCall
      writeChunk (fun2Code (m^._2) (fun, length args))
    go (Number a b) = do
      writeChunk OpConstant
      addConstant (a :+ b) >>= writeChunk
    go (Id a) = do
      writeChunk OpConstant
      addConstant ((m^._1) M.! a) >>= writeChunk
    go e = throwError ("Cannot compile yet: " <> showT e)

op2Code :: OpMap -> Text -> Word8
op2Code m op = toWord8 $ M.findIndex op m

fun2Code :: FunMap -> (Text, Int) -> Word8
fun2Code m (fun, l) = 0x80 .|. toWord8 (M.findIndex (fun, l) m)

vec2Bytes :: V.Vector Word8 -> B.ByteString
vec2Bytes = B.unfoldr V.uncons