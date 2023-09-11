{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Calculator.Generator where

import Calculator.Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except
import Control.Lens ((^.), (%~), _3, _1, _2)
import Data.Word
import qualified Data.Vector as V

import Debug.Trace

data Tac = TacOp Text Text Text Text
         | TacFun Text Text Text

instance Show Tac where
  show (TacOp var a op b) = T.unpack $ var <> " := " <> a <> " " <> op <> " " <> b
  show (TacFun var fun a) = T.unpack $ var <> " := " <> fun <> "(" <> a <> ")"

type ResultG = ExceptT Text (State (Int, Int, [Tac]))

isFinal :: Expr -> Bool
isFinal (Number _ _) = True
isFinal (Id _) = True
isFinal _ = False

extractFinal :: Expr -> Text
extractFinal (Number a _) = showT @Double (fromRational a)
extractFinal (Id a) = a
extractFinal _ = error "Extraction is impossible"

generate' :: Expr -> ResultG ()
generate' expr = do
  (n, ln, acc) <- get
  case expr of
    e | isFinal e -> do
      modify (_3 %~ const [TacOp (t n) (extractFinal e) "" ""])
      do_a_barrel_roll n
    (Asgn var (Call fun [a])) | isFinal a -> modify (_3 %~ (:) (TacFun var fun (extractFinal a)))
    (Asgn var (Call fun [a])) -> do
      generate' a
      ln_ <- gets (^._2)
      modify (_3 %~ (:) (TacFun var fun (t ln_)))
    (Call fun [a]) | isFinal a -> do
      modify (_3 %~ (:) (TacFun (t n) fun (extractFinal a)))
      do_a_barrel_roll n
    (Call fun [a]) -> do
      generate' a
      (n_, ln_, _) <- get
      modify (_3 %~ (:) (TacFun (t n_) fun (t ln_)))
      do_a_barrel_roll n_
    (Asgn var (Call op [a, b])) | isFinal a && isFinal b -> modify (_3 %~ (:) (TacOp var (extractFinal a) op (extractFinal b)))
    (Asgn var (Call op [a, b])) | isFinal b && not (isFinal a) -> do
      generate' a
      ln_ <- gets (^._2)
      modify (_3 %~ (:) (TacOp var (t ln_) op (extractFinal b)))
    (Asgn var (Call op [a, b])) | isFinal a && not (isFinal b) -> do
      generate' b
      ln_ <- gets (^._2)
      modify (_3 %~ (:) (TacOp var (extractFinal a) op (t ln_)))
    (Asgn var (Call op [a, b])) -> do
      generate' a
      ln_a <- gets (^._2)
      generate' b
      ln_b <- gets (^._2)
      modify (_3 %~ (:) (TacOp var (t ln_a) op (t ln_b)))
    (Call op [a, b]) | isFinal a && isFinal b -> do
      modify (_3 %~ (:) (TacOp (t n) (extractFinal a) op (extractFinal b)))
      do_a_barrel_roll n
    (Call op [a, b]) | isFinal a && not (isFinal b) -> do
      generate' b
      (n_, ln_, _) <- get
      modify (_3 %~ (:) (TacOp (t n_) (extractFinal a) op (t ln_)))
      do_a_barrel_roll n_
    (Call op [a, b]) | isFinal b && not (isFinal a) -> do
      generate' a
      (n_, ln_, _) <- get
      modify (_3 %~ (:) (TacOp (t n_) (t ln_) op (extractFinal b)))
      do_a_barrel_roll n_
    (Call op [a, b]) -> do
      generate' a
      ln_a <- gets (^._2)
      generate' b
      (n_, ln_b, _) <- get
      modify (_3 %~ (:) (TacOp (t n_) (t ln_a) op (t ln_b)))
    (Par e) -> do
      generate' e
    _ -> throwError "Not supported"
  return ()
  where t = ("t" <>) . showT
        do_a_barrel_roll n = do
          modify (_1 %~ (+ 1))
          modify (_2 %~ const n)

generate :: Expr -> [Tac]
generate e = let a = runExceptT (generate' e) in reverse $ execState a (0, 0, []) ^. _3

-- Here is the scaffolding for the future bytecode.

data StateChunk = ExceptT Text (State Chunk ())

data OpCode = OpReturn | OpConstant | OpNegate | OpAdd | OpSubtract | OpMultiply | OpDivide deriving (Show, Bounded, Enum)

data Chunk = Chunk { code :: V.Vector Word8, constants :: ValueArray } deriving Show

type Value = Double

newtype ValueArray = ValueArray { unarray :: V.Vector Value } deriving Show

data VM = VM { chunks :: Chunk, ip :: Int , stack :: [Value]} deriving Show

data InterpretResult = IrOk | IrCompileError | IrRuntimeError deriving Show

writeChunk :: Enum a => a -> State Chunk ()
writeChunk v = modify (\(Chunk c s) -> Chunk (V.snoc c (toWord8 v)) s)

disassembleChunk :: Chunk -> String
disassembleChunk = show

writeValueArray :: ValueArray -> Value -> ValueArray
writeValueArray (ValueArray v) w = ValueArray (V.snoc v w)

addConstant :: Value -> State Chunk Int
addConstant v = do
      (Chunk c s) <- get
      let i = V.length (unarray s)
      put (Chunk c $ writeValueArray s v)
      return i

toWord8 :: (Enum a) => a -> Word8
toWord8 = fromIntegral . fromEnum

fromWord8 :: (Enum a) => Word8 -> a
fromWord8 = toEnum . fromIntegral

interpret :: Chunk -> (VM, InterpretResult)
interpret c = run $ VM c 0 []

push :: Value -> [Value] -> [Value]
push = (:)

pop :: [Value] -> (Value, [Value])
pop [] = error "Stack underflow!"
pop (x:xs) = (x, xs)

run :: VM -> (VM, InterpretResult)
run vm@(VM c i s) =
  if i >= V.length (code c)
    then (vm, IrRuntimeError)
    else let new_i = i + 1
         in case fromWord8 $ code c V.! i of
      OpReturn -> let (v, s1) = pop s in trace (show v) (VM c i s1, IrOk)
      OpConstant -> let new_i_2 = new_i + 1
                        n = fromWord8 @Int $ code c V.! new_i
                        v = readConstant c n
                    in trace (show s) . run $ VM c new_i_2 (push v s)
      OpNegate -> let (v, s1) = pop s in run $ VM c new_i (push (-v) s1)
      OpAdd -> binaryOp (+)
      OpSubtract -> binaryOp (-)
      OpMultiply -> binaryOp (*)
      OpDivide -> binaryOp (/)
  where readConstant cc n = (V.!n) . unarray . constants $ cc
        binaryOp f = let (v1, s1) = pop s
                         (v2, s2) = pop s1
                   in run $ VM c (i + 1) (push (f v2 v1) s2)
      -- _ -> run $ VM c new_i

testBytecode :: Chunk
testBytecode = execState go (Chunk V.empty (ValueArray V.empty))
  where
    go = do
      writeChunk OpConstant
      addConstant 1.2 >>= writeChunk
      writeChunk OpConstant
      addConstant 3.4 >>= writeChunk
      writeChunk OpAdd
      writeChunk OpConstant
      addConstant 5.6 >>= writeChunk 
      writeChunk OpDivide
      writeChunk OpReturn
