{-# LANGUAGE OverloadedStrings, OverloadedLists  #-}
module Calculator.Tests (testLoop) where

import Calculator.Builtins (opMap)
import Calculator.Evaluator (evalS, MessageType(ErrMsg, MsgMsg))
import Calculator.Lexer (tloop)
import qualified Calculator.Parser as P
import Calculator.Types (showT, Maps, VarMap, EvalState(..))
import Control.Lens ((%~), (&), _1)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO (putStrLn)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)
import System.Random (StdGen, initStdGen)
import Data.Complex ( Complex(..) )
import Data.Bifunctor ( Bifunctor(second) )

data Backend = Internal deriving Show

type Tests = [(Text, Either MessageType (Complex Rational))]

loop :: Tests -> Maps -> StdGen -> Backend -> Int -> IO Int
loop [] _ _ _ n = return n
loop (x:xs) mps rgen bk n = do
  let sample = fst x
  if not $ T.null sample
    then do
      let e = case bk of
                Internal -> tloop sample >>= P.parse mps
      let t = case e of
                Left err -> (Left (ErrMsg err), EvalState mps rgen M.empty)
                Right r  -> runState (runExceptT (evalS r)) (EvalState mps rgen M.empty)
      let tt = fst t
      do
        new_n <- if tt == snd x
          then do
            TIO.putStrLn $ "Passed: " <> sample
            return 0
          else do
            TIO.putStrLn $ "Failed: " <> sample <> " expected: " <> showT x <> " received: " <> showT t
            return 1
        let (m, g) = case t of
                      (Right r, EvalState m_ g_ _)  -> (m_ & _1 %~ M.insert "_" r, g_)
                      (Left _, EvalState m_ g_ _) -> (m_, g_)
        loop xs m g bk (n + new_n)

    else do
      TIO.putStrLn "Empty!"
      loop xs mps rgen bk n

tests :: Tests
tests = map (Data.Bifunctor.second ((:+ 0) <$>)) [
   ("_", Right 0)
  ,("1", Right 1)
  ,("_", Right 1)
  ,("-1", Right (-1))
  ,("2+2", Right 4)
  ,("2-2-2-2", Right (-4))
  ,("1024/2/2/2", Right 128)
  ,("f(x) = x", Left . MsgMsg $ "Function f/1")
  ,("f(g,x) = g(x)", Left . MsgMsg $ "Function f/2")
  ,("f(sin,m.pi)", Right 0)
  ,("h() = 2", Left . MsgMsg $ "Function h/0")
  ,("hh = h()", Left . MsgMsg $ "Variable hh=2")
  ,("p(x,y) = x - y", Left . MsgMsg $ "Function p/2")
  ,("p(2,2)", Right 0)
  ,("$(1,0) = x - y", Left . MsgMsg $ "Operator $ p=1 a=left")
  ,("2$2$2$2", Right (-4))
  ,("2$2-2$2", Right 0)
  ,("$(5,0) = x - y", Left . MsgMsg $ "Operator $ p=5 a=left")
  ,("2$2-2$2", Right (-4))
  ,("2^3^4", Right 2417851639229258349412352)
  ,("2+2*2", Right 6)
  ,("-((1))", Right (-1))
  ,("-1^2", Right 1)
  ,("(2+2)*2", Right 8)
  ,("x = 5",Left . MsgMsg $ "Variable x=5")
  ,("abs(-x)==x", Right 1)
  ,("1!=2", Right 1)
  ,("sin(m.pi)==0", Right 1)
  ,("/= = !=", Left . MsgMsg $ "Operator alias /= = !=")
  ,("1|0", Right 1)
  ,("1&0", Right 0)
  ,("0xff", Right 255)
  ,("0o45", Right 37)
  ,("0b1010", Right 10)
  ]

defVar :: VarMap
defVar = [("m.pi", (:+0) $ toRational (pi :: Double)), ("m.e", (:+0) $ toRational . exp $ (1.0 :: Double)), ("_",0.0:+0.0)]

testLoop :: IO ()
testLoop = do
  g <- initStdGen
  TIO.putStrLn "Internal parser:"
  n <- loop tests (defVar, M.empty, opMap) g Internal 0
  if n == 0
    then exitSuccess
    else exitWith $ ExitFailure n
